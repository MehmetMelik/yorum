//! Ownership and move checker for the Yorum language.
//!
//! Enforces:
//! - Move semantics: using a moved non-copy value is an error
//! - Branch safety: moves in any branch propagate conservatively
//! - Loop safety: cannot move outer-scope variables inside loops
//! - Task must-join: Task variables must be `.join()`'d before scope exit

use crate::compiler::ast::*;
use crate::compiler::span::Span;
use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Debug, Clone)]
pub struct OwnershipError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for OwnershipError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ownership error at {}: {}", self.span, self.message)
    }
}

impl std::error::Error for OwnershipError {}

#[derive(Debug, Clone, PartialEq)]
enum VarState {
    Owned,
    Moved,
}

#[derive(Debug, Clone)]
struct VarInfo {
    state: VarState,
    ty: Type,
    def_span: Span,
}

/// Copy types are freely duplicated — assigning or returning them does not move.
fn is_copy_type(ty: &Type) -> bool {
    match ty {
        Type::Int | Type::Float | Type::Bool | Type::Char | Type::Str | Type::Unit => true,
        // Arrays, Maps, and Sets use reference semantics at runtime (heap-allocated,
        // accessed via pointer), so copying is cheap and safe — like string.
        Type::Array(_) => true,
        Type::Generic(name, _) if name == "Map" || name == "Set" => true,
        Type::Tuple(types) => types.iter().all(is_copy_type),
        _ => false,
    }
}

pub struct OwnershipChecker {
    scopes: Vec<HashMap<String, VarInfo>>,
    /// Track Task variables that must be joined before scope exit
    task_vars: Vec<HashSet<String>>,
    errors: Vec<OwnershipError>,
    /// Nesting depth inside while/for loops (> 0 means we're in a loop)
    loop_depth: usize,
}

impl Default for OwnershipChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl OwnershipChecker {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            task_vars: Vec::new(),
            errors: Vec::new(),
            loop_depth: 0,
        }
    }

    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<OwnershipError>> {
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) => self.check_function(f),
                Declaration::Impl(i) => {
                    for method in &i.methods {
                        self.check_function(method);
                    }
                }
                Declaration::Trait(t) => {
                    for method in &t.methods {
                        if let Some(body) = &method.default_body {
                            self.push_scope();
                            for param in &method.params {
                                self.define(&param.name, param.ty.clone(), param.span);
                            }
                            self.check_block(body);
                            self.pop_scope();
                        }
                    }
                }
                _ => {}
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn check_function(&mut self, f: &FnDecl) {
        self.push_scope();
        for param in &f.params {
            self.define(&param.name, param.ty.clone(), param.span);
        }
        self.check_block(&f.body);
        self.pop_scope();
    }

    fn check_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(s) => {
                self.check_expr_move(&s.value);
                // Handle tuple destructuring: define each name with its element type
                if let Some(ref names) = s.destructure {
                    if let Type::Tuple(ref elem_types) = s.ty {
                        for (name, ty) in names.iter().zip(elem_types.iter()) {
                            self.define(name, ty.clone(), s.span);
                        }
                    }
                } else {
                    self.define(&s.name, s.ty.clone(), s.span);
                }
                // Track Task variables for must-consume enforcement
                if matches!(s.value.kind, ExprKind::Spawn(_)) || matches!(s.ty, Type::Task(_)) {
                    if let Some(tasks) = self.task_vars.last_mut() {
                        tasks.insert(s.name.clone());
                    }
                }
                // Track .join() calls in the value expression
                self.track_join_calls(&s.value);
            }
            Stmt::Assign(s) => {
                self.check_expr_move(&s.value);
                // Re-own the target: assigning a new value to a moved variable is
                // legitimate because it binds a fresh value, restoring ownership.
                // This is intentional — it allows patterns like `x = new_value;`
                // after `x` has been moved.
                if let ExprKind::Ident(name) = &s.target.kind {
                    self.set_state(name, VarState::Owned);
                }
            }
            Stmt::Return(s) => {
                self.check_expr_move(&s.value);
            }
            Stmt::If(s) => {
                self.check_if_stmt(s);
            }
            Stmt::While(s) => {
                self.check_expr_use(&s.condition);
                self.loop_depth += 1;
                self.push_scope();
                self.check_block(&s.body);
                self.pop_scope();
                self.loop_depth -= 1;
            }
            Stmt::For(s) => {
                self.check_expr_use(&s.iterable);
                let elem_ty = self.infer_iterable_elem_type(&s.iterable);
                self.loop_depth += 1;
                self.push_scope();
                self.define(&s.var_name, elem_ty, s.span);
                self.check_block(&s.body);
                self.pop_scope();
                self.loop_depth -= 1;
            }
            Stmt::Match(s) => {
                self.check_expr_use(&s.subject);

                if s.arms.is_empty() {
                    return;
                }

                let snapshot = self.snapshot_states();
                let mut arm_states = Vec::new();

                for arm in &s.arms {
                    self.restore_states(&snapshot);
                    self.push_scope();
                    self.bind_pattern(&arm.pattern);
                    self.check_block(&arm.body);
                    self.pop_scope();
                    arm_states.push(self.snapshot_states());
                }

                self.restore_states(&snapshot);
                self.apply_merged_states(&arm_states);
            }
            Stmt::Expr(s) => {
                self.check_expr_use(&s.expr);
                // Track .join() calls to remove from must-consume
                self.track_join_calls(&s.expr);
            }
            Stmt::Break(_) | Stmt::Continue(_) => {}
        }
    }

    /// Handle if/else-if/else with conservative branch merging.
    fn check_if_stmt(&mut self, s: &IfStmt) {
        self.check_expr_use(&s.condition);

        if let Some(else_branch) = &s.else_branch {
            // Snapshot pre-if state for branch merging
            let snapshot = self.snapshot_states();

            // Check then-block
            self.push_scope();
            self.check_block(&s.then_block);
            self.pop_scope();
            let post_then = self.snapshot_states();

            // Restore pre-if state and check else-block
            self.restore_states(&snapshot);
            self.check_else_branch(else_branch);
            let post_else = self.snapshot_states();

            // Merge: moved in either branch = moved in outer scope
            self.restore_states(&snapshot);
            self.apply_merged_states(&[post_then, post_else]);
        } else {
            // No else: then-block state applies (conservative — conditional move = moved)
            self.push_scope();
            self.check_block(&s.then_block);
            self.pop_scope();
        }
    }

    /// Check the else branch of an if statement (handles else-if chains recursively).
    fn check_else_branch(&mut self, else_branch: &ElseBranch) {
        match else_branch {
            ElseBranch::ElseIf(elif) => {
                self.check_if_stmt(elif);
            }
            ElseBranch::Else(block) => {
                self.push_scope();
                self.check_block(block);
                self.pop_scope();
            }
        }
    }

    /// If the expression is a .join() call, mark the Task as consumed.
    fn track_join_calls(&mut self, expr: &Expr) {
        if let ExprKind::MethodCall(receiver, method, _) = &expr.kind {
            if method == "join" {
                if let ExprKind::Ident(name) = &receiver.kind {
                    for tasks in self.task_vars.iter_mut().rev() {
                        tasks.remove(name);
                    }
                }
            }
        }
    }

    /// Check that an expression's variables are readable (not moved).
    /// Used for read-only contexts: conditions, operands, function args, field access.
    fn check_expr_use(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if self.is_moved(name) {
                    self.errors.push(OwnershipError {
                        message: format!("use of moved variable '{}'", name),
                        span: expr.span,
                    });
                }
            }
            ExprKind::Binary(lhs, _, rhs) => {
                self.check_expr_use(lhs);
                self.check_expr_use(rhs);
            }
            ExprKind::Unary(_, operand) => {
                self.check_expr_use(operand);
            }
            ExprKind::Call(callee, args) => {
                self.check_expr_use(callee);
                for arg in args {
                    self.check_expr_use(arg);
                }
            }
            ExprKind::FieldAccess(obj, _) => {
                self.check_expr_use(obj);
            }
            ExprKind::MethodCall(receiver, _, args) => {
                self.check_expr_use(receiver);
                for arg in args {
                    self.check_expr_use(arg);
                }
            }
            ExprKind::Index(arr, idx) => {
                self.check_expr_use(arr);
                self.check_expr_use(idx);
            }
            ExprKind::StructInit(_, fields) => {
                for fi in fields {
                    self.check_expr_use(&fi.value);
                }
            }
            ExprKind::ArrayLit(elements) | ExprKind::TupleLit(elements) => {
                for elem in elements {
                    self.check_expr_use(elem);
                }
            }
            ExprKind::Closure(closure) => {
                self.push_scope();
                for param in &closure.params {
                    self.define(&param.name, param.ty.clone(), param.span);
                }
                self.check_block(&closure.body);
                self.pop_scope();
            }
            ExprKind::Spawn(block) => {
                self.push_scope();
                self.check_block(block);
                self.pop_scope();
            }
            ExprKind::Range(start, end) | ExprKind::RangeInclusive(start, end) => {
                self.check_expr_use(start);
                self.check_expr_use(end);
            }
            ExprKind::Try(inner) => {
                self.check_expr_use(inner);
            }
            ExprKind::Literal(_) => {}
        }
    }

    /// Check that an expression can be moved out of.
    /// Marks non-copy identifiers as Moved. Falls back to check_expr_use for compound exprs.
    fn check_expr_move(&mut self, expr: &Expr) {
        if let ExprKind::Ident(name) = &expr.kind {
            // Extract needed info before mutable borrow
            let (is_already_moved, should_move) = match self.lookup(name) {
                Some(info) => (info.state == VarState::Moved, !is_copy_type(&info.ty)),
                None => return,
            };

            if is_already_moved {
                self.errors.push(OwnershipError {
                    message: format!("use of moved variable '{}'", name),
                    span: expr.span,
                });
                return;
            }

            if should_move {
                // Cannot move outer-scope variables inside a loop
                if self.loop_depth > 0 && !self.is_in_current_scope(name) {
                    self.errors.push(OwnershipError {
                        message: format!("cannot move '{}' inside a loop", name),
                        span: expr.span,
                    });
                    return;
                }
                self.set_state(name, VarState::Moved);
            }
        } else {
            self.check_expr_use(expr);
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Binding(name, span) => {
                self.define(name, Type::Unit, *span);
            }
            Pattern::Variant(_, sub_pats, _) => {
                for sp in sub_pats {
                    self.bind_pattern(sp);
                }
            }
            _ => {}
        }
    }

    /// Try to infer the element type of a for-loop iterable.
    /// Falls back to Unit (copy) if the type cannot be determined.
    fn infer_iterable_elem_type(&self, iterable: &Expr) -> Type {
        if let ExprKind::Range(_, _) | ExprKind::RangeInclusive(_, _) = &iterable.kind {
            return Type::Int;
        }
        // Handle iterator pipeline chains: .iter().map(f).filter(g)
        // Only treat map/filter as pipeline steps when the chain has .iter()
        // at the base — struct methods named map/filter should fall through
        // to normal array element inference.
        if let ExprKind::MethodCall(receiver, method, args) = &iterable.kind {
            match method.as_str() {
                "map" if Self::has_iter_base(receiver) => {
                    if args.len() == 1 {
                        if let ExprKind::Closure(c) = &args[0].kind {
                            return c.return_type.clone();
                        }
                    }
                }
                "filter" if Self::has_iter_base(receiver) => {
                    return self.infer_iterable_elem_type(receiver);
                }
                "enumerate" if Self::has_iter_base(receiver) => {
                    let inner = self.infer_iterable_elem_type(receiver);
                    return Type::Tuple(vec![Type::Int, inner]);
                }
                "zip" if Self::has_iter_base(receiver) => {
                    let left = self.infer_iterable_elem_type(receiver);
                    if args.len() == 1 {
                        let right = self.infer_iterable_elem_type(&args[0]);
                        return Type::Tuple(vec![left, right]);
                    }
                    return Type::Unit;
                }
                "take" | "skip" if Self::has_iter_base(receiver) => {
                    return self.infer_iterable_elem_type(receiver);
                }
                "iter" => {
                    if let ExprKind::Ident(name) = &receiver.kind {
                        if let Some(info) = self.lookup(name) {
                            if let Type::Array(elem) = &info.ty {
                                return *elem.clone();
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        if let ExprKind::Ident(name) = &iterable.kind {
            if let Some(info) = self.lookup(name) {
                if let Type::Array(elem) = &info.ty {
                    return *elem.clone();
                }
            }
        }
        Type::Unit
    }

    /// Walk a MethodCall chain looking for `.iter()` at the base.
    fn has_iter_base(expr: &Expr) -> bool {
        if let ExprKind::MethodCall(ref receiver, ref method, _) = expr.kind {
            match method.as_str() {
                "iter" => true,
                "map" | "filter" | "enumerate" | "zip" | "take" | "skip" | "reduce" | "fold"
                | "collect" | "find" | "any" | "all" => Self::has_iter_base(receiver),
                _ => false,
            }
        } else {
            false
        }
    }

    // ── Scope management ─────────────────────────────────────

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.task_vars.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        // Check for unconsumed Task variables
        if let Some(tasks) = self.task_vars.pop() {
            for name in &tasks {
                let span = self
                    .scopes
                    .last()
                    .and_then(|scope| scope.get(name))
                    .map(|info| info.def_span)
                    .unwrap_or(Span::synthetic());
                self.errors.push(OwnershipError {
                    message: format!("Task '{}' must be joined before scope exit", name),
                    span,
                });
            }
        }
        self.scopes.pop();
    }

    fn define(&mut self, name: &str, ty: Type, def_span: Span) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name.to_string(),
                VarInfo {
                    state: VarState::Owned,
                    ty,
                    def_span,
                },
            );
        }
    }

    fn lookup(&self, name: &str) -> Option<&VarInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }

    fn set_state(&mut self, name: &str, state: VarState) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                info.state = state;
                return;
            }
        }
    }

    fn is_moved(&self, name: &str) -> bool {
        self.lookup(name)
            .is_some_and(|info| info.state == VarState::Moved)
    }

    /// Returns true if the variable is defined in the innermost (current) scope.
    fn is_in_current_scope(&self, name: &str) -> bool {
        self.scopes
            .last()
            .is_some_and(|scope| scope.contains_key(name))
    }

    // ── State snapshot/restore/merge for branch analysis ─────

    /// Capture the VarState of every variable across all scopes.
    fn snapshot_states(&self) -> Vec<HashMap<String, VarState>> {
        self.scopes
            .iter()
            .map(|scope| {
                scope
                    .iter()
                    .map(|(k, v)| (k.clone(), v.state.clone()))
                    .collect()
            })
            .collect()
    }

    /// Restore variable states from a previous snapshot.
    fn restore_states(&mut self, snapshot: &[HashMap<String, VarState>]) {
        for (scope, snap) in self.scopes.iter_mut().zip(snapshot.iter()) {
            for (name, info) in scope.iter_mut() {
                if let Some(state) = snap.get(name) {
                    info.state = state.clone();
                }
            }
        }
    }

    /// Conservative merge: if a variable is Moved in ANY of the given state snapshots,
    /// set it to Moved in the current scopes.
    fn apply_merged_states(&mut self, states: &[Vec<HashMap<String, VarState>>]) {
        for (i, scope) in self.scopes.iter_mut().enumerate() {
            for (name, info) in scope.iter_mut() {
                for state_snap in states {
                    if let Some(s) = state_snap.get(i).and_then(|s| s.get(name)) {
                        if *s == VarState::Moved {
                            info.state = VarState::Moved;
                            break;
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::lexer::Lexer;
    use crate::compiler::parser::Parser;

    fn check_ownership(input: &str) -> Result<(), Vec<OwnershipError>> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();
        let mut checker = OwnershipChecker::new();
        checker.check_program(&program)
    }

    #[test]
    fn test_valid_ownership() {
        assert!(check_ownership("fn f() -> int { let x: int = 1; return x; }").is_ok());
    }

    #[test]
    fn test_basic_variable_use() {
        assert!(
            check_ownership("fn f() -> int { let x: int = 1; let y: int = x + 1; return y; }")
                .is_ok()
        );
    }
}
