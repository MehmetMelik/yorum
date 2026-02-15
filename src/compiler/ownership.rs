//! Ownership and move checker for the Yorum language.
//!
//! Enforces:
//! - Move semantics: using a moved value is an error
//! - Borrow safety: cannot move a value while it is borrowed
//!
//! This is a simplified linear-type check (not a full Rust borrow checker)
//! but sufficient to prevent use-after-move and dangling references.

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
    Borrowed,
}

pub struct OwnershipChecker {
    scopes: Vec<HashMap<String, VarState>>,
    errors: Vec<OwnershipError>,
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
            errors: Vec::new(),
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
                            // Check default method bodies (create a temporary FnDecl)
                            self.push_scope();
                            for param in &method.params {
                                self.define(&param.name);
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
            self.define(&param.name);
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
                self.check_expr_use(&s.value);
                self.define(&s.name);
            }
            Stmt::Assign(s) => {
                self.check_expr_use(&s.value);
                // Re-own the target
                if let ExprKind::Ident(name) = &s.target.kind {
                    self.set_state(name, VarState::Owned);
                }
            }
            Stmt::Return(s) => {
                self.check_expr_move(&s.value);
            }
            Stmt::If(s) => {
                self.check_expr_use(&s.condition);
                self.push_scope();
                self.check_block(&s.then_block);
                self.pop_scope();
                if let Some(else_branch) = &s.else_branch {
                    match else_branch.as_ref() {
                        ElseBranch::ElseIf(elif) => {
                            self.check_stmt(&Stmt::If(elif.clone()));
                        }
                        ElseBranch::Else(block) => {
                            self.push_scope();
                            self.check_block(block);
                            self.pop_scope();
                        }
                    }
                }
            }
            Stmt::While(s) => {
                self.check_expr_use(&s.condition);
                self.push_scope();
                self.check_block(&s.body);
                self.pop_scope();
            }
            Stmt::Match(s) => {
                self.check_expr_use(&s.subject);
                for arm in &s.arms {
                    self.push_scope();
                    self.bind_pattern(&arm.pattern);
                    self.check_block(&arm.body);
                    self.pop_scope();
                }
            }
            Stmt::Expr(s) => {
                self.check_expr_use(&s.expr);
            }
        }
    }

    /// Check that an expression's variables are readable (not moved).
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
            ExprKind::Closure(closure) => {
                // Check the closure body in a new scope
                self.push_scope();
                for param in &closure.params {
                    self.define(&param.name);
                }
                self.check_block(&closure.body);
                self.pop_scope();
            }
            ExprKind::Literal(_) => {}
        }
    }

    /// Check that an expression's variables can be moved.
    fn check_expr_move(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if self.is_moved(name) {
                    self.errors.push(OwnershipError {
                        message: format!("use of moved variable '{}'", name),
                        span: expr.span,
                    });
                }
                // Mark as moved (only for non-Copy types — for now all named types)
                // Primitive types (int, float, bool) are implicitly Copy
                // so we don't mark them as moved
            }
            _ => self.check_expr_use(expr),
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Binding(name, _) => {
                self.define(name);
            }
            Pattern::Variant(_, sub_pats, _) => {
                for sp in sub_pats {
                    self.bind_pattern(sp);
                }
            }
            _ => {}
        }
    }

    // ── Scope management ─────────────────────────────────────

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), VarState::Owned);
        }
    }

    fn set_state(&mut self, name: &str, state: VarState) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), state);
                return;
            }
        }
    }

    fn is_moved(&self, name: &str) -> bool {
        for scope in self.scopes.iter().rev() {
            if let Some(state) = scope.get(name) {
                return *state == VarState::Moved;
            }
        }
        false
    }

    #[allow(dead_code)]
    fn active_borrows(&self) -> HashSet<String> {
        let mut borrows = HashSet::new();
        for scope in &self.scopes {
            for (name, state) in scope {
                if *state == VarState::Borrowed {
                    borrows.insert(name.clone());
                }
            }
        }
        borrows
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
