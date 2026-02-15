use crate::compiler::ast::*;
use crate::compiler::span::Span;
use std::collections::HashMap;
use std::fmt;

// ═══════════════════════════════════════════════════════════════
//  Error type
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type error at {}: {}", self.span, self.message)
    }
}

impl std::error::Error for TypeError {}

// ═══════════════════════════════════════════════════════════════
//  Type environment
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
struct FnSig {
    params: Vec<Type>,
    ret: Type,
    #[allow(dead_code)]
    is_pure: bool,
}

#[derive(Debug, Clone)]
struct StructInfo {
    fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
struct EnumInfo {
    variants: Vec<(String, Vec<Type>)>,
}

#[derive(Debug, Clone)]
struct VarInfo {
    ty: Type,
    is_mut: bool,
}

// ═══════════════════════════════════════════════════════════════
//  Type checker
// ═══════════════════════════════════════════════════════════════

pub struct TypeChecker {
    scopes: Vec<HashMap<String, VarInfo>>,
    functions: HashMap<String, FnSig>,
    structs: HashMap<String, StructInfo>,
    enums: HashMap<String, EnumInfo>,
    current_fn_ret: Option<Type>,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut checker = Self {
            scopes: Vec::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            current_fn_ret: None,
            errors: Vec::new(),
        };
        checker.register_builtins();
        checker
    }

    fn register_builtins(&mut self) {
        // Built-in print functions
        self.functions.insert(
            "print_int".to_string(),
            FnSig {
                params: vec![Type::Int],
                ret: Type::Unit,
                is_pure: false,
            },
        );
        self.functions.insert(
            "print_float".to_string(),
            FnSig {
                params: vec![Type::Float],
                ret: Type::Unit,
                is_pure: false,
            },
        );
        self.functions.insert(
            "print_bool".to_string(),
            FnSig {
                params: vec![Type::Bool],
                ret: Type::Unit,
                is_pure: false,
            },
        );
        self.functions.insert(
            "print_str".to_string(),
            FnSig {
                params: vec![Type::Str],
                ret: Type::Unit,
                is_pure: false,
            },
        );
    }

    /// Type-check an entire program. Returns Ok(()) or collected errors.
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<TypeError>> {
        // Pass 1: register all top-level declarations
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) => self.register_function(f),
                Declaration::Struct(s) => self.register_struct(s),
                Declaration::Enum(e) => self.register_enum(e),
                Declaration::Const(_) => {} // handled in pass 2
            }
        }

        // Pass 2: check function bodies and const values
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) => self.check_function(f),
                Declaration::Const(c) => self.check_const(c),
                _ => {}
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    // ── Registration ─────────────────────────────────────────

    fn register_function(&mut self, f: &FnDecl) {
        let sig = FnSig {
            params: f.params.iter().map(|p| p.ty.clone()).collect(),
            ret: f.return_type.clone(),
            is_pure: f.is_pure,
        };
        self.functions.insert(f.name.clone(), sig);
    }

    fn register_struct(&mut self, s: &StructDecl) {
        let info = StructInfo {
            fields: s.fields.iter().map(|f| (f.name.clone(), f.ty.clone())).collect(),
        };
        self.structs.insert(s.name.clone(), info);
    }

    fn register_enum(&mut self, e: &EnumDecl) {
        let info = EnumInfo {
            variants: e
                .variants
                .iter()
                .map(|v| (v.name.clone(), v.fields.clone()))
                .collect(),
        };
        self.enums.insert(e.name.clone(), info);
    }

    // ── Checking ─────────────────────────────────────────────

    fn check_function(&mut self, f: &FnDecl) {
        self.current_fn_ret = Some(f.return_type.clone());
        self.push_scope();

        // Bind parameters
        for param in &f.params {
            if !self.is_valid_type(&param.ty) {
                self.errors.push(TypeError {
                    message: format!("unknown type '{}'", param.ty),
                    span: param.span,
                });
            }
            self.define(&param.name, param.ty.clone(), false);
        }

        // Bind "result" for ensures clauses
        self.define("result", f.return_type.clone(), false);

        self.check_block(&f.body);
        self.pop_scope();
        self.current_fn_ret = None;
    }

    fn check_const(&mut self, c: &ConstDecl) {
        self.push_scope();
        let ty = self.infer_expr(&c.value);
        if let Some(ty) = ty {
            if ty != c.ty {
                self.errors.push(TypeError {
                    message: format!(
                        "const '{}' declared as '{}' but initialized with '{}'",
                        c.name, c.ty, ty
                    ),
                    span: c.span,
                });
            }
        }
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
                if !self.is_valid_type(&s.ty) {
                    self.errors.push(TypeError {
                        message: format!("unknown type '{}'", s.ty),
                        span: s.span,
                    });
                }
                if let Some(val_ty) = self.infer_expr(&s.value) {
                    if val_ty != s.ty {
                        self.errors.push(TypeError {
                            message: format!(
                                "cannot assign '{}' to variable of type '{}'",
                                val_ty, s.ty
                            ),
                            span: s.span,
                        });
                    }
                }
                self.define(&s.name, s.ty.clone(), s.is_mut);
            }

            Stmt::Assign(s) => {
                // Check that target is mutable
                if let ExprKind::Ident(name) = &s.target.kind {
                    if let Some(info) = self.lookup(name) {
                        if !info.is_mut {
                            self.errors.push(TypeError {
                                message: format!(
                                    "cannot assign to immutable variable '{}'",
                                    name
                                ),
                                span: s.span,
                            });
                        }
                    }
                }

                let target_ty = self.infer_expr(&s.target);
                let value_ty = self.infer_expr(&s.value);
                if let (Some(tt), Some(vt)) = (target_ty, value_ty) {
                    if tt != vt {
                        self.errors.push(TypeError {
                            message: format!("cannot assign '{}' to '{}'", vt, tt),
                            span: s.span,
                        });
                    }
                }
            }

            Stmt::Return(s) => {
                if let Some(expected) = &self.current_fn_ret {
                    let expected = expected.clone();
                    if let Some(actual) = self.infer_expr(&s.value) {
                        if actual != expected {
                            self.errors.push(TypeError {
                                message: format!(
                                    "return type mismatch: expected '{}', found '{}'",
                                    expected, actual
                                ),
                                span: s.span,
                            });
                        }
                    }
                }
            }

            Stmt::If(s) => {
                if let Some(cond_ty) = self.infer_expr(&s.condition) {
                    if cond_ty != Type::Bool {
                        self.errors.push(TypeError {
                            message: format!(
                                "if condition must be 'bool', found '{}'",
                                cond_ty
                            ),
                            span: s.condition.span,
                        });
                    }
                }
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
                if let Some(cond_ty) = self.infer_expr(&s.condition) {
                    if cond_ty != Type::Bool {
                        self.errors.push(TypeError {
                            message: format!(
                                "while condition must be 'bool', found '{}'",
                                cond_ty
                            ),
                            span: s.condition.span,
                        });
                    }
                }
                self.push_scope();
                self.check_block(&s.body);
                self.pop_scope();
            }

            Stmt::Match(s) => {
                let _subject_ty = self.infer_expr(&s.subject);
                for arm in &s.arms {
                    self.push_scope();
                    self.check_pattern(&arm.pattern);
                    self.check_block(&arm.body);
                    self.pop_scope();
                }
            }

            Stmt::Expr(s) => {
                self.infer_expr(&s.expr);
            }
        }
    }

    fn check_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard(_) => {}
            Pattern::Binding(name, _) => {
                // Bind as unknown type for now — full inference would require the match subject type
                self.define(name, Type::Int, false);
            }
            Pattern::Literal(_, _) => {}
            Pattern::Variant(name, sub_patterns, span) => {
                // Clone enums to avoid borrow conflict with self.define()
                let enums_snapshot: Vec<_> = self
                    .enums
                    .values()
                    .flat_map(|info| info.variants.clone())
                    .collect();

                let mut found = false;
                for (vname, vfields) in &enums_snapshot {
                    if vname == name {
                        found = true;
                        if sub_patterns.len() != vfields.len() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "variant '{}' expects {} fields, found {}",
                                    name,
                                    vfields.len(),
                                    sub_patterns.len()
                                ),
                                span: *span,
                            });
                        }
                        for (i, sp) in sub_patterns.iter().enumerate() {
                            if let Pattern::Binding(bname, _) = sp {
                                if let Some(ty) = vfields.get(i) {
                                    self.define(bname, ty.clone(), false);
                                }
                            }
                        }
                    }
                }
                if !found {
                    self.errors.push(TypeError {
                        message: format!("unknown variant '{}'", name),
                        span: *span,
                    });
                }
            }
        }
    }

    // ── Expression type inference ────────────────────────────

    fn infer_expr(&mut self, expr: &Expr) -> Option<Type> {
        match &expr.kind {
            ExprKind::Literal(lit) => Some(match lit {
                Literal::Int(_) => Type::Int,
                Literal::Float(_) => Type::Float,
                Literal::Bool(_) => Type::Bool,
                Literal::String(_) => Type::Str,
            }),

            ExprKind::Ident(name) => {
                if let Some(info) = self.lookup(name) {
                    Some(info.ty.clone())
                } else {
                    self.errors.push(TypeError {
                        message: format!("undefined variable '{}'", name),
                        span: expr.span,
                    });
                    None
                }
            }

            ExprKind::Binary(lhs, op, rhs) => {
                let lt = self.infer_expr(lhs)?;
                let rt = self.infer_expr(rhs)?;
                self.check_binary_op(*op, &lt, &rt, expr.span)
            }

            ExprKind::Unary(op, operand) => {
                let ty = self.infer_expr(operand)?;
                match op {
                    UnaryOp::Neg => {
                        if ty != Type::Int && ty != Type::Float {
                            self.errors.push(TypeError {
                                message: format!("cannot negate type '{}'", ty),
                                span: expr.span,
                            });
                            None
                        } else {
                            Some(ty)
                        }
                    }
                    UnaryOp::Not => {
                        if ty != Type::Bool {
                            self.errors.push(TypeError {
                                message: format!("'not' requires 'bool', found '{}'", ty),
                                span: expr.span,
                            });
                            None
                        } else {
                            Some(Type::Bool)
                        }
                    }
                }
            }

            ExprKind::Call(callee, args) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    if let Some(sig) = self.functions.get(name).cloned() {
                        if args.len() != sig.params.len() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'{}' expects {} arguments, found {}",
                                    name,
                                    sig.params.len(),
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        for (i, arg) in args.iter().enumerate() {
                            if let Some(arg_ty) = self.infer_expr(arg) {
                                if arg_ty != sig.params[i] {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "argument {} of '{}': expected '{}', found '{}'",
                                            i + 1,
                                            name,
                                            sig.params[i],
                                            arg_ty
                                        ),
                                        span: arg.span,
                                    });
                                }
                            }
                        }
                        Some(sig.ret)
                    } else {
                        // Check if it's an enum variant constructor
                        for (_enum_name, info) in &self.enums {
                            for (vname, vfields) in &info.variants {
                                if vname == name && !vfields.is_empty() {
                                    // For now, return the enum as Named type
                                    return Some(Type::Named(_enum_name.clone()));
                                }
                            }
                        }
                        self.errors.push(TypeError {
                            message: format!("undefined function '{}'", name),
                            span: expr.span,
                        });
                        None
                    }
                } else {
                    // Indirect call — not supported in v1
                    self.errors.push(TypeError {
                        message: "indirect function calls are not yet supported".to_string(),
                        span: expr.span,
                    });
                    None
                }
            }

            ExprKind::FieldAccess(obj, field) => {
                let obj_ty = self.infer_expr(obj)?;
                if let Type::Named(struct_name) = &obj_ty {
                    if let Some(info) = self.structs.get(struct_name).cloned() {
                        if let Some((_, fty)) = info.fields.iter().find(|(n, _)| n == field) {
                            Some(fty.clone())
                        } else {
                            self.errors.push(TypeError {
                                message: format!(
                                    "struct '{}' has no field '{}'",
                                    struct_name, field
                                ),
                                span: expr.span,
                            });
                            None
                        }
                    } else {
                        self.errors.push(TypeError {
                            message: format!("'{}' is not a struct type", struct_name),
                            span: expr.span,
                        });
                        None
                    }
                } else {
                    self.errors.push(TypeError {
                        message: format!(
                            "field access requires a struct type, found '{}'",
                            obj_ty
                        ),
                        span: expr.span,
                    });
                    None
                }
            }

            ExprKind::Index(arr, idx) => {
                let arr_ty = self.infer_expr(arr)?;
                let idx_ty = self.infer_expr(idx)?;
                if idx_ty != Type::Int {
                    self.errors.push(TypeError {
                        message: format!("index must be 'int', found '{}'", idx_ty),
                        span: idx.span,
                    });
                }
                if let Type::Array(inner) = arr_ty {
                    Some(*inner)
                } else {
                    self.errors.push(TypeError {
                        message: format!("cannot index into type '{}'", arr_ty),
                        span: expr.span,
                    });
                    None
                }
            }

            ExprKind::StructInit(name, fields) => {
                if let Some(info) = self.structs.get(name).cloned() {
                    // Check that all fields are provided
                    for (expected_name, expected_ty) in &info.fields {
                        if let Some(fi) = fields.iter().find(|f| &f.name == expected_name) {
                            if let Some(actual_ty) = self.infer_expr(&fi.value) {
                                if actual_ty != *expected_ty {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "field '{}' of '{}': expected '{}', found '{}'",
                                            expected_name, name, expected_ty, actual_ty
                                        ),
                                        span: fi.span,
                                    });
                                }
                            }
                        } else {
                            self.errors.push(TypeError {
                                message: format!(
                                    "missing field '{}' in struct '{}' initializer",
                                    expected_name, name
                                ),
                                span: expr.span,
                            });
                        }
                    }
                    // Check for extra fields
                    for fi in fields {
                        if !info.fields.iter().any(|(n, _)| n == &fi.name) {
                            self.errors.push(TypeError {
                                message: format!(
                                    "unknown field '{}' in struct '{}'",
                                    fi.name, name
                                ),
                                span: fi.span,
                            });
                        }
                    }
                    Some(Type::Named(name.clone()))
                } else {
                    self.errors.push(TypeError {
                        message: format!("undefined struct '{}'", name),
                        span: expr.span,
                    });
                    None
                }
            }
        }
    }

    fn check_binary_op(
        &mut self,
        op: BinOp,
        lt: &Type,
        rt: &Type,
        span: Span,
    ) -> Option<Type> {
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if lt != rt {
                    self.errors.push(TypeError {
                        message: format!(
                            "operands of '{}' must have same type, found '{}' and '{}'",
                            op, lt, rt
                        ),
                        span,
                    });
                    return None;
                }
                if *lt != Type::Int && *lt != Type::Float {
                    self.errors.push(TypeError {
                        message: format!(
                            "arithmetic operator '{}' requires 'int' or 'float', found '{}'",
                            op, lt
                        ),
                        span,
                    });
                    return None;
                }
                Some(lt.clone())
            }
            BinOp::Eq | BinOp::NotEq => {
                if lt != rt {
                    self.errors.push(TypeError {
                        message: format!(
                            "cannot compare '{}' and '{}' for equality",
                            lt, rt
                        ),
                        span,
                    });
                    return None;
                }
                Some(Type::Bool)
            }
            BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                if lt != rt {
                    self.errors.push(TypeError {
                        message: format!(
                            "cannot compare '{}' and '{}'",
                            lt, rt
                        ),
                        span,
                    });
                    return None;
                }
                if *lt != Type::Int && *lt != Type::Float {
                    self.errors.push(TypeError {
                        message: format!(
                            "comparison requires 'int' or 'float', found '{}'",
                            lt
                        ),
                        span,
                    });
                    return None;
                }
                Some(Type::Bool)
            }
            BinOp::And | BinOp::Or => {
                if *lt != Type::Bool || *rt != Type::Bool {
                    self.errors.push(TypeError {
                        message: format!(
                            "'{}' requires 'bool' operands, found '{}' and '{}'",
                            op, lt, rt
                        ),
                        span,
                    });
                    return None;
                }
                Some(Type::Bool)
            }
        }
    }

    // ── Scope management ─────────────────────────────────────

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &str, ty: Type, is_mut: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), VarInfo { ty, is_mut });
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

    fn is_valid_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Int | Type::Float | Type::Bool | Type::Str | Type::Unit => true,
            Type::Named(name) => self.structs.contains_key(name) || self.enums.contains_key(name),
            Type::Array(inner) => self.is_valid_type(inner),
            Type::Ref(inner) | Type::MutRef(inner) | Type::Own(inner) => self.is_valid_type(inner),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::lexer::Lexer;
    use crate::compiler::parser::Parser;

    fn check(input: &str) -> Result<(), Vec<TypeError>> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();
        let mut checker = TypeChecker::new();
        checker.check_program(&program)
    }

    #[test]
    fn test_valid_program() {
        assert!(check("fn main() -> int { return 0; }").is_ok());
    }

    #[test]
    fn test_type_mismatch() {
        let result = check("fn f() -> int { return true; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_undefined_variable() {
        let result = check("fn f() -> int { return x; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_arithmetic_type_check() {
        assert!(check("fn f() -> int { let x: int = 1 + 2; return x; }").is_ok());
    }

    #[test]
    fn test_bool_in_arithmetic() {
        let result = check("fn f() -> int { return true + 1; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_immutable_assign() {
        let result = check("fn f() -> int { let x: int = 1; x = 2; return x; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_mutable_assign() {
        assert!(check("fn f() -> int { let mut x: int = 1; x = 2; return x; }").is_ok());
    }

    #[test]
    fn test_struct_type_check() {
        assert!(check(
            "struct Point { x: int, y: int } fn f() -> Point { return Point { x: 1, y: 2 }; }"
        )
        .is_ok());
    }
}
