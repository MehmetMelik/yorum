use crate::compiler::ast::*;
use crate::compiler::span::Span;
use std::collections::{HashMap, HashSet};
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
    type_params: Vec<String>,
}

#[derive(Debug, Clone)]
struct StructInfo {
    fields: Vec<(String, Type)>,
    type_params: Vec<String>,
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

#[derive(Debug, Clone)]
struct MethodInfo {
    mangled_name: String,
    sig: FnSig,
    #[allow(dead_code)]
    self_param_ty: Type,
}

#[derive(Debug, Clone)]
struct TraitInfo {
    methods: Vec<(String, FnSig)>,
}

// ═══════════════════════════════════════════════════════════════
//  Type checker
// ═══════════════════════════════════════════════════════════════

pub struct TypeChecker {
    scopes: Vec<HashMap<String, VarInfo>>,
    functions: HashMap<String, FnSig>,
    structs: HashMap<String, StructInfo>,
    enums: HashMap<String, EnumInfo>,
    methods: HashMap<String, Vec<MethodInfo>>,
    traits: HashMap<String, TraitInfo>,
    trait_impls: HashMap<(String, String), bool>,
    type_param_scope: Vec<HashSet<String>>,
    current_self_type: Option<Type>,
    current_fn_ret: Option<Type>,
    errors: Vec<TypeError>,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut checker = Self {
            scopes: Vec::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            methods: HashMap::new(),
            traits: HashMap::new(),
            trait_impls: HashMap::new(),
            type_param_scope: Vec::new(),
            current_self_type: None,
            current_fn_ret: None,
            errors: Vec::new(),
        };
        checker.register_builtins();
        checker
    }

    fn register_builtins(&mut self) {
        let builtin = |params: Vec<Type>| FnSig {
            params,
            ret: Type::Unit,
            is_pure: false,
            type_params: Vec::new(),
        };
        self.functions
            .insert("print_int".to_string(), builtin(vec![Type::Int]));
        self.functions
            .insert("print_float".to_string(), builtin(vec![Type::Float]));
        self.functions
            .insert("print_bool".to_string(), builtin(vec![Type::Bool]));
        self.functions
            .insert("print_str".to_string(), builtin(vec![Type::Str]));
    }

    /// Type-check an entire program. Returns Ok(()) or collected errors.
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<TypeError>> {
        // Pass 1: register all top-level declarations
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) => self.register_function(f),
                Declaration::Struct(s) => self.register_struct(s),
                Declaration::Enum(e) => self.register_enum(e),
                Declaration::Impl(i) => self.register_impl(i),
                Declaration::Trait(t) => self.register_trait(t),
                Declaration::Const(_) => {} // handled in pass 2
            }
        }

        // Pass 2: check function bodies, const values, and impl method bodies
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) => self.check_function(f),
                Declaration::Const(c) => self.check_const(c),
                Declaration::Impl(i) => {
                    self.current_self_type = Some(Type::Named(i.target_type.clone()));
                    for method in &i.methods {
                        self.check_function(method);
                    }
                    self.current_self_type = None;
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

    // ── Registration ─────────────────────────────────────────

    fn register_function(&mut self, f: &FnDecl) {
        let sig = FnSig {
            params: f.params.iter().map(|p| p.ty.clone()).collect(),
            ret: f.return_type.clone(),
            is_pure: f.is_pure,
            type_params: f.type_params.iter().map(|tp| tp.name.clone()).collect(),
        };
        self.functions.insert(f.name.clone(), sig);
    }

    fn register_struct(&mut self, s: &StructDecl) {
        let info = StructInfo {
            fields: s
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect(),
            type_params: s.type_params.iter().map(|tp| tp.name.clone()).collect(),
        };
        self.structs.insert(s.name.clone(), info);
    }

    fn register_trait(&mut self, t: &TraitDecl) {
        let methods: Vec<(String, FnSig)> = t
            .methods
            .iter()
            .map(|m| {
                let sig = FnSig {
                    params: m.params.iter().map(|p| p.ty.clone()).collect(),
                    ret: m.return_type.clone(),
                    is_pure: false,
                    type_params: Vec::new(),
                };
                (m.name.clone(), sig)
            })
            .collect();
        self.traits.insert(t.name.clone(), TraitInfo { methods });
    }

    fn register_impl(&mut self, i: &ImplDecl) {
        // Resolve Self to the target type for sig registration
        let self_ty = Type::Named(i.target_type.clone());

        for method in &i.methods {
            let mangled = format!("{}_{}", i.target_type, method.name);
            let sig = FnSig {
                params: method
                    .params
                    .iter()
                    .map(|p| self.resolve_self_type(&p.ty, &self_ty))
                    .collect(),
                ret: self.resolve_self_type(&method.return_type, &self_ty),
                is_pure: method.is_pure,
                type_params: method
                    .type_params
                    .iter()
                    .map(|tp| tp.name.clone())
                    .collect(),
            };
            let self_param_ty = if let Some(p) = method.params.first() {
                self.resolve_self_type(&p.ty, &self_ty)
            } else {
                self_ty.clone()
            };
            let info = MethodInfo {
                mangled_name: mangled.clone(),
                sig: sig.clone(),
                self_param_ty,
            };
            self.methods
                .entry(i.target_type.clone())
                .or_default()
                .push(info);
            self.functions.insert(mangled, sig);
        }

        // If this is a trait impl, verify all required methods are present
        if let Some(trait_name) = &i.trait_name {
            if let Some(trait_info) = self.traits.get(trait_name).cloned() {
                let impl_method_names: Vec<&str> =
                    i.methods.iter().map(|m| m.name.as_str()).collect();
                for (required_name, _) in &trait_info.methods {
                    if !impl_method_names.contains(&required_name.as_str()) {
                        self.errors.push(TypeError {
                            message: format!(
                                "impl of '{}' for '{}' is missing method '{}'",
                                trait_name, i.target_type, required_name
                            ),
                            span: i.span,
                        });
                    }
                }
            }
            self.trait_impls
                .insert((trait_name.clone(), i.target_type.clone()), true);
        }
    }

    fn resolve_self_type(&self, ty: &Type, concrete: &Type) -> Type {
        match ty {
            Type::SelfType => concrete.clone(),
            Type::Ref(inner) => Type::Ref(Box::new(self.resolve_self_type(inner, concrete))),
            Type::MutRef(inner) => Type::MutRef(Box::new(self.resolve_self_type(inner, concrete))),
            Type::Own(inner) => Type::Own(Box::new(self.resolve_self_type(inner, concrete))),
            Type::Array(inner) => Type::Array(Box::new(self.resolve_self_type(inner, concrete))),
            Type::Fn(params, ret) => Type::Fn(
                params
                    .iter()
                    .map(|p| self.resolve_self_type(p, concrete))
                    .collect(),
                Box::new(self.resolve_self_type(ret, concrete)),
            ),
            other => other.clone(),
        }
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
        // Push type params scope for generic functions
        if !f.type_params.is_empty() {
            let tps: HashSet<String> = f.type_params.iter().map(|tp| tp.name.clone()).collect();
            self.type_param_scope.push(tps);
        }

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

        if !f.type_params.is_empty() {
            self.type_param_scope.pop();
        }
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
                    // For generic types, compare base name
                    let compatible = match (&s.ty, &val_ty) {
                        (Type::Generic(name, _), Type::Named(vname)) => name == vname,
                        _ => val_ty == s.ty,
                    };
                    if !compatible {
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
                                message: format!("cannot assign to immutable variable '{}'", name),
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
                            message: format!("if condition must be 'bool', found '{}'", cond_ty),
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
                            message: format!("while condition must be 'bool', found '{}'", cond_ty),
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

                        // For generic functions, infer type args and substitute
                        if !sig.type_params.is_empty() {
                            let mut bindings: HashMap<String, Type> = HashMap::new();
                            for (i, arg) in args.iter().enumerate() {
                                if let Some(arg_ty) = self.infer_expr(arg) {
                                    // If param is a type param name (Named("T")), bind it
                                    if let Type::Named(ref pname) = sig.params[i] {
                                        if sig.type_params.contains(pname) {
                                            bindings.insert(pname.clone(), arg_ty);
                                        }
                                    }
                                }
                            }
                            // Substitute return type
                            let ret = self.substitute_type_vars(&sig.ret, &bindings);
                            return Some(ret);
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
                        // Check if it's a local variable with function type (indirect call)
                        if let Some(var_info) = self.lookup(name) {
                            let var_ty = var_info.ty.clone();
                            if let Type::Fn(param_types, ret_type) = var_ty {
                                if args.len() != param_types.len() {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "'{}' expects {} arguments, found {}",
                                            name,
                                            param_types.len(),
                                            args.len()
                                        ),
                                        span: expr.span,
                                    });
                                    return None;
                                }
                                for (i, arg) in args.iter().enumerate() {
                                    if let Some(arg_ty) = self.infer_expr(arg) {
                                        if arg_ty != param_types[i] {
                                            self.errors.push(TypeError {
                                                message: format!(
                                                    "argument {} of '{}': expected '{}', found '{}'",
                                                    i + 1,
                                                    name,
                                                    param_types[i],
                                                    arg_ty
                                                ),
                                                span: arg.span,
                                            });
                                        }
                                    }
                                }
                                return Some(*ret_type);
                            }
                        }
                        self.errors.push(TypeError {
                            message: format!("undefined function '{}'", name),
                            span: expr.span,
                        });
                        None
                    }
                } else {
                    // Indirect call — callee must be a Fn type
                    let callee_ty = self.infer_expr(callee)?;
                    if let Type::Fn(param_types, ret_type) = callee_ty {
                        if args.len() != param_types.len() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "closure expects {} arguments, found {}",
                                    param_types.len(),
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        for (i, arg) in args.iter().enumerate() {
                            if let Some(arg_ty) = self.infer_expr(arg) {
                                if arg_ty != param_types[i] {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "argument {}: expected '{}', found '{}'",
                                            i + 1,
                                            param_types[i],
                                            arg_ty
                                        ),
                                        span: arg.span,
                                    });
                                }
                            }
                        }
                        Some(*ret_type)
                    } else {
                        self.errors.push(TypeError {
                            message: format!("'{}' is not callable", callee_ty),
                            span: expr.span,
                        });
                        None
                    }
                }
            }

            ExprKind::MethodCall(receiver, method_name, args) => {
                let recv_ty = self.infer_expr(receiver)?;
                let type_name = match &recv_ty {
                    Type::Named(n) => n.clone(),
                    Type::Ref(inner) | Type::MutRef(inner) => {
                        if let Type::Named(n) = inner.as_ref() {
                            n.clone()
                        } else {
                            self.errors.push(TypeError {
                                message: format!(
                                    "method call requires a struct type, found '{}'",
                                    recv_ty
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                    }
                    _ => {
                        self.errors.push(TypeError {
                            message: format!(
                                "method call requires a struct type, found '{}'",
                                recv_ty
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                };

                let methods = self.methods.get(&type_name).cloned().unwrap_or_default();
                let method = methods.iter().find(|m| {
                    // Extract method name from mangled name: TypeName_method
                    let prefix = format!("{}_", type_name);
                    m.mangled_name.strip_prefix(&prefix) == Some(method_name)
                });

                if let Some(method) = method {
                    let sig = method.sig.clone();
                    // sig.params includes the self param; external args exclude self
                    let expected_args = sig.params.len() - 1;
                    if args.len() != expected_args {
                        self.errors.push(TypeError {
                            message: format!(
                                "method '{}' expects {} arguments, found {}",
                                method_name,
                                expected_args,
                                args.len()
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(arg_ty) = self.infer_expr(arg) {
                            if arg_ty != sig.params[i + 1] {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "argument {} of '{}': expected '{}', found '{}'",
                                        i + 1,
                                        method_name,
                                        sig.params[i + 1],
                                        arg_ty
                                    ),
                                    span: arg.span,
                                });
                            }
                        }
                    }
                    Some(sig.ret)
                } else {
                    self.errors.push(TypeError {
                        message: format!(
                            "no method '{}' found for type '{}'",
                            method_name, type_name
                        ),
                        span: expr.span,
                    });
                    None
                }
            }

            ExprKind::FieldAccess(obj, field) => {
                let obj_ty = self.infer_expr(obj)?;
                // Auto-deref through references
                let obj_ty = match &obj_ty {
                    Type::Ref(inner) | Type::MutRef(inner) => inner.as_ref().clone(),
                    other => other.clone(),
                };
                // Extract struct name and optional type args
                let (struct_name, type_args) = match &obj_ty {
                    Type::Named(name) => (name.clone(), None),
                    Type::Generic(name, args) => (name.clone(), Some(args.clone())),
                    _ => {
                        self.errors.push(TypeError {
                            message: format!(
                                "field access requires a struct type, found '{}'",
                                obj_ty
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                };
                if let Some(info) = self.structs.get(&struct_name).cloned() {
                    if let Some((_, fty)) = info.fields.iter().find(|(n, _)| n == field) {
                        // If we have type args, substitute type params in the field type
                        if let Some(args) = type_args {
                            let subst: HashMap<String, Type> = info
                                .type_params
                                .iter()
                                .zip(args.iter())
                                .map(|(p, a)| (p.clone(), a.clone()))
                                .collect();
                            Some(self.substitute_type_vars(fty, &subst))
                        } else {
                            Some(fty.clone())
                        }
                    } else {
                        self.errors.push(TypeError {
                            message: format!("struct '{}' has no field '{}'", struct_name, field),
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

            ExprKind::Closure(closure) => {
                self.push_scope();
                for param in &closure.params {
                    if !self.is_valid_type(&param.ty) {
                        self.errors.push(TypeError {
                            message: format!("unknown type '{}'", param.ty),
                            span: param.span,
                        });
                    }
                    self.define(&param.name, param.ty.clone(), false);
                }
                let prev_ret = self.current_fn_ret.clone();
                self.current_fn_ret = Some(closure.return_type.clone());
                self.check_block(&closure.body);
                self.current_fn_ret = prev_ret;
                self.pop_scope();

                let param_types: Vec<Type> = closure.params.iter().map(|p| p.ty.clone()).collect();
                Some(Type::Fn(param_types, Box::new(closure.return_type.clone())))
            }

            ExprKind::StructInit(name, fields) => {
                if let Some(info) = self.structs.get(name).cloned() {
                    // Check that all fields are provided
                    for (expected_name, expected_ty) in &info.fields {
                        if let Some(fi) = fields.iter().find(|f| &f.name == expected_name) {
                            if let Some(actual_ty) = self.infer_expr(&fi.value) {
                                // Skip type check if the expected type is a type parameter
                                let is_type_param = if let Type::Named(ref tname) = expected_ty {
                                    info.type_params.contains(tname)
                                } else {
                                    false
                                };
                                if !is_type_param && actual_ty != *expected_ty {
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

    fn check_binary_op(&mut self, op: BinOp, lt: &Type, rt: &Type, span: Span) -> Option<Type> {
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
                        message: format!("cannot compare '{}' and '{}' for equality", lt, rt),
                        span,
                    });
                    return None;
                }
                Some(Type::Bool)
            }
            BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                if lt != rt {
                    self.errors.push(TypeError {
                        message: format!("cannot compare '{}' and '{}'", lt, rt),
                        span,
                    });
                    return None;
                }
                if *lt != Type::Int && *lt != Type::Float {
                    self.errors.push(TypeError {
                        message: format!("comparison requires 'int' or 'float', found '{}'", lt),
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

    fn is_type_param(&self, name: &str) -> bool {
        for scope in self.type_param_scope.iter().rev() {
            if scope.contains(name) {
                return true;
            }
        }
        false
    }

    fn substitute_type_vars(&self, ty: &Type, bindings: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Named(name) if bindings.contains_key(name) => bindings[name].clone(),
            Type::TypeVar(name) if bindings.contains_key(name) => bindings[name].clone(),
            Type::Ref(inner) => Type::Ref(Box::new(self.substitute_type_vars(inner, bindings))),
            Type::MutRef(inner) => {
                Type::MutRef(Box::new(self.substitute_type_vars(inner, bindings)))
            }
            Type::Array(inner) => Type::Array(Box::new(self.substitute_type_vars(inner, bindings))),
            Type::Fn(params, ret) => Type::Fn(
                params
                    .iter()
                    .map(|p| self.substitute_type_vars(p, bindings))
                    .collect(),
                Box::new(self.substitute_type_vars(ret, bindings)),
            ),
            other => other.clone(),
        }
    }

    fn is_valid_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Int | Type::Float | Type::Bool | Type::Str | Type::Unit => true,
            Type::Named(name) => {
                self.structs.contains_key(name)
                    || self.enums.contains_key(name)
                    || self.is_type_param(name)
            }
            Type::Array(inner) => self.is_valid_type(inner),
            Type::Ref(inner) | Type::MutRef(inner) | Type::Own(inner) => self.is_valid_type(inner),
            Type::SelfType => self.current_self_type.is_some(),
            Type::TypeVar(_) => true,
            Type::Generic(name, _) => {
                self.structs.contains_key(name) || self.enums.contains_key(name)
            }
            Type::Fn(_, _) => true,
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
