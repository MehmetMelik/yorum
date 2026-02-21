//! Monomorphization pass for generic functions and structs.
//!
//! Walks the AST collecting all concrete instantiations of generic functions/structs,
//! then clones declarations with type variables substituted by concrete types.
//! Output is a Program with no generics — only concrete declarations.

use crate::compiler::ast::*;
use crate::compiler::span::Span;
use std::collections::{HashMap, HashSet};

pub fn monomorphize(program: Program) -> Program {
    let mut mono = Monomorphizer::new(&program);
    mono.collect_instantiations(&program);
    mono.emit(program)
}

struct Monomorphizer {
    /// Generic function declarations (name → decl)
    generic_fns: HashMap<String, FnDecl>,
    /// Generic struct declarations (name → decl)
    generic_structs: HashMap<String, StructDecl>,
    /// Generic enum declarations (name → decl)
    generic_enums: HashMap<String, EnumDecl>,
    /// Collected instantiations: (generic_name, concrete_type_args)
    fn_instantiations: HashSet<(String, Vec<Type>)>,
    struct_instantiations: HashSet<(String, Vec<Type>)>,
    enum_instantiations: HashSet<(String, Vec<Type>)>,
}

impl Monomorphizer {
    fn new(program: &Program) -> Self {
        let mut generic_fns = HashMap::new();
        let mut generic_structs = HashMap::new();
        let mut generic_enums = HashMap::new();

        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) if !f.type_params.is_empty() => {
                    generic_fns.insert(f.name.clone(), f.clone());
                }
                Declaration::Struct(s) if !s.type_params.is_empty() => {
                    generic_structs.insert(s.name.clone(), s.clone());
                }
                Declaration::Enum(e) if !e.type_params.is_empty() => {
                    generic_enums.insert(e.name.clone(), e.clone());
                }
                Declaration::Impl(i) => {
                    for method in &i.methods {
                        if !method.type_params.is_empty() {
                            let mangled = format!("{}_{}", i.target_type, method.name);
                            generic_fns.insert(mangled, method.clone());
                        }
                    }
                }
                _ => {}
            }
        }

        // Register prelude generic enums (Option<T>, Result<T, E>)
        // These are not declared in the source, but need monomorphization.
        if !generic_enums.contains_key("Option") {
            generic_enums.insert(
                "Option".to_string(),
                EnumDecl {
                    name: "Option".to_string(),
                    is_pub: true,
                    type_params: vec![TypeParam {
                        name: "T".to_string(),
                        bounds: Vec::new(),
                        span: Span::synthetic(),
                    }],
                    variants: vec![
                        Variant {
                            name: "Some".to_string(),
                            fields: vec![Type::TypeVar("T".to_string())],
                            span: Span::synthetic(),
                        },
                        Variant {
                            name: "None".to_string(),
                            fields: vec![],
                            span: Span::synthetic(),
                        },
                    ],
                    span: Span::synthetic(),
                },
            );
        }
        if !generic_enums.contains_key("Result") {
            generic_enums.insert(
                "Result".to_string(),
                EnumDecl {
                    name: "Result".to_string(),
                    is_pub: true,
                    type_params: vec![
                        TypeParam {
                            name: "T".to_string(),
                            bounds: Vec::new(),
                            span: Span::synthetic(),
                        },
                        TypeParam {
                            name: "E".to_string(),
                            bounds: Vec::new(),
                            span: Span::synthetic(),
                        },
                    ],
                    variants: vec![
                        Variant {
                            name: "Ok".to_string(),
                            fields: vec![Type::TypeVar("T".to_string())],
                            span: Span::synthetic(),
                        },
                        Variant {
                            name: "Err".to_string(),
                            fields: vec![Type::TypeVar("E".to_string())],
                            span: Span::synthetic(),
                        },
                    ],
                    span: Span::synthetic(),
                },
            );
        }

        Self {
            generic_fns,
            generic_structs,
            generic_enums,
            fn_instantiations: HashSet::new(),
            struct_instantiations: HashSet::new(),
            enum_instantiations: HashSet::new(),
        }
    }

    /// Walk the AST to find all concrete uses of generic functions/structs.
    fn collect_instantiations(&mut self, program: &Program) {
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) if f.type_params.is_empty() => {
                    // Collect from return type and parameter types
                    self.collect_from_type(&f.return_type);
                    for p in &f.params {
                        self.collect_from_type(&p.ty);
                    }
                    self.collect_from_block(&f.body);
                }
                Declaration::Impl(i) => {
                    for method in &i.methods {
                        if method.type_params.is_empty() {
                            self.collect_from_type(&method.return_type);
                            for p in &method.params {
                                self.collect_from_type(&p.ty);
                            }
                            self.collect_from_block(&method.body);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_from_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.collect_from_stmt(stmt);
        }
    }

    fn collect_from_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(s) => {
                self.collect_from_type(&s.ty);
                self.collect_from_expr(&s.value);
            }
            Stmt::Assign(s) => {
                self.collect_from_expr(&s.target);
                self.collect_from_expr(&s.value);
            }
            Stmt::Return(s) => {
                self.collect_from_expr(&s.value);
            }
            Stmt::If(s) => {
                self.collect_from_expr(&s.condition);
                self.collect_from_block(&s.then_block);
                if let Some(else_branch) = &s.else_branch {
                    match else_branch.as_ref() {
                        ElseBranch::ElseIf(elif) => {
                            self.collect_from_stmt(&Stmt::If(elif.clone()));
                        }
                        ElseBranch::Else(block) => {
                            self.collect_from_block(block);
                        }
                    }
                }
            }
            Stmt::While(s) => {
                self.collect_from_expr(&s.condition);
                self.collect_from_block(&s.body);
            }
            Stmt::For(s) => {
                self.collect_from_expr(&s.iterable);
                self.collect_from_block(&s.body);
            }
            Stmt::Match(s) => {
                self.collect_from_expr(&s.subject);
                for arm in &s.arms {
                    self.collect_from_block(&arm.body);
                }
            }
            Stmt::Expr(s) => {
                self.collect_from_expr(&s.expr);
            }
            Stmt::Break(_) | Stmt::Continue(_) => {}
        }
    }

    fn collect_from_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Call(callee, args) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    if self.generic_fns.contains_key(name) {
                        // Infer type args from argument types heuristically
                        if let Some(generic_fn) = self.generic_fns.get(name).cloned() {
                            let type_args = self.infer_type_args(&generic_fn, args);
                            if !type_args.is_empty() {
                                self.fn_instantiations.insert((name.clone(), type_args));
                            }
                        }
                    }
                }
                self.collect_from_expr(callee);
                for arg in args {
                    self.collect_from_expr(arg);
                }
            }
            ExprKind::Binary(lhs, _, rhs) => {
                self.collect_from_expr(lhs);
                self.collect_from_expr(rhs);
            }
            ExprKind::Unary(_, operand) => {
                self.collect_from_expr(operand);
            }
            ExprKind::FieldAccess(obj, _) => {
                self.collect_from_expr(obj);
            }
            ExprKind::MethodCall(receiver, method, args) => {
                self.collect_from_expr(receiver);
                for arg in args {
                    self.collect_from_expr(arg);
                }
                // Pipeline terminators that return Option<T>: find() and reduce()
                if (method == "find" || method == "reduce") && has_iter_base_static(receiver) {
                    if let Some(first_arg) = args.first() {
                        if let ExprKind::Closure(c) = &first_arg.kind {
                            if let Some(param) = c.params.first() {
                                self.collect_from_type(&Type::Generic(
                                    "Option".to_string(),
                                    vec![param.ty.clone()],
                                ));
                            }
                        }
                    }
                }
            }
            ExprKind::Index(arr, idx) => {
                self.collect_from_expr(arr);
                self.collect_from_expr(idx);
            }
            ExprKind::StructInit(name, fields) => {
                if self.generic_structs.contains_key(name) {
                    // Can't easily infer type args from struct init without type annotation
                    // This is handled by let statements with Generic type annotations
                }
                for fi in fields {
                    self.collect_from_expr(&fi.value);
                }
            }
            ExprKind::ArrayLit(elements) | ExprKind::TupleLit(elements) => {
                for elem in elements {
                    self.collect_from_expr(elem);
                }
            }
            ExprKind::ArrayRepeat(val, count) => {
                self.collect_from_expr(val);
                self.collect_from_expr(count);
            }
            ExprKind::Closure(c) => {
                self.collect_from_block(&c.body);
            }
            ExprKind::Spawn(block) => {
                self.collect_from_block(block);
            }
            ExprKind::Range(start, end) | ExprKind::RangeInclusive(start, end) => {
                self.collect_from_expr(start);
                self.collect_from_expr(end);
            }
            ExprKind::Try(inner) => {
                self.collect_from_expr(inner);
            }
            _ => {}
        }
    }

    fn collect_from_type(&mut self, ty: &Type) {
        if let Type::Generic(name, args) = ty {
            if self.generic_structs.contains_key(name) {
                self.struct_instantiations
                    .insert((name.clone(), args.clone()));
            }
            if self.generic_enums.contains_key(name) {
                // Only collect if all type args are concrete (no TypeVars)
                if args.iter().all(|a| !matches!(a, Type::TypeVar(_))) {
                    self.enum_instantiations
                        .insert((name.clone(), args.clone()));
                }
            }
        }
    }

    /// Simple type argument inference: match generic fn params against concrete arg types.
    fn infer_type_args(&self, generic_fn: &FnDecl, args: &[Expr]) -> Vec<Type> {
        let mut bindings: HashMap<String, Type> = HashMap::new();

        for (param, arg) in generic_fn.params.iter().zip(args.iter()) {
            if let Type::TypeVar(tv) = &param.ty {
                let inferred = self.infer_expr_type(arg);
                if let Some(ty) = inferred {
                    bindings.insert(tv.clone(), ty);
                }
            } else if let Type::Named(name) = &param.ty {
                // Check if this name is a type param
                if generic_fn.type_params.iter().any(|tp| tp.name == *name) {
                    let inferred = self.infer_expr_type(arg);
                    if let Some(ty) = inferred {
                        bindings.insert(name.clone(), ty);
                    }
                }
            }
        }

        // Return type args in declaration order
        generic_fn
            .type_params
            .iter()
            .map(|tp| {
                let resolved = bindings.get(&tp.name).cloned().unwrap_or(Type::Unit);
                debug_assert!(
                    bindings.contains_key(&tp.name),
                    "unresolved type param '{}' during monomorphization",
                    tp.name
                );
                resolved
            })
            .collect()
    }

    /// Simple expression type inference for monomorphization.
    fn infer_expr_type(&self, expr: &Expr) -> Option<Type> {
        match &expr.kind {
            ExprKind::Literal(Literal::Int(_)) => Some(Type::Int),
            ExprKind::Literal(Literal::Float(_)) => Some(Type::Float),
            ExprKind::Literal(Literal::Bool(_)) => Some(Type::Bool),
            ExprKind::Literal(Literal::Char(_)) => Some(Type::Char),
            ExprKind::Literal(Literal::String(_)) => Some(Type::Str),
            ExprKind::Literal(Literal::Unit) => Some(Type::Unit),
            _ => None,
        }
    }

    /// Produce the monomorphized program.
    fn emit(&self, mut program: Program) -> Program {
        let mut new_decls: Vec<Declaration> = Vec::new();

        // Emit monomorphized structs
        for (name, type_args) in &self.struct_instantiations {
            if let Some(generic_struct) = self.generic_structs.get(name) {
                let mangled = mangle_name(name, type_args);
                let subst = build_substitution(&generic_struct.type_params, type_args);
                let mono_struct = StructDecl {
                    name: mangled,
                    is_pub: generic_struct.is_pub,
                    type_params: Vec::new(),
                    fields: generic_struct
                        .fields
                        .iter()
                        .map(|f| Field {
                            name: f.name.clone(),
                            ty: substitute_type(&f.ty, &subst),
                            span: f.span,
                        })
                        .collect(),
                    span: generic_struct.span,
                };
                new_decls.push(Declaration::Struct(mono_struct));
            }
        }

        // Emit monomorphized enums
        for (name, type_args) in &self.enum_instantiations {
            if let Some(generic_enum) = self.generic_enums.get(name) {
                let mangled = mangle_name(name, type_args);
                let subst = build_substitution(&generic_enum.type_params, type_args);
                let mono_enum = EnumDecl {
                    name: mangled,
                    is_pub: generic_enum.is_pub,
                    type_params: Vec::new(),
                    variants: generic_enum
                        .variants
                        .iter()
                        .map(|v| Variant {
                            name: v.name.clone(),
                            fields: v
                                .fields
                                .iter()
                                .map(|f| substitute_type(f, &subst))
                                .collect(),
                            span: v.span,
                        })
                        .collect(),
                    span: generic_enum.span,
                };
                new_decls.push(Declaration::Enum(mono_enum));
            }
        }

        // Emit monomorphized functions
        for (name, type_args) in &self.fn_instantiations {
            if let Some(generic_fn) = self.generic_fns.get(name) {
                let mangled = mangle_name(name, type_args);
                let subst = build_substitution(&generic_fn.type_params, type_args);
                let mono_fn = substitute_fn_decl(generic_fn, &mangled, &subst);
                new_decls.push(Declaration::Function(mono_fn));
            }
        }

        // Rewrite existing declarations to use mangled names
        for decl in &mut program.declarations {
            match decl {
                Declaration::Function(f) if f.type_params.is_empty() => {
                    rewrite_block(
                        &mut f.body,
                        &self.generic_fns,
                        &self.generic_structs,
                        &self.fn_instantiations,
                        &self.struct_instantiations,
                    );
                }
                Declaration::Impl(i) => {
                    for method in &mut i.methods {
                        if method.type_params.is_empty() {
                            rewrite_block(
                                &mut method.body,
                                &self.generic_fns,
                                &self.generic_structs,
                                &self.fn_instantiations,
                                &self.struct_instantiations,
                            );
                        }
                    }
                }
                _ => {}
            }
        }

        // Remove generic function declarations (keep non-generic ones)
        program.declarations.retain(|d| match d {
            Declaration::Function(f) => f.type_params.is_empty(),
            Declaration::Struct(s) => s.type_params.is_empty(),
            Declaration::Enum(e) => e.type_params.is_empty(),
            _ => true,
        });

        // Add monomorphized declarations
        program.declarations.extend(new_decls);

        // Rewrite type annotations in let statements
        for decl in &mut program.declarations {
            match decl {
                Declaration::Function(f) => {
                    // Rewrite function return type if it's a generic enum
                    if let Type::Generic(name, args) = &f.return_type {
                        if self.generic_enums.contains_key(name) {
                            f.return_type = Type::Named(mangle_name(name, args));
                        }
                    }
                    // Rewrite parameter types if they're generic enums
                    for p in &mut f.params {
                        if let Type::Generic(name, args) = &p.ty {
                            if self.generic_enums.contains_key(name) {
                                p.ty = Type::Named(mangle_name(name, args));
                            }
                        }
                    }
                    rewrite_types_in_block(
                        &mut f.body,
                        &self.generic_structs,
                        &self.struct_instantiations,
                        &self.generic_enums,
                    );
                }
                Declaration::Impl(i) => {
                    for method in &mut i.methods {
                        rewrite_types_in_block(
                            &mut method.body,
                            &self.generic_structs,
                            &self.struct_instantiations,
                            &self.generic_enums,
                        );
                    }
                }
                _ => {}
            }
        }

        program
    }
}

/// Walk a MethodCall chain looking for `.iter()` at the base (static version).
fn has_iter_base_static(expr: &Expr) -> bool {
    if let ExprKind::MethodCall(ref receiver, ref method, _) = expr.kind {
        match method.as_str() {
            "iter" | "chars" => true,
            "map" | "filter" | "enumerate" | "zip" | "take" | "skip" | "chain" | "take_while"
            | "rev" => has_iter_base_static(receiver),
            _ => false,
        }
    } else {
        false
    }
}

fn mangle_type_arg(t: &Type) -> String {
    match t {
        Type::Int => "int".to_string(),
        Type::Float => "float".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::Str => "string".to_string(),
        Type::Named(n) => n.clone(),
        Type::Tuple(inner) => {
            let parts: Vec<String> = inner.iter().map(mangle_type_arg).collect();
            format!("tuple.{}", parts.join("."))
        }
        Type::Array(inner) => format!("array.{}", mangle_type_arg(inner)),
        _ => format!("{}", t),
    }
}

fn mangle_name(base: &str, type_args: &[Type]) -> String {
    let args_str: Vec<String> = type_args.iter().map(mangle_type_arg).collect();
    format!("{}__{}", base, args_str.join("__"))
}

fn build_substitution(type_params: &[TypeParam], type_args: &[Type]) -> HashMap<String, Type> {
    type_params
        .iter()
        .zip(type_args.iter())
        .map(|(tp, ta)| (tp.name.clone(), ta.clone()))
        .collect()
}

fn substitute_type(ty: &Type, subst: &HashMap<String, Type>) -> Type {
    match ty {
        Type::Named(name) if subst.contains_key(name) => subst[name].clone(),
        Type::TypeVar(name) if subst.contains_key(name) => subst[name].clone(),
        Type::Ref(inner) => Type::Ref(Box::new(substitute_type(inner, subst))),
        Type::MutRef(inner) => Type::MutRef(Box::new(substitute_type(inner, subst))),
        Type::Own(inner) => Type::Own(Box::new(substitute_type(inner, subst))),
        Type::Array(inner) => Type::Array(Box::new(substitute_type(inner, subst))),
        Type::Task(inner) => Type::Task(Box::new(substitute_type(inner, subst))),
        Type::Chan(inner) => Type::Chan(Box::new(substitute_type(inner, subst))),
        Type::Fn(params, ret) => Type::Fn(
            params.iter().map(|p| substitute_type(p, subst)).collect(),
            Box::new(substitute_type(ret, subst)),
        ),
        Type::Tuple(types) => {
            Type::Tuple(types.iter().map(|t| substitute_type(t, subst)).collect())
        }
        Type::Generic(name, args) => {
            let new_args: Vec<Type> = args.iter().map(|a| substitute_type(a, subst)).collect();
            // If all args are now concrete, produce a mangled Named type
            if new_args.iter().all(|a| !matches!(a, Type::TypeVar(_))) {
                Type::Named(mangle_name(name, &new_args))
            } else {
                Type::Generic(name.clone(), new_args)
            }
        }
        other => other.clone(),
    }
}

fn substitute_fn_decl(f: &FnDecl, mangled_name: &str, subst: &HashMap<String, Type>) -> FnDecl {
    FnDecl {
        name: mangled_name.to_string(),
        is_pure: f.is_pure,
        is_pub: f.is_pub,
        type_params: Vec::new(),
        params: f
            .params
            .iter()
            .map(|p| Param {
                name: p.name.clone(),
                ty: substitute_type(&p.ty, subst),
                span: p.span,
            })
            .collect(),
        return_type: substitute_type(&f.return_type, subst),
        contracts: f
            .contracts
            .iter()
            .map(|c| match c {
                Contract::Requires(e) => Contract::Requires(substitute_expr(e, subst)),
                Contract::Ensures(e) => Contract::Ensures(substitute_expr(e, subst)),
                Contract::Effects(e) => Contract::Effects(e.clone()),
            })
            .collect(),
        body: substitute_block(&f.body, subst),
        span: f.span,
    }
}

fn substitute_block(block: &Block, subst: &HashMap<String, Type>) -> Block {
    Block {
        stmts: block
            .stmts
            .iter()
            .map(|s| substitute_stmt(s, subst))
            .collect(),
        span: block.span,
    }
}

fn substitute_stmt(stmt: &Stmt, subst: &HashMap<String, Type>) -> Stmt {
    match stmt {
        Stmt::Let(s) => Stmt::Let(LetStmt {
            name: s.name.clone(),
            is_mut: s.is_mut,
            ty: substitute_type(&s.ty, subst),
            value: substitute_expr(&s.value, subst),
            destructure: s.destructure.clone(),
            span: s.span,
        }),
        Stmt::Assign(s) => Stmt::Assign(AssignStmt {
            target: substitute_expr(&s.target, subst),
            value: substitute_expr(&s.value, subst),
            span: s.span,
        }),
        Stmt::Return(s) => Stmt::Return(ReturnStmt {
            value: substitute_expr(&s.value, subst),
            span: s.span,
        }),
        Stmt::If(s) => Stmt::If(substitute_if(s, subst)),
        Stmt::While(s) => Stmt::While(WhileStmt {
            condition: substitute_expr(&s.condition, subst),
            body: substitute_block(&s.body, subst),
            span: s.span,
        }),
        Stmt::For(s) => Stmt::For(ForStmt {
            var_name: s.var_name.clone(),
            iterable: substitute_expr(&s.iterable, subst),
            body: substitute_block(&s.body, subst),
            span: s.span,
        }),
        Stmt::Match(s) => Stmt::Match(MatchStmt {
            subject: substitute_expr(&s.subject, subst),
            arms: s
                .arms
                .iter()
                .map(|a| MatchArm {
                    pattern: a.pattern.clone(),
                    body: substitute_block(&a.body, subst),
                    span: a.span,
                })
                .collect(),
            span: s.span,
        }),
        Stmt::Expr(s) => Stmt::Expr(ExprStmt {
            expr: substitute_expr(&s.expr, subst),
            span: s.span,
        }),
        Stmt::Break(s) => Stmt::Break(BreakStmt { span: s.span }),
        Stmt::Continue(s) => Stmt::Continue(ContinueStmt { span: s.span }),
    }
}

fn substitute_if(s: &IfStmt, subst: &HashMap<String, Type>) -> IfStmt {
    IfStmt {
        condition: substitute_expr(&s.condition, subst),
        then_block: substitute_block(&s.then_block, subst),
        else_branch: s.else_branch.as_ref().map(|eb| {
            Box::new(match eb.as_ref() {
                ElseBranch::ElseIf(elif) => ElseBranch::ElseIf(substitute_if(elif, subst)),
                ElseBranch::Else(block) => ElseBranch::Else(substitute_block(block, subst)),
            })
        }),
        span: s.span,
    }
}

fn substitute_expr(expr: &Expr, subst: &HashMap<String, Type>) -> Expr {
    let kind = match &expr.kind {
        ExprKind::Binary(lhs, op, rhs) => ExprKind::Binary(
            Box::new(substitute_expr(lhs, subst)),
            *op,
            Box::new(substitute_expr(rhs, subst)),
        ),
        ExprKind::Unary(op, operand) => {
            ExprKind::Unary(*op, Box::new(substitute_expr(operand, subst)))
        }
        ExprKind::Call(callee, args) => ExprKind::Call(
            Box::new(substitute_expr(callee, subst)),
            args.iter().map(|a| substitute_expr(a, subst)).collect(),
        ),
        ExprKind::FieldAccess(obj, field) => {
            ExprKind::FieldAccess(Box::new(substitute_expr(obj, subst)), field.clone())
        }
        ExprKind::MethodCall(recv, method, args) => ExprKind::MethodCall(
            Box::new(substitute_expr(recv, subst)),
            method.clone(),
            args.iter().map(|a| substitute_expr(a, subst)).collect(),
        ),
        ExprKind::Index(arr, idx) => ExprKind::Index(
            Box::new(substitute_expr(arr, subst)),
            Box::new(substitute_expr(idx, subst)),
        ),
        ExprKind::StructInit(name, fields) => {
            // If name matches a generic struct, this should have been mangled already
            ExprKind::StructInit(
                name.clone(),
                fields
                    .iter()
                    .map(|f| FieldInit {
                        name: f.name.clone(),
                        value: substitute_expr(&f.value, subst),
                        span: f.span,
                    })
                    .collect(),
            )
        }
        ExprKind::ArrayLit(elements) => {
            ExprKind::ArrayLit(elements.iter().map(|e| substitute_expr(e, subst)).collect())
        }
        ExprKind::ArrayRepeat(val, count) => ExprKind::ArrayRepeat(
            Box::new(substitute_expr(val, subst)),
            Box::new(substitute_expr(count, subst)),
        ),
        ExprKind::TupleLit(elements) => {
            ExprKind::TupleLit(elements.iter().map(|e| substitute_expr(e, subst)).collect())
        }
        ExprKind::Closure(c) => ExprKind::Closure(ClosureExpr {
            params: c
                .params
                .iter()
                .map(|p| Param {
                    name: p.name.clone(),
                    ty: substitute_type(&p.ty, subst),
                    span: p.span,
                })
                .collect(),
            return_type: substitute_type(&c.return_type, subst),
            body: substitute_block(&c.body, subst),
            span: c.span,
        }),
        ExprKind::Spawn(block) => ExprKind::Spawn(substitute_block(block, subst)),
        ExprKind::Range(start, end) => ExprKind::Range(
            Box::new(substitute_expr(start, subst)),
            Box::new(substitute_expr(end, subst)),
        ),
        ExprKind::RangeInclusive(start, end) => ExprKind::RangeInclusive(
            Box::new(substitute_expr(start, subst)),
            Box::new(substitute_expr(end, subst)),
        ),
        ExprKind::Try(inner) => ExprKind::Try(Box::new(substitute_expr(inner, subst))),
        other => other.clone(),
    };
    Expr {
        kind,
        span: expr.span,
    }
}

/// Rewrite call sites in a block to use mangled names.
fn rewrite_block(
    block: &mut Block,
    generic_fns: &HashMap<String, FnDecl>,
    generic_structs: &HashMap<String, StructDecl>,
    fn_insts: &HashSet<(String, Vec<Type>)>,
    struct_insts: &HashSet<(String, Vec<Type>)>,
) {
    for stmt in &mut block.stmts {
        rewrite_stmt(stmt, generic_fns, generic_structs, fn_insts, struct_insts);
    }
}

fn rewrite_stmt(
    stmt: &mut Stmt,
    generic_fns: &HashMap<String, FnDecl>,
    generic_structs: &HashMap<String, StructDecl>,
    fn_insts: &HashSet<(String, Vec<Type>)>,
    struct_insts: &HashSet<(String, Vec<Type>)>,
) {
    match stmt {
        Stmt::Let(s) => {
            rewrite_expr(
                &mut s.value,
                generic_fns,
                generic_structs,
                fn_insts,
                struct_insts,
            );
            // Rewrite struct init name in value if it's a generic struct
            if let Type::Generic(name, args) = &s.ty {
                if generic_structs.contains_key(name) {
                    let mangled = mangle_name(name, args);
                    let name_clone = name.clone();
                    s.ty = Type::Named(mangled.clone());
                    if let ExprKind::StructInit(ref mut sname, _) = s.value.kind {
                        if *sname == name_clone {
                            *sname = mangled;
                        }
                    }
                }
            }
        }
        Stmt::Assign(s) => {
            rewrite_expr(
                &mut s.target,
                generic_fns,
                generic_structs,
                fn_insts,
                struct_insts,
            );
            rewrite_expr(
                &mut s.value,
                generic_fns,
                generic_structs,
                fn_insts,
                struct_insts,
            );
        }
        Stmt::Return(s) => {
            rewrite_expr(
                &mut s.value,
                generic_fns,
                generic_structs,
                fn_insts,
                struct_insts,
            );
        }
        Stmt::If(s) => {
            rewrite_if_stmt(s, generic_fns, generic_structs, fn_insts, struct_insts);
        }
        Stmt::While(s) => {
            rewrite_expr(
                &mut s.condition,
                generic_fns,
                generic_structs,
                fn_insts,
                struct_insts,
            );
            rewrite_block(
                &mut s.body,
                generic_fns,
                generic_structs,
                fn_insts,
                struct_insts,
            );
        }
        Stmt::For(s) => {
            rewrite_expr(
                &mut s.iterable,
                generic_fns,
                generic_structs,
                fn_insts,
                struct_insts,
            );
            rewrite_block(
                &mut s.body,
                generic_fns,
                generic_structs,
                fn_insts,
                struct_insts,
            );
        }
        Stmt::Match(s) => {
            rewrite_expr(
                &mut s.subject,
                generic_fns,
                generic_structs,
                fn_insts,
                struct_insts,
            );
            for arm in &mut s.arms {
                rewrite_block(
                    &mut arm.body,
                    generic_fns,
                    generic_structs,
                    fn_insts,
                    struct_insts,
                );
            }
        }
        Stmt::Expr(s) => {
            rewrite_expr(
                &mut s.expr,
                generic_fns,
                generic_structs,
                fn_insts,
                struct_insts,
            );
        }
        Stmt::Break(_) | Stmt::Continue(_) => {}
    }
}

fn rewrite_if_stmt(
    s: &mut IfStmt,
    generic_fns: &HashMap<String, FnDecl>,
    generic_structs: &HashMap<String, StructDecl>,
    fn_insts: &HashSet<(String, Vec<Type>)>,
    struct_insts: &HashSet<(String, Vec<Type>)>,
) {
    rewrite_expr(
        &mut s.condition,
        generic_fns,
        generic_structs,
        fn_insts,
        struct_insts,
    );
    rewrite_block(
        &mut s.then_block,
        generic_fns,
        generic_structs,
        fn_insts,
        struct_insts,
    );
    if let Some(else_branch) = &mut s.else_branch {
        match else_branch.as_mut() {
            ElseBranch::ElseIf(elif) => {
                rewrite_if_stmt(elif, generic_fns, generic_structs, fn_insts, struct_insts);
            }
            ElseBranch::Else(block) => {
                rewrite_block(block, generic_fns, generic_structs, fn_insts, struct_insts);
            }
        }
    }
}

fn rewrite_expr(
    expr: &mut Expr,
    generic_fns: &HashMap<String, FnDecl>,
    _generic_structs: &HashMap<String, StructDecl>,
    fn_insts: &HashSet<(String, Vec<Type>)>,
    _struct_insts: &HashSet<(String, Vec<Type>)>,
) {
    match &mut expr.kind {
        ExprKind::Call(callee, args) => {
            if let ExprKind::Ident(name) = &callee.kind {
                if let Some(generic_fn) = generic_fns.get(name) {
                    // Find the instantiation that was collected for this call
                    let type_args_opt = infer_type_args_simple(generic_fn, args);
                    if let Some(type_args) = type_args_opt {
                        if fn_insts.contains(&(name.clone(), type_args.clone())) {
                            let mangled = mangle_name(name, &type_args);
                            **callee = Expr {
                                kind: ExprKind::Ident(mangled),
                                span: callee.span,
                            };
                        }
                    }
                }
            }
            for arg in args {
                rewrite_expr(arg, generic_fns, _generic_structs, fn_insts, _struct_insts);
            }
        }
        ExprKind::Binary(lhs, _, rhs) => {
            rewrite_expr(lhs, generic_fns, _generic_structs, fn_insts, _struct_insts);
            rewrite_expr(rhs, generic_fns, _generic_structs, fn_insts, _struct_insts);
        }
        ExprKind::Unary(_, operand) => {
            rewrite_expr(
                operand,
                generic_fns,
                _generic_structs,
                fn_insts,
                _struct_insts,
            );
        }
        ExprKind::FieldAccess(obj, _) => {
            rewrite_expr(obj, generic_fns, _generic_structs, fn_insts, _struct_insts);
        }
        ExprKind::MethodCall(recv, _, args) => {
            rewrite_expr(recv, generic_fns, _generic_structs, fn_insts, _struct_insts);
            for arg in args {
                rewrite_expr(arg, generic_fns, _generic_structs, fn_insts, _struct_insts);
            }
        }
        ExprKind::Index(arr, idx) => {
            rewrite_expr(arr, generic_fns, _generic_structs, fn_insts, _struct_insts);
            rewrite_expr(idx, generic_fns, _generic_structs, fn_insts, _struct_insts);
        }
        ExprKind::StructInit(_, fields) => {
            for f in fields {
                rewrite_expr(
                    &mut f.value,
                    generic_fns,
                    _generic_structs,
                    fn_insts,
                    _struct_insts,
                );
            }
        }
        ExprKind::ArrayLit(elements) | ExprKind::TupleLit(elements) => {
            for elem in elements {
                rewrite_expr(elem, generic_fns, _generic_structs, fn_insts, _struct_insts);
            }
        }
        ExprKind::ArrayRepeat(val, count) => {
            rewrite_expr(val, generic_fns, _generic_structs, fn_insts, _struct_insts);
            rewrite_expr(
                count,
                generic_fns,
                _generic_structs,
                fn_insts,
                _struct_insts,
            );
        }
        ExprKind::Closure(c) => {
            rewrite_block(
                &mut c.body,
                generic_fns,
                _generic_structs,
                fn_insts,
                _struct_insts,
            );
        }
        ExprKind::Spawn(block) => {
            rewrite_block(
                block,
                generic_fns,
                _generic_structs,
                fn_insts,
                _struct_insts,
            );
        }
        ExprKind::Range(start, end) | ExprKind::RangeInclusive(start, end) => {
            rewrite_expr(
                start,
                generic_fns,
                _generic_structs,
                fn_insts,
                _struct_insts,
            );
            rewrite_expr(end, generic_fns, _generic_structs, fn_insts, _struct_insts);
        }
        ExprKind::Try(inner) => {
            rewrite_expr(
                inner,
                generic_fns,
                _generic_structs,
                fn_insts,
                _struct_insts,
            );
        }
        _ => {}
    }
}

/// Rewrite Generic type annotations to Named types with mangled names.
/// Also rewrites map/set builtin calls to mangled names based on variable types.
fn rewrite_types_in_block(
    block: &mut Block,
    generic_structs: &HashMap<String, StructDecl>,
    _struct_insts: &HashSet<(String, Vec<Type>)>,
    generic_enums: &HashMap<String, EnumDecl>,
) {
    rewrite_types_in_block_with_vars(block, generic_structs, _struct_insts, generic_enums, None);
}

/// Inner implementation that carries parent variable type mappings into nested blocks.
fn rewrite_types_in_block_with_vars(
    block: &mut Block,
    generic_structs: &HashMap<String, StructDecl>,
    _struct_insts: &HashSet<(String, Vec<Type>)>,
    generic_enums: &HashMap<String, EnumDecl>,
    parent_var_types: Option<&HashMap<String, String>>,
) {
    // Track variable name → mangled Map/Set type name for collection call rewriting
    let mut var_types: HashMap<String, String> = parent_var_types.cloned().unwrap_or_default();

    for stmt in &mut block.stmts {
        match stmt {
            Stmt::Let(s) => {
                if let Type::Generic(name, args) = &s.ty {
                    if generic_structs.contains_key(name) {
                        let mangled = mangle_name(name, args);
                        let name_clone = name.clone();
                        s.ty = Type::Named(mangled.clone());
                        if let ExprKind::StructInit(ref mut sname, _) = s.value.kind {
                            if *sname == name_clone {
                                *sname = mangled;
                            }
                        }
                    } else if generic_enums.contains_key(name) {
                        let mangled = mangle_name(name, args);
                        s.ty = Type::Named(mangled);
                    } else if name == "Map" || name == "Set" {
                        let mangled = mangle_name(name, args);
                        var_types.insert(s.name.clone(), mangled.clone());
                        // Rewrite map_new()/set_new() to mangled version
                        let type_suffix = &mangled[name.len()..]; // e.g., "__string__int"
                        if let ExprKind::Call(ref mut callee, _) = s.value.kind {
                            if let ExprKind::Ident(ref fn_name) = callee.kind {
                                let expected_new = format!("{}_new", name.to_lowercase());
                                if *fn_name == expected_new {
                                    let mangled_fn = format!("{}{}", fn_name, type_suffix);
                                    **callee = Expr {
                                        kind: ExprKind::Ident(mangled_fn),
                                        span: callee.span,
                                    };
                                }
                            }
                        }
                        s.ty = Type::Named(mangled);
                    }
                }
                // Rewrite collection calls in the value expression
                rewrite_collection_calls_in_expr(&mut s.value, &var_types);
            }
            Stmt::Assign(s) => {
                rewrite_collection_calls_in_expr(&mut s.value, &var_types);
            }
            Stmt::Return(s) => {
                rewrite_collection_calls_in_expr(&mut s.value, &var_types);
            }
            Stmt::Expr(s) => {
                rewrite_collection_calls_in_expr(&mut s.expr, &var_types);
            }
            Stmt::If(s) => {
                rewrite_collection_calls_in_if(
                    s,
                    generic_structs,
                    _struct_insts,
                    generic_enums,
                    &var_types,
                );
            }
            Stmt::While(s) => {
                rewrite_collection_calls_in_expr(&mut s.condition, &var_types);
                rewrite_types_in_block_with_vars(
                    &mut s.body,
                    generic_structs,
                    _struct_insts,
                    generic_enums,
                    Some(&var_types),
                );
            }
            Stmt::For(s) => {
                rewrite_collection_calls_in_expr(&mut s.iterable, &var_types);
                rewrite_types_in_block_with_vars(
                    &mut s.body,
                    generic_structs,
                    _struct_insts,
                    generic_enums,
                    Some(&var_types),
                );
            }
            Stmt::Match(s) => {
                rewrite_collection_calls_in_expr(&mut s.subject, &var_types);
                for arm in &mut s.arms {
                    rewrite_types_in_block_with_vars(
                        &mut arm.body,
                        generic_structs,
                        _struct_insts,
                        generic_enums,
                        Some(&var_types),
                    );
                }
            }
            Stmt::Break(_) | Stmt::Continue(_) => {}
        }
    }
}

/// Rewrite map/set builtin calls in an if statement, propagating var_types into nested blocks.
fn rewrite_collection_calls_in_if(
    s: &mut IfStmt,
    generic_structs: &HashMap<String, StructDecl>,
    _struct_insts: &HashSet<(String, Vec<Type>)>,
    generic_enums: &HashMap<String, EnumDecl>,
    var_types: &HashMap<String, String>,
) {
    rewrite_collection_calls_in_expr(&mut s.condition, var_types);
    rewrite_types_in_block_with_vars(
        &mut s.then_block,
        generic_structs,
        _struct_insts,
        generic_enums,
        Some(var_types),
    );
    if let Some(else_branch) = &mut s.else_branch {
        match else_branch.as_mut() {
            ElseBranch::ElseIf(elif) => {
                rewrite_collection_calls_in_if(
                    elif,
                    generic_structs,
                    _struct_insts,
                    generic_enums,
                    var_types,
                );
            }
            ElseBranch::Else(block) => {
                rewrite_types_in_block_with_vars(
                    block,
                    generic_structs,
                    _struct_insts,
                    generic_enums,
                    Some(var_types),
                );
            }
        }
    }
}

/// Rewrite map/set builtin calls (e.g., map_set(m, k, v) → map_set__string__int(m, k, v))
/// based on the first argument's tracked type.
fn rewrite_collection_calls_in_expr(expr: &mut Expr, var_types: &HashMap<String, String>) {
    match &mut expr.kind {
        ExprKind::Call(callee, args) => {
            // First recurse into args
            for arg in args.iter_mut() {
                rewrite_collection_calls_in_expr(arg, var_types);
            }
            if let ExprKind::Ident(fn_name) = &callee.kind {
                let fn_name_clone = fn_name.clone();
                // map builtins: first arg is the map variable
                if let Some(suffix) = match fn_name_clone.as_str() {
                    "map_set" | "map_get" | "map_has" | "map_size" | "map_remove" | "map_keys"
                    | "map_values" => args
                        .first()
                        .and_then(|a| infer_expr_type_for_rewrite(a, var_types)),
                    "set_add" | "set_has" | "set_remove" | "set_size" | "set_values" => args
                        .first()
                        .and_then(|a| infer_expr_type_for_rewrite(a, var_types)),
                    "map_new" | "set_new" => None, // handled by let binding
                    _ => None,
                } {
                    // suffix is like "Map__string__int" or "Set__int" → extract the type suffix
                    let type_suffix = if suffix.starts_with("Map__") || suffix.starts_with("Set__")
                    {
                        &suffix[3..] // "__string__int" or "__int"
                    } else {
                        ""
                    };
                    if !type_suffix.is_empty() {
                        let mangled_fn = format!("{}{}", fn_name_clone, type_suffix);
                        **callee = Expr {
                            kind: ExprKind::Ident(mangled_fn),
                            span: callee.span,
                        };
                    }
                }
                // For map_new/set_new, rewrite based on the let binding (done in Let handler)
                // We handle them here for non-let contexts too
                if fn_name_clone == "map_new" || fn_name_clone == "set_new" {
                    // Can't rewrite without knowing the target type — handled by let stmt
                }
            }
        }
        ExprKind::Binary(lhs, _, rhs) => {
            rewrite_collection_calls_in_expr(lhs, var_types);
            rewrite_collection_calls_in_expr(rhs, var_types);
        }
        ExprKind::Unary(_, operand) => {
            rewrite_collection_calls_in_expr(operand, var_types);
        }
        ExprKind::Index(arr, idx) => {
            rewrite_collection_calls_in_expr(arr, var_types);
            rewrite_collection_calls_in_expr(idx, var_types);
        }
        ExprKind::FieldAccess(obj, _) => {
            rewrite_collection_calls_in_expr(obj, var_types);
        }
        ExprKind::MethodCall(recv, _, margs) => {
            rewrite_collection_calls_in_expr(recv, var_types);
            for arg in margs {
                rewrite_collection_calls_in_expr(arg, var_types);
            }
        }
        ExprKind::ArrayLit(elements) | ExprKind::TupleLit(elements) => {
            for elem in elements {
                rewrite_collection_calls_in_expr(elem, var_types);
            }
        }
        ExprKind::ArrayRepeat(val, count) => {
            rewrite_collection_calls_in_expr(val, var_types);
            rewrite_collection_calls_in_expr(count, var_types);
        }
        ExprKind::Try(inner) => {
            rewrite_collection_calls_in_expr(inner, var_types);
        }
        _ => {}
    }
}

/// Infer the mangled type name for a variable used in a collection builtin call.
fn infer_expr_type_for_rewrite(expr: &Expr, var_types: &HashMap<String, String>) -> Option<String> {
    if let ExprKind::Ident(name) = &expr.kind {
        var_types.get(name).cloned()
    } else {
        None
    }
}

fn infer_type_args_simple(generic_fn: &FnDecl, args: &[Expr]) -> Option<Vec<Type>> {
    let mut bindings: HashMap<String, Type> = HashMap::new();

    for (param, arg) in generic_fn.params.iter().zip(args.iter()) {
        let param_type_var = match &param.ty {
            Type::TypeVar(tv) => Some(tv.clone()),
            Type::Named(name) if generic_fn.type_params.iter().any(|tp| tp.name == *name) => {
                Some(name.clone())
            }
            _ => None,
        };

        if let Some(tv) = param_type_var {
            let inferred = match &arg.kind {
                ExprKind::Literal(Literal::Int(_)) => Some(Type::Int),
                ExprKind::Literal(Literal::Float(_)) => Some(Type::Float),
                ExprKind::Literal(Literal::Bool(_)) => Some(Type::Bool),
                ExprKind::Literal(Literal::String(_)) => Some(Type::Str),
                _ => None,
            };
            if let Some(ty) = inferred {
                bindings.insert(tv, ty);
            }
        }
    }

    if bindings.len() == generic_fn.type_params.len() {
        Some(
            generic_fn
                .type_params
                .iter()
                .map(|tp| {
                    let resolved = bindings.get(&tp.name).cloned().unwrap_or(Type::Unit);
                    debug_assert!(
                        bindings.contains_key(&tp.name),
                        "unresolved type param '{}' during monomorphization",
                        tp.name
                    );
                    resolved
                })
                .collect(),
        )
    } else {
        None
    }
}
