//! Monomorphization pass for generic functions and structs.
//!
//! Walks the AST collecting all concrete instantiations of generic functions/structs,
//! then clones declarations with type variables substituted by concrete types.
//! Output is a Program with no generics — only concrete declarations.

use crate::compiler::ast::*;
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
    /// Collected instantiations: (generic_name, concrete_type_args)
    fn_instantiations: HashSet<(String, Vec<Type>)>,
    struct_instantiations: HashSet<(String, Vec<Type>)>,
}

impl Monomorphizer {
    fn new(program: &Program) -> Self {
        let mut generic_fns = HashMap::new();
        let mut generic_structs = HashMap::new();

        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) if !f.type_params.is_empty() => {
                    generic_fns.insert(f.name.clone(), f.clone());
                }
                Declaration::Struct(s) if !s.type_params.is_empty() => {
                    generic_structs.insert(s.name.clone(), s.clone());
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

        Self {
            generic_fns,
            generic_structs,
            fn_instantiations: HashSet::new(),
            struct_instantiations: HashSet::new(),
        }
    }

    /// Walk the AST to find all concrete uses of generic functions/structs.
    fn collect_instantiations(&mut self, program: &Program) {
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) if f.type_params.is_empty() => {
                    self.collect_from_block(&f.body);
                }
                Declaration::Impl(i) => {
                    for method in &i.methods {
                        if method.type_params.is_empty() {
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
            ExprKind::MethodCall(receiver, _, args) => {
                self.collect_from_expr(receiver);
                for arg in args {
                    self.collect_from_expr(arg);
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
            ExprKind::ArrayLit(elements) => {
                for elem in elements {
                    self.collect_from_expr(elem);
                }
            }
            ExprKind::Closure(c) => {
                self.collect_from_block(&c.body);
            }
            ExprKind::Spawn(block) => {
                self.collect_from_block(block);
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
            .map(|tp| bindings.get(&tp.name).cloned().unwrap_or(Type::Int))
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
                    rewrite_types_in_block(
                        &mut f.body,
                        &self.generic_structs,
                        &self.struct_instantiations,
                    );
                }
                Declaration::Impl(i) => {
                    for method in &mut i.methods {
                        rewrite_types_in_block(
                            &mut method.body,
                            &self.generic_structs,
                            &self.struct_instantiations,
                        );
                    }
                }
                _ => {}
            }
        }

        program
    }
}

fn mangle_name(base: &str, type_args: &[Type]) -> String {
    let args_str: Vec<String> = type_args
        .iter()
        .map(|t| match t {
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::Str => "string".to_string(),
            Type::Named(n) => n.clone(),
            _ => format!("{}", t),
        })
        .collect();
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
                        rewrite_stmt(
                            &mut Stmt::If(elif.clone()),
                            generic_fns,
                            generic_structs,
                            fn_insts,
                            struct_insts,
                        );
                    }
                    ElseBranch::Else(block) => {
                        rewrite_block(block, generic_fns, generic_structs, fn_insts, struct_insts);
                    }
                }
            }
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
        ExprKind::ArrayLit(elements) => {
            for elem in elements {
                rewrite_expr(elem, generic_fns, _generic_structs, fn_insts, _struct_insts);
            }
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
        _ => {}
    }
}

/// Rewrite Generic type annotations to Named types with mangled names.
fn rewrite_types_in_block(
    block: &mut Block,
    generic_structs: &HashMap<String, StructDecl>,
    _struct_insts: &HashSet<(String, Vec<Type>)>,
) {
    for stmt in &mut block.stmts {
        if let Stmt::Let(s) = stmt {
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
                .map(|tp| bindings.get(&tp.name).cloned().unwrap_or(Type::Int))
                .collect(),
        )
    } else {
        None
    }
}
