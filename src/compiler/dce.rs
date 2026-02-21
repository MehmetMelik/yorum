//! Dead code elimination pass.
//!
//! Performs BFS reachability analysis from `main` and removes unreachable
//! functions, structs, enums, and impl blocks before codegen.

use crate::compiler::ast::*;
use std::collections::{HashMap, HashSet, VecDeque};

/// Remove unreachable declarations from a program via BFS from `main`.
/// If the program has no `main` function, returns the program unchanged.
pub fn eliminate_dead_code(mut program: Program) -> Program {
    // Only run DCE if there is a main function
    let has_main = program
        .declarations
        .iter()
        .any(|d| matches!(d, Declaration::Function(f) if f.name == "main"));
    if !has_main {
        return program;
    }

    // Build indexes
    let mut fn_bodies: HashMap<String, (&Block, &[Contract])> = HashMap::new();
    let mut method_bodies: HashMap<String, (&Block, &[Contract])> = HashMap::new();
    let mut variant_to_enum: HashMap<String, String> = HashMap::new();
    let mut struct_fields: HashMap<String, Vec<Type>> = HashMap::new();
    let mut impl_targets: HashMap<String, Vec<String>> = HashMap::new();
    let mut enum_variants: HashMap<String, Vec<(String, Vec<Type>)>> = HashMap::new();

    for decl in &program.declarations {
        match decl {
            Declaration::Function(f) => {
                fn_bodies.insert(f.name.clone(), (&f.body, &f.contracts));
            }
            Declaration::Struct(s) => {
                struct_fields.insert(
                    s.name.clone(),
                    s.fields.iter().map(|f| f.ty.clone()).collect(),
                );
            }
            Declaration::Enum(e) => {
                for v in &e.variants {
                    variant_to_enum.insert(v.name.clone(), e.name.clone());
                }
                enum_variants.insert(
                    e.name.clone(),
                    e.variants
                        .iter()
                        .map(|v| (v.name.clone(), v.fields.clone()))
                        .collect(),
                );
            }
            Declaration::Impl(i) => {
                let methods: Vec<String> = i.methods.iter().map(|m| m.name.clone()).collect();
                impl_targets.insert(i.target_type.clone(), methods);
                for method in &i.methods {
                    let mangled = format!("{}_{}", i.target_type, method.name);
                    method_bodies.insert(mangled, (&method.body, &method.contracts));
                }
            }
            _ => {}
        }
    }

    // Reachable sets
    let mut reachable_fns: HashSet<String> = HashSet::new();
    let mut reachable_structs: HashSet<String> = HashSet::new();
    let mut reachable_enums: HashSet<String> = HashSet::new();
    let mut reachable_impls: HashSet<String> = HashSet::new();

    // BFS queue of function/method names to process
    let mut queue: VecDeque<String> = VecDeque::new();
    queue.push_back("main".to_string());

    while let Some(name) = queue.pop_front() {
        if reachable_fns.contains(&name) {
            continue;
        }
        reachable_fns.insert(name.clone());

        // Find the body and contracts to scan
        let entry = fn_bodies
            .get(name.as_str())
            .or_else(|| method_bodies.get(name.as_str()))
            .copied();

        if let Some((body, contracts)) = entry {
            let mut refs = References::default();
            collect_block_refs(body, &mut refs);

            // Scan contract expressions (requires/ensures) for references
            for contract in contracts {
                match contract {
                    Contract::Requires(expr) | Contract::Ensures(expr) => {
                        collect_expr_refs(expr, &mut refs);
                    }
                    Contract::Effects(_) => {}
                }
            }

            // Enqueue called functions
            for fn_name in &refs.called_fns {
                if !reachable_fns.contains(fn_name) {
                    queue.push_back(fn_name.clone());
                }
                // If it's an enum variant constructor, mark the enum and its impl methods
                if let Some(enum_name) = variant_to_enum.get(fn_name) {
                    mark_enum_reachable(
                        enum_name,
                        &enum_variants,
                        &mut reachable_enums,
                        &mut reachable_structs,
                        &struct_fields,
                    );
                    if let Some(methods) = impl_targets.get(enum_name) {
                        reachable_impls.insert(enum_name.clone());
                        for method_name in methods {
                            let mangled = format!("{}_{}", enum_name, method_name);
                            if !reachable_fns.contains(&mangled) {
                                queue.push_back(mangled);
                            }
                        }
                    }
                }
            }

            // Mark structs
            for sname in &refs.used_structs {
                mark_struct_reachable(
                    sname,
                    &struct_fields,
                    &mut reachable_structs,
                    &mut reachable_enums,
                    &enum_variants,
                );
                // When a struct is reachable, mark all its impl methods
                if let Some(methods) = impl_targets.get(sname) {
                    reachable_impls.insert(sname.clone());
                    for method_name in methods {
                        let mangled = format!("{}_{}", sname, method_name);
                        if !reachable_fns.contains(&mangled) {
                            queue.push_back(mangled);
                        }
                    }
                }
            }

            // Mark enums from variant usage
            for vname in &refs.used_variants {
                if let Some(enum_name) = variant_to_enum.get(vname) {
                    mark_enum_reachable(
                        enum_name,
                        &enum_variants,
                        &mut reachable_enums,
                        &mut reachable_structs,
                        &struct_fields,
                    );
                    if let Some(methods) = impl_targets.get(enum_name) {
                        reachable_impls.insert(enum_name.clone());
                        for method_name in methods {
                            let mangled = format!("{}_{}", enum_name, method_name);
                            if !reachable_fns.contains(&mangled) {
                                queue.push_back(mangled);
                            }
                        }
                    }
                }
            }

            // Mark types referenced in let bindings, params, return types
            for ty in &refs.referenced_types {
                mark_type_reachable(
                    ty,
                    &struct_fields,
                    &enum_variants,
                    &impl_targets,
                    &mut reachable_structs,
                    &mut reachable_enums,
                    &mut reachable_impls,
                    &mut reachable_fns,
                    &mut queue,
                );
            }

            // Method calls: mark the target type's impl
            for (recv_type, method_name) in &refs.method_calls {
                if let Some(methods) = impl_targets.get(recv_type) {
                    reachable_impls.insert(recv_type.clone());
                    for m in methods {
                        let mangled = format!("{}_{}", recv_type, m);
                        if !reachable_fns.contains(&mangled) {
                            queue.push_back(mangled);
                        }
                    }
                    let _ = method_name; // all methods of the type are conservatively marked
                }
            }
        }
    }

    // Filter declarations
    program.declarations.retain(|decl| match decl {
        Declaration::Function(f) => reachable_fns.contains(&f.name),
        Declaration::Struct(s) => reachable_structs.contains(&s.name),
        Declaration::Enum(e) => reachable_enums.contains(&e.name),
        Declaration::Impl(i) => reachable_impls.contains(&i.target_type),
        Declaration::Const(_) => true,
        Declaration::Trait(_) => true,
    });

    program
}

/// Collected references from scanning a block.
#[derive(Default)]
struct References {
    called_fns: HashSet<String>,
    used_structs: HashSet<String>,
    used_variants: HashSet<String>,
    referenced_types: Vec<Type>,
    method_calls: Vec<(String, String)>, // (receiver_type_name, method_name)
}

fn collect_block_refs(block: &Block, refs: &mut References) {
    for stmt in &block.stmts {
        collect_stmt_refs(stmt, refs);
    }
}

fn collect_stmt_refs(stmt: &Stmt, refs: &mut References) {
    match stmt {
        Stmt::Let(s) => {
            refs.referenced_types.push(s.ty.clone());
            collect_expr_refs(&s.value, refs);
        }
        Stmt::Assign(s) => {
            collect_expr_refs(&s.target, refs);
            collect_expr_refs(&s.value, refs);
        }
        Stmt::Return(s) => {
            collect_expr_refs(&s.value, refs);
        }
        Stmt::If(s) => collect_if_refs(s, refs),
        Stmt::While(s) => {
            collect_expr_refs(&s.condition, refs);
            collect_block_refs(&s.body, refs);
        }
        Stmt::For(s) => {
            collect_expr_refs(&s.iterable, refs);
            collect_block_refs(&s.body, refs);
        }
        Stmt::Match(s) => {
            collect_expr_refs(&s.subject, refs);
            for arm in &s.arms {
                // Patterns may reference enum variants
                collect_pattern_refs(&arm.pattern, refs);
                collect_block_refs(&arm.body, refs);
            }
        }
        Stmt::Expr(s) => {
            collect_expr_refs(&s.expr, refs);
        }
        Stmt::Break(_) | Stmt::Continue(_) => {}
    }
}

fn collect_if_refs(s: &IfStmt, refs: &mut References) {
    collect_expr_refs(&s.condition, refs);
    collect_block_refs(&s.then_block, refs);
    if let Some(else_branch) = &s.else_branch {
        match else_branch.as_ref() {
            ElseBranch::ElseIf(elif) => collect_if_refs(elif, refs),
            ElseBranch::Else(block) => collect_block_refs(block, refs),
        }
    }
}

fn collect_expr_refs(expr: &Expr, refs: &mut References) {
    match &expr.kind {
        ExprKind::Call(callee, args) => {
            if let ExprKind::Ident(name) = &callee.kind {
                refs.called_fns.insert(name.clone());
            }
            collect_expr_refs(callee, refs);
            for arg in args {
                collect_expr_refs(arg, refs);
            }
        }
        ExprKind::StructInit(name, fields) => {
            refs.used_structs.insert(name.clone());
            for f in fields {
                collect_expr_refs(&f.value, refs);
            }
        }
        ExprKind::MethodCall(recv, method_name, args) => {
            collect_expr_refs(recv, refs);
            // Try to determine receiver type for impl marking
            if let ExprKind::Ident(_) = &recv.kind {
                // We can't determine the type from just the expression without type info,
                // but method calls through known struct types will be caught when the
                // struct is marked reachable (all its impl methods get enqueued).
                let _ = method_name;
            }
            for arg in args {
                collect_expr_refs(arg, refs);
            }
        }
        ExprKind::Binary(lhs, _, rhs) => {
            collect_expr_refs(lhs, refs);
            collect_expr_refs(rhs, refs);
        }
        ExprKind::Unary(_, operand) => {
            collect_expr_refs(operand, refs);
        }
        ExprKind::FieldAccess(obj, _) => {
            collect_expr_refs(obj, refs);
        }
        ExprKind::Index(arr, idx) => {
            collect_expr_refs(arr, refs);
            collect_expr_refs(idx, refs);
        }
        ExprKind::ArrayLit(elements) | ExprKind::TupleLit(elements) => {
            for elem in elements {
                collect_expr_refs(elem, refs);
            }
        }
        ExprKind::ArrayRepeat(val, count) => {
            collect_expr_refs(val, refs);
            collect_expr_refs(count, refs);
        }
        ExprKind::Closure(c) => {
            for p in &c.params {
                refs.referenced_types.push(p.ty.clone());
            }
            refs.referenced_types.push(c.return_type.clone());
            collect_block_refs(&c.body, refs);
        }
        ExprKind::Spawn(block) => {
            collect_block_refs(block, refs);
        }
        ExprKind::Range(start, end) | ExprKind::RangeInclusive(start, end) => {
            collect_expr_refs(start, refs);
            collect_expr_refs(end, refs);
        }
        ExprKind::Try(inner) => {
            collect_expr_refs(inner, refs);
        }
        ExprKind::Ident(name) => {
            // Could be an enum variant (e.g., None)
            refs.used_variants.insert(name.clone());
        }
        ExprKind::Literal(_) => {}
    }
}

fn collect_pattern_refs(pattern: &Pattern, refs: &mut References) {
    if let Pattern::Variant(name, sub_patterns, _) = pattern {
        refs.used_variants.insert(name.clone());
        for p in sub_patterns {
            collect_pattern_refs(p, refs);
        }
    }
}

fn mark_struct_reachable(
    name: &str,
    struct_fields: &HashMap<String, Vec<Type>>,
    reachable_structs: &mut HashSet<String>,
    reachable_enums: &mut HashSet<String>,
    enum_variants: &HashMap<String, Vec<(String, Vec<Type>)>>,
) {
    if reachable_structs.contains(name) {
        return;
    }
    reachable_structs.insert(name.to_string());
    // Mark types in struct fields
    if let Some(fields) = struct_fields.get(name) {
        for ty in fields {
            mark_type_in_struct(
                ty,
                struct_fields,
                reachable_structs,
                reachable_enums,
                enum_variants,
            );
        }
    }
}

fn mark_enum_reachable(
    name: &str,
    enum_variants: &HashMap<String, Vec<(String, Vec<Type>)>>,
    reachable_enums: &mut HashSet<String>,
    reachable_structs: &mut HashSet<String>,
    struct_fields: &HashMap<String, Vec<Type>>,
) {
    if reachable_enums.contains(name) {
        return;
    }
    reachable_enums.insert(name.to_string());
    // Mark types in variant fields
    if let Some(variants) = enum_variants.get(name) {
        for (_, fields) in variants {
            for ty in fields {
                mark_type_in_struct(
                    ty,
                    struct_fields,
                    reachable_structs,
                    reachable_enums,
                    enum_variants,
                );
            }
        }
    }
}

fn mark_type_in_struct(
    ty: &Type,
    struct_fields: &HashMap<String, Vec<Type>>,
    reachable_structs: &mut HashSet<String>,
    reachable_enums: &mut HashSet<String>,
    enum_variants: &HashMap<String, Vec<(String, Vec<Type>)>>,
) {
    match ty {
        Type::Named(name) => {
            if struct_fields.contains_key(name) {
                mark_struct_reachable(
                    name,
                    struct_fields,
                    reachable_structs,
                    reachable_enums,
                    enum_variants,
                );
            } else if enum_variants.contains_key(name) {
                mark_enum_reachable(
                    name,
                    enum_variants,
                    reachable_enums,
                    reachable_structs,
                    struct_fields,
                );
            }
        }
        Type::Array(inner)
        | Type::Ref(inner)
        | Type::MutRef(inner)
        | Type::Own(inner)
        | Type::Task(inner)
        | Type::Chan(inner) => {
            mark_type_in_struct(
                inner,
                struct_fields,
                reachable_structs,
                reachable_enums,
                enum_variants,
            );
        }
        Type::Generic(name, args) => {
            if enum_variants.contains_key(name) {
                mark_enum_reachable(
                    name,
                    enum_variants,
                    reachable_enums,
                    reachable_structs,
                    struct_fields,
                );
            }
            for arg in args {
                mark_type_in_struct(
                    arg,
                    struct_fields,
                    reachable_structs,
                    reachable_enums,
                    enum_variants,
                );
            }
        }
        Type::Tuple(types) => {
            for t in types {
                mark_type_in_struct(
                    t,
                    struct_fields,
                    reachable_structs,
                    reachable_enums,
                    enum_variants,
                );
            }
        }
        Type::Fn(params, ret) => {
            for p in params {
                mark_type_in_struct(
                    p,
                    struct_fields,
                    reachable_structs,
                    reachable_enums,
                    enum_variants,
                );
            }
            mark_type_in_struct(
                ret,
                struct_fields,
                reachable_structs,
                reachable_enums,
                enum_variants,
            );
        }
        _ => {}
    }
}

#[allow(clippy::too_many_arguments)]
fn mark_type_reachable(
    ty: &Type,
    struct_fields: &HashMap<String, Vec<Type>>,
    enum_variants: &HashMap<String, Vec<(String, Vec<Type>)>>,
    impl_targets: &HashMap<String, Vec<String>>,
    reachable_structs: &mut HashSet<String>,
    reachable_enums: &mut HashSet<String>,
    reachable_impls: &mut HashSet<String>,
    reachable_fns: &mut HashSet<String>,
    queue: &mut VecDeque<String>,
) {
    match ty {
        Type::Named(name) => {
            if struct_fields.contains_key(name) {
                mark_struct_reachable(
                    name,
                    struct_fields,
                    reachable_structs,
                    reachable_enums,
                    enum_variants,
                );
                // Mark all impl methods for reachable structs
                if let Some(methods) = impl_targets.get(name) {
                    reachable_impls.insert(name.clone());
                    for method_name in methods {
                        let mangled = format!("{}_{}", name, method_name);
                        if !reachable_fns.contains(&mangled) {
                            reachable_fns.insert(mangled.clone());
                            queue.push_back(mangled);
                        }
                    }
                }
            } else if enum_variants.contains_key(name) {
                mark_enum_reachable(
                    name,
                    enum_variants,
                    reachable_enums,
                    reachable_structs,
                    struct_fields,
                );
                // Mark all impl methods for reachable enums
                if let Some(methods) = impl_targets.get(name) {
                    reachable_impls.insert(name.clone());
                    for method_name in methods {
                        let mangled = format!("{}_{}", name, method_name);
                        if !reachable_fns.contains(&mangled) {
                            reachable_fns.insert(mangled.clone());
                            queue.push_back(mangled);
                        }
                    }
                }
            }
        }
        Type::Array(inner)
        | Type::Ref(inner)
        | Type::MutRef(inner)
        | Type::Own(inner)
        | Type::Task(inner)
        | Type::Chan(inner) => {
            mark_type_reachable(
                inner,
                struct_fields,
                enum_variants,
                impl_targets,
                reachable_structs,
                reachable_enums,
                reachable_impls,
                reachable_fns,
                queue,
            );
        }
        Type::Generic(name, args) => {
            if enum_variants.contains_key(name) {
                mark_enum_reachable(
                    name,
                    enum_variants,
                    reachable_enums,
                    reachable_structs,
                    struct_fields,
                );
            }
            for arg in args {
                mark_type_reachable(
                    arg,
                    struct_fields,
                    enum_variants,
                    impl_targets,
                    reachable_structs,
                    reachable_enums,
                    reachable_impls,
                    reachable_fns,
                    queue,
                );
            }
        }
        Type::Tuple(types) => {
            for t in types {
                mark_type_reachable(
                    t,
                    struct_fields,
                    enum_variants,
                    impl_targets,
                    reachable_structs,
                    reachable_enums,
                    reachable_impls,
                    reachable_fns,
                    queue,
                );
            }
        }
        Type::Fn(params, ret) => {
            for p in params {
                mark_type_reachable(
                    p,
                    struct_fields,
                    enum_variants,
                    impl_targets,
                    reachable_structs,
                    reachable_enums,
                    reachable_impls,
                    reachable_fns,
                    queue,
                );
            }
            mark_type_reachable(
                ret,
                struct_fields,
                enum_variants,
                impl_targets,
                reachable_structs,
                reachable_enums,
                reachable_impls,
                reachable_fns,
                queue,
            );
        }
        _ => {}
    }
}
