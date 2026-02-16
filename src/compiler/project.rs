use crate::compiler::ast::*;
use crate::compiler::codegen::Codegen;
use crate::compiler::module_resolver::ModuleResolver;
use crate::compiler::monomorphize::monomorphize;
use crate::compiler::ownership::OwnershipChecker;
use crate::compiler::typechecker::TypeChecker;
use crate::manifest::YorumManifest;
use std::path::Path;

/// Compile a multi-file Yorum project from a root directory containing yorum.toml.
pub fn compile_project(root_dir: &Path) -> Result<String, String> {
    // Read manifest
    let manifest_path = root_dir.join("yorum.toml");
    let manifest_content = std::fs::read_to_string(&manifest_path)
        .map_err(|e| format!("cannot read '{}': {}", manifest_path.display(), e))?;
    let manifest: YorumManifest = manifest_content.parse()?;

    // Discover and parse all modules
    let src_dir = root_dir.join(&manifest.package.src_dir);
    let resolver = ModuleResolver::new(src_dir);
    let modules = resolver.resolve_all()?;

    if modules.is_empty() {
        return Err("no .yrm files found in src directory".to_string());
    }

    // Find the main module (contains fn main)
    let main_module_path = find_main_module(&modules)?;

    // Merge all modules into a single Program
    let merged = merge_modules(&modules, &main_module_path)?;

    // Run standard pipeline: typecheck → ownership → monomorphize → codegen
    let mut typechecker = TypeChecker::new();
    typechecker.check_program(&merged).map_err(|errs| {
        errs.iter()
            .map(|e| format!("{}", e))
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let mut ownership = OwnershipChecker::new();
    ownership.check_program(&merged).map_err(|errs| {
        errs.iter()
            .map(|e| format!("{}", e))
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let merged = monomorphize(merged);

    let mut codegen = Codegen::new();
    let ir = codegen.generate(&merged).map_err(|e| format!("{}", e))?;

    Ok(ir)
}

fn find_main_module(
    modules: &std::collections::HashMap<String, crate::compiler::module_resolver::ParsedModule>,
) -> Result<String, String> {
    for (path, module) in modules {
        for decl in &module.program.declarations {
            if let Declaration::Function(f) = decl {
                if f.name == "main" {
                    return Ok(path.clone());
                }
            }
        }
    }
    Err("no module contains a 'fn main()' function".to_string())
}

fn merge_modules(
    modules: &std::collections::HashMap<String, crate::compiler::module_resolver::ParsedModule>,
    main_module_path: &str,
) -> Result<Program, String> {
    let main_module = modules
        .get(main_module_path)
        .ok_or("main module not found")?;

    let mut merged_decls: Vec<Declaration> = Vec::new();

    // Collect use declarations from the main module
    let uses: Vec<String> = main_module
        .program
        .uses
        .iter()
        .map(|u| u.path.join("."))
        .collect();

    // Add declarations from imported modules (pub only, with prefixed names)
    for use_path in &uses {
        let imported_module = modules
            .get(use_path)
            .ok_or_else(|| format!("module '{}' not found (used by main module)", use_path))?;

        let prefix = use_path.replace('.', "__");

        for decl in &imported_module.program.declarations {
            match decl {
                Declaration::Function(f) if f.is_pub => {
                    let mut prefixed = f.clone();
                    prefixed.name = format!("{}__{}", prefix, f.name);
                    merged_decls.push(Declaration::Function(prefixed));
                }
                Declaration::Struct(s) if s.is_pub => {
                    let mut prefixed = s.clone();
                    prefixed.name = format!("{}__{}", prefix, s.name);
                    merged_decls.push(Declaration::Struct(prefixed));
                }
                Declaration::Enum(e) if e.is_pub => {
                    let mut prefixed = e.clone();
                    prefixed.name = format!("{}__{}", prefix, e.name);
                    merged_decls.push(Declaration::Enum(prefixed));
                }
                Declaration::Const(c) if c.is_pub => {
                    let mut prefixed = c.clone();
                    prefixed.name = format!("{}__{}", prefix, c.name);
                    merged_decls.push(Declaration::Const(prefixed));
                }
                _ => {}
            }
        }
    }

    // Add main module declarations (all of them, not just pub)
    merged_decls.extend(main_module.program.declarations.clone());

    // Rewrite call sites in the main module to use prefixed names
    rewrite_calls_for_imports(&mut merged_decls, &uses, modules)?;

    Ok(Program {
        module_name: main_module.program.module_name.clone(),
        uses: Vec::new(), // Uses are resolved
        declarations: merged_decls,
        span: main_module.program.span,
    })
}

fn rewrite_calls_for_imports(
    decls: &mut [Declaration],
    uses: &[String],
    modules: &std::collections::HashMap<String, crate::compiler::module_resolver::ParsedModule>,
) -> Result<(), String> {
    // Build a map of function names from imported modules → prefixed names
    let mut name_map: std::collections::HashMap<String, String> = std::collections::HashMap::new();

    for use_path in uses {
        if let Some(imported_module) = modules.get(use_path) {
            let prefix = use_path.replace('.', "__");
            for decl in &imported_module.program.declarations {
                match decl {
                    Declaration::Function(f) if f.is_pub => {
                        name_map.insert(f.name.clone(), format!("{}__{}", prefix, f.name));
                    }
                    Declaration::Struct(s) if s.is_pub => {
                        name_map.insert(s.name.clone(), format!("{}__{}", prefix, s.name));
                    }
                    _ => {}
                }
            }
        }
    }

    if name_map.is_empty() {
        return Ok(());
    }

    // Rewrite declarations
    for decl in decls.iter_mut() {
        match decl {
            Declaration::Function(f) => {
                // Don't rewrite the imported functions themselves (they already have prefixed names)
                if !f.name.contains("__") {
                    rewrite_block_calls(&mut f.body, &name_map);
                }
            }
            Declaration::Impl(i) => {
                for method in &mut i.methods {
                    rewrite_block_calls(&mut method.body, &name_map);
                }
            }
            _ => {}
        }
    }

    Ok(())
}

fn rewrite_block_calls(block: &mut Block, name_map: &std::collections::HashMap<String, String>) {
    for stmt in &mut block.stmts {
        rewrite_stmt_calls(stmt, name_map);
    }
}

fn rewrite_stmt_calls(stmt: &mut Stmt, name_map: &std::collections::HashMap<String, String>) {
    match stmt {
        Stmt::Let(s) => {
            rewrite_expr_calls(&mut s.value, name_map);
            // Rewrite struct init names in type and value
            if let Type::Named(ref name) = s.ty {
                if let Some(new_name) = name_map.get(name) {
                    let new_name = new_name.clone();
                    let old_name = name.clone();
                    s.ty = Type::Named(new_name.clone());
                    if let ExprKind::StructInit(ref mut sname, _) = s.value.kind {
                        if *sname == old_name {
                            *sname = new_name;
                        }
                    }
                }
            }
        }
        Stmt::Assign(s) => {
            rewrite_expr_calls(&mut s.target, name_map);
            rewrite_expr_calls(&mut s.value, name_map);
        }
        Stmt::Return(s) => {
            rewrite_expr_calls(&mut s.value, name_map);
        }
        Stmt::If(s) => {
            rewrite_if_stmt_calls(s, name_map);
        }
        Stmt::While(s) => {
            rewrite_expr_calls(&mut s.condition, name_map);
            rewrite_block_calls(&mut s.body, name_map);
        }
        Stmt::For(s) => {
            rewrite_expr_calls(&mut s.iterable, name_map);
            rewrite_block_calls(&mut s.body, name_map);
        }
        Stmt::Match(s) => {
            rewrite_expr_calls(&mut s.subject, name_map);
            for arm in &mut s.arms {
                rewrite_block_calls(&mut arm.body, name_map);
            }
        }
        Stmt::Expr(s) => {
            rewrite_expr_calls(&mut s.expr, name_map);
        }
        Stmt::Break(_) | Stmt::Continue(_) => {}
    }
}

fn rewrite_if_stmt_calls(s: &mut IfStmt, name_map: &std::collections::HashMap<String, String>) {
    rewrite_expr_calls(&mut s.condition, name_map);
    rewrite_block_calls(&mut s.then_block, name_map);
    if let Some(else_branch) = &mut s.else_branch {
        match else_branch.as_mut() {
            ElseBranch::ElseIf(elif) => {
                rewrite_if_stmt_calls(elif, name_map);
            }
            ElseBranch::Else(block) => {
                rewrite_block_calls(block, name_map);
            }
        }
    }
}

fn rewrite_expr_calls(expr: &mut Expr, name_map: &std::collections::HashMap<String, String>) {
    match &mut expr.kind {
        ExprKind::Call(callee, args) => {
            if let ExprKind::Ident(ref mut name) = callee.kind {
                if let Some(new_name) = name_map.get(name.as_str()) {
                    *name = new_name.clone();
                }
            }
            rewrite_expr_calls(callee, name_map);
            for arg in args {
                rewrite_expr_calls(arg, name_map);
            }
        }
        ExprKind::Binary(lhs, _, rhs) => {
            rewrite_expr_calls(lhs, name_map);
            rewrite_expr_calls(rhs, name_map);
        }
        ExprKind::Unary(_, operand) => {
            rewrite_expr_calls(operand, name_map);
        }
        ExprKind::FieldAccess(obj, _) => {
            rewrite_expr_calls(obj, name_map);
        }
        ExprKind::MethodCall(recv, _, args) => {
            rewrite_expr_calls(recv, name_map);
            for arg in args {
                rewrite_expr_calls(arg, name_map);
            }
        }
        ExprKind::Index(arr, idx) => {
            rewrite_expr_calls(arr, name_map);
            rewrite_expr_calls(idx, name_map);
        }
        ExprKind::StructInit(ref mut name, fields) => {
            if let Some(new_name) = name_map.get(name.as_str()) {
                *name = new_name.clone();
            }
            for f in fields {
                rewrite_expr_calls(&mut f.value, name_map);
            }
        }
        ExprKind::ArrayLit(elements) | ExprKind::TupleLit(elements) => {
            for elem in elements {
                rewrite_expr_calls(elem, name_map);
            }
        }
        ExprKind::Closure(c) => {
            rewrite_block_calls(&mut c.body, name_map);
        }
        ExprKind::Spawn(block) => {
            rewrite_block_calls(block, name_map);
        }
        ExprKind::Range(start, end) => {
            rewrite_expr_calls(start, name_map);
            rewrite_expr_calls(end, name_map);
        }
        _ => {}
    }
}
