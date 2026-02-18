use crate::compiler::ast::*;
use crate::compiler::codegen::Codegen;
use crate::compiler::dce::eliminate_dead_code;
use crate::compiler::lockfile::{LockFile, LockedPackage};
use crate::compiler::module_resolver::{ModuleResolver, ParsedModule};
use crate::compiler::monomorphize::monomorphize;
use crate::compiler::ownership::OwnershipChecker;
use crate::compiler::package::{self, PackageCache, ResolvedDep};
use crate::compiler::typechecker::TypeChecker;
use crate::manifest::YorumManifest;
use std::collections::HashMap;
use std::path::Path;

/// Compile a multi-file Yorum project from a root directory containing yorum.toml.
pub fn compile_project(root_dir: &Path) -> Result<String, String> {
    // Read manifest
    let manifest_path = root_dir.join("yorum.toml");
    let manifest_content = std::fs::read_to_string(&manifest_path)
        .map_err(|e| format!("cannot read '{}': {}", manifest_path.display(), e))?;
    let manifest: YorumManifest = manifest_content.parse()?;

    // Resolve dependencies
    let resolved_deps = resolve_dependencies(root_dir, &manifest)?;

    // Discover and parse all local modules
    let src_dir = root_dir.join(&manifest.package.src_dir);
    let resolver = ModuleResolver::new(src_dir);
    let local_modules = resolver.resolve_all()?;

    if local_modules.is_empty() {
        return Err("no .yrm files found in src directory".to_string());
    }

    // Discover and parse dependency modules
    let dep_modules = resolve_dep_modules(&resolved_deps)?;

    // Find the main module (contains fn main)
    let main_module_path = find_main_module(&local_modules)?;

    // Merge all modules into a single Program
    let merged = merge_modules(&local_modules, &dep_modules, &main_module_path)?;

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
    let merged = eliminate_dead_code(merged);

    let mut codegen = Codegen::new();
    let ir = codegen.generate(&merged).map_err(|e| format!("{}", e))?;

    Ok(ir)
}

/// Resolve all dependencies declared in the manifest.
/// Fetches/caches git deps, resolves path deps, writes lock file if needed.
pub fn resolve_dependencies(
    root_dir: &Path,
    manifest: &YorumManifest,
) -> Result<Vec<ResolvedDep>, String> {
    if manifest.dependencies.is_empty() {
        return Ok(Vec::new());
    }

    let cache = PackageCache::new()?;
    let lock_path = root_dir.join("yorum.lock");

    // Try to read existing lock file
    let existing_lock = if lock_path.exists() {
        Some(LockFile::read(&lock_path)?)
    } else {
        None
    };

    // Check if lock file is stale (deps in manifest not in lock)
    let lock_stale = if let Some(ref lock) = existing_lock {
        manifest
            .dependencies
            .keys()
            .any(|name| lock.find_package(name).is_none())
    } else {
        true
    };

    if lock_stale && existing_lock.is_some() {
        eprintln!("warning: yorum.lock is out of date, resolving dependencies...");
    }

    let mut resolved = Vec::new();

    for (name, spec) in &manifest.dependencies {
        let dep = package::resolve_dep(name, spec, root_dir, &cache)?;
        resolved.push(dep);
    }

    // Write/update lock file if needed
    if lock_stale || existing_lock.is_none() {
        let lock = build_lock_file(&resolved, &manifest.dependencies)?;
        lock.write(&lock_path)?;
    }

    Ok(resolved)
}

/// Install all dependencies (always regenerate lock file).
pub fn install_dependencies(root_dir: &Path) -> Result<usize, String> {
    let manifest_path = root_dir.join("yorum.toml");
    let manifest_content = std::fs::read_to_string(&manifest_path)
        .map_err(|e| format!("cannot read '{}': {}", manifest_path.display(), e))?;
    let manifest: YorumManifest = manifest_content.parse()?;

    if manifest.dependencies.is_empty() {
        return Ok(0);
    }

    let cache = PackageCache::new()?;
    let mut resolved = Vec::new();

    for (name, spec) in &manifest.dependencies {
        let dep = package::resolve_dep(name, spec, root_dir, &cache)?;
        resolved.push(dep);
    }

    let count = resolved.len();

    // Always regenerate lock file
    let lock = build_lock_file(&resolved, &manifest.dependencies)?;
    lock.write(&root_dir.join("yorum.lock"))?;

    Ok(count)
}

/// Update dependencies (fetch latest, regenerate lock file).
pub fn update_dependencies(root_dir: &Path, dep_name: Option<&str>) -> Result<usize, String> {
    let manifest_path = root_dir.join("yorum.toml");
    let manifest_content = std::fs::read_to_string(&manifest_path)
        .map_err(|e| format!("cannot read '{}': {}", manifest_path.display(), e))?;
    let manifest: YorumManifest = manifest_content.parse()?;

    if manifest.dependencies.is_empty() {
        return Ok(0);
    }

    let cache = PackageCache::new()?;
    let mut resolved = Vec::new();

    let deps_to_update: Vec<(&String, &crate::manifest::DependencySpec)> =
        if let Some(name) = dep_name {
            let spec = manifest
                .dependencies
                .get(name)
                .ok_or_else(|| format!("dependency '{}' not found in yorum.toml", name))?;
            vec![(
                manifest
                    .dependencies
                    .keys()
                    .find(|k| k.as_str() == name)
                    .unwrap(),
                spec,
            )]
        } else {
            manifest.dependencies.iter().collect()
        };

    // For update, we need to handle git deps specially — remove cached copy to force re-fetch
    for (name, spec) in &deps_to_update {
        if let Some(ref url) = spec.git {
            let cached_dir = cache.cache_dir_for(name, url);
            if cached_dir.exists() {
                let _ = std::fs::remove_dir_all(&cached_dir);
            }
        }
    }

    // Now resolve all deps (updated ones will be re-fetched)
    for (name, spec) in &manifest.dependencies {
        let dep = package::resolve_dep(name, spec, root_dir, &cache)?;
        resolved.push(dep);
    }

    let count = deps_to_update.len();

    // Regenerate lock file
    let lock = build_lock_file(&resolved, &manifest.dependencies)?;
    lock.write(&root_dir.join("yorum.lock"))?;

    Ok(count)
}

fn build_lock_file(
    resolved: &[ResolvedDep],
    dep_specs: &HashMap<String, crate::manifest::DependencySpec>,
) -> Result<LockFile, String> {
    let mut packages = Vec::new();

    for dep in resolved {
        let spec = dep_specs
            .get(&dep.name)
            .ok_or_else(|| format!("internal error: no spec for resolved dep '{}'", dep.name))?;

        let source = if let Some(ref url) = spec.git {
            let sha = dep.git_commit.as_deref().unwrap_or("unknown");
            LockedPackage::git_source(url, sha)
        } else if let Some(ref path) = spec.path {
            LockedPackage::path_source(path)
        } else {
            return Err(format!(
                "internal error: dep '{}' has neither git nor path",
                dep.name
            ));
        };

        packages.push(LockedPackage {
            name: dep.name.clone(),
            version: dep.version.clone(),
            source,
        });
    }

    Ok(LockFile { packages })
}

/// Discover and parse all modules from resolved dependencies.
fn resolve_dep_modules(deps: &[ResolvedDep]) -> Result<HashMap<String, Vec<ParsedModule>>, String> {
    let mut dep_modules = HashMap::new();

    for dep in deps {
        let resolver = ModuleResolver::new(dep.src_dir.clone());
        let modules = resolver.resolve_all()?;
        let module_list: Vec<ParsedModule> = modules.into_values().collect();
        dep_modules.insert(dep.name.clone(), module_list);
    }

    Ok(dep_modules)
}

fn find_main_module(modules: &HashMap<String, ParsedModule>) -> Result<String, String> {
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
    local_modules: &HashMap<String, ParsedModule>,
    dep_modules: &HashMap<String, Vec<ParsedModule>>,
    main_module_path: &str,
) -> Result<Program, String> {
    let main_module = local_modules
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

    // Track which uses are local modules vs dependency imports
    let mut local_uses: Vec<String> = Vec::new();
    let mut dep_uses: Vec<String> = Vec::new();

    for use_path in &uses {
        if local_modules.contains_key(use_path) {
            local_uses.push(use_path.clone());
        } else if dep_modules.contains_key(use_path) {
            dep_uses.push(use_path.clone());
        } else {
            return Err(format!(
                "module '{}' not found (not a local module or declared dependency)",
                use_path
            ));
        }
    }

    // Build name_map for rewriting call sites
    let mut name_map: HashMap<String, String> = HashMap::new();

    // Add declarations from imported local modules (pub only, with prefixed names)
    for use_path in &local_uses {
        let imported_module = local_modules.get(use_path).unwrap();
        let prefix = use_path.replace('.', "__");

        for decl in &imported_module.program.declarations {
            add_prefixed_pub_decl(decl, &prefix, &mut merged_decls, &mut name_map)?;
        }
    }

    // Add declarations from imported dependencies (pub only, with dep_name__ prefix)
    for dep_name in &dep_uses {
        let modules = dep_modules.get(dep_name).unwrap();
        for module in modules {
            // For single-module deps, prefix is just dep_name
            // For multi-module deps, prefix is dep_name__submodule
            let prefix = if module.module_path == *dep_name {
                dep_name.clone()
            } else {
                format!("{}__{}", dep_name, module.module_path.replace('.', "__"))
            };

            for decl in &module.program.declarations {
                add_prefixed_pub_decl(decl, &prefix, &mut merged_decls, &mut name_map)?;
            }
        }
    }

    // Add main module declarations (all of them, not just pub)
    merged_decls.extend(main_module.program.declarations.clone());

    // Rewrite type references and call sites in ALL declarations
    // This is needed because imported declarations may reference types from
    // their own module (e.g., `fn origin() -> Point` needs `Point` → `geom__Point`)
    if !name_map.is_empty() {
        for decl in merged_decls.iter_mut() {
            match decl {
                Declaration::Function(f) => {
                    rewrite_fn_types(f, &name_map);
                    rewrite_block_calls(&mut f.body, &name_map);
                }
                Declaration::Impl(i) => {
                    if let Some(new_name) = name_map.get(&i.target_type) {
                        i.target_type = new_name.clone();
                    }
                    for method in &mut i.methods {
                        rewrite_fn_types(method, &name_map);
                        rewrite_block_calls(&mut method.body, &name_map);
                    }
                }
                Declaration::Struct(s) => {
                    for field in &mut s.fields {
                        rewrite_type(&mut field.ty, &name_map);
                    }
                }
                _ => {}
            }
        }
    }

    Ok(Program {
        module_name: main_module.program.module_name.clone(),
        uses: Vec::new(), // Uses are resolved
        declarations: merged_decls,
        span: main_module.program.span,
    })
}

/// Add a public declaration with a prefix to merged_decls and update name_map.
fn add_prefixed_pub_decl(
    decl: &Declaration,
    prefix: &str,
    merged_decls: &mut Vec<Declaration>,
    name_map: &mut HashMap<String, String>,
) -> Result<(), String> {
    match decl {
        Declaration::Function(f) if f.is_pub => {
            let prefixed_name = format!("{}__{}", prefix, f.name);
            if let Some(existing) = name_map.get(&f.name) {
                if *existing != prefixed_name {
                    return Err(format!(
                        "name collision: '{}' exported by both '{}' and '{}'",
                        f.name,
                        existing.replace("__", "."),
                        prefixed_name.replace("__", ".")
                    ));
                }
            }
            name_map.insert(f.name.clone(), prefixed_name.clone());
            let mut prefixed = f.clone();
            prefixed.name = prefixed_name;
            merged_decls.push(Declaration::Function(prefixed));
        }
        Declaration::Struct(s) if s.is_pub => {
            let prefixed_name = format!("{}__{}", prefix, s.name);
            if let Some(existing) = name_map.get(&s.name) {
                if *existing != prefixed_name {
                    return Err(format!(
                        "name collision: '{}' exported by both '{}' and '{}'",
                        s.name,
                        existing.replace("__", "."),
                        prefixed_name.replace("__", ".")
                    ));
                }
            }
            name_map.insert(s.name.clone(), prefixed_name.clone());
            let mut prefixed = s.clone();
            prefixed.name = prefixed_name;
            merged_decls.push(Declaration::Struct(prefixed));
        }
        Declaration::Enum(e) if e.is_pub => {
            let prefixed_name = format!("{}__{}", prefix, e.name);
            if let Some(existing) = name_map.get(&e.name) {
                if *existing != prefixed_name {
                    return Err(format!(
                        "name collision: '{}' exported by both '{}' and '{}'",
                        e.name,
                        existing.replace("__", "."),
                        prefixed_name.replace("__", ".")
                    ));
                }
            }
            name_map.insert(e.name.clone(), prefixed_name.clone());
            let mut prefixed = e.clone();
            prefixed.name = prefixed_name;
            merged_decls.push(Declaration::Enum(prefixed));
        }
        Declaration::Const(c) if c.is_pub => {
            let prefixed_name = format!("{}__{}", prefix, c.name);
            name_map.insert(c.name.clone(), prefixed_name.clone());
            let mut prefixed = c.clone();
            prefixed.name = prefixed_name;
            merged_decls.push(Declaration::Const(prefixed));
        }
        _ => {}
    }
    Ok(())
}

fn rewrite_type(ty: &mut Type, name_map: &HashMap<String, String>) {
    match ty {
        Type::Named(ref mut name) => {
            if let Some(new_name) = name_map.get(name.as_str()) {
                *name = new_name.clone();
            }
        }
        Type::Array(inner)
        | Type::Ref(inner)
        | Type::MutRef(inner)
        | Type::Own(inner)
        | Type::Task(inner)
        | Type::Chan(inner) => rewrite_type(inner, name_map),
        Type::Generic(ref mut name, args) => {
            if let Some(new_name) = name_map.get(name.as_str()) {
                *name = new_name.clone();
            }
            for arg in args {
                rewrite_type(arg, name_map);
            }
        }
        Type::Tuple(elems) => {
            for elem in elems {
                rewrite_type(elem, name_map);
            }
        }
        Type::Fn(params, ret) => {
            for p in params {
                rewrite_type(p, name_map);
            }
            rewrite_type(ret, name_map);
        }
        _ => {}
    }
}

fn rewrite_fn_types(f: &mut FnDecl, name_map: &HashMap<String, String>) {
    rewrite_type(&mut f.return_type, name_map);
    for param in &mut f.params {
        rewrite_type(&mut param.ty, name_map);
    }
}

fn rewrite_block_calls(block: &mut Block, name_map: &HashMap<String, String>) {
    for stmt in &mut block.stmts {
        rewrite_stmt_calls(stmt, name_map);
    }
}

fn rewrite_stmt_calls(stmt: &mut Stmt, name_map: &HashMap<String, String>) {
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

fn rewrite_if_stmt_calls(s: &mut IfStmt, name_map: &HashMap<String, String>) {
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

fn rewrite_expr_calls(expr: &mut Expr, name_map: &HashMap<String, String>) {
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
