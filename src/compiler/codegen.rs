//! LLVM IR code generation for the Yorum language.
//!
//! Emits textual LLVM IR that can be compiled by `llc` + `clang` or consumed
//! by any LLVM-based toolchain.  Uses the alloca/load/store pattern so that
//! LLVM's mem2reg pass promotes stack slots to SSA registers automatically.

use crate::compiler::ast::*;
use std::collections::HashMap;
use std::fmt;

// ═══════════════════════════════════════════════════════════════
//  Error type
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct CodegenError {
    pub message: String,
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "codegen error: {}", self.message)
    }
}

impl std::error::Error for CodegenError {}

// ═══════════════════════════════════════════════════════════════
//  Internal types
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
struct VarSlot {
    ptr: String,    // LLVM alloca name (e.g., "%x.addr")
    llvm_ty: String, // LLVM type string (e.g., "i64")
}

#[derive(Debug, Clone)]
struct StructLayout {
    #[allow(dead_code)]
    name: String,
    fields: Vec<(String, Type)>,
    llvm_ty: String,
}

#[derive(Debug, Clone)]
struct EnumLayout {
    #[allow(dead_code)]
    name: String,
    variants: Vec<(String, Vec<Type>)>,
    #[allow(dead_code)]
    tag_count: usize,
}

// ═══════════════════════════════════════════════════════════════
//  Code generator
// ═══════════════════════════════════════════════════════════════

pub struct Codegen {
    // Output buffers
    preamble: String,
    type_defs: String,
    globals: String,
    body: String,

    // Counters for generating unique names
    temp_counter: u32,
    label_counter: u32,
    string_counter: u32,

    // Symbol tables
    vars: Vec<HashMap<String, VarSlot>>,
    struct_layouts: HashMap<String, StructLayout>,
    enum_layouts: HashMap<String, EnumLayout>,
    fn_ret_types: HashMap<String, Type>,

    // State within a function
    current_fn_ret_ty: Option<String>,
    block_terminated: bool,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            preamble: String::new(),
            type_defs: String::new(),
            globals: String::new(),
            body: String::new(),
            temp_counter: 0,
            label_counter: 0,
            string_counter: 0,
            vars: Vec::new(),
            struct_layouts: HashMap::new(),
            enum_layouts: HashMap::new(),
            fn_ret_types: HashMap::new(),
            current_fn_ret_ty: None,
            block_terminated: false,
        }
    }

    /// Generate LLVM IR for the entire program. Returns the IR as a string.
    pub fn generate(&mut self, program: &Program) -> Result<String, CodegenError> {
        self.emit_preamble(program);
        self.register_types(program);
        self.emit_type_defs();
        self.emit_builtin_decls();
        self.emit_builtin_helpers();

        // Register function return types
        for decl in &program.declarations {
            if let Declaration::Function(f) = decl {
                self.fn_ret_types.insert(f.name.clone(), f.return_type.clone());
            }
        }

        // Emit each function
        for decl in &program.declarations {
            if let Declaration::Function(f) = decl {
                self.emit_function(f)?;
            }
        }

        // Assemble final output
        let mut output = String::new();
        output.push_str(&self.preamble);
        output.push('\n');
        output.push_str(&self.type_defs);
        output.push('\n');
        output.push_str(&self.globals);
        output.push('\n');
        output.push_str(&self.body);
        Ok(output)
    }

    // ── Preamble ─────────────────────────────────────────────

    fn emit_preamble(&mut self, program: &Program) {
        let module_name = program
            .module_name
            .as_deref()
            .unwrap_or("yorum_module");
        self.preamble.push_str(&format!(
            "; ModuleID = '{}'\n\
             source_filename = \"{}.yrm\"\n\n",
            module_name, module_name
        ));
    }

    fn emit_builtin_decls(&mut self) {
        self.globals
            .push_str("; ── External declarations ──\n");
        self.globals
            .push_str("declare i32 @printf(ptr, ...)\n");
        self.globals
            .push_str("declare i32 @puts(ptr)\n\n");

        // Format strings (%lld\n\0 = 6 bytes, %f\n\0 = 4 bytes)
        self.globals.push_str(
            "@.fmt.int = private unnamed_addr constant [6 x i8] c\"%lld\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.fmt.float = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.fmt.true = private unnamed_addr constant [6 x i8] c\"true\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.fmt.false = private unnamed_addr constant [7 x i8] c\"false\\0A\\00\"\n",
        );
        self.globals.push('\n');
    }

    fn emit_builtin_helpers(&mut self) {
        // print_int
        self.body.push_str(
            "define void @print_int(i64 %x) {\n\
             entry:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.int, i64 %x)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // print_float
        self.body.push_str(
            "define void @print_float(double %x) {\n\
             entry:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.float, double %x)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // print_bool
        self.body.push_str(
            "define void @print_bool(i1 %x) {\n\
             entry:\n\
             \x20 br i1 %x, label %is_true, label %is_false\n\
             is_true:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.true)\n\
             \x20 ret void\n\
             is_false:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.false)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // print_str — prints a global string pointer
        self.body.push_str(
            "define void @print_str(ptr %s) {\n\
             entry:\n\
             \x20 call i32 @puts(ptr %s)\n\
             \x20 ret void\n\
             }\n\n",
        );
    }

    // ── Type registration ────────────────────────────────────

    fn register_types(&mut self, program: &Program) {
        for decl in &program.declarations {
            match decl {
                Declaration::Struct(s) => {
                    let field_types: Vec<String> = s
                        .fields
                        .iter()
                        .map(|f| self.llvm_type(&f.ty))
                        .collect();
                    let llvm_ty = format!("{{ {} }}", field_types.join(", "));
                    self.struct_layouts.insert(
                        s.name.clone(),
                        StructLayout {
                            name: s.name.clone(),
                            fields: s
                                .fields
                                .iter()
                                .map(|f| (f.name.clone(), f.ty.clone()))
                                .collect(),
                            llvm_ty,
                        },
                    );
                }
                Declaration::Enum(e) => {
                    self.enum_layouts.insert(
                        e.name.clone(),
                        EnumLayout {
                            name: e.name.clone(),
                            variants: e
                                .variants
                                .iter()
                                .map(|v| (v.name.clone(), v.fields.clone()))
                                .collect(),
                            tag_count: e.variants.len(),
                        },
                    );
                }
                _ => {}
            }
        }
    }

    fn emit_type_defs(&mut self) {
        self.type_defs.push_str("; ── Type definitions ──\n");
        for (name, layout) in &self.struct_layouts {
            self.type_defs
                .push_str(&format!("%{} = type {}\n", name, layout.llvm_ty));
        }
        // Enums are represented as { i32, <largest_payload> }
        // For simple enums (no data), just i32
        for (name, layout) in &self.enum_layouts {
            let has_data = layout.variants.iter().any(|(_, fields)| !fields.is_empty());
            if has_data {
                // Find the largest variant payload size
                let max_payload = layout
                    .variants
                    .iter()
                    .map(|(_, fields)| {
                        fields
                            .iter()
                            .map(|t| self.type_size(t))
                            .sum::<usize>()
                    })
                    .max()
                    .unwrap_or(0);
                let payload_bytes = if max_payload == 0 { 8 } else { max_payload };
                self.type_defs.push_str(&format!(
                    "%{} = type {{ i32, [{} x i8] }}\n",
                    name, payload_bytes
                ));
            } else {
                self.type_defs
                    .push_str(&format!("%{} = type {{ i32 }}\n", name));
            }
        }
        self.type_defs.push('\n');
    }

    // ── Function emission ────────────────────────────────────

    fn emit_function(&mut self, f: &FnDecl) -> Result<(), CodegenError> {
        self.temp_counter = 0;
        self.label_counter = 0;
        self.block_terminated = false;

        let ret_ty = self.llvm_type(&f.return_type);
        self.current_fn_ret_ty = Some(ret_ty.clone());

        let params: Vec<String> = f
            .params
            .iter()
            .map(|p| format!("{} %{}", self.llvm_type(&p.ty), p.name))
            .collect();

        self.body.push_str(&format!(
            "define {} @{}({}) {{\n",
            ret_ty,
            f.name,
            params.join(", ")
        ));
        self.body.push_str("entry:\n");

        self.push_scope();

        // Alloca for each parameter
        for param in &f.params {
            let ty = self.llvm_type(&param.ty);
            let ptr = format!("%{}.addr", param.name);
            self.emit_line(&format!("{} = alloca {}", ptr, ty));
            self.emit_line(&format!("store {} %{}, ptr {}", ty, param.name, ptr));
            self.define_var(&param.name, &ptr, &ty);
        }

        // Emit body statements
        self.emit_block(&f.body)?;

        // If the block didn't terminate, add a default return
        if !self.block_terminated {
            if ret_ty == "void" {
                self.emit_line("ret void");
            } else {
                self.emit_line(&format!("ret {} 0", ret_ty));
            }
        }

        self.pop_scope();
        self.body.push_str("}\n\n");
        self.current_fn_ret_ty = None;
        Ok(())
    }

    fn emit_block(&mut self, block: &Block) -> Result<(), CodegenError> {
        for stmt in &block.stmts {
            if self.block_terminated {
                break; // Dead code after return/branch
            }
            self.emit_stmt(stmt)?;
        }
        Ok(())
    }

    // ── Statement emission ───────────────────────────────────

    fn emit_stmt(&mut self, stmt: &Stmt) -> Result<(), CodegenError> {
        match stmt {
            Stmt::Let(s) => self.emit_let(s),
            Stmt::Assign(s) => self.emit_assign(s),
            Stmt::Return(s) => self.emit_return(s),
            Stmt::If(s) => self.emit_if(s),
            Stmt::While(s) => self.emit_while(s),
            Stmt::Match(s) => self.emit_match(s),
            Stmt::Expr(s) => {
                self.emit_expr(&s.expr)?;
                Ok(())
            }
        }
    }

    fn emit_let(&mut self, s: &LetStmt) -> Result<(), CodegenError> {
        let ty = self.llvm_type(&s.ty);

        // For struct-typed variables, StructInit already creates an alloca and
        // fills in the fields.  Use that alloca directly as the variable slot
        // instead of trying to store a whole struct as a value.
        if let Type::Named(ref name) = s.ty {
            if self.struct_layouts.contains_key(name) {
                let val_ptr = self.emit_expr(&s.value)?;
                self.define_var(&s.name, &val_ptr, &ty);
                return Ok(());
            }
        }

        let ptr = format!("%{}.addr", s.name);
        self.emit_line(&format!("{} = alloca {}", ptr, ty));

        let val = self.emit_expr(&s.value)?;
        self.emit_line(&format!("store {} {}, ptr {}", ty, val, ptr));
        self.define_var(&s.name, &ptr, &ty);
        Ok(())
    }

    fn emit_assign(&mut self, s: &AssignStmt) -> Result<(), CodegenError> {
        match &s.target.kind {
            ExprKind::Ident(name) => {
                let slot = self
                    .lookup_var(name)
                    .ok_or_else(|| CodegenError {
                        message: format!("undefined variable '{}'", name),
                    })?
                    .clone();
                let val = self.emit_expr(&s.value)?;
                self.emit_line(&format!("store {} {}, ptr {}", slot.llvm_ty, val, slot.ptr));
                Ok(())
            }
            ExprKind::FieldAccess(obj, field) => {
                let obj_ptr = self.emit_expr_ptr(obj)?;
                let struct_name = self.expr_struct_name(obj)?;
                let (idx, field_ty) = self.struct_field_index(&struct_name, field)?;
                let fty = self.llvm_type(&field_ty);

                let gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                    gep, struct_name, obj_ptr, idx
                ));
                let val = self.emit_expr(&s.value)?;
                self.emit_line(&format!("store {} {}, ptr {}", fty, val, gep));
                Ok(())
            }
            _ => Err(CodegenError {
                message: "invalid assignment target".to_string(),
            }),
        }
    }

    fn emit_return(&mut self, s: &ReturnStmt) -> Result<(), CodegenError> {
        let ret_ty = self.current_fn_ret_ty.clone().unwrap_or("void".to_string());
        if ret_ty == "void" {
            self.emit_line("ret void");
        } else {
            let val = self.emit_expr(&s.value)?;
            self.emit_line(&format!("ret {} {}", ret_ty, val));
        }
        self.block_terminated = true;
        Ok(())
    }

    fn emit_if(&mut self, s: &IfStmt) -> Result<(), CodegenError> {
        let cond = self.emit_expr(&s.condition)?;
        let then_label = self.fresh_label("then");
        let else_label = self.fresh_label("else");
        let merge_label = self.fresh_label("ifcont");

        let has_else = s.else_branch.is_some();
        let target_else = if has_else {
            &else_label
        } else {
            &merge_label
        };

        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cond, then_label, target_else
        ));

        // Then block
        self.emit_label(&then_label);
        self.block_terminated = false;
        self.emit_block(&s.then_block)?;
        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", merge_label));
        }
        let then_terminated = self.block_terminated;

        // Else block
        if let Some(else_branch) = &s.else_branch {
            self.emit_label(&else_label);
            self.block_terminated = false;
            match else_branch.as_ref() {
                ElseBranch::Else(block) => {
                    self.emit_block(block)?;
                }
                ElseBranch::ElseIf(elif) => {
                    self.emit_if(elif)?;
                }
            }
            if !self.block_terminated {
                self.emit_line(&format!("br label %{}", merge_label));
            }
            let else_terminated = self.block_terminated;

            // Merge block — only needed if at least one branch falls through
            if !then_terminated || !else_terminated {
                self.emit_label(&merge_label);
                self.block_terminated = false;
            } else {
                // Both branches returned; emit merge block as dead code landing pad
                self.emit_label(&merge_label);
                self.block_terminated = false;
            }
        } else {
            // No else branch
            self.emit_label(&merge_label);
            self.block_terminated = false;
        }

        Ok(())
    }

    fn emit_while(&mut self, s: &WhileStmt) -> Result<(), CodegenError> {
        let cond_label = self.fresh_label("while.cond");
        let body_label = self.fresh_label("while.body");
        let end_label = self.fresh_label("while.end");

        self.emit_line(&format!("br label %{}", cond_label));

        // Condition check
        self.emit_label(&cond_label);
        self.block_terminated = false;
        let cond = self.emit_expr(&s.condition)?;
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cond, body_label, end_label
        ));

        // Loop body
        self.emit_label(&body_label);
        self.block_terminated = false;
        self.emit_block(&s.body)?;
        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", cond_label));
        }

        // End
        self.emit_label(&end_label);
        self.block_terminated = false;
        Ok(())
    }

    fn emit_match(&mut self, s: &MatchStmt) -> Result<(), CodegenError> {
        let subject_val = self.emit_expr(&s.subject)?;
        let merge_label = self.fresh_label("match.end");

        // For enums with tag: extract the tag
        // For int literals: compare directly
        let mut arm_labels: Vec<String> = Vec::new();
        for _ in &s.arms {
            arm_labels.push(self.fresh_label("match.arm"));
        }
        let default_label = self.fresh_label("match.default");

        // Emit cascading comparisons
        for (i, arm) in s.arms.iter().enumerate() {
            match &arm.pattern {
                Pattern::Literal(Literal::Int(n), _) => {
                    let cmp = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = icmp eq i64 {}, {}",
                        cmp, subject_val, n
                    ));
                    let next = if i + 1 < s.arms.len() {
                        format!("match.check.{}", i + 1)
                    } else {
                        default_label.clone()
                    };
                    self.emit_line(&format!(
                        "br i1 {}, label %{}, label %{}",
                        cmp, arm_labels[i], next
                    ));
                    if i + 1 < s.arms.len() {
                        self.emit_label(&next);
                        self.block_terminated = false;
                    }
                }
                Pattern::Wildcard(_) | Pattern::Binding(_, _) => {
                    self.emit_line(&format!("br label %{}", arm_labels[i]));
                }
                Pattern::Variant(vname, _, _) => {
                    // Extract tag and compare
                    let tag = self.variant_tag(vname)?;
                    let cmp = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = icmp eq i32 {}, {}",
                        cmp, subject_val, tag
                    ));
                    let next = if i + 1 < s.arms.len() {
                        format!("match.check.{}", i + 1)
                    } else {
                        default_label.clone()
                    };
                    self.emit_line(&format!(
                        "br i1 {}, label %{}, label %{}",
                        cmp, arm_labels[i], next
                    ));
                    if i + 1 < s.arms.len() {
                        self.emit_label(&next);
                        self.block_terminated = false;
                    }
                }
                _ => {
                    self.emit_line(&format!("br label %{}", arm_labels[i]));
                }
            }
        }

        // Default (unreachable if match is exhaustive)
        self.emit_label(&default_label);
        self.emit_line("unreachable");

        // Emit arm bodies
        for (i, arm) in s.arms.iter().enumerate() {
            self.emit_label(&arm_labels[i]);
            self.block_terminated = false;

            self.push_scope();

            // Bind pattern variables
            if let Pattern::Binding(name, _) = &arm.pattern {
                let ty_str = "i64".to_string(); // default to i64 for now
                let ptr = format!("%{}.match.addr", name);
                self.emit_line(&format!("{} = alloca {}", ptr, ty_str));
                self.emit_line(&format!(
                    "store {} {}, ptr {}",
                    ty_str, subject_val, ptr
                ));
                self.define_var(name, &ptr, &ty_str);
            }

            self.emit_block(&arm.body)?;
            if !self.block_terminated {
                self.emit_line(&format!("br label %{}", merge_label));
            }
            self.pop_scope();
        }

        self.emit_label(&merge_label);
        self.block_terminated = false;
        Ok(())
    }

    // ── Expression emission ──────────────────────────────────
    // Each `emit_expr` returns the LLVM value name (SSA temp or literal).

    fn emit_expr(&mut self, expr: &Expr) -> Result<String, CodegenError> {
        match &expr.kind {
            ExprKind::Literal(lit) => self.emit_literal(lit),

            ExprKind::Ident(name) => {
                let slot = self
                    .lookup_var(name)
                    .ok_or_else(|| CodegenError {
                        message: format!("undefined variable '{}'", name),
                    })?
                    .clone();
                let tmp = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = load {}, ptr {}",
                    tmp, slot.llvm_ty, slot.ptr
                ));
                Ok(tmp)
            }

            ExprKind::Binary(lhs, op, rhs) => {
                let l = self.emit_expr(lhs)?;
                let r = self.emit_expr(rhs)?;
                let tmp = self.fresh_temp();

                // Determine the operand type from the LHS
                let is_float = self.expr_is_float(lhs);

                let instr = match (op, is_float) {
                    (BinOp::Add, false) => format!("{} = add i64 {}, {}", tmp, l, r),
                    (BinOp::Add, true) => format!("{} = fadd double {}, {}", tmp, l, r),
                    (BinOp::Sub, false) => format!("{} = sub i64 {}, {}", tmp, l, r),
                    (BinOp::Sub, true) => format!("{} = fsub double {}, {}", tmp, l, r),
                    (BinOp::Mul, false) => format!("{} = mul i64 {}, {}", tmp, l, r),
                    (BinOp::Mul, true) => format!("{} = fmul double {}, {}", tmp, l, r),
                    (BinOp::Div, false) => format!("{} = sdiv i64 {}, {}", tmp, l, r),
                    (BinOp::Div, true) => format!("{} = fdiv double {}, {}", tmp, l, r),
                    (BinOp::Mod, false) => format!("{} = srem i64 {}, {}", tmp, l, r),
                    (BinOp::Mod, true) => format!("{} = frem double {}, {}", tmp, l, r),
                    (BinOp::Eq, false) => format!("{} = icmp eq i64 {}, {}", tmp, l, r),
                    (BinOp::Eq, true) => format!("{} = fcmp oeq double {}, {}", tmp, l, r),
                    (BinOp::NotEq, false) => format!("{} = icmp ne i64 {}, {}", tmp, l, r),
                    (BinOp::NotEq, true) => format!("{} = fcmp one double {}, {}", tmp, l, r),
                    (BinOp::Lt, false) => format!("{} = icmp slt i64 {}, {}", tmp, l, r),
                    (BinOp::Lt, true) => format!("{} = fcmp olt double {}, {}", tmp, l, r),
                    (BinOp::Gt, false) => format!("{} = icmp sgt i64 {}, {}", tmp, l, r),
                    (BinOp::Gt, true) => format!("{} = fcmp ogt double {}, {}", tmp, l, r),
                    (BinOp::LtEq, false) => format!("{} = icmp sle i64 {}, {}", tmp, l, r),
                    (BinOp::LtEq, true) => format!("{} = fcmp ole double {}, {}", tmp, l, r),
                    (BinOp::GtEq, false) => format!("{} = icmp sge i64 {}, {}", tmp, l, r),
                    (BinOp::GtEq, true) => format!("{} = fcmp oge double {}, {}", tmp, l, r),
                    (BinOp::And, _) => format!("{} = and i1 {}, {}", tmp, l, r),
                    (BinOp::Or, _) => format!("{} = or i1 {}, {}", tmp, l, r),
                };
                self.emit_line(&instr);
                Ok(tmp)
            }

            ExprKind::Unary(op, operand) => {
                let val = self.emit_expr(operand)?;
                let tmp = self.fresh_temp();
                let is_float = self.expr_is_float(operand);
                match op {
                    UnaryOp::Neg if is_float => {
                        self.emit_line(&format!("{} = fneg double {}", tmp, val));
                    }
                    UnaryOp::Neg => {
                        self.emit_line(&format!("{} = sub i64 0, {}", tmp, val));
                    }
                    UnaryOp::Not => {
                        self.emit_line(&format!("{} = xor i1 {}, true", tmp, val));
                    }
                }
                Ok(tmp)
            }

            ExprKind::Call(callee, args) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    let mut arg_strs = Vec::new();
                    for arg in args {
                        let val = self.emit_expr(arg)?;
                        let ty = self.expr_llvm_type(arg);
                        arg_strs.push(format!("{} {}", ty, val));
                    }

                    let ret_ty = self
                        .fn_ret_types
                        .get(name.as_str())
                        .cloned()
                        .unwrap_or(Type::Unit);
                    let llvm_ret = self.llvm_type(&ret_ty);

                    if llvm_ret == "void" {
                        self.emit_line(&format!(
                            "call void @{}({})",
                            name,
                            arg_strs.join(", ")
                        ));
                        Ok("void".to_string())
                    } else {
                        let tmp = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call {} @{}({})",
                            tmp,
                            llvm_ret,
                            name,
                            arg_strs.join(", ")
                        ));
                        Ok(tmp)
                    }
                } else {
                    Err(CodegenError {
                        message: "indirect calls not supported".to_string(),
                    })
                }
            }

            ExprKind::FieldAccess(obj, field) => {
                let obj_ptr = self.emit_expr_ptr(obj)?;
                let struct_name = self.expr_struct_name(obj)?;
                let (idx, field_ty) = self.struct_field_index(&struct_name, field)?;
                let fty = self.llvm_type(&field_ty);

                let gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                    gep, struct_name, obj_ptr, idx
                ));
                let val = self.fresh_temp();
                self.emit_line(&format!("{} = load {}, ptr {}", val, fty, gep));
                Ok(val)
            }

            ExprKind::StructInit(name, fields) => {
                let _layout = self.struct_layouts.get(name).cloned().ok_or_else(|| {
                    CodegenError {
                        message: format!("undefined struct '{}'", name),
                    }
                })?;
                let ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca %{}", ptr, name));

                for fi in fields {
                    let (idx, field_ty) = self.struct_field_index(name, &fi.name)?;
                    let fty = self.llvm_type(&field_ty);
                    let val = self.emit_expr(&fi.value)?;
                    let gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                        gep, name, ptr, idx
                    ));
                    self.emit_line(&format!("store {} {}, ptr {}", fty, val, gep));
                }

                // Return the pointer as the struct value
                Ok(ptr)
            }

            ExprKind::Index(_, _) => Err(CodegenError {
                message: "array indexing not yet implemented in codegen".to_string(),
            }),
        }
    }

    /// Emit an expression and return its alloca pointer (for lvalues).
    fn emit_expr_ptr(&mut self, expr: &Expr) -> Result<String, CodegenError> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                let slot = self.lookup_var(name).ok_or_else(|| CodegenError {
                    message: format!("undefined variable '{}'", name),
                })?;
                Ok(slot.ptr.clone())
            }
            _ => {
                // For complex expressions, evaluate to a temp alloca
                self.emit_expr(expr)
            }
        }
    }

    fn emit_literal(&mut self, lit: &Literal) -> Result<String, CodegenError> {
        match lit {
            Literal::Int(n) => Ok(n.to_string()),
            Literal::Float(f) => {
                // LLVM requires hex float format for exact representation
                Ok(format!("{:.6e}", f))
            }
            Literal::Bool(b) => Ok(if *b { "true".to_string() } else { "false".to_string() }),
            Literal::String(s) => {
                // Emit as a global constant
                let id = self.string_counter;
                self.string_counter += 1;
                let escaped = self.escape_llvm_string(s);
                let len = s.len() + 1; // +1 for null terminator
                let global_name = format!("@.str.{}", id);
                self.globals.push_str(&format!(
                    "{} = private unnamed_addr constant [{} x i8] c\"{}\\00\"\n",
                    global_name, len, escaped
                ));
                Ok(global_name)
            }
        }
    }

    // ── Type helpers ─────────────────────────────────────────

    fn llvm_type(&self, ty: &Type) -> String {
        match ty {
            Type::Int => "i64".to_string(),
            Type::Float => "double".to_string(),
            Type::Bool => "i1".to_string(),
            Type::Str => "ptr".to_string(),
            Type::Unit => "void".to_string(),
            Type::Named(name) => {
                if self.struct_layouts.contains_key(name) || self.enum_layouts.contains_key(name) {
                    format!("%{}", name)
                } else {
                    "i64".to_string() // fallback
                }
            }
            Type::Array(_) => "ptr".to_string(),
            Type::Ref(_) | Type::MutRef(_) | Type::Own(_) => "ptr".to_string(),
        }
    }

    fn type_size(&self, ty: &Type) -> usize {
        match ty {
            Type::Int => 8,
            Type::Float => 8,
            Type::Bool => 1,
            Type::Str => 8,
            Type::Unit => 0,
            Type::Named(name) => {
                if let Some(layout) = self.struct_layouts.get(name) {
                    layout.fields.iter().map(|(_, t)| self.type_size(t)).sum()
                } else {
                    8
                }
            }
            _ => 8,
        }
    }

    /// Heuristic to determine if an expression produces a float value.
    fn expr_is_float(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Literal(Literal::Float(_)) => true,
            ExprKind::Literal(Literal::Int(_)) => false,
            ExprKind::Literal(Literal::Bool(_)) => false,
            ExprKind::Literal(Literal::String(_)) => false,
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    slot.llvm_ty == "double"
                } else {
                    false
                }
            }
            ExprKind::Binary(lhs, op, _) => {
                // Comparison operators always produce bool
                match op {
                    BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::LtEq
                    | BinOp::GtEq | BinOp::And | BinOp::Or => false,
                    _ => self.expr_is_float(lhs),
                }
            }
            ExprKind::Unary(UnaryOp::Not, _) => false,
            ExprKind::Unary(_, operand) => self.expr_is_float(operand),
            ExprKind::Call(callee, _) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    if let Some(ret_ty) = self.fn_ret_types.get(name.as_str()) {
                        return *ret_ty == Type::Float;
                    }
                }
                false
            }
            ExprKind::FieldAccess(obj, field) => {
                if let Ok(sname) = self.expr_struct_name(obj) {
                    if let Ok((_, fty)) = self.struct_field_index(&sname, field) {
                        return fty == Type::Float;
                    }
                }
                false
            }
            _ => false,
        }
    }

    fn expr_llvm_type(&self, expr: &Expr) -> String {
        match &expr.kind {
            ExprKind::Literal(Literal::Int(_)) => "i64".to_string(),
            ExprKind::Literal(Literal::Float(_)) => "double".to_string(),
            ExprKind::Literal(Literal::Bool(_)) => "i1".to_string(),
            ExprKind::Literal(Literal::String(_)) => "ptr".to_string(),
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    slot.llvm_ty.clone()
                } else {
                    "i64".to_string()
                }
            }
            ExprKind::Binary(_, op, _) => match op {
                BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::LtEq
                | BinOp::GtEq | BinOp::And | BinOp::Or => "i1".to_string(),
                _ => {
                    if self.expr_is_float(expr) {
                        "double".to_string()
                    } else {
                        "i64".to_string()
                    }
                }
            },
            ExprKind::Unary(UnaryOp::Not, _) => "i1".to_string(),
            ExprKind::Unary(_, operand) => self.expr_llvm_type(operand),
            ExprKind::Call(callee, _) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    if let Some(ret_ty) = self.fn_ret_types.get(name.as_str()) {
                        return self.llvm_type(ret_ty);
                    }
                }
                "i64".to_string()
            }
            _ => "i64".to_string(),
        }
    }

    fn expr_struct_name(&self, expr: &Expr) -> Result<String, CodegenError> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    let ty = &slot.llvm_ty;
                    if ty.starts_with('%') {
                        Ok(ty[1..].to_string())
                    } else {
                        Err(CodegenError {
                            message: format!("'{}' is not a struct", name),
                        })
                    }
                } else {
                    Err(CodegenError {
                        message: format!("undefined variable '{}'", name),
                    })
                }
            }
            _ => Err(CodegenError {
                message: "cannot determine struct name from expression".to_string(),
            }),
        }
    }

    fn struct_field_index(
        &self,
        struct_name: &str,
        field_name: &str,
    ) -> Result<(usize, Type), CodegenError> {
        let layout = self.struct_layouts.get(struct_name).ok_or_else(|| {
            CodegenError {
                message: format!("undefined struct '{}'", struct_name),
            }
        })?;
        for (i, (fname, fty)) in layout.fields.iter().enumerate() {
            if fname == field_name {
                return Ok((i, fty.clone()));
            }
        }
        Err(CodegenError {
            message: format!("struct '{}' has no field '{}'", struct_name, field_name),
        })
    }

    fn variant_tag(&self, variant_name: &str) -> Result<i32, CodegenError> {
        for (_ename, layout) in &self.enum_layouts {
            for (i, (vname, _)) in layout.variants.iter().enumerate() {
                if vname == variant_name {
                    return Ok(i as i32);
                }
            }
        }
        Err(CodegenError {
            message: format!("unknown variant '{}'", variant_name),
        })
    }

    // ── Variable scope management ────────────────────────────

    fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.vars.pop();
    }

    fn define_var(&mut self, name: &str, ptr: &str, llvm_ty: &str) {
        if let Some(scope) = self.vars.last_mut() {
            scope.insert(
                name.to_string(),
                VarSlot {
                    ptr: ptr.to_string(),
                    llvm_ty: llvm_ty.to_string(),
                },
            );
        }
    }

    fn lookup_var(&self, name: &str) -> Option<&VarSlot> {
        for scope in self.vars.iter().rev() {
            if let Some(slot) = scope.get(name) {
                return Some(slot);
            }
        }
        None
    }

    // ── Output helpers ───────────────────────────────────────

    fn emit_line(&mut self, line: &str) {
        self.body.push_str("  ");
        self.body.push_str(line);
        self.body.push('\n');
    }

    fn emit_label(&mut self, label: &str) {
        self.body.push_str(label);
        self.body.push_str(":\n");
        self.block_terminated = false;
    }

    fn fresh_temp(&mut self) -> String {
        let n = self.temp_counter;
        self.temp_counter += 1;
        format!("%t{}", n)
    }

    fn fresh_label(&mut self, prefix: &str) -> String {
        let n = self.label_counter;
        self.label_counter += 1;
        format!("{}.{}", prefix, n)
    }

    fn escape_llvm_string(&self, s: &str) -> String {
        let mut out = String::new();
        for c in s.chars() {
            match c {
                '\n' => out.push_str("\\0A"),
                '\t' => out.push_str("\\09"),
                '\r' => out.push_str("\\0D"),
                '\\' => out.push_str("\\5C"),
                '"' => out.push_str("\\22"),
                '\0' => out.push_str("\\00"),
                c if c.is_ascii() && !c.is_ascii_control() => out.push(c),
                c => {
                    // Encode as UTF-8 hex bytes
                    let mut buf = [0u8; 4];
                    let encoded = c.encode_utf8(&mut buf);
                    for b in encoded.bytes() {
                        out.push_str(&format!("\\{:02X}", b));
                    }
                }
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::lexer::Lexer;
    use crate::compiler::parser::Parser;

    fn compile_to_ir(input: &str) -> String {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();
        let mut codegen = Codegen::new();
        codegen.generate(&program).unwrap()
    }

    #[test]
    fn test_simple_function() {
        let ir = compile_to_ir("fn main() -> int { return 42; }");
        assert!(ir.contains("define i64 @main()"));
        assert!(ir.contains("ret i64 42"));
    }

    #[test]
    fn test_arithmetic() {
        let ir = compile_to_ir("fn add(a: int, b: int) -> int { return a + b; }");
        assert!(ir.contains("define i64 @add(i64 %a, i64 %b)"));
        assert!(ir.contains("add i64"));
    }

    #[test]
    fn test_if_else() {
        let ir = compile_to_ir(
            "fn abs(x: int) -> int { if x < 0 { return 0 - x; } else { return x; } }",
        );
        assert!(ir.contains("icmp slt"));
        assert!(ir.contains("br i1"));
    }

    #[test]
    fn test_while_loop() {
        let ir = compile_to_ir(
            "fn countdown(n: int) -> int { let mut x: int = n; while x > 0 { x = x - 1; } return x; }",
        );
        assert!(ir.contains("while.cond"));
        assert!(ir.contains("while.body"));
        assert!(ir.contains("while.end"));
    }

    #[test]
    fn test_function_call() {
        let ir = compile_to_ir(
            "fn double(x: int) -> int { return x * 2; } fn main() -> int { return double(21); }",
        );
        assert!(ir.contains("call i64 @double(i64 21)"));
    }
}
