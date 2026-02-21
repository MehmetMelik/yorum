use super::*;

impl Codegen {
    pub(crate) fn llvm_type_size(&self, llvm_ty: &str) -> usize {
        match llvm_ty {
            "i64" => 8,
            "double" => 8,
            "i1" => 1,
            "i8" => 1,
            "ptr" => 8,
            "i32" => 4,
            _ => {
                if let Some(name) = llvm_ty.strip_prefix('%') {
                    // Handle struct types like %Token
                    if let Some(layout) = self.struct_layouts.get(name) {
                        return layout.fields.iter().map(|(_, t)| self.type_size(t)).sum();
                    }
                    // Handle tuple types like %tuple.int.string
                    if let Some(elem_types) = self.tuple_elem_types.get(name) {
                        return elem_types.iter().map(|t| self.llvm_type_size(t)).sum();
                    }
                }
                8
            }
        }
    }

    pub(crate) fn is_aggregate_type(llvm_ty: &str) -> bool {
        llvm_ty.starts_with('%')
    }

    // ── Constant folding ─────────────────────────────────────

    /// Try to evaluate a constant expression at compile time.
    /// Returns the LLVM literal string if fully foldable, None otherwise.
    pub(crate) fn try_const_fold(expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Literal(Literal::Int(n)) => Some(n.to_string()),
            ExprKind::Literal(Literal::Bool(b)) => {
                Some(if *b { "true" } else { "false" }.to_string())
            }
            ExprKind::Unary(UnaryOp::Neg, operand) => {
                if let Some(val) = Self::try_const_fold(operand) {
                    val.parse::<i64>()
                        .ok()
                        .map(|n| n.wrapping_neg().to_string())
                } else {
                    None
                }
            }
            ExprKind::Unary(UnaryOp::Not, operand) => {
                match Self::try_const_fold(operand)?.as_str() {
                    "true" => Some("false".to_string()),
                    "false" => Some("true".to_string()),
                    _ => None,
                }
            }
            ExprKind::Binary(lhs, op, rhs) => {
                let l = Self::try_const_fold(lhs)?;
                let r = Self::try_const_fold(rhs)?;
                // Try int arithmetic
                if let (Ok(li), Ok(ri)) = (l.parse::<i64>(), r.parse::<i64>()) {
                    return Self::fold_binary_int(li, op, ri);
                }
                // Try bool logic
                let lb = match l.as_str() {
                    "true" => Some(true),
                    "false" => Some(false),
                    _ => None,
                };
                let rb = match r.as_str() {
                    "true" => Some(true),
                    "false" => Some(false),
                    _ => None,
                };
                if let (Some(lb), Some(rb)) = (lb, rb) {
                    return match op {
                        BinOp::And => Some(if lb && rb { "true" } else { "false" }.to_string()),
                        BinOp::Or => Some(if lb || rb { "true" } else { "false" }.to_string()),
                        BinOp::Eq => Some(if lb == rb { "true" } else { "false" }.to_string()),
                        BinOp::NotEq => Some(if lb != rb { "true" } else { "false" }.to_string()),
                        _ => None,
                    };
                }
                None
            }
            _ => None,
        }
    }

    /// Fold a binary operation on two integer constants.
    pub(crate) fn fold_binary_int(l: i64, op: &BinOp, r: i64) -> Option<String> {
        match op {
            BinOp::Add => Some(l.wrapping_add(r).to_string()),
            BinOp::Sub => Some(l.wrapping_sub(r).to_string()),
            BinOp::Mul => Some(l.wrapping_mul(r).to_string()),
            BinOp::Div => {
                if r == 0 {
                    None
                } else {
                    l.checked_div(r).map(|v| v.to_string())
                }
            }
            BinOp::Mod => {
                if r == 0 {
                    None
                } else {
                    l.checked_rem(r).map(|v| v.to_string())
                }
            }
            BinOp::BitAnd => Some((l & r).to_string()),
            BinOp::BitOr => Some((l | r).to_string()),
            BinOp::BitXor => Some((l ^ r).to_string()),
            BinOp::Shl => Some(l.wrapping_shl(r as u32).to_string()),
            BinOp::Shr => Some(l.wrapping_shr(r as u32).to_string()),
            BinOp::Eq => Some(if l == r { "true" } else { "false" }.to_string()),
            BinOp::NotEq => Some(if l != r { "true" } else { "false" }.to_string()),
            BinOp::Lt => Some(if l < r { "true" } else { "false" }.to_string()),
            BinOp::Gt => Some(if l > r { "true" } else { "false" }.to_string()),
            BinOp::LtEq => Some(if l <= r { "true" } else { "false" }.to_string()),
            BinOp::GtEq => Some(if l >= r { "true" } else { "false" }.to_string()),
            BinOp::And | BinOp::Or => None, // and/or on ints not applicable
        }
    }

    // ── Tuple helpers ─────────────────────────────────────

    /// Generate a stable LLVM type name for a tuple type.
    pub(crate) fn tuple_type_name(&self, types: &[Type]) -> String {
        let parts: Vec<String> = types
            .iter()
            .map(|t| match t {
                Type::Int => "int".to_string(),
                Type::Float => "float".to_string(),
                Type::Bool => "bool".to_string(),
                Type::Char => "char".to_string(),
                Type::Str => "string".to_string(),
                Type::Unit => "unit".to_string(),
                Type::Named(n) => n.clone(),
                Type::Tuple(inner) => self.tuple_type_name(inner),
                _ => format!("{}", t),
            })
            .collect();
        format!("tuple.{}", parts.join("."))
    }

    /// Ensure a tuple type definition is emitted and element types are tracked.
    pub(crate) fn ensure_tuple_type(&mut self, types: &[Type]) {
        let type_name = self.tuple_type_name(types);
        if self.tuple_elem_types.contains_key(&type_name) {
            return;
        }
        let llvm_elem_types: Vec<String> = types.iter().map(|t| self.llvm_type(t)).collect();
        self.ensure_pipeline_tuple_type(&type_name, &llvm_elem_types);
    }

    /// Register a tuple type from pre-computed LLVM type strings.
    /// Used by pipeline preamble and steps where LLVM types are already known.
    pub(crate) fn ensure_pipeline_tuple_type(&mut self, name: &str, llvm_types: &[String]) {
        if self.tuple_elem_types.contains_key(name) {
            return;
        }
        let type_def = format!("%{} = type {{ {} }}\n", name, llvm_types.join(", "));
        if !self.type_defs.contains(&format!("%{} = type", name)) {
            self.type_defs.push_str(&type_def);
        }
        self.tuple_elem_types
            .insert(name.to_string(), llvm_types.to_vec());
    }

    /// Emit a tuple literal: alloca the tuple struct, store each element.
    pub(crate) fn emit_tuple_lit(&mut self, elements: &[Expr]) -> Result<String, CodegenError> {
        // Compute element LLVM types
        let elem_types: Vec<String> = elements.iter().map(|e| self.expr_llvm_type(e)).collect();
        let type_name = format!(
            "tuple.{}",
            elem_types
                .iter()
                .map(|t| llvm_to_semantic_name(t))
                .collect::<Vec<_>>()
                .join(".")
        );

        // Ensure the tuple type is defined and element types are tracked
        let llvm_fields = elem_types.join(", ");
        let type_def = format!("%{} = type {{ {} }}\n", type_name, llvm_fields);
        if !self.type_defs.contains(&format!("%{} = type", type_name)) {
            self.type_defs.push_str(&type_def);
        }
        self.tuple_elem_types
            .insert(type_name.clone(), elem_types.clone());

        // Alloca the tuple
        let ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca %{}", ptr, type_name));

        // Store each element
        for (i, elem) in elements.iter().enumerate() {
            let val = self.emit_expr(elem)?;
            self.emit_struct_field_store(&type_name, &ptr, i, &elem_types[i], &val);
        }

        Ok(ptr)
    }

    // ── Type helpers ─────────────────────────────────────────

    pub(crate) fn llvm_type(&self, ty: &Type) -> String {
        match ty {
            Type::Int => "i64".to_string(),
            Type::Float => "double".to_string(),
            Type::Bool => "i1".to_string(),
            Type::Char => "i8".to_string(),
            Type::Str => "ptr".to_string(),
            Type::Unit => "void".to_string(),
            Type::Named(name) => {
                if self.struct_layouts.contains_key(name) || self.enum_layouts.contains_key(name) {
                    format!("%{}", name)
                } else if name.starts_with("Map__") || name.starts_with("Set__") {
                    "ptr".to_string()
                } else {
                    "i64".to_string() // fallback
                }
            }
            Type::Tuple(types) => {
                let name = self.tuple_type_name(types);
                format!("%{}", name)
            }
            Type::Array(_) => "ptr".to_string(),
            Type::Ref(_) | Type::MutRef(_) | Type::Own(_) => "ptr".to_string(),
            Type::SelfType | Type::TypeVar(_) => {
                debug_assert!(
                    false,
                    "Type::SelfType/TypeVar should be resolved before codegen"
                );
                "i64".to_string()
            }
            Type::Generic(name, _) => {
                if name == "Map" || name == "Set" {
                    "ptr".to_string()
                } else {
                    format!("%{}", name)
                }
            }
            Type::Fn(_, _) => "ptr".to_string(),
            Type::Task(_) => "ptr".to_string(),
            Type::Chan(_) => "ptr".to_string(),
        }
    }

    pub(crate) fn type_size(&self, ty: &Type) -> usize {
        match ty {
            Type::Int => 8,
            Type::Float => 8,
            Type::Bool => 1,
            Type::Char => 1,
            Type::Str => 8,
            Type::Unit => 0,
            Type::Named(name) => {
                if let Some(layout) = self.struct_layouts.get(name) {
                    layout.fields.iter().map(|(_, t)| self.type_size(t)).sum()
                } else {
                    8
                }
            }
            Type::SelfType | Type::TypeVar(_) => {
                debug_assert!(
                    false,
                    "Type::SelfType/TypeVar should be resolved before codegen"
                );
                8
            }
            Type::Generic(_, _) | Type::Fn(_, _) => 8,
            _ => 8,
        }
    }

    /// Heuristic to determine if an expression produces a float value.
    pub(crate) fn expr_is_float(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Literal(Literal::Float(_)) => true,
            ExprKind::Literal(Literal::Int(_)) => false,
            ExprKind::Literal(Literal::Bool(_)) => false,
            ExprKind::Literal(Literal::Char(_)) => false,
            ExprKind::Literal(Literal::String(_)) => false,
            ExprKind::Literal(Literal::Unit) => false,
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
                    BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::LtEq
                    | BinOp::GtEq
                    | BinOp::And
                    | BinOp::Or
                    | BinOp::BitAnd
                    | BinOp::BitOr
                    | BinOp::BitXor
                    | BinOp::Shl
                    | BinOp::Shr => false,
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
                    // Tuple field access
                    if sname.starts_with("tuple.") {
                        if let Ok(idx) = field.parse::<usize>() {
                            if let Some(elem_types) = self.tuple_elem_types.get(&sname) {
                                if idx < elem_types.len() {
                                    return elem_types[idx] == "double";
                                }
                            }
                        }
                        return false;
                    }
                    if let Ok((_, fty)) = self.struct_field_index(&sname, field) {
                        return fty == Type::Float;
                    }
                }
                false
            }
            ExprKind::MethodCall(receiver, method_name, _) => {
                if let Ok(sname) = self.expr_struct_name(receiver) {
                    let mangled = format!("{}_{}", sname, method_name);
                    if let Some(ret_ty) = self.fn_ret_types.get(&mangled) {
                        return *ret_ty == Type::Float;
                    }
                }
                false
            }
            ExprKind::ArrayLit(_) => false,
            ExprKind::Index(arr_expr, _) => {
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    if let Some(elem_ty) = self.array_elem_types.get(name) {
                        return elem_ty == "double";
                    }
                }
                false
            }
            ExprKind::Closure(_) => false,
            _ => false,
        }
    }

    /// Heuristic to determine if an expression produces a char (i8) value.
    pub(crate) fn expr_is_char(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Literal(Literal::Char(_)) => true,
            ExprKind::Literal(_) => false,
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    slot.llvm_ty == "i8"
                } else {
                    false
                }
            }
            ExprKind::Call(callee, _) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    if let Some(ret_ty) = self.fn_ret_types.get(name.as_str()) {
                        return *ret_ty == Type::Char;
                    }
                }
                false
            }
            ExprKind::Index(arr_expr, _) => {
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    if let Some(elem_ty) = self.array_elem_types.get(name) {
                        return elem_ty == "i8";
                    }
                }
                false
            }
            _ => false,
        }
    }

    pub(crate) fn expr_llvm_type(&self, expr: &Expr) -> String {
        match &expr.kind {
            ExprKind::Literal(Literal::Int(_)) => "i64".to_string(),
            ExprKind::Literal(Literal::Float(_)) => "double".to_string(),
            ExprKind::Literal(Literal::Bool(_)) => "i1".to_string(),
            ExprKind::Literal(Literal::Char(_)) => "i8".to_string(),
            ExprKind::Literal(Literal::String(_)) => "ptr".to_string(),
            ExprKind::Literal(Literal::Unit) => "void".to_string(),
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    // Array idents are returned as raw pointers by emit_expr (reference semantics)
                    if slot.llvm_ty == "{ ptr, i64, i64 }" {
                        "ptr".to_string()
                    } else {
                        slot.llvm_ty.clone()
                    }
                } else {
                    "i64".to_string()
                }
            }
            ExprKind::Binary(_, op, _) => match op {
                BinOp::Eq
                | BinOp::NotEq
                | BinOp::Lt
                | BinOp::Gt
                | BinOp::LtEq
                | BinOp::GtEq
                | BinOp::And
                | BinOp::Or => "i1".to_string(),
                BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::Shl | BinOp::Shr => {
                    "i64".to_string()
                }
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
            ExprKind::FieldAccess(obj, field) => {
                if let Ok(sname) = self.expr_struct_name(obj) {
                    // Tuple field access
                    if sname.starts_with("tuple.") {
                        if let Ok(idx) = field.parse::<usize>() {
                            if let Some(elem_types) = self.tuple_elem_types.get(&sname) {
                                if idx < elem_types.len() {
                                    return elem_types[idx].clone();
                                }
                            }
                        }
                        return "i64".to_string();
                    }
                    if let Ok((_, fty)) = self.struct_field_index(&sname, field) {
                        return self.llvm_type(&fty);
                    }
                }
                "i64".to_string()
            }
            ExprKind::MethodCall(receiver, method_name, args) => {
                // Pipeline terminators
                if Self::has_iter_base(receiver) {
                    match method_name.as_str() {
                        "collect" => return "ptr".to_string(), // array fat pointer
                        "any" | "all" => return "i1".to_string(),
                        "fold" => {
                            // Return type matches the closure return type
                            if args.len() == 2 {
                                if let ExprKind::Closure(c) = &args[1].kind {
                                    return self.llvm_type(&c.return_type);
                                }
                            }
                            return self.expr_llvm_type(&args[0]);
                        }
                        "find" | "reduce" => {
                            // Returns Option<T> — determine T from closure param
                            if let Some(first_arg) = args.first() {
                                if let ExprKind::Closure(c) = &first_arg.kind {
                                    if let Some(param) = c.params.first() {
                                        let elem_llvm = self.llvm_type(&param.ty);
                                        let opt_name = self.yorum_type_to_option_name(&elem_llvm);
                                        return format!("%{}", opt_name);
                                    }
                                }
                            }
                            return "ptr".to_string();
                        }
                        _ => {}
                    }
                }
                // Option/Result boolean methods are special-cased in emit_expr
                // and return i1 (icmp result), not registered in fn_ret_types
                match method_name.as_str() {
                    "is_some" | "is_none" | "is_ok" | "is_err" => return "i1".to_string(),
                    _ => {}
                }
                if let Ok(sname) = self.expr_struct_name(receiver) {
                    let mangled = format!("{}_{}", sname, method_name);
                    if let Some(ret_ty) = self.fn_ret_types.get(&mangled) {
                        return self.llvm_type(ret_ty);
                    }
                }
                "i64".to_string()
            }
            ExprKind::ArrayLit(_) => "{ ptr, i64, i64 }".to_string(),
            ExprKind::Index(arr_expr, _) => {
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    if let Some(elem_ty) = self.array_elem_types.get(name) {
                        return elem_ty.clone();
                    }
                }
                "i64".to_string()
            }
            ExprKind::StructInit(name, _) => format!("%{}", name),
            ExprKind::Closure(_) => "ptr".to_string(),
            ExprKind::TupleLit(elements) => {
                // Infer element types and construct the tuple type name using semantic names
                // (matching emit_tuple_lit and tuple_type_name conventions)
                let elem_types: Vec<String> =
                    elements.iter().map(|e| self.expr_llvm_type(e)).collect();
                let semantic_name = elem_types
                    .iter()
                    .map(|t| llvm_to_semantic_name(t))
                    .collect::<Vec<_>>()
                    .join(".");
                format!("%tuple.{}", semantic_name)
            }
            ExprKind::Spawn(_) => "ptr".to_string(),
            ExprKind::Range(_, _) | ExprKind::RangeInclusive(_, _) => "i64".to_string(),
            ExprKind::Try(inner) => {
                // The result type of ? is T from Option<T> or T from Result<T, E>
                let inner_ty = self.expr_llvm_type(inner);
                if let Some(enum_name) = inner_ty.strip_prefix('%') {
                    if let Some(layout) = self.enum_layouts.get(enum_name) {
                        // First variant is Some/Ok — its first field is T
                        if let Some((_, fields)) = layout.variants.first() {
                            if !fields.is_empty() {
                                return self.llvm_type(&fields[0]);
                            }
                        }
                    }
                }
                "i64".to_string()
            }
        }
    }

    pub(crate) fn expr_struct_name(&self, expr: &Expr) -> Result<String, CodegenError> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    let ty = &slot.llvm_ty;
                    if let Some(stripped) = ty.strip_prefix('%') {
                        Ok(stripped.to_string())
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
            ExprKind::Index(arr_expr, _) => {
                // Array indexing: element type is stored in array_elem_types
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    if let Some(elem_ty) = self.array_elem_types.get(name) {
                        if let Some(stripped) = elem_ty.strip_prefix('%') {
                            return Ok(stripped.to_string());
                        }
                    }
                }
                Err(CodegenError {
                    message: "cannot determine struct name from index expression".to_string(),
                })
            }
            ExprKind::StructInit(name, _) => Ok(name.clone()),
            ExprKind::Call(callee, _) => {
                // Function call: look up return type from function signatures
                if let ExprKind::Ident(fn_name) = &callee.kind {
                    if let Some(Type::Named(name)) = self.fn_ret_types.get(fn_name) {
                        return Ok(name.clone());
                    }
                }
                Err(CodegenError {
                    message: "cannot determine struct name from call expression".to_string(),
                })
            }
            ExprKind::FieldAccess(recv, field) => {
                // Resolve the parent struct, then look up the field's type
                let parent_struct = self.expr_struct_name(recv)?;
                let (_, field_ty) = self.struct_field_index(&parent_struct, field)?;
                if let Type::Named(name) = field_ty {
                    if self.struct_layouts.contains_key(&name)
                        || self.enum_layouts.contains_key(&name)
                    {
                        return Ok(name);
                    }
                }
                Err(CodegenError {
                    message: format!("field '{}' is not a struct type", field),
                })
            }
            _ => Err(CodegenError {
                message: "cannot determine struct name from expression".to_string(),
            }),
        }
    }

    /// Infer the LLVM element type for an array expression (used by for-loop codegen).
    /// Handles identifiers, struct field accesses, and function call return types.
    pub(crate) fn infer_array_elem_type(&self, expr: &Expr) -> String {
        match &expr.kind {
            ExprKind::Ident(name) => self
                .array_elem_types
                .get(name)
                .cloned()
                .unwrap_or_else(|| "i64".to_string()),
            ExprKind::FieldAccess(recv, field) => {
                if let Ok(struct_name) = self.expr_struct_name(recv) {
                    if let Ok((_, Type::Array(inner))) =
                        self.struct_field_index(&struct_name, field)
                    {
                        return self.llvm_type(&inner);
                    }
                }
                "i64".to_string()
            }
            ExprKind::Call(callee, _) => {
                if let ExprKind::Ident(fn_name) = &callee.kind {
                    if let Some(Type::Array(inner)) = self.fn_ret_types.get(fn_name) {
                        return self.llvm_type(inner);
                    }
                }
                "i64".to_string()
            }
            ExprKind::MethodCall(receiver, method, _) => {
                // Look up the mangled method name (StructName_method) in fn_ret_types
                if let Ok(struct_name) = self.expr_struct_name(receiver) {
                    let mangled = format!("{}_{}", struct_name, method);
                    if let Some(Type::Array(inner)) = self.fn_ret_types.get(&mangled) {
                        return self.llvm_type(inner);
                    }
                }
                "i64".to_string()
            }
            _ => "i64".to_string(),
        }
    }

    pub(crate) fn struct_field_index(
        &self,
        struct_name: &str,
        field_name: &str,
    ) -> Result<(usize, Type), CodegenError> {
        let layout = self
            .struct_layouts
            .get(struct_name)
            .ok_or_else(|| CodegenError {
                message: format!("undefined struct '{}'", struct_name),
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

    pub(crate) fn find_enum_for_variant(&self, variant_name: &str) -> Option<String> {
        // Use the expected enum hint first for disambiguation
        if let Some(ref expected) = self.current_expected_enum {
            if let Some(layout) = self.enum_layouts.get(expected) {
                if layout.variants.iter().any(|(vn, _)| vn == variant_name) {
                    return Some(expected.clone());
                }
            }
        }
        // Fallback: search all enums
        for (ename, layout) in &self.enum_layouts {
            for (vname, _) in &layout.variants {
                if vname == variant_name {
                    return Some(ename.clone());
                }
            }
        }
        None
    }

    pub(crate) fn expr_enum_name(&self, expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    if let Some(stripped) = slot.llvm_ty.strip_prefix('%') {
                        if self.enum_layouts.contains_key(stripped) {
                            return Some(stripped.to_string());
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Check if emit_expr for this expression returns a pointer to an aggregate
    /// (enum variant constructor, struct init, tuple lit) rather than a loaded value.
    pub(crate) fn expr_returns_ptr(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::StructInit(_, _) | ExprKind::TupleLit(_) => true,
            ExprKind::Call(callee, _) => {
                // Enum variant constructors (e.g., Some(42)) return alloca pointers
                if let ExprKind::Ident(name) = &callee.kind {
                    self.find_enum_for_variant(name).is_some()
                } else {
                    false
                }
            }
            ExprKind::Ident(name) => {
                // Data-less enum variant (e.g., None) returns alloca pointer
                if self.lookup_var(name).is_some() {
                    false // regular variable, emit_expr loads it
                } else {
                    self.find_enum_for_variant(name).is_some()
                }
            }
            ExprKind::Index(arr_expr, _) => {
                // Array index of aggregate types returns alloca pointer (via memcpy)
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    self.array_elem_types
                        .get(name)
                        .map(|ty| Self::is_aggregate_type(ty))
                        .unwrap_or(false)
                } else {
                    false
                }
            }
            ExprKind::MethodCall(receiver, method, _) => {
                // Pipeline terminators that return aggregate pointers
                if Self::has_iter_base(receiver) {
                    matches!(method.as_str(), "find" | "reduce")
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub(crate) fn variant_tag(&self, variant_name: &str) -> Result<i32, CodegenError> {
        for layout in self.enum_layouts.values() {
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
}
