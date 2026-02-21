use super::*;

impl Codegen {
    pub(crate) fn emit_let(&mut self, s: &LetStmt) -> Result<(), CodegenError> {
        let ty = self.llvm_type(&s.ty);

        // Unit-typed let bindings: evaluate the RHS for side effects only.
        // `alloca void` and `store void` are invalid LLVM IR.
        if s.ty == Type::Unit {
            self.emit_expr(&s.value)?;
            return Ok(());
        }

        // For struct-typed variables, StructInit already creates an alloca and
        // fills in the fields.  Use that alloca directly as the variable slot
        // instead of trying to store a whole struct as a value.
        if let Type::Named(ref name) = s.ty {
            if self.struct_layouts.contains_key(name) {
                let returns_ptr = self.expr_returns_ptr(&s.value);
                let val = self.emit_expr(&s.value)?;
                if returns_ptr {
                    // StructInit: reuse alloca directly
                    self.define_var(&s.name, &val, &ty);
                } else {
                    // Function call or variable: value returned, need alloca + store
                    let ptr = self.emit_alloca_store(&ty, &val);
                    self.define_var(&s.name, &ptr, &ty);
                }
                return Ok(());
            }
            // Enum-typed let
            if self.enum_layouts.contains_key(name) {
                self.current_expected_enum = Some(name.clone());
                let returns_ptr = self.expr_returns_ptr(&s.value);
                let val = self.emit_expr(&s.value)?;
                self.current_expected_enum = None;
                if returns_ptr {
                    // Variant constructor: reuse alloca directly
                    self.define_var(&s.name, &val, &ty);
                } else {
                    // Function call or variable: value returned, need alloca + store
                    let ptr = self.emit_alloca_store(&ty, &val);
                    self.define_var(&s.name, &ptr, &ty);
                }
                return Ok(());
            }
        }

        // For tuple-typed variables, TupleLit already creates an alloca.
        // Reuse that alloca directly as the variable slot.
        if let Type::Tuple(ref elem_types) = s.ty {
            let returns_ptr = self.expr_returns_ptr(&s.value);
            let val = self.emit_expr(&s.value)?;
            let val_ptr = if returns_ptr {
                val
            } else {
                // Use fresh_temp() to avoid duplicate names when multiple
                // tuple let bindings or destructures exist in one function.
                self.emit_alloca_store(&ty, &val)
            };

            // Handle destructuring: let (a, b): (int, string) = expr;
            if let Some(ref names) = s.destructure {
                let tuple_name = self.tuple_type_name(elem_types);
                // Ensure the tuple type is defined
                let llvm_elem_types: Vec<String> =
                    elem_types.iter().map(|t| self.llvm_type(t)).collect();
                let llvm_fields = llvm_elem_types.join(", ");
                let type_def = format!("%{} = type {{ {} }}\n", tuple_name, llvm_fields);
                if !self.type_defs.contains(&format!("%{} = type", tuple_name)) {
                    self.type_defs.push_str(&type_def);
                }
                self.tuple_elem_types
                    .insert(tuple_name.clone(), llvm_elem_types.clone());

                for (i, name) in names.iter().enumerate() {
                    let elem_ty = &llvm_elem_types[i];
                    let loaded = self.emit_struct_field_load(&tuple_name, &val_ptr, i, elem_ty);
                    let ptr = self.emit_alloca_store(elem_ty, &loaded);
                    self.define_var(name, &ptr, elem_ty);
                    self.var_ast_types
                        .insert(name.clone(), elem_types[i].clone());
                }
                return Ok(());
            }

            self.define_var(&s.name, &val_ptr, &ty);
            self.var_ast_types.insert(s.name.clone(), s.ty.clone());
            return Ok(());
        }

        // For array-typed variables, ArrayLit creates a { ptr, i64, i64 } alloca.
        // Reuse that alloca directly.
        if let Type::Array(ref inner) = s.ty {
            let elem_llvm_ty = self.llvm_type(inner);
            let val_ptr = self.emit_expr(&s.value)?;
            self.define_var(&s.name, &val_ptr, "{ ptr, i64, i64 }");
            self.array_elem_types.insert(s.name.clone(), elem_llvm_ty);
            return Ok(());
        }

        // Track fn-typed variables for indirect calls
        if let Type::Fn(_, _) = &s.ty {
            self.closure_var_types.insert(s.name.clone(), s.ty.clone());
        }

        let ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", ptr, ty));

        let val = self.emit_expr(&s.value)?;
        self.emit_line(&format!("store {} {}, ptr {}", ty, val, ptr));
        self.define_var(&s.name, &ptr, &ty);

        // String buffer tracking: create len/cap allocas for string variables
        if s.ty == Type::Str {
            let len_ptr = self.fresh_temp();
            self.emit_line(&format!("{} = alloca i64", len_ptr));
            let cap_ptr = self.fresh_temp();
            self.emit_line(&format!("{} = alloca i64", cap_ptr));

            // Determine initial length from string literal, else 0
            let init_len = if let ExprKind::Literal(Literal::String(ref lit)) = s.value.kind {
                lit.len() as i64
            } else {
                0
            };
            self.emit_line(&format!("store i64 {}, ptr {}", init_len, len_ptr));
            // cap = 0 means "not an owned heap buffer yet"
            self.emit_line(&format!("store i64 0, ptr {}", cap_ptr));
            self.string_buf_vars
                .insert(ptr.clone(), StringBufInfo { len_ptr, cap_ptr });
        }

        // If the RHS was a spawn, record the env info for .join() to use
        if let Some(env_info) = self.last_spawn_env_info.take() {
            self.task_env_info.insert(s.name.clone(), env_info);
        }

        Ok(())
    }

    pub(crate) fn emit_assign(&mut self, s: &AssignStmt) -> Result<(), CodegenError> {
        match &s.target.kind {
            ExprKind::Ident(name) => {
                let slot = self
                    .lookup_var(name)
                    .ok_or_else(|| CodegenError {
                        message: format!("undefined variable '{}'", name),
                    })?
                    .clone();

                // Detect s = str_concat(s, literal) for capacity-aware inline concat.
                // Only applied when the suffix is a string literal (provably non-aliasing).
                if let ExprKind::Call(callee, args) = &s.value.kind {
                    if let ExprKind::Ident(fn_name) = &callee.kind {
                        if fn_name == "str_concat"
                            && args.len() == 2
                            && matches!(&args[0].kind, ExprKind::Ident(a) if a == name)
                            && matches!(&args[1].kind, ExprKind::Literal(Literal::String(_)))
                            && self.string_buf_vars.contains_key(&slot.ptr)
                        {
                            return self.emit_str_concat_inplace(name, &args[1]);
                        }
                    }
                }

                // If assigning a new (non-concat) value to a tracked string var,
                // reset len/cap to 0 so the next inline concat re-initializes
                if slot.llvm_ty == "ptr" {
                    if let Some(buf_info) = self.string_buf_vars.get(&slot.ptr) {
                        let len_ptr = buf_info.len_ptr.clone();
                        let cap_ptr = buf_info.cap_ptr.clone();
                        let val = self.emit_expr(&s.value)?;
                        self.emit_line(&format!("store ptr {}, ptr {}", val, slot.ptr));
                        self.emit_line(&format!("store i64 0, ptr {}", len_ptr));
                        self.emit_line(&format!("store i64 0, ptr {}", cap_ptr));
                        return Ok(());
                    }
                }
                let val = self.emit_expr(&s.value)?;
                // Unit-typed assignment: evaluate RHS for side effects only.
                // `store void` is invalid LLVM IR.
                if slot.llvm_ty == "void" {
                    return Ok(());
                }
                self.emit_line(&format!("store {} {}, ptr {}", slot.llvm_ty, val, slot.ptr));
                Ok(())
            }
            ExprKind::FieldAccess(obj, field) => {
                let obj_ptr = self.emit_expr_ptr(obj)?;
                let struct_name = self.expr_struct_name(obj)?;
                let (idx, field_ty) = self.struct_field_index(&struct_name, field)?;
                let fty = self.llvm_type(&field_ty);
                let val = self.emit_expr(&s.value)?;
                self.emit_struct_field_store(&struct_name, &obj_ptr, idx, &fty, &val);
                Ok(())
            }
            ExprKind::Index(arr_expr, idx_expr) => {
                // arr[idx] = value
                let arr_name = if let ExprKind::Ident(name) = &arr_expr.kind {
                    name.clone()
                } else {
                    return Err(CodegenError {
                        message: "index assignment target must be a variable".to_string(),
                    });
                };
                let elem_ty = self
                    .array_elem_types
                    .get(&arr_name)
                    .cloned()
                    .unwrap_or_else(|| "i64".to_string());
                let arr_ptr = self.emit_expr_ptr(arr_expr)?;

                // Load data pointer and length from fat pointer
                let (data_ptr, len_val) = self.emit_fat_ptr_load(&arr_ptr);

                let idx_val = self.emit_expr(idx_expr)?;

                // Bounds check (elide if index is a provably bounded loop var)
                let elide = if let ExprKind::Ident(idx_name) = &idx_expr.kind {
                    if let Some(bounded_arr) = self.bounded_loop_vars.get(idx_name) {
                        bounded_arr == &arr_name
                    } else {
                        false
                    }
                } else {
                    false
                };
                if !elide {
                    self.emit_line(&format!(
                        "call void @__yorum_bounds_check(i64 {}, i64 {})",
                        idx_val, len_val
                    ));
                }

                let elem_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {}, ptr {}, i64 {}",
                    elem_gep, elem_ty, data_ptr, idx_val
                ));
                let val = self.emit_expr(&s.value)?;
                self.emit_line(&format!("store {} {}, ptr {}", elem_ty, val, elem_gep));
                Ok(())
            }
            _ => Err(CodegenError {
                message: "invalid assignment target".to_string(),
            }),
        }
    }

    /// Emit inline capacity-aware string concatenation for `name = str_concat(name, suffix)`.
    ///
    /// Instead of calling `@str_concat` (which mallocs a new buffer every time),
    /// this tracks {len, cap} per variable and uses realloc for amortized O(1) appends.
    pub(crate) fn emit_str_concat_inplace(
        &mut self,
        name: &str,
        suffix_expr: &Expr,
    ) -> Result<(), CodegenError> {
        let slot = self
            .lookup_var(name)
            .ok_or_else(|| CodegenError {
                message: format!("undefined variable '{}'", name),
            })?
            .clone();
        let buf_info = self.string_buf_vars.get(&slot.ptr).unwrap().clone();

        // Load current data, len, cap
        let data = self.emit_load("ptr", &slot.ptr);
        let len = self.emit_load("i64", &buf_info.len_ptr);
        let cap = self.emit_load("i64", &buf_info.cap_ptr);

        // Evaluate suffix and get its length
        let suffix_val = self.emit_expr(suffix_expr)?;
        let suffix_len = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call i64 @strlen(ptr {})",
            suffix_len, suffix_val
        ));

        // Compute new_len and need (new_len + 1 for null terminator)
        let new_len = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, {}", new_len, len, suffix_len));
        let need = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", need, new_len));

        // Branch on cap == 0 (uninitialized: literal or function result)
        let cap_zero = self.fresh_temp();
        self.emit_line(&format!("{} = icmp eq i64 {}, 0", cap_zero, cap));
        let lbl_init = self.fresh_label("sbuf_init");
        let lbl_check_grow = self.fresh_label("sbuf_check_grow");
        let lbl_grow = self.fresh_label("sbuf_grow");
        let lbl_append = self.fresh_label("sbuf_append");
        self.emit_cond_branch(&cap_zero, &lbl_init, &lbl_check_grow);
        self.block_terminated = true;

        // --- init_buf: first time, malloc a new buffer and copy old data ---
        self.emit_label(&lbl_init);
        // Compute actual data length first (tracked len may be 0 for non-literal init)
        let old_len = self.fresh_temp();
        self.emit_line(&format!("{} = call i64 @strlen(ptr {})", old_len, data));
        let real_new_len = self.fresh_temp();
        self.emit_line(&format!(
            "{} = add i64 {}, {}",
            real_new_len, old_len, suffix_len
        ));
        let real_need = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", real_need, real_new_len));
        // init_cap = max(real_need * 2, 64)
        let need_x2 = self.fresh_temp();
        self.emit_line(&format!("{} = mul i64 {}, 2", need_x2, real_need));
        let cmp_64 = self.fresh_temp();
        self.emit_line(&format!("{} = icmp ugt i64 {}, 64", cmp_64, need_x2));
        let init_cap = self.fresh_temp();
        self.emit_line(&format!(
            "{} = select i1 {}, i64 {}, i64 64",
            init_cap, cmp_64, need_x2
        ));
        let init_buf = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call ptr @malloc(i64 {})",
            init_buf, init_cap
        ));
        // Copy old data into new buffer
        self.emit_memcpy(&init_buf, &data, &old_len);
        // Store updated data, len, cap
        self.emit_line(&format!("store ptr {}, ptr {}", init_buf, slot.ptr));
        self.emit_line(&format!("store i64 {}, ptr {}", init_cap, buf_info.cap_ptr));
        // Append suffix at old_len offset
        let init_dest = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            init_dest, init_buf, old_len
        ));
        self.emit_memcpy(&init_dest, &suffix_val, &suffix_len);
        let init_end = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            init_end, init_buf, real_new_len
        ));
        self.emit_line(&format!("store i8 0, ptr {}", init_end));
        self.emit_line(&format!(
            "store i64 {}, ptr {}",
            real_new_len, buf_info.len_ptr
        ));
        self.emit_line(&format!("br label %{}", lbl_append));
        self.block_terminated = true;

        // --- check_grow: cap > 0, check if need > cap ---
        self.emit_label(&lbl_check_grow);
        let need_grow = self.fresh_temp();
        self.emit_line(&format!("{} = icmp ugt i64 {}, {}", need_grow, need, cap));
        self.emit_cond_branch(&need_grow, &lbl_grow, &lbl_append);
        self.block_terminated = true;

        // --- grow: realloc to max(need, cap * 2) ---
        self.emit_label(&lbl_grow);
        let cap_x2 = self.fresh_temp();
        self.emit_line(&format!("{} = mul i64 {}, 2", cap_x2, cap));
        let cmp_grow = self.fresh_temp();
        self.emit_line(&format!("{} = icmp ugt i64 {}, {}", cmp_grow, need, cap_x2));
        let grow_cap = self.fresh_temp();
        self.emit_line(&format!(
            "{} = select i1 {}, i64 {}, i64 {}",
            grow_cap, cmp_grow, need, cap_x2
        ));
        let grow_buf = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call ptr @realloc(ptr {}, i64 {})",
            grow_buf, data, grow_cap
        ));
        self.emit_line(&format!("store ptr {}, ptr {}", grow_buf, slot.ptr));
        self.emit_line(&format!("store i64 {}, ptr {}", grow_cap, buf_info.cap_ptr));
        // Append suffix at len offset
        let grow_dest = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            grow_dest, grow_buf, len
        ));
        self.emit_memcpy(&grow_dest, &suffix_val, &suffix_len);
        let grow_end = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            grow_end, grow_buf, new_len
        ));
        self.emit_line(&format!("store i8 0, ptr {}", grow_end));
        self.emit_line(&format!("store i64 {}, ptr {}", new_len, buf_info.len_ptr));
        self.emit_line(&format!("br label %{}", lbl_append));
        self.block_terminated = true;

        // --- append: no-grow path, cap > 0 and need <= cap ---
        self.emit_label(&lbl_append);
        // Reload data from alloca (may have changed in init or grow)
        let final_data = self.emit_load("ptr", &slot.ptr);
        let final_len = self.emit_load("i64", &buf_info.len_ptr);
        // Check if we already handled this in init/grow (len was updated there)
        let already_done = self.fresh_temp();
        self.emit_line(&format!(
            "{} = icmp ne i64 {}, {}",
            already_done, final_len, len
        ));
        let lbl_do_append = self.fresh_label("sbuf_do_append");
        let lbl_done = self.fresh_label("sbuf_done");
        self.emit_cond_branch(&already_done, &lbl_done, &lbl_do_append);
        self.block_terminated = true;

        // --- do_append: the no-realloc/no-init path ---
        self.emit_label(&lbl_do_append);
        let app_dest = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            app_dest, final_data, len
        ));
        self.emit_memcpy(&app_dest, &suffix_val, &suffix_len);
        let app_end = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            app_end, final_data, new_len
        ));
        self.emit_line(&format!("store i8 0, ptr {}", app_end));
        self.emit_line(&format!("store i64 {}, ptr {}", new_len, buf_info.len_ptr));
        self.emit_line(&format!("br label %{}", lbl_done));
        self.block_terminated = true;

        // --- done ---
        self.emit_label(&lbl_done);

        Ok(())
    }

    pub(crate) fn emit_return(&mut self, s: &ReturnStmt) -> Result<(), CodegenError> {
        // In spawn context, the user's `return X` stores the result into the
        // env struct's result slot and emits `ret ptr null` for the pthread
        // wrapper ABI. The actual result is read by .join() from the env.
        if let Some((ref env_type, result_idx)) = self.spawn_return_ctx.clone() {
            let val = self.emit_expr(&s.value)?;
            self.emit_struct_field_store(env_type, "%env", result_idx, "i64", &val);
            self.emit_line("ret ptr null");
            self.block_terminated = true;
            return Ok(());
        }

        let ret_ty = self.current_fn_ret_ty.clone().unwrap_or("void".to_string());
        if ret_ty == "void" {
            self.emit_line("ret void");
        } else {
            // Tail call optimization: detect `return f(args)` pattern
            let is_tail_call = {
                let contracts = &self.current_fn_contracts;
                let no_ensures = !Self::has_ensures(contracts);
                let is_simple_call = matches!(
                    &s.value.kind,
                    ExprKind::Call(callee, _) if matches!(&callee.kind, ExprKind::Ident(_))
                );
                let no_ptr_load = !(ret_ty.starts_with('%') && self.expr_returns_ptr(&s.value));
                no_ensures && is_simple_call && no_ptr_load
            };
            if is_tail_call {
                self.tail_call_hint = true;
            }

            // Set expected enum hint for return value (e.g., return Some(42);)
            if let Some(stripped) = ret_ty.strip_prefix('%') {
                if self.enum_layouts.contains_key(stripped) {
                    self.current_expected_enum = Some(stripped.to_string());
                }
            }
            let val = self.emit_expr(&s.value)?;
            self.current_expected_enum = None;
            self.tail_call_hint = false;

            // Enum/struct variant constructors return alloca pointers (ptr type),
            // but `ret %EnumType %ptr` is invalid — need to load the value first
            let val = if ret_ty.starts_with('%') && self.expr_returns_ptr(&s.value) {
                self.emit_load(&ret_ty, &val)
            } else {
                val
            };

            // Emit ensures checks before the actual return
            let contracts = self.current_fn_contracts.clone();
            let fn_name = self.current_fn_name.clone();
            if Self::has_ensures(&contracts) {
                // Store the return value into the result slot
                self.emit_line(&format!("store {} {}, ptr %result.addr", ret_ty, val));
                self.emit_ensures_checks(&contracts, &fn_name)?;
                // Reload the value (it hasn't changed, but keeps IR correct)
                let reloaded = self.emit_load(&ret_ty, "%result.addr");
                self.emit_line(&format!("ret {} {}", ret_ty, reloaded));
            } else {
                self.emit_line(&format!("ret {} {}", ret_ty, val));
            }
        }
        self.block_terminated = true;
        Ok(())
    }

    pub(crate) fn emit_if(&mut self, s: &IfStmt) -> Result<(), CodegenError> {
        let cond = self.emit_expr(&s.condition)?;
        let then_label = self.fresh_label("then");
        let else_label = self.fresh_label("else");
        let merge_label = self.fresh_label("ifcont");

        let has_else = s.else_branch.is_some();
        let target_else = if has_else { &else_label } else { &merge_label };

        self.emit_cond_branch(&cond, &then_label, target_else);

        // Then block
        self.emit_label(&then_label);
        self.block_terminated = false;
        self.push_scope();
        self.emit_block(&s.then_block)?;
        self.pop_scope();
        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", merge_label));
        }
        let then_terminated = self.block_terminated;

        // Else block
        if let Some(else_branch) = &s.else_branch {
            self.emit_label(&else_label);
            self.block_terminated = false;
            self.push_scope();
            match else_branch.as_ref() {
                ElseBranch::Else(block) => {
                    self.emit_block(block)?;
                }
                ElseBranch::ElseIf(elif) => {
                    self.emit_if(elif)?;
                }
            }
            self.pop_scope();
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

    pub(crate) fn emit_while(&mut self, s: &WhileStmt) -> Result<(), CodegenError> {
        let cond_label = self.fresh_label("while.cond");
        let body_label = self.fresh_label("while.body");
        let end_label = self.fresh_label("while.end");

        self.emit_line(&format!("br label %{}", cond_label));

        // Condition check
        self.emit_label(&cond_label);
        self.block_terminated = false;
        let cond = self.emit_expr(&s.condition)?;
        self.emit_cond_branch(&cond, &body_label, &end_label);

        // Loop body
        self.emit_label(&body_label);
        self.block_terminated = false;
        self.loop_labels
            .push((cond_label.clone(), end_label.clone()));
        self.push_scope();
        self.emit_block(&s.body)?;
        self.pop_scope();
        self.loop_labels.pop();
        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", cond_label));
        }

        // End
        self.emit_label(&end_label);
        self.block_terminated = false;
        Ok(())
    }

    pub(crate) fn emit_for(&mut self, s: &ForStmt) -> Result<(), CodegenError> {
        // Range-based for loop: for i in start..end
        if let ExprKind::Range(ref start, ref end) = s.iterable.kind {
            return self.emit_for_range(&s.var_name, start, end, false, &s.body);
        }
        // Inclusive range: for i in start..=end
        if let ExprKind::RangeInclusive(ref start, ref end) = s.iterable.kind {
            return self.emit_for_range(&s.var_name, start, end, true, &s.body);
        }

        // Bare (range).iter() → same as range for-loop
        if let ExprKind::MethodCall(ref receiver, ref method, ref args) = s.iterable.kind {
            if method == "iter" && args.is_empty() {
                if let ExprKind::Range(ref start, ref end) = receiver.kind {
                    return self.emit_for_range(&s.var_name, start, end, false, &s.body);
                }
                if let ExprKind::RangeInclusive(ref start, ref end) = receiver.kind {
                    return self.emit_for_range(&s.var_name, start, end, true, &s.body);
                }
            }
        }

        // Fused iterator pipeline (.iter().map().filter() chains)
        if let Some(pipeline) = self.try_extract_pipeline(&s.iterable) {
            return self.emit_for_pipeline(&s.var_name, &pipeline, &s.body);
        }

        // Guard: reject pipeline-shaped iterables that couldn't be extracted
        if Self::is_iter_pipeline(&s.iterable) {
            return Err(CodegenError {
                message: "iterator pipeline requires inline closures".to_string(),
            });
        }

        // Evaluate the iterable (array fat pointer)
        let arr_val = self.emit_expr(&s.iterable)?;

        // Determine the array element type. For array .iter() (the no-op case),
        // look through to the receiver. For struct .iter() methods, keep the
        // full MethodCall so infer_array_elem_type can resolve the return type.
        let arr_expr = match &s.iterable.kind {
            ExprKind::MethodCall(receiver, method, args)
                if method == "iter"
                    && args.is_empty()
                    && self.expr_struct_name(receiver).is_err() =>
            {
                receiver.as_ref()
            }
            _ => &s.iterable,
        };
        let elem_ty = self.infer_array_elem_type(arr_expr);

        // Load data ptr and length from fat pointer
        let (data_ptr, len_val) = self.emit_fat_ptr_load(&arr_val);

        // Alloca for index counter
        let idx_ptr = self.emit_alloca_store("i64", "0");

        // Alloca for loop variable — use fresh_temp() to avoid duplicate
        // names when the same variable name is used in multiple for loops.
        let var_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", var_ptr, elem_ty));

        let cond_label = self.fresh_label("for.cond");
        let body_label = self.fresh_label("for.body");
        let inc_label = self.fresh_label("for.inc");
        let end_label = self.fresh_label("for.end");

        self.emit_line(&format!("br label %{}", cond_label));

        // Condition: idx < len
        self.emit_label(&cond_label);
        self.block_terminated = false;
        let cur_idx = self.emit_load("i64", &idx_ptr);
        let cmp = self.fresh_temp();
        self.emit_line(&format!("{} = icmp slt i64 {}, {}", cmp, cur_idx, len_val));
        self.emit_cond_branch(&cmp, &body_label, &end_label);

        // Body
        self.emit_label(&body_label);
        self.block_terminated = false;

        self.push_scope();

        // Load element at current index into loop variable
        let elem_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {}, ptr {}, i64 {}",
            elem_gep, elem_ty, data_ptr, cur_idx
        ));
        let elem_val = self.emit_load(&elem_ty, &elem_gep);
        self.emit_line(&format!("store {} {}, ptr {}", elem_ty, elem_val, var_ptr));

        self.define_var(&s.var_name, &var_ptr, &elem_ty);

        // Push loop labels: continue → for.inc, break → for.end
        self.loop_labels
            .push((inc_label.clone(), end_label.clone()));
        self.emit_block(&s.body)?;
        self.loop_labels.pop();

        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", inc_label));
        }

        self.pop_scope();

        // Increment block
        self.emit_label(&inc_label);
        self.block_terminated = false;
        let next_idx_tmp = self.fresh_temp();
        let cur_idx2 = self.emit_load("i64", &idx_ptr);
        self.emit_line(&format!("{} = add i64 {}, 1", next_idx_tmp, cur_idx2));
        self.emit_line(&format!("store i64 {}, ptr {}", next_idx_tmp, idx_ptr));
        self.emit_line(&format!("br label %{}", cond_label));

        self.emit_label(&end_label);
        self.block_terminated = false;
        Ok(())
    }

    pub(crate) fn emit_match(&mut self, s: &MatchStmt) -> Result<(), CodegenError> {
        // Determine if the subject is an enum type
        let subject_enum_name = self.expr_enum_name(&s.subject);
        let is_enum = subject_enum_name.is_some();

        // For enums, we need the alloca pointer (not the loaded value) for GEP
        let subject_val = if is_enum {
            self.emit_expr_ptr(&s.subject)?
        } else {
            self.emit_expr(&s.subject)?
        };
        let merge_label = self.fresh_label("match.end");

        // For enums: extract tag from alloca via GEP
        let tag_val = if is_enum {
            let enum_name = subject_enum_name.as_ref().unwrap();
            let tag = self.emit_struct_field_load(enum_name, &subject_val, 0, "i32");
            Some(tag)
        } else {
            None
        };

        let compare_val = if let Some(ref tv) = tag_val {
            tv.clone()
        } else {
            subject_val.clone()
        };

        let mut arm_labels: Vec<String> = Vec::new();
        let mut check_labels: Vec<String> = Vec::new();
        for _ in &s.arms {
            arm_labels.push(self.fresh_label("match.arm"));
            check_labels.push(self.fresh_label("match.check"));
        }
        let default_label = self.fresh_label("match.default");

        // Emit cascading comparisons
        for (i, arm) in s.arms.iter().enumerate() {
            let next = if i + 1 < s.arms.len() {
                check_labels[i + 1].clone()
            } else {
                default_label.clone()
            };
            match &arm.pattern {
                Pattern::Literal(Literal::Int(n), _) => {
                    let cmp = self.fresh_temp();
                    self.emit_line(&format!("{} = icmp eq i64 {}, {}", cmp, compare_val, n));
                    self.emit_cond_branch(&cmp, &arm_labels[i], &next);
                    if i + 1 < s.arms.len() {
                        self.emit_label(&next);
                        self.block_terminated = false;
                    }
                }
                Pattern::Literal(Literal::Char(c), _) => {
                    let cmp = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = icmp eq i8 {}, {}",
                        cmp, compare_val, *c as u8
                    ));
                    self.emit_cond_branch(&cmp, &arm_labels[i], &next);
                    if i + 1 < s.arms.len() {
                        self.emit_label(&next);
                        self.block_terminated = false;
                    }
                }
                Pattern::Wildcard(_) => {
                    self.emit_line(&format!("br label %{}", arm_labels[i]));
                }
                Pattern::Binding(bname, _) => {
                    // Check if this binding name is actually a no-data enum variant
                    if is_enum {
                        if let Ok(tag) = self.variant_tag(bname) {
                            let cmp = self.fresh_temp();
                            self.emit_line(&format!(
                                "{} = icmp eq i32 {}, {}",
                                cmp, compare_val, tag
                            ));
                            self.emit_cond_branch(&cmp, &arm_labels[i], &next);
                            if i + 1 < s.arms.len() {
                                self.emit_label(&next);
                                self.block_terminated = false;
                            }
                        } else {
                            self.emit_line(&format!("br label %{}", arm_labels[i]));
                        }
                    } else {
                        self.emit_line(&format!("br label %{}", arm_labels[i]));
                    }
                }
                Pattern::Variant(vname, _, _) => {
                    let tag = self.variant_tag(vname)?;
                    let cmp = self.fresh_temp();
                    self.emit_line(&format!("{} = icmp eq i32 {}, {}", cmp, compare_val, tag));
                    self.emit_cond_branch(&cmp, &arm_labels[i], &next);
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
        let enum_name_clone = subject_enum_name.clone();
        for (i, arm) in s.arms.iter().enumerate() {
            self.emit_label(&arm_labels[i]);
            self.block_terminated = false;

            self.push_scope();

            // Bind pattern variables
            match &arm.pattern {
                Pattern::Binding(name, _) => {
                    // Don't bind if the name matches a no-data enum variant
                    let is_variant = is_enum && self.variant_tag(name).is_ok();
                    if !is_variant {
                        let ty_str = if is_enum {
                            "i32".to_string()
                        } else {
                            "i64".to_string()
                        };
                        let val = &compare_val;
                        let ptr = self.fresh_temp();
                        self.emit_line(&format!("{} = alloca {}", ptr, ty_str));
                        self.emit_line(&format!("store {} {}, ptr {}", ty_str, val, ptr));
                        self.define_var(name, &ptr, &ty_str);
                    }
                }
                Pattern::Variant(vname, sub_patterns, _) => {
                    // Extract payload from enum and bind sub-patterns
                    if let Some(ref ename) = enum_name_clone {
                        let layout = self.enum_layouts.get(ename).cloned();
                        if let Some(layout) = layout {
                            let variant_fields: Vec<Type> = layout
                                .variants
                                .iter()
                                .find(|(n, _)| n == vname)
                                .map(|(_, f)| f.clone())
                                .unwrap_or_default();

                            if !variant_fields.is_empty() {
                                // GEP into payload bytes
                                let payload_gep = self.emit_struct_gep(ename, &subject_val, 1);

                                // Extract each field from payload and bind to sub-patterns
                                let mut byte_offset = 0usize;
                                for (fi, field_ty) in variant_fields.iter().enumerate() {
                                    let field_llvm_ty = self.llvm_type(field_ty);
                                    if fi < sub_patterns.len() {
                                        if let Pattern::Binding(bname, _) = &sub_patterns[fi] {
                                            let field_ptr = self.fresh_temp();
                                            self.emit_line(&format!(
                                                "{} = getelementptr [0 x i8], ptr {}, i64 0, i64 {}",
                                                field_ptr, payload_gep, byte_offset
                                            ));
                                            let field_val =
                                                self.emit_load(&field_llvm_ty, &field_ptr);
                                            let bind_ptr =
                                                self.emit_alloca_store(&field_llvm_ty, &field_val);
                                            self.define_var(bname, &bind_ptr, &field_llvm_ty);
                                        }
                                    }
                                    byte_offset += self.llvm_type_size(&field_llvm_ty);
                                }
                            }
                        }
                    }
                }
                _ => {}
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
}
