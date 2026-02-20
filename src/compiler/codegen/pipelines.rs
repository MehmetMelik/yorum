use super::*;

impl Codegen {
    /// Try to extract an iterator pipeline from an expression.
    /// Walks a MethodCall chain right-to-left, recognizing .map(closure) and
    /// .filter(closure) steps. Base case: .iter() on a non-struct receiver.
    /// Returns None for non-pipeline expressions or pipelines with no combinators
    /// (just .iter() with no map/filter — let existing codepath handle it).
    pub(crate) fn try_extract_pipeline<'a>(&self, expr: &'a Expr) -> Option<IterPipeline<'a>> {
        let mut steps = Vec::new();
        let mut current = expr;

        loop {
            if let ExprKind::MethodCall(ref receiver, ref method, ref args) = current.kind {
                match method.as_str() {
                    "map" => {
                        if args.len() == 1 {
                            if let ExprKind::Closure(ref c) = args[0].kind {
                                steps.push(IterStep::Map(c));
                                current = receiver;
                                continue;
                            }
                        }
                        return None; // Not an inline closure
                    }
                    "filter" => {
                        if args.len() == 1 {
                            if let ExprKind::Closure(ref c) = args[0].kind {
                                steps.push(IterStep::Filter(c));
                                current = receiver;
                                continue;
                            }
                        }
                        return None;
                    }
                    "enumerate" => {
                        if args.is_empty() {
                            steps.push(IterStep::Enumerate);
                            current = receiver;
                            continue;
                        }
                        return None;
                    }
                    "zip" => {
                        if args.len() == 1 {
                            steps.push(IterStep::Zip(&args[0]));
                            current = receiver;
                            continue;
                        }
                        return None;
                    }
                    "take" => {
                        if args.len() == 1 {
                            steps.push(IterStep::Take(&args[0]));
                            current = receiver;
                            continue;
                        }
                        return None;
                    }
                    "skip" => {
                        if args.len() == 1 {
                            steps.push(IterStep::Skip(&args[0]));
                            current = receiver;
                            continue;
                        }
                        return None;
                    }
                    "iter" => {
                        if args.is_empty() {
                            let is_range = matches!(
                                receiver.kind,
                                ExprKind::Range(_, _) | ExprKind::RangeInclusive(_, _)
                            );
                            if is_range || self.expr_struct_name(receiver).is_err() {
                                if steps.is_empty() {
                                    return None; // Just .iter() with no combinators
                                }
                                steps.reverse(); // We walked right-to-left
                                return Some(IterPipeline {
                                    source: receiver,
                                    steps,
                                    is_range_source: is_range,
                                });
                            }
                        }
                        return None; // Struct .iter() or .iter() with args
                    }
                    _ => return None,
                }
            } else {
                return None;
            }
        }
    }

    /// Check if the expression is a pipeline-shaped iterable: a `.map()` or `.filter()`
    /// chain with `.iter()` at the base. Used for error reporting when
    /// `try_extract_pipeline` returns `None` (e.g., named closure variables).
    /// Returns false for struct methods named map/filter without `.iter()`.
    pub(crate) fn is_iter_pipeline(expr: &Expr) -> bool {
        if let ExprKind::MethodCall(ref receiver, ref method, _) = expr.kind {
            match method.as_str() {
                "map" | "filter" | "enumerate" | "zip" | "take" | "skip" => {
                    Self::has_iter_base(receiver)
                }
                _ => false,
            }
        } else {
            false
        }
    }

    pub(crate) fn has_iter_base(expr: &Expr) -> bool {
        if let ExprKind::MethodCall(ref receiver, ref method, _) = expr.kind {
            match method.as_str() {
                "iter" => true,
                "map" | "filter" | "enumerate" | "zip" | "take" | "skip" | "reduce" | "fold"
                | "collect" | "find" | "any" | "all" => Self::has_iter_base(receiver),
                _ => false,
            }
        } else {
            false
        }
    }

    /// Try to extract a terminated pipeline from a MethodCall expression.
    /// Checks if the outermost method is a terminator, then extracts pipeline steps
    /// from the receiver. Handles bare `.iter()` receivers (empty steps).
    pub(crate) fn try_extract_terminated_pipeline<'a>(
        &self,
        expr: &'a Expr,
    ) -> Option<TerminatedPipeline<'a>> {
        if let ExprKind::MethodCall(ref receiver, ref method, ref args) = expr.kind {
            let terminator = match method.as_str() {
                "collect" if args.is_empty() => PipelineTerminator::Collect,
                "any" if args.len() == 1 => {
                    if let ExprKind::Closure(ref c) = args[0].kind {
                        PipelineTerminator::Any(c)
                    } else {
                        return None;
                    }
                }
                "all" if args.len() == 1 => {
                    if let ExprKind::Closure(ref c) = args[0].kind {
                        PipelineTerminator::All(c)
                    } else {
                        return None;
                    }
                }
                "find" if args.len() == 1 => {
                    if let ExprKind::Closure(ref c) = args[0].kind {
                        PipelineTerminator::Find(c)
                    } else {
                        return None;
                    }
                }
                "reduce" if args.len() == 1 => {
                    if let ExprKind::Closure(ref c) = args[0].kind {
                        PipelineTerminator::Reduce(c)
                    } else {
                        return None;
                    }
                }
                "fold" if args.len() == 2 => {
                    if let ExprKind::Closure(ref c) = args[1].kind {
                        PipelineTerminator::Fold(&args[0], c)
                    } else {
                        return None;
                    }
                }
                _ => return None,
            };

            // Now extract the pipeline from the receiver.
            // try_extract_pipeline returns None for bare .iter() (no combinators),
            // but we still need to handle that case for terminators.
            if let Some(pipeline) = self.try_extract_pipeline(receiver) {
                return Some(TerminatedPipeline {
                    source: pipeline.source,
                    steps: pipeline.steps,
                    terminator,
                    is_range_source: pipeline.is_range_source,
                });
            }

            // Handle bare .iter() with no combinators
            if let ExprKind::MethodCall(ref inner_recv, ref inner_method, ref inner_args) =
                receiver.kind
            {
                if inner_method == "iter" && inner_args.is_empty() {
                    let is_range = matches!(
                        inner_recv.kind,
                        ExprKind::Range(_, _) | ExprKind::RangeInclusive(_, _)
                    );
                    if is_range || self.expr_struct_name(inner_recv).is_err() {
                        return Some(TerminatedPipeline {
                            source: inner_recv,
                            steps: Vec::new(),
                            terminator,
                            is_range_source: is_range,
                        });
                    }
                }
            }
        }
        None
    }

    /// Emit closures for pipeline steps, returning (fn_ptr, env_ptr, ret_ty) per closure.
    pub(crate) fn emit_pipeline_closure(
        &mut self,
        closure: &ClosureExpr,
    ) -> Result<(String, String, String), CodegenError> {
        let pair_ptr = self.emit_closure(closure)?;
        let fn_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, ptr }}, ptr {}, i32 0, i32 0",
            fn_gep, pair_ptr
        ));
        let fn_ptr = self.emit_load("ptr", &fn_gep);
        let env_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, ptr }}, ptr {}, i32 0, i32 1",
            env_gep, pair_ptr
        ));
        let env_ptr = self.emit_load("ptr", &env_gep);
        let ret_ty = self.llvm_type(&closure.return_type);
        Ok((fn_ptr, env_ptr, ret_ty))
    }

    /// Determine the final LLVM element type after applying all pipeline steps.
    pub(crate) fn pipeline_final_elem_type(
        &self,
        src_elem_ty: &str,
        steps: &[IterStep<'_>],
    ) -> String {
        let mut ty = src_elem_ty.to_string();
        for step in steps {
            match step {
                IterStep::Map(c) => {
                    ty = self.llvm_type(&c.return_type);
                }
                IterStep::Enumerate => {
                    // Build tuple type name from inner type
                    let inner_semantic = llvm_to_semantic_name(&ty);
                    ty = format!("%tuple.int.{}", inner_semantic);
                }
                IterStep::Zip(zip_expr) => {
                    let left_semantic = llvm_to_semantic_name(&ty);
                    let right_llvm = self.infer_array_elem_type(zip_expr);
                    let right_semantic = llvm_to_semantic_name(&right_llvm);
                    ty = format!("%tuple.{}.{}", left_semantic, right_semantic);
                }
                IterStep::Filter(_) | IterStep::Take(_) | IterStep::Skip(_) => {}
            }
        }
        ty
    }

    /// Emit a fused for-loop over an iterator pipeline.
    /// Compiles `for x in arr.iter().map(f).filter(g) { body }` into a single
    /// loop with inline closure calls — no allocation, no iterator structs.
    /// Shared preamble for iterator pipelines: emit source array, load data_ptr + len,
    /// pre-emit step closures/setup. Used by both `emit_for_pipeline` and `emit_pipeline_terminator`.
    #[allow(clippy::type_complexity)]
    pub(crate) fn emit_pipeline_preamble(
        &mut self,
        source: &Expr,
        steps: &[IterStep<'_>],
        is_range_source: bool,
    ) -> Result<
        (
            String, // data_ptr (or range_start for range sources)
            String, // len_val (used for collect allocation; for ranges, a best-effort estimate)
            String, // src_elem_ty
            Vec<Option<ClosureInfo>>,
            Vec<Option<ZipInfo>>,
            Vec<Option<String>>,
            Vec<Option<String>>,
            Option<(String, bool)>, // range_end: Some((end_val, inclusive)) for ranges, None for arrays
        ),
        CodegenError,
    > {
        let (data_ptr, len_val, src_elem_ty, range_end) = if is_range_source {
            // Range source: emit start and end, compute length estimate for allocation.
            // The loop condition uses direct value comparison (start + idx vs end)
            // instead of this length, avoiding overflow for wide ranges.
            let (start_expr, end_expr, inclusive) = match &source.kind {
                ExprKind::Range(s, e) => (s, e, false),
                ExprKind::RangeInclusive(s, e) => (s, e, true),
                _ => unreachable!(),
            };
            let start_val = self.emit_expr(start_expr)?;
            let end_val = self.emit_expr(end_expr)?;

            // Compute a saturating non-negative allocation length:
            //   len = max(0, end-start[+1]) clamped to i64::MAX.
            // Use i128 arithmetic so wide valid ranges don't wrap negative.
            let start_i128 = self.fresh_temp();
            self.emit_line(&format!("{} = sext i64 {} to i128", start_i128, start_val));
            let end_i128 = self.fresh_temp();
            self.emit_line(&format!("{} = sext i64 {} to i128", end_i128, end_val));
            let span_i128 = self.fresh_temp();
            self.emit_line(&format!(
                "{} = sub i128 {}, {}",
                span_i128, end_i128, start_i128
            ));
            let len_i128 = if inclusive {
                let len_inc = self.fresh_temp();
                self.emit_line(&format!("{} = add i128 {}, 1", len_inc, span_i128));
                len_inc
            } else {
                span_i128
            };
            let len_non_pos = self.fresh_temp();
            self.emit_line(&format!("{} = icmp sle i128 {}, 0", len_non_pos, len_i128));
            let len_gt_i64_max = self.fresh_temp();
            self.emit_line(&format!(
                "{} = icmp sgt i128 {}, 9223372036854775807",
                len_gt_i64_max, len_i128
            ));
            let len_hi_clamped = self.fresh_temp();
            self.emit_line(&format!(
                "{} = select i1 {}, i128 9223372036854775807, i128 {}",
                len_hi_clamped, len_gt_i64_max, len_i128
            ));
            let len_sat_i128 = self.fresh_temp();
            self.emit_line(&format!(
                "{} = select i1 {}, i128 0, i128 {}",
                len_sat_i128, len_non_pos, len_hi_clamped
            ));
            let alloc_len = self.fresh_temp();
            self.emit_line(&format!(
                "{} = trunc i128 {} to i64",
                alloc_len, len_sat_i128
            ));

            (
                start_val,
                alloc_len,
                "i64".to_string(),
                Some((end_val, inclusive)),
            )
        } else {
            // Emit the source array expression → fat pointer
            let arr_val = self.emit_expr(source)?;
            let src_elem_ty = self.infer_array_elem_type(source);

            // Load data ptr and length from fat pointer
            let (data_ptr, len_val) = self.emit_fat_ptr_load(&arr_val);

            (data_ptr, len_val, src_elem_ty, None)
        };

        // Pre-emit closures and prepare step data.
        // Track current_elem_ty so Enumerate/Zip can register tuple types
        // before downstream closures that reference those types are emitted.
        let mut closure_infos: Vec<Option<ClosureInfo>> = Vec::new();
        let mut zip_infos: Vec<Option<ZipInfo>> = Vec::new();
        let mut take_skip_ptrs: Vec<Option<String>> = Vec::new();
        let mut enumerate_ptrs: Vec<Option<String>> = Vec::new();
        let mut current_elem_ty = src_elem_ty.clone();

        for step in steps {
            match step {
                IterStep::Map(c) | IterStep::Filter(c) => {
                    let (fn_ptr, env_ptr, ret_ty) = self.emit_pipeline_closure(c)?;
                    if matches!(step, IterStep::Map(_)) {
                        current_elem_ty = ret_ty.clone();
                    }
                    closure_infos.push(Some(ClosureInfo {
                        fn_ptr,
                        env_ptr,
                        ret_ty,
                    }));
                    zip_infos.push(None);
                    take_skip_ptrs.push(None);
                    enumerate_ptrs.push(None);
                }
                IterStep::Enumerate => {
                    // Register tuple type so downstream closures can reference it
                    let inner_semantic = llvm_to_semantic_name(&current_elem_ty);
                    let tuple_name = format!("tuple.int.{}", inner_semantic);
                    self.ensure_pipeline_tuple_type(
                        &tuple_name,
                        &["i64".to_string(), current_elem_ty.clone()],
                    );
                    current_elem_ty = format!("%{}", tuple_name);

                    let counter_ptr = self.emit_alloca_store("i64", "0");
                    closure_infos.push(None);
                    zip_infos.push(None);
                    take_skip_ptrs.push(None);
                    enumerate_ptrs.push(Some(counter_ptr));
                }
                IterStep::Zip(zip_expr) => {
                    let zip_arr = self.emit_expr(zip_expr)?;
                    let zip_elem_ty = self.infer_array_elem_type(zip_expr);

                    // Register tuple type so downstream closures can reference it
                    let left_semantic = llvm_to_semantic_name(&current_elem_ty);
                    let right_semantic = llvm_to_semantic_name(&zip_elem_ty);
                    let tuple_name = format!("tuple.{}.{}", left_semantic, right_semantic);
                    self.ensure_pipeline_tuple_type(
                        &tuple_name,
                        &[current_elem_ty.clone(), zip_elem_ty.clone()],
                    );
                    current_elem_ty = format!("%{}", tuple_name);

                    let (zip_data, zip_len) = self.emit_fat_ptr_load(&zip_arr);
                    let zip_idx = self.emit_alloca_store("i64", "0");
                    closure_infos.push(None);
                    zip_infos.push(Some(ZipInfo {
                        data_ptr: zip_data,
                        len_val: zip_len,
                        idx_ptr: zip_idx,
                        elem_ty: zip_elem_ty,
                    }));
                    take_skip_ptrs.push(None);
                    enumerate_ptrs.push(None);
                }
                IterStep::Take(n_expr) | IterStep::Skip(n_expr) => {
                    let n_val = self.emit_expr(n_expr)?;
                    let remaining_ptr = self.emit_alloca_store("i64", &n_val);
                    closure_infos.push(None);
                    zip_infos.push(None);
                    take_skip_ptrs.push(Some(remaining_ptr));
                    enumerate_ptrs.push(None);
                }
            }
        }

        // For range sources, cap alloc_len at known pipeline bounds (take / zip)
        // so that e.g. (0..=i64::MAX).iter().take(1).collect() or
        // (0..=i64::MAX).iter().zip([1]).collect() don't try to malloc
        // i64::MAX elements.
        let len_val = if is_range_source {
            let mut capped = len_val;
            for (i, step) in steps.iter().enumerate() {
                match step {
                    IterStep::Take(_) => {
                        if let Some(ref take_ptr) = take_skip_ptrs[i] {
                            let take_raw = self.emit_load("i64", take_ptr);
                            // Clamp negative take counts to 0
                            let take_neg = self.fresh_temp();
                            self.emit_line(&format!("{} = icmp slt i64 {}, 0", take_neg, take_raw));
                            let take_val = self.fresh_temp();
                            self.emit_line(&format!(
                                "{} = select i1 {}, i64 0, i64 {}",
                                take_val, take_neg, take_raw
                            ));
                            capped = self.emit_min_i64(&capped, &take_val);
                        }
                        break; // take bounds all subsequent output
                    }
                    IterStep::Zip(_) => {
                        if let Some(ref zi) = zip_infos[i] {
                            capped = self.emit_min_i64(&capped, &zi.len_val);
                        }
                    }
                    _ => {}
                }
            }
            capped
        } else {
            len_val
        };

        Ok((
            data_ptr,
            len_val,
            src_elem_ty,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            range_end,
        ))
    }

    /// Emit `min(a, b)` for two i64 SSA values, returning the result SSA name.
    pub(crate) fn emit_min_i64(&mut self, a: &str, b: &str) -> String {
        let is_less = self.fresh_temp();
        self.emit_line(&format!("{} = icmp slt i64 {}, {}", is_less, b, a));
        let result = self.fresh_temp();
        self.emit_line(&format!(
            "{} = select i1 {}, i64 {}, i64 {}",
            result, is_less, b, a
        ));
        result
    }

    /// Emit the loop condition for a pipeline.
    /// For arrays: `idx < len`.  For ranges: `(start + idx) < end` or `<= end`,
    /// avoiding overflow from pre-computing the total length.
    pub(crate) fn emit_pipeline_loop_cond(
        &mut self,
        idx_ptr: &str,
        data_ptr: &str,
        len_val: &str,
        range_end: &Option<(String, bool)>,
        step_label: &str,
        end_label: &str,
    ) -> String {
        let cur_idx = self.emit_load("i64", idx_ptr);
        let cmp = self.fresh_temp();
        if let Some((ref end_val, inclusive)) = *range_end {
            // Range source: compare actual range value against end bound.
            // Guard against idx overflow (negative after wrap) and add overflow
            // so inclusive ranges ending at i64::MAX still terminate.
            let idx_non_neg = self.fresh_temp();
            self.emit_line(&format!("{} = icmp sge i64 {}, 0", idx_non_neg, cur_idx));
            let sum_pair = self.fresh_temp();
            self.emit_line(&format!(
                "{} = call {{ i64, i1 }} @llvm.sadd.with.overflow.i64(i64 {}, i64 {})",
                sum_pair, data_ptr, cur_idx
            ));
            let cur_val = self.fresh_temp();
            self.emit_line(&format!(
                "{} = extractvalue {{ i64, i1 }} {}, 0",
                cur_val, sum_pair
            ));
            let add_overflow = self.fresh_temp();
            self.emit_line(&format!(
                "{} = extractvalue {{ i64, i1 }} {}, 1",
                add_overflow, sum_pair
            ));
            let cmp_op = if inclusive { "sle" } else { "slt" };
            let in_bound = self.fresh_temp();
            self.emit_line(&format!(
                "{} = icmp {} i64 {}, {}",
                in_bound, cmp_op, cur_val, end_val
            ));
            let no_add_overflow = self.fresh_temp();
            self.emit_line(&format!(
                "{} = xor i1 {}, true",
                no_add_overflow, add_overflow
            ));
            let valid_arith = self.fresh_temp();
            self.emit_line(&format!(
                "{} = and i1 {}, {}",
                valid_arith, idx_non_neg, no_add_overflow
            ));
            self.emit_line(&format!("{} = and i1 {}, {}", cmp, valid_arith, in_bound));
        } else {
            // Array source: compare index against length.
            self.emit_line(&format!("{} = icmp slt i64 {}, {}", cmp, cur_idx, len_val));
        }
        self.emit_cond_branch(&cmp, step_label, end_label);
        cur_idx
    }

    pub(crate) fn emit_for_pipeline(
        &mut self,
        var_name: &str,
        pipeline: &IterPipeline<'_>,
        body: &Block,
    ) -> Result<(), CodegenError> {
        // 1. Shared preamble: emit source, load data/len, pre-emit step closures
        let (
            data_ptr,
            len_val,
            src_elem_ty,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            range_end,
        ) = self.emit_pipeline_preamble(
            pipeline.source,
            &pipeline.steps,
            pipeline.is_range_source,
        )?;
        let is_range = pipeline.is_range_source;

        // 2. Determine final output type
        let final_elem_ty = self.pipeline_final_elem_type(&src_elem_ty, &pipeline.steps);

        // 3. Alloca for index counter and loop variable
        let idx_ptr = self.emit_alloca_store("i64", "0");

        let var_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", var_ptr, final_elem_ty));

        let cond_label = self.fresh_label("for.cond");
        let step_label = self.fresh_label("for.step");
        let body_label = self.fresh_label("for.body");
        let end_label = self.fresh_label("for.end");

        self.emit_line(&format!("br label %{}", cond_label));

        // 4. Condition
        self.emit_label(&cond_label);
        self.block_terminated = false;
        let cur_idx = self.emit_pipeline_loop_cond(
            &idx_ptr,
            &data_ptr,
            &len_val,
            &range_end,
            &step_label,
            &end_label,
        );

        // 5. Step block: load element, increment index, apply pipeline steps
        self.emit_label(&step_label);
        self.block_terminated = false;
        let (current_val, _) = self.emit_pipeline_steps(
            &data_ptr,
            &src_elem_ty,
            &cur_idx,
            &idx_ptr,
            &cond_label,
            &body_label,
            &end_label,
            &pipeline.steps,
            &closure_infos,
            &zip_infos,
            &take_skip_ptrs,
            &enumerate_ptrs,
            is_range,
        )?;

        // 6. Body block
        self.emit_label(&body_label);
        self.block_terminated = false;

        self.push_scope();

        // Store final value into loop variable (current_val is always by-value SSA)
        self.emit_line(&format!(
            "store {} {}, ptr {}",
            final_elem_ty, current_val, var_ptr
        ));
        self.define_var(var_name, &var_ptr, &final_elem_ty);

        // continue → cond (index already incremented in step block)
        // break → end
        self.loop_labels
            .push((cond_label.clone(), end_label.clone()));
        self.emit_block(body)?;
        self.loop_labels.pop();

        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", cond_label));
        }

        self.pop_scope();

        // 7. End block
        self.emit_label(&end_label);
        self.block_terminated = false;
        Ok(())
    }

    // ── Pipeline terminator emission ──────────────────────────

    /// Emit a terminated pipeline expression (collect, fold, any, all, find, reduce).
    pub(crate) fn emit_pipeline_terminator(
        &mut self,
        pipeline: &TerminatedPipeline<'_>,
    ) -> Result<String, CodegenError> {
        // 1. Shared preamble: emit source, load data/len, pre-emit step closures
        let (
            data_ptr,
            len_val,
            src_elem_ty,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            range_end,
        ) = self.emit_pipeline_preamble(
            pipeline.source,
            &pipeline.steps,
            pipeline.is_range_source,
        )?;
        let is_range = pipeline.is_range_source;

        // Emit terminator closure if any
        let term_closure_info = match &pipeline.terminator {
            PipelineTerminator::Any(c)
            | PipelineTerminator::All(c)
            | PipelineTerminator::Find(c) => {
                let (fn_ptr, env_ptr, ret_ty) = self.emit_pipeline_closure(c)?;
                Some(ClosureInfo {
                    fn_ptr,
                    env_ptr,
                    ret_ty,
                })
            }
            PipelineTerminator::Reduce(c) => {
                let (fn_ptr, env_ptr, ret_ty) = self.emit_pipeline_closure(c)?;
                Some(ClosureInfo {
                    fn_ptr,
                    env_ptr,
                    ret_ty,
                })
            }
            PipelineTerminator::Fold(_, c) => {
                let (fn_ptr, env_ptr, ret_ty) = self.emit_pipeline_closure(c)?;
                Some(ClosureInfo {
                    fn_ptr,
                    env_ptr,
                    ret_ty,
                })
            }
            PipelineTerminator::Collect => None,
        };

        // Determine element type after all pipeline steps (before terminator)
        let final_elem_ty = self.pipeline_final_elem_type(&src_elem_ty, &pipeline.steps);

        // 3. Alloca for index counter
        let idx_ptr = self.emit_alloca_store("i64", "0");

        // 4. Dispatch to specific terminator emitter
        match &pipeline.terminator {
            PipelineTerminator::Collect => self.emit_terminator_collect(
                &data_ptr,
                &len_val,
                &idx_ptr,
                &src_elem_ty,
                &final_elem_ty,
                &pipeline.steps,
                &closure_infos,
                &zip_infos,
                &take_skip_ptrs,
                &enumerate_ptrs,
                is_range,
                &range_end,
            ),
            PipelineTerminator::Fold(init_expr, _) => {
                let init_val = self.emit_expr(init_expr)?;
                let tci = term_closure_info.unwrap();
                self.emit_terminator_fold(
                    &data_ptr,
                    &len_val,
                    &idx_ptr,
                    &src_elem_ty,
                    &final_elem_ty,
                    &pipeline.steps,
                    &closure_infos,
                    &zip_infos,
                    &take_skip_ptrs,
                    &enumerate_ptrs,
                    &init_val,
                    &tci.fn_ptr,
                    &tci.env_ptr,
                    &tci.ret_ty,
                    is_range,
                    &range_end,
                )
            }
            PipelineTerminator::Any(_) => {
                let tci = term_closure_info.unwrap();
                self.emit_terminator_any_all(
                    true,
                    &data_ptr,
                    &len_val,
                    &idx_ptr,
                    &src_elem_ty,
                    &final_elem_ty,
                    &pipeline.steps,
                    &closure_infos,
                    &zip_infos,
                    &take_skip_ptrs,
                    &enumerate_ptrs,
                    &tci.fn_ptr,
                    &tci.env_ptr,
                    is_range,
                    &range_end,
                )
            }
            PipelineTerminator::All(_) => {
                let tci = term_closure_info.unwrap();
                self.emit_terminator_any_all(
                    false,
                    &data_ptr,
                    &len_val,
                    &idx_ptr,
                    &src_elem_ty,
                    &final_elem_ty,
                    &pipeline.steps,
                    &closure_infos,
                    &zip_infos,
                    &take_skip_ptrs,
                    &enumerate_ptrs,
                    &tci.fn_ptr,
                    &tci.env_ptr,
                    is_range,
                    &range_end,
                )
            }
            PipelineTerminator::Find(_) => {
                let tci = term_closure_info.unwrap();
                self.emit_terminator_find(
                    &data_ptr,
                    &len_val,
                    &idx_ptr,
                    &src_elem_ty,
                    &final_elem_ty,
                    &pipeline.steps,
                    &closure_infos,
                    &zip_infos,
                    &take_skip_ptrs,
                    &enumerate_ptrs,
                    &tci.fn_ptr,
                    &tci.env_ptr,
                    is_range,
                    &range_end,
                )
            }
            PipelineTerminator::Reduce(_) => {
                let tci = term_closure_info.unwrap();
                self.emit_terminator_reduce(
                    &data_ptr,
                    &len_val,
                    &idx_ptr,
                    &src_elem_ty,
                    &final_elem_ty,
                    &pipeline.steps,
                    &closure_infos,
                    &zip_infos,
                    &take_skip_ptrs,
                    &enumerate_ptrs,
                    &tci.fn_ptr,
                    &tci.env_ptr,
                    &tci.ret_ty,
                    is_range,
                    &range_end,
                )
            }
        }
    }

    /// Emit the pipeline step chain (shared between for-loop and terminators).
    /// Emits the step block body: loads element, increments index, applies pipeline.
    /// Returns (current_val, current_ty) after all steps are applied.
    /// `cond_label` is the target for filter-skip, `body_label` is the final target,
    /// `end_label` is for take/zip exhaustion.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit_pipeline_steps(
        &mut self,
        data_ptr: &str,
        src_elem_ty: &str,
        cur_idx: &str,
        idx_ptr: &str,
        cond_label: &str,
        body_label: &str,
        end_label: &str,
        steps: &[IterStep<'_>],
        closure_infos: &[Option<ClosureInfo>],
        zip_infos: &[Option<ZipInfo>],
        take_skip_ptrs: &[Option<String>],
        enumerate_ptrs: &[Option<String>],
        is_range_source: bool,
    ) -> Result<(String, String), CodegenError> {
        // Load element at current index
        let mut current_val = if is_range_source {
            // Range source: element = start + index
            let val = self.fresh_temp();
            self.emit_line(&format!("{} = add i64 {}, {}", val, data_ptr, cur_idx));
            val
        } else {
            let elem_gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr {}, ptr {}, i64 {}",
                elem_gep, src_elem_ty, data_ptr, cur_idx
            ));
            self.emit_load(src_elem_ty, &elem_gep)
        };

        // Increment index (always, even if filtered out)
        let next_idx = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", next_idx, cur_idx));
        self.emit_line(&format!("store i64 {}, ptr {}", next_idx, idx_ptr));

        // Apply each pipeline step
        let mut current_ty = src_elem_ty.to_string();
        let step_count = steps.len();
        for (i, step) in steps.iter().enumerate() {
            let is_last = i + 1 == step_count;
            match step {
                IterStep::Filter(_) => {
                    let ci = closure_infos[i].as_ref().unwrap();
                    let result = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = call i1 {}(ptr {}, {} {})",
                        result, ci.fn_ptr, ci.env_ptr, current_ty, current_val
                    ));
                    let next_label = if !is_last {
                        self.fresh_label("for.pipe")
                    } else {
                        body_label.to_string()
                    };
                    self.emit_cond_branch(&result, &next_label, cond_label);
                    if !is_last {
                        self.emit_label(&next_label);
                        self.block_terminated = false;
                    }
                }
                IterStep::Map(_) => {
                    let ci = closure_infos[i].as_ref().unwrap();
                    let result = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = call {} {}(ptr {}, {} {})",
                        result, ci.ret_ty, ci.fn_ptr, ci.env_ptr, current_ty, current_val
                    ));
                    current_val = result;
                    current_ty = ci.ret_ty.clone();
                    if is_last {
                        self.emit_line(&format!("br label %{}", body_label));
                    }
                }
                IterStep::Enumerate => {
                    let counter_ptr = enumerate_ptrs[i].as_ref().unwrap();
                    let counter_val = self.emit_load("i64", counter_ptr);
                    let inner_semantic = llvm_to_semantic_name(&current_ty);
                    let tuple_name = format!("tuple.int.{}", inner_semantic);
                    let inner_llvm = current_ty.clone();
                    self.ensure_pipeline_tuple_type(
                        &tuple_name,
                        &["i64".to_string(), inner_llvm.clone()],
                    );
                    let tuple_ptr = self.fresh_temp();
                    self.emit_line(&format!("{} = alloca %{}", tuple_ptr, tuple_name));
                    self.emit_struct_field_store(&tuple_name, &tuple_ptr, 0, "i64", &counter_val);
                    self.emit_struct_field_store(
                        &tuple_name,
                        &tuple_ptr,
                        1,
                        &inner_llvm,
                        &current_val,
                    );
                    let next_counter = self.fresh_temp();
                    self.emit_line(&format!("{} = add i64 {}, 1", next_counter, counter_val));
                    self.emit_line(&format!("store i64 {}, ptr {}", next_counter, counter_ptr));
                    // Load by-value so current_val is always an SSA value, not a pointer
                    let tuple_loaded = self.emit_load(&format!("%{}", tuple_name), &tuple_ptr);
                    current_val = tuple_loaded;
                    current_ty = format!("%{}", tuple_name);
                    if is_last {
                        self.emit_line(&format!("br label %{}", body_label));
                    }
                }
                IterStep::Zip(_) => {
                    let zi = zip_infos[i].as_ref().unwrap();
                    let zip_cur = self.emit_load("i64", &zi.idx_ptr);
                    let zip_cmp = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = icmp slt i64 {}, {}",
                        zip_cmp, zip_cur, zi.len_val
                    ));
                    let zip_cont = self.fresh_label("for.zip.cont");
                    self.emit_cond_branch(&zip_cmp, &zip_cont, end_label);
                    self.emit_label(&zip_cont);
                    self.block_terminated = false;
                    let zip_gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr {}, ptr {}, i64 {}",
                        zip_gep, zi.elem_ty, zi.data_ptr, zip_cur
                    ));
                    let zip_val = self.emit_load(&zi.elem_ty, &zip_gep);
                    let zip_next = self.fresh_temp();
                    self.emit_line(&format!("{} = add i64 {}, 1", zip_next, zip_cur));
                    self.emit_line(&format!("store i64 {}, ptr {}", zip_next, zi.idx_ptr));
                    let left_semantic = llvm_to_semantic_name(&current_ty);
                    let right_semantic = llvm_to_semantic_name(&zi.elem_ty);
                    let tuple_name = format!("tuple.{}.{}", left_semantic, right_semantic);
                    self.ensure_pipeline_tuple_type(
                        &tuple_name,
                        &[current_ty.clone(), zi.elem_ty.clone()],
                    );
                    let tuple_ptr = self.fresh_temp();
                    self.emit_line(&format!("{} = alloca %{}", tuple_ptr, tuple_name));
                    self.emit_struct_field_store(
                        &tuple_name,
                        &tuple_ptr,
                        0,
                        &current_ty,
                        &current_val,
                    );
                    self.emit_struct_field_store(&tuple_name, &tuple_ptr, 1, &zi.elem_ty, &zip_val);
                    // Load by-value so current_val is always an SSA value, not a pointer
                    let tuple_loaded = self.emit_load(&format!("%{}", tuple_name), &tuple_ptr);
                    current_val = tuple_loaded;
                    current_ty = format!("%{}", tuple_name);
                    if is_last {
                        self.emit_line(&format!("br label %{}", body_label));
                    }
                }
                IterStep::Take(_) => {
                    let remaining_ptr = take_skip_ptrs[i].as_ref().unwrap();
                    let remaining = self.emit_load("i64", remaining_ptr);
                    let take_cmp = self.fresh_temp();
                    self.emit_line(&format!("{} = icmp sgt i64 {}, 0", take_cmp, remaining));
                    let take_cont = self.fresh_label("for.take.cont");
                    self.emit_cond_branch(&take_cmp, &take_cont, end_label);
                    self.emit_label(&take_cont);
                    self.block_terminated = false;
                    let take_dec = self.fresh_temp();
                    self.emit_line(&format!("{} = sub i64 {}, 1", take_dec, remaining));
                    self.emit_line(&format!("store i64 {}, ptr {}", take_dec, remaining_ptr));
                    if is_last {
                        self.emit_line(&format!("br label %{}", body_label));
                    }
                }
                IterStep::Skip(_) => {
                    let remaining_ptr = take_skip_ptrs[i].as_ref().unwrap();
                    let remaining = self.emit_load("i64", remaining_ptr);
                    let skip_cmp = self.fresh_temp();
                    self.emit_line(&format!("{} = icmp sgt i64 {}, 0", skip_cmp, remaining));
                    let skip_do = self.fresh_label("for.skip.do");
                    let skip_done = self.fresh_label("for.skip.done");
                    self.emit_cond_branch(&skip_cmp, &skip_do, &skip_done);
                    self.emit_label(&skip_do);
                    self.block_terminated = false;
                    let skip_dec = self.fresh_temp();
                    self.emit_line(&format!("{} = sub i64 {}, 1", skip_dec, remaining));
                    self.emit_line(&format!("store i64 {}, ptr {}", skip_dec, remaining_ptr));
                    self.emit_line(&format!("br label %{}", cond_label));
                    self.emit_label(&skip_done);
                    self.block_terminated = false;
                    if is_last {
                        self.emit_line(&format!("br label %{}", body_label));
                    }
                }
            }
        }

        // If no steps, branch to body
        if steps.is_empty() {
            self.emit_line(&format!("br label %{}", body_label));
        }

        Ok((current_val, current_ty))
    }

    // ── Pipeline loop shared skeleton ──────────────────────

    /// Emit the cond → step → body-entry blocks common to every pipeline
    /// terminator.  Returns labels and the pipeline-processed element value
    /// so the caller only has to emit the body-specific and end-specific IR.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit_pipeline_loop_header(
        &mut self,
        prefix: &str,
        data_ptr: &str,
        len_val: &str,
        idx_ptr: &str,
        src_elem_ty: &str,
        steps: &[IterStep<'_>],
        closure_infos: &[Option<ClosureInfo>],
        zip_infos: &[Option<ZipInfo>],
        take_skip_ptrs: &[Option<String>],
        enumerate_ptrs: &[Option<String>],
        is_range_source: bool,
        range_end: &Option<(String, bool)>,
    ) -> Result<PipelineLoopParts, CodegenError> {
        let cond_label = self.fresh_label(&format!("{prefix}.cond"));
        let step_label = self.fresh_label(&format!("{prefix}.step"));
        let body_label = self.fresh_label(&format!("{prefix}.body"));
        let end_label = self.fresh_label(&format!("{prefix}.end"));

        self.emit_line(&format!("br label %{cond_label}"));

        self.emit_label(&cond_label);
        self.block_terminated = false;
        let cur_idx = self.emit_pipeline_loop_cond(
            idx_ptr,
            data_ptr,
            len_val,
            range_end,
            &step_label,
            &end_label,
        );

        self.emit_label(&step_label);
        self.block_terminated = false;
        let (current_val, _) = self.emit_pipeline_steps(
            data_ptr,
            src_elem_ty,
            &cur_idx,
            idx_ptr,
            &cond_label,
            &body_label,
            &end_label,
            steps,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            is_range_source,
        )?;

        self.emit_label(&body_label);
        self.block_terminated = false;

        Ok(PipelineLoopParts {
            cond_label,
            end_label,
            current_val,
        })
    }

    // ── Terminator: collect ─────────────────────────────────

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit_terminator_collect(
        &mut self,
        data_ptr: &str,
        len_val: &str,
        idx_ptr: &str,
        src_elem_ty: &str,
        final_elem_ty: &str,
        steps: &[IterStep<'_>],
        closure_infos: &[Option<ClosureInfo>],
        zip_infos: &[Option<ZipInfo>],
        take_skip_ptrs: &[Option<String>],
        enumerate_ptrs: &[Option<String>],
        is_range_source: bool,
        range_end: &Option<(String, bool)>,
    ) -> Result<String, CodegenError> {
        let elem_size = self.llvm_type_size(final_elem_ty);

        // Cap len_val so that len_val * elem_size cannot overflow i64.
        let max_safe_elems = i64::MAX as usize / elem_size.max(1);
        let len_over = self.fresh_temp();
        self.emit_line(&format!(
            "{} = icmp sgt i64 {}, {}",
            len_over, len_val, max_safe_elems
        ));
        let safe_len = self.fresh_temp();
        self.emit_line(&format!(
            "{} = select i1 {}, i64 {}, i64 {}",
            safe_len, len_over, max_safe_elems, len_val
        ));

        // Allocate output array with capacity = safe_len
        let out_bytes = self.fresh_temp();
        self.emit_line(&format!(
            "{} = mul i64 {}, {}",
            out_bytes, safe_len, elem_size
        ));
        let out_data = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call ptr @malloc(i64 {})",
            out_data, out_bytes
        ));
        let out_idx_ptr = self.emit_alloca_store("i64", "0");

        let lp = self.emit_pipeline_loop_header(
            "col",
            data_ptr,
            len_val,
            idx_ptr,
            src_elem_ty,
            steps,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            is_range_source,
            range_end,
        )?;

        // Body: store element to output array (guard against exceeding safe_len)
        let out_idx = self.emit_load("i64", &out_idx_ptr);
        let cap_ok = self.fresh_temp();
        self.emit_line(&format!(
            "{} = icmp slt i64 {}, {}",
            cap_ok, out_idx, safe_len
        ));
        let store_label = self.fresh_label("col.store");
        self.emit_cond_branch(&cap_ok, &store_label, &lp.end_label);
        self.emit_label(&store_label);
        self.block_terminated = false;
        let out_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {}, ptr {}, i64 {}",
            out_gep, final_elem_ty, out_data, out_idx
        ));
        self.emit_line(&format!(
            "store {} {}, ptr {}",
            final_elem_ty, lp.current_val, out_gep
        ));
        let next_out = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", next_out, out_idx));
        self.emit_line(&format!("store i64 {}, ptr {}", next_out, out_idx_ptr));
        self.emit_line(&format!("br label %{}", lp.cond_label));

        // End: build fat pointer
        self.emit_label(&lp.end_label);
        self.block_terminated = false;
        let final_len = self.emit_load("i64", &out_idx_ptr);
        let fat = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat));
        self.emit_fat_ptr_init(&fat, &out_data, &final_len, &safe_len);
        Ok(fat)
    }

    // ── Terminator: fold ─────────────────────────────────

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit_terminator_fold(
        &mut self,
        data_ptr: &str,
        len_val: &str,
        idx_ptr: &str,
        src_elem_ty: &str,
        final_elem_ty: &str,
        steps: &[IterStep<'_>],
        closure_infos: &[Option<ClosureInfo>],
        zip_infos: &[Option<ZipInfo>],
        take_skip_ptrs: &[Option<String>],
        enumerate_ptrs: &[Option<String>],
        init_val: &str,
        term_fn_ptr: &str,
        term_env_ptr: &str,
        acc_ty: &str,
        is_range_source: bool,
        range_end: &Option<(String, bool)>,
    ) -> Result<String, CodegenError> {
        let acc_ptr = self.emit_alloca_store(acc_ty, init_val);

        let lp = self.emit_pipeline_loop_header(
            "fold",
            data_ptr,
            len_val,
            idx_ptr,
            src_elem_ty,
            steps,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            is_range_source,
            range_end,
        )?;

        // Body: call closure(env, acc, elem) → new acc
        let acc_val = self.emit_load(acc_ty, &acc_ptr);
        let new_acc = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call {} {}(ptr {}, {} {}, {} {})",
            new_acc,
            acc_ty,
            term_fn_ptr,
            term_env_ptr,
            acc_ty,
            acc_val,
            final_elem_ty,
            lp.current_val
        ));
        self.emit_line(&format!("store {} {}, ptr {}", acc_ty, new_acc, acc_ptr));
        self.emit_line(&format!("br label %{}", lp.cond_label));

        // End: return accumulator
        self.emit_label(&lp.end_label);
        self.block_terminated = false;
        let result = self.emit_load(acc_ty, &acc_ptr);
        Ok(result)
    }

    // ── Terminator: any / all (unified) ─────────────────

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit_terminator_any_all(
        &mut self,
        is_any: bool,
        data_ptr: &str,
        len_val: &str,
        idx_ptr: &str,
        src_elem_ty: &str,
        final_elem_ty: &str,
        steps: &[IterStep<'_>],
        closure_infos: &[Option<ClosureInfo>],
        zip_infos: &[Option<ZipInfo>],
        take_skip_ptrs: &[Option<String>],
        enumerate_ptrs: &[Option<String>],
        term_fn_ptr: &str,
        term_env_ptr: &str,
        is_range_source: bool,
        range_end: &Option<(String, bool)>,
    ) -> Result<String, CodegenError> {
        let prefix = if is_any { "any" } else { "all" };
        let init_val = if is_any { "false" } else { "true" };
        let short_circuit_val = if is_any { "true" } else { "false" };

        let result_ptr = self.emit_alloca_store("i1", init_val);

        let lp = self.emit_pipeline_loop_header(
            prefix,
            data_ptr,
            len_val,
            idx_ptr,
            src_elem_ty,
            steps,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            is_range_source,
            range_end,
        )?;

        // Body: call predicate, short-circuit on match
        let pred = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call i1 {}(ptr {}, {} {})",
            pred, term_fn_ptr, term_env_ptr, final_elem_ty, lp.current_val
        ));
        let short_label = self.fresh_label(&format!("{prefix}.short"));
        let (true_target, false_target) = if is_any {
            (short_label.as_str(), lp.cond_label.as_str())
        } else {
            (lp.cond_label.as_str(), short_label.as_str())
        };
        self.emit_cond_branch(&pred, true_target, false_target);

        self.emit_label(&short_label);
        self.block_terminated = false;
        self.emit_line(&format!(
            "store i1 {}, ptr {}",
            short_circuit_val, result_ptr
        ));
        self.emit_line(&format!("br label %{}", lp.end_label));

        // End: return result
        self.emit_label(&lp.end_label);
        self.block_terminated = false;
        let result = self.emit_load("i1", &result_ptr);
        Ok(result)
    }

    // ── Terminator: find ─────────────────────────────────

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit_terminator_find(
        &mut self,
        data_ptr: &str,
        len_val: &str,
        idx_ptr: &str,
        src_elem_ty: &str,
        final_elem_ty: &str,
        steps: &[IterStep<'_>],
        closure_infos: &[Option<ClosureInfo>],
        zip_infos: &[Option<ZipInfo>],
        take_skip_ptrs: &[Option<String>],
        enumerate_ptrs: &[Option<String>],
        term_fn_ptr: &str,
        term_env_ptr: &str,
        is_range_source: bool,
        range_end: &Option<(String, bool)>,
    ) -> Result<String, CodegenError> {
        let option_name = self.yorum_type_to_option_name(final_elem_ty);
        let option_llvm = format!("%{}", option_name);

        // Alloca result as None (tag=1)
        let result_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", result_ptr, option_llvm));
        self.emit_struct_field_store(&option_name, &result_ptr, 0, "i32", "1");

        let lp = self.emit_pipeline_loop_header(
            "find",
            data_ptr,
            len_val,
            idx_ptr,
            src_elem_ty,
            steps,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            is_range_source,
            range_end,
        )?;

        // Body: call predicate, branch to found on match
        let pred = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call i1 {}(ptr {}, {} {})",
            pred, term_fn_ptr, term_env_ptr, final_elem_ty, lp.current_val
        ));
        let found_label = self.fresh_label("find.found");
        self.emit_cond_branch(&pred, &found_label, &lp.cond_label);

        // Found: store Some(val)
        self.emit_label(&found_label);
        self.block_terminated = false;
        self.emit_struct_field_store(&option_name, &result_ptr, 0, "i32", "0");
        self.emit_struct_field_store(&option_name, &result_ptr, 1, final_elem_ty, &lp.current_val);
        self.emit_line(&format!("br label %{}", lp.end_label));

        // End
        self.emit_label(&lp.end_label);
        self.block_terminated = false;
        Ok(result_ptr)
    }

    // ── Terminator: reduce ─────────────────────────────────

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit_terminator_reduce(
        &mut self,
        data_ptr: &str,
        len_val: &str,
        idx_ptr: &str,
        src_elem_ty: &str,
        final_elem_ty: &str,
        steps: &[IterStep<'_>],
        closure_infos: &[Option<ClosureInfo>],
        zip_infos: &[Option<ZipInfo>],
        take_skip_ptrs: &[Option<String>],
        enumerate_ptrs: &[Option<String>],
        term_fn_ptr: &str,
        term_env_ptr: &str,
        ret_ty: &str,
        is_range_source: bool,
        range_end: &Option<(String, bool)>,
    ) -> Result<String, CodegenError> {
        let option_name = self.yorum_type_to_option_name(final_elem_ty);
        let option_llvm = format!("%{}", option_name);

        // Alloca result as None (tag=1)
        let result_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", result_ptr, option_llvm));
        self.emit_struct_field_store(&option_name, &result_ptr, 0, "i32", "1");

        // Alloca accumulator and has_value flag
        let acc_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", acc_ptr, ret_ty));
        let has_value_ptr = self.emit_alloca_store("i1", "false");

        let lp = self.emit_pipeline_loop_header(
            "red",
            data_ptr,
            len_val,
            idx_ptr,
            src_elem_ty,
            steps,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            is_range_source,
            range_end,
        )?;

        // Body: check has_value to decide first vs accumulate
        let hv = self.emit_load("i1", &has_value_ptr);
        let first_label = self.fresh_label("red.first");
        let accum_label = self.fresh_label("red.accum");
        let cont_label = self.fresh_label("red.cont");
        self.emit_cond_branch(&hv, &accum_label, &first_label);

        // First element: store as initial accumulator
        self.emit_label(&first_label);
        self.block_terminated = false;
        self.emit_line(&format!(
            "store {} {}, ptr {}",
            final_elem_ty, lp.current_val, acc_ptr
        ));
        self.emit_line(&format!("store i1 true, ptr {}", has_value_ptr));
        self.emit_line(&format!("br label %{}", cont_label));

        // Accumulate: call closure(env, acc, val) → new acc
        self.emit_label(&accum_label);
        self.block_terminated = false;
        let acc_val = self.emit_load(ret_ty, &acc_ptr);
        let new_acc = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call {} {}(ptr {}, {} {}, {} {})",
            new_acc,
            ret_ty,
            term_fn_ptr,
            term_env_ptr,
            ret_ty,
            acc_val,
            final_elem_ty,
            lp.current_val
        ));
        self.emit_line(&format!("store {} {}, ptr {}", ret_ty, new_acc, acc_ptr));
        self.emit_line(&format!("br label %{}", cont_label));

        self.emit_label(&cont_label);
        self.block_terminated = false;
        self.emit_line(&format!("br label %{}", lp.cond_label));

        // End: if has_value, store Some(acc) to result
        self.emit_label(&lp.end_label);
        self.block_terminated = false;
        let final_hv = self.emit_load("i1", &has_value_ptr);
        let some_label = self.fresh_label("red.some");
        let done_label = self.fresh_label("red.done");
        self.emit_cond_branch(&final_hv, &some_label, &done_label);

        self.emit_label(&some_label);
        self.block_terminated = false;
        self.emit_struct_field_store(&option_name, &result_ptr, 0, "i32", "0");
        let final_acc = self.emit_load(ret_ty, &acc_ptr);
        self.emit_struct_field_store(&option_name, &result_ptr, 1, ret_ty, &final_acc);
        self.emit_line(&format!("br label %{}", done_label));

        self.emit_label(&done_label);
        self.block_terminated = false;
        Ok(result_ptr)
    }

    /// Map an LLVM element type to the monomorphized Option enum name.
    pub(crate) fn yorum_type_to_option_name(&self, llvm_ty: &str) -> String {
        let base = match llvm_ty {
            "i64" => "int",
            "double" => "float",
            "i1" => "bool",
            "i8" => "char",
            "ptr" => "string",
            _ => {
                if let Some(name) = llvm_ty.strip_prefix('%') {
                    return format!("Option__{}", name);
                }
                "int"
            }
        };
        format!("Option__{}", base)
    }

    pub(crate) fn emit_for_range(
        &mut self,
        var_name: &str,
        start_expr: &Expr,
        end_expr: &Expr,
        inclusive: bool,
        body: &Block,
    ) -> Result<(), CodegenError> {
        // Evaluate start and end bounds
        let start_val = self.emit_expr(start_expr)?;
        let end_val = self.emit_expr(end_expr)?;

        // Alloca for the loop counter (also the loop variable)
        // Use fresh_temp() to avoid duplicate names when the same variable
        // name is used in multiple for-range loops in one function.
        let var_ptr = self.emit_alloca_store("i64", &start_val);

        let cond_label = self.fresh_label("for.cond");
        let body_label = self.fresh_label("for.body");
        let inc_label = self.fresh_label("for.inc");
        let end_label = self.fresh_label("for.end");

        self.emit_line(&format!("br label %{}", cond_label));

        // Condition: counter < end (exclusive) or counter <= end (inclusive)
        self.emit_label(&cond_label);
        self.block_terminated = false;
        let cur_val = self.emit_load("i64", &var_ptr);
        let cmp = self.fresh_temp();
        let cmp_op = if inclusive { "sle" } else { "slt" };
        self.emit_line(&format!(
            "{} = icmp {} i64 {}, {}",
            cmp, cmp_op, cur_val, end_val
        ));
        self.emit_cond_branch(&cmp, &body_label, &end_label);

        // Body
        self.emit_label(&body_label);
        self.block_terminated = false;

        self.push_scope();
        self.define_var(var_name, &var_ptr, "i64");

        self.loop_labels
            .push((inc_label.clone(), end_label.clone()));
        self.emit_block(body)?;
        self.loop_labels.pop();

        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", inc_label));
        }

        self.pop_scope();

        // Increment block
        self.emit_label(&inc_label);
        self.block_terminated = false;
        let cur_val2 = self.emit_load("i64", &var_ptr);
        if inclusive {
            // Guard against overflow: if counter == end, this was the last
            // iteration — exit instead of incrementing (which would wrap
            // i64::MAX to i64::MIN and loop forever).
            let at_end = self.fresh_temp();
            self.emit_line(&format!(
                "{} = icmp eq i64 {}, {}",
                at_end, cur_val2, end_val
            ));
            let inc_do_label = self.fresh_label("for.inc.do");
            self.emit_cond_branch(&at_end, &end_label, &inc_do_label);
            self.emit_label(&inc_do_label);
            self.block_terminated = false;
        }
        let next_val = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", next_val, cur_val2));
        self.emit_line(&format!("store i64 {}, ptr {}", next_val, var_ptr));
        self.emit_line(&format!("br label %{}", cond_label));

        self.emit_label(&end_label);
        self.block_terminated = false;
        Ok(())
    }
}
