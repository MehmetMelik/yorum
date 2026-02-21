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
                    "chain" => {
                        if args.len() == 1 {
                            steps.push(IterStep::Chain(&args[0]));
                            current = receiver;
                            continue;
                        }
                        return None;
                    }
                    "take_while" => {
                        if args.len() == 1 {
                            if let ExprKind::Closure(ref c) = args[0].kind {
                                steps.push(IterStep::TakeWhile(c));
                                current = receiver;
                                continue;
                            }
                        }
                        return None;
                    }
                    "flat_map" => {
                        if args.len() == 1 {
                            if let ExprKind::Closure(ref c) = args[0].kind {
                                steps.push(IterStep::FlatMap(c));
                                current = receiver;
                                continue;
                            }
                        }
                        return None;
                    }
                    "flatten" => {
                        if args.is_empty() {
                            steps.push(IterStep::Flatten);
                            current = receiver;
                            continue;
                        }
                        return None;
                    }
                    "rev" => {
                        if args.is_empty() {
                            steps.push(IterStep::Rev);
                            current = receiver;
                            continue;
                        }
                        return None;
                    }
                    "iter" => {
                        if args.is_empty() {
                            let is_range = matches!(
                                receiver.kind,
                                ExprKind::Range(_, _)
                                    | ExprKind::RangeInclusive(_, _)
                                    | ExprKind::RangeFrom(_)
                            );
                            let is_unbounded = matches!(receiver.kind, ExprKind::RangeFrom(_));
                            if is_range || self.expr_struct_name(receiver).is_err() {
                                if steps.is_empty() {
                                    return None; // Just .iter() with no combinators
                                }
                                steps.reverse(); // We walked right-to-left
                                return Some(IterPipeline {
                                    source: receiver,
                                    steps,
                                    is_range_source: is_range,
                                    is_unbounded_range: is_unbounded,
                                });
                            }
                        }
                        return None; // Struct .iter() or .iter() with args
                    }
                    "chars" => {
                        if args.is_empty() {
                            if steps.is_empty() {
                                return None; // Just .chars() with no combinators — handle in emit_for
                            }
                            steps.reverse();
                            // Use the full MethodCall as the source so emit_pipeline_preamble
                            // can detect the .chars() call and set up string iteration.
                            return Some(IterPipeline {
                                source: current, // current is the chars() MethodCall node
                                steps,
                                is_range_source: false,
                                is_unbounded_range: false,
                            });
                        }
                        return None;
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
                "map" | "filter" | "enumerate" | "zip" | "take" | "skip" | "chain"
                | "take_while" | "rev" | "flat_map" | "flatten" => Self::has_iter_base(receiver),
                _ => false,
            }
        } else {
            false
        }
    }

    pub(crate) fn has_iter_base(expr: &Expr) -> bool {
        if let ExprKind::MethodCall(ref receiver, ref method, _) = expr.kind {
            match method.as_str() {
                "iter" | "chars" => true,
                "map" | "filter" | "enumerate" | "zip" | "take" | "skip" | "chain" | "reduce"
                | "fold" | "collect" | "find" | "any" | "all" | "take_while" | "rev" | "sum"
                | "count" | "position" | "flat_map" | "flatten" => Self::has_iter_base(receiver),
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
                "sum" if args.is_empty() => PipelineTerminator::Sum,
                "count" if args.is_empty() => PipelineTerminator::Count,
                "position" if args.len() == 1 => {
                    if let ExprKind::Closure(ref c) = args[0].kind {
                        PipelineTerminator::Position(c)
                    } else {
                        return None;
                    }
                }
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
                    is_unbounded_range: pipeline.is_unbounded_range,
                });
            }

            // Handle bare .iter() or .chars() with no combinators
            if let ExprKind::MethodCall(ref inner_recv, ref inner_method, ref inner_args) =
                receiver.kind
            {
                if inner_method == "iter" && inner_args.is_empty() {
                    let is_range = matches!(
                        inner_recv.kind,
                        ExprKind::Range(_, _)
                            | ExprKind::RangeInclusive(_, _)
                            | ExprKind::RangeFrom(_)
                    );
                    let is_unbounded = matches!(inner_recv.kind, ExprKind::RangeFrom(_));
                    if is_range || self.expr_struct_name(inner_recv).is_err() {
                        return Some(TerminatedPipeline {
                            source: inner_recv,
                            steps: Vec::new(),
                            terminator,
                            is_range_source: is_range,
                            is_unbounded_range: is_unbounded,
                        });
                    }
                }
                if inner_method == "chars" && inner_args.is_empty() {
                    // Bare .chars() terminator: use the whole MethodCall as source
                    return Some(TerminatedPipeline {
                        source: receiver,
                        steps: Vec::new(),
                        terminator,
                        is_range_source: false,
                        is_unbounded_range: false,
                    });
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
        // For chars() source, the effective element type is i32 (char) after sext
        let mut ty = if src_elem_ty == "i8" {
            "i32".to_string()
        } else {
            src_elem_ty.to_string()
        };
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
                IterStep::FlatMap(c) => {
                    if let Type::Array(ref inner) = c.return_type {
                        ty = self.llvm_type(inner);
                    }
                }
                IterStep::Flatten => {
                    // Flatten can't derive inner type from LLVM type alone (arrays are ptr).
                    // Leave ty unchanged here — callers with FlatMapInfo should use
                    // pipeline_final_elem_type_with_flat instead.
                }
                IterStep::Filter(_)
                | IterStep::Take(_)
                | IterStep::Skip(_)
                | IterStep::Chain(_)
                | IterStep::TakeWhile(_)
                | IterStep::Rev => {}
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
            Vec<Option<ChainInfo>>,
            Option<MapIterInfo>, // Map.iter() implicit values zip
            Option<FlatMapInfo>, // flat_map/flatten info for nested loop emission
        ),
        CodegenError,
    > {
        let (data_ptr, mut len_val, src_elem_ty, range_end, map_iter_info) = if is_range_source {
            // Unbounded range: (start..).iter() — no end, no length
            if let ExprKind::RangeFrom(ref start_expr) = source.kind {
                let start_val = self.emit_expr(start_expr)?;
                // Use a large allocation hint for terminators that need it (capped by take later)
                (start_val, "16".to_string(), "i64".to_string(), None, None)
            } else {
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
                    None,
                )
            }
        } else if let ExprKind::MethodCall(ref string_recv, ref method, _) = source.kind {
            if method == "chars" {
                // String .chars() source: iterate bytes as char (i32)
                let str_val = self.emit_expr(string_recv)?;
                let str_len = self.fresh_temp();
                self.emit_line(&format!("{} = call i64 @strlen(ptr {})", str_len, str_val));
                (str_val, str_len, "i8".to_string(), None, None)
            } else {
                // Regular array source via method call
                let arr_val = self.emit_expr(source)?;
                let src_elem_ty = self.infer_array_elem_type(source);
                let (data_ptr, len_val) = self.emit_fat_ptr_load(&arr_val);
                (data_ptr, len_val, src_elem_ty, None, None)
            }
        } else {
            // Check if source is a Set or Map variable (monomorphized to Type::Named)
            let collection_mangled = if let ExprKind::Ident(ref name) = source.kind {
                if let Some(Type::Named(ref mangled)) = self.var_ast_types.get(name) {
                    if mangled.starts_with("Set__") || mangled.starts_with("Map__") {
                        Some(mangled.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(ref mangled) = collection_mangled {
                if let Some(suffix) = mangled.strip_prefix("Set__") {
                    // Set.iter(): materialize to array via set_values__T
                    let set_ptr = self.emit_expr(source)?;
                    let fn_name = format!("set_values__{}", suffix);
                    let fat_ptr = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = call ptr @{}(ptr {})",
                        fat_ptr, fn_name, set_ptr
                    ));
                    let elem_type = Self::yorum_name_to_ast_type(suffix);
                    let elem_ty = self.llvm_type(&elem_type);
                    let (data_ptr, len_val) = self.emit_fat_ptr_load(&fat_ptr);
                    (data_ptr, len_val, elem_ty, None, None)
                } else if let Some(suffix) = mangled.strip_prefix("Map__") {
                    // Map.iter(): materialize keys and values arrays
                    let parts: Vec<&str> = suffix.splitn(2, "__").collect();
                    let map_ptr = self.emit_expr(source)?;
                    let keys_fn = format!("map_keys__{}", suffix);
                    let vals_fn = format!("map_values__{}", suffix);
                    let keys_fat = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = call ptr @{}(ptr {})",
                        keys_fat, keys_fn, map_ptr
                    ));
                    let vals_fat = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = call ptr @{}(ptr {})",
                        vals_fat, vals_fn, map_ptr
                    ));
                    let key_type = Self::yorum_name_to_ast_type(parts[0]);
                    let val_type = Self::yorum_name_to_ast_type(parts[1]);
                    let key_elem_ty = self.llvm_type(&key_type);
                    let val_elem_ty = self.llvm_type(&val_type);
                    let (key_data, key_len) = self.emit_fat_ptr_load(&keys_fat);
                    let (val_data, _val_len) = self.emit_fat_ptr_load(&vals_fat);

                    // Register tuple type for (K, V)
                    let key_semantic = llvm_to_semantic_name(&key_elem_ty);
                    let val_semantic = llvm_to_semantic_name(&val_elem_ty);
                    let tuple_name = format!("tuple.{}.{}", key_semantic, val_semantic);
                    self.ensure_pipeline_tuple_type(
                        &tuple_name,
                        &[key_elem_ty.clone(), val_elem_ty.clone()],
                    );
                    let src_elem_ty = format!("%{}", tuple_name);

                    // Use keys as primary source; values info stored in map_iter_info
                    // src_elem_ty is the tuple type so pipeline_final_elem_type works correctly
                    (
                        key_data,
                        key_len,
                        src_elem_ty,
                        None,
                        Some(MapIterInfo {
                            val_data_ptr: val_data,
                            val_elem_ty,
                            key_elem_ty,
                            tuple_name,
                        }),
                    )
                } else {
                    // Unknown prefix — treat as array
                    let arr_val = self.emit_expr(source)?;
                    let src_elem_ty = self.infer_array_elem_type(source);
                    let (data_ptr, len_val) = self.emit_fat_ptr_load(&arr_val);
                    (data_ptr, len_val, src_elem_ty, None, None)
                }
            } else {
                // Emit the source array expression → fat pointer
                let arr_val = self.emit_expr(source)?;
                let src_elem_ty = self.infer_array_elem_type(source);

                // Load data ptr and length from fat pointer
                let (data_ptr, len_val) = self.emit_fat_ptr_load(&arr_val);

                (data_ptr, len_val, src_elem_ty, None, None)
            }
        };

        // Pre-emit closures and prepare step data.
        // Track current_elem_ty so Enumerate/Zip can register tuple types
        // before downstream closures that reference those types are emitted.
        let mut closure_infos: Vec<Option<ClosureInfo>> = Vec::new();
        let mut zip_infos: Vec<Option<ZipInfo>> = Vec::new();
        let mut take_skip_ptrs: Vec<Option<String>> = Vec::new();
        let mut enumerate_ptrs: Vec<Option<String>> = Vec::new();
        let mut chain_infos: Vec<Option<ChainInfo>> = Vec::new();
        let mut current_elem_ty = src_elem_ty.clone();
        let mut flat_map_info: Option<FlatMapInfo> = None;

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
                    chain_infos.push(None);
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
                    chain_infos.push(None);
                }
                IterStep::Zip(zip_expr) => {
                    // Check if zip argument is a range expression
                    let is_zip_range = matches!(
                        zip_expr.kind,
                        ExprKind::Range(_, _) | ExprKind::RangeInclusive(_, _)
                    );

                    if is_zip_range {
                        let (start_expr, end_expr, inclusive) = match &zip_expr.kind {
                            ExprKind::Range(s, e) => (s, e, false),
                            ExprKind::RangeInclusive(s, e) => (s, e, true),
                            _ => unreachable!(),
                        };
                        let start_val = self.emit_expr(start_expr)?;
                        let end_val = self.emit_expr(end_expr)?;

                        // Compute range length for zip bounds
                        let diff = self.fresh_temp();
                        self.emit_line(&format!("{} = sub i64 {}, {}", diff, end_val, start_val));
                        let zip_len = if inclusive {
                            let len_inc = self.fresh_temp();
                            self.emit_line(&format!("{} = add i64 {}, 1", len_inc, diff));
                            len_inc
                        } else {
                            diff
                        };
                        // Clamp negative lengths to 0
                        let is_neg = self.fresh_temp();
                        self.emit_line(&format!("{} = icmp slt i64 {}, 0", is_neg, zip_len));
                        let safe_len = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = select i1 {}, i64 0, i64 {}",
                            safe_len, is_neg, zip_len
                        ));

                        let left_semantic = llvm_to_semantic_name(&current_elem_ty);
                        let tuple_name = format!("tuple.{}.int", left_semantic);
                        self.ensure_pipeline_tuple_type(
                            &tuple_name,
                            &[current_elem_ty.clone(), "i64".to_string()],
                        );
                        current_elem_ty = format!("%{}", tuple_name);

                        let zip_idx = self.emit_alloca_store("i64", "0");
                        closure_infos.push(None);
                        zip_infos.push(Some(ZipInfo {
                            data_ptr: start_val,
                            len_val: safe_len,
                            idx_ptr: zip_idx,
                            elem_ty: "i64".to_string(),
                            range_zip: Some((end_val, inclusive)),
                        }));
                        take_skip_ptrs.push(None);
                        enumerate_ptrs.push(None);
                        chain_infos.push(None);
                    } else {
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
                            range_zip: None,
                        }));
                        take_skip_ptrs.push(None);
                        enumerate_ptrs.push(None);
                        chain_infos.push(None);
                    }
                }
                IterStep::Take(n_expr) | IterStep::Skip(n_expr) => {
                    let n_val = self.emit_expr(n_expr)?;
                    let remaining_ptr = self.emit_alloca_store("i64", &n_val);
                    closure_infos.push(None);
                    zip_infos.push(None);
                    take_skip_ptrs.push(Some(remaining_ptr));
                    enumerate_ptrs.push(None);
                    chain_infos.push(None);
                }
                IterStep::TakeWhile(c) => {
                    let (fn_ptr, env_ptr, ret_ty) = self.emit_pipeline_closure(c)?;
                    closure_infos.push(Some(ClosureInfo {
                        fn_ptr,
                        env_ptr,
                        ret_ty,
                    }));
                    zip_infos.push(None);
                    take_skip_ptrs.push(None);
                    enumerate_ptrs.push(None);
                    chain_infos.push(None);
                }
                IterStep::Rev => {
                    // Rev is handled at the element load level, not in the step chain.
                    // We just need placeholder entries for each info vector.
                    closure_infos.push(None);
                    zip_infos.push(None);
                    take_skip_ptrs.push(None);
                    enumerate_ptrs.push(None);
                    chain_infos.push(None);
                }
                IterStep::Chain(chain_expr) => {
                    let chain_arr = self.emit_expr(chain_expr)?;
                    let chain_elem_ty = self.infer_array_elem_type(chain_expr);
                    let (chain_data, chain_len) = self.emit_fat_ptr_load(&chain_arr);

                    // Save first source length, then compute combined length
                    let first_len = len_val.clone();
                    let total_len = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = add i64 {}, {}",
                        total_len, first_len, chain_len
                    ));
                    len_val = total_len;

                    closure_infos.push(None);
                    zip_infos.push(None);
                    take_skip_ptrs.push(None);
                    enumerate_ptrs.push(None);
                    chain_infos.push(Some(ChainInfo {
                        data_ptr: chain_data,
                        first_len,
                        elem_ty: chain_elem_ty,
                    }));
                }
                IterStep::FlatMap(c) => {
                    // Pre-emit the flat_map closure
                    let (fn_ptr, env_ptr, ret_ty) = self.emit_pipeline_closure(c)?;
                    let inner_ty = if let Type::Array(ref inner) = c.return_type {
                        self.llvm_type(inner)
                    } else {
                        unreachable!("flat_map closure must return array")
                    };
                    flat_map_info = Some(FlatMapInfo {
                        closure_info: Some(ClosureInfo {
                            fn_ptr,
                            env_ptr,
                            ret_ty,
                        }),
                        inner_elem_ty: inner_ty,
                        step_index: closure_infos.len(),
                    });
                    current_elem_ty = if let Type::Array(ref inner) = c.return_type {
                        self.llvm_type(inner)
                    } else {
                        unreachable!()
                    };
                    // Push placeholders for all info vectors
                    closure_infos.push(None);
                    zip_infos.push(None);
                    take_skip_ptrs.push(None);
                    enumerate_ptrs.push(None);
                    chain_infos.push(None);
                }
                IterStep::Flatten => {
                    // For flatten, the current_elem_ty is an array (fat ptr).
                    // We need to figure out the inner element type from the AST types.
                    // current_elem_ty is "{ ptr, i64, i64 }" for arrays. The inner
                    // element type comes from the source's AST type.
                    // We'll compute this from the source and steps so far.
                    let inner_ty = self.infer_flatten_inner_type(source, steps, &closure_infos);
                    flat_map_info = Some(FlatMapInfo {
                        closure_info: None,
                        inner_elem_ty: inner_ty.clone(),
                        step_index: closure_infos.len(),
                    });
                    current_elem_ty = inner_ty;
                    closure_infos.push(None);
                    zip_infos.push(None);
                    take_skip_ptrs.push(None);
                    enumerate_ptrs.push(None);
                    chain_infos.push(None);
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

        // When chain is present and source is a range, use simple idx < total_len
        // condition instead of range-style overflow-protected condition. The total_len
        // is already pre-computed so overflow isn't an issue.
        let has_chain = chain_infos.iter().any(|c| c.is_some());
        let range_end = if has_chain { None } else { range_end };

        Ok((
            data_ptr,
            len_val,
            src_elem_ty,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            range_end,
            chain_infos,
            map_iter_info,
            flat_map_info,
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

    /// Infer the inner element type for flatten() by walking source + steps.
    pub(crate) fn infer_flatten_inner_type(
        &self,
        source: &Expr,
        steps: &[IterStep<'_>],
        _closure_infos: &[Option<ClosureInfo>],
    ) -> String {
        // The element type before flatten should be an array.
        // Walk steps to find the last map before flatten.
        // Start with source elem type.
        let mut ast_ty: Option<Type> = None;

        // Determine base element type from source — handle various expression forms
        match &source.kind {
            ExprKind::Ident(ref name) => {
                if let Some(Type::Array(ref inner)) = self.var_ast_types.get(name) {
                    ast_ty = Some(*inner.clone());
                }
            }
            ExprKind::Call(callee, _) => {
                // Function call: look up return type
                if let ExprKind::Ident(fn_name) = &callee.kind {
                    if let Some(Type::Array(ref inner)) = self.fn_ret_types.get(fn_name) {
                        ast_ty = Some(*inner.clone());
                    }
                }
            }
            ExprKind::FieldAccess(recv, field) => {
                // Struct field access: look up field type
                if let Ok(struct_name) = self.expr_struct_name(recv) {
                    if let Ok((_, Type::Array(ref inner))) =
                        self.struct_field_index(&struct_name, field)
                    {
                        ast_ty = Some(*inner.clone());
                    }
                }
            }
            _ => {}
        }

        // Walk steps and track type changes
        for step in steps {
            match step {
                IterStep::Map(c) => {
                    ast_ty = Some(c.return_type.clone());
                }
                IterStep::Flatten => {
                    // Unwrap one array layer
                    if let Some(Type::Array(ref inner)) = ast_ty {
                        return self.llvm_type(inner);
                    }
                    break;
                }
                _ => {} // Filter, Take, Skip, etc. don't change the type
            }
        }

        // Fallback: if we couldn't determine, use i64
        if let Some(Type::Array(ref inner)) = ast_ty {
            return self.llvm_type(inner);
        }
        "i64".to_string()
    }

    /// Emit the loop condition for a pipeline.
    /// For arrays: `idx < len`.  For ranges: `(start + idx) < end` or `<= end`,
    /// avoiding overflow from pre-computing the total length.
    /// For unbounded ranges: unconditional branch to step (no bounds check).
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit_pipeline_loop_cond(
        &mut self,
        idx_ptr: &str,
        data_ptr: &str,
        len_val: &str,
        range_end: &Option<(String, bool)>,
        is_unbounded: bool,
        step_label: &str,
        end_label: &str,
    ) -> String {
        let cur_idx = self.emit_load("i64", idx_ptr);
        if is_unbounded {
            // Unbounded range: always continue (take/take_while/find/etc. will break)
            self.emit_line(&format!("br label %{}", step_label));
            return cur_idx;
        }
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
            chain_infos,
            map_iter_info,
            flat_map_info,
        ) = self.emit_pipeline_preamble(
            pipeline.source,
            &pipeline.steps,
            pipeline.is_range_source,
        )?;

        // 2. Determine final output type and build context
        let idx_ptr = self.emit_alloca_store("i64", "0");
        let final_elem_ty = if let Some(ref fmi) = flat_map_info {
            let post_steps = &pipeline.steps[fmi.step_index + 1..];
            self.pipeline_final_elem_type(&fmi.inner_elem_ty, post_steps)
        } else {
            self.pipeline_final_elem_type(&src_elem_ty, &pipeline.steps)
        };
        let ctx = PipelineContext {
            data_ptr,
            len_val,
            idx_ptr,
            src_elem_ty: src_elem_ty.clone(),
            final_elem_ty: final_elem_ty.clone(),
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            is_range_source: pipeline.is_range_source,
            is_unbounded_range: pipeline.is_unbounded_range,
            range_end,
            chain_infos,
            map_iter_info,
            flat_map_info,
        };

        // Check for flat_map/flatten — delegate to nested loop emitter
        if ctx.flat_map_info.is_some() {
            return self.emit_for_flat_pipeline(var_name, &ctx, &pipeline.steps, body);
        }

        // 3. Alloca for loop variable
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
            &ctx.idx_ptr,
            &ctx.data_ptr,
            &ctx.len_val,
            &ctx.range_end,
            ctx.is_unbounded_range,
            &step_label,
            &end_label,
        );

        // 5. Step block: load element, increment index, apply pipeline steps
        self.emit_label(&step_label);
        self.block_terminated = false;
        let (current_val, _) = self.emit_pipeline_steps(
            &cur_idx,
            &cond_label,
            &body_label,
            &end_label,
            &ctx,
            &pipeline.steps,
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

    /// Emit a for-loop over a flat_map/flatten pipeline using nested loops.
    /// Outer loop iterates source elements, applies pre-flat_map steps.
    /// Inner loop iterates flattened elements, applies post-flat_map steps + user body.
    fn emit_for_flat_pipeline(
        &mut self,
        var_name: &str,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
        body: &Block,
    ) -> Result<(), CodegenError> {
        let fmi = ctx.flat_map_info.as_ref().unwrap();
        let flat_idx = fmi.step_index;
        let inner_elem_ty = fmi.inner_elem_ty.clone();
        let pre_steps = &steps[..flat_idx];
        let post_steps = &steps[flat_idx + 1..];

        // Alloca for the loop variable (final output type)
        let var_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", var_ptr, ctx.final_elem_ty));

        // Outer loop labels
        let outer_cond = self.fresh_label("flat.outer.cond");
        let outer_step = self.fresh_label("flat.outer.step");
        let outer_body = self.fresh_label("flat.outer.body");
        let outer_end = self.fresh_label("flat.outer.end");

        self.emit_line(&format!("br label %{}", outer_cond));

        // Outer condition
        self.emit_label(&outer_cond);
        self.block_terminated = false;
        let cur_idx = self.emit_pipeline_loop_cond(
            &ctx.idx_ptr,
            &ctx.data_ptr,
            &ctx.len_val,
            &ctx.range_end,
            ctx.is_unbounded_range,
            &outer_step,
            &outer_end,
        );

        // Outer step: load element, apply pre-steps
        self.emit_label(&outer_step);
        self.block_terminated = false;
        let (outer_val, _outer_ty) = self.emit_pipeline_steps(
            &cur_idx,
            &outer_cond,
            &outer_body,
            &outer_end,
            ctx,
            pre_steps,
        )?;

        // Outer body: call flat_map closure or use element as inner array
        self.emit_label(&outer_body);
        self.block_terminated = false;

        let (inner_data_ptr, inner_len) = if let Some(ref ci) = fmi.closure_info {
            // flat_map: call closure to get inner array
            let pre_elem_ty = if pre_steps.is_empty() {
                ctx.src_elem_ty.clone()
            } else {
                self.pipeline_final_elem_type(&ctx.src_elem_ty, pre_steps)
            };
            let inner_arr = self.fresh_temp();
            self.emit_line(&format!(
                "{} = call ptr {}(ptr {}, {} {})",
                inner_arr, ci.fn_ptr, ci.env_ptr, pre_elem_ty, outer_val
            ));
            self.emit_fat_ptr_load(&inner_arr)
        } else {
            // flatten: outer_val IS the inner array (fat ptr alloca)
            self.emit_fat_ptr_load(&outer_val)
        };

        // Inner loop
        let inner_idx_ptr = self.emit_alloca_store("i64", "0");
        let inner_cond = self.fresh_label("flat.inner.cond");
        let inner_step = self.fresh_label("flat.inner.step");
        let inner_body = self.fresh_label("flat.inner.body");
        let inner_end = self.fresh_label("flat.inner.end");

        self.emit_line(&format!("br label %{}", inner_cond));

        // Inner condition: inner_idx < inner_len
        self.emit_label(&inner_cond);
        self.block_terminated = false;
        let inner_cur = self.emit_load("i64", &inner_idx_ptr);
        let inner_cmp = self.fresh_temp();
        self.emit_line(&format!(
            "{} = icmp slt i64 {}, {}",
            inner_cmp, inner_cur, inner_len
        ));
        self.emit_cond_branch(&inner_cmp, &inner_step, &inner_end);

        // Inner step: load inner element, apply post-steps
        self.emit_label(&inner_step);
        self.block_terminated = false;
        let inner_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {}, ptr {}, i64 {}",
            inner_gep, inner_elem_ty, inner_data_ptr, inner_cur
        ));
        let inner_val = self.emit_load(&inner_elem_ty, &inner_gep);
        // Increment inner index
        let inner_next = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", inner_next, inner_cur));
        self.emit_line(&format!("store i64 {}, ptr {}", inner_next, inner_idx_ptr));

        // Apply post-steps (filter, take, etc.) — for now, branch directly to body
        // For post-steps that need processing, we'd need to apply them here.
        // Most common usage is flat_map().collect() or flat_map().filter().collect()
        // For simplicity, handle the no-post-steps case and filter case.
        let mut final_val = inner_val.clone();
        let mut final_ty = inner_elem_ty.clone();
        if post_steps.is_empty() {
            self.emit_line(&format!("br label %{}", inner_body));
        } else {
            // Apply post-steps inline
            for (i, step) in post_steps.iter().enumerate() {
                let is_last = i + 1 == post_steps.len();
                let next_label = if is_last {
                    inner_body.clone()
                } else {
                    self.fresh_label("flat.post")
                };
                match step {
                    IterStep::Filter(_) => {
                        let ci = ctx.closure_infos[flat_idx + 1 + i].as_ref().unwrap();
                        let result = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call i1 {}(ptr {}, {} {})",
                            result, ci.fn_ptr, ci.env_ptr, final_ty, final_val
                        ));
                        self.emit_cond_branch(&result, &next_label, &inner_cond);
                    }
                    IterStep::Map(_) => {
                        let ci = ctx.closure_infos[flat_idx + 1 + i].as_ref().unwrap();
                        let result = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call {} {}(ptr {}, {} {})",
                            result, ci.ret_ty, ci.fn_ptr, ci.env_ptr, final_ty, final_val
                        ));
                        final_val = result;
                        final_ty = ci.ret_ty.clone();
                        if is_last {
                            self.emit_line(&format!("br label %{}", next_label));
                        }
                    }
                    IterStep::Take(_) => {
                        let take_ptr = ctx.take_skip_ptrs[flat_idx + 1 + i].as_ref().unwrap();
                        let remaining = self.emit_load("i64", take_ptr);
                        let done = self.fresh_temp();
                        self.emit_line(&format!("{} = icmp sle i64 {}, 0", done, remaining));
                        let take_cont = self.fresh_label("flat.take.cont");
                        self.emit_cond_branch(&done, &outer_end, &take_cont);
                        self.emit_label(&take_cont);
                        self.block_terminated = false;
                        let new_remaining = self.fresh_temp();
                        self.emit_line(&format!("{} = sub i64 {}, 1", new_remaining, remaining));
                        self.emit_line(&format!("store i64 {}, ptr {}", new_remaining, take_ptr));
                        if is_last {
                            self.emit_line(&format!("br label %{}", next_label));
                        }
                    }
                    IterStep::TakeWhile(_) => {
                        let ci = ctx.closure_infos[flat_idx + 1 + i].as_ref().unwrap();
                        let result = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call i1 {}(ptr {}, {} {})",
                            result, ci.fn_ptr, ci.env_ptr, final_ty, final_val
                        ));
                        self.emit_cond_branch(&result, &next_label, &outer_end);
                    }
                    _ => {
                        return Err(CodegenError {
                            message: format!(
                                "unsupported combinator after flat_map/flatten: {:?}",
                                step.name()
                            ),
                        });
                    }
                }
                if !is_last && !matches!(step, IterStep::Filter(_) | IterStep::TakeWhile(_)) {
                    self.emit_label(&next_label);
                    self.block_terminated = false;
                }
            }
        }

        // Inner body: store value, emit user body
        self.emit_label(&inner_body);
        self.block_terminated = false;

        self.push_scope();
        self.emit_line(&format!(
            "store {} {}, ptr {}",
            ctx.final_elem_ty, final_val, var_ptr
        ));
        self.define_var(var_name, &var_ptr, &ctx.final_elem_ty);

        // continue → inner_cond (next inner element), break → outer_end
        self.loop_labels
            .push((inner_cond.clone(), outer_end.clone()));
        self.emit_block(body)?;
        self.loop_labels.pop();

        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", inner_cond));
        }
        self.pop_scope();

        // Inner end: go to next outer element
        self.emit_label(&inner_end);
        self.block_terminated = false;
        self.emit_line(&format!("br label %{}", outer_cond));

        // Outer end
        self.emit_label(&outer_end);
        self.block_terminated = false;
        Ok(())
    }

    /// Emit a terminated pipeline (collect, sum, etc.) over a flat_map/flatten pipeline.
    fn emit_flat_terminated_pipeline(
        &mut self,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
        terminator: &PipelineTerminator<'_>,
        term_closure_info: Option<&ClosureInfo>,
    ) -> Result<String, CodegenError> {
        let fmi = ctx.flat_map_info.as_ref().unwrap();
        let flat_idx = fmi.step_index;
        let inner_elem_ty = fmi.inner_elem_ty.clone();
        let pre_steps = &steps[..flat_idx];
        let post_steps = &steps[flat_idx + 1..];

        // Set up terminator accumulator based on terminator type
        let mut collect_out_data_ptr = String::new();
        let mut collect_out_idx_ptr = String::new();
        let mut collect_safe_len = String::new();
        let (result_ptr, result_ty) = match terminator {
            PipelineTerminator::Collect => {
                // Allocate output array with initial capacity
                let elem_size_val = self.llvm_type_size(&ctx.final_elem_ty);
                let init_cap_val: usize = 16;
                let alloc_bytes = elem_size_val * init_cap_val;
                let out_data = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = call ptr @malloc(i64 {})",
                    out_data, alloc_bytes
                ));
                let out_idx = self.emit_alloca_store("i64", "0");
                // Store capacity in a local for grow checks
                let cap_ptr = self.emit_alloca_store("i64", &format!("{}", init_cap_val));
                let data_ptr_store = self.emit_alloca_store("ptr", &out_data);
                collect_out_data_ptr = data_ptr_store.clone();
                collect_out_idx_ptr = out_idx.clone();
                collect_safe_len = cap_ptr;
                (data_ptr_store, "ptr".to_string())
            }
            PipelineTerminator::Sum => {
                let ptr = self.emit_alloca_store(&ctx.final_elem_ty, "0");
                (ptr, ctx.final_elem_ty.clone())
            }
            PipelineTerminator::Count => {
                let ptr = self.emit_alloca_store("i64", "0");
                (ptr, "i64".to_string())
            }
            PipelineTerminator::Find(_) => {
                // Option: allocate and set to None (tag=1)
                let enum_name = format!("Option__{}", llvm_to_semantic_name(&ctx.final_elem_ty));
                let enum_ty = format!("%{}", enum_name);
                let result_ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca {}", result_ptr, enum_ty));
                // Initialize to None (tag=1)
                let tag_ptr = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {}, ptr {}, i32 0, i32 0",
                    tag_ptr, enum_ty, result_ptr
                ));
                self.emit_line(&format!("store i32 1, ptr {}", tag_ptr));
                (result_ptr, enum_ty)
            }
            PipelineTerminator::Any(_) => {
                let ptr = self.emit_alloca_store("i1", "0");
                (ptr, "i1".to_string())
            }
            PipelineTerminator::All(_) => {
                let ptr = self.emit_alloca_store("i1", "1");
                (ptr, "i1".to_string())
            }
            _ => {
                // For Fold, Reduce, Position — fall back
                return Err(CodegenError {
                    message: "flat_map/flatten with this terminator is not yet supported"
                        .to_string(),
                });
            }
        };

        // Outer loop
        let outer_cond = self.fresh_label("flat.t.outer.cond");
        let outer_step = self.fresh_label("flat.t.outer.step");
        let outer_body = self.fresh_label("flat.t.outer.body");
        let outer_end = self.fresh_label("flat.t.outer.end");

        self.emit_line(&format!("br label %{}", outer_cond));

        self.emit_label(&outer_cond);
        self.block_terminated = false;
        let cur_idx = self.emit_pipeline_loop_cond(
            &ctx.idx_ptr,
            &ctx.data_ptr,
            &ctx.len_val,
            &ctx.range_end,
            ctx.is_unbounded_range,
            &outer_step,
            &outer_end,
        );

        // Outer step: load element, apply pre-steps
        self.emit_label(&outer_step);
        self.block_terminated = false;
        let (outer_val, _) = self.emit_pipeline_steps(
            &cur_idx,
            &outer_cond,
            &outer_body,
            &outer_end,
            ctx,
            pre_steps,
        )?;

        // Outer body: get inner array
        self.emit_label(&outer_body);
        self.block_terminated = false;

        let (inner_data_ptr, inner_len) = if let Some(ref ci) = fmi.closure_info {
            let pre_elem_ty = if pre_steps.is_empty() {
                ctx.src_elem_ty.clone()
            } else {
                self.pipeline_final_elem_type(&ctx.src_elem_ty, pre_steps)
            };
            let inner_arr = self.fresh_temp();
            self.emit_line(&format!(
                "{} = call ptr {}(ptr {}, {} {})",
                inner_arr, ci.fn_ptr, ci.env_ptr, pre_elem_ty, outer_val
            ));
            self.emit_fat_ptr_load(&inner_arr)
        } else {
            self.emit_fat_ptr_load(&outer_val)
        };

        // Inner loop
        let inner_idx_ptr = self.emit_alloca_store("i64", "0");
        let inner_cond = self.fresh_label("flat.t.inner.cond");
        let inner_step = self.fresh_label("flat.t.inner.step");
        let inner_body_label = self.fresh_label("flat.t.inner.body");
        let inner_end = self.fresh_label("flat.t.inner.end");

        self.emit_line(&format!("br label %{}", inner_cond));

        self.emit_label(&inner_cond);
        self.block_terminated = false;
        let inner_cur = self.emit_load("i64", &inner_idx_ptr);
        let inner_cmp = self.fresh_temp();
        self.emit_line(&format!(
            "{} = icmp slt i64 {}, {}",
            inner_cmp, inner_cur, inner_len
        ));
        self.emit_cond_branch(&inner_cmp, &inner_step, &inner_end);

        // Inner step: load element
        self.emit_label(&inner_step);
        self.block_terminated = false;
        let inner_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {}, ptr {}, i64 {}",
            inner_gep, inner_elem_ty, inner_data_ptr, inner_cur
        ));
        let inner_val = self.emit_load(&inner_elem_ty, &inner_gep);
        let inner_next = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", inner_next, inner_cur));
        self.emit_line(&format!("store i64 {}, ptr {}", inner_next, inner_idx_ptr));

        // Apply post-steps
        let mut final_val = inner_val.clone();
        let mut final_ty = inner_elem_ty.clone();
        if post_steps.is_empty() {
            self.emit_line(&format!("br label %{}", inner_body_label));
        } else {
            for (i, step) in post_steps.iter().enumerate() {
                let is_last = i + 1 == post_steps.len();
                let next_label = if is_last {
                    inner_body_label.clone()
                } else {
                    self.fresh_label("flat.t.post")
                };
                match step {
                    IterStep::Filter(_) => {
                        let ci = ctx.closure_infos[flat_idx + 1 + i].as_ref().unwrap();
                        let result = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call i1 {}(ptr {}, {} {})",
                            result, ci.fn_ptr, ci.env_ptr, final_ty, final_val
                        ));
                        self.emit_cond_branch(&result, &next_label, &inner_cond);
                    }
                    IterStep::Map(_) => {
                        let ci = ctx.closure_infos[flat_idx + 1 + i].as_ref().unwrap();
                        let result = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call {} {}(ptr {}, {} {})",
                            result, ci.ret_ty, ci.fn_ptr, ci.env_ptr, final_ty, final_val
                        ));
                        final_val = result;
                        final_ty = ci.ret_ty.clone();
                        if is_last {
                            self.emit_line(&format!("br label %{}", next_label));
                        }
                    }
                    IterStep::Take(_) => {
                        let take_ptr = ctx.take_skip_ptrs[flat_idx + 1 + i].as_ref().unwrap();
                        let remaining = self.emit_load("i64", take_ptr);
                        let done = self.fresh_temp();
                        self.emit_line(&format!("{} = icmp sle i64 {}, 0", done, remaining));
                        let take_cont = self.fresh_label("flat.t.take.cont");
                        self.emit_cond_branch(&done, &outer_end, &take_cont);
                        self.emit_label(&take_cont);
                        self.block_terminated = false;
                        let new_remaining = self.fresh_temp();
                        self.emit_line(&format!("{} = sub i64 {}, 1", new_remaining, remaining));
                        self.emit_line(&format!("store i64 {}, ptr {}", new_remaining, take_ptr));
                        if is_last {
                            self.emit_line(&format!("br label %{}", next_label));
                        }
                    }
                    IterStep::TakeWhile(_) => {
                        let ci = ctx.closure_infos[flat_idx + 1 + i].as_ref().unwrap();
                        let result = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call i1 {}(ptr {}, {} {})",
                            result, ci.fn_ptr, ci.env_ptr, final_ty, final_val
                        ));
                        self.emit_cond_branch(&result, &next_label, &outer_end);
                    }
                    _ => {
                        return Err(CodegenError {
                            message: format!(
                                "unsupported combinator after flat_map/flatten: {:?}",
                                step.name()
                            ),
                        });
                    }
                }
                if !is_last && !matches!(step, IterStep::Filter(_) | IterStep::TakeWhile(_)) {
                    self.emit_label(&next_label);
                    self.block_terminated = false;
                }
            }
        }

        // Inner body: apply terminator accumulation
        self.emit_label(&inner_body_label);
        self.block_terminated = false;

        match terminator {
            PipelineTerminator::Collect => {
                // Push element to result array with grow
                let cur_len = self.emit_load("i64", &collect_out_idx_ptr);
                let cur_cap = self.emit_load("i64", &collect_safe_len);
                let cur_data = self.emit_load("ptr", &collect_out_data_ptr);
                // Check if we need to grow
                let needs_grow = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = icmp sge i64 {}, {}",
                    needs_grow, cur_len, cur_cap
                ));
                let grow_label = self.fresh_label("flat.grow");
                let store_label = self.fresh_label("flat.store");
                self.emit_cond_branch(&needs_grow, &grow_label, &store_label);

                // Grow: double capacity and realloc
                self.emit_label(&grow_label);
                self.block_terminated = false;
                let new_cap = self.fresh_temp();
                self.emit_line(&format!("{} = mul i64 {}, 2", new_cap, cur_cap));
                let elem_size_val = self.llvm_type_size(&ctx.final_elem_ty);
                let new_bytes = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = mul i64 {}, {}",
                    new_bytes, new_cap, elem_size_val
                ));
                let new_data = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = call ptr @realloc(ptr {}, i64 {})",
                    new_data, cur_data, new_bytes
                ));
                self.emit_line(&format!(
                    "store ptr {}, ptr {}",
                    new_data, collect_out_data_ptr
                ));
                self.emit_line(&format!("store i64 {}, ptr {}", new_cap, collect_safe_len));
                self.emit_line(&format!("br label %{}", store_label));

                // Store element
                self.emit_label(&store_label);
                self.block_terminated = false;
                let data_phi = self.emit_load("ptr", &collect_out_data_ptr);
                let out_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {}, ptr {}, i64 {}",
                    out_gep, ctx.final_elem_ty, data_phi, cur_len
                ));
                self.emit_line(&format!(
                    "store {} {}, ptr {}",
                    ctx.final_elem_ty, final_val, out_gep
                ));
                let next_len = self.fresh_temp();
                self.emit_line(&format!("{} = add i64 {}, 1", next_len, cur_len));
                self.emit_line(&format!(
                    "store i64 {}, ptr {}",
                    next_len, collect_out_idx_ptr
                ));
            }
            PipelineTerminator::Sum => {
                let cur = self.emit_load(&ctx.final_elem_ty, &result_ptr);
                let new_val = self.fresh_temp();
                if ctx.final_elem_ty == "double" {
                    self.emit_line(&format!("{} = fadd double {}, {}", new_val, cur, final_val));
                } else {
                    self.emit_line(&format!(
                        "{} = add {} {}, {}",
                        new_val, ctx.final_elem_ty, cur, final_val
                    ));
                }
                self.emit_line(&format!(
                    "store {} {}, ptr {}",
                    ctx.final_elem_ty, new_val, result_ptr
                ));
            }
            PipelineTerminator::Count => {
                let cur = self.emit_load("i64", &result_ptr);
                let new_val = self.fresh_temp();
                self.emit_line(&format!("{} = add i64 {}, 1", new_val, cur));
                self.emit_line(&format!("store i64 {}, ptr {}", new_val, result_ptr));
            }
            PipelineTerminator::Find(_) => {
                let tci = term_closure_info.unwrap();
                let pred_result = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = call i1 {}(ptr {}, {} {})",
                    pred_result, tci.fn_ptr, tci.env_ptr, final_ty, final_val
                ));
                let found_label = self.fresh_label("flat.t.found");
                let cont_label = self.fresh_label("flat.t.cont");
                self.emit_cond_branch(&pred_result, &found_label, &cont_label);

                self.emit_label(&found_label);
                self.block_terminated = false;
                // Store Some(val)
                let tag_ptr = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {}, ptr {}, i32 0, i32 0",
                    tag_ptr, result_ty, result_ptr
                ));
                self.emit_line(&format!("store i32 0, ptr {}", tag_ptr));
                let data_field = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {}, ptr {}, i32 0, i32 1",
                    data_field, result_ty, result_ptr
                ));
                self.emit_line(&format!(
                    "store {} {}, ptr {}",
                    final_ty, final_val, data_field
                ));
                self.emit_line(&format!("br label %{}", outer_end));

                self.emit_label(&cont_label);
                self.block_terminated = false;
            }
            PipelineTerminator::Any(_) => {
                let tci = term_closure_info.unwrap();
                let pred_result = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = call i1 {}(ptr {}, {} {})",
                    pred_result, tci.fn_ptr, tci.env_ptr, final_ty, final_val
                ));
                let found_label = self.fresh_label("flat.t.any.found");
                let cont_label = self.fresh_label("flat.t.any.cont");
                self.emit_cond_branch(&pred_result, &found_label, &cont_label);

                self.emit_label(&found_label);
                self.block_terminated = false;
                self.emit_line(&format!("store i1 1, ptr {}", result_ptr));
                self.emit_line(&format!("br label %{}", outer_end));

                self.emit_label(&cont_label);
                self.block_terminated = false;
            }
            PipelineTerminator::All(_) => {
                let tci = term_closure_info.unwrap();
                let pred_result = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = call i1 {}(ptr {}, {} {})",
                    pred_result, tci.fn_ptr, tci.env_ptr, final_ty, final_val
                ));
                let fail_label = self.fresh_label("flat.t.all.fail");
                let cont_label = self.fresh_label("flat.t.all.cont");
                self.emit_cond_branch(&pred_result, &cont_label, &fail_label);

                self.emit_label(&fail_label);
                self.block_terminated = false;
                self.emit_line(&format!("store i1 0, ptr {}", result_ptr));
                self.emit_line(&format!("br label %{}", outer_end));

                self.emit_label(&cont_label);
                self.block_terminated = false;
            }
            _ => {}
        }

        // Continue inner loop
        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", inner_cond));
        }

        // Inner end → outer cond
        self.emit_label(&inner_end);
        self.block_terminated = false;
        self.emit_line(&format!("br label %{}", outer_cond));

        // Outer end: return result
        self.emit_label(&outer_end);
        self.block_terminated = false;

        // Return the result value
        match terminator {
            PipelineTerminator::Collect => {
                let final_data = self.emit_load("ptr", &collect_out_data_ptr);
                let final_len = self.emit_load("i64", &collect_out_idx_ptr);
                let final_cap = self.emit_load("i64", &collect_safe_len);
                let fat = self.fresh_temp();
                self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat));
                self.emit_fat_ptr_init(&fat, &final_data, &final_len, &final_cap);
                Ok(fat)
            }
            PipelineTerminator::Sum => {
                let result = self.emit_load(&ctx.final_elem_ty, &result_ptr);
                Ok(result)
            }
            PipelineTerminator::Count => {
                let result = self.emit_load("i64", &result_ptr);
                Ok(result)
            }
            PipelineTerminator::Find(_) => {
                let result = self.emit_load(&result_ty, &result_ptr);
                Ok(result)
            }
            PipelineTerminator::Any(_) | PipelineTerminator::All(_) => {
                let result = self.emit_load("i1", &result_ptr);
                Ok(result)
            }
            _ => Err(CodegenError {
                message: "unsupported flat_map terminator".to_string(),
            }),
        }
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
            chain_infos,
            map_iter_info,
            flat_map_info,
        ) = self.emit_pipeline_preamble(
            pipeline.source,
            &pipeline.steps,
            pipeline.is_range_source,
        )?;

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
            PipelineTerminator::Collect | PipelineTerminator::Sum | PipelineTerminator::Count => {
                None
            }
            PipelineTerminator::Position(c) => {
                let (fn_ptr, env_ptr, ret_ty) = self.emit_pipeline_closure(c)?;
                Some(ClosureInfo {
                    fn_ptr,
                    env_ptr,
                    ret_ty,
                })
            }
        };

        // Determine element type after all pipeline steps (before terminator)
        let final_elem_ty = if let Some(ref fmi) = flat_map_info {
            // Start from the flattened inner type, then walk post-flat steps
            let post_steps = &pipeline.steps[fmi.step_index + 1..];
            self.pipeline_final_elem_type(&fmi.inner_elem_ty, post_steps)
        } else {
            self.pipeline_final_elem_type(&src_elem_ty, &pipeline.steps)
        };

        // 3. Alloca for index counter and build context
        let idx_ptr = self.emit_alloca_store("i64", "0");
        let ctx = PipelineContext {
            data_ptr,
            len_val,
            idx_ptr,
            src_elem_ty,
            final_elem_ty,
            closure_infos,
            zip_infos,
            take_skip_ptrs,
            enumerate_ptrs,
            is_range_source: pipeline.is_range_source,
            is_unbounded_range: pipeline.is_unbounded_range,
            range_end,
            chain_infos,
            map_iter_info,
            flat_map_info,
        };

        // 4. Check for flat_map/flatten — delegate to nested-loop terminator
        if ctx.flat_map_info.is_some() {
            return self.emit_flat_terminated_pipeline(
                &ctx,
                &pipeline.steps,
                &pipeline.terminator,
                term_closure_info.as_ref(),
            );
        }

        // 4b. Dispatch to specific terminator emitter
        match &pipeline.terminator {
            PipelineTerminator::Collect => self.emit_terminator_collect(&ctx, &pipeline.steps),
            PipelineTerminator::Fold(init_expr, _) => {
                let init_val = self.emit_expr(init_expr)?;
                let tci = term_closure_info.unwrap();
                self.emit_terminator_fold(
                    &ctx,
                    &pipeline.steps,
                    &init_val,
                    &tci.fn_ptr,
                    &tci.env_ptr,
                    &tci.ret_ty,
                )
            }
            PipelineTerminator::Any(_) => {
                let tci = term_closure_info.unwrap();
                self.emit_terminator_any_all(true, &ctx, &pipeline.steps, &tci.fn_ptr, &tci.env_ptr)
            }
            PipelineTerminator::All(_) => {
                let tci = term_closure_info.unwrap();
                self.emit_terminator_any_all(
                    false,
                    &ctx,
                    &pipeline.steps,
                    &tci.fn_ptr,
                    &tci.env_ptr,
                )
            }
            PipelineTerminator::Find(_) => {
                let tci = term_closure_info.unwrap();
                self.emit_terminator_find(&ctx, &pipeline.steps, &tci.fn_ptr, &tci.env_ptr)
            }
            PipelineTerminator::Reduce(_) => {
                let tci = term_closure_info.unwrap();
                self.emit_terminator_reduce(
                    &ctx,
                    &pipeline.steps,
                    &tci.fn_ptr,
                    &tci.env_ptr,
                    &tci.ret_ty,
                )
            }
            PipelineTerminator::Sum => self.emit_terminator_sum(&ctx, &pipeline.steps),
            PipelineTerminator::Count => self.emit_terminator_count(&ctx, &pipeline.steps),
            PipelineTerminator::Position(_) => {
                let tci = term_closure_info.unwrap();
                self.emit_terminator_position(&ctx, &pipeline.steps, &tci.fn_ptr, &tci.env_ptr)
            }
        }
    }

    /// Emit the pipeline step chain (shared between for-loop and terminators).
    /// Emits the step block body: loads element, increments index, applies pipeline.
    /// Returns (current_val, current_ty) after all steps are applied.
    /// `cond_label` is the target for filter-skip, `body_label` is the final target,
    /// `end_label` is for take/zip exhaustion.
    pub(crate) fn emit_pipeline_steps(
        &mut self,
        cur_idx: &str,
        cond_label: &str,
        body_label: &str,
        end_label: &str,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
    ) -> Result<(String, String), CodegenError> {
        // For Map.iter(), the data_ptr points to keys array; use key type for GEP/load
        let load_elem_ty = if let Some(ref mii) = ctx.map_iter_info {
            &mii.key_elem_ty
        } else {
            &ctx.src_elem_ty
        };

        // Check if first step is Rev — compute reversed index
        let has_rev = !steps.is_empty() && matches!(steps[0], IterStep::Rev);
        let effective_idx = if has_rev {
            let len_minus_1 = self.fresh_temp();
            self.emit_line(&format!("{} = sub i64 {}, 1", len_minus_1, ctx.len_val));
            let rev_idx = self.fresh_temp();
            self.emit_line(&format!(
                "{} = sub i64 {}, {}",
                rev_idx, len_minus_1, cur_idx
            ));
            rev_idx
        } else {
            cur_idx.to_string()
        };

        // Load element at current index — with chain conditional if chain is at position 0
        // When rev is active, effective_idx is the reversed index for element access.
        let load_idx = &effective_idx;
        let mut current_val = if !ctx.chain_infos.is_empty() {
            if let Some(ref ci) = ctx.chain_infos[0] {
                // Chain at position 0: conditionally load from first or second source
                let in_first = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = icmp slt i64 {}, {}",
                    in_first, load_idx, ci.first_len
                ));
                let chain_first_label = self.fresh_label("chain.first");
                let chain_second_label = self.fresh_label("chain.second");
                let chain_merge_label = self.fresh_label("chain.merge");
                self.emit_cond_branch(&in_first, &chain_first_label, &chain_second_label);

                // First source
                self.emit_label(&chain_first_label);
                self.block_terminated = false;
                let first_val = if ctx.is_range_source {
                    let val = self.fresh_temp();
                    self.emit_line(&format!("{} = add i64 {}, {}", val, ctx.data_ptr, load_idx));
                    val
                } else {
                    let elem_gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr {}, ptr {}, i64 {}",
                        elem_gep, ctx.src_elem_ty, ctx.data_ptr, load_idx
                    ));
                    self.emit_load(&ctx.src_elem_ty, &elem_gep)
                };
                self.emit_line(&format!("br label %{}", chain_merge_label));

                // Second source (chain array)
                self.emit_label(&chain_second_label);
                self.block_terminated = false;
                let chain_off = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = sub i64 {}, {}",
                    chain_off, load_idx, ci.first_len
                ));
                let chain_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {}, ptr {}, i64 {}",
                    chain_gep, ci.elem_ty, ci.data_ptr, chain_off
                ));
                let chain_val = self.emit_load(&ci.elem_ty, &chain_gep);
                self.emit_line(&format!("br label %{}", chain_merge_label));

                // Merge
                self.emit_label(&chain_merge_label);
                self.block_terminated = false;
                let merged = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = phi {} [{}, %{}], [{}, %{}]",
                    merged,
                    ctx.src_elem_ty,
                    first_val,
                    chain_first_label,
                    chain_val,
                    chain_second_label
                ));
                merged
            } else {
                // No chain at position 0, normal load
                if ctx.is_range_source {
                    let val = self.fresh_temp();
                    self.emit_line(&format!("{} = add i64 {}, {}", val, ctx.data_ptr, load_idx));
                    val
                } else {
                    let elem_gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr {}, ptr {}, i64 {}",
                        elem_gep, load_elem_ty, ctx.data_ptr, load_idx
                    ));
                    self.emit_load(load_elem_ty, &elem_gep)
                }
            }
        } else if ctx.is_range_source {
            // Range source: element = start + index
            let val = self.fresh_temp();
            self.emit_line(&format!("{} = add i64 {}, {}", val, ctx.data_ptr, load_idx));
            val
        } else {
            let elem_gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr {}, ptr {}, i64 {}",
                elem_gep, load_elem_ty, ctx.data_ptr, load_idx
            ));
            self.emit_load(load_elem_ty, &elem_gep)
        };

        // For chars() source, sext i8 → i32 so downstream closures see char type
        if ctx.src_elem_ty == "i8" {
            let char_val = self.fresh_temp();
            self.emit_line(&format!("{} = sext i8 {} to i32", char_val, current_val));
            current_val = char_val;
        }

        // For Map.iter(): current_val is the key; load value and construct (K, V) tuple
        if let Some(ref mii) = ctx.map_iter_info {
            let val_gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr {}, ptr {}, i64 {}",
                val_gep, mii.val_elem_ty, mii.val_data_ptr, load_idx
            ));
            let val_loaded = self.emit_load(&mii.val_elem_ty, &val_gep);
            let tuple_ptr = self.fresh_temp();
            self.emit_line(&format!("{} = alloca %{}", tuple_ptr, mii.tuple_name));
            self.emit_struct_field_store(
                &mii.tuple_name,
                &tuple_ptr,
                0,
                &mii.key_elem_ty,
                &current_val,
            );
            self.emit_struct_field_store(
                &mii.tuple_name,
                &tuple_ptr,
                1,
                &mii.val_elem_ty,
                &val_loaded,
            );
            let tuple_loaded = self.emit_load(&format!("%{}", mii.tuple_name), &tuple_ptr);
            current_val = tuple_loaded;
        }

        // Increment index (always, even if filtered out)
        let next_idx = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", next_idx, cur_idx));
        self.emit_line(&format!("store i64 {}, ptr {}", next_idx, ctx.idx_ptr));

        // Apply each pipeline step — use i32 as the effective type for chars() pipelines
        let mut current_ty = if ctx.src_elem_ty == "i8" {
            "i32".to_string()
        } else {
            ctx.src_elem_ty.to_string()
        };
        let step_count = steps.len();
        for (i, step) in steps.iter().enumerate() {
            let is_last = i + 1 == step_count;
            match step {
                IterStep::Filter(_) => {
                    let ci = ctx.closure_infos[i].as_ref().unwrap();
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
                    let ci = ctx.closure_infos[i].as_ref().unwrap();
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
                    let counter_ptr = ctx.enumerate_ptrs[i].as_ref().unwrap();
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
                    let zi = ctx.zip_infos[i].as_ref().unwrap();
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
                    // Load zip element: range → start + idx, array → GEP load
                    let zip_val = if zi.range_zip.is_some() {
                        let val = self.fresh_temp();
                        self.emit_line(&format!("{} = add i64 {}, {}", val, zi.data_ptr, zip_cur));
                        val
                    } else {
                        let zip_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {}, ptr {}, i64 {}",
                            zip_gep, zi.elem_ty, zi.data_ptr, zip_cur
                        ));
                        self.emit_load(&zi.elem_ty, &zip_gep)
                    };
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
                    let remaining_ptr = ctx.take_skip_ptrs[i].as_ref().unwrap();
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
                    let remaining_ptr = ctx.take_skip_ptrs[i].as_ref().unwrap();
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
                IterStep::Chain(_) => {
                    // No-op: element already selected by conditional load at the top.
                    if is_last {
                        self.emit_line(&format!("br label %{}", body_label));
                    }
                }
                IterStep::TakeWhile(_) => {
                    let ci = ctx.closure_infos[i].as_ref().unwrap();
                    let result = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = call i1 {}(ptr {}, {} {})",
                        result, ci.fn_ptr, ci.env_ptr, current_ty, current_val
                    ));
                    // Unlike filter (which skips to cond_label on false), take_while
                    // terminates the entire loop when the predicate is false.
                    let next_label = if !is_last {
                        self.fresh_label("for.pipe")
                    } else {
                        body_label.to_string()
                    };
                    self.emit_cond_branch(&result, &next_label, end_label);
                    if !is_last {
                        self.emit_label(&next_label);
                        self.block_terminated = false;
                    }
                }
                IterStep::Rev => {
                    // No-op in step processing: rev is handled at element load.
                    if is_last {
                        self.emit_line(&format!("br label %{}", body_label));
                    }
                }
                IterStep::FlatMap(_) | IterStep::Flatten => {
                    // No-op: flat_map/flatten are handled by the outer nested-loop wrapper.
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
    pub(crate) fn emit_pipeline_loop_header(
        &mut self,
        prefix: &str,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
    ) -> Result<PipelineLoopParts, CodegenError> {
        let cond_label = self.fresh_label(&format!("{prefix}.cond"));
        let step_label = self.fresh_label(&format!("{prefix}.step"));
        let body_label = self.fresh_label(&format!("{prefix}.body"));
        let end_label = self.fresh_label(&format!("{prefix}.end"));

        self.emit_line(&format!("br label %{cond_label}"));

        self.emit_label(&cond_label);
        self.block_terminated = false;
        let cur_idx = self.emit_pipeline_loop_cond(
            &ctx.idx_ptr,
            &ctx.data_ptr,
            &ctx.len_val,
            &ctx.range_end,
            ctx.is_unbounded_range,
            &step_label,
            &end_label,
        );

        self.emit_label(&step_label);
        self.block_terminated = false;
        let (current_val, _) =
            self.emit_pipeline_steps(&cur_idx, &cond_label, &body_label, &end_label, ctx, steps)?;

        self.emit_label(&body_label);
        self.block_terminated = false;

        Ok(PipelineLoopParts {
            cond_label,
            end_label,
            current_val,
        })
    }

    // ── Terminator: collect ─────────────────────────────────

    pub(crate) fn emit_terminator_collect(
        &mut self,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
    ) -> Result<String, CodegenError> {
        let elem_size = self.llvm_type_size(&ctx.final_elem_ty);

        // Cap len_val so that len_val * elem_size cannot overflow i64.
        let max_safe_elems = i64::MAX as usize / elem_size.max(1);
        let len_over = self.fresh_temp();
        self.emit_line(&format!(
            "{} = icmp sgt i64 {}, {}",
            len_over, ctx.len_val, max_safe_elems
        ));
        let safe_len = self.fresh_temp();
        self.emit_line(&format!(
            "{} = select i1 {}, i64 {}, i64 {}",
            safe_len, len_over, max_safe_elems, ctx.len_val
        ));

        // Allocate output array with initial capacity = safe_len
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

        // For unbounded ranges, use dynamic grow-on-push (cap is just a hint)
        let cap_ptr = if ctx.is_unbounded_range {
            Some(self.emit_alloca_store("i64", &safe_len))
        } else {
            None
        };
        let data_ptr_ptr = if ctx.is_unbounded_range {
            Some(self.emit_alloca_store("ptr", &out_data))
        } else {
            None
        };

        let lp = self.emit_pipeline_loop_header("col", ctx, steps)?;

        // Body: store element to output array
        let out_idx = self.emit_load("i64", &out_idx_ptr);

        if ctx.is_unbounded_range {
            // Dynamic grow path: check len >= cap, realloc if needed
            let cur_cap = self.emit_load("i64", cap_ptr.as_ref().unwrap());
            let cur_data = self.emit_load("ptr", data_ptr_ptr.as_ref().unwrap());
            let needs_grow = self.fresh_temp();
            self.emit_line(&format!(
                "{} = icmp sge i64 {}, {}",
                needs_grow, out_idx, cur_cap
            ));
            let grow_label = self.fresh_label("col.grow");
            let store_label = self.fresh_label("col.store");
            self.emit_cond_branch(&needs_grow, &grow_label, &store_label);

            self.emit_label(&grow_label);
            self.block_terminated = false;
            let new_cap = self.fresh_temp();
            self.emit_line(&format!("{} = mul i64 {}, 2", new_cap, cur_cap));
            let new_bytes = self.fresh_temp();
            self.emit_line(&format!(
                "{} = mul i64 {}, {}",
                new_bytes, new_cap, elem_size
            ));
            let new_data = self.fresh_temp();
            self.emit_line(&format!(
                "{} = call ptr @realloc(ptr {}, i64 {})",
                new_data, cur_data, new_bytes
            ));
            self.emit_line(&format!(
                "store ptr {}, ptr {}",
                new_data,
                data_ptr_ptr.as_ref().unwrap()
            ));
            self.emit_line(&format!(
                "store i64 {}, ptr {}",
                new_cap,
                cap_ptr.as_ref().unwrap()
            ));
            self.emit_line(&format!("br label %{}", store_label));

            self.emit_label(&store_label);
            self.block_terminated = false;
            let data_final = self.emit_load("ptr", data_ptr_ptr.as_ref().unwrap());
            let out_gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr {}, ptr {}, i64 {}",
                out_gep, ctx.final_elem_ty, data_final, out_idx
            ));
            self.emit_line(&format!(
                "store {} {}, ptr {}",
                ctx.final_elem_ty, lp.current_val, out_gep
            ));
        } else {
            // Fixed-size path: guard against exceeding safe_len
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
                out_gep, ctx.final_elem_ty, out_data, out_idx
            ));
            self.emit_line(&format!(
                "store {} {}, ptr {}",
                ctx.final_elem_ty, lp.current_val, out_gep
            ));
        }

        let next_out = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", next_out, out_idx));
        self.emit_line(&format!("store i64 {}, ptr {}", next_out, out_idx_ptr));
        self.emit_line(&format!("br label %{}", lp.cond_label));

        // End: build fat pointer
        self.emit_label(&lp.end_label);
        self.block_terminated = false;
        let final_len = self.emit_load("i64", &out_idx_ptr);
        let (final_data, final_cap) = if ctx.is_unbounded_range {
            let d = self.emit_load("ptr", data_ptr_ptr.as_ref().unwrap());
            let c = self.emit_load("i64", cap_ptr.as_ref().unwrap());
            (d, c)
        } else {
            (out_data.clone(), safe_len.clone())
        };
        let fat = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat));
        self.emit_fat_ptr_init(&fat, &final_data, &final_len, &final_cap);
        Ok(fat)
    }

    // ── Terminator: fold ─────────────────────────────────

    pub(crate) fn emit_terminator_fold(
        &mut self,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
        init_val: &str,
        term_fn_ptr: &str,
        term_env_ptr: &str,
        acc_ty: &str,
    ) -> Result<String, CodegenError> {
        let acc_ptr = self.emit_alloca_store(acc_ty, init_val);

        let lp = self.emit_pipeline_loop_header("fold", ctx, steps)?;

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
            ctx.final_elem_ty,
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

    pub(crate) fn emit_terminator_any_all(
        &mut self,
        is_any: bool,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
        term_fn_ptr: &str,
        term_env_ptr: &str,
    ) -> Result<String, CodegenError> {
        let prefix = if is_any { "any" } else { "all" };
        let init_val = if is_any { "false" } else { "true" };
        let short_circuit_val = if is_any { "true" } else { "false" };

        let result_ptr = self.emit_alloca_store("i1", init_val);

        let lp = self.emit_pipeline_loop_header(prefix, ctx, steps)?;

        // Body: call predicate, short-circuit on match
        let pred = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call i1 {}(ptr {}, {} {})",
            pred, term_fn_ptr, term_env_ptr, ctx.final_elem_ty, lp.current_val
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

    pub(crate) fn emit_terminator_find(
        &mut self,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
        term_fn_ptr: &str,
        term_env_ptr: &str,
    ) -> Result<String, CodegenError> {
        let option_name = self.yorum_type_to_option_name(&ctx.final_elem_ty);
        let option_llvm = format!("%{}", option_name);

        // Alloca result as None (tag=1)
        let result_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", result_ptr, option_llvm));
        self.emit_struct_field_store(&option_name, &result_ptr, 0, "i32", "1");

        let lp = self.emit_pipeline_loop_header("find", ctx, steps)?;

        // Body: call predicate, branch to found on match
        let pred = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call i1 {}(ptr {}, {} {})",
            pred, term_fn_ptr, term_env_ptr, ctx.final_elem_ty, lp.current_val
        ));
        let found_label = self.fresh_label("find.found");
        self.emit_cond_branch(&pred, &found_label, &lp.cond_label);

        // Found: store Some(val)
        self.emit_label(&found_label);
        self.block_terminated = false;
        self.emit_struct_field_store(&option_name, &result_ptr, 0, "i32", "0");
        self.emit_struct_field_store(
            &option_name,
            &result_ptr,
            1,
            &ctx.final_elem_ty,
            &lp.current_val,
        );
        self.emit_line(&format!("br label %{}", lp.end_label));

        // End
        self.emit_label(&lp.end_label);
        self.block_terminated = false;
        Ok(result_ptr)
    }

    // ── Terminator: reduce ─────────────────────────────────

    pub(crate) fn emit_terminator_reduce(
        &mut self,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
        term_fn_ptr: &str,
        term_env_ptr: &str,
        ret_ty: &str,
    ) -> Result<String, CodegenError> {
        let option_name = self.yorum_type_to_option_name(&ctx.final_elem_ty);
        let option_llvm = format!("%{}", option_name);

        // Alloca result as None (tag=1)
        let result_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", result_ptr, option_llvm));
        self.emit_struct_field_store(&option_name, &result_ptr, 0, "i32", "1");

        // Alloca accumulator and has_value flag
        let acc_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", acc_ptr, ret_ty));
        let has_value_ptr = self.emit_alloca_store("i1", "false");

        let lp = self.emit_pipeline_loop_header("red", ctx, steps)?;

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
            ctx.final_elem_ty, lp.current_val, acc_ptr
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
            ctx.final_elem_ty,
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

    // ── Terminator: sum ─────────────────────────────────

    pub(crate) fn emit_terminator_sum(
        &mut self,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
    ) -> Result<String, CodegenError> {
        let (init_val, add_op) = if ctx.final_elem_ty == "double" {
            ("0.0", "fadd")
        } else {
            ("0", "add")
        };
        let acc_ptr = self.emit_alloca_store(&ctx.final_elem_ty, init_val);

        let lp = self.emit_pipeline_loop_header("sum", ctx, steps)?;

        // Body: acc += elem
        let acc_val = self.emit_load(&ctx.final_elem_ty, &acc_ptr);
        let new_acc = self.fresh_temp();
        self.emit_line(&format!(
            "{} = {} {} {}, {}",
            new_acc, add_op, ctx.final_elem_ty, acc_val, lp.current_val
        ));
        self.emit_line(&format!(
            "store {} {}, ptr {}",
            ctx.final_elem_ty, new_acc, acc_ptr
        ));
        self.emit_line(&format!("br label %{}", lp.cond_label));

        // End: return accumulator
        self.emit_label(&lp.end_label);
        self.block_terminated = false;
        let result = self.emit_load(&ctx.final_elem_ty, &acc_ptr);
        Ok(result)
    }

    // ── Terminator: count ─────────────────────────────────

    pub(crate) fn emit_terminator_count(
        &mut self,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
    ) -> Result<String, CodegenError> {
        let counter_ptr = self.emit_alloca_store("i64", "0");

        let lp = self.emit_pipeline_loop_header("cnt", ctx, steps)?;

        // Body: counter += 1 (lp.current_val is unused but must be emitted for filters)
        let _ = lp.current_val;
        let cnt_val = self.emit_load("i64", &counter_ptr);
        let new_cnt = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", new_cnt, cnt_val));
        self.emit_line(&format!("store i64 {}, ptr {}", new_cnt, counter_ptr));
        self.emit_line(&format!("br label %{}", lp.cond_label));

        // End: return counter
        self.emit_label(&lp.end_label);
        self.block_terminated = false;
        let result = self.emit_load("i64", &counter_ptr);
        Ok(result)
    }

    // ── Terminator: position ─────────────────────────────────

    pub(crate) fn emit_terminator_position(
        &mut self,
        ctx: &PipelineContext,
        steps: &[IterStep<'_>],
        term_fn_ptr: &str,
        term_env_ptr: &str,
    ) -> Result<String, CodegenError> {
        let option_name = self.yorum_type_to_option_name("i64"); // Option<int>
        let option_llvm = format!("%{}", option_name);

        // Alloca result as None (tag=1)
        let result_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", result_ptr, option_llvm));
        self.emit_struct_field_store(&option_name, &result_ptr, 0, "i32", "1");

        // Position counter (counts elements passing through the pipeline)
        let pos_ptr = self.emit_alloca_store("i64", "0");

        let lp = self.emit_pipeline_loop_header("pos", ctx, steps)?;

        // Body: call predicate, branch to found on match
        let pred = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call i1 {}(ptr {}, {} {})",
            pred, term_fn_ptr, term_env_ptr, ctx.final_elem_ty, lp.current_val
        ));
        let found_label = self.fresh_label("pos.found");
        let cont_label = self.fresh_label("pos.cont");
        self.emit_cond_branch(&pred, &found_label, &cont_label);

        // Found: store Some(position_counter)
        self.emit_label(&found_label);
        self.block_terminated = false;
        let pos_val = self.emit_load("i64", &pos_ptr);
        self.emit_struct_field_store(&option_name, &result_ptr, 0, "i32", "0");
        self.emit_struct_field_store(&option_name, &result_ptr, 1, "i64", &pos_val);
        self.emit_line(&format!("br label %{}", lp.end_label));

        // Continue: increment position counter
        self.emit_label(&cont_label);
        self.block_terminated = false;
        let cur_pos = self.emit_load("i64", &pos_ptr);
        let next_pos = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", next_pos, cur_pos));
        self.emit_line(&format!("store i64 {}, ptr {}", next_pos, pos_ptr));
        self.emit_line(&format!("br label %{}", lp.cond_label));

        // End
        self.emit_label(&lp.end_label);
        self.block_terminated = false;
        Ok(result_ptr)
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

        // Bounds check elision: detect `for i in 0..len(arr)` pattern
        let elide_arr_name = if !inclusive {
            Self::detect_bounded_loop(start_expr, end_expr, body)
        } else {
            None
        };
        // Save previous binding (handles nested loops with same var name)
        let prev_binding = if let Some(ref arr_name) = elide_arr_name {
            self.bounded_loop_vars
                .insert(var_name.to_string(), arr_name.clone())
        } else {
            None
        };

        self.loop_labels
            .push((inc_label.clone(), end_label.clone()));
        self.emit_block(body)?;
        self.loop_labels.pop();

        // Restore previous bounded loop var binding (or remove if none)
        if elide_arr_name.is_some() {
            if let Some(prev) = prev_binding {
                self.bounded_loop_vars.insert(var_name.to_string(), prev);
            } else {
                self.bounded_loop_vars.remove(var_name);
            }
        }

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

    /// Emit a for-loop over an unbounded range: `for i in start.. { body }`
    /// Loop has no bounds check; user must break out manually.
    pub(crate) fn emit_for_unbounded_range(
        &mut self,
        var_name: &str,
        start_expr: &Expr,
        body: &Block,
    ) -> Result<(), CodegenError> {
        let start_val = self.emit_expr(start_expr)?;
        let var_ptr = self.emit_alloca_store("i64", &start_val);

        let cond_label = self.fresh_label("for.cond");
        let body_label = self.fresh_label("for.body");
        let inc_label = self.fresh_label("for.inc");
        let end_label = self.fresh_label("for.end");

        self.emit_line(&format!("br label %{}", cond_label));

        // Condition: unconditional (user must break)
        self.emit_label(&cond_label);
        self.block_terminated = false;
        self.emit_line(&format!("br label %{}", body_label));

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

        // Increment
        self.emit_label(&inc_label);
        self.block_terminated = false;
        let cur_val = self.emit_load("i64", &var_ptr);
        let next_val = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", next_val, cur_val));
        self.emit_line(&format!("store i64 {}, ptr {}", next_val, var_ptr));
        self.emit_line(&format!("br label %{}", cond_label));

        self.emit_label(&end_label);
        self.block_terminated = false;
        Ok(())
    }

    /// Detect `for i in 0..len(arr)` pattern for bounds check elision.
    /// Returns `Some(arr_name)` if the pattern matches and body doesn't mutate the array.
    fn detect_bounded_loop(start_expr: &Expr, end_expr: &Expr, body: &Block) -> Option<String> {
        // start must be literal 0
        let is_zero_start = matches!(&start_expr.kind, ExprKind::Literal(Literal::Int(0)));
        if !is_zero_start {
            return None;
        }

        // end must be Call(Ident("len"), [Ident(arr_name)])
        let arr_name = if let ExprKind::Call(callee, args) = &end_expr.kind {
            if let ExprKind::Ident(fn_name) = &callee.kind {
                if fn_name == "len" && args.len() == 1 {
                    if let ExprKind::Ident(name) = &args[0].kind {
                        Some(name.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }?;

        // Body must not mutate the array (no push/pop/reassignment)
        if Self::body_mutates_array(body, &arr_name) {
            return None;
        }

        Some(arr_name)
    }
}
