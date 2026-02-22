# Changelog

All notable changes to the Yorum programming language compiler.

## [1.12.2] - 2026-02-22

**While-Loop Bounds Check Elision** — Extends the bounds check elision optimization from `for i in 0..len(arr)` loops to `while` loops with `while var < len(arr)` or `while var < n` (where `n` is a known alias for `len(arr)`). Enables safe elision for patterns like the Sieve of Eratosthenes with non-sequential index increments.

### Added

- **While-loop bounds elision:** `while j < len(arr) { arr[j] = ...; j += step; }` now elides bounds checks when the compiler can prove `j < len(arr)` holds at every array access. Supports direct `len()` calls and indirect aliases via `let n = len(arr)` or `let arr = [0; n]`
- **Len alias tracking:** immutable bindings from `let n = len(arr)` and array-repeat `let arr = [val; n]` are tracked as length aliases, enabling elision in `while j < n` patterns
- **Access-before-modification analysis:** static analysis verifies all `arr[var]` accesses precede any `var` modifications within the loop body, ensuring the loop condition still holds at access time
- **Safety guards:** elision is conservatively disabled when the bound variable is mutable/reassigned, the array is mutated (push/pop), or the index is modified before access
- **`examples/sieve.yrm`:** new example demonstrating the Sieve of Eratosthenes with while-loop bounds check elision
- 8 new integration tests for while-loop elision (4 positive + 3 negative safety + 1 end-to-end sieve)

**Stats:** 4 files changed | Tests: 764 (86 unit + 678 integration)

---

## [1.12.1] - 2026-02-21

**LSP Chain-Aware Completions** — Dot-completions now work after any point in an iterator pipeline chain, with accurate element types derived by walking the AST through `.map()`, `.filter()`, `.enumerate()`, `.zip()`, and all other combinators/terminators.

### Added

- **Chain-aware dot-completions:** typing `.` after `arr.iter()`, `arr.iter().map(|x: int| -> string { ... })`, or any pipeline chain now shows the correct iterator combinators and terminators with typed signatures. Element types are propagated through each pipeline step — e.g., after `.map(|x: int| -> string { ... })`, `.collect()` shows `() -> [string]`
- **Chain text extraction:** `extract_chain_text()` scans backwards from the trailing dot, tracking `()`, `{}`, `[]` depth to extract the full method chain expression from the line text
- **AST-based chain parsing:** `parse_chain_expr()` wraps the chain in a synthetic function, parses it with the real Yorum parser, and extracts the expression AST node for type-safe analysis
- **Type propagation engine:** `walk_chain_type()` recursively walks nested `MethodCall` AST nodes, propagating element types through all 11 combinators and 9 terminators. Reads closure return type annotations directly from the AST (no type inference needed)
- **Iterator completion list:** `iterator_methods()` returns typed completions for `map`, `filter`, `enumerate`, `zip`, `take`, `skip`, `chain`, `take_while`, `rev`, `flat_map`, `flatten`, `collect`, `fold`, `reduce`, `any`, `all`, `find`, `sum`, `count`, `position` — all parameterized by the resolved element type
- **Non-iterator chain results:** `.collect().` on a pipeline correctly shows array methods (`len`, `push`, `iter`, etc.) since `collect()` returns `[T]`
- 13 new unit tests for chain completion (extract_chain_text, parse_chain_expr, walk_chain_type, end-to-end scenarios)

### Fixed

- **LSP dot-completion for v1.12.0 features:** completions now correctly resolve types for `Set<T>`, `Map<K,V>`, `Task<T>`, `Chan<T>`, `Result<T,E>`, and other generic types introduced or expanded in v1.12.0

### Changed

- Expanded README iterator pipelines code example with comprehensive coverage of all combinators and terminators
- Updated ROADMAP with iterator trait/protocol analysis and UTF-8/Unicode analysis

**Stats:** 3 files changed, +771 -59 | Tests: 837 (81 unit + 756 integration)

---

## [1.12.0] - 2026-02-21

**Iterator Ecosystem** — Complete iterator pipeline expansion: `.chain()`, `.flat_map()`, `.flatten()`, `.take_while()`, `.chars()`, `.rev()`, `.sum()`, `.count()`, `.position()`, `.clear()`, unbounded ranges, `Set.iter()`, `Map.iter()`, and extensive codegen hardening.

### Added

- **`.chain(arr)` combinator:** concatenates two iterables in a fused pipeline. Must be the first combinator after `.iter()`. Works with array sources, range sources, all downstream combinators, and all terminators. Emits conditional source selection via `phi` nodes
- **`.flat_map(f)` combinator:** maps each element to an array and flattens the results into a single stream. Fused into a single nested loop — no intermediate allocations
- **`.flatten()` combinator:** flattens arrays of arrays into a single stream. Works with all downstream combinators and terminators
- **`.take_while(f)` combinator:** yields elements while predicate returns true, then exits the loop
- **`.chars()` source:** `"hello".chars()` iterates over string characters as `char` values. Works in for-loops and with all terminators
- **`.rev()` combinator:** reverses iteration order for arrays (iterates from last to first element)
- **`.sum()` terminator:** accumulates all elements with `+` (int or float), returns the sum
- **`.count()` terminator:** counts elements in the pipeline, returns `int`
- **`.position(f)` terminator:** finds index of first element matching predicate, returns `Option<int>`
- **`.clear()` method:** empties an array by setting length to 0 (preserves capacity)
- **Unbounded ranges:** `(start..).iter()` creates infinite ranges — must be bounded by `.take(n)` or `.take_while(f)`. Compile-time error if unbounded range has no limiter
- **`Set.iter()` and `Map.iter()`:** iterate over Set elements and Map key-value pairs as `(K, V)` tuples. Work in for-loops and with all combinators/terminators
- **`zip(range)` source:** `.zip((0..n))` pairs elements with range values
- `examples/iterators.yrm` expanded with chain, flat_map, flatten, take_while, chars, rev, sum, count, position, Set/Map iteration demos
- 112 new integration tests (756 total: 68 unit + 688 integration)

### Fixed

- **Set/Map return-by-value SIGBUS crash:** `infer_collection_type()` now handles `Type::Generic("Set"|"Map", args)` in addition to `Type::Named`, fixing crashes when calling `.iter()` on Set/Map values returned from functions or accessed via struct fields
- **Set/Map parameter binding detection:** `emit_function` now inserts Set/Map parameter types into `var_ast_types` for both `Type::Named` and `Type::Generic` forms, fixing pipeline dispatch for function parameters
- **Flat-map label emission for filter/take_while:** removed overly-restrictive guard that skipped label emission for non-final filter and take_while steps in flat-map pipelines
- **Flat-map find Option registration:** `ensure_option_enum_for_llvm_type` now called before Find alloca in flat-map terminators, preventing missing Option type layout
- **Flat-map find pointer return:** find terminator in flat-map now returns the result pointer directly instead of loading it (matching the expected pointer semantics)
- **Map.iter().chain() type mismatch:** chain first-source branch now constructs (K,V) tuples inline for Map sources and uses correct element type for GEP operations
- **Tuple array allocation size:** `ensure_pipeline_tuple_type` now called before `llvm_type_size` for TupleLit elements, fixing incorrect `malloc(8)` instead of `malloc(16)` for tuple arrays
- **Array-of-arrays inner pointer lifetime:** inner array fat pointers are now heap-allocated via `malloc(24)` + `memcpy` instead of stack allocas, preventing dangling pointers when the enclosing function returns
- **False-positive regression tests:** changed 3 test assertions from `ir.contains("@set_values__int")` to `ir.contains("call ptr @set_values__int")` to verify actual call sites, not just function definitions
- **Collect truncation safety:** collect terminator now uses correct length tracking
- **chars type consistency:** `.chars()` now consistently returns `i8` element type across all pipeline stages

### Changed

- Refactored codegen boilerplate in pipeline step emission and terminator loop setup
- Extended `mangle_type_suffix` usage for Set/Map type mangling from Generic form

**Stats:** 15 files changed, +5,102 -510 | Tests: 756 (68 unit + 688 integration)

---

## [1.11.0] - 2026-02-21

**Array Repeat Syntax & Bounds Check Elision** — Two codegen optimizations: bulk array allocation with `[value; count]` and automatic bounds check removal in provably safe loops.

### Added

- **Array repeat syntax:** `[value; count]` creates an array of `count` copies of `value` in one allocation. Zero values (`0`, `0.0`, `false`) use `memset` fast path; non-zero values (including structs) use a fill loop with `memcpy` for aggregates. Count can be a runtime expression
- **Bounds check elision:** `for i in 0..len(arr) { arr[i] }` no longer emits `@__yorum_bounds_check` calls when the compiler can prove the index is in-bounds. Conditions: exclusive range starting at 0, end is `len(arr)`, loop body doesn't mutate the array (no `push`/`pop`/reassignment). Works for both read (`arr[i]`) and write (`arr[i] = val`) index access. Correctly handles nested loops with shadowed variables via save/restore of bindings
- **New AST variant:** `ExprKind::ArrayRepeat(Box<Expr>, Box<Expr>)` with full pipeline support (parser, typechecker, ownership checker, monomorphizer, DCE, formatter, codegen)
- `examples/array_repeat.yrm` — demonstrates zero-init, non-zero fill, runtime count, empty arrays
- 17 new integration tests covering both features, including safety tests for elision edge cases (inclusive ranges, different arrays, mutation, expression indices, nested loops, struct values)

### Fixed

- **LSP symbol tracking:** fixed missing symbol references for variables and struct fields in the language server
- **LSP dot completions:** type-aware completions for struct fields and methods after `.`
- **LSP keyword fix:** corrected keyword completion list

**Stats:** 14 files changed | Tests: 712 (68 unit + 644 integration)

---

## [1.10.0] - 2026-02-20

**Codegen Refactor** — Extract helpers, deduplicate boilerplate, and split the monolithic `codegen.rs` into a directory module.

### Changed

- **Fat pointer helpers:** extracted `emit_fat_ptr_load`, `emit_fat_ptr_init`, `emit_fat_ptr_field_load`, `emit_fat_ptr_field_store` to eliminate repeated GEP/load/store boilerplate for array fat pointers
- **Struct field helpers:** added `emit_struct_gep`, `emit_struct_field_load`, `emit_struct_field_store` for named struct field access
- **Low-level IR helpers:** added `emit_load`, `emit_alloca_store`, `emit_memcpy`, `emit_cond_branch` to reduce inline format string repetition
- **Builtin helpers split:** `emit_builtin_helpers` decomposed into 7 thematic sub-methods (`emit_builtin_io_helpers`, `emit_builtin_concurrency_helpers`, `emit_builtin_conversion_helpers`, `emit_builtin_map_core_helpers`, `emit_builtin_math_helpers`, `emit_builtin_string_helpers`, `emit_builtin_collection_helpers`)
- **Pipeline deduplication:** unified 5 terminator loop prologues into shared `emit_pipeline_loop_header`
- **Module extraction:** split `codegen.rs` (9,892 lines) into directory module `codegen/`:
  - `mod.rs` (3,138 lines) — core infrastructure, `emit_expr`, `emit_function`, closures, spawn
  - `builtins.rs` (3,513 lines) — all `emit_builtin_*`, map/set/networking helpers
  - `pipelines.rs` (1,609 lines) — pipeline extraction, steps, terminators, `emit_for_range`
  - `statements.rs` (886 lines) — `emit_let`/`assign`/`if`/`while`/`for`/`match`/`return`
  - `types.rs` (772 lines) — `llvm_type`, type predicates, tuple management, const folding

All changes are purely mechanical refactoring — no behavior changes.

**Stats:** 7 files changed | Tests: 695 (68 unit + 627 integration)

---

## [1.9.1] - 2026-02-20

**Iterator Combinators, Terminators & Range Pipelines** — `.enumerate()`, `.zip()`, `.take()`, `.skip()` combinators, `.collect()`, `.fold()`, `.any()`, `.all()`, `.find()`, `.reduce()` terminators, and range expressions as pipeline sources (Phase 3).

### Added

- **4 combinators** for for-loop pipelines, all fused into a single loop:
  - `.enumerate()` — yields `(int, T)` tuples with 0-based index
  - `.zip(arr2)` — pairs elements from two arrays as `(T, U)` tuples, stops at shorter length
  - `.take(n)` — yields only the first N elements, then exits the loop
  - `.skip(n)` — skips the first N elements, then yields the rest
- **6 terminators** as standalone expressions (not inside for-loops):
  - `.collect()` — materializes a pipeline into a new array `[T]` (only allocation in the system)
  - `.fold(init, |acc, x| -> U)` — accumulates with initial value, returns `U`
  - `.any(|x| -> bool)` — short-circuits on first true, returns `bool`
  - `.all(|x| -> bool)` — short-circuits on first false, returns `bool`
  - `.find(|x| -> bool)` — short-circuits on first match, returns `Option<T>`
  - `.reduce(|acc, x| -> T)` — accumulates without initial value, returns `Option<T>`
- **Range pipeline sources:** `(start..end).iter()` and `(start..=end).iter()` work with all combinators and terminators. Loop condition uses direct value comparison (not pre-computed length) to avoid overflow on wide ranges. Collect preallocation uses saturating i128 math clamped to `[0, i64::MAX]`
- **Terminator infrastructure:** `PipelineTerminator` enum, `TerminatedPipeline` struct, `try_extract_terminated_pipeline()` in codegen, `is_pipeline_terminator()` and `infer_pipeline_terminator_type()` in typechecker. Terminators intercepted before receiver inference in both `infer_expr` and `emit_expr`
- **Monomorphizer extension:** `find()` and `reduce()` automatically register `Option<T>` monomorphization via `has_iter_base_static()` in `collect_from_expr`
- `examples/iterators.yrm` expanded with range pipeline demos (range+map+collect, range+filter+fold, inclusive range+enumerate, range+skip+take)
- 47 new integration tests (combinators: 14, terminators: 16, combined chains: 3, range pipelines: 6, post-review: 8)

### Fixed

- **Tuple mangle bug:** `mangle_name` produced invalid LLVM identifiers for tuple types (e.g., `Option__(int, int)`). Now generates `Option__tuple.int.int` via recursive `mangle_type_arg` helper
- **Pipeline aggregate representation:** tuple types registered before downstream closures are emitted; tuple values loaded by-value after enumerate/zip; by-value `store` replaces `memcpy` for pipeline outputs in for-loop, collect, and find terminators
- **Range overflow hardening:** descending ranges clamped to zero length; inclusive ranges ending at `i64::MAX` terminate correctly via `llvm.sadd.with.overflow` guard; negative `take()` counts clamped to zero; collect byte-size multiplication guarded against overflow; zip length included in range preallocation cap
- **Zero-sized element panic:** removed unreachable `elem_size == 0` branch in collect (could have caused divide-by-zero if reached)

### Changed

- Merged `emit_terminator_any` and `emit_terminator_all` into unified `emit_terminator_any_all` parameterized by `is_any`
- Removed dead `ClosureInfo::param_ty` field
- Added `ensure_pipeline_tuple_type` helper replacing 4 inline tuple type registration patterns
- Dropped redundant `is_range_source` from preamble return tuple (callers read `pipeline.is_range_source` directly)
- Shared step emission logic between for-loop and terminators via `emit_pipeline_preamble` and `emit_pipeline_steps`

### Design decisions

- Combinators compose freely with existing `.map()` and `.filter()` in any order
- Terminators work on any pipeline chain (e.g., `.iter().filter(f).map(g).collect()`)
- All fused — zero allocations except `.collect()` which pre-allocates capacity equal to source length
- Range pipelines use direct value comparison in loop condition, avoiding overflow from pre-computing total length
- `.find()` and `.reduce()` return `Option<T>`, requiring monomorphizer coordination
- `.any()` and `.all()` short-circuit for performance (break out of loop on first decisive element)
- `.fold()` supports type-changing accumulation (init type can differ from element type)
- Empty arrays: `.any()` returns false, `.all()` returns true (vacuous truth), `.reduce()` returns None, `.fold()` returns init

**Stats:** 11 files changed, +3,480 -203 | Tests: 695 (68 unit + 627 integration)

---

## [1.9.0-beta] - 2026-02-19

**Iterator Pipelines** — `.map()` and `.filter()` with fused for-loop codegen (Phase 2).

### Added

- **Iterator pipelines:** `for x in arr.iter().map(f).filter(g) { ... }` compiles to a single fused LLVM loop with inline closure calls — zero allocation, zero iterator struct overhead
- **Typechecker pipeline validation:** `infer_pipeline_elem_type()` recursively walks `.iter().map().filter()` chains, validates closure parameter/return types at each step. Requires inline closures (named closure variables rejected with clear error). Unit-returning `map()` rejected
- **Pipeline-aware ownership:** `infer_iterable_elem_type()` extended with `has_iter_base()` guard to correctly infer element types through map/filter chains without affecting struct methods named map/filter
- **Codegen fused loop emitter:** `try_extract_pipeline()` AST walker + `emit_for_pipeline()` with `IterStep`/`IterPipeline` types. Closures emitted as `{ fn_ptr, env_ptr }` pairs; index incremented in step block before filter branches
- `examples/iterators.yrm` — 9 demos: basic map, basic filter, filter+map chain, map+filter chain, captures, long 4-step chain, break, continue, float pipeline
- 21 new integration tests (compilation, type rejection, break/continue, edge cases, regressions)

### Design decisions

- `.map()` and `.filter()` only valid on chains starting with `.iter()` on an array, only inside for-loops
- Struct methods named `map`/`filter` are not affected — all three checkers (typechecker, codegen, ownership) use `is_iter_pipeline`/`has_iter_base` to require `.iter()` at the chain base
- Named closure variables not supported in pipelines (typechecker rejects at check time, matching codegen constraint)
- `continue` in pipeline loops targets `for.cond` (index already incremented in step block)
- `break` targets `for.end`

**Stats:** 8 files changed | Tests: 622 (68 unit + 554 integration)

---

## [1.9.0-alpha] - 2026-02-19

**Inclusive Range & Array Iterators** — `..=` and `.iter()` for for-loops (Phase 1).

### Added

- **Inclusive range `..=`:** `for i in 0..=n { ... }` iterates from 0 to n (inclusive), using `icmp sle` instead of `icmp slt`. Overflow guard at `i64::MAX` prevents counter wrapping
- **Array `.iter()` in for-loops:** `for x in arr.iter() { ... }` iterates over array elements. Handled structurally (no phantom `ArrayIter<T>` type) — `.iter()` on arrays is a no-op that returns the receiver directly
- **Struct `.iter()` preserved:** user-defined `iter()` methods on structs dispatch normally. `expr_struct_name()` extended with `FieldAccess` handling to correctly classify nested struct field accesses (e.g. `h.w.iter()`)
- **`infer_array_elem_type()` helper:** resolves array element LLVM types through struct field layouts and function return types, fixing `i64` default fallback for non-Ident receivers like `b.items.iter()` where `items: [float]`
- **Struct iter return type inference:** struct `.iter()` methods that return arrays (e.g. `fn iter(self) -> [float]`) correctly propagate element type to for-loop codegen
- `.iter()` rejected outside for-loops (hits "requires struct type" error in `infer_expr`)
- `RangeInclusive` handled across all compiler passes: lexer, parser, typechecker, codegen, ownership, monomorphizer, DCE, formatter, project rewriter
- 21 new integration tests: inclusive range (basic, variables, expression bounds, type error, outside-for error, typecheck, overflow guard, AST JSON, formatter), array iter (basic, typecheck, string elements, no-args, wrong method, outside-for rejected, field access), struct iter (method in for-loop, compiles, float return type, nested field dispatch)

### Design decisions

- No phantom types — `.iter()` on arrays handled structurally, not via `ArrayIter<T>`
- `.iter()` only valid in for-loop iterables, not as standalone expressions
- Struct methods named `iter()` fall through to normal method dispatch via `expr_struct_name()` guard

**Stats:** 12 files changed | Tests: 601 (68 unit + 533 integration)

---

## [1.8.2] - 2026-02-18

**Performance** — capacity-aware `str_concat` optimization eliminates quadratic string building.

### Changed

- **`@str_concat` uses `memcpy`** — replaced `strcpy`/`strcat` with `memcpy` in the `@str_concat` LLVM IR definition, eliminating redundant `strlen` scans per call
- **Inline capacity-aware concat** — `s = str_concat(s, x)` pattern is detected at codegen time and emits inline code with per-variable `{len, cap}` tracking and `realloc`-based growth, turning O(n^2) string building loops into amortized O(n)

### Added

- 6 new integration tests for string builder optimization: `test_str_concat_inplace_loop`, `test_str_concat_non_self_fallback`, `test_str_concat_literal_init`, `test_str_concat_reassignment_resets`, `test_str_concat_self_self_fallback`, `test_str_concat_memcpy_definition`

### Fixed

- **Temp/parameter name collision** — `fresh_temp()` now emits `%.tN` instead of `%tN`, preventing collisions with user parameter names like `t0`, `t1` that occupy the same LLVM namespace
- **Scope-safe buffer metadata** — string buffer tracking keyed by alloca pointer (not variable name), preventing cross-scope metadata corruption when inner scopes shadow string variables
- **Init path heap safety** — `strlen` of existing data computed before `malloc` in the init path, preventing heap overflow when a string initialized from a function call is longer than the initial buffer
- **Conservative alias safety** — inline concat restricted to string-literal suffixes only, preventing use-after-realloc when the suffix could alias the target buffer
- 3 new regression tests: `test_param_name_no_collision_with_temps`, `test_str_concat_variable_suffix_fallback`, `test_str_concat_shadowing_safety`

**Stats:** 3 files changed | Tests: 512 (68 unit + 512 integration)

---

## [1.8.1] - 2026-02-18

**Codegen Bug Fix** — duplicate variable names in nested scopes no longer produce invalid LLVM IR.

### Fixed

- **Unique alloca names** — `emit_let` and function parameter allocas now use `fresh_temp()` instead of `format!("%{}.addr", name)`, preventing duplicate LLVM SSA names when the same variable name appears in nested scopes or is re-bound in the same scope
- **Block scoping in codegen** — `emit_if` (then/else branches) and `emit_while` (loop body) now push/pop variable scopes, so inner `let` bindings correctly shadow outer variables instead of overwriting them

### Added

- 4 new integration tests for variable shadowing: `test_variable_shadowing_in_if`, `test_variable_shadowing_in_else`, `test_variable_rebinding_same_scope`, `test_variable_shadowing_param`

**Stats:** 2 files changed | Tests: 503 (68 unit + 503 integration)

---

## [1.8.0] - 2026-02-18

**Package Manager** — external dependencies via git URLs or local filesystem paths.

### Added

- **`yorum install`** — fetch and cache all dependencies declared in `yorum.toml`, write `yorum.lock`
- **`yorum update [name]`** — fetch latest versions for all (or named) dependencies, regenerate lock file
- **Dependency declaration** — `[dependencies]` section in `yorum.toml` supports `{ git = "...", tag/branch/rev = "..." }` and `{ path = "..." }` specs
- **Lock file** — `yorum.lock` records exact git SHAs and path sources for reproducible builds (TOML format)
- **Package cache** — git dependencies cached in `~/.yorum/cache/` with content-addressed directory names
- **Namespace isolation** — dependency symbols prefixed with `<dep_name>__` (e.g., `math_utils__add`) to prevent collisions with local modules
- **Semver parsing** — `Version` struct for package metadata validation (`MAJOR.MINOR.PATCH` with optional `v` prefix)
- **Dependency validation** — clear error messages for missing paths, missing `yorum.toml`, package name mismatches, and invalid dependency specs
- **`use` resolution for deps** — `use dep_name;` imports all pub symbols from a declared dependency, with automatic name rewriting
- **Type rewriting for imported declarations** — function signatures, struct fields, and bodies in imported modules have their type references properly prefixed
- `examples/dependencies/` — example project demonstrating path dependencies between packages
- `install_dependencies()` and `update_dependencies()` public API functions
- 13 new integration tests for package manager features

### Changed

- `yorum.toml` `dependencies` field changed from `HashMap<String, String>` to `HashMap<String, DependencySpec>` (structured git/path specs)
- `compile_project()` now resolves dependencies before module discovery, automatically generating `yorum.lock` on first build
- Module merging extended to handle dependency modules alongside local modules
- Imported declarations (from both local modules and deps) now have their internal type references rewritten to use prefixed names

**Stats:** 13 files changed | Tests: 499 (68 unit + 431 integration)

---

## [1.7.0] - 2026-02-18

**Performance & Optimization** — inline hints, constant folding, tail call optimization, heap sort, dead code elimination.

### Added

- **Inline hint annotations** — small pure functions (<=3 statements, no contracts) get `alwaysinline` LLVM attribute via `#0` attribute group
- **Constant folding** — literal integer and boolean expressions evaluated at compile time. Arithmetic (`+`, `-`, `*`), bitwise, comparison, and logical operators folded. Division/modulo by zero falls through to runtime
- **Tail call optimization** — `return f(args)` patterns annotated with `tail call` LLVM hint, enabling LLVM to reuse the caller's stack frame for tail-recursive functions
- **Heap sort upgrade** — `sort_int` and `sort_str` builtins upgraded from O(n^2) insertion sort to O(n log n) heap sort (in-place, no extra allocation beyond the copy)
- **Dead code elimination** — new `dce.rs` pass performs BFS reachability from `main`, removing unreachable functions, structs, enums, and impl blocks before codegen. Reduces IR size for programs with unused declarations
- `examples/optimization.yrm` — new example exercising all v1.7 features
- 18 new integration tests for optimization features

### Changed

- Compilation pipeline now includes DCE pass between monomorphization and codegen (both single-file and multi-file)
- 7 existing integration tests updated to account for constant folding and DCE behavior

**Stats:** 9 files changed | Tests: 481 (50 unit + 431 integration)

---

## [1.6.1] - 2026-02-18

**Formatter Bug Fix** — trailing comments now stay on the same line.

### Fixed

- **Trailing comments displaced to next line:** `let x: int = 15; // note` was reformatted with `// note` on its own line before the next statement. Root cause: `emit_leading_comments` consumed all comments with `span.end <= node_start`, including same-line trailing comments. Fix: after each construct is emitted, `emit_trailing_comment_for()` checks if the next unconsumed comment is on the same source line and inlines it with two-space separation
- **Block comments mis-tagged in lexer:** `/* ... */` comments had `is_block: false`, which prevented the multi-line block comment exclusion from working. Fix: `is_block: true` for block comments
- **Compact construct comment mis-association:** Comments after closing delimiters (`fn a() -> int { return 1; } // note`) could be pulled into the inner node (`return 1;`). Fix: `is_trailing_gap()` requires only whitespace and punctuation (`,`, `;`) between construct end and comment start — closing delimiters (`}`, `)`, `]`) break the trailing association

### Added

- `is_same_line()`, `is_trailing_gap()`, `emit_trailing_comment_for()`, `decl_span_end()` in formatter
- Trailing comment attachment at 6 call sites: statements, top-level declarations, impl methods, trait methods, struct fields, enum variants
- 10 new integration tests for trailing comments
- All 23 example programs reformatted with corrected formatter

**Stats:** 26 files changed | Tests: 513 (50 unit + 463 integration)

---

## [1.6.0] - 2026-02-18

**Auto-Formatter** — `yorum fmt` for consistent code style.

### Added

- **`yorum fmt`** — auto-format Yorum source files. Reads source, lexes with comment collection, parses to AST, then walks the AST to produce consistently formatted output. Preserves comments (line and block), string interpolation, compound assignments (`+=`, `-=`, etc.), and blank lines between logical sections
- **`--check` flag** — exits with code 1 if any file would change (for CI integration)
- **Comment collection in lexer** — `Lexer::new_with_comments()` and `tokenize_with_comments()` capture comments as a side-channel without affecting the normal lexing path
- **`format_source()` public API** — format Yorum source code without type-checking or codegen
- 20 new integration tests

### Formatting rules

- 4-space indentation, no tabs
- K&R brace style: `{` on same line, `}` on own line
- Exception: when function has contract clauses (`requires`/`ensures`/`effects`), `{` goes on its own line below the clauses
- One blank line between top-level declarations
- Spaces around binary operators: `a + b`
- No space between function name and `(`: `print_int(x)`
- Match arms: `pattern => { body },` with trailing comma
- Compound assignments round-trip: `x += 1` not `x = x + 1`
- String interpolation preserved from source: `"hello {name}"`
- `{{` and `}}` brace escaping preserved in plain strings
- Single trailing newline at end of file

**Stats:** 7 files changed, ~1,100 lines added | Tests: 503 (50 unit + 453 integration)

---

## [1.5.0] - 2026-02-18

**Tooling & Developer Experience** — 5 new features for a complete developer workflow.

### Added

- **`yorum run`** — compile, link (via clang), and execute in one command. Auto-detects clang (system or Homebrew LLVM), auto-links `-lpthread` when needed, forwards `-- args` to the binary. Supports both single-file and project mode
- **`yorum repl`** — interactive REPL for expression evaluation. Accumulates function/struct/enum definitions across lines. Special commands: `:type <expr>` (show type), `:clear` (reset definitions), `:help`, `:exit`. Multiline input with brace tracking
- **LSP completions** — autocomplete triggered by typing or `.` (dot). Completes identifiers from source, 70+ builtin functions with signatures, language keywords, and struct fields via dot-completion
- **LSP code actions** — quick fixes attached to diagnostics: "Did you mean X?" with Levenshtein distance matching (threshold <= 2) for undefined variables/functions, effect clause hints for effect violations, missing match arm hints for non-exhaustive patterns
- **DWARF debug info** — `-g`/`--debug` flag on `compile` and `run` commands. Emits `!DICompileUnit`, `!DIFile`, `!DISubprogram`, `!DISubroutineType`, `!DIBasicType` metadata. Enables source-level stepping in `lldb`/`gdb`
- `compile_to_ir_with_options(source, filename, debug)` public API
- `builtin_function_list()` public API for tooling

### Changed

- `ServerCapabilities` now advertises `completionProvider` (with `.` trigger) and `codeActionProvider`
- 12 new tests (478 total: 50 unit + 428 integration)

**Stats:** 10 files changed, ~800 lines added | Formatter (`yorum fmt`) deferred — requires lexer to preserve comments

---

## [1.4.1] - 2026-02-17

**Restore Self-hosting Bootstrap & Bug Fixes** — [PR #19](https://github.com/MehmetMelik/yorum/pull/19)

The self-hosting compiler (`yorum-in-yorum`) was broken by features added in v0.8–v1.3. Four fixes restore the full bootstrap chain (`gen1.ll == gen2.ll`).

### Fixed

- **String interpolation breaks self-hosting compiler:** Escaped `{`/`}` → `{{`/`}}` in 64 string literals in `main.yrm` containing LLVM IR braces. Added `{{`/`}}` → `{`/`}` handling in the self-hosted lexer so it can read its own source
- **Ownership checker rejects arrays/maps:** Made `[T]`, `Map<K,V>`, and `Set<T>` copy types — they use reference semantics at runtime (heap-allocated pointers), consistent with `string` which was already copy
- **Array index of aggregates produces invalid IR:** `expr_returns_ptr()` didn't handle `ExprKind::Index` for struct/enum elements. Array index codegen returns alloca pointers via memcpy, but `emit_let` treated them as values, producing `store %Token %ptr`
- **Nested generic type comparison fails:** Extracted recursive `types_compatible()` helper with TypeVar wildcard matching. Fixes `push(arr, map_new())` and `let x: [Map] = [map_new()]` where `Map<K,V>` TypeVars weren't matched against `Map<string, int>` inside `Array` wrappers. Replaces duplicated inline checks in let-bindings, assignments, and returns

### Changed

- Copy types expanded: `int`, `float`, `bool`, `char`, `string`, `unit`, `[T]`, `Map<K,V>`, `Set<T>`. Structs and enums remain move types
- ROADMAP.md updated: v1.4 marked as done

**Stats:** 6 files changed, +168 -142 | Bootstrap: gen1.ll == gen2.ll (fixed-point verified)

---

## [1.4.0] - 2026-02-17

**Effect System Enforcement** — [PR #17](https://github.com/MehmetMelik/yorum/pull/17)

The `effects` clause, parsed since v1.0 but previously ignored, is now enforced at compile time. This is Yorum's most distinctive feature for LLM-first development — an AI agent can read `effects io, net` and know exactly what a function can touch.

### Added

- Six effect categories: `io`, `fs`, `net`, `time`, `env`, `concurrency`
- Compile-time effect checking at all call sites (general calls, method calls, `chan`/`send`/`recv`, `args`, `spawn`)
- Automatic effect inference for unannotated functions via fixed-point call graph iteration (pass 1.5 in typechecker)
- `FnSig.effects: Option<Vec<String>>` — `None` = unchecked, `Some([])` = no effects, `Some(["io"])` = io only
- `VALID_EFFECTS` constant and `builtin_effects()` mapping for all ~70 builtins
- `check_call_effects()` enforcement at every call site
- `infer_all_effects()` engine with AST traversal helpers (`collect_calls_in_block/stmt/expr`)
- Empty `effects {}` syntax (parser fix — previously required at least one effect name)
- 39 new integration tests

### Fixed

- Method calls bypassed purity and effect enforcement — added checks to `MethodCall` handler
- Effect inference missed method-call edges — added `__method__<name>` sentinel in call graph
- Unknown effect names on `main` silently accepted — validation now runs before unchecked/main branching

### Design decisions

- Functions without `effects` clause remain unchecked (backward compatible)
- `main` is always unchecked (entry point)
- `pure fn` + `effects` is a compile error (`pure` is strictly more restrictive)
- Memory-only builtins (`push`, `pop`, `str_concat`, `map_*`, `set_*`, math) need no effect
- Closures inherit enclosing function's effects
- Effects are compile-time only — no LLVM IR emitted

**Stats:** 9 files changed, +1,198 -129 | Tests: 462 (46 unit + 416 integration)

---

## [1.3.2] - 2026-02-17

**Codegen Fixes & Example Expansion** — [PR #16](https://github.com/MehmetMelik/yorum/pull/16)

Seven codegen bugs fixed and 10 new example programs covering all major language features.

### Fixed

- **Math intrinsic name collision:** Wrapper functions `@sqrt`, `@pow`, `@floor`, `@ceil`, `@round` shadowed C library symbols causing infinite recursion. Fix: inline LLVM intrinsic calls at call sites
- **Spawn return type mismatch:** `return 0;` in spawn blocks emitted `ret ptr 0` (invalid IR). Fix: `spawn_return_ctx` field stores value in env struct result slot
- **Spawn join returns hardcoded 0:** `.join()` always returned 0. Fix: `task_env_info` tracking so `.join()` loads from env struct result slot
- **Channel multi-send deadlock:** `send()` didn't wait for slot consumption; `recv()` didn't signal after consuming. Fix: wait loop in `send`, signal in `recv`
- **Duplicate for-loop variable allocas:** Same variable name in multiple for loops caused "multiple definition". Fix: `fresh_temp()` for loop variable allocas
- **Duplicate tuple destructure allocas:** Multiple `let (a, b) = ...` created duplicate `%.tuple.addr`. Fix: `fresh_temp()` for intermediate allocas
- **Option/Result method type mismatch:** `.is_none()` etc. returned `i1` but `expr_llvm_type` said `"i64"`. Fix: explicit `"i1"` return for boolean methods

### Added

- 10 new examples: `tuples.yrm`, `string_interpolation.yrm`, `option_result.yrm`, `operators.yrm`, `loop_control.yrm`, `math.yrm`, `collections.yrm`, `string_utils.yrm`, `channels.yrm`, `http_client.yrm`

**Stats:** 16 files changed, +687 -125 | Tests: 423 (46 unit + 377 integration)

---

## [1.3.1] - 2026-02-17

**Match Codegen Fixes** — [PR #15](https://github.com/MehmetMelik/yorum/pull/15)

Two pre-existing codegen bugs exposed by multiple match statements in a single function.

### Fixed

- **Duplicate match binding allocas:** Match pattern bindings used fixed names (`%v.match.addr`) instead of unique SSA names. Fix: `fresh_temp()` for match binding allocas
- **Duplicate match check labels:** Match check labels used arm-index naming (`match.check.1`) instead of globally unique labels, causing clang segfaults. Fix: pre-generate via `fresh_label()`

### Added

- 3 new examples: `maps.yrm`, `sets.yrm`, `try_operator.yrm`

**Stats:** 8 files changed, +207 -27 | Tests: 423 (46 unit + 377 integration)

---

## [1.3.0] - 2026-02-17

**Generic Collections & Error Handling Sugar** — [PR #14](https://github.com/MehmetMelik/yorum/pull/14)

Four features: match exhaustiveness checking, generic `Map<K, V>`, generic `Set<T>`, and the `?` (try) operator.

### Added

- **Match exhaustiveness checking:** `check_match_exhaustiveness()` verifies all enum variants are covered. Wildcard (`_`) and binding catch-all patterns satisfy exhaustiveness
- **Generic `Map<K, V>`:** Replaced hardcoded `Type::Map` with `Type::Generic("Map", [K, V])`. Keys restricted to hashable primitives (`int`, `string`, `char`, `bool`). Lazy LLVM IR emission per (K, V) pair via `emit_map_helpers_for_suffix()`
- **Generic `Set<T>`:** 40-byte struct `{keys, flags, cap, size, tombstones}`. Builtins: `set_new`, `set_add`, `set_has`, `set_remove`, `set_size`, `set_values`. Reuses hash functions from Map when same key type
- **`?` operator:** Postfix try for `Option<T>` and `Result<T, E>`. Compiles to tag check + branch; error path constructs None/Err and emits early `ret`
- 33 new integration tests

**Stats:** 15 files changed, +3,180 -241 | Tests: 423 (46 unit + 377 integration)

---

## [1.2.1] - 2026-02-16

**PR Review Bug Fixes** — [PR #13](https://github.com/MehmetMelik/yorum/pull/13)

Five bugs found in v1.2 review: 3 codegen issues, 1 type-safety regression, 1 parser leniency issue.

### Fixed

- `to_str` missing from `fn_ret_types` — string interpolation emitted invalid IR
- Tuple let binding assumed RHS is pointer — function calls returning tuples crashed
- Tuple type naming inconsistency between `expr_llvm_type` and `emit_tuple_lit`
- Generic argument mismatches accepted — `Option<int>` would accept `Some("hello")`
- Interpolation parser accepted trailing tokens — `"bad {1 2}"` silently dropped `2`

**Stats:** 16 files changed, +2,011 -110 | Tests: 390 (46 unit + 344 integration)

---

## [1.2.0] - 2026-02-16

**String Interpolation, Tuple Types, Option/Result** — [PR #13](https://github.com/MehmetMelik/yorum/pull/13)

### Added

- **String interpolation:** `"hello {expr}"` syntax with `{{`/`}}` escapes. Desugars in parser to `str_concat`/`to_str` chains. `to_str` accepts any type
- **Tuple types:** `(int, string)` for types, `(1, "hello")` for values, `.0`/`.1` for access, `let (a, b) = t;` for destructuring. Compile to LLVM named structs. Copy iff all elements are copy types
- **`Option<T>` & `Result<T, E>` prelude types:** Generic enums always available without declaration. Methods: `.unwrap()`, `.is_some()`, `.is_none()`, `.is_ok()`, `.is_err()`, `.unwrap_err()`. Required adding generic enum monomorphization
- `float_to_str` and `bool_to_str` builtins
- `expr_returns_ptr()` helper in codegen for distinguishing alloca pointers from values
- 23 new integration tests

### Fixed (pre-existing)

- Enum match codegen used loaded value as pointer for GEP
- Enum/struct return emitted `ret %Enum %ptr` instead of loading first
- Enum/struct let from function calls assumed RHS was always a pointer

**Stats:** (included in v1.2.1 stats) | Tests: 385 (46 unit + 339 integration)

---

## [1.1.0] - 2026-02-16

**Ergonomics & Missing Operators** — [PR #12](https://github.com/MehmetMelik/yorum/pull/12)

Four quality-of-life features. All additive — no breaking changes.

### Added

- **Compound assignment:** `+=`, `-=`, `*=`, `/=`, `%=` — desugared in parser to `x = x + e`. Works with array index and field access
- **Bitwise operators:** `&` (BitAnd), `|` (BitOr), `^` (BitXor), `<<` (Shl), `>>` (Shr). Int-only. C-style precedence. `>>` parsed as two `Gt` tokens to avoid conflict with generic closing `>>`
- **`break` and `continue`:** Loop control for `while` and `for`. Codegen uses `loop_labels` stack. For-loop `continue` correctly increments index via `for.inc` label
- **Range expressions:** `for i in 0..n` — counter-based for loop. `..` has lowest binding power so `0..n + 5` parses as `0..(n + 5)`
- 22 new integration tests

**Stats:** 16 files changed, +1,119 -75 | Tests: 362 (46 unit + 316 integration)

---

## [1.0.2] - 2026-02-16

**Codegen Bug Fixes** — [PR #11](https://github.com/MehmetMelik/yorum/pull/11)

Four correctness bugs identified by staff-engineer code review.

### Fixed

- **Stale `current_block` in `emit_function`:** PHI nodes referenced previous function's labels. Fix: reset `current_block = "entry"` alongside temp/label/terminated counters
- **HashMap tombstone infinite loop:** After insert/remove churn, all slots occupied+tombstoned with zero empty slots. Fix: added `tombstones` counter, load factor includes tombstones, grow resets tombstones
- **Spawn env under-allocation:** Computed by summing raw type sizes, ignoring alignment padding (9 bytes vs 16 needed for `{ i1, i64 }`). Fix: LLVM sizeof idiom via GEP
- **Invalid IR for unit-typed let bindings:** `alloca void` and `store void` are illegal. Fix: skip alloca/store for `Type::Unit`

**Stats:** 4 files changed, +189 -10 | Tests: 340 (46 unit + 294 integration)

---

## [1.0.1] - 2026-02-16

**Staff-Engineer Code Review Fixes** — [PR #10](https://github.com/MehmetMelik/yorum/pull/10)

17 bugs fixed from comprehensive code review across all compiler modules.

**Stats:** 13 files changed, +369 -88 | Tests: 336 (46 unit + 290 integration)

---

## [1.0.0] - 2026-02-16

**Stable Language Specification and ABI** — [PR #8](https://github.com/MehmetMelik/yorum/pull/8), [PR #9](https://github.com/MehmetMelik/yorum/pull/9)

Stabilization release — no new language features. Focus on robustness, ABI documentation, and test coverage.

### Added

- ABI appendix in `SPEC.md` — primitive layout, struct/enum/array representation, calling convention, name mangling, closure/concurrency/HashMap internals
- Dynamic versioning via `env!("CARGO_PKG_VERSION")`
- 56 new integration tests (parser errors, module system, control flow, struct/trait, example programs)
- VS Code extension bumped to 1.0.0

### Changed

- Replaced all `panic!()` in codegen with `debug_assert!` + fallback
- Replaced all `unwrap()` in LSP server with error handling

**Stats:** 11 files changed, +892 -26 | Tests: 336 (46 unit + 290 integration)

---

## [0.9.0] - 2026-02-16

**Networking** — [PR #7](https://github.com/MehmetMelik/yorum/pull/7)

14 networking builtins implemented as LLVM IR function bodies calling POSIX C library functions.

### Added

- **TCP (6):** `tcp_connect`, `tcp_listen`, `tcp_accept`, `tcp_send`, `tcp_recv`, `tcp_close`
- **UDP (4):** `udp_socket`, `udp_bind`, `udp_send_to`, `udp_recv_from`
- **DNS (1):** `dns_resolve`
- **HTTP (3):** `http_request`, `http_get`, `http_post` — HTTP/1.0, plain text only (no TLS)
- Socket FDs as `int`, `-1` for errors (POSIX convention)
- Platform-specific codegen for macOS vs Linux (`sockaddr_in` layout, `SOL_SOCKET`/`SO_REUSEADDR` constants)
- `htons` byte-swap helper emitted directly in LLVM IR
- `CONTRIBUTING.md` and `SECURITY.md`

**Stats:** 14 files changed, +1,368 -23

---

## [0.8.0] - 2026-02-16

**Ownership Checker Verification** — [PR #6](https://github.com/MehmetMelik/yorum/pull/6)

Fixed and verified the ownership checker to properly enforce SPEC.md section 8.

### Changed

- Type-aware tracking: `VarInfo { state, ty, def_span }` replaces bare `VarState`
- `is_copy_type()` classifies `int`/`float`/`bool`/`char`/`string`/`unit` as copy; everything else is move
- `check_expr_move()` marks non-copy identifiers as `Moved`
- Conservative branch merging for if/else/match: moved in any branch = moved after
- `loop_depth` prevents moving outer-scope non-copy variables inside loops
- Must-join errors for `Task` variables use real `def_span`

### Design decisions

- `string` is copy (immutable pointer at runtime)
- Function args are NOT moved (`foo(x)` does not consume `x`)
- Loop moves of outer variables always error (even if loop runs exactly once)

**Stats:** 8 files changed, +656 -78

---

## [0.7.0] - 2026-02-16

**LSP Server** — [PR #5](https://github.com/MehmetMelik/yorum/pull/5)

Built-in LSP server implementing JSON-RPC 2.0 over stdin/stdout. No new dependencies.

### Added

- `yorum lsp` command — synchronous blocking server
- **Diagnostics:** Full lex/parse/typecheck/ownership pipeline on every document change
- **Hover:** Type information for variables, parameters, functions, structs, enums
- **Go-to-definition:** Jump to variable and function definition sites
- `check_diagnostics()` and `check_with_symbols()` public API
- `SymbolTable`, `SymbolDef`, `SymbolRef`, `SymbolKind` types
- VS Code extension (`editors/vscode/`) — TextMate grammar + language client

**Stats:** 19 files changed, +1,633 -14

---

## [0.6.0] - 2026-02-16

**Standard Library Builtins** — [PR #4](https://github.com/MehmetMelik/yorum/pull/4)

All standard library functions implemented as compiler builtins (no Yorum source files).

### Added

- **Math (17 builtins):** `abs_int`, `abs_float`, `min_int`, `max_int`, `min_float`, `max_float`, `sqrt`, `pow`, `sin`, `cos`, `tan`, `floor`, `ceil`, `round`, `log`, `log10`, `exp`
- **String utilities (10):** `str_contains`, `str_index_of`, `str_starts_with`, `str_ends_with`, `str_trim`, `str_replace`, `str_split`, `str_to_upper`, `str_to_lower`, `str_repeat`
- **Collection operations (11):** `slice`, `concat_arrays`, `reverse`, `contains_int`, `contains_str`, `sort_int`, `sort_str`, `map_keys`, `map_values`, `map_size`, `map_remove`
- **Enhanced I/O (6):** `file_exists`, `file_append`, `read_line`, `print_flush`, `env_get`, `time_ms`

**Stats:** 9 files changed, +2,329 -10

---

## [0.5.0] - 2026-02-16

**Self-hosting Compiler** — [PR #3](https://github.com/MehmetMelik/yorum/pull/3)

The compiler written in Yorum itself (5,226 lines), achieving bootstrap fixed-point (gen1.ll == gen2.ll).

### Added

- `yorum-in-yorum/src/main.yrm` — self-hosting compiler implementing bootstrap subset (no generics, closures, traits, ownership, multi-file)
- Arena-based AST with flat global arrays (no `Box<T>` in Yorum)
- **Language extensions for self-hosting:**
  - `char` type (i8), char literals, type casting builtins
  - String/char ops: `str_charAt`, `str_sub`, `str_from_char`, `char_is_alpha`, `char_is_digit`, `char_is_whitespace`
  - Dynamic arrays: `push(arr, val)`, `pop(arr)` with realloc-based growth
  - File I/O: `file_read`, `file_write`, `print_err`
  - Process builtins: `args()`, `exit()`
  - HashMap: `map_new`, `map_set`, `map_get`, `map_has` (FNV-1a hash, linear probing)

**Stats:** 17 files changed, +7,600 -89

---

## [0.4.0] - 2026-02-15

**Contracts, Multi-file Compilation, Concurrency** — [PR #2](https://github.com/MehmetMelik/yorum/pull/2)

### Added

- **Runtime contracts:** `requires` (preconditions) and `ensures` (postconditions) with `result` keyword
- **`effects` clause:** Parsed and stored in AST (enforcement deferred to v1.4)
- **`pure fn`:** Compile-time purity enforcement — cannot call impure functions or use `spawn`
- **Multi-file projects:** `yorum build` with `yorum.toml` manifest, `ModuleResolver`, name prefixing
- **`yorum init`:** Project scaffolding command
- **Structured concurrency:** `spawn { block }`, `Task` type, `.join()`, must-join enforcement
- **Channels:** `chan()`, `send()`, `recv()` with pthread mutex + condvar

**Stats:** 25 files changed, +2,595 -80

---

## [0.3.0] - 2026-02-15

**Arrays, Loops, Pattern Matching** — [PR #1](https://github.com/MehmetMelik/yorum/pull/1)

### Added

- **Arrays:** `[int]` type, array literals `[1, 2, 3]`, `len(arr)`, index access `arr[i]`
- **For loops:** `for x in arr { ... }` iteration
- **While loops:** `while cond { ... }`
- **String operations:** `str_len`, `str_concat`, `str_eq`
- **Nested pattern matching:** `match` with struct/enum destructuring

**Stats:** 15 files changed, +1,509 -258

---

## [0.2.0] - 2026-02-15

**Type System & Polymorphism**

### Added

- **Impl blocks:** `impl Type { fn method(...) { ... } }`
- **Traits:** `trait Name { fn method(...); }` with `impl Trait for Type`
- **Generics:** `fn identity<T>(x: T) -> T`, generic structs, monomorphization
- **Closures:** `|x: int| -> int { x + 1 }`, environment capture, compiled to `{ fn_ptr, env_ptr }` pairs

**Stats:** 20 files changed, +2,691 -161

---

## [0.1.0] - 2026-02-15

**Initial Release**

### Added

- Lexer, parser, typechecker, codegen pipeline
- Primitive types: `int`, `float`, `bool`, `string`, `unit`
- Functions, structs, enums (tagged unions)
- `if`/`else` expressions
- `match` expressions
- `let` bindings with explicit type annotations
- Logical operators: `and`, `or`, `not`
- Comparison and arithmetic operators
- Print builtins: `print_int`, `print_float`, `print_bool`, `print_str`
- LLVM IR text output (no LLVM library dependency)
- CI/CD workflows, MIT license

**Stats:** 13 files changed, +571 -107

---

[1.12.1]: https://github.com/MehmetMelik/yorum/compare/v1.12.0...v1.12.1
[1.12.0]: https://github.com/MehmetMelik/yorum/compare/v1.11.0...v1.12.0
[1.11.0]: https://github.com/MehmetMelik/yorum/compare/v1.10.0...v1.11.0
[1.10.0]: https://github.com/MehmetMelik/yorum/compare/v1.9.1...v1.10.0
[1.9.1]: https://github.com/MehmetMelik/yorum/compare/v1.9.0...v1.9.1
[1.9.0]: https://github.com/MehmetMelik/yorum/compare/v1.9.0-beta...v1.9.0
[1.9.0-beta]: https://github.com/MehmetMelik/yorum/compare/v1.9.0-alpha...v1.9.0-beta
[1.9.0-alpha]: https://github.com/MehmetMelik/yorum/compare/v1.8.2...v1.9.0-alpha
[1.8.2]: https://github.com/MehmetMelik/yorum/compare/v1.8.1...v1.8.2
[1.8.1]: https://github.com/MehmetMelik/yorum/compare/v1.8.0...v1.8.1
[1.8.0]: https://github.com/MehmetMelik/yorum/compare/v1.7.0...v1.8.0
[1.7.0]: https://github.com/MehmetMelik/yorum/compare/v1.6.1...v1.7.0
[1.6.1]: https://github.com/MehmetMelik/yorum/compare/v1.6.0...v1.6.1
[1.6.0]: https://github.com/MehmetMelik/yorum/compare/v1.5.0...v1.6.0
[1.5.0]: https://github.com/MehmetMelik/yorum/compare/v1.4.1...v1.5.0
[1.4.1]: https://github.com/MehmetMelik/yorum/compare/v1.4.0...v1.4.1
[1.4.0]: https://github.com/MehmetMelik/yorum/compare/v1.3.2...v1.4.0
[1.3.2]: https://github.com/MehmetMelik/yorum/compare/v1.3.1...v1.3.2
[1.3.1]: https://github.com/MehmetMelik/yorum/compare/v1.3.0...v1.3.1
[1.3.0]: https://github.com/MehmetMelik/yorum/compare/v1.2.1...v1.3.0
[1.2.1]: https://github.com/MehmetMelik/yorum/compare/v1.1.0...v1.2.1
[1.2.0]: https://github.com/MehmetMelik/yorum/compare/v1.1.0...v1.2.1
[1.1.0]: https://github.com/MehmetMelik/yorum/compare/v1.0.2...v1.1.0
[1.0.2]: https://github.com/MehmetMelik/yorum/compare/v1.0.1...v1.0.2
[1.0.1]: https://github.com/MehmetMelik/yorum/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/MehmetMelik/yorum/compare/v0.9.0...v1.0.0
[0.9.0]: https://github.com/MehmetMelik/yorum/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/MehmetMelik/yorum/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/MehmetMelik/yorum/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/MehmetMelik/yorum/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/MehmetMelik/yorum/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/MehmetMelik/yorum/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/MehmetMelik/yorum/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/MehmetMelik/yorum/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/MehmetMelik/yorum/releases/tag/v0.1.0
