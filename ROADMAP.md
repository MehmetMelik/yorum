# Yorum Roadmap

Post-v1.0 development plan, organized by themes and priorities.

## v1.1 — Ergonomics & Missing Operators (Done)

The language is functionally complete but has daily-use paper cuts.

- ~~**Compound assignment operators**: `+=`, `-=`, `*=`, `/=`, `%=` — desugar to `x = x + y` in the parser~~
- ~~**`break` and `continue`** for `while` and `for` loops — currently impossible to exit a loop early~~
- ~~**Range expressions**: `for i in 0..n { }` — avoids the manual `while` + counter pattern~~
- ~~**Bitwise operators**: `&`, `|`, `^`, `<<`, `>>` — needed for systems-level and hashing code~~
- **String interpolation** — deferred to v1.2

---

## v1.2 — String Interpolation, Tuple Types, Option/Result (Done)

- ~~**String interpolation**: `"hello {name}"` — desugars to `str_concat`/`to_str` chains in the parser~~
- ~~**Tuple types**: `(int, string)` — anonymous product types with `.0` access and destructuring~~
- ~~**Standard `Option<T>` and `Result<T, E>`** — prelude generic enums with monomorphization and methods~~
- **`Map<K, V>`** and **`Set<T>`** — deferred to v1.3

---

## v1.3 — Generic Collections & Error Handling Sugar (Done)

- ~~**`Map<K, V>`** — generic hash map with keys restricted to hashable primitives (`int`, `string`, `char`, `bool`). Bare `Map` still works as `Map<string, int>` for backward compat~~
- ~~**`Set<T>`** — generic hash set with T restricted to hashable primitives. 40-byte struct layout (no value array)~~
- ~~**`?` operator** — postfix try operator, propagates `None`/`Err` up the call stack. Works in `Option`-returning and `Result`-returning functions with matching error types~~
- ~~**`match` exhaustiveness checking** — error when not all enum variants are covered (wildcard and binding catch-alls accepted)~~

---

## v1.4 — Effect System Enforcement (Done)

- ~~**Effect tracking**: 6 categories (`io`, `fs`, `net`, `time`, `env`, `concurrency`) — every builtin annotated~~
- ~~**Effect propagation**: calling an effectful function requires declaring the effect in the caller~~
- ~~**Effect inference**: fixed-point call graph iteration infers effects for unannotated functions~~
- ~~**Backward compatible**: functions without `effects` clause remain unchecked; `main` always unchecked~~

---

## v1.5 — Tooling & Developer Experience (Done)

- ~~**`yorum run`** — compile + link + execute in one command (auto-detects clang, auto-links pthread)~~
- ~~**LSP completions** — autocomplete for identifiers, struct fields, methods, builtins, keywords~~
- ~~**LSP code actions** — quick fixes: "Did you mean X?" (Levenshtein), effect clause hints, missing match arms~~
- ~~**REPL** (`yorum repl`) — interactive expression evaluation via compile-link-execute loop, definition accumulation, `:type` command~~
- ~~**Debug info** — DWARF metadata in LLVM IR (`-g` flag) so `lldb`/`gdb` support source-level stepping~~
Rationale: The gap between "language features" and "usable language" is mostly tooling.

---

## v1.6 — Auto-Formatter (Done)

- ~~**`yorum fmt`** — auto-formatter using hybrid approach: lexer comment collection + AST walk + source extraction for desugared constructs~~
- ~~**`--check` flag** — CI integration (exit 1 if any file needs formatting)~~
- ~~**Comment preservation** — line and block comments collected via lexer side-channel, reattached with blank-line-aware spacing~~
- ~~**Round-trip safety** — string interpolation, compound assignments, and brace escaping preserved through format cycle~~

Rationale: Deferred from v1.5 because the lexer discarded comments. Solved with a hybrid approach — modified lexer optionally collects comments into a side-channel, then the formatter walks the AST for structure while using source extraction for desugared constructs.

---

## v1.7 — Performance & Optimization (Done)

Currently the compiler emits unoptimized alloca/load/store IR and relies entirely on LLVM's mem2reg.

- ~~**Tail call optimization** — annotate tail calls with `tail call` hint in LLVM IR for tail-recursive returns~~
- ~~**Constant folding** — evaluate pure constant expressions (int/bool arithmetic, comparisons, logical ops) at compile time~~
- ~~**Dead code elimination** — BFS reachability from `main`, removes unused functions/structs/enums before codegen~~
- ~~**Inline hint annotation** — small pure functions (≤3 stmts, no contracts) get `alwaysinline` attribute~~
- ~~**Sort algorithm upgrade** — replace O(n²) insertion sort with O(n log n) heap sort for `sort_int`/`sort_str`~~

Rationale: Performance doesn't matter until people write real programs, but these are low-hanging fruit.

---

## v1.8 — Package Manager (Done)

- ~~**`yorum.toml` dependencies section** — declare deps with git URLs or local filesystem paths~~
- ~~**`yorum install`** — fetch and cache dependencies, write `yorum.lock`~~
- ~~**`yorum update [name]`** — fetch latest, regenerate lock file~~
- ~~**Namespace isolation** — dep symbols prefixed with `<dep_name>__` to prevent collisions~~
- ~~**Lock file** — `yorum.lock` records exact git SHAs for reproducible builds~~
- ~~**Package cache** — git deps cached in `~/.yorum/cache/`~~

Rationale: No language ecosystem grows without a package manager. Prerequisite for community adoption.

---

## v1.9 — Iterators & Functional Patterns (Done)

- ~~**Iterator trait/protocol** — `for x in expr` works with any type implementing `Iterator`~~
- ~~**Lazy iterator combinators** — `map`, `filter`, `fold`, `take`, `skip`, `zip`, `enumerate`, `chain`~~
- ~~**Range types as iterators** — `0..n`, `0..=n`~~
- ~~**`collect()`** — materialize an iterator into an array~~

---

## v1.10 — Codegen Refactor (Done)

- ~~**Fat pointer helpers:** extracted reusable `emit_fat_ptr_load`/`init`/`field_load`/`field_store`~~
- ~~**Struct field helpers:** `emit_struct_gep`, `emit_struct_field_load`, `emit_struct_field_store`~~
- ~~**Module extraction:** split monolithic `codegen.rs` (9,892 lines) into directory module with 5 files~~

---

## v1.11 — Array Repeat Syntax & Bounds Check Elision (Done)

- ~~**Array repeat syntax:** `[value; count]` — bulk array allocation with memset fast path for zero values and fill loop for non-zero/aggregate values~~
- ~~**Bounds check elision:** compiler skips `@__yorum_bounds_check` for `arr[i]` inside `for i in 0..len(arr)` loops when the body doesn't mutate the array~~
- ~~**LSP improvements:** symbol tracking, type-aware dot completions, keyword fix~~

---

## Longer-term (v2.0+)

| Feature | Notes |
|---|---|
| **Async/await** | Cooperative concurrency alongside thread-based `spawn`. Needs an event loop runtime |
| **WebAssembly target** | LLVM can emit wasm, but C-library-dependent builtins need wasm-compatible alternatives |
| **TLS/HTTPS** | Requires linking a TLS library. Breaks the "no C runtime" purity |
| **Algebraic effects** | The `effects` system could evolve beyond tracking into actual effect handlers |
| **Borrow checker** | Full lifetime tracking (Rust-style). Currently `&T` exists but borrows aren't deeply enforced |
| **Cross-compilation** | Emit LLVM IR for different targets. Platform-specific logic already exists |
| **Self-hosted compiler parity** | Bring Yorum-in-Yorum up to full-feature parity (generics, closures, traits, multi-file) |
| **JSON/Regex builtins** | Commonly needed for scripting and web use cases |

---

## Priority Recommendation

The top 3 highest-impact releases:

1. ~~**v1.1 (Ergonomics)** — removes friction that makes users bounce immediately~~ **Done**
2. ~~**v1.2 (String Interpolation, Tuples, Option/Result)** — unblocks real-world programs~~ **Done**
3. ~~**v1.3 (Generic Collections & Error Sugar)** — `Map<K,V>`, `Set<T>`, `?` operator~~ **Done**
4. ~~**v1.4 (Effect System)** — the unique differentiator that no other language has done well~~ **Done**
5. ~~**v1.5 (Tooling)** — `yorum run`, `yorum repl`, LSP completions/code actions, debug info~~ **Done**
6. ~~**v1.6 (Formatter)** — `yorum fmt` auto-formatter with comment preservation~~ **Done**
7. ~~**v1.7 (Performance)** — tail call optimization, constant folding, dead code elimination, inline hints, heap sort~~ **Done**
8. ~~**v1.8 (Package Manager)** — `yorum install`, `yorum update`, git + path dependencies, lock file, namespace isolation~~ **Done**
9. ~~**v1.9 (Iterators)** — iterator combinators, terminators, range pipeline sources, fused codegen~~ **Done**
10. ~~**v1.10 (Codegen Refactor)** — fat pointer/struct helpers, pipeline deduplication, module extraction~~ **Done**
11. ~~**v1.11 (Optimizations)** — array repeat `[value; count]`, bounds check elision~~ **Done**
