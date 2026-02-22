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

- ~~**Structural iterator pipelines** — `.iter()` recognized by compiler for arrays, ranges, Sets, Maps; fused into single LLVM loops with no iterator structs~~
- ~~**Lazy iterator combinators** — `map`, `filter`, `fold`, `take`, `skip`, `zip`, `enumerate`, `chain`~~
- ~~**Range types as iterators** — `0..n`, `0..=n`~~
- ~~**`collect()`** — materialize an iterator into an array~~
- **User-definable Iterator trait/protocol** — not implemented; see Longer-term section

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

## v1.12 — Iterator Ecosystem (Done)

- ~~**`.chain(arr)` combinator:** concatenates two iterables in a fused pipeline, conditional source selection via `phi` nodes~~
- ~~**`.flat_map(f)` and `.flatten()` combinators:** map-and-flatten and flatten nested arrays into single stream~~
- ~~**`.take_while(f)` combinator:** yield elements while predicate holds, then exit~~
- ~~**`.chars()` source:** iterate over string characters as `char` values~~
- ~~**`.rev()` combinator:** reverse iteration order for arrays~~
- ~~**`.sum()`, `.count()`, `.position(f)` terminators:** arithmetic sum, element count, and index-of-first-match~~
- ~~**`.clear()` method:** empty an array (set length to 0, preserve capacity)~~
- ~~**Unbounded ranges:** `(start..).iter()` with compile-time enforcement of `.take(n)` or `.take_while(f)`~~
- ~~**Set/Map iteration:** `set.iter()` yields elements, `map.iter()` yields `(K, V)` tuples~~
- ~~**Codegen hardening:** SIGBUS crash fix for Set/Map return-by-value, flat-map pipeline fixes, array-of-arrays lifetime fix, tuple allocation size fix~~

---

## v1.12.1 — LSP Chain-Aware Completions (Done)

- ~~**Chain-aware dot-completions:** `.` after any iterator pipeline chain shows correct combinators/terminators with typed signatures derived by walking the AST~~
- ~~**Type propagation through pipelines:** element types tracked through `.map()` (closure return type), `.enumerate()` (wraps in tuple), `.zip()` (pairs types), `.filter()` (preserves type), `.collect()` (returns array), etc.~~
- ~~**Non-iterator chain results:** `.collect().` shows array methods, enabling accurate completions after terminators~~
- ~~**LSP dot-completion fixes:** generic types from v1.12.0 (Set, Map, Task, Chan, Result) now resolve correctly~~

---

## v1.12.2 — While-Loop Bounds Check Elision (Done)

- ~~**While-loop elision:** `while j < len(arr)` and `while j < n` (where `n` aliases `len(arr)`) now elide bounds checks for `arr[j]` when accesses provably precede index modifications~~
- ~~**Len-alias tracking:** immutable bindings from `let n = len(arr)` and `let arr = [val; n]` tracked as length aliases for elision in while-loop conditions~~
- ~~**Safety analysis:** access-before-modification analysis, bound immutability checks, and array mutation guards ensure elision is only applied when provably safe~~

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
| **Iterator trait/protocol** | User-definable `Iterator` trait so custom types can be iterable. See analysis below |
| **UTF-8 / Unicode support** | Upgrade `char` from `i8` to `i32` (codepoints). See analysis below |

### Iterator Trait/Protocol — Analysis

Currently, iterator pipelines are **structural**: the compiler recognizes `.iter()` on built-in types (arrays, ranges, Sets, Maps) and `.chars()` on strings, then fuses the entire pipeline into a single LLVM loop. There are no iterator structs, no `next()` method, no vtables.

**Pros of adding Iterator trait:**
- Custom types become iterable in for-loops (e.g., `for node in tree { ... }`)
- User-defined data structures can participate in the full pipeline ecosystem (map, filter, collect, etc.)
- Closer to Rust/Python/Java convention — familiar mental model for users
- Enables library authors to provide iterable APIs

**Cons of adding Iterator trait:**
- **Performance regression:** the current approach emits a single fused loop with zero overhead. A trait-based `next()` protocol requires either virtual dispatch (vtables, indirect calls) or monomorphization of every pipeline combination — both are slower or more complex than structural fusion
- **Complexity explosion:** the codegen currently pattern-matches pipeline shapes at compile time. A trait-based system would need to compose arbitrary combinator chains generically, requiring either a lazy iterator state machine (like Rust) or runtime dispatch
- **Closures can't implement traits:** Yorum closures are `{ fn_ptr, env_ptr }` pairs with no trait impl mechanism. Adapting combinators like `.map(f)` to work on trait-based iterators would require either trait objects or a different closure model
- **Limited benefit today:** the 6 built-in sources (array, range, unbounded range, string, Set, Map) cover the vast majority of iteration use cases. Custom iterables are rare in practice
- **Breaks the "no hidden behavior" principle:** an Iterator trait with `next()` introduces hidden state mutation on each call, which goes against Yorum's deterministic/explicit design

**Recommendation:** keep the structural approach. If user-defined iteration is needed, a simpler alternative is allowing `impl` blocks to define a `to_array()` or `iter()` method that returns `[T]`, which then participates in the existing pipeline system without adding trait-based iteration machinery.

### UTF-8 / Unicode — Analysis

Currently, `char` is `i8` (1 byte, ASCII). All string APIs operate on bytes: `str_len` calls `strlen`, `str_charAt` returns a byte, `.chars()` iterates bytes. This is consistent and unambiguous but breaks on multi-byte UTF-8 input (e.g., `"café".chars()` yields 5 bytes, not 4 characters).

**Rejected approach: dual-type system (`char` + `rune`)**

Adding a `rune` type (`i32`) alongside `char` was considered and rejected. It would create a "which one do I use?" decision at every string operation — exactly the kind of ambiguity Yorum is designed to eliminate. Precedent is negative: Go's `byte`/`rune` split is a common source of bugs, Python 2's `str`/`unicode` was bad enough to justify a breaking Python 3 migration. An LLM generating code would have to choose between two APIs where one silently produces wrong results on non-ASCII input. This violates the "one obvious way" principle.

**Recommended approach: upgrade `char` to `i32` (v2.0)**

Make `char` a 32-bit Unicode codepoint in a major version bump. One type, one set of APIs, no ambiguity. `.chars()` decodes UTF-8 automatically. `str_len` returns codepoint count. Every program handles Unicode correctly by default. This is a breaking change affecting:
- ABI (`char` from `i8` to `i32`)
- All char builtins (`char_is_alpha`, `char_to_int`, etc.)
- All string builtins (`str_charAt`, `str_sub`, `str_len` become O(n) for indexed access)
- Self-hosted compiler (5,226 lines using `char` for lexing — works because source is ASCII, but type size changes)

**Current status:** Yorum is an ASCII language. This is documented and consistent. UTF-8 support is deferred to v2.0 as a clean breaking change rather than adding a parallel type system that undermines LLM friendliness

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
12. ~~**v1.12 (Iterator Ecosystem)** — `.chain()`, `.flat_map()`, `.flatten()`, `.take_while()`, `.chars()`, `.rev()`, `.sum()`, `.count()`, `.position()`, unbounded ranges, Set/Map `.iter()`~~ **Done**
13. ~~**v1.12.1 (LSP Chain Completions)** — chain-aware dot-completions with type propagation through iterator pipelines~~ **Done**
14. ~~**v1.12.2 (While-Loop Bounds Elision)** — while-loop bounds check elision with len-alias tracking and access-before-modification analysis~~ **Done**
