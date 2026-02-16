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

## v1.2 — String Interpolation & Generic Collections

- **String interpolation**: `"hello {name}"` or a `format()` builtin — `str_concat` chains are painful
- **`Map<K, V>`** — generic hash map. Keys need a hash/equality constraint (either built-in for primitive types, or a `Hashable` trait)
- **`Set<T>`** — built on top of `Map<T, unit>` or similar
- **Tuple types**: `(int, string)` — anonymous product types for multi-return and lightweight grouping
- **Standard `Option<T>` and `Result<T, E>`** — ship as prelude types (always available, no import needed)

Rationale: String interpolation is the last remaining ergonomic paper cut. Generic collections unblock real-world data structures — the current `Map` severely limits what can be built.

---

## v1.3 — Error Handling Sugar

With `Option<T>` and `Result<T, E>` in place, add ergonomic error propagation.

- **`?` operator** or `try` expression — propagates `Err`/`None` up the call stack without manual match boilerplate
- **`unwrap()` method** on `Option`/`Result` — aborts with a message on failure (useful for prototyping)
- **`match` exhaustiveness checking** — warn or error when not all variants are covered

Rationale: Yorum's error-as-values philosophy is right, but without `?` it's extremely verbose. This is the #1 thing that would make the language pleasant to use at scale.

---

## v1.4 — Effect System Enforcement

The `effects` clause is parsed but not enforced. This is Yorum's most unique design element.

- **Effect tracking**: the compiler tracks which effects each function uses (`io`, `net`, `fs`, `time`, etc.)
- **Effect propagation**: calling an effectful function requires declaring the effect in the caller
- **Effect polymorphism**: generic functions that are agnostic about effects
- **Effect inference** (optional): infer effects for non-`pub` functions, require explicit annotation for `pub` functions

Rationale: The "killer feature" for LLM-first development. An AI agent could look at `effects io, net` and know exactly what a function can touch, making automated refactoring provably safe.

---

## v1.5 — Tooling & Developer Experience

- **`yorum fmt`** — auto-formatter (the AST already has spans, so reconstruction is feasible)
- **LSP completions** — autocomplete for identifiers, struct fields, methods, builtins
- **LSP code actions** — quick fixes for common errors (missing import, wrong type, unused variable)
- **REPL** (`yorum repl`) — JIT-compile expressions via a small wrapper
- **`yorum run`** — compile + link + execute in one command (pipes through clang automatically)
- **Debug info** — emit DWARF metadata in LLVM IR so `lldb`/`gdb` work with source-level stepping

Rationale: The gap between "language features" and "usable language" is mostly tooling. `yorum fmt` alone would double productivity.

---

## v1.6 — Performance & Optimization

Currently the compiler emits unoptimized alloca/load/store IR and relies entirely on LLVM's mem2reg.

- **Tail call optimization** — annotate tail calls with `musttail` in LLVM IR (critical for recursive Yorum code)
- **Constant folding** — evaluate pure constant expressions at compile time
- **Dead code elimination** — don't emit unused functions/structs
- **Inline hint annotation** — small pure functions get `alwaysinline`
- **Sort algorithm upgrade** — replace insertion sort (`sort_int`/`sort_str`) with O(n log n)

Rationale: Performance doesn't matter until people write real programs, but these are low-hanging fruit.

---

## v1.7 — Package Manager

- **`yorum.toml` dependencies section** — declare deps with git URLs or a registry
- **`yorum install`** — fetch and cache dependencies
- **`yorum publish`** — publish to a package registry
- **Namespace isolation** — prevent name collisions between packages
- **Semver resolution** — basic version constraint solving

Rationale: No language ecosystem grows without a package manager. Prerequisite for community adoption.

---

## v1.8 — Iterators & Functional Patterns

- **Iterator trait/protocol** — `for x in expr` works with any type implementing `Iterator`
- **Lazy iterator combinators** — `map`, `filter`, `fold`, `take`, `skip`, `zip`, `enumerate`, `chain`
- **Range types as iterators** — `0..n`, `0..=n`
- **`collect()`** — materialize an iterator into an array

Rationale: Natural evolution of the `for` loop and array builtins. Lazy evaluation prevents unnecessary allocations.

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
2. **v1.2 (String Interpolation & Generic Collections)** — unblocks building anything non-trivial
3. **v1.4 (Effect System)** — the unique differentiator that no other language has done well

The collections work is "catch-up" to be competitive. The effect system is where Yorum can lead.
