# Changelog

All notable changes to the Yorum programming language compiler.

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

**Stats:** 7 files changed, ~1,100 lines added | Tests: 498 (50 unit + 448 integration)

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
