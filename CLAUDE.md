# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands

```bash
cargo build                          # dev build
cargo build --release                # release build
cargo test                           # all tests (423: 46 unit + 377 integration)
cargo test compiler::lexer           # tests in one module
cargo test test_fibonacci_compiles   # single test by name
cargo test -- --nocapture            # see stdout from tests
```

### Running the compiler

```bash
cargo run -- compile file.yrm              # emit LLVM IR to stdout
cargo run -- compile file.yrm -o out.ll    # emit LLVM IR to file
cargo run -- check file.yrm                # type-check only
cargo run -- ast file.yrm                  # dump AST as JSON
cargo run -- build                         # build multi-file project (needs yorum.toml)
cargo run -- build -o out.ll               # build project to file
cargo run -- init myproject                # scaffold new project
cargo run -- lsp                          # start LSP server (stdin/stdout)
```

### Producing native binaries (requires clang)

```bash
clang -x ir out.ll -o binary -Wno-override-module
clang -x ir out.ll -o binary -lpthread -Wno-override-module   # if using spawn/channels
```

## Architecture

This is a compiler for the Yorum language — an LLM-first, statically typed language that emits textual LLVM IR. No LLVM library dependency; the compiler is pure Rust with only `serde`/`serde_json`/`toml`. The compiler is **self-hosting** — `yorum-in-yorum/src/main.yrm` is the compiler written in Yorum itself (5,226 lines), achieving bootstrap fixed-point.

### Compilation pipeline

The pipeline is strictly sequential — each phase consumes the output of the previous one. Single-file compilation is orchestrated in `src/lib.rs`, multi-file in `src/compiler/project.rs`:

```
Source → Lexer → Vec<Token> → Parser → AST (Program) → TypeChecker → OwnershipChecker → Monomorphizer → Codegen → LLVM IR string
```

Multi-file projects (`yorum build`) add a front-end step: `ModuleResolver` discovers all `.yrm` files, parses each, then `project.rs` merges them into a single `Program` (prefixing imported names) before the standard pipeline runs.

### Key modules (in `src/compiler/`)

- **`span.rs`** — `Span` type (byte offsets + line/col) carried by every token and AST node
- **`token.rs`** — `TokenKind` enum; `keyword_from_str()` maps identifiers to keyword tokens
- **`lexer.rs`** — Hand-written char-by-char lexer. Supports nested `/* */` block comments. Returns `Vec<Token>`
- **`ast.rs`** — Complete AST types. All nodes derive `Serialize`/`Deserialize` for JSON round-tripping. `Type` enum has `PartialEq`/`Eq` for type checking comparisons
- **`parser.rs`** — Recursive descent parser with Pratt expression parsing (`parse_expr_bp`). Operator precedence is defined in `infix_binding_power()`. Struct init is disambiguated from blocks via 2-token lookahead in `is_struct_init_lookahead()`
- **`typechecker.rs`** — Two-pass: first registers all function signatures, struct layouts, and enum definitions; then checks function bodies. Uses a scope stack (`Vec<HashMap<String, VarInfo>>`) for lexical scoping. Built-in functions registered in `register_builtins()`: print (`print_int`, `print_float`, `print_bool`, `print_str`, `print_char`, `print_err`), string ops (`str_len`, `str_concat`, `str_eq`, `str_charAt`, `str_sub`, `str_from_char`), char classification (`char_is_alpha`, `char_is_digit`, `char_is_whitespace`), type casting (`int_to_str`, `str_to_int`, `char_to_int`, `int_to_char`, `int_to_float`, `float_to_int`), string conversion (`float_to_str`, `bool_to_str`), file I/O (`file_read`, `file_write`, `file_exists`, `file_append`), math (`abs_int`, `abs_float`, `min_int`, `max_int`, `min_float`, `max_float`, `sqrt`, `pow`, `sin`, `cos`, `tan`, `floor`, `ceil`, `round`, `log`, `log10`, `exp`), string utilities (`str_contains`, `str_index_of`, `str_starts_with`, `str_ends_with`, `str_trim`, `str_replace`, `str_to_upper`, `str_to_lower`, `str_repeat`), collections (`contains_int`, `contains_str`, `sort_int`, `sort_str`), enhanced I/O (`read_line`, `print_flush`, `env_get`, `time_ms`), networking — TCP (`tcp_connect`, `tcp_listen`, `tcp_accept`, `tcp_send`, `tcp_recv`, `tcp_close`), UDP (`udp_socket`, `udp_bind`, `udp_send_to`, `udp_recv_from`), DNS (`dns_resolve`), HTTP (`http_request`, `http_get`, `http_post`). `len()`, `push()`, `pop()`, `args()`, `exit()`, `chan()`, `send()`, `recv()`, `slice()`, `concat_arrays()`, `reverse()`, `str_split()`, `to_str()` are special-cased in Call handling. Generic `Map<K, V>` builtins (`map_new`, `map_set`, `map_get`, `map_has`, `map_size`, `map_remove`, `map_keys`, `map_values`) and `Set<T>` builtins (`set_new`, `set_add`, `set_has`, `set_remove`, `set_size`, `set_values`) are special-cased in Call handling with key type validation (hashable primitives only). `spawn` type inference and `.join()` method handling are in `infer_expr`. Prelude types `Option<T>` and `Result<T, E>` are registered as generic enums with methods (`.unwrap()`, `.is_some()`, `.is_none()`, `.is_ok()`, `.is_err()`, `.unwrap_err()`). Contract expressions are type-checked (must be `bool`). Purity enforcement: pure functions cannot call impure functions or use `spawn`
- **`ownership.rs`** — Type-aware move checker. Tracks `VarInfo { state, ty, def_span }` per variable with `is_copy_type()` distinguishing copy types (`int`, `float`, `bool`, `char`, `string`, `unit`) from move types (structs, enums, arrays, etc.). `check_expr_move()` marks non-copy identifiers as `Moved`; `check_expr_use()` checks read-only contexts. Branch merging (`snapshot_states`/`restore_states`/`apply_merged_states`) for if/else and match ensures moves in any branch propagate conservatively. `loop_depth` tracking prevents moving outer-scope variables inside while/for loops. Enforces must-join for `Task` variables via `task_vars: Vec<HashSet<String>>` with real `def_span` on errors
- **`monomorphize.rs`** — Eliminates generics before codegen. Collects all concrete instantiations of generic functions/structs/enums, clones declarations with type variables substituted, and rewrites call sites to use mangled names (`identity__int`, `Pair__int__float`). Also substitutes type vars in contract expressions. Handles generic enum monomorphization for prelude types (`Option__int`, `Result__int__string`) via `generic_enums`/`enum_instantiations` tracking
- **`codegen.rs`** — Emits textual LLVM IR. Uses alloca/load/store pattern (LLVM's mem2reg promotes to SSA). Each expression emitter returns the LLVM SSA value name (e.g., `%t3`). Struct-typed, enum-typed, and unit-typed let bindings are special-cased (unit skips alloca/store entirely since `alloca void` is invalid IR). Closures compile to `{ ptr, ptr }` pairs (fn_ptr + env_ptr) with deferred function emission. Arrays are heap-allocated fat pointers `{ ptr, i64, i64 }` (data, length, capacity) with runtime bounds checking and `realloc`-based growth for `push`. Aggregate types (structs, enums) in arrays use `memcpy` for push/pop/index/array-lit operations. `requires`/`ensures` emit conditional branches to `@__yorum_contract_fail`. `spawn` compiles to `pthread_create` with env capture (same pattern as closures); env struct size is computed via LLVM sizeof idiom (`getelementptr %type, ptr null, i32 1` + `ptrtoint`) to account for alignment padding. Channel helpers are emitted as LLVM IR functions using mutex/condvar. Generic `Map<K, V>` helpers are lazily emitted per monomorphized (K, V) pair via `emit_map_helpers_for_suffix()` — includes hash function, find_slot, grow, and all 8 map builtins parameterized by key/value LLVM types. `Set<T>` helpers emitted via `emit_set_helpers_for_suffix()`, reusing hash functions from Map when same key type. Map struct is 48 bytes `{keys, vals, flags, cap, size, tombstones}`, Set is 40 bytes `{keys, flags, cap, size, tombstones}`. The `?` (try) operator compiles to tag check + branch: success extracts payload, error constructs None/Err and emits early `ret`
- **`module_resolver.rs`** — Discovers `.yrm` files under `src/`, maps filesystem paths to module names, parses each file, validates `module` declarations match paths
- **`project.rs`** — Orchestrates multi-file compilation: reads `yorum.toml` manifest, resolves modules, merges `pub` declarations with name prefixing (`math__add`), rewrites call sites, runs standard pipeline

### Important codegen details

- `fresh_temp()` and `fresh_label()` generate unique SSA names and labels per function
- `emit_function` resets `temp_counter`, `label_counter`, `block_terminated`, and `current_block` to `"entry"` — all four must be reset to avoid cross-function label leakage in PHI nodes
- `block_terminated` flag tracks whether a `ret`/`br` has been emitted — prevents emitting instructions after a terminator
- Built-in print functions are emitted directly as LLVM IR definitions (not linked from a C runtime)
- Format strings for printf are global constants with manually counted byte sizes — update sizes if changing format strings
- Structs map to LLVM struct types (`%Name = type { ... }`), accessed via `getelementptr`
- Enums without data are `{ i32 }`; enums with data are `{ i32, [N x i8] }` tagged unions. Enum variant constructors (e.g., `Some(42)`) are detected in Call handling and emit alloca + tag + payload store
- Arrays use reference semantics in codegen: `emit_expr` on an array Ident returns the alloca pointer directly (not a load), and `expr_llvm_type` returns `"ptr"` for array idents. Array function parameters are passed as `ptr` (opaque pointer to `{ ptr, i64, i64 }`)
- `array_elem_types: HashMap<String, String>` tracks the LLVM element type per array variable, used by Index, For, push, pop, and assignment codegen
- `push(arr, val)` checks if `len == cap`, doubles capacity via `realloc` when needed, then stores the element. For aggregate types (structs, enums), uses `memcpy` instead of `store`
- `pop(arr)` checks `len > 0`, decrements length, loads element (or `memcpy` for aggregates). Aborts on empty array
- `type_size(&str)` computes actual LLVM type sizes by summing struct field sizes recursively — needed for `memcpy` of aggregate array elements
- String builtins (`str_len`, `str_concat`, `str_eq`) are emitted as LLVM IR function bodies that call C library functions (`strlen`, `strcpy`, `strcat`, `strcmp`, `malloc`)
- Contract checks (`requires`/`ensures`): `requires` emits conditional branches at function entry after allocas; `ensures` wraps each `ret` — stores return value to `result` alloca, checks condition, reloads and returns. Failures call `@__yorum_contract_fail` (prints message, aborts)
- Spawn uses the closure capture pattern: env struct with captured vars + result slot, wrapper function `@__spawn_N`, `pthread_create` to launch. Env struct size is computed via LLVM sizeof idiom (`getelementptr %type, ptr null, i32 1` + `ptrtoint`) to account for alignment padding — do not use raw `llvm_type_size` sums. Task control block is `{ pthread_t, env_ptr }`. `.join()` calls `pthread_join`
- Channel helpers (`@__yorum_chan_create`, `@__yorum_chan_send`, `@__yorum_chan_recv`) are emitted as LLVM IR functions using pthread mutex + condvar. `spawn_counter` tracks unique spawn IDs for wrapper function naming
- Unit-typed let bindings and assignments skip alloca/store — `alloca void` and `store void` are invalid LLVM IR. The RHS is still evaluated for side effects

### Yorum language design choices

- Semicolons terminate statements; `{}` delimit blocks
- Logical operators are keywords: `and`, `or`, `not` (no `&&`/`||`/`!`)
- All let bindings require explicit type annotation: `let x: int = 5;`
- Functions have `requires`/`ensures`/`effects` contract clauses — `requires` and `ensures` are verified at runtime (abort on violation); `effects` is parsed but not yet enforced
- `pure` marks side-effect-free functions — enforced at compile time (cannot call impure functions or use `spawn`)
- `spawn { block }` creates concurrent tasks; `Task` variables must be `.join()`'d before scope exit (enforced by ownership checker)
- Reserved words cannot be used as identifiers (including module names) — `effects`, `requires`, etc. are keywords

### Test organization

- **Unit tests**: inline `#[cfg(test)] mod tests` in each compiler module
- **Integration tests**: `tests/integration_tests.rs` — uses the public API (`yorum::compile_to_ir`, `yorum::typecheck`, `yorum::source_to_ast_json`)
- Integration test helpers: `compile()` for IR output assertions, `parse_and_check()` for type-check-only, `parse_to_json()` for AST structure assertions
- Multi-file project tests use `yorum::compile_project()` with temp directories containing `yorum.toml` + `.yrm` files

### Example programs

`examples/*.yrm` — all compile to native binaries and run correctly. Use these as references for valid Yorum syntax.

## Completed: v0.5 — Self-hosting Compiler

The self-hosting compiler is complete. `yorum-in-yorum/src/main.yrm` (5,226 lines) is the Yorum compiler written in Yorum itself, achieving bootstrap fixed-point (gen1.ll == gen2.ll).

### Self-hosted compiler architecture

The self-hosted compiler is a single file (`yorum-in-yorum/src/main.yrm`) that implements a bootstrap subset of Yorum — no generics, closures, traits, ownership checking, or multi-file support.

**Key design decisions:**
- **Arena-based AST** — No recursive types in Yorum (no `Box<T>`). AST nodes stored in flat global arrays, referenced by integer index
- **`idx_lists: [int]`** — Shared pool for variable-length child lists (call args, array elements, block stmts, struct fields, match arms). Uses batch-push pattern to prevent interleaving during nested parsing
- **Map workaround** — `Map` only supports `string -> int`. Symbol tables use integer indices into pool arrays as values
- **Single file** — Multi-file project system can't resolve cross-imports between non-main modules. Single file avoids this

**Components (all in main.yrm):**
- Lexer (~400 lines): ~50 token kind constants, char-by-char tokenization, nested `/* */` comments
- Parser (~800 lines): Recursive descent + Pratt expression parsing, arena pool structs
- Type checker (~600 lines): Two-pass (register signatures → check bodies), scope stack using `[Map]`
- Code generator (~1,500 lines): LLVM IR string emission, alloca/load/store pattern, ~30 builtin helper functions
- CLI (~100 lines): `./yorumc file.yrm [-o output.ll]`

**Bootstrap chain:**
```bash
cd yorum-in-yorum
cargo run --manifest-path ../Cargo.toml -- build -o yorumc.ll    # Rust compiler → yorumc.ll
clang -x ir yorumc.ll -o yorumc -Wno-override-module             # yorumc.ll → yorumc (native)
./yorumc src/main.yrm -o gen1.ll                                  # yorumc compiles itself → gen1
clang -x ir gen1.ll -o yorumc_gen1 -Wno-override-module
./yorumc_gen1 src/main.yrm -o gen2.ll                             # gen1 compiles itself → gen2
diff gen1.ll gen2.ll                                               # identical (fixed-point)
```

### Phase 1 language extensions (all complete)

These features were added to the Rust compiler for self-hosting:

- **P1a:** `char` type (i8), char literals (`'a'`, `'\n'`), type casting builtins (`int_to_str`, `str_to_int`, `char_to_int`, `int_to_char`, `float_to_int`, `int_to_float`)
- **P1b:** String/char ops (`str_charAt`, `str_sub`, `str_from_char`, `char_is_alpha`, `char_is_digit`, `char_is_whitespace`)
- **P1c:** Dynamic arrays with `push(arr, val)` and `pop(arr)` — realloc-based growth, `{ ptr, i64, i64 }` fat pointers
- **P1d:** File I/O (`file_read`, `file_write`, `print_err`)
- **P1e:** Process builtins (`args()`, `exit()`)
- **P1f:** HashMap (`map_new`, `map_set`, `map_get`, `map_has`) — FNV-1a hash, linear probing

## Completed: v0.6 — Standard Library Builtins

All standard library functions are implemented as compiler builtins (no Yorum source files or auto-import infrastructure).

- **Math (17 builtins, all pure):** `abs_int`, `abs_float`, `min_int`, `max_int`, `min_float`, `max_float`, `sqrt`, `pow`, `sin`, `cos`, `tan`, `floor`, `ceil`, `round`, `log`, `log10`, `exp`. Uses LLVM intrinsics where available (fabs, sqrt, pow, floor, ceil, round, minnum, maxnum) and libm for trig/log/exp
- **String utilities (10 builtins):** `str_contains`, `str_index_of`, `str_starts_with`, `str_ends_with`, `str_trim`, `str_replace`, `str_split`, `str_to_upper`, `str_to_lower`, `str_repeat`. `str_split` returns `[string]` and is special-cased in the typechecker
- **Collection operations (11 builtins):** `slice`, `concat_arrays`, `reverse` (special-cased for generic element types), `contains_int`, `contains_str`, `sort_int`, `sort_str` (insertion sort), `map_keys`, `map_values`, `map_size`, `map_remove`
- **Enhanced I/O (6 builtins):** `file_exists` (access), `file_append` (fopen "a"), `read_line` (fgets from stdin), `print_flush` (printf + fflush), `env_get` (getenv), `time_ms` (gettimeofday)

## Completed: v0.7 — LSP Server for Editor Integration

Built-in LSP server (`yorum lsp`) implementing JSON-RPC 2.0 over stdin/stdout. No new dependencies — uses `serde_json` for JSON-RPC framing.

### LSP architecture

- **`src/lsp/transport.rs`** — JSON-RPC 2.0 read/write over stdin/stdout with `Content-Length` framing
- **`src/lsp/types.rs`** — Minimal serde structs for LSP protocol (positions, ranges, diagnostics, hover, locations)
- **`src/lsp/server.rs`** — Synchronous blocking server loop with document store. Handles `initialize`, `textDocument/didOpen`, `textDocument/didChange`, `textDocument/hover`, `textDocument/definition`, `shutdown`, `exit`

### LSP features

- **Diagnostics:** On every document open/change, runs lex → parse → typecheck → ownership check and publishes errors with precise spans
- **Hover:** Shows type information for variables, parameters, functions, structs, enums at cursor position
- **Go-to-definition:** Jumps to definition site for variables and function calls (skips builtins which have no source location)

### Key design decisions

- **No new dependencies.** JSON-RPC transport is ~50 lines, LSP types are ~160 lines of serde structs
- **Synchronous blocking I/O.** Simple read-process-respond loop, no async/threads
- **Full document sync (mode 1).** Editor sends complete text on every change
- **Symbol collection via opt-in flag.** `TypeChecker::new_with_symbols()` enables collection; normal compilation has zero overhead
- **`def_span` on `VarInfo`/`FnSig`.** Tracks where each variable/function was defined for go-to-definition

### Public API additions

- `check_diagnostics(source) -> Vec<CompilerDiagnostic>` — structured error reporting with spans
- `check_with_symbols(source) -> (Vec<CompilerDiagnostic>, Option<SymbolTable>)` — diagnostics + symbol table for hover/go-to-def
- `TypeChecker::new_with_symbols()` — opt-in symbol collection mode
- `SymbolTable`, `SymbolDef`, `SymbolRef`, `SymbolKind` — symbol table types (public in `compiler::typechecker`)

### VS Code extension

`editors/vscode/` contains a minimal VS Code extension:
- TextMate grammar for syntax highlighting (keywords, types, strings, comments, numbers)
- Language client that launches `yorum lsp` as a subprocess

Setup:
```bash
cd editors/vscode
npm install
npm run compile
# Then: VS Code → Extensions → Install from VSIX, or symlink to ~/.vscode/extensions/
```

## Completed: v0.8 — Ownership Checker Verification

Fixed and verified the ownership checker to properly enforce the spec (SPEC.md section 8).

### Key changes to `ownership.rs`

- **Type-aware tracking:** `VarInfo { state, ty, def_span }` replaces bare `VarState`. `is_copy_type()` classifies `int`/`float`/`bool`/`char`/`string`/`unit` as copy; everything else is move.
- **Actual move marking:** `check_expr_move()` now marks non-copy identifiers as `Moved`. Called from `Stmt::Let`, `Stmt::Return`, and `Stmt::Assign` value positions.
- **Branch merging:** `snapshot_states()`/`restore_states()`/`apply_merged_states()` implement conservative merging for `if`/`else`/`match`: moved in any branch = moved after.
- **Loop safety:** `loop_depth` counter prevents moving outer-scope non-copy variables inside `while`/`for` loops. Loop-local variables are fine.
- **Task span fix:** `pop_scope()` now uses `VarInfo.def_span` for must-join errors instead of `Span::synthetic()`.

### Design decisions

1. **`string` is copy** — immutable pointer at runtime, safe to duplicate
2. **Function args are NOT moved** — `foo(x)` does not consume `x` (matches pass-by-pointer runtime)
3. **Conservative branch merging** — moved in any branch = moved in outer scope
4. **Loop moves of outer variables always error** — even if the loop runs exactly once

## Completed: v0.9 — Networking (TCP/UDP Sockets, HTTP Client)

14 networking builtins implemented as LLVM IR function bodies calling POSIX C library functions. Socket file descriptors represented as `int` values. All builtins are impure.

### TCP (6 builtins)

- `tcp_connect(host: string, port: int) -> int` — Connect to host:port via `getaddrinfo` + `socket` + `connect`. Returns fd or -1
- `tcp_listen(host: string, port: int) -> int` — Bind + listen with `SO_REUSEADDR`. Returns fd or -1
- `tcp_accept(fd: int) -> int` — Accept connection. Returns new fd or -1
- `tcp_send(fd: int, data: string) -> int` — Send string data via `write`. Returns bytes sent or -1
- `tcp_recv(fd: int, max_len: int) -> string` — Receive up to N bytes via `read`. Returns string (empty on error)
- `tcp_close(fd: int) -> unit` — Close socket fd

### UDP (4 builtins)

- `udp_socket() -> int` — Create UDP socket (`SOCK_DGRAM`). Returns fd or -1
- `udp_bind(fd: int, host: string, port: int) -> int` — Bind UDP socket. Returns 0 or -1
- `udp_send_to(fd: int, data: string, host: string, port: int) -> int` — Send datagram via `sendto`. Returns bytes sent or -1
- `udp_recv_from(fd: int, max_len: int) -> string` — Receive datagram via `recvfrom`. Returns string

### DNS (1 builtin)

- `dns_resolve(hostname: string) -> string` — Resolve hostname to IP string via `getaddrinfo` + `inet_ntop`. Returns empty on failure

### HTTP (3 builtins)

- `http_request(method: string, url: string, headers: string, body: string) -> string` — HTTP/1.0 request. Parses URL, connects, sends request, reads response, strips headers. Returns body or empty on failure
- `http_get(url: string) -> string` — Convenience wrapper: `http_request("GET", url, "", "")`
- `http_post(url: string, body: string) -> string` — Convenience wrapper: `http_request("POST", url, "", body)`

### Key design decisions

1. **Socket FDs as `int`** — No new types needed. `-1` indicates error (POSIX convention)
2. **Prefix naming (`tcp_`, `udp_`, etc.)** — Avoids conflict with existing `send()`/`recv()` channel builtins
3. **`getaddrinfo` for host resolution** — Handles both hostnames and IP addresses
4. **Platform-specific `sockaddr_in`** — macOS has `sin_len` byte prefix; `cfg!(target_os = "macos")` in codegen
5. **Platform-specific constants** — `SOL_SOCKET`: 1 (Linux) vs 65535 (macOS). `SO_REUSEADDR`: 2 (Linux) vs 4 (macOS)
6. **HTTP/1.0 with `Connection: close`** — Simplifies response reading. No chunked transfer encoding
7. **No TLS/HTTPS** — Plain HTTP only. TLS would require linking OpenSSL
8. **`htons` implemented in LLVM IR** — Byte-swap helper emitted directly, no external dependency

## Completed: v1.0 — Stable Language Specification and ABI

Stabilization release — no new language features or builtins. Focus on robustness, ABI documentation, and test coverage.

### Key changes

- **Robustness:** Replaced all `panic!()` calls in codegen (`Type::SelfType`/`Type::TypeVar`) with `debug_assert!` + fallback. Replaced all `unwrap()` calls in LSP server with error handling (`unwrap_or`, match with early return)
- **Dynamic versioning:** `src/main.rs` and `src/lsp/server.rs` use `env!("CARGO_PKG_VERSION")` instead of hardcoded strings
- **ABI appendix:** `SPEC.md` Appendix A documents primitive layout, struct/enum/array representation, calling convention, name mangling, closure/concurrency/HashMap internals
- **Effects clarification:** `SPEC.md` section 5.1 notes that `effects` is parsed but not enforced
- **Test coverage:** 56 new integration tests (5 robustness, 15 parser errors, 7 module system, 7 control flow, 5 struct/trait, 17 example programs) — total 336 tests (46 unit + 290 integration)

## Completed: v1.0.2 — Codegen Bug Fixes

Four correctness bugs in `codegen.rs` identified by staff-engineer code review and fixed.

### Bug fixes

1. **Stale `current_block` in `emit_function`:** `emit_function` reset `temp_counter`, `label_counter`, `block_terminated` but not `current_block`. If the previous function ended on a label like `ifcont.5`, the next function's `and`/`or` PHI nodes would reference that stale label. Fix: reset `self.current_block = "entry".to_string()` after line 2561, matching `emit_closure_function` and `emit_spawn_function`.

2. **HashMap tombstone infinite loop:** `__yorum_map_find_slot` only stops on `flag==0` (empty) or key match, skipping tombstones (`flag==2`). After insert/remove churn, all slots could become occupied+tombstoned with zero empty slots, causing infinite probe. Fix: added `tombstones` counter at offset 40 in the map struct (48 bytes total). `map_set` load factor check now uses `(size + tombstones) * 4 >= cap * 3`. `map_remove` increments tombstones. `__yorum_map_grow` resets tombstones to 0 after rehash.

3. **Spawn env under-allocation:** `env_size` was computed by summing raw `llvm_type_size` values, ignoring struct alignment padding. For `{ i1, i64 }` this gave 9 bytes but LLVM struct requires 16 (alignment padding). GEP writes at offset 8 caused heap buffer overflow. Fix: replaced with LLVM sizeof idiom (`getelementptr %type, ptr null, i32 1` + `ptrtoint ptr to i64`).

4. **Invalid LLVM IR for unit-typed let bindings:** `let x: unit = foo()` emitted `alloca void` and `store void void, ptr %x.addr`, both illegal in LLVM IR. Fix: `emit_let` early-returns for `Type::Unit` (evaluates RHS for side effects only), `emit_assign` skips the store when `slot.llvm_ty == "void"`.

### Test coverage

4 new integration tests — total 340 tests (46 unit + 294 integration).

## Completed: v1.1 — Ergonomics & Missing Operators

Four quality-of-life features that eliminate daily-use paper cuts. All are additive — no breaking changes.

### Feature 1: Compound Assignment Operators

`+=`, `-=`, `*=`, `/=`, `%=` — desugared in the parser to `x = x + e` form. No typechecker, ownership, or codegen changes needed.

- **Tokens:** `PlusEq`, `MinusEq`, `StarEq`, `SlashEq`, `PercentEq`
- **Lexer:** Each of `+`, `-`, `*`, `/`, `%` peeks for `=`
- **Parser:** `try_compound_assign()` helper maps compound tokens to `BinOp`; `parse_assign_or_expr_stmt()` desugars after the existing `=` check
- Works with array index (`arr[i] += 1;`) and field access (`p.x += 1;`)

### Feature 2: Bitwise Operators

`&` (BitAnd), `|` (BitOr), `^` (BitXor), `<<` (Shl), `>>` (Shr) — full pipeline support.

- **Tokens:** `Caret` (^), `LShift` (<<). `Ampersand`/`Pipe` already existed. `>>` is NOT a single token — parsed as two `Gt` tokens to avoid conflict with generic closing `>>`
- **AST:** `BinOp::BitAnd`, `BitOr`, `BitXor`, `Shl`, `Shr`
- **Parser:** `try_binop()` returns `Option<(BinOp, usize)>` where `usize` is token width (2 for `>>`). `peek_kind_at(pos)` enables arbitrary lookahead. C-style precedence: `Or(3,4) And(5,6) BitOr(7,8) BitXor(9,10) BitAnd(11,12) Eq/NotEq(13,14) comparisons(15,16) shifts(17,18) add/sub(19,20) mul/div/mod(21,22)`
- **Typechecker:** Both operands must be `int`, returns `int`
- **Codegen:** `and i64`, `or i64`, `xor i64`, `shl i64`, `ashr i64` (arithmetic shift right, preserves sign)

### Feature 3: `break` and `continue`

Loop control flow statements for `while` and `for` loops.

- **Tokens:** `Break`, `Continue` keywords
- **AST:** `BreakStmt { span }`, `ContinueStmt { span }`, `Stmt::Break`, `Stmt::Continue`
- **Typechecker:** `loop_depth: usize` field — incremented in while/for, decremented after. Error if break/continue when `loop_depth == 0`
- **Codegen:** `loop_labels: Vec<(String, String)>` stack of `(continue_label, break_label)` pairs. `emit_break` → `br label %{break_label}`. `emit_continue` → `br label %{continue_label}`. For-loop increment extracted into `for.inc` label so `continue` increments the index before jumping to condition

### Feature 4: Range Expressions (`for i in 0..n`)

Counter-based for loops using range syntax.

- **Token:** `DotDot` (..)
- **AST:** `ExprKind::Range(Box<Expr>, Box<Expr>)`
- **Parser:** `..` handled in `parse_expr_bp` before `try_binop` with binding power `(1, 2)` — lowest precedence. `0..n + 5` parses as `0..(n + 5)`
- **Typechecker:** Both bounds must be `int`. Standalone range expressions error: "range expression is only valid in for loops"
- **Codegen:** `emit_for_range()` — alloca for counter, `icmp slt` against end, `for.cond`/`for.body`/`for.inc`/`for.end` labels with `loop_labels` stack integration

### Test coverage

22 new integration tests — total 362 tests (46 unit + 316 integration).

## Completed: v1.2 — String Interpolation, Tuple Types, Option/Result

Three features: string interpolation as an ergonomic paper cut fix, tuple types for lightweight grouping, and prelude Option/Result for real-world error handling.

### Feature 1: String Interpolation

`"hello {expr}"` syntax with `{{`/`}}` escapes. Desugars in the parser to `str_concat`/`to_str` chains.

- **Token:** `InterpStringLit(Vec<InterpPart>)` with `InterpPart::Literal(String)` / `InterpPart::Expr(Vec<Token>)`
- **Lexer:** Modified `lex_string()` to detect `{`/`}` and lex inner expressions, tracking brace depth
- **Parser:** `InterpStringLit` desugars each part to `to_str(expr)` wrapped in `str_concat` left-fold
- **Typechecker:** `to_str` special-cased to accept any type and return `string`. `float_to_str` and `bool_to_str` registered as builtins
- **Codegen:** `to_str` dispatches to `int_to_str`/`float_to_str`/`bool_to_str`/`str_from_char`/identity based on LLVM type. `float_to_str` uses `snprintf(%g)`, `bool_to_str` branches on `i1`

### Feature 2: Tuple Types

`(int, string)` for types, `(1, "hello")` for values, `.0`/`.1` for access, `let (a, b) = t;` for destructuring.

- **AST:** `Type::Tuple(Vec<Type>)`, `ExprKind::TupleLit(Vec<Expr>)`, `LetStmt.destructure: Option<Vec<String>>`
- **Parser:** Tuple literals detected by comma after first expr. Type parsing handles `(T, U)`. Let parsing handles `(a, b)` destructure patterns
- **Typechecker:** Infers element types, validates destructure arity, handles field access with numeric index
- **Codegen:** Tuples compile to LLVM named structs (`%tuple.int.string = type { i64, ptr }`). `ensure_tuple_type()` lazily emits type defs. GEP for field access, alloca+GEP+store for destructuring
- **Ownership:** Tuples are copy iff all elements are copy types. Destructuring defines each name

### Feature 3: Option\<T\> & Result\<T, E\> Prelude Types

Generic enums always available without declaration. Required adding generic enum monomorphization.

- **Typechecker:** `EnumInfo.type_params` added. `Option<T>` and `Result<T, E>` registered in `register_builtins()`. Generic variant inference in Call handler (two-pass: non-generic first, then generic). Pattern matching uses `subject_enum_info()` to substitute type vars in variant fields. Methods: `.unwrap()`, `.is_some()`, `.is_none()`, `.is_ok()`, `.is_err()`, `.unwrap_err()`
- **Monomorphizer:** `generic_enums`/`enum_instantiations` tracking. Prelude Option/Result registered as `EnumDecl`. Emits monomorphized enum declarations with mangled names (`Option__int`, `Result__int__string`). Rewrites `Type::Generic` to `Type::Named(mangled)`
- **Codegen:** `current_expected_enum` hint for variant disambiguation. `expr_returns_ptr()` helper distinguishes alloca pointers from values. Fixed enum match to use alloca pointer for GEP. Fixed enum/struct return to load value before `ret`. Fixed enum/struct let from function calls to alloca+store

### Bug fixes (pre-existing)

- **Enum match codegen:** `emit_match` was loading enum struct value then using it as pointer for GEP — fixed to use `emit_expr_ptr` for enum subjects
- **Enum/struct return:** Variant constructors return alloca pointers but `ret %Enum %ptr` is invalid — added load before ret
- **Enum/struct let from function calls:** `emit_let` assumed RHS always returns pointer for aggregate types, but function calls return values — now checks `expr_returns_ptr()` and alloca+stores when needed

### Test coverage

23 new integration tests — total 385 tests (46 unit + 339 integration).

## Completed: v1.2.1 — PR Review Bug Fixes

Five bugs found in the v1.2 PR review: 3 codegen issues producing invalid LLVM IR, 1 type-safety regression, and 1 parser leniency issue.

### Bug fixes

1. **`to_str` missing from `fn_ret_types`:** `to_str` was special-cased in `emit_expr` but never registered in `fn_ret_types`. `expr_llvm_type` fell back to `"i64"` instead of `"ptr"`, making string interpolation emit `str_concat(ptr, i64)` — invalid IR. Fix: registered `to_str` → `Type::Str` in codegen init.

2. **Tuple let binding assumes RHS is pointer:** Tuple path in `emit_let` always treated `emit_expr` result as an alloca pointer. Function calls returning tuples produce values, not pointers (same bug already fixed for structs/enums). Fix: added `expr_returns_ptr()` check with `alloca + store` fallback.

3. **Tuple type naming inconsistency:** `expr_llvm_type` for `TupleLit` built names using LLVM types (`%tuple.i64.ptr`) while `emit_tuple_lit` and `tuple_type_name` used semantic names (`%tuple.int.string`). Downstream code referenced undefined types. Fix: extracted `llvm_to_semantic_name()` helper, used in both locations.

4. **Generic argument mismatches accepted:** Compatibility checks in `Stmt::Let` and `Stmt::Return` only compared the base name of `Type::Generic`, ignoring type arguments. `Option<int>` would accept `Some("hello")`. Fix: compare type args pairwise, allowing `TypeVar` as wildcard (for unresolved params like `None` → `Option<T>`) but rejecting concrete mismatches.

5. **Interpolation parser accepts trailing tokens:** `desugar_interp_string` called `parse_expr()` but didn't verify the token stream was exhausted. `"bad {1 2}"` silently dropped `2`. Fix: check `peek_kind() != EOF` after `parse_expr()`.

### Test coverage

5 new integration tests — total 390 tests (46 unit + 344 integration).

## Completed: v1.3 — Generic Collections & Error Handling Sugar

Four features: match exhaustiveness checking, generic `Map<K, V>`, generic `Set<T>`, and the `?` (try) operator.

### Feature 1: Match Exhaustiveness Checking

Added `check_match_exhaustiveness()` in typechecker. For enum match statements, verifies all variants are covered. Wildcard (`_`) and binding catch-all patterns satisfy exhaustiveness. Non-enum matches (int, string) are not checked.

### Feature 2: Generic Map\<K, V\>

Replaced the hardcoded `Type::Map` AST variant with `Type::Generic("Map", [K, V])`. Bare `Map` parses as `Map<string, int>` for backward compatibility. Map keys restricted to hashable primitives (`int`, `string`, `char`, `bool`).

- **Typechecker:** Removed fixed-signature `map_*` registrations. Map builtins (`map_new`, `map_set`, `map_get`, `map_has`, `map_size`, `map_remove`, `map_keys`, `map_values`) are special-cased in the Call handler, extracting K/V from the map variable's type.
- **Monomorphizer:** Rewrites `Type::Generic("Map", [K, V])` to `Type::Named("Map__K__V")` and mangles call sites (`map_set__string__int`).
- **Codegen:** Lazy emission of LLVM IR map helpers per (K, V) pair. `emit_map_helpers_for_suffix()` emits hash function, find_slot, grow, and all 8 map builtins parameterized by key/value LLVM types. Existing `Map<string, int>` helpers retained with LLVM aliases for backward compat.

### Feature 3: Set\<T\>

Generic hash set with 40-byte struct layout `{keys: ptr, flags: ptr, cap: i64, size: i64, tombstones: i64}`. Elements restricted to hashable primitives.

- **Builtins:** `set_new`, `set_add`, `set_has`, `set_remove`, `set_size`, `set_values`
- **Codegen:** `emit_set_helpers_for_suffix()` reuses hash functions from Map if same key type already emitted.

### Feature 4: `?` Operator

Postfix try operator for ergonomic error propagation.

- **Token/Lexer:** `QuestionMark` token, `'?' => QuestionMark` in lexer
- **AST:** `ExprKind::Try(Box<Expr>)`
- **Parser:** Added `?` to postfix parsing loop (alongside `.`, `[`, method calls)
- **Typechecker:** `Option<T>?` yields `T` (fn must return `Option<_>`). `Result<T, E>?` yields `T` (fn must return `Result<_, E>` with matching E)
- **Codegen:** `emit_try_expr` extracts tag, branches on Some/Ok (tag 0) vs None/Err. Success path extracts payload T. Error path constructs None or Err(e) for the function's return type and emits early `ret`

### Test coverage

33 new integration tests — total 423 tests (46 unit + 377 integration).

## Completed: v1.3.1 — Match Codegen Fixes & Examples

Two pre-existing codegen bugs exposed by multiple match statements in a single function, plus three new example programs.

### Bug fixes

1. **Duplicate match binding allocas:** Match pattern bindings used fixed names (`%v.match.addr`) instead of unique SSA names. When two match statements in the same function bound the same variable name, clang rejected the IR with "multiple definition of local value". Fix: use `fresh_temp()` for match binding allocas.

2. **Duplicate match check labels:** Match check labels used arm-index naming (`match.check.1`) instead of globally unique labels. Multiple match statements in one function produced duplicate labels causing clang segfaults. Fix: pre-generate check labels via `fresh_label()` alongside arm labels.

### New examples

- `examples/maps.yrm` — Generic `Map<K, V>` with all hashable key types (string, int, char, bool) and all builtins
- `examples/sets.yrm` — Generic `Set<T>` with all hashable element types and all builtins
- `examples/try_operator.yrm` — `?` operator with `Option<T>` and `Result<T, E>`, success and error propagation paths

## Git Workflow

- **Branches**: Use `feature/<descriptive-name>` for feature branches (e.g., `feature/arrays-and-loops`). Never use version numbers in branch names — versions are only for tags and releases.
- **CI**: Runs `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, and `cargo test`. All three must pass before merging.
- **Releases**: Tag with `vX.Y.Z` and push the tag. The release workflow builds binaries for 5 targets and creates a GitHub release automatically.
