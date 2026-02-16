# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands

```bash
cargo build                          # dev build
cargo build --release                # release build
cargo test                           # all tests (120: 34 unit + 86 integration)
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
- **`typechecker.rs`** — Two-pass: first registers all function signatures, struct layouts, and enum definitions; then checks function bodies. Uses a scope stack (`Vec<HashMap<String, VarInfo>>`) for lexical scoping. Built-in functions registered in `register_builtins()`: print (`print_int`, `print_float`, `print_bool`, `print_str`, `print_char`, `print_err`), string ops (`str_len`, `str_concat`, `str_eq`, `str_charAt`, `str_sub`, `str_from_char`), char classification (`char_is_alpha`, `char_is_digit`, `char_is_whitespace`), type casting (`int_to_str`, `str_to_int`, `char_to_int`, `int_to_char`, `int_to_float`, `float_to_int`), file I/O (`file_read`, `file_write`), HashMap (`map_new`, `map_set`, `map_get`, `map_has`). `len()`, `push()`, `pop()`, `args()`, `exit()`, `chan()`, `send()`, `recv()` are special-cased in Call handling. `spawn` type inference and `.join()` method handling are in `infer_expr`. Contract expressions are type-checked (must be `bool`). Purity enforcement: pure functions cannot call impure functions or use `spawn`
- **`ownership.rs`** — Simplified move checker. Tracks `Owned`/`Moved`/`Borrowed` state per variable. Prevents use-after-move. Enforces must-join for `Task` variables via `task_vars: Vec<HashSet<String>>` — tasks must be `.join()`'d before scope exit
- **`monomorphize.rs`** — Eliminates generics before codegen. Collects all concrete instantiations of generic functions/structs/enums, clones declarations with type variables substituted, and rewrites call sites to use mangled names (`identity__int`, `Pair__int__float`). Also substitutes type vars in contract expressions
- **`codegen.rs`** — Emits textual LLVM IR. Uses alloca/load/store pattern (LLVM's mem2reg promotes to SSA). Each expression emitter returns the LLVM SSA value name (e.g., `%t3`). Struct-typed and enum-typed let bindings reuse the alloca directly instead of store/load. Closures compile to `{ ptr, ptr }` pairs (fn_ptr + env_ptr) with deferred function emission. Arrays are heap-allocated fat pointers `{ ptr, i64, i64 }` (data, length, capacity) with runtime bounds checking and `realloc`-based growth for `push`. Aggregate types (structs, enums) in arrays use `memcpy` for push/pop/index/array-lit operations. `requires`/`ensures` emit conditional branches to `@__yorum_contract_fail`. `spawn` compiles to `pthread_create` with env capture (same pattern as closures). Channel helpers are emitted as LLVM IR functions using mutex/condvar. HashMap uses FNV-1a hashing with linear probing, emitted as LLVM IR helper functions
- **`module_resolver.rs`** — Discovers `.yrm` files under `src/`, maps filesystem paths to module names, parses each file, validates `module` declarations match paths
- **`project.rs`** — Orchestrates multi-file compilation: reads `yorum.toml` manifest, resolves modules, merges `pub` declarations with name prefixing (`math__add`), rewrites call sites, runs standard pipeline

### Important codegen details

- `fresh_temp()` and `fresh_label()` generate unique SSA names and labels per function
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
- Spawn uses the closure capture pattern: env struct with captured vars + result slot, wrapper function `@__spawn_N`, `pthread_create` to launch. Task control block is `{ pthread_t, env_ptr }`. `.join()` calls `pthread_join`
- Channel helpers (`@__yorum_chan_create`, `@__yorum_chan_send`, `@__yorum_chan_recv`) are emitted as LLVM IR functions using pthread mutex + condvar. `spawn_counter` tracks unique spawn IDs for wrapper function naming

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

## Git Workflow

- **Branches**: Use `feature/<descriptive-name>` for feature branches (e.g., `feature/arrays-and-loops`). Never use version numbers in branch names — versions are only for tags and releases.
- **CI**: Runs `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, and `cargo test`. All three must pass before merging.
- **Releases**: Tag with `vX.Y.Z` and push the tag. The release workflow builds binaries for 5 targets and creates a GitHub release automatically.
