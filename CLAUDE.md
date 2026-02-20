# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands

```bash
cargo build                          # dev build
cargo build --release                # release build
cargo test                           # all tests (695: 68 unit + 627 integration)
cargo test compiler::lexer           # tests in one module
cargo test test_fibonacci_compiles   # single test by name
cargo test -- --nocapture            # see stdout from tests
```

### Running the compiler

```bash
cargo run -- compile file.yrm              # emit LLVM IR to stdout
cargo run -- compile file.yrm -o out.ll    # emit LLVM IR to file
cargo run -- compile file.yrm -g -o out.ll # emit LLVM IR with DWARF debug info
cargo run -- check file.yrm                # type-check only
cargo run -- ast file.yrm                  # dump AST as JSON
cargo run -- build                         # build multi-file project (needs yorum.toml)
cargo run -- build -o out.ll               # build project to file
cargo run -- init myproject                # scaffold new project
cargo run -- run file.yrm                  # compile + link + execute (requires clang)
cargo run -- run file.yrm -- arg1 arg2     # run with arguments
cargo run -- fmt file.yrm                  # auto-format source file
cargo run -- fmt --check file.yrm          # check formatting (CI mode, exit 1 if unformatted)
cargo run -- install                       # fetch and cache dependencies
cargo run -- update                        # update all dependencies
cargo run -- update dep_name               # update a single dependency
cargo run -- repl                          # interactive REPL
cargo run -- lsp                           # start LSP server (stdin/stdout)
```

### Producing native binaries (requires clang)

```bash
clang -x ir out.ll -o binary -Wno-override-module
clang -x ir out.ll -o binary -lpthread -Wno-override-module   # if using spawn/channels
```

### Homebrew LLVM (recommended for debugging)

Homebrew LLVM provides more explanatory error messages than Xcode's bundled clang, and includes `lli` for JIT execution of LLVM IR without producing a binary.

```bash
brew install llvm
/opt/homebrew/opt/llvm/bin/clang -x ir out.ll -o binary -Wno-override-module   # better error messages
/opt/homebrew/opt/llvm/bin/lli out.ll                                           # JIT execute directly
```

## Architecture

This is a compiler for the Yorum language — an LLM-first, statically typed language that emits textual LLVM IR. No LLVM library dependency; the compiler is pure Rust with only `serde`/`serde_json`/`toml`. The compiler is **self-hosting** — `yorum-in-yorum/src/main.yrm` is the compiler written in Yorum itself (5,226 lines), achieving bootstrap fixed-point.

### Compilation pipeline

The pipeline is strictly sequential — each phase consumes the output of the previous one. Single-file compilation is orchestrated in `src/lib.rs`, multi-file in `src/compiler/project.rs`:

```
Source → Lexer → Vec<Token> → Parser → AST (Program) → TypeChecker → OwnershipChecker → Monomorphizer → DCE → Codegen → LLVM IR string
```

Multi-file projects (`yorum build`) add a front-end step: `ModuleResolver` discovers all `.yrm` files, parses each, then `project.rs` merges them into a single `Program` (prefixing imported names) before the standard pipeline runs. Projects with dependencies resolve deps first (fetch/cache git repos or resolve local paths), discover dep modules, then merge them alongside local modules with `<dep_name>__` namespace prefixing.

### Key modules (in `src/compiler/`)

- **`span.rs`** — `Span` type (byte offsets + line/col) carried by every token and AST node
- **`token.rs`** — `TokenKind` enum; `keyword_from_str()` maps identifiers to keyword tokens
- **`lexer.rs`** — Hand-written char-by-char lexer. Supports nested `/* */` block comments, string interpolation (`"hello {expr}"`). Returns `Vec<Token>`
- **`ast.rs`** — Complete AST types. All nodes derive `Serialize`/`Deserialize` for JSON round-tripping. `Type` enum has `PartialEq`/`Eq` for type checking comparisons
- **`parser.rs`** — Recursive descent parser with Pratt expression parsing (`parse_expr_bp`). Operator precedence is defined in `infix_binding_power()`. Struct init is disambiguated from blocks via 2-token lookahead in `is_struct_init_lookahead()`. Compound assignments (`+=`, `-=`, etc.) desugared here. String interpolation desugared to `str_concat`/`to_str` chains
- **`typechecker.rs`** — Three-pass: (1) registers all function signatures, struct layouts, and enum definitions; (1.5) infers effects for unannotated functions via fixed-point call graph iteration; (2) checks function bodies. Uses a scope stack (`Vec<HashMap<String, VarInfo>>`) for lexical scoping. ~70 built-in functions registered in `register_builtins()` with effect annotations. Special-cased builtins in Call handler: `len`, `push`, `pop`, `args`, `exit`, `chan`, `send`, `recv`, `slice`, `concat_arrays`, `reverse`, `str_split`, `to_str`, `map_*`, `set_*`. Prelude types `Option<T>` and `Result<T, E>` are registered as generic enums with methods. Purity enforcement: pure functions cannot call impure functions or use `spawn`. Effect enforcement: functions with `effects` clause can only call functions whose effects are a subset of the declared effects. Iterator pipelines: `infer_pipeline_elem_type()` recursively validates `.iter().map(f).filter(g)` chains, checking closure param/return types at each step
- **`ownership.rs`** — Type-aware move checker. Tracks `VarInfo { state, ty, def_span }` per variable with `is_copy_type()` distinguishing copy types (`int`, `float`, `bool`, `char`, `string`, `unit`) from move types (structs, enums, arrays, etc.). Branch merging for if/else and match ensures moves in any branch propagate conservatively. `loop_depth` tracking prevents moving outer-scope variables inside loops. Enforces must-join for `Task` variables
- **`monomorphize.rs`** — Eliminates generics before codegen. Collects all concrete instantiations of generic functions/structs/enums, clones declarations with type variables substituted, and rewrites call sites to use mangled names (`identity__int`, `Pair__int__float`). Handles generic enum monomorphization for prelude types (`Option__int`, `Result__int__string`)
- **`dce.rs`** — Dead code elimination. BFS reachability from `main` removes unreachable functions, structs, enums, and impl blocks. Skips programs without `main` (e.g., test-only compilations). Runs between monomorphization and codegen
- **`semver.rs`** — Minimal semver `Version` struct with `parse()`, `Ord`, `Display`. Supports `"1.2.3"` and `"v1.2.3"` formats
- **`lockfile.rs`** — `LockFile`/`LockedPackage` structs for `yorum.lock`. TOML-based with `git+url#sha` and `path+dir` source formats. Read/write methods
- **`package.rs`** — `PackageCache` for git clone/cache in `~/.yorum/cache/`. `resolve_dep()` handles git (shallow clone, tag/branch/rev checkout) and path dependencies. Validates dep manifests and package name matches
- **`codegen.rs`** — Emits textual LLVM IR. Uses alloca/load/store pattern (LLVM's mem2reg promotes to SSA). Each expression emitter returns the LLVM SSA value name (e.g., `%t3`). Closures compile to `{ ptr, ptr }` pairs (fn_ptr + env_ptr). Arrays are heap-allocated fat pointers `{ ptr, i64, i64 }` (data, length, capacity). Aggregate types (structs, enums) in arrays use `memcpy`. Generic `Map<K, V>` and `Set<T>` helpers are lazily emitted per monomorphized type pair. The `?` (try) operator compiles to tag check + branch with early `ret`
- **`module_resolver.rs`** — Discovers `.yrm` files under `src/`, maps filesystem paths to module names, parses each file, validates `module` declarations match paths
- **`project.rs`** — Orchestrates multi-file compilation: reads `yorum.toml` manifest, resolves dependencies (git/path), discovers local and dep modules, merges `pub` declarations with name prefixing (`math__add`, `dep_name__fn`), rewrites call sites and type references, runs standard pipeline. Also implements `install_dependencies()` and `update_dependencies()`

### Important codegen details

- `fresh_temp()` and `fresh_label()` generate unique SSA names and labels per function
- `emit_function` resets `temp_counter`, `label_counter`, `block_terminated`, and `current_block` to `"entry"` — all four must be reset to avoid cross-function label leakage in PHI nodes
- `block_terminated` flag tracks whether a `ret`/`br` has been emitted — prevents emitting instructions after a terminator
- Built-in print functions are emitted directly as LLVM IR definitions (not linked from a C runtime)
- Format strings for printf are global constants with manually counted byte sizes — update sizes if changing format strings
- Structs map to LLVM struct types (`%Name = type { ... }`), accessed via `getelementptr`
- Enums without data are `{ i32 }`; enums with data are `{ i32, [N x i8] }` tagged unions. Enum variant constructors (e.g., `Some(42)`) are detected in Call handling and emit alloca + tag + payload store
- Arrays use reference semantics in codegen: `emit_expr` on an array Ident returns the alloca pointer directly (not a load), and `expr_llvm_type` returns `"ptr"` for array idents
- `array_elem_types: HashMap<String, String>` tracks the LLVM element type per array variable, used by Index, For, push, pop, and assignment codegen
- `push(arr, val)` checks if `len == cap`, doubles capacity via `realloc` when needed. For aggregate types, uses `memcpy` instead of `store`
- `type_size(&str)` computes actual LLVM type sizes by summing struct field sizes recursively — needed for `memcpy` of aggregate array elements
- Contract checks: `requires` emits conditional branches at function entry; `ensures` wraps each `ret`. Failures call `@__yorum_contract_fail` (prints message, aborts)
- Spawn uses the closure capture pattern: env struct with captured vars + result slot, wrapper function `@__spawn_N`, `pthread_create` to launch. Env struct size computed via LLVM sizeof idiom (`getelementptr %type, ptr null, i32 1` + `ptrtoint`) — do not use raw `llvm_type_size` sums
- Channel helpers (`@__yorum_chan_create`, `@__yorum_chan_send`, `@__yorum_chan_recv`) use pthread mutex + condvar
- Unit-typed let bindings and assignments skip alloca/store — `alloca void` and `store void` are invalid LLVM IR. The RHS is still evaluated for side effects
- Map struct is 48 bytes `{keys, vals, flags, cap, size, tombstones}`, Set is 40 bytes `{keys, flags, cap, size, tombstones}`
- Tuples compile to LLVM named structs (`%tuple.int.string = type { i64, ptr }`). `ensure_tuple_type()` lazily emits type defs
- Iterator pipelines (`.iter().map().filter().enumerate().zip().take().skip()` chains) are fused into a single loop by `emit_for_pipeline()`. `try_extract_pipeline()` walks the AST chain right-to-left, extracting `IterStep` variants (Map, Filter, Enumerate, Zip, Take, Skip). Closures are emitted as `{ ptr, ptr }` pairs; fn_ptr and env_ptr are loaded pre-loop. Index is incremented in the step block (before filter branches), so `continue` targets `for.cond` and filtered-out elements correctly advance
- Pipeline terminators (`.collect()`, `.fold()`, `.any()`, `.all()`, `.find()`, `.reduce()`) are standalone expressions intercepted before receiver inference in both `infer_expr` and `emit_expr`. `try_extract_terminated_pipeline()` extracts pipeline steps + terminator. Each terminator emits its own loop with shared step logic via `emit_pipeline_steps()`. `.find()` and `.reduce()` return `Option<T>` (monomorphizer registers instantiation via `has_iter_base_static()`)
- Math builtins inline LLVM intrinsic calls at call sites (no wrapper functions — wrappers would shadow C library symbols)

### Yorum language design choices

- Semicolons terminate statements; `{}` delimit blocks
- Logical operators are keywords: `and`, `or`, `not` (no `&&`/`||`/`!`)
- All let bindings require explicit type annotation: `let x: int = 5;`
- Functions have `requires`/`ensures`/`effects` contract clauses — `requires` and `ensures` are verified at runtime (abort on violation); `effects` is enforced at compile time (only declared effects allowed at call sites)
- `pure fn` marks side-effect-free functions — enforced at compile time (cannot call impure functions, use `spawn`, or perform mutation/allocation). Strictly more restrictive than empty `effects`
- Effect system: 6 categories (`io`, `fs`, `net`, `time`, `env`, `concurrency`). Functions without `effects` clause are unchecked (backward compatible). Functions with `effects` clause are checked. `main` is always unchecked. Effect inference propagates through the call graph for unannotated functions
- `spawn { block }` creates concurrent tasks; `Task` variables must be `.join()`'d before scope exit (enforced by ownership checker)
- Compound assignment operators: `+=`, `-=`, `*=`, `/=`, `%=` (desugared in parser)
- Bitwise operators: `&`, `|`, `^`, `<<`, `>>` (int only). `>>` is parsed as two `Gt` tokens (avoids conflict with generic closing `>>`)
- `break` and `continue` for while/for loops
- Range syntax: `for i in 0..n` (counter-based for loop)
- Iterator pipelines: combinators (`.map()`, `.filter()`, `.enumerate()`, `.zip()`, `.take()`, `.skip()`) work in for-loops, terminators (`.collect()`, `.fold()`, `.any()`, `.all()`, `.find()`, `.reduce()`) work as standalone expressions. All chains start with `.iter()` on an array. All fused into single loops with zero allocation (except `.collect()`). Only inline closures supported
- String interpolation: `"hello {expr}"` with `{{`/`}}` escapes
- Tuple types: `(int, string)`, destructuring: `let (a, b) = t;`
- `Option<T>` and `Result<T, E>` are prelude types (always available)
- `?` operator: `Option<T>?` yields `T` (fn must return `Option<_>`), `Result<T, E>?` yields `T` (fn must return `Result<_, E>`)
- Match exhaustiveness checked for enums
- Generic `Map<K, V>` and `Set<T>` with keys restricted to hashable primitives (`int`, `string`, `char`, `bool`)
- Copy types: `int`, `float`, `bool`, `char`, `string`, `unit`, `[T]` (arrays), `Map<K,V>`, `Set<T>`. Structs and enums use move semantics
- Reserved words cannot be used as identifiers (including module names)

### Test organization

- **Unit tests**: inline `#[cfg(test)] mod tests` in each compiler module
- **Integration tests**: `tests/integration_tests.rs` — uses the public API (`yorum::compile_to_ir`, `yorum::typecheck`, `yorum::source_to_ast_json`)
- Integration test helpers: `compile()` for IR output assertions, `parse_and_check()` for type-check-only, `parse_to_json()` for AST structure assertions
- Multi-file project tests use `yorum::compile_project()` with temp directories containing `yorum.toml` + `.yrm` files

### Example programs

`examples/*.yrm` — 32 examples covering all major language features. All compile to native binaries and run correctly. Use these as references for valid Yorum syntax.

**Iteration rule:** Every version that adds new language features MUST also add or expand example programs exercising those features. After adding examples, verify the full cycle: compile with `cargo run -- compile`, link with `clang`, and run the binary. Do not use workarounds in examples — if a codegen bug prevents clean usage, fix the bug first.

## Self-hosting Compiler

`yorum-in-yorum/src/main.yrm` (5,226 lines) implements a bootstrap subset of Yorum — no generics, closures, traits, ownership checking, or multi-file support. Uses arena-based AST (flat global arrays referenced by integer index) since Yorum has no `Box<T>`.

**Bootstrap chain:**
```bash
cd yorum-in-yorum
cargo run --manifest-path ../Cargo.toml -- build -o yorumc.ll
clang -x ir yorumc.ll -o yorumc -Wno-override-module
./yorumc src/main.yrm -o gen1.ll
clang -x ir gen1.ll -o yorumc_gen1 -Wno-override-module
./yorumc_gen1 src/main.yrm -o gen2.ll
diff gen1.ll gen2.ll   # identical (fixed-point)
```

## LSP Server

Built-in LSP server (`yorum lsp`) implementing JSON-RPC 2.0 over stdin/stdout. No new dependencies.

- **`src/lsp/transport.rs`** — JSON-RPC 2.0 read/write with `Content-Length` framing
- **`src/lsp/types.rs`** — Serde structs for LSP protocol (diagnostics, completion, code actions)
- **`src/lsp/server.rs`** — Synchronous blocking server loop. Diagnostics, hover (type info), go-to-definition, completions (prefix + dot), code actions (did-you-mean, effect hints, match arm hints)
- **`src/repl.rs`** — Interactive REPL with compile-link-execute loop

VS Code extension in `editors/vscode/` — TextMate grammar + language client launching `yorum lsp`.

## Git Workflow

- **Branches**: Use `feature/<descriptive-name>` for feature branches (e.g., `feature/arrays-and-loops`). Never use version numbers in branch names — versions are only for tags and releases.
- **CI**: Runs `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, and `cargo test`. All three must pass before merging.
- **Releases**: Tag with `vX.Y.Z` and push the tag. The release workflow builds binaries for 5 targets and creates a GitHub release automatically.
