# CLAUDE.md

## Build & Test

```bash
cargo build                          # dev build
cargo test                           # all tests (712: 68 unit + 644 integration)
cargo test compiler::lexer           # one module
cargo test test_fibonacci_compiles   # single test
```

CI: `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`. All must pass.

### Running the compiler

```bash
cargo run -- compile file.yrm -o out.ll    # emit LLVM IR
cargo run -- compile file.yrm -g -o out.ll # with DWARF debug info
cargo run -- check file.yrm                # type-check only
cargo run -- ast file.yrm                  # dump AST as JSON
cargo run -- build                         # multi-file project (needs yorum.toml)
cargo run -- run file.yrm                  # compile + link + execute (requires clang)
cargo run -- fmt file.yrm                  # auto-format
cargo run -- lsp                           # start LSP server
cargo run -- repl                          # interactive REPL
```

### Native binaries

```bash
clang -x ir out.ll -o binary -Wno-override-module
clang -x ir out.ll -o binary -lpthread -Wno-override-module   # if using spawn/channels
```

## Architecture

Yorum compiler — statically typed language emitting textual LLVM IR. Pure Rust, no LLVM library dependency. Self-hosting (`yorum-in-yorum/src/main.yrm`, 5,226 lines, bootstrap fixed-point).

### Pipeline

```
Source → Lexer → Parser → AST → TypeChecker → OwnershipChecker → Monomorphizer → DCE → Codegen → LLVM IR
```

Single-file: `src/lib.rs`. Multi-file: `src/compiler/project.rs` (merges modules with name prefixing).

### Key modules (`src/compiler/`)

| Module | Role |
|---|---|
| `lexer.rs` | Hand-written lexer. Nested `/* */`, string interpolation `"hello {expr}"` |
| `parser.rs` | Recursive descent + Pratt expressions (`parse_expr_bp`). Desugars `+=`, string interpolation |
| `typechecker.rs` | Three-pass: (1) register signatures/structs/enums, (1.5) infer effects, (2) check bodies. ~70 builtins. Prelude: `Option<T>`, `Result<T,E>` |
| `ownership.rs` | Move checker. Copy types: `int float bool char string unit [T] Map Set`. Must-join for `Task` |
| `monomorphize.rs` | Generic elimination. Mangles names: `identity__int`, `Pair__int__float` |
| `dce.rs` | BFS reachability from `main`, removes dead code |
| `codegen/` | Emits LLVM IR (directory module). `mod.rs` core + `builtins.rs`, `pipelines.rs`, `statements.rs`, `types.rs`. See codegen details below |
| `project.rs` | Multi-file: reads `yorum.toml`, resolves deps, merges with name prefixing |
| `module_resolver.rs` | Discovers `.yrm` files, validates `module` declarations |

### Codegen details

- `fresh_temp()`/`fresh_label()` generate unique SSA names per function
- `emit_function` resets `temp_counter`, `label_counter`, `block_terminated`, `current_block` — all four must reset
- `block_terminated` prevents emitting after `ret`/`br`
- Arrays: fat pointers `{ ptr, i64, i64 }` (data, len, cap). Use `emit_fat_ptr_load`/`emit_fat_ptr_init`/`emit_fat_ptr_field_load`/`emit_fat_ptr_field_store` helpers
- Arrays use reference semantics: `emit_expr` on array Ident returns alloca ptr (not a load)
- `array_elem_types: HashMap<String, String>` tracks element type per array variable
- Closures: `{ ptr, ptr }` pairs (fn_ptr + env_ptr)
- Enums: `{ i32 }` (no data) or `{ i32, [N x i8] }` (tagged union)
- Structs: LLVM struct types, accessed via `getelementptr`
- `push()` grows via `realloc` when `len == cap`. Aggregates use `memcpy`
- Unit-typed bindings skip alloca/store (`alloca void` is invalid)
- Map: 48 bytes `{keys, vals, flags, cap, size, tombstones}`, Set: 40 bytes `{keys, flags, cap, size, tombstones}`
- Spawn: env struct + wrapper fn + `pthread_create`. Size via LLVM sizeof idiom (not raw sums)
- Contract `requires`/`ensures`: conditional branches, abort on failure
- Pipelines (`.iter().map().filter()...`): fused into single loop by `emit_for_pipeline()`. Terminators (`.collect()`, `.fold()`, etc.) emit own loops
- Printf format strings: global constants with manually counted byte sizes — update if changing
- Math builtins: inline LLVM intrinsics (no wrappers — would shadow C symbols)

### Language design

- Semicolons; `{}` blocks. Logical: `and`, `or`, `not`
- Explicit types: `let x: int = 5;`
- Contracts: `requires`/`ensures` (runtime), `effects` (compile-time), `pure fn`
- Effects: `io`, `fs`, `net`, `time`, `env`, `concurrency`
- `spawn { }` + must-join `Task`. Channels via `chan()`/`send()`/`recv()`
- Bitwise: `& | ^ << >>`. `>>` parsed as two `Gt` tokens (avoids generic `>>` conflict)
- Iterators: `.iter().map().filter().enumerate().zip().take().skip()` in for-loops; `.collect()/.fold()/.any()/.all()/.find()/.reduce()` as expressions
- `Option<T>`, `Result<T,E>` prelude. `?` operator. Match exhaustiveness for enums
- `Map<K,V>`, `Set<T>` — keys must be hashable primitives
- Structs/enums use move semantics

### Tests

- Unit: inline `#[cfg(test)]` per module
- Integration: `tests/integration_tests.rs` via `yorum::compile_to_ir`, `yorum::typecheck`
- Multi-file: `yorum::compile_project()` with temp dirs

### Examples

`examples/*.yrm` — 32 examples. New features MUST add examples. Verify full cycle: compile, link with clang, run.

## Self-hosting

Bootstrap: `yorum-in-yorum/src/main.yrm` (arena-based AST, no generics/closures/traits).

```bash
cd yorum-in-yorum
cargo run --manifest-path ../Cargo.toml -- build -o yorumc.ll
clang -x ir yorumc.ll -o yorumc -Wno-override-module
./yorumc src/main.yrm -o gen1.ll && clang -x ir gen1.ll -o yorumc_gen1 -Wno-override-module
./yorumc_gen1 src/main.yrm -o gen2.ll && diff gen1.ll gen2.ll  # identical
```

## Git Workflow

- Branches: `feature/<descriptive-name>`. No version numbers in branch names.
- Releases: Tag `vX.Y.Z`, push tag. CI builds binaries for 5 targets.
