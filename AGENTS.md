# Repository Guidelines

## Project Structure & Module Organization
- `src/` contains the Rust implementation:
  - `src/compiler/` for the language pipeline (`lexer.rs`, `parser.rs`, `typechecker.rs`, `ownership.rs`, `monomorphize.rs`, `dce.rs`, `codegen/`, etc.).
  - `src/lsp/` for LSP server and transport.
  - `src/main.rs` for CLI entrypoints, `src/lib.rs` for library APIs.
- `tests/integration_tests.rs` holds end-to-end compiler/runtime behavior tests.
- `examples/` contains runnable `.yrm` language examples.
- `runtime/runtime.c` contains C runtime support used by generated LLVM IR.
- `editors/vscode/` contains the VS Code extension.

## Build, Test, and Development Commands
- `cargo build --release` builds the compiler binary.
- `cargo run -- compile examples/hello.yrm -o out.ll` compiles Yorum source to LLVM IR.
- `cargo run -- run examples/hello.yrm` compiles, links, and runs a program.
- `cargo test` runs all unit + integration tests.
- `cargo test --all-targets` matches CI test scope.
- `cargo clippy --all-targets -- -D warnings` enforces lint cleanliness.
- `cargo fmt --check` verifies formatting (`cargo fmt` to fix).

## Coding Style & Naming Conventions
- Rust style is `rustfmt` default (4-space indentation, trailing commas as formatted).
- Treat clippy warnings as errors.
- Naming:
  - `snake_case` for functions/modules/files.
  - `PascalCase` for structs/enums/traits.
  - Tests should use `test_*` names describing behavior/regression.
- Keep changes focused; avoid unrelated formatting-only edits.

## Testing Guidelines
- Add/extend unit tests near the relevant module when logic is local.
- Add regression and feature behavior tests in `tests/integration_tests.rs`.
- Prefer targeted runs while iterating, for example:
  - `cargo test test_bounds_no_elision_while_body_alias_rebind`
  - `cargo test compiler::parser::tests::test_operator_precedence`
- Before opening a PR, run: `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`.

## Commit & Pull Request Guidelines
- Follow the repository’s commit style: short imperative subject lines like `Fix ...`, `Add ...`, `Bump ...`.
- Branch from `main` using prefixes from `CONTRIBUTING.md` (for example, `feature/...`, `bug/...`, `docs/...`, `test/...`).
- PRs should include:
  - what changed,
  - why it changed,
  - test plan/results,
  - linked issue (for example, `Fixes #12`) when applicable.
