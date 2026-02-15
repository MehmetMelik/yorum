# Yorum

**LLM-first, deterministic, LLVM-compiled programming language.**

Yorum is a statically typed language designed from the ground up for safe
autonomous refactoring by AI agents. Every syntactic construct maps 1:1 to an
AST node, the AST round-trips losslessly through JSON, and the compiler
emits LLVM IR for native binary production.

## Quick Start

```bash
# Build the compiler
cargo build --release

# Compile a Yorum program to LLVM IR
./target/release/yorum compile examples/fibonacci.yrm -o fib.ll

# Produce a native binary (requires clang)
clang -x ir fib.ll -o fib -Wno-override-module
./fib

# Type-check only
./target/release/yorum check examples/hello.yrm

# Export AST as JSON
./target/release/yorum ast examples/hello.yrm
```

## Language Overview

```
module main;

struct Point {
    x: int,
    y: int,
}

// Pure function with contracts
pure fn manhattan(px: int, py: int) -> int
    requires px >= 0
    requires py >= 0
    ensures result >= 0
{
    return px + py;
}

// Effect-annotated function
fn display(value: int) -> unit
    effects io
{
    print_int(value);
}

fn main() -> int {
    let p: Point = Point { x: 3, y: 4 };
    let dist: int = manhattan(p.x, p.y);
    display(dist);
    return 0;
}
```

## Design Principles

| Principle | How Yorum achieves it |
|---|---|
| **No ambiguous grammar** | LL(1) recursive descent; Pratt expression parsing |
| **No context-sensitive parsing** | Struct init resolved by 2-token lookahead |
| **No implicit coercion** | All type conversions must be explicit |
| **No hidden control flow** | No exceptions, no fallthrough, no implicit returns |
| **No undefined behavior** | Deterministic evaluation order, memory safety |
| **Machine-readable contracts** | `requires`/`ensures`/`effects` on every function |
| **AST ↔ Source round-trip** | Full AST serializable to/from JSON with spans |

## Type System

| Type | Description | LLVM |
|---|---|---|
| `int` | 64-bit signed integer | `i64` |
| `float` | 64-bit IEEE 754 | `double` |
| `bool` | Boolean | `i1` |
| `string` | UTF-8 string | `ptr` |
| `unit` | No value (void) | `void` |
| `&T` | Immutable reference | `ptr` |
| `&mut T` | Mutable reference | `ptr` |
| `own T` | Explicit ownership | `ptr` |

## Compiler Pipeline

```
source.yrm → Lexer → Tokens → Parser → AST → TypeChecker → OwnershipChecker → Codegen → LLVM IR
                                         ↓
                                    JSON (serde)
```

Each phase is independent and testable:

- **Lexer** (`src/compiler/lexer.rs`) — Hand-written, supports nested block comments
- **Parser** (`src/compiler/parser.rs`) — Recursive descent + Pratt expressions
- **Type Checker** (`src/compiler/typechecker.rs`) — Two-pass: register declarations, then check bodies
- **Ownership Checker** (`src/compiler/ownership.rs`) — Move tracking, borrow safety
- **Codegen** (`src/compiler/codegen.rs`) — Textual LLVM IR emission (alloca/load/store)

## Project Structure

```
yorum/
├── Cargo.toml
├── SPEC.md                 # Language specification
├── GRAMMAR.ebnf             # Formal grammar
├── src/
│   ├── main.rs              # CLI entry point
│   ├── lib.rs               # Library API (compile_to_ir, typecheck, etc.)
│   └── compiler/
│       ├── span.rs           # Source location tracking
│       ├── token.rs          # Token definitions
│       ├── lexer.rs          # Lexical analysis
│       ├── ast.rs            # AST node types (serde-serializable)
│       ├── parser.rs         # Recursive descent parser
│       ├── typechecker.rs    # Static type checking
│       ├── ownership.rs      # Ownership/move analysis
│       └── codegen.rs        # LLVM IR generation
├── runtime/
│   └── runtime.c            # C runtime support library
├── examples/
│   ├── hello.yrm            # Hello world
│   ├── fibonacci.yrm        # Recursive + iterative Fibonacci
│   ├── structs.yrm          # Struct types and field access
│   ├── pattern_match.yrm    # Enums and match statements
│   └── effects.yrm          # Purity and effect annotations
└── tests/
    └── integration_tests.rs  # End-to-end compiler tests
```

## LLVM IR Lowering

The compiler uses the **alloca/load/store** pattern for local variables. LLVM's
`mem2reg` pass automatically promotes these to efficient SSA registers.

Example: `fn add(a: int, b: int) -> int { return a + b; }` produces:

```llvm
define i64 @add(i64 %a, i64 %b) {
entry:
  %a.addr = alloca i64
  store i64 %a, ptr %a.addr
  %b.addr = alloca i64
  store i64 %b, ptr %b.addr
  %t0 = load i64, ptr %a.addr
  %t1 = load i64, ptr %b.addr
  %t2 = add i64 %t0, %t1
  ret i64 %t2
}
```

## Testing

```bash
cargo test           # 52 unit + integration tests
cargo test -- --nocapture  # See test output
```

## Roadmap

See [SPEC.md](SPEC.md) for the full evolution plan. Key upcoming features:

- **v0.2** — Generics, closures, `impl` blocks, trait system
- **v0.3** — Arrays/slices, string ops, `for` loops, nested pattern matching
- **v0.4** — Structured concurrency, runtime contract verification, package manager
- **v1.0** — Self-hosting compiler, standard library, LSP server
