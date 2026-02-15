<p align="center">
  <img src="assets/yorum-icon-256.png" alt="Yorum" width="128" height="128">
</p>

<h1 align="center">Yorum</h1>

<p align="center">
  <a href="https://github.com/MehmetMelik/yorum/actions/workflows/ci.yml"><img src="https://github.com/MehmetMelik/yorum/actions/workflows/ci.yml/badge.svg" alt="CI"></a>
  <a href="https://github.com/MehmetMelik/yorum/releases/latest"><img src="https://img.shields.io/github/v/release/MehmetMelik/yorum?label=release" alt="Release"></a>
  <a href="LICENSE"><img src="https://img.shields.io/badge/license-MIT-blue.svg" alt="License: MIT"></a>
  <img src="https://img.shields.io/badge/lang-Rust-orange.svg" alt="Rust">
  <img src="https://img.shields.io/badge/LLVM-IR%20output-purple.svg" alt="LLVM IR">
</p>

<p align="center">
A statically typed, LLVM-compiled programming language designed for deterministic<br>
execution, structural clarity, and safe autonomous refactoring by AI agents.
</p>

---

Every syntactic construct maps 1:1 to an AST node. The AST serializes losslessly
to JSON. The compiler emits textual LLVM IR — no LLVM library dependency, just
pure Rust.

```
pure fn fib(n: int) -> int
    requires n >= 0
    ensures result >= 0
{
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}
```

## Quick Start

```bash
cargo build --release

# Compile to LLVM IR
./target/release/yorum compile examples/fibonacci.yrm -o fib.ll

# Produce a native binary (requires clang)
clang -x ir fib.ll -o fib -Wno-override-module
./fib
# 0 1 1 2 3 5 8 13 21 34 55
```

## CLI

```
yorum compile <file.yrm> [-o output]   Compile to LLVM IR (stdout if no -o)
yorum check   <file.yrm>               Type-check and ownership-check only
yorum ast     <file.yrm>               Dump the full AST as JSON
```

## Why Yorum

Most languages were designed for humans writing code by hand. Yorum is designed
for the emerging workflow where LLMs read, write, and refactor code autonomously.

**No ambiguity.** The grammar is LL(1) with Pratt expression parsing. Struct
initializers are disambiguated from blocks by a deterministic 2-token lookahead.
There is no context-sensitive parsing anywhere.

**No hidden behavior.** No exceptions, no implicit returns, no fallthrough, no
operator overloading, no implicit type coercion, no null. Error handling uses
explicit enum return types. Evaluation order is strictly left-to-right.

**Machine-readable contracts.** Functions carry `requires`/`ensures`/`effects`
clauses that are parsed into the AST as structured data — not comments, not
strings.

**Round-trip guarantee.** Every AST node carries exact source spans. The entire
tree serializes to JSON via serde. An AI agent can parse a file to JSON,
manipulate the AST, and reconstruct the source losslessly.

**Deterministic ownership.** Move semantics prevent use-after-free. Immutable
bindings are the default. Mutable state is explicit (`let mut`). Every value has
exactly one owner.

## Language Tour

### Types

| Type | Description | LLVM |
|---|---|---|
| `int` | 64-bit signed integer | `i64` |
| `float` | 64-bit IEEE 754 double | `double` |
| `bool` | Boolean | `i1` |
| `string` | UTF-8 string | `ptr` |
| `unit` | No value | `void` |
| `&T` | Immutable reference | `ptr` |
| `&mut T` | Mutable reference | `ptr` |
| `own T` | Explicit ownership | `ptr` |

No implicit coercion between any types.

### Functions and Contracts

```
// Pure function — no side effects, deterministic
pure fn safe_div(a: int, b: int) -> int
    requires b != 0
    ensures result * b <= a
{
    return a / b;
}

// Impure function — declares its effects
fn log_value(x: int) -> unit
    effects io
{
    print_int(x);
}
```

`pure` marks a function as side-effect-free. `requires` and `ensures` are
preconditions and postconditions stored in the AST. `effects` declares which
capabilities the function uses. The `result` identifier refers to the return
value inside `ensures` clauses.

### Structs

```
struct Point {
    x: int,
    y: int,
}

fn main() -> int {
    let p: Point = Point { x: 3, y: 4 };
    print_int(p.x);    // 3
    print_int(p.y);    // 4
    return 0;
}
```

### Enums and Pattern Matching

```
enum Direction {
    North,
    South,
    East,
    West,
}

fn delta_x(dir: int) -> int {
    match dir {
        2 => { return 1; },
        3 => { return -1; },
        _ => { return 0; },
    }
}
```

Match arms always use block bodies (`=> { ... }`). Wildcard `_` catches
remaining cases. No fallthrough.

### Control Flow

```
fn abs(x: int) -> int {
    if x < 0 {
        return 0 - x;
    }
    return x;
}

fn sum_to(n: int) -> int {
    let mut total: int = 0;
    let mut i: int = 1;
    while i <= n {
        total = total + i;
        i = i + 1;
    }
    return total;
}
```

All blocks are delimited with `{ }`. All statements end with `;`. Logical
operators are keywords: `and`, `or`, `not`.

### Error Handling

No exceptions. Errors are modeled with enums:

```
enum Result {
    Ok(int),
    Err(int),
}

fn divide(a: int, b: int) -> Result {
    if b == 0 {
        return Err(0);
    }
    return Ok(a / b);
}
```

### Operator Precedence (low to high)

| Level | Operators | Associativity |
|---|---|---|
| 1 | `or` | left |
| 2 | `and` | left |
| 3 | `==` `!=` | left |
| 4 | `<` `>` `<=` `>=` | left |
| 5 | `+` `-` | left |
| 6 | `*` `/` `%` | left |
| 7 | `-` (unary), `not` | prefix |
| 8 | `()` `.field` `[i]` | postfix |

## LLVM IR Output

The compiler emits textual LLVM IR using the alloca/load/store pattern. LLVM's
`mem2reg` pass promotes stack slots to SSA registers.

This Yorum function:

```
pure fn fib(n: int) -> int {
    if n <= 1 { return n; }
    return fib(n - 1) + fib(n - 2);
}
```

Produces this LLVM IR:

```llvm
define i64 @fib(i64 %n) {
entry:
  %n.addr = alloca i64
  store i64 %n, ptr %n.addr
  %t0 = load i64, ptr %n.addr
  %t1 = icmp sle i64 %t0, 1
  br i1 %t1, label %then.0, label %ifcont.2
then.0:
  %t2 = load i64, ptr %n.addr
  ret i64 %t2
ifcont.2:
  %t3 = load i64, ptr %n.addr
  %t4 = sub i64 %t3, 1
  %t5 = call i64 @fib(i64 %t4)
  %t6 = load i64, ptr %n.addr
  %t7 = sub i64 %t6, 2
  %t8 = call i64 @fib(i64 %t7)
  %t9 = add i64 %t5, %t8
  ret i64 %t9
}
```

### Producing Native Binaries

```bash
yorum compile program.yrm -o program.ll
clang -x ir program.ll -o program -Wno-override-module
./program
```

Or with separate object file:

```bash
yorum compile program.yrm -o program.ll
llc -filetype=obj program.ll -o program.o
clang program.o -o program
```

## AST as JSON

Every AST node carries exact source positions (`line`, `col`, `start`, `end`
byte offsets). The entire tree serializes to JSON:

```bash
yorum ast examples/hello.yrm
```

```json
{
  "module_name": "hello",
  "uses": [],
  "declarations": [
    {
      "Function": {
        "name": "main",
        "is_pure": false,
        "is_pub": false,
        "params": [],
        "return_type": "Int",
        "contracts": [],
        "body": { "stmts": [...], "span": {...} },
        "span": { "start": 135, "end": 188, "line": 7, "col": 1 }
      }
    }
  ]
}
```

This enables AI agents to parse Yorum source to structured JSON, manipulate the
tree programmatically, and reconstruct valid source code — with exact span
information preserved.

## Compiler Architecture

```
source.yrm
    |
    v
  Lexer ──────── Vec<Token>
    |
    v
  Parser ─────── AST (Program)  ──── serde ────> JSON
    |
    v
  TypeChecker ── type errors
    |
    v
  OwnershipChecker ── move errors
    |
    v
  Codegen ─────── LLVM IR (text)
    |
    v
  clang ────────── native binary
```

| Phase | Source | What it does |
|---|---|---|
| Lexer | `src/compiler/lexer.rs` | Hand-written, nested `/* */` comments, escape sequences |
| Parser | `src/compiler/parser.rs` | Recursive descent + Pratt expression parsing |
| Type Checker | `src/compiler/typechecker.rs` | Two-pass: register declarations, then check bodies |
| Ownership | `src/compiler/ownership.rs` | Move tracking, prevents use-after-move |
| Codegen | `src/compiler/codegen.rs` | Textual LLVM IR, alloca/load/store pattern |

The compiler is ~4,600 lines of Rust with only `serde` and `serde_json` as
dependencies.

## Testing

```bash
cargo test                    # 52 tests (34 unit + 18 integration)
cargo test compiler::lexer    # tests in one module
cargo test test_fibonacci     # single test by name
```

## Documentation

- **[SPEC.md](SPEC.md)** — Full language specification
- **[GRAMMAR.ebnf](GRAMMAR.ebnf)** — Formal grammar in EBNF notation
- **[examples/](examples/)** — Working programs that compile to native binaries

## Roadmap

| Version | Features |
|---|---|
| **v0.2** | Generics, closures, `impl` blocks, trait system |
| **v0.3** | Arrays/slices, string operations, `for` loops, nested pattern matching |
| **v0.4** | Structured concurrency, runtime contract verification, package manager |
| **v1.0** | Self-hosting compiler, standard library, LSP server |

## License

[MIT](LICENSE)
