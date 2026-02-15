# Yorum Language Specification

**Version:** 0.1.0

## 1. Overview

Yorum is a statically typed, compiled programming language designed for
**deterministic execution**, **structural clarity**, and **LLM-first interaction**.
Every syntactic construct maps 1:1 to an AST node.  The AST is serializable to
JSON, enabling lossless source-to-AST-to-source round-tripping.

Yorum compiles to LLVM IR and can produce native binaries via any LLVM toolchain.

## 2. Design Principles

| Principle | Mechanism |
|---|---|
| No ambiguous grammar | LL(1) recursive descent; Pratt precedence climbing for expressions |
| No context-sensitive parsing | Struct init disambiguated by 2-token lookahead (`Ident {` → `Ident :`) |
| No implicit type coercion | All conversions must be explicit |
| No hidden control flow | No exceptions; no fallthrough; no implicit returns |
| No undefined behavior | Signed overflow is defined (wrapping); null pointers impossible without `unsafe` |
| Memory safety by default | Ownership model prevents use-after-move; references prevent dangling pointers |
| Deterministic execution | No unsequenced side effects; evaluation order is strictly left-to-right |
| Machine-readable contracts | `requires`, `ensures`, `effects` clauses on every function |

## 3. Lexical Structure

### 3.1 Character Set
Source files are UTF-8.  Only ASCII characters appear in keywords and operators.

### 3.2 Comments
- **Line comments:** `// ...` until end of line.
- **Block comments:** `/* ... */`, nestable.
- **Doc comments:** `/// ...` — stored as structured metadata in the AST.

### 3.3 Keywords
```
fn  let  mut  return  if  else  while  for  in  match
struct  enum  module  use  const  pub  pure  own  impl
and  or  not  true  false
requires  ensures  effects
int  float  bool  string  unit
```

### 3.4 Operators and Punctuation
```
+  -  *  /  %
==  !=  <  >  <=  >=
and  or  not
=  ->  =>
(  )  {  }  [  ]
,  :  ;  .  &  _
```

## 4. Type System

### 4.1 Primitive Types

| Type | Description | LLVM representation |
|---|---|---|
| `int` | 64-bit signed integer | `i64` |
| `float` | 64-bit IEEE 754 double | `double` |
| `bool` | Boolean value | `i1` |
| `string` | UTF-8 string (pointer to null-terminated data) | `ptr` |
| `unit` | No meaningful value (like `void`) | `void` |

### 4.2 Composite Types

- **Structs:** Named product types with ordered fields.
- **Enums:** Tagged unions (sum types) with optional payload per variant.
- **Arrays:** `[T]` — homogeneous sequences (future).
- **References:** `&T` (immutable), `&mut T` (mutable).
- **Owned:** `own T` — explicit ownership marker.

### 4.3 No Implicit Coercion
There is no implicit widening, narrowing, or type coercion.  An `int` cannot be
used where a `float` is expected without an explicit cast function.

### 4.4 Named Types
Struct and enum names create new nominal types.  Two structs with identical
fields but different names are distinct types.

## 5. Declarations

### 5.1 Functions
```
[pub] [pure] fn name(param: Type, ...) -> ReturnType
    [requires expr]
    [ensures expr]
    [effects name, ...]
{
    body
}
```

- `pure` marks a function as free of side effects.
- `requires` specifies preconditions (machine-readable contracts).
- `ensures` specifies postconditions; the identifier `result` refers to the
  return value.
- `effects` lists the effect capabilities the function may use.

### 5.2 Structs
```
[pub] struct Name {
    field1: Type,
    field2: Type,
}
```

### 5.3 Enums
```
[pub] enum Name {
    Variant1,
    Variant2(Type),
    Variant3(Type, Type),
}
```

### 5.4 Constants
```
[pub] const NAME: Type = expr;
```

## 6. Statements

| Statement | Syntax |
|---|---|
| Let binding | `let [mut] name: Type = expr;` |
| Assignment | `target = expr;` |
| Return | `return expr;` |
| If | `if expr { ... } [else { ... }]` |
| While | `while expr { ... }` |
| Match | `match expr { pattern => { ... }, ... }` |
| Expression | `expr;` |

All statements are semicolon-terminated (except block-bearing statements like
`if`, `while`, `match` which end with `}`).

## 7. Expressions

### 7.1 Precedence Table (low → high)

| Level | Operators | Associativity |
|---|---|---|
| 1 | `or` | left |
| 2 | `and` | left |
| 3 | `==` `!=` | left |
| 4 | `<` `>` `<=` `>=` | left |
| 5 | `+` `-` | left |
| 6 | `*` `/` `%` | left |
| 7 | `-` (unary) `not` | prefix |
| 8 | `()` `.` `[]` | postfix |

### 7.2 Logical Operators
Yorum uses **keyword-based** logical operators (`and`, `or`, `not`) rather than
symbolic ones (`&&`, `||`, `!`).  This eliminates ambiguity with bitwise
operators and makes code more LLM-readable.

### 7.3 No Implicit Null
There is no `null`, `nil`, or `None` built into the language.  Optional values
are modeled explicitly with enum types.

## 8. Ownership Model

### 8.1 Principles
- Every value has exactly one owner.
- Assignment **moves** ownership for non-primitive types.
- Using a moved value is a compile-time error.
- Primitive types (`int`, `float`, `bool`) are implicitly copyable.

### 8.2 References
- `&T` creates an immutable borrow.
- `&mut T` creates a mutable borrow.
- A value cannot be moved while it is borrowed.

### 8.3 Explicit Ownership
The `own T` type annotation makes ownership transfer explicit in function
signatures, improving readability for AI agents performing refactoring.

## 9. Error Handling

Yorum has **no exceptions**.  Error handling uses explicit return types:

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

## 10. Module System

```
module my_module;

use std.io;
use math.vector;
```

Modules provide namespace isolation.  `pub` controls visibility.

## 11. LLVM Lowering

### 11.1 Strategy
The compiler emits textual LLVM IR.  Key mappings:

| Yorum construct | LLVM IR |
|---|---|
| `int` | `i64` |
| `float` | `double` |
| `bool` | `i1` |
| Local variables | `alloca` + `load`/`store` |
| Function calls | `call` instruction |
| If/else | Conditional `br` with labeled basic blocks |
| While loops | Loop header + conditional `br` + back-edge |
| Structs | LLVM struct types with `getelementptr` |
| Enums | Tagged union: `{ i32, [N x i8] }` |

### 11.2 SSA Construction
The compiler uses the **alloca/load/store** pattern.  Every local variable is
stack-allocated with `alloca` in the function entry block.  LLVM's `mem2reg`
optimization pass promotes these to SSA registers automatically.

### 11.3 Compilation Pipeline
```
source.yrm → Lexer → Tokens → Parser → AST → TypeChecker → OwnershipChecker → Codegen → LLVM IR
```

### 11.4 Producing Native Binaries
```bash
yorum compile program.yrm -o program.ll
llc -filetype=obj program.ll -o program.o
clang program.o -o program
./program
```

## 12. AST Serialization

The entire AST is serializable to JSON via `serde`.  This enables:

- **Lossless round-tripping:** `source → AST → JSON → AST → source`
- **AI agent interaction:** LLMs can manipulate the AST as structured data
- **Tooling:** Linters, formatters, and refactoring tools operate on the
  JSON AST

## 13. Roadmap

### v0.2
- Generics / type parameters
- Closures / lambda expressions
- `impl` blocks for methods on structs
- Trait system for ad-hoc polymorphism

### v0.3
- Array and slice types with bounds checking
- String operations (concatenation, slicing, formatting)
- `for` loops with iterators
- Pattern matching on nested structs

### v0.4
- Deterministic concurrency (structured concurrency primitives)
- Runtime contract verification (requires/ensures checking)
- Module system with separate compilation
- Package manager

### v1.0
- Self-hosting compiler
- Standard library (io, collections, math, networking)
- LSP server for editor integration
- Formal verification of the ownership checker
