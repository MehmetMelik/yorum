# Yorum Language Specification

**Version:** 0.2.0

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
struct  enum  module  use  const  pub  pure  own  impl  trait  Self
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
(  )  {  }  [  ]  <  >
,  :  ;  .  &  _  |
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

- **Structs:** Named product types with ordered fields. May be generic (`struct Pair<T, U>`).
- **Enums:** Tagged unions (sum types) with optional payload per variant. May be generic.
- **Arrays:** `[T]` — homogeneous sequences (future).
- **References:** `&T` (immutable), `&mut T` (mutable).
- **Owned:** `own T` — explicit ownership marker.
- **Function types:** `fn(T, U) -> V` — type of closures and function values.
- **Generic types:** `Name<T, U>` — parameterized struct/enum with concrete type arguments.

### 4.3 Self Type
Inside `trait` definitions and `impl` blocks, `Self` refers to the implementing
type.  It is resolved to the concrete type during type checking.

### 4.4 Type Parameters
Functions, structs, and enums may declare type parameters:
```
fn identity<T>(x: T) -> T { return x; }
struct Pair<T, U> { first: T, second: U }
```
Type arguments are inferred at call sites.  The compiler monomorphizes all
generic code — each unique instantiation produces a separate concrete definition.
No generics survive to LLVM IR.

### 4.5 No Implicit Coercion
There is no implicit widening, narrowing, or type coercion.  An `int` cannot be
used where a `float` is expected without an explicit cast function.

### 4.6 Named Types
Struct and enum names create new nominal types.  Two structs with identical
fields but different names are distinct types.

## 5. Declarations

### 5.1 Functions
```
[pub] [pure] fn name[<TypeParams>](param: Type, ...) -> ReturnType
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
- Type parameters (e.g., `<T, U>`) make the function generic.

### 5.2 Structs
```
[pub] struct Name[<TypeParams>] {
    field1: Type,
    field2: Type,
}
```

### 5.3 Enums
```
[pub] enum Name[<TypeParams>] {
    Variant1,
    Variant2(Type),
    Variant3(Type, Type),
}
```

### 5.4 Constants
```
[pub] const NAME: Type = expr;
```

### 5.5 Impl Blocks
```
impl Name {
    fn method(self: &Name, ...) -> ReturnType { body }
}
```

Methods are declared inside `impl` blocks.  The receiver must be an explicit
`self` parameter (typically `self: &Name`).  Method calls use dot syntax
(`value.method(args)`).  Methods compile to top-level functions with mangled
names (`@Name_method`).

### 5.6 Trait Declarations
```
trait Name {
    fn required_method(self: &Self, ...) -> ReturnType;
    fn default_method(self: &Self) -> ReturnType { body }
}
```

Traits define interfaces.  Methods ending with `;` are required; methods with
a block body provide defaults.  `Self` refers to the implementing type.

### 5.7 Trait Implementations
```
impl TraitName for TypeName {
    fn required_method(self: &TypeName, ...) -> ReturnType { body }
}
```

The compiler verifies all required methods are present with matching signatures.
Dispatch is always static — no vtables, no runtime cost.

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
| 8 | `()` `.field` `.method()` `[]` | postfix |

### 7.2 Logical Operators
Yorum uses **keyword-based** logical operators (`and`, `or`, `not`) rather than
symbolic ones (`&&`, `||`, `!`).  This eliminates ambiguity with bitwise
operators and makes code more LLM-readable.

### 7.3 Method Calls
When `.IDENT` is followed by `(`, it is parsed as a method call rather than a
field access.  The receiver becomes the implicit first argument:
```
p.distance()       // calls Point_distance(&p)
p.scale(2)         // calls Point_scale(&p, 2)
```

### 7.4 Closures
Closure expressions capture variables from their enclosing scope:
```
|param: Type, ...| -> ReturnType { body }
```

Closures have type `fn(T, ...) -> U`.  The `|` delimiter is unambiguous because
Yorum uses `or` keyword for logical disjunction.

Closures compile to `{ fn_ptr, env_ptr }` pairs.  The environment struct holds
captured values by copy.  Closures can be stored in variables, passed to
functions, and called indirectly:
```
let f: fn(int) -> int = |x: int| -> int { return x + offset; };
let result: int = f(42);
```

### 7.5 No Implicit Null
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
| `fn(T) -> U` | `ptr` (opaque pointer to `{ ptr, ptr }` pair) |
| Local variables | `alloca` + `load`/`store` |
| Function calls | `call` instruction |
| Method calls | `call @Type_method(ptr %self, ...)` |
| If/else | Conditional `br` with labeled basic blocks |
| While loops | Loop header + conditional `br` + back-edge |
| Structs | LLVM struct types with `getelementptr` |
| Enums | Tagged union: `{ i32, [N x i8] }` |
| Generic types | Monomorphized: `Pair<int, float>` → `%Pair__int__float` |
| Closures | `{ ptr, ptr }` (fn_ptr + env_ptr), env struct via `getelementptr` |
| Indirect calls | Extract fn_ptr from closure pair, `call` via function pointer |

### 11.2 SSA Construction
The compiler uses the **alloca/load/store** pattern.  Every local variable is
stack-allocated with `alloca` in the function entry block.  LLVM's `mem2reg`
optimization pass promotes these to SSA registers automatically.

### 11.3 Monomorphization
Generic functions, structs, and enums are eliminated before codegen.  The
monomorphization pass:
1. Collects all concrete instantiations (e.g., `identity<int>`, `Pair<int, float>`)
2. Clones each generic declaration with type variables substituted
3. Rewrites call sites and type references to use mangled names
4. Removes the original generic declarations

Name mangling scheme: type arguments are joined with `__` — `identity<int>` →
`identity__int`, `Pair<int, float>` → `Pair__int__float`.

### 11.4 Compilation Pipeline
```
source.yrm → Lexer → Tokens → Parser → AST → TypeChecker → OwnershipChecker → Monomorphizer → Codegen → LLVM IR
```

### 11.5 Producing Native Binaries
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

### v0.2 (Done)
- Generics / type parameters with monomorphization
- Closures / lambda expressions with environment capture
- `impl` blocks for methods on structs
- Trait system with static dispatch

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
