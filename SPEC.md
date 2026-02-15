# Yorum Language Specification

**Version:** 0.4.0

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
fn  let  mut  return  if  else  while  for  in  match  spawn
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
- **Arrays:** `[T]` — homogeneous, heap-allocated sequences with runtime bounds checking.
- **Tasks:** `Task<T>` — handle to a spawned concurrent task that produces a value of type `T`.
- **Channels:** `Chan<T>` — synchronized channel for sending values of type `T` between tasks.
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

- `pure` marks a function as free of side effects.  Enforced at compile time:
  a pure function cannot call impure functions or use `spawn`.
- `requires` specifies preconditions.  Checked at runtime on function entry —
  violation prints an error message and aborts.  Contract expressions must have
  type `bool`.
- `ensures` specifies postconditions.  Checked at runtime before each `return`.
  The identifier `result` is bound to the return value and may be used in the
  expression.
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
| For | `for name in expr { ... }` |
| Match | `match expr { pattern => { ... }, ... }` |
| Expression | `expr;` |

All statements are semicolon-terminated (except block-bearing statements like
`if`, `while`, `for`, `match` which end with `}`).

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

### 7.5 Array Literals and Indexing
Array literals are written with square brackets:
```
let nums: [int] = [10, 20, 30];
```

All elements must have the same type.  The element type is inferred from the
first element.  Empty array literals are rejected (cannot infer type).

Indexing uses postfix square brackets:
```
let first: int = nums[0];
```

Every index operation is bounds-checked at runtime.  Out-of-bounds access prints
an error message and aborts the program.

Mutable arrays support index assignment:
```
let mut arr: [int] = [1, 2, 3];
arr[0] = 100;
```

The built-in function `len(arr)` returns the number of elements as an `int`.

### 7.6 For Loops
For loops iterate over arrays:
```
for x in nums {
    total = total + x;
}
```

The loop variable is bound to each element in order.  Its type is the array's
element type.  The loop variable is scoped to the loop body.  Only array types
are iterable — there is no general iterator protocol.

### 7.7 String Operations
Three built-in functions operate on strings:

| Function | Signature | Pure | Description |
|---|---|---|---|
| `str_len(s)` | `(string) -> int` | yes | Returns the byte length of `s` |
| `str_concat(a, b)` | `(string, string) -> string` | no | Returns a new string `a + b` (heap-allocated) |
| `str_eq(a, b)` | `(string, string) -> bool` | yes | Returns `true` if `a` and `b` are equal |

### 7.8 Pattern Matching
Match statements destructure values against patterns:
```
match expr {
    pattern => { body },
    ...
}
```

Supported pattern forms:

| Pattern | Syntax | Description |
|---|---|---|
| Wildcard | `_` | Matches any value, binds nothing |
| Binding | `name` | Matches any value and binds it to `name` |
| Literal | `42`, `true`, `"str"` | Matches if value equals the literal |
| Variant | `Name(p1, p2)` | Matches enum variant, recursively matches sub-patterns |

Patterns may be nested — a variant sub-pattern can itself be a variant, literal,
wildcard, or binding.  The compiler validates that variant names exist and that
the number of sub-patterns matches the variant's field count.

Arms are checked top-to-bottom.  The first matching arm executes.

### 7.9 Spawn and Join
The `spawn` expression creates a new concurrent task:
```
let t: Task<int> = spawn {
    return 42;
};
let result: int = t.join();
```

`spawn` takes a block and returns `Task<T>`, where `T` is the return type of
the block.  The block executes concurrently on a new OS thread (via `pthreads`).
Variables from the enclosing scope are captured by copy into the task, using the
same mechanism as closures.

`.join()` is a method on `Task<T>` that blocks until the task completes and
returns the result of type `T`.

**Structured concurrency guarantee:** the ownership checker enforces that every
`Task` variable must be `.join()`'d before the scope in which it was declared
exits.  Forgetting to join a task is a compile-time error.

Pure functions cannot use `spawn` — it is a side effect.

### 7.10 Channels
Channels provide synchronized communication between tasks:
```
let ch: Chan<int> = chan();
send(ch, 42);
let val: int = recv(ch);
```

- `chan()` creates a new synchronous (unbuffered) channel.
- `send(ch, val)` sends a value into the channel.  Blocks until a receiver is
  ready.
- `recv(ch)` receives a value from the channel.  Blocks until a sender is ready.

Channel operations are built-in functions, not methods.

### 7.11 No Implicit Null
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

### 10.1 Module Declarations
Every source file begins with a module declaration:
```
module math;
```

The module name must match the file's path relative to the `src/` directory
(e.g., `src/math.yrm` must declare `module math;`).

### 10.2 Imports
```
use math;
use util.helpers;
```

`use` declarations import `pub` declarations from another module.  The module
path uses `.` as a separator (matching the filesystem hierarchy).  Imported
names are available unqualified at call sites — the compiler rewrites them
internally to prefixed names (e.g., `add` from `module math` becomes
`math__add`).

### 10.3 Visibility
All declarations support the `pub` modifier.  Only `pub` declarations are
visible to other modules.  Within the declaring module, all declarations are
visible regardless of `pub`.

### 10.4 Multi-file Compilation
Projects with multiple source files use a `yorum.toml` manifest:
```toml
[package]
name = "my_project"
version = "0.1.0"
```

The `src_dir` field defaults to `"src"`.  All `.yrm` files under the source
directory are discovered automatically.  The main module is the one containing
`fn main()`.

Modules are merged at the AST level into a single `Program` before type
checking.  Only `pub` declarations from imported modules are included, with
names prefixed by the module path to prevent collisions.

### 10.5 CLI Commands
```bash
yorum init [name]                       # scaffold a new project
yorum build [-o output]                 # build project from yorum.toml
```

`yorum init` creates `yorum.toml` and `src/main.yrm`.  If a name argument is
given, a subdirectory is created; otherwise the current directory is used.

`yorum build` finds `yorum.toml` by searching upward from the current directory,
resolves all modules, and emits LLVM IR.

## 11. LLVM Lowering

### 11.1 Strategy
The compiler emits textual LLVM IR.  Key mappings:

| Yorum construct | LLVM IR |
|---|---|
| `int` | `i64` |
| `float` | `double` |
| `bool` | `i1` |
| `string` | `ptr` (null-terminated C string) |
| `fn(T) -> U` | `ptr` (opaque pointer to `{ ptr, ptr }` pair) |
| `[T]` (array) | `{ ptr, i64 }` fat pointer (data pointer + length) |
| Local variables | `alloca` + `load`/`store` |
| Function calls | `call` instruction |
| Method calls | `call @Type_method(ptr %self, ...)` |
| If/else | Conditional `br` with labeled basic blocks |
| While loops | Loop header + conditional `br` + back-edge |
| For loops | Index counter + conditional `br` + element load + back-edge |
| Match | Cascading `icmp`/`br` chains; variant tag extraction for enums |
| Structs | LLVM struct types with `getelementptr` |
| Enums | Tagged union: `{ i32, [N x i8] }` |
| Generic types | Monomorphized: `Pair<int, float>` → `%Pair__int__float` |
| Closures | `{ ptr, ptr }` (fn_ptr + env_ptr), env struct via `getelementptr` |
| Indirect calls | Extract fn_ptr from closure pair, `call` via function pointer |
| `Task<T>` | `ptr` (pointer to task control block: `{ pthread_t, env_ptr }`) |
| `Chan<T>` | `ptr` (pointer to channel struct: mutex + condvar + value + flag) |
| `spawn { }` | `pthread_create` with wrapper function + captured environment |
| `.join()` | `pthread_join` + result extraction from environment |
| `requires`/`ensures` | Conditional `br` to `@__yorum_contract_fail` on violation |

### 11.2 Arrays
Arrays are heap-allocated fat pointers: `{ ptr, i64 }`.  The first field points
to a contiguous `malloc`'d buffer of elements; the second holds the length.

Every index operation emits a call to a bounds-check helper that aborts on
out-of-range access (negative index or index >= length).

Array parameters are passed as `ptr` (opaque pointer to the fat pointer struct).
`len(arr)` loads the length field directly from the fat pointer.

For loops compile to an index-counting loop: an `i64` counter starts at 0,
increments each iteration, and the loop exits when the counter reaches the
array length.

### 11.3 String Builtins
`str_len`, `str_concat`, and `str_eq` are emitted as LLVM IR function
definitions that call C library functions (`strlen`, `strcpy`, `strcat`,
`strcmp`, `malloc`).  No separate C runtime is linked for string operations.

### 11.4 Runtime Contracts
`requires` clauses emit a conditional branch after parameter allocas at function
entry.  If the condition evaluates to false, control transfers to a failure
label that calls `@__yorum_contract_fail` with an error message and aborts.

`ensures` clauses emit checks before each `ret` instruction.  The return value
is stored into a `result` alloca so it can be referenced by the contract
expression.  After all checks pass, the value is reloaded and returned.

### 11.5 Concurrency
`spawn` compiles similarly to closures: captured variables are packed into a
heap-allocated environment struct, and a wrapper function `@__spawn_N` is
emitted that unpacks them and executes the block.  A task control block
`{ pthread_t, env_ptr }` is allocated, and `pthread_create` launches the
wrapper on a new thread.

`.join()` calls `pthread_join` on the stored thread handle.

Channels are implemented as LLVM IR helper functions (`@__yorum_chan_create`,
`@__yorum_chan_send`, `@__yorum_chan_recv`) using pthread mutex and condvar for
synchronization.  No separate C runtime is required.

### 11.6 SSA Construction
The compiler uses the **alloca/load/store** pattern.  Every local variable is
stack-allocated with `alloca` in the function entry block.  LLVM's `mem2reg`
optimization pass promotes these to SSA registers automatically.

### 11.7 Monomorphization
Generic functions, structs, and enums are eliminated before codegen.  The
monomorphization pass:
1. Collects all concrete instantiations (e.g., `identity<int>`, `Pair<int, float>`)
2. Clones each generic declaration with type variables substituted
3. Rewrites call sites and type references to use mangled names
4. Removes the original generic declarations

Name mangling scheme: type arguments are joined with `__` — `identity<int>` →
`identity__int`, `Pair<int, float>` → `Pair__int__float`.

### 11.8 Compilation Pipeline
```
source.yrm → Lexer → Tokens → Parser → AST → TypeChecker → OwnershipChecker → Monomorphizer → Codegen → LLVM IR
```

### 11.9 Producing Native Binaries
```bash
yorum compile program.yrm -o program.ll
clang -x ir program.ll -o program -Wno-override-module
./program
```

Programs using `spawn` or channels require linking with pthreads:
```bash
clang -x ir program.ll -o program -lpthread -Wno-override-module
```

## 12. AST Serialization

The entire AST is serializable to JSON via `serde`.  This enables:

- **Lossless round-tripping:** `source → AST → JSON → AST → source`
- **AI agent interaction:** LLMs can manipulate the AST as structured data
- **Tooling:** Linters, formatters, and refactoring tools operate on the
  JSON AST

## 13. Built-in Functions

All built-in functions are available without imports.

| Function | Signature | Pure | Description |
|---|---|---|---|
| `print_int(n)` | `(int) -> unit` | no | Print an integer followed by newline |
| `print_float(f)` | `(float) -> unit` | no | Print a float followed by newline |
| `print_bool(b)` | `(bool) -> unit` | no | Print `true` or `false` followed by newline |
| `print_str(s)` | `(string) -> unit` | no | Print a string followed by newline |
| `len(arr)` | `([T]) -> int` | yes | Return the length of an array |
| `str_len(s)` | `(string) -> int` | yes | Return the byte length of a string |
| `str_concat(a, b)` | `(string, string) -> string` | no | Concatenate two strings (heap-allocates result) |
| `str_eq(a, b)` | `(string, string) -> bool` | yes | Compare two strings for equality |
| `chan()` | `() -> Chan<T>` | no | Create a new synchronous channel |
| `send(ch, val)` | `(Chan<T>, T) -> unit` | no | Send a value into a channel (blocks until received) |
| `recv(ch)` | `(Chan<T>) -> T` | no | Receive a value from a channel (blocks until sent) |

`len` is special-cased in the type checker — it is not a registered function but
is handled directly in call expression checking.  It works on any array type.

`chan`, `send`, and `recv` are also special-cased in the type checker.  The
channel element type for `chan()` defaults to `int` when not otherwise
constrained by the binding context.

## 14. Roadmap

### v0.2 (Done)
- Generics / type parameters with monomorphization
- Closures / lambda expressions with environment capture
- `impl` blocks for methods on structs
- Trait system with static dispatch

### v0.3 (Done)
- Array type with heap allocation and runtime bounds checking
- String operations (`str_len`, `str_concat`, `str_eq`)
- `for` loops over arrays
- Pattern matching: wildcard, binding, literal, and nested variant patterns

### v0.4 (Done)
- Runtime contract verification: `requires`/`ensures` checked at runtime, `pure` enforced at compile time
- Multi-file compilation: `yorum.toml` manifest, module discovery, `yorum build` and `yorum init`
- Structured concurrency: `spawn`/`.join()` backed by pthreads, must-join enforcement
- Channels: `chan()`/`send()`/`recv()` with mutex+condvar synchronization

### v1.0
- Self-hosting compiler
- Standard library (io, collections, math, networking)
- LSP server for editor integration
- Formal verification of the ownership checker
