# Yorum Language Specification

**Version:** 0.5.0

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
int  float  bool  char  string  unit
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
| `char` | 8-bit character | `i8` |
| `string` | UTF-8 string (pointer to null-terminated data) | `ptr` |
| `unit` | No meaningful value (like `void`) | `void` |

### 4.2 Composite Types

- **Structs:** Named product types with ordered fields. May be generic (`struct Pair<T, U>`).
- **Enums:** Tagged unions (sum types) with optional payload per variant. May be generic.
- **Arrays:** `[T]` — homogeneous, heap-allocated dynamic arrays with runtime bounds checking, `push`/`pop` support.
- **Maps:** `Map` — hash map with `string` keys and `int` values. Built-in type backed by FNV-1a hashing with linear probing.
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

### 7.5.1 Dynamic Array Operations
Arrays are dynamic — they can grow and shrink at runtime:
```
let mut arr: [int] = [1, 2, 3];
push(arr, 4);           // arr is now [1, 2, 3, 4]
let last: int = pop(arr);  // last is 4, arr is [1, 2, 3]
```

`push(arr, val)` appends an element to the end of the array, growing the backing
buffer via `realloc` when capacity is exceeded (doubling strategy).  `pop(arr)`
removes and returns the last element; it aborts if the array is empty.

Arrays are represented internally as `{ ptr, i64, i64 }` fat pointers (data
pointer + length + capacity).

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

### 7.7 String and Character Operations

**String functions:**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `str_len(s)` | `(string) -> int` | yes | Returns the byte length of `s` |
| `str_concat(a, b)` | `(string, string) -> string` | no | Returns a new string `a + b` (heap-allocated) |
| `str_eq(a, b)` | `(string, string) -> bool` | yes | Returns `true` if `a` and `b` are equal |
| `str_charAt(s, i)` | `(string, int) -> char` | yes | Returns the character at byte index `i` (bounds-checked) |
| `str_sub(s, start, len)` | `(string, int, int) -> string` | no | Returns a substring (heap-allocated) |
| `str_from_char(c)` | `(char) -> string` | no | Returns a single-character string (heap-allocated) |

**Character classification:**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `char_is_alpha(c)` | `(char) -> bool` | yes | True if `c` is a letter (a-z, A-Z) |
| `char_is_digit(c)` | `(char) -> bool` | yes | True if `c` is a digit (0-9) |
| `char_is_whitespace(c)` | `(char) -> bool` | yes | True if `c` is space, tab, newline, or carriage return |

Character literals use single quotes: `'a'`, `'\n'`, `'\t'`, `'\0'`, `'\\'`.

### 7.7.1 Type Casting

| Function | Signature | Pure | Description |
|---|---|---|---|
| `char_to_int(c)` | `(char) -> int` | yes | Zero-extends char to 64-bit integer |
| `int_to_char(n)` | `(int) -> char` | yes | Truncates integer to 8-bit character |
| `int_to_str(n)` | `(int) -> string` | no | Converts integer to decimal string |
| `str_to_int(s)` | `(string) -> int` | yes | Parses decimal string to integer |
| `int_to_float(n)` | `(int) -> float` | yes | Converts integer to float |
| `float_to_int(f)` | `(float) -> int` | yes | Truncates float to integer |

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
| `char` | `i8` |
| `[T]` (array) | `{ ptr, i64, i64 }` fat pointer (data pointer + length + capacity) |
| `Map` | `ptr` (hash table: keys, values, flags, capacity, size) |
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
Arrays are heap-allocated fat pointers: `{ ptr, i64, i64 }`.  The first field
points to a contiguous `malloc`'d buffer of elements; the second holds the
length; the third holds the capacity.

Every index operation emits a call to a bounds-check helper that aborts on
out-of-range access (negative index or index >= length).

Array parameters are passed as `ptr` (opaque pointer to the fat pointer struct).
`len(arr)` loads the length field directly from the fat pointer.

`push` checks if length equals capacity, doubles capacity via `realloc` when
needed, then stores the element at `data[len]` and increments length.  For
aggregate element types (structs, enums), `memcpy` is used instead of `store`.
`pop` checks length > 0, decrements length, and loads the element at the new
end position.

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

**Output:**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `print_int(n)` | `(int) -> unit` | no | Print an integer followed by newline |
| `print_float(f)` | `(float) -> unit` | no | Print a float followed by newline |
| `print_bool(b)` | `(bool) -> unit` | no | Print `true` or `false` followed by newline |
| `print_str(s)` | `(string) -> unit` | no | Print a string followed by newline |
| `print_char(c)` | `(char) -> unit` | no | Print a character (no newline) |
| `print_err(s)` | `(string) -> unit` | no | Print a string to stderr followed by newline |

**String and character operations:**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `str_len(s)` | `(string) -> int` | yes | Return the byte length of a string |
| `str_concat(a, b)` | `(string, string) -> string` | no | Concatenate two strings (heap-allocates result) |
| `str_eq(a, b)` | `(string, string) -> bool` | yes | Compare two strings for equality |
| `str_charAt(s, i)` | `(string, int) -> char` | yes | Return character at byte index (bounds-checked) |
| `str_sub(s, start, len)` | `(string, int, int) -> string` | no | Return substring (heap-allocated) |
| `str_from_char(c)` | `(char) -> string` | no | Return single-character string |
| `char_is_alpha(c)` | `(char) -> bool` | yes | True if letter (a-z, A-Z) |
| `char_is_digit(c)` | `(char) -> bool` | yes | True if digit (0-9) |
| `char_is_whitespace(c)` | `(char) -> bool` | yes | True if space, tab, newline, or CR |

**Type casting:**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `char_to_int(c)` | `(char) -> int` | yes | Zero-extend char to integer |
| `int_to_char(n)` | `(int) -> char` | yes | Truncate integer to char |
| `int_to_str(n)` | `(int) -> string` | no | Convert integer to decimal string |
| `str_to_int(s)` | `(string) -> int` | yes | Parse decimal string to integer |
| `int_to_float(n)` | `(int) -> float` | yes | Convert integer to float |
| `float_to_int(f)` | `(float) -> int` | yes | Truncate float to integer |

**String utilities (v0.6):**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `str_contains(s, sub)` | `(string, string) -> bool` | yes | Check if string contains substring |
| `str_index_of(s, sub)` | `(string, string) -> int` | yes | Find index of substring, -1 if not found |
| `str_starts_with(s, prefix)` | `(string, string) -> bool` | yes | Check if string starts with prefix |
| `str_ends_with(s, suffix)` | `(string, string) -> bool` | yes | Check if string ends with suffix |
| `str_trim(s)` | `(string) -> string` | no | Trim leading/trailing whitespace |
| `str_replace(s, from, to)` | `(string, string, string) -> string` | no | Replace all occurrences |
| `str_split(s, delim)` | `(string, string) -> [string]` | no | Split string by delimiter |
| `str_to_upper(s)` | `(string) -> string` | no | Convert to uppercase |
| `str_to_lower(s)` | `(string) -> string` | no | Convert to lowercase |
| `str_repeat(s, n)` | `(string, int) -> string` | no | Repeat string n times |

**Math (v0.6):**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `abs_int(x)` | `(int) -> int` | yes | Absolute value of integer |
| `abs_float(x)` | `(float) -> float` | yes | Absolute value of float |
| `min_int(a, b)` | `(int, int) -> int` | yes | Minimum of two integers |
| `max_int(a, b)` | `(int, int) -> int` | yes | Maximum of two integers |
| `min_float(a, b)` | `(float, float) -> float` | yes | Minimum of two floats |
| `max_float(a, b)` | `(float, float) -> float` | yes | Maximum of two floats |
| `sqrt(x)` | `(float) -> float` | yes | Square root |
| `pow(base, exp)` | `(float, float) -> float` | yes | Exponentiation |
| `sin(x)` | `(float) -> float` | yes | Sine (radians) |
| `cos(x)` | `(float) -> float` | yes | Cosine (radians) |
| `tan(x)` | `(float) -> float` | yes | Tangent (radians) |
| `floor(x)` | `(float) -> float` | yes | Round down |
| `ceil(x)` | `(float) -> float` | yes | Round up |
| `round(x)` | `(float) -> float` | yes | Round to nearest |
| `log(x)` | `(float) -> float` | yes | Natural logarithm |
| `log10(x)` | `(float) -> float` | yes | Base-10 logarithm |
| `exp(x)` | `(float) -> float` | yes | Exponential (e^x) |

**Array operations:**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `len(arr)` | `([T]) -> int` | yes | Return the length of an array |
| `push(arr, val)` | `([T], T) -> unit` | no | Append element, growing buffer if needed |
| `pop(arr)` | `([T]) -> T` | no | Remove and return last element (aborts if empty) |
| `slice(arr, start, end)` | `([T], int, int) -> [T]` | no | Extract sub-array [start, end) |
| `concat_arrays(a, b)` | `([T], [T]) -> [T]` | no | Concatenate two arrays |
| `reverse(arr)` | `([T]) -> [T]` | no | Return new reversed array |
| `contains_int(arr, val)` | `([int], int) -> bool` | yes | Linear search in int array |
| `contains_str(arr, val)` | `([string], string) -> bool` | yes | Linear search in string array |
| `sort_int(arr)` | `([int]) -> [int]` | no | Return sorted copy of int array |
| `sort_str(arr)` | `([string]) -> [string]` | no | Return sorted copy of string array |

**File I/O:**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `file_read(path)` | `(string) -> string` | no | Read entire file to string |
| `file_write(path, content)` | `(string, string) -> bool` | no | Write string to file, returns success |
| `file_exists(path)` | `(string) -> bool` | no | Check if file exists |
| `file_append(path, content)` | `(string, string) -> bool` | no | Append string to file |

**Process and environment:**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `args()` | `() -> [string]` | no | Return command-line arguments |
| `exit(code)` | `(int) -> unit` | no | Terminate with exit code |
| `env_get(name)` | `(string) -> string` | no | Get environment variable (empty if unset) |
| `read_line()` | `() -> string` | no | Read a line from stdin |
| `print_flush(s)` | `(string) -> unit` | no | Print without newline, flush stdout |
| `time_ms()` | `() -> int` | no | Current time in milliseconds |

**HashMap:**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `map_new()` | `() -> Map` | no | Create a new empty hash map |
| `map_set(m, key, val)` | `(Map, string, int) -> unit` | no | Insert or update a key-value pair |
| `map_get(m, key)` | `(Map, string) -> int` | yes | Look up a key (aborts if not found) |
| `map_has(m, key)` | `(Map, string) -> bool` | yes | Check if a key exists |
| `map_size(m)` | `(Map) -> int` | yes | Return number of entries |
| `map_remove(m, key)` | `(Map, string) -> bool` | no | Remove key, returns true if found |
| `map_keys(m)` | `(Map) -> [string]` | no | Collect all keys into an array |
| `map_values(m)` | `(Map) -> [int]` | no | Collect all values into an array |

**Concurrency:**

| Function | Signature | Pure | Description |
|---|---|---|---|
| `chan()` | `() -> Chan<T>` | no | Create a new synchronous channel |
| `send(ch, val)` | `(Chan<T>, T) -> unit` | no | Send a value into a channel (blocks until received) |
| `recv(ch)` | `(Chan<T>) -> T` | no | Receive a value from a channel (blocks until sent) |

`len`, `push`, `pop`, `args`, and `exit` are special-cased in the type checker —
they are handled directly in call expression checking rather than being
registered as normal functions. `slice`, `concat_arrays`, and `reverse` are
also special-cased to preserve the array element type. `str_split` is
special-cased because it returns `[string]`.

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

### v0.5 (Done)
- `char` type with literals and classification builtins
- Type casting builtins (`int_to_str`, `str_to_int`, `char_to_int`, `int_to_char`, `int_to_float`, `float_to_int`)
- Extended string/char operations (`str_charAt`, `str_sub`, `str_from_char`)
- Dynamic arrays with `push`/`pop` (realloc-based growth)
- File I/O (`file_read`, `file_write`, `print_err`)
- Process interaction (`args()`, `exit()`)
- HashMap (`map_new`, `map_set`, `map_get`, `map_has`)
- Self-hosting compiler (5,226 lines of Yorum, bootstrap fixed-point achieved)

### v0.6 (Done)
- Standard library builtins: math (17 functions), string utilities (10), collection operations (11), enhanced I/O (6)

### v0.7
- LSP server for editor integration

### v0.8
- Formal verification of the ownership checker

### v0.9
- Networking (TCP/UDP sockets, HTTP client)

### v1.0
- Stable language specification and ABI
- Production-ready toolchain
