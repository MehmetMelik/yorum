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
yorum compile <file.yrm> [-o output] [-g]   Compile to LLVM IR (stdout if no -o)
yorum check   <file.yrm>                    Type-check and ownership-check only
yorum ast     <file.yrm>                    Dump the full AST as JSON
yorum build   [-o output]                   Build multi-file project (requires yorum.toml)
yorum init    [name]                        Scaffold a new project
yorum run     <file.yrm> [-- args...]       Compile, link, and execute in one step
yorum fmt     [--check] <file.yrm>...       Auto-format source files
yorum install                               Fetch and cache dependencies
yorum update  [name]                        Update dependencies
yorum repl                                  Interactive expression evaluator
yorum lsp                                   Start LSP server (stdin/stdout)
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
strings. Preconditions and postconditions are verified at runtime.

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
| `char` | 8-bit character | `i8` |
| `unit` | No value | `void` |
| `&T` | Immutable reference | `ptr` |
| `&mut T` | Mutable reference | `ptr` |
| `own T` | Explicit ownership | `ptr` |
| `(T, U)` | Tuple type | `%tuple.T.U` struct |
| `[T]` | Dynamic array (heap-allocated) | `{ ptr, i64, i64 }` |
| `Map<K, V>` | Generic hash map (hashable keys) | `ptr` |
| `Set<T>` | Generic hash set (hashable elements) | `ptr` |
| `Task<T>` | Concurrent task handle | `ptr` |
| `Chan<T>` | Synchronous channel | `ptr` |
| `fn(T) -> U` | Function type (closures) | `ptr` |

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

`pure` marks a function as side-effect-free — enforced at compile time (a pure
function cannot call impure functions or use `spawn`). `requires` and `ensures`
are preconditions and postconditions verified at runtime — a violation prints an
error and aborts. `effects` declares which capabilities the function uses. The
`result` identifier refers to the return value inside `ensures` clauses.

### Structs and Impl Blocks

```
struct Point {
    x: int,
    y: int,
}

impl Point {
    fn get_x(self: &Point) -> int {
        return self.x;
    }

    fn manhattan(self: &Point) -> int {
        return self.x + self.y;
    }
}

fn main() -> int {
    let p: Point = Point { x: 3, y: 4 };
    print_int(p.get_x());       // 3
    print_int(p.manhattan());    // 7
    return 0;
}
```

Methods are declared inside `impl` blocks. The receiver is an explicit `self`
parameter. Method calls use dot syntax (`p.get_x()`). Methods compile to
top-level functions with mangled names (`@Point_get_x`).

### Traits

```
trait Describable {
    fn describe(self: &Self) -> int;
}

impl Describable for Point {
    fn describe(self: &Point) -> int {
        return self.x + self.y;
    }
}
```

Traits define interfaces that types must implement. `Self` refers to the
implementing type inside trait and impl blocks. Dispatch is static — no vtables,
no runtime cost. The compiler verifies that all required methods are implemented
with matching signatures.

### Generics

```
fn identity<T>(x: T) -> T {
    return x;
}

struct Pair<T, U> {
    first: T,
    second: U,
}

fn main() -> int {
    let x: int = identity(42);
    let p: Pair<int, float> = Pair { first: 1, second: 2.0 };
    print_int(p.first);      // 1
    print_float(p.second);   // 2.0
    return x;
}
```

Type parameters work on functions, structs, and enums. Type arguments are
inferred at call sites. The compiler monomorphizes all generic code before
codegen — `identity<int>` becomes `identity__int`, `Pair<int, float>` becomes
`Pair__int__float`. No generics survive to LLVM IR.

### Closures

```
fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x);
}

fn main() -> int {
    let offset: int = 10;
    let f: fn(int) -> int = |x: int| -> int { return x + offset; };
    return apply(f, 32);   // 42
}
```

Closures capture variables from their enclosing scope. They compile to
`{ fn_ptr, env_ptr }` pairs — the environment struct holds captured values,
and the function takes the environment as its first argument. Closures can be
passed to higher-order functions via `fn(T) -> U` types.

### Dynamic Arrays and For Loops

```
fn sum_array(nums: [int]) -> int {
    let mut total: int = 0;
    for x in nums {
        total = total + x;
    }
    return total;
}

fn main() -> int {
    let nums: [int] = [10, 20, 30, 40, 50];
    print_int(len(nums));          // 5
    print_int(nums[0]);            // 10
    print_int(sum_array(nums));    // 150

    let mut arr: [int] = [1, 2, 3];
    push(arr, 4);                  // [1, 2, 3, 4]
    let last: int = pop(arr);     // 4, arr is [1, 2, 3]
    arr[0] = 100;
    print_int(arr[0]);             // 100

    // Array repeat: allocate and fill in one shot
    let zeros: [int] = [0; 100];   // 100 zero-initialized ints
    let ones: [int] = [1; 50];     // 50 ones

    return 0;
}
```

Arrays are heap-allocated fat pointers (`{ ptr, i64, i64 }` — data pointer +
length + capacity). `len(arr)` returns the length. `push(arr, val)` appends an
element (growing the buffer via `realloc` when needed). `pop(arr)` removes and
returns the last element. `[value; count]` creates an array of `count` copies of `value` in one allocation (zero values use `memset`, non-zero values use a fill loop).

Index access includes runtime bounds checking. The compiler elides bounds checks inside `for i in 0..len(arr)` loops when `i` is provably in-bounds (no array mutation in the loop body).

`for x in arr { ... }` iterates over array elements. `for x in arr.iter() { ... }` is equivalent. `for i in 0..n { ... }` iterates with a counter from 0 to n-1. `for i in 0..=n { ... }` iterates inclusively from 0 to n.

### Iterator Pipelines

```
fn main() -> int {
    let nums: [int] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Combinators in for-loops: filter, map, enumerate, zip, take, skip
    let mut sum: int = 0;
    for x in nums.iter().filter(|v: int| -> bool { return v % 2 == 0; }).map(|v: int| -> int { return v * v; }) {
        sum += x;
    }
    print_int(sum);    // 220 (4+16+36+64+100)

    // Terminators as standalone expressions
    let evens: [int] = nums.iter().filter(|v: int| -> bool { return v % 2 == 0; }).collect();
    let total: int = nums.iter().fold(0, |acc: int, x: int| -> int { return acc + x; });
    let has_big: bool = nums.iter().any(|v: int| -> bool { return v > 8; });
    let first: Option<int> = nums.iter().find(|v: int| -> bool { return v > 5; });

    // Range pipelines
    let squares: [int] = (0..10).iter().map(|x: int| -> int { return x * x; }).collect();

    // Chain: concatenate two iterables
    let a: [int] = [1, 2, 3];
    let b: [int] = [4, 5, 6];
    let all: [int] = a.iter().chain(b).collect();           // [1, 2, 3, 4, 5, 6]

    // Flat_map and flatten
    let nested: [[int]] = [[1, 2], [3, 4], [5]];
    let flat: [int] = nested.iter().flatten().collect();     // [1, 2, 3, 4, 5]

    // Sum, count, position
    let s: int = nums.iter().sum();                          // 55
    let n: int = nums.iter().filter(|v: int| -> bool { return v > 5; }).count();  // 5
    let pos: Option<int> = nums.iter().position(|v: int| -> bool { return v > 7; });  // Some(7)

    // Take_while and rev
    let prefix: [int] = nums.iter().take_while(|v: int| -> bool { return v < 4; }).collect();  // [1, 2, 3]
    let reversed: [int] = nums.iter().rev().take(3).collect();  // [10, 9, 8]

    // String chars
    let mut vowels: int = 0;
    for c in "hello world".chars() {
        if c == 'o' { vowels += 1; }
    }

    // Unbounded ranges (must have take or take_while)
    let first_10: [int] = (0..).iter().take(10).collect();   // [0, 1, ..., 9]

    // Set and Map iteration
    let s: Set<int> = set_new();
    set_add(s, 10);
    set_add(s, 20);
    let set_sum: int = s.iter().sum();                       // 30

    let m: Map<string, int> = map_new();
    map_set(m, "a", 1);
    map_set(m, "b", 2);
    let entries: int = m.iter().count();                     // 2

    return 0;
}
```

**Sources:** `.iter()` on arrays, ranges (`(0..n)`, `(0..=n)`, `(0..)`), Sets, and Maps; `.chars()`
on strings. **Combinators** (in for-loops and before terminators): `.map(f)`, `.filter(f)`,
`.enumerate()`, `.zip(arr)`, `.take(n)`, `.skip(n)`, `.chain(arr)`, `.flat_map(f)`, `.flatten()`,
`.take_while(f)`, `.rev()`. **Terminators** (standalone expressions): `.collect()`, `.fold(init, f)`,
`.any(f)`, `.all(f)`, `.find(f)`, `.reduce(f)`, `.sum()`, `.count()`, `.position(f)`. All pipelines
are fused into a single LLVM loop — no intermediate allocations (except `.collect()`), no iterator
structs. Closures must be inline.

### String Interpolation

```
fn main() -> int {
    let name: string = "Alice";
    let age: int = 30;

    // Embed variables and expressions in strings with {expr}
    print_str("Hello, {name}! You are {age} years old.");
    print_str("{10 + 32} is the answer.");

    // Any expression works: function calls, arithmetic, array ops
    let arr: [int] = [1, 2, 3];
    print_str("Array has {len(arr)} elements.");

    // Escape braces with {{ and }}
    print_str("Use {{expr}} for interpolation.");
    return 0;
}
```

String interpolation with `{expr}` syntax is desugared by the parser into
`str_concat` chains with `to_str` conversions. `int`, `float`, `bool`, and
`char` values are automatically converted. Literal braces are escaped with
`{{` and `}}`.

### Tuples

```
fn swap(a: int, b: int) -> (int, int) {
    return (b, a);
}

fn main() -> int {
    // Tuple literal and field access
    let pair: (int, string) = (42, "hello");
    print_int(pair.0);       // 42
    print_str(pair.1);       // hello

    // Tuple destructuring in let bindings
    let (a, b): (int, int) = swap(1, 2);
    print_int(a);            // 2
    print_int(b);            // 1

    // Multi-element tuples
    let triple: (int, int, int) = (10, 20, 30);
    print_int(triple.0 + triple.1 + triple.2);   // 60
    return 0;
}
```

Tuples group values of different types. Fields are accessed by index (`t.0`,
`t.1`). Destructuring binds tuple elements to individual variables. Tuples
compile to LLVM struct types (e.g., `%tuple.int.string`).

### Option and Result

```
fn find(arr: [int], target: int) -> Option<int> {
    let mut i: int = 0;
    while i < len(arr) {
        if arr[i] == target {
            return Some(i);
        }
        i += 1;
    }
    return None;
}

fn divide(a: int, b: int) -> Result<int, string> {
    if b == 0 {
        return Err("division by zero");
    }
    return Ok(a / b);
}

fn main() -> int {
    // Option methods
    let idx: Option<int> = find([10, 20, 30], 20);
    print_int(idx.unwrap());         // 1
    print_bool(idx.is_some());       // true
    print_bool(idx.is_none());       // false

    // Result methods
    let res: Result<int, string> = divide(42, 7);
    print_int(res.unwrap());         // 6
    print_bool(res.is_ok());         // true

    let err: Result<int, string> = divide(1, 0);
    print_bool(err.is_err());        // true
    let msg: string = err.unwrap_err();
    print_str(msg);                  // division by zero

    // Pattern matching on Option/Result
    match idx {
        Some(i) => { print_int(i); }
        None => { print_str("not found"); }
    }
    return 0;
}
```

`Option<T>` and `Result<T, E>` are prelude types — available without declaration.
Construct with `Some(val)`/`None` and `Ok(val)`/`Err(e)`. Methods: `.unwrap()`
(aborts on wrong variant), `.is_some()`/`.is_none()`, `.is_ok()`/`.is_err()`,
`.unwrap_err()`. Use `?` for early return (see [Error Handling with ? Operator](#error-handling-with--operator)).

### Char Type and String Operations

```
let c: char = 'A';
let n: int = char_to_int(c);                      // 65
let alpha: bool = char_is_alpha(c);               // true

let s: string = str_concat("hello", " world");    // "hello world"
let ch: char = str_charAt(s, 0);                  // 'h'
let sub: string = str_sub(s, 0, 5);              // "hello"
let eq: bool = str_eq("abc", "abc");              // true
```

Built-in string functions: `str_len`, `str_concat`, `str_eq`, `str_charAt`,
`str_sub`, `str_from_char`. Character classification: `char_is_alpha`,
`char_is_digit`, `char_is_whitespace`. Type casting: `char_to_int`,
`int_to_char`, `int_to_str`, `str_to_int`, `int_to_float`, `float_to_int`.

### Standard Library (v0.6)

**Math:** `abs_int`, `abs_float`, `min_int`, `max_int`, `min_float`, `max_float`,
`sqrt`, `pow`, `sin`, `cos`, `tan`, `floor`, `ceil`, `round`, `log`, `log10`, `exp`
— all pure, backed by LLVM intrinsics and libm.

```
let x: float = sqrt(16.0);          // 4.0
let y: float = pow(2.0, 10.0);      // 1024.0
let a: int = abs_int(-5);           // 5
let m: int = min_int(3, 7);         // 3
```

**String utilities:** `str_contains`, `str_index_of`, `str_starts_with`,
`str_ends_with`, `str_trim`, `str_replace`, `str_split`, `str_to_upper`,
`str_to_lower`, `str_repeat`.

```
let parts: [string] = str_split("a,b,c", ",");  // ["a", "b", "c"]
let upper: string = str_to_upper("hello");       // "HELLO"
let found: bool = str_contains("hello", "ell");  // true
let idx: int = str_index_of("hello", "ll");      // 2
```

**Collection operations:** `slice`, `concat_arrays`, `reverse`, `contains_int`,
`contains_str`, `sort_int`, `sort_str`, `map_keys`, `map_values`, `map_size`,
`map_remove`, `clear`.

```
let arr: [int] = [3, 1, 2];
let sorted: [int] = sort_int(arr);               // [1, 2, 3]
let rev: [int] = reverse(arr);                    // [2, 1, 3]
let sub: [int] = slice(arr, 0, 2);               // [3, 1]
let found: bool = contains_int(arr, 2);           // true
```

**Enhanced I/O:** `file_exists`, `file_append`, `read_line`, `print_flush`,
`env_get`, `time_ms`.

```
print_flush("Enter name: ");
let name: string = read_line();
let home: string = env_get("HOME");
let t: int = time_ms();
```

### Networking (v0.9)

**TCP sockets:**

```
fn main() -> int {
    let fd: int = tcp_connect("example.com", 80);
    if fd < 0 { return 1; }
    let sent: int = tcp_send(fd, "GET / HTTP/1.0\r\nHost: example.com\r\n\r\n");
    let resp: string = tcp_recv(fd, 4096);
    tcp_close(fd);
    print_str(resp);
    return 0;
}
```

`tcp_connect`, `tcp_listen`, `tcp_accept`, `tcp_send`, `tcp_recv`, `tcp_close` —
full TCP client and server support. Socket fds are plain `int` values; `-1`
indicates error.

**UDP sockets:** `udp_socket`, `udp_bind`, `udp_send_to`, `udp_recv_from`.

**DNS:** `dns_resolve("hostname")` returns an IP address string.

**HTTP client:**

```
let body: string = http_get("http://example.com");
let resp: string = http_post("http://api.example.com/data", "{\"key\": 42}");
let custom: string = http_request("PUT", "http://api.example.com/item", "Content-Type: application/json", "{\"val\": 1}");
```

`http_get` and `http_post` are convenience wrappers around `http_request`. HTTP/1.0
with `Connection: close`. Plain HTTP only (no TLS).

### File I/O and Process

```
let content: string = file_read("input.txt");
file_write("output.txt", content);
print_err("error message");       // writes to stderr

let argv: [string] = args();      // command-line arguments
exit(1);                           // terminate with exit code
```

### HashMap and Set

```
let m: Map<string, int> = map_new();     // or bare `Map` for string->int compat
map_set(m, "key", 42);
let v: int = map_get(m, "key");          // 42
let ok: bool = map_has(m, "key");        // true
let n: int = map_size(m);               // 1
let keys: [string] = map_keys(m);       // ["key"]

let ids: Map<int, string> = map_new();   // int keys, string values
let s: Set<int> = set_new();
set_add(s, 42);
let found: bool = set_has(s, 42);       // true
```

Maps and sets are generic with keys/elements restricted to hashable primitives
(`int`, `string`, `char`, `bool`). Backed by FNV-1a hashing with linear probing
and tombstone-aware load factor.

### Error Handling with ? Operator

```
fn parse_config() -> Result<int, string> {
    return Ok(42);
}

fn run() -> Result<int, string> {
    let val: int = parse_config()?;  // propagates Err automatically
    return Ok(val + 1);
}
```

The `?` operator extracts the value from `Option<T>` or `Result<T, E>`,
returning early with `None` or `Err(e)` if the inner value is an error variant.
The enclosing function must return a compatible `Option` or `Result` type.

### Structured Concurrency

```
fn main() -> int {
    let t1: Task<int> = spawn {
        return 10;
    };
    let t2: Task<int> = spawn {
        return 32;
    };
    let a: int = t1.join();
    let b: int = t2.join();
    print_int(a + b);    // 42
    return 0;
}
```

`spawn { ... }` creates a new OS thread and returns a `Task<T>` handle. `.join()`
blocks until the task completes and returns its result. Spawn blocks capture
variables from the enclosing scope by copy (same mechanism as closures).

The ownership checker enforces **structured concurrency**: every `Task` must be
`.join()`'d before its scope exits. Forgetting to join is a compile-time error.

Channels provide synchronized communication between tasks:

```
let ch: Chan<int> = chan();
send(ch, 42);
let val: int = recv(ch);
```

### Multi-file Projects

```bash
yorum init myproject     # creates myproject/yorum.toml + src/main.yrm
cd myproject
yorum build -o out.ll    # discovers all .yrm files, resolves imports
```

Each source file declares its module name. `pub` controls cross-module visibility:

```
// src/math.yrm
module math;

pub fn add(a: int, b: int) -> int {
    return a + b;
}
```

```
// src/main.yrm
module main;
use math;

fn main() -> int {
    print_int(add(10, 32));    // 42
    return 0;
}
```

### Enums and Pattern Matching

```
enum Option { Some(int), None }

fn get_or(opt: Option, default_val: int) -> int {
    match opt {
        Some(val) => { return val; }
        None => { return default_val; }
    }
}

fn main() -> int {
    let x: Option = Some(42);
    print_int(get_or(x, 0));    // 42
    return 0;
}
```

Enums support data-carrying variants. Match arms use block bodies (`=> { ... }`)
and can destructure variant payloads into bindings. Wildcard `_` catches
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
        total += i;
        i += 1;
    }
    return total;
}

fn sum_range(n: int) -> int {
    let mut total: int = 0;
    for i in 0..n {
        total += i;
    }
    return total;
}
```

All blocks are delimited with `{ }`. All statements end with `;`. Logical
operators are keywords: `and`, `or`, `not`. Compound assignment operators
(`+=`, `-=`, `*=`, `/=`, `%=`) are supported. `break` and `continue` control
loop flow. Bitwise operators: `&`, `|`, `^`, `<<`, `>>`.

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
| 1 | `..` (range) | non-chainable |
| 2 | `or` | left |
| 3 | `and` | left |
| 4 | `\|` (bitwise OR) | left |
| 5 | `^` (bitwise XOR) | left |
| 6 | `&` (bitwise AND) | left |
| 7 | `==` `!=` | left |
| 8 | `<` `>` `<=` `>=` | left |
| 9 | `<<` `>>` | left |
| 10 | `+` `-` | left |
| 11 | `*` `/` `%` | left |
| 12 | `-` (unary), `not` | prefix |
| 13 | `()` `.field` `.method()` `[i]` | postfix |

## Dependencies

Yorum projects can depend on external packages via git URLs or local paths.

**`yorum.toml`:**
```toml
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
math_utils = { git = "https://github.com/user/math_utils.git", tag = "v1.0.0" }
local_lib = { path = "../my_lib" }
```

**Using a dependency:**
```
module main;
use math_utils;

fn main() -> int {
    let result: int = add(1, 2);
    return result;
}
```

**Commands:**
```bash
yorum install              # Fetch all dependencies, write yorum.lock
yorum update               # Fetch latest versions, regenerate lock file
yorum update math_utils    # Update a single dependency
yorum build                # Build project (auto-installs deps if no lock file)
```

Git dependencies support `tag`, `branch`, or `rev` specifiers (default: `branch = "main"`).
Dependencies are cached in `~/.yorum/cache/`. A `yorum.lock` file records exact
git SHAs for reproducible builds.

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

Programs using `spawn` or channels require linking with pthreads:

```bash
clang -x ir program.ll -o program -lpthread -Wno-override-module
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

## Editor Support (v0.7)

Yorum ships with a built-in LSP server for editor integration:

```bash
yorum lsp    # starts the language server (JSON-RPC over stdin/stdout)
```

**Features:**
- **Diagnostics** — Real-time error reporting as you type (type errors, ownership errors, parse errors)
- **Hover** — Shows type information for variables, functions, structs, and enums
- **Go-to-definition** — Jump to where a variable or function was defined

### VS Code

A VS Code extension is included in `editors/vscode/`:

```bash
cd editors/vscode
npm install
npm run compile
```

Then install the extension in VS Code (symlink or copy `editors/vscode/` to
`~/.vscode/extensions/yorum`). The extension provides syntax highlighting and
connects to the `yorum lsp` server for diagnostics, hover, and go-to-definition.

## Compiler Architecture

```
source.yrm
    |
    v
  Lexer ──────────── Vec<Token>
    |
    v
  Parser ─────────── AST (Program)  ──── serde ────> JSON
    |
    v
  TypeChecker ────── type errors
    |
    v
  OwnershipChecker ─ move errors
    |
    v
  Monomorphizer ──── concrete AST (no generics)
    |
    v
  DCE ──────────────  pruned AST (no dead code)
    |
    v
  Codegen ────────── LLVM IR (text)
    |
    v
  clang ──────────── native binary
```

| Phase | Source | What it does |
|---|---|---|
| Lexer | `src/compiler/lexer.rs` | Hand-written, nested `/* */` comments, escape sequences |
| Parser | `src/compiler/parser.rs` | Recursive descent + Pratt expression parsing |
| Type Checker | `src/compiler/typechecker.rs` | Three-pass: register declarations, infer effects, check bodies |
| Ownership | `src/compiler/ownership.rs` | Move tracking, use-after-move prevention, must-join enforcement for tasks |
| Monomorphizer | `src/compiler/monomorphize.rs` | Eliminates generics by cloning concrete instantiations |
| DCE | `src/compiler/dce.rs` | Dead code elimination via BFS reachability from `main` |
| Codegen | `src/compiler/codegen/` | Textual LLVM IR, alloca/load/store pattern, contract checks, pthread spawn/join. Directory module: `mod.rs` (core), `builtins.rs`, `pipelines.rs`, `statements.rs`, `types.rs` |
| Module Resolver | `src/compiler/module_resolver.rs` | Discovers `.yrm` files, maps filesystem paths to module names |
| Project Builder | `src/compiler/project.rs` | Reads `yorum.toml`, merges modules, runs compilation pipeline |

The Rust compiler is ~26,000 lines of Rust with `serde`, `serde_json`, and `toml`
as dependencies.

## Self-Hosting

The Yorum compiler is **self-hosting** — the compiler is written in Yorum itself
(`yorum-in-yorum/src/main.yrm`, 5,226 lines). It implements a bootstrap subset
of the language (no generics, closures, traits, ownership checking, or multi-file
support) using arena-based AST pools with integer index references.

```bash
# Build the self-hosted compiler with the Rust compiler
cd yorum-in-yorum
cargo run -- build -o yorumc.ll
clang -x ir yorumc.ll -o yorumc -Wno-override-module

# Use it to compile programs
./yorumc examples/fibonacci.yrm -o fib.ll
clang -x ir fib.ll -o fib -Wno-override-module

# Bootstrap: yorumc compiles itself
./yorumc src/main.yrm -o gen1.ll
clang -x ir gen1.ll -o yorumc_gen1 -Wno-override-module
./yorumc_gen1 src/main.yrm -o gen2.ll
diff gen1.ll gen2.ll    # identical — fixed-point achieved
```

## Testing

```bash
cargo test                    # 837 tests (81 unit + 756 integration)
cargo test compiler::lexer    # tests in one module
cargo test test_fibonacci     # single test by name
```

## Documentation

- **[SPEC.md](SPEC.md)** — Full language specification
- **[GRAMMAR.ebnf](GRAMMAR.ebnf)** — Formal grammar in EBNF notation
- **[CHANGELOG.md](CHANGELOG.md)** — Detailed version history with release notes
- **[examples/](examples/)** — Working programs that compile to native binaries (methods, traits, generics, closures, arrays, strings, contracts, concurrency)

## Roadmap

| Version | Features | Status |
|---|---|---|
| **v0.2** | Generics, closures, `impl` blocks, trait system | Done |
| **v0.3** | Arrays, string operations, `for` loops, nested pattern matching | Done |
| **v0.4** | Structured concurrency, runtime contract verification, multi-file compilation | Done |
| **v0.5** | Self-hosting compiler, char type, dynamic arrays, file I/O, HashMap | Done |
| **v0.6** | Standard library builtins: math, string utilities, collections, enhanced I/O | Done |
| **v0.7** | LSP server for editor integration (diagnostics, hover, go-to-definition) | Done |
| **v0.8** | Ownership checker: type-aware move tracking, branch merging, loop safety | Done |
| **v0.9** | Networking: TCP/UDP sockets, DNS resolution, HTTP client (14 builtins) | Done |
| **v1.0** | Stable language specification and ABI, production-ready toolchain | Done |
| **v1.0.2** | Codegen bug fixes: stale PHI labels, HashMap tombstone loop, spawn alignment, unit IR | Done |
| **v1.1** | Ergonomics: compound assignment, bitwise operators, break/continue, range for-loops | Done |
| **v1.2** | String interpolation, tuple types, prelude Option\<T\>/Result\<T,E\> with methods | Done |
| **v1.2.1** | Bug fixes: `to_str` codegen, tuple let from functions, tuple type naming, generic arg checks, interp parser | Done |
| **v1.3** | Generic `Map<K,V>`/`Set<T>`, match exhaustiveness checking, `?` operator for Option/Result | Done |
| **v1.3.1** | Codegen bug fixes: duplicate match labels/allocas. New examples: maps, sets, try operator | Done |
| **v1.3.2** | Codegen fixes: math intrinsic naming, spawn return/join, channel sync, for-loop/tuple alloca duplicates, Option/Result method types. 10 new examples | Done |
| **v1.4** | Effect system enforcement: 6 effect categories (io, fs, net, time, env, concurrency), compile-time checking, effect inference, backward compatibility | Done |
| **v1.4.1** | Restore self-hosting bootstrap, fix 4 compiler bugs (ownership, codegen, typechecker) | Done |
| **v1.5.0** | Tooling & DX: `yorum run`, `yorum repl`, LSP completions/code actions, DWARF debug info (`-g`) | Done |
| **v1.6.0** | Auto-formatter: `yorum fmt` with comment preservation, `--check` for CI | Done |
| **v1.7** | Performance & optimization: inline hints, constant folding, tail calls, heap sort, dead code elimination | Done |
| **v1.8** | Package manager: `yorum install`/`update`, git + path dependencies, lock file, namespace isolation | Done |
| **v1.8.1** | Codegen bug fix: unique alloca names and block scoping for nested variable shadowing | Done |
| **v1.8.2** | Performance: capacity-aware `str_concat` optimization (~1000x faster string building loops) | Done |
| **v1.9-alpha** | Inclusive range (`..=`), `.iter()` on arrays, struct iter dispatch, overflow guard | Done |
| **v1.9-beta** | Iterator pipelines: `.map()`, `.filter()` with fused for-loop codegen | Done |
| **v1.9** | Iterator combinators (`.enumerate()`, `.zip()`, `.take()`, `.skip()`) and terminators (`.collect()`, `.fold()`, `.any()`, `.all()`, `.find()`, `.reduce()`) | Done |
| **v1.9.1** | Range pipeline sources, aggregate codegen fixes, overflow hardening, tuple mangle bug fix, codegen cleanup | Done |
| **v1.10** | Codegen refactor: fat pointer/struct helpers, pipeline deduplication, module extraction into 5 files | Done |
| **v1.11** | Array repeat syntax `[value; count]`, bounds check elision for `for i in 0..len(arr)` loops | Done |
| **v1.12** | Iterator ecosystem: `.chain()`, `.flat_map()`, `.flatten()`, `.take_while()`, `.chars()`, `.rev()`, `.sum()`, `.count()`, `.position()`, unbounded ranges, Set/Map `.iter()`, codegen hardening | Done |
| **v1.12.1** | LSP chain-aware dot-completions with type propagation through iterator pipelines | Done |

## License

[MIT](LICENSE)
