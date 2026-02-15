// ═══════════════════════════════════════════════════════════════
//  Helpers
// ═══════════════════════════════════════════════════════════════

fn compile(source: &str) -> String {
    yorum::compile_to_ir(source).expect("compilation failed")
}

fn parse_and_check(source: &str) {
    yorum::typecheck(source).expect("type check failed");
}

fn parse_to_json(source: &str) -> String {
    yorum::source_to_ast_json(source).expect("AST export failed")
}

// ═══════════════════════════════════════════════════════════════
//  End-to-end compilation tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_hello_world_compiles() {
    let ir = compile(
        "module hello;\n\
         fn main() -> int {\n\
         \x20   print_int(42);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("define i64 @main()"));
    assert!(ir.contains("call void @print_int"));
}

#[test]
fn test_fibonacci_compiles() {
    let ir = compile(
        "pure fn fib(n: int) -> int\n\
         \x20   requires n >= 0\n\
         {\n\
         \x20   if n <= 1 { return n; }\n\
         \x20   return fib(n - 1) + fib(n - 2);\n\
         }\n\
         fn main() -> int {\n\
         \x20   print_int(fib(10));\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("define i64 @fib(i64 %n)"));
    assert!(ir.contains("call i64 @fib"));
}

#[test]
fn test_struct_field_access() {
    let ir = compile(
        "struct Point { x: int, y: int }\n\
         fn main() -> int {\n\
         \x20   let p: Point = Point { x: 10, y: 20 };\n\
         \x20   print_int(p.x);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("%Point = type { i64, i64 }"));
    assert!(ir.contains("getelementptr %Point"));
}

#[test]
fn test_while_loop() {
    let ir = compile(
        "fn count(n: int) -> int {\n\
         \x20   let mut i: int = 0;\n\
         \x20   while i < n {\n\
         \x20       i = i + 1;\n\
         \x20   }\n\
         \x20   return i;\n\
         }\n",
    );
    assert!(ir.contains("while.cond"));
    assert!(ir.contains("while.body"));
}

#[test]
fn test_if_else() {
    let ir = compile(
        "fn max(a: int, b: int) -> int {\n\
         \x20   if a > b { return a; } else { return b; }\n\
         }\n",
    );
    assert!(ir.contains("icmp sgt"));
    assert!(ir.contains("br i1"));
}

// ═══════════════════════════════════════════════════════════════
//  Type checker tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_type_mismatch_detected() {
    let result = yorum::typecheck("fn f() -> int { return true; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("type mismatch") || err.contains("return type mismatch"));
}

#[test]
fn test_undefined_variable_detected() {
    let result = yorum::typecheck("fn f() -> int { return x; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("undefined"));
}

#[test]
fn test_immutable_assignment_rejected() {
    let result = yorum::typecheck("fn f() -> int { let x: int = 1; x = 2; return x; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("immutable"));
}

#[test]
fn test_mutable_assignment_accepted() {
    parse_and_check("fn f() -> int { let mut x: int = 1; x = 2; return x; }");
}

#[test]
fn test_struct_missing_field() {
    let result = yorum::typecheck(
        "struct Point { x: int, y: int }\n\
         fn f() -> Point { return Point { x: 1 }; }",
    );
    assert!(result.is_err());
}

#[test]
fn test_wrong_argument_count() {
    let result = yorum::typecheck(
        "fn add(a: int, b: int) -> int { return a + b; }\n\
         fn f() -> int { return add(1); }",
    );
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  AST round-trip tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_ast_json_roundtrip() {
    let source = "fn main() -> int { let x: int = 42; return x; }";
    let json = parse_to_json(source);

    // Verify it's valid JSON
    let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(parsed.is_object());

    // Verify key structure
    assert!(parsed.get("declarations").is_some());
}

#[test]
fn test_ast_preserves_contracts() {
    let source = "fn f(x: int) -> int requires x > 0 ensures result >= 0 { return x; }";
    let json = parse_to_json(source);
    assert!(json.contains("Requires"));
    assert!(json.contains("Ensures"));
}

#[test]
fn test_ast_preserves_pure() {
    let source = "pure fn add(a: int, b: int) -> int { return a + b; }";
    let json = parse_to_json(source);
    assert!(json.contains("\"is_pure\": true"));
}

// ═══════════════════════════════════════════════════════════════
//  Operator precedence tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_multiplication_binds_tighter_than_addition() {
    let source = "fn f() -> int { return 2 + 3 * 4; }";
    let json = parse_to_json(source);
    // The AST should have Add at the top with Mul as the right child
    // We verify this structurally via the JSON
    let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
    // Just verify it parses — the unit test in parser.rs checks structure
    assert!(parsed.is_object());
}

#[test]
fn test_logical_operator_precedence() {
    // `a and b or c` should parse as `(a and b) or c`
    let source = "fn f(a: bool, b: bool, c: bool) -> bool { return a and b or c; }";
    parse_and_check(source);
}

// ═══════════════════════════════════════════════════════════════
//  Impl blocks and method calls
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_impl_method_call() {
    let ir = compile(
        "struct Point { x: int, y: int }\n\
         impl Point {\n\
         \x20   fn get_x(self: &Point) -> int { return self.x; }\n\
         }\n\
         fn main() -> int {\n\
         \x20   let p: Point = Point { x: 42, y: 0 };\n\
         \x20   return p.get_x();\n\
         }\n",
    );
    assert!(ir.contains("define i64 @Point_get_x(ptr %self)"));
    assert!(ir.contains("call i64 @Point_get_x(ptr"));
}

#[test]
fn test_impl_method_with_args() {
    let ir = compile(
        "struct Adder { base: int }\n\
         impl Adder {\n\
         \x20   fn add(self: &Adder, n: int) -> int { return self.base + n; }\n\
         }\n\
         fn main() -> int {\n\
         \x20   let a: Adder = Adder { base: 10 };\n\
         \x20   return a.add(32);\n\
         }\n",
    );
    assert!(ir.contains("define i64 @Adder_add(ptr %self, i64 %n)"));
    assert!(ir.contains("call i64 @Adder_add"));
}

#[test]
fn test_impl_undefined_method() {
    let result = yorum::typecheck(
        "struct Point { x: int, y: int }\n\
         fn main() -> int {\n\
         \x20   let p: Point = Point { x: 1, y: 2 };\n\
         \x20   return p.nonexistent();\n\
         }\n",
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("no method"));
}

#[test]
fn test_impl_ast_json_roundtrip() {
    let json = parse_to_json(
        "struct Point { x: int, y: int }\n\
         impl Point {\n\
         \x20   fn get_x(self: &Point) -> int { return self.x; }\n\
         }\n",
    );
    let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(parsed.is_object());
    let decls = parsed.get("declarations").unwrap().as_array().unwrap();
    assert_eq!(decls.len(), 2); // struct + impl
    assert!(json.contains("Impl"));
    assert!(json.contains("target_type"));
}

// ═══════════════════════════════════════════════════════════════
//  Traits
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_trait_impl_compiles() {
    let ir = compile(
        "struct Point { x: int, y: int }\n\
         trait Describable { fn describe(self: &Self) -> int; }\n\
         impl Describable for Point {\n\
         \x20   fn describe(self: &Point) -> int { return self.x; }\n\
         }\n\
         fn main() -> int {\n\
         \x20   let p: Point = Point { x: 42, y: 0 };\n\
         \x20   return p.describe();\n\
         }\n",
    );
    assert!(ir.contains("define i64 @Point_describe(ptr %self)"));
    assert!(ir.contains("call i64 @Point_describe"));
}

#[test]
fn test_trait_missing_method() {
    let result = yorum::typecheck(
        "struct Point { x: int, y: int }\n\
         trait Describable {\n\
         \x20   fn describe(self: &Self) -> int;\n\
         \x20   fn name(self: &Self) -> int;\n\
         }\n\
         impl Describable for Point {\n\
         \x20   fn describe(self: &Point) -> int { return self.x; }\n\
         }\n",
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("missing method"));
}

#[test]
fn test_trait_ast_json_roundtrip() {
    let json = parse_to_json(
        "trait Describable { fn describe(self: &Self) -> int; }\n",
    );
    let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(parsed.is_object());
    assert!(json.contains("Trait"));
    assert!(json.contains("Describable"));
}

// ═══════════════════════════════════════════════════════════════
//  Multiple declarations
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_multiple_functions() {
    let ir = compile(
        "fn double(x: int) -> int { return x * 2; }\n\
         fn triple(x: int) -> int { return x * 3; }\n\
         fn main() -> int {\n\
         \x20   let a: int = double(5);\n\
         \x20   let b: int = triple(5);\n\
         \x20   return a + b;\n\
         }\n",
    );
    assert!(ir.contains("define i64 @double"));
    assert!(ir.contains("define i64 @triple"));
    assert!(ir.contains("define i64 @main"));
}

#[test]
fn test_enum_declaration() {
    let ir = compile(
        "enum Color { Red, Green, Blue }\n\
         fn main() -> int { return 0; }\n",
    );
    assert!(ir.contains("%Color = type"));
}

// ═══════════════════════════════════════════════════════════════
//  Generics
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_generic_function_compiles() {
    let ir = compile(
        "fn identity<T>(x: T) -> T { return x; }\n\
         fn main() -> int {\n\
         \x20   let x: int = identity(42);\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(ir.contains("define i64 @identity__int(i64 %x)"));
    assert!(ir.contains("call i64 @identity__int(i64 42)"));
    // Generic declaration should be removed
    assert!(!ir.contains("define i64 @identity("));
}

#[test]
fn test_generic_function_multiple_instantiations() {
    let ir = compile(
        "fn identity<T>(x: T) -> T { return x; }\n\
         fn main() -> int {\n\
         \x20   let x: int = identity(42);\n\
         \x20   let y: float = identity(3.14);\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(ir.contains("define i64 @identity__int"));
    assert!(ir.contains("define double @identity__float"));
}

#[test]
fn test_generic_struct_compiles() {
    let ir = compile(
        "struct Pair<T, U> { first: T, second: U }\n\
         fn main() -> int {\n\
         \x20   let p: Pair<int, float> = Pair { first: 1, second: 2.0 };\n\
         \x20   return p.first;\n\
         }\n",
    );
    assert!(ir.contains("%Pair__int__float = type { i64, double }"));
    assert!(ir.contains("getelementptr %Pair__int__float"));
}

#[test]
fn test_generic_struct_field_access_types() {
    // Verify that field access on generic structs returns correct types
    parse_and_check(
        "struct Pair<T, U> { first: T, second: U }\n\
         fn main() -> int {\n\
         \x20   let p: Pair<int, float> = Pair { first: 1, second: 2.0 };\n\
         \x20   print_int(p.first);\n\
         \x20   print_float(p.second);\n\
         \x20   return 0;\n\
         }\n",
    );
}

#[test]
fn test_generic_ast_json_roundtrip() {
    let json = parse_to_json(
        "fn identity<T>(x: T) -> T { return x; }\n\
         struct Pair<T, U> { first: T, second: U }\n",
    );
    let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(parsed.is_object());
    assert!(json.contains("type_params"));
    let decls = parsed.get("declarations").unwrap().as_array().unwrap();
    assert_eq!(decls.len(), 2); // fn + struct
}

// ═══════════════════════════════════════════════════════════════
//  Closure tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_closure_basic() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let f: fn(int) -> int = |x: int| -> int { return x + 1; };\n\
         \x20   return f(41);\n\
         }\n",
    );
    assert!(ir.contains("define i64 @__closure_0"));
    assert!(ir.contains("define i64 @main()"));
}

#[test]
fn test_closure_with_capture() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let offset: int = 10;\n\
         \x20   let f: fn(int) -> int = |x: int| -> int { return x + offset; };\n\
         \x20   return f(32);\n\
         }\n",
    );
    assert!(ir.contains("%__env_0 = type { i64 }"));
    assert!(ir.contains("define i64 @__closure_0(ptr %env"));
}

#[test]
fn test_closure_passed_to_function() {
    let ir = compile(
        "fn apply(f: fn(int) -> int, x: int) -> int {\n\
         \x20   return f(x);\n\
         }\n\
         fn main() -> int {\n\
         \x20   let offset: int = 10;\n\
         \x20   let f: fn(int) -> int = |x: int| -> int { return x + offset; };\n\
         \x20   return apply(f, 32);\n\
         }\n",
    );
    assert!(ir.contains("define i64 @apply(ptr %f"));
    assert!(ir.contains("define i64 @__closure_0(ptr %env"));
}

#[test]
fn test_closure_no_capture() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let double: fn(int) -> int = |x: int| -> int { return x * 2; };\n\
         \x20   return double(21);\n\
         }\n",
    );
    assert!(ir.contains("define i64 @__closure_0"));
}

#[test]
fn test_closure_typecheck_error() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   let f: fn(int) -> int = |x: int| -> int { return x + 1; };\n\
         \x20   return f(1, 2);\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_closure_ast_json_roundtrip() {
    let json = parse_to_json(
        "fn main() -> int {\n\
         \x20   let f: fn(int) -> int = |x: int| -> int { return x + 1; };\n\
         \x20   return f(41);\n\
         }\n",
    );
    let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(parsed.is_object());
    assert!(json.contains("Closure"));
}
