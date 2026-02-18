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
    assert!(ir.contains("define i64 @main(i32 %__argc, ptr %__argv)"));
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
    let json = parse_to_json("trait Describable { fn describe(self: &Self) -> int; }\n");
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
         fn main() -> int {\n\
         \x20   let c: Color = Red;\n\
         \x20   return 0;\n\
         }\n",
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
    assert!(ir.contains("define i64 @main(i32 %__argc, ptr %__argv)"));
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

// ═══════════════════════════════════════════════════════════════
//  Array tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_array_literal_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let nums: [int] = [1, 2, 3];\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @malloc"));
    assert!(ir.contains("store i64 1"));
    assert!(ir.contains("store i64 3")); // length
}

#[test]
fn test_array_index_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let nums: [int] = [10, 20, 30];\n\
         \x20   let x: int = nums[0];\n\
         \x20   print_int(x);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call void @__yorum_bounds_check"));
    assert!(ir.contains("getelementptr i64"));
}

#[test]
fn test_array_len_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let nums: [int] = [1, 2, 3];\n\
         \x20   let n: int = len(nums);\n\
         \x20   print_int(n);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("load i64")); // loading length
}

#[test]
fn test_array_index_assignment() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut arr: [int] = [10, 20];\n\
         \x20   arr[1] = 99;\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call void @__yorum_bounds_check"));
    assert!(ir.contains("store i64"));
}

#[test]
fn test_array_typecheck_element_mismatch() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   let nums: [int] = [1, true, 3];\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  For loop tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_for_loop_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let nums: [int] = [1, 2, 3];\n\
         \x20   let mut sum: int = 0;\n\
         \x20   for x in nums {\n\
         \x20       sum = sum + x;\n\
         \x20   }\n\
         \x20   print_int(sum);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("for.cond"));
    assert!(ir.contains("for.body"));
    assert!(ir.contains("for.end"));
}

#[test]
fn test_for_loop_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20   let nums: [int] = [1, 2, 3];\n\
         \x20   let mut sum: int = 0;\n\
         \x20   for x in nums {\n\
         \x20       sum = sum + x;\n\
         \x20   }\n\
         \x20   return sum;\n\
         }\n",
    );
}

#[test]
fn test_for_loop_non_array_rejected() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   let x: int = 5;\n\
         \x20   for i in x {\n\
         \x20       print_int(i);\n\
         \x20   }\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_for_loop_ast_json() {
    let json = parse_to_json(
        "fn main() -> int {\n\
         \x20   let nums: [int] = [1, 2, 3];\n\
         \x20   for x in nums { print_int(x); }\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(json.contains("For"));
    assert!(json.contains("var_name"));
}

// ═══════════════════════════════════════════════════════════════
//  String operation tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_str_len_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let n: int = str_len(\"hello\");\n\
         \x20   print_int(n);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @str_len"));
}

#[test]
fn test_str_concat_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let s: string = str_concat(\"hello\", \" world\");\n\
         \x20   print_str(s);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @str_concat"));
}

#[test]
fn test_str_eq_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let eq: bool = str_eq(\"abc\", \"abc\");\n\
         \x20   print_bool(eq);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @str_eq"));
}

#[test]
fn test_str_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20   let n: int = str_len(\"hello\");\n\
         \x20   let s: string = str_concat(\"a\", \"b\");\n\
         \x20   let eq: bool = str_eq(\"x\", \"y\");\n\
         \x20   return 0;\n\
         }\n",
    );
}

// ═══════════════════════════════════════════════════════════════
//  Nested pattern matching tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_enum_variant_constructor_compiles() {
    let ir = compile(
        "enum MyOption { Some(int), None }\n\
         fn main() -> int {\n\
         \x20   let opt: MyOption = Some(42);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("getelementptr %MyOption"));
    assert!(ir.contains("store i32")); // tag
}

#[test]
fn test_enum_match_with_data() {
    let ir = compile(
        "enum MyOption { Some(int), None }\n\
         fn get_or(opt: MyOption, default_val: int) -> int {\n\
         \x20   match opt {\n\
         \x20       Some(val) => { return val; }\n\
         \x20       None => { return default_val; }\n\
         \x20   }\n\
         }\n\
         fn main() -> int {\n\
         \x20   let opt: MyOption = Some(42);\n\
         \x20   return get_or(opt, 0);\n\
         }\n",
    );
    assert!(ir.contains("define i64 @get_or"));
    assert!(ir.contains("icmp eq i32")); // tag comparison
}

#[test]
fn test_enum_match_typecheck() {
    parse_and_check(
        "enum MyOption { Some(int), None }\n\
         fn get_or(opt: MyOption, default_val: int) -> int {\n\
         \x20   match opt {\n\
         \x20       Some(val) => { return val; }\n\
         \x20       None => { return default_val; }\n\
         \x20   }\n\
         }\n",
    );
}

// ═══════════════════════════════════════════════════════════════
//  Contract verification tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_requires_emits_check() {
    let ir = compile(
        "fn positive(n: int) -> int\n\
         \x20   requires n > 0\n\
         {\n\
         \x20   return n;\n\
         }\n\
         fn main() -> int {\n\
         \x20   return positive(5);\n\
         }\n",
    );
    assert!(ir.contains("@__yorum_contract_fail"));
    assert!(ir.contains("req_ok"));
    assert!(ir.contains("req_fail"));
    assert!(ir.contains("requires clause in 'positive'"));
}

#[test]
fn test_ensures_emits_check() {
    let ir = compile(
        "fn abs(n: int) -> int\n\
         \x20   ensures result >= 0\n\
         {\n\
         \x20   if n < 0 { return 0 - n; }\n\
         \x20   return n;\n\
         }\n\
         fn main() -> int {\n\
         \x20   return abs(0 - 5);\n\
         }\n",
    );
    assert!(ir.contains("@__yorum_contract_fail"));
    assert!(ir.contains("ens_ok"));
    assert!(ir.contains("ens_fail"));
    assert!(ir.contains("ensures clause in 'abs'"));
    assert!(ir.contains("%result.addr"));
}

#[test]
fn test_pure_cannot_call_impure() {
    let result = yorum::typecheck(
        "pure fn add(a: int, b: int) -> int {\n\
         \x20   print_int(a);\n\
         \x20   return a + b;\n\
         }\n",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("pure function cannot call impure function"));
}

#[test]
fn test_pure_can_call_pure() {
    parse_and_check(
        "pure fn double(x: int) -> int { return x * 2; }\n\
         pure fn quad(x: int) -> int { return double(double(x)); }\n",
    );
}

#[test]
fn test_contract_expr_must_be_bool() {
    let result = yorum::typecheck(
        "fn bad(x: int) -> int\n\
         \x20   requires 42\n\
         {\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("requires clause must be 'bool'"));
}

#[test]
fn test_pure_can_call_pure_builtins() {
    parse_and_check(
        "pure fn check_len(s: string) -> int {\n\
         \x20   return str_len(s);\n\
         }\n",
    );
}

#[test]
fn test_ensures_expr_must_be_bool() {
    let result = yorum::typecheck(
        "fn bad(x: int) -> int\n\
         \x20   ensures 42\n\
         {\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("ensures clause must be 'bool'"));
}

#[test]
fn test_requires_and_ensures_combined() {
    let ir = compile(
        "fn clamp(x: int, lo: int, hi: int) -> int\n\
         \x20   requires lo <= hi\n\
         \x20   ensures result >= lo\n\
         {\n\
         \x20   if x < lo { return lo; }\n\
         \x20   if x > hi { return hi; }\n\
         \x20   return x;\n\
         }\n\
         fn main() -> int {\n\
         \x20   return clamp(5, 0, 10);\n\
         }\n",
    );
    assert!(ir.contains("req_ok"));
    assert!(ir.contains("ens_ok"));
}

// ═══════════════════════════════════════════════════════════════
//  Multi-file compilation tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_multi_file_compilation() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    // yorum.toml
    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    // src/math.yrm — a library module
    std::fs::write(
        src.join("math.yrm"),
        "module math;\n\
         pub fn add(a: int, b: int) -> int { return a + b; }\n\
         fn internal_helper() -> int { return 0; }\n",
    )
    .unwrap();

    // src/main.yrm — uses the math module
    std::fs::write(
        src.join("main.yrm"),
        "module main;\n\
         use math;\n\
         fn main() -> int {\n\
         \x20   let result: int = add(10, 32);\n\
         \x20   print_int(result);\n\
         \x20   return 0;\n\
         }\n",
    )
    .unwrap();

    let ir = yorum::compile_project(dir.path()).expect("multi-file compilation failed");
    assert!(ir.contains("define i64 @main(i32 %__argc, ptr %__argv)"));
    assert!(ir.contains("define i64 @math__add(i64 %a, i64 %b)"));
    // Internal helper should NOT be in the output
    assert!(!ir.contains("internal_helper"));
}

#[test]
fn test_visibility_enforcement() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    std::fs::write(
        src.join("math.yrm"),
        "module math;\n\
         fn private_add(a: int, b: int) -> int { return a + b; }\n",
    )
    .unwrap();

    std::fs::write(
        src.join("main.yrm"),
        "module main;\n\
         use math;\n\
         fn main() -> int {\n\
         \x20   return private_add(1, 2);\n\
         }\n",
    )
    .unwrap();

    let result = yorum::compile_project(dir.path());
    assert!(result.is_err());
}

#[test]
fn test_module_name_mismatch() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    // File is math.yrm but declares module wrong_name
    std::fs::write(
        src.join("math.yrm"),
        "module wrong_name;\n\
         pub fn add(a: int, b: int) -> int { return a + b; }\n",
    )
    .unwrap();

    std::fs::write(
        src.join("main.yrm"),
        "module main;\n\
         fn main() -> int { return 0; }\n",
    )
    .unwrap();

    let result = yorum::compile_project(dir.path());
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("module name mismatch"));
}

#[test]
fn test_yorum_init_creates_scaffold() {
    let dir = tempfile::tempdir().unwrap();
    let project_dir = dir.path().join("testproject");

    // Simulate init by creating files directly
    std::fs::create_dir_all(project_dir.join("src")).unwrap();
    std::fs::write(
        project_dir.join("yorum.toml"),
        "[package]\nname = \"testproject\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        project_dir.join("src/main.yrm"),
        "module main;\n\nfn main() -> int {\n    print_int(42);\n    return 0;\n}\n",
    )
    .unwrap();

    // Verify files exist and project compiles
    assert!(project_dir.join("yorum.toml").exists());
    assert!(project_dir.join("src/main.yrm").exists());
    let ir = yorum::compile_project(&project_dir).expect("scaffold project should compile");
    assert!(ir.contains("define i64 @main(i32 %__argc, ptr %__argv)"));
}

// ═══════════════════════════════════════════════════════════════
//  Phase 3: Structured Concurrency
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_spawn_join_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let t: Task<int> = spawn { return 42; };\n\
         \x20   let result: int = t.join();\n\
         \x20   return result;\n\
         }\n",
    );
    assert!(ir.contains("@pthread_create"));
    assert!(ir.contains("@pthread_join"));
    assert!(ir.contains("define ptr @__spawn_0"));
}

#[test]
fn test_task_type_inference() {
    // spawn { 42 } should produce Task<int> — typecheck should succeed
    parse_and_check(
        "fn main() -> int {\n\
         \x20   let t: Task<int> = spawn { return 42; };\n\
         \x20   let result: int = t.join();\n\
         \x20   return result;\n\
         }\n",
    );
}

#[test]
fn test_task_must_be_joined() {
    // Task not joined before scope exit — should fail ownership check
    // compile_to_ir runs the full pipeline including ownership checker
    let full_result = yorum::compile_to_ir(
        "fn main() -> int {\n\
         \x20   let t: Task<int> = spawn { return 42; };\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(full_result.is_err());
    let err = full_result.unwrap_err();
    assert!(err.contains("must be joined"));
}

#[test]
fn test_channel_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let ch: Chan<int> = chan();\n\
         \x20   let t: Task<int> = spawn {\n\
         \x20       return 0;\n\
         \x20   };\n\
         \x20   send(ch, 42);\n\
         \x20   let val: int = recv(ch);\n\
         \x20   let r: int = t.join();\n\
         \x20   return val;\n\
         }\n",
    );
    assert!(ir.contains("@__yorum_chan_create"));
    assert!(ir.contains("@__yorum_chan_send"));
    assert!(ir.contains("@__yorum_chan_recv"));
}

#[test]
fn test_spawn_captures_variables() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let x: int = 10;\n\
         \x20   let t: Task<int> = spawn {\n\
         \x20       return x;\n\
         \x20   };\n\
         \x20   let r: int = t.join();\n\
         \x20   return r;\n\
         }\n",
    );
    // Should have a spawn env type with captured variable
    assert!(ir.contains("__spawn_env_0"));
    assert!(ir.contains("@__spawn_0"));
}

#[test]
fn test_pure_cannot_spawn() {
    let result = yorum::typecheck(
        "pure fn compute() -> int {\n\
         \x20   let t: Task<int> = spawn { return 42; };\n\
         \x20   let r: int = t.join();\n\
         \x20   return r;\n\
         }\n",
    );
    assert!(result.is_err());
    let msg = result.unwrap_err();
    assert!(msg.contains("pure function cannot use spawn"));
}

// ═══════════════════════════════════════════════════════════════
//  Char type tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_char_literal_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let c: char = 'a';\n\
         \x20   print_char(c);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("@print_char"));
    assert!(ir.contains("i8"));
}

#[test]
fn test_char_comparison_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let c: char = 'x';\n\
         \x20   if c == 'x' { return 1; }\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("icmp eq i8"));
}

#[test]
fn test_char_ordering_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let c: char = 'b';\n\
         \x20   if c >= 'a' { return 1; }\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("icmp sge i8"));
}

#[test]
fn test_char_to_int_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let c: char = 'A';\n\
         \x20   let n: int = char_to_int(c);\n\
         \x20   return n;\n\
         }\n",
    );
    assert!(ir.contains("@char_to_int"));
    assert!(ir.contains("zext i8"));
}

#[test]
fn test_int_to_char_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let c: char = int_to_char(65);\n\
         \x20   print_char(c);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("@int_to_char"));
    assert!(ir.contains("trunc i64"));
}

#[test]
fn test_int_to_float_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let f: float = int_to_float(42);\n\
         \x20   print_float(f);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("@int_to_float"));
    assert!(ir.contains("sitofp i64"));
}

#[test]
fn test_float_to_int_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let n: int = float_to_int(3.14);\n\
         \x20   return n;\n\
         }\n",
    );
    assert!(ir.contains("@float_to_int"));
    assert!(ir.contains("fptosi double"));
}

#[test]
fn test_int_to_str_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let s: string = int_to_str(42);\n\
         \x20   print_str(s);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("@int_to_str"));
    assert!(ir.contains("@snprintf"));
}

#[test]
fn test_str_to_int_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let n: int = str_to_int(\"42\");\n\
         \x20   return n;\n\
         }\n",
    );
    assert!(ir.contains("@str_to_int"));
    assert!(ir.contains("@atol"));
}

#[test]
fn test_char_typecheck_errors() {
    // Cannot add chars
    let result = yorum::typecheck(
        "fn f() -> char {\n\
         \x20   let a: char = 'a';\n\
         \x20   let b: char = 'b';\n\
         \x20   return a + b;\n\
         }\n",
    );
    assert!(result.is_err());

    // Cannot assign int to char
    let result = yorum::typecheck(
        "fn f() -> int {\n\
         \x20   let c: char = 42;\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_char_escape_in_literal() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20   let nl: char = '\\n';\n\
         \x20   let tab: char = '\\t';\n\
         \x20   let nul: char = '\\0';\n\
         \x20   return 0;\n\
         }\n",
    );
}

#[test]
fn test_casting_type_errors() {
    // char_to_int expects char, not int
    let result = yorum::typecheck("fn f() -> int { return char_to_int(42); }");
    assert!(result.is_err());

    // int_to_char expects int, not string
    let result = yorum::typecheck("fn f() -> char { return int_to_char(\"hello\"); }");
    assert!(result.is_err());

    // float_to_int expects float, not int
    let result = yorum::typecheck("fn f() -> int { return float_to_int(42); }");
    assert!(result.is_err());
}

// ── Phase 1b: String/char operations ─────────────────────

#[test]
fn test_str_char_at_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let s: string = \"hello\";\n\
         \x20   let c: char = str_charAt(s, 0);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i8 @str_charAt"));
    assert!(ir.contains("@strlen"));
}

#[test]
fn test_str_sub_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let s: string = \"hello world\";\n\
         \x20   let sub: string = str_sub(s, 0, 5);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @str_sub"));
    assert!(ir.contains("@memcpy"));
}

#[test]
fn test_str_from_char_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let c: char = 'x';\n\
         \x20   let s: string = str_from_char(c);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @str_from_char"));
}

#[test]
fn test_char_is_alpha_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let c: char = 'a';\n\
         \x20   let result: bool = char_is_alpha(c);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @char_is_alpha"));
}

#[test]
fn test_char_is_digit_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let c: char = '5';\n\
         \x20   let result: bool = char_is_digit(c);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @char_is_digit"));
}

#[test]
fn test_char_is_whitespace_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let c: char = ' ';\n\
         \x20   let result: bool = char_is_whitespace(c);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @char_is_whitespace"));
}

#[test]
fn test_str_char_ops_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20   let s: string = \"hello\";\n\
         \x20   let c: char = str_charAt(s, 0);\n\
         \x20   let sub: string = str_sub(s, 1, 3);\n\
         \x20   let s2: string = str_from_char(c);\n\
         \x20   let a: bool = char_is_alpha(c);\n\
         \x20   let d: bool = char_is_digit(c);\n\
         \x20   let w: bool = char_is_whitespace(c);\n\
         \x20   return 0;\n\
         }\n",
    );
}

#[test]
fn test_str_char_ops_type_errors() {
    // str_charAt expects (string, int), not (int, int)
    let result = yorum::typecheck("fn f() -> char { return str_charAt(42, 0); }");
    assert!(result.is_err());

    // str_sub expects (string, int, int), not (string, string, int)
    let result = yorum::typecheck("fn f() -> string { return str_sub(\"hi\", \"a\", 1); }");
    assert!(result.is_err());

    // str_from_char expects char, not string
    let result = yorum::typecheck("fn f() -> string { return str_from_char(\"x\"); }");
    assert!(result.is_err());

    // char_is_alpha expects char, not int
    let result = yorum::typecheck("fn f() -> bool { return char_is_alpha(42); }");
    assert!(result.is_err());

    // char_is_digit expects char, not string
    let result = yorum::typecheck("fn f() -> bool { return char_is_digit(\"5\"); }");
    assert!(result.is_err());

    // char_is_whitespace expects char, not bool
    let result = yorum::typecheck("fn f() -> bool { return char_is_whitespace(true); }");
    assert!(result.is_err());
}

// ── Phase 1c: Dynamic arrays (push/pop) ─────────────────

#[test]
fn test_push_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut arr: [int] = [1, 2, 3];\n\
         \x20   push(arr, 4);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("@realloc"));
    assert!(ir.contains("push.grow"));
    assert!(ir.contains("push.store"));
}

#[test]
fn test_pop_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut arr: [int] = [1, 2, 3];\n\
         \x20   let val: int = pop(arr);\n\
         \x20   return val;\n\
         }\n",
    );
    assert!(ir.contains("pop.fail"));
    assert!(ir.contains("pop.ok"));
    assert!(ir.contains("@.fmt.pop_empty"));
}

#[test]
fn test_push_pop_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20   let mut arr: [int] = [10, 20];\n\
         \x20   push(arr, 30);\n\
         \x20   let v: int = pop(arr);\n\
         \x20   return v;\n\
         }\n",
    );
}

#[test]
fn test_push_type_mismatch() {
    // push: array expects int, got string
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   let mut arr: [int] = [1, 2];\n\
         \x20   push(arr, \"hello\");\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_push_non_array() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   let x: int = 5;\n\
         \x20   push(x, 10);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_pop_non_array() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   let x: int = 5;\n\
         \x20   let v: int = pop(x);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_push_wrong_arg_count() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   let mut arr: [int] = [1];\n\
         \x20   push(arr);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_array_capacity_field_in_ir() {
    // Verify the new { ptr, i64, i64 } layout is used
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let arr: [int] = [1, 2, 3];\n\
         \x20   return len(arr);\n\
         }\n",
    );
    assert!(ir.contains("{ ptr, i64, i64 }"));
}

#[test]
fn test_push_growth() {
    // Multiple pushes should trigger growth (capacity starts at element count)
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut arr: [int] = [1];\n\
         \x20   push(arr, 2);\n\
         \x20   push(arr, 3);\n\
         \x20   push(arr, 4);\n\
         \x20   push(arr, 5);\n\
         \x20   return len(arr);\n\
         }\n",
    );
    assert!(ir.contains("@realloc"));
    assert!(ir.contains("select"));
}

// ── Phase 1d: File I/O ──────────────────────────────────

#[test]
fn test_file_read_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let content: string = file_read(\"test.txt\");\n\
         \x20   print_str(content);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @file_read"));
    assert!(ir.contains("@fopen"));
    assert!(ir.contains("@fread"));
}

#[test]
fn test_file_write_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let ok: bool = file_write(\"out.txt\", \"hello\");\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @file_write"));
    assert!(ir.contains("@fwrite"));
}

#[test]
fn test_print_err_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   print_err(\"error message\");\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("call void @print_err"));
    assert!(ir.contains("@write"));
}

#[test]
fn test_file_io_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20   let content: string = file_read(\"input.txt\");\n\
         \x20   let ok: bool = file_write(\"output.txt\", content);\n\
         \x20   print_err(\"done\");\n\
         \x20   return 0;\n\
         }\n",
    );
}

#[test]
fn test_file_io_type_errors() {
    // file_read expects string, not int
    let result = yorum::typecheck("fn f() -> string { return file_read(42); }");
    assert!(result.is_err());

    // file_write expects (string, string), not (string, int)
    let result = yorum::typecheck("fn f() -> bool { return file_write(\"a.txt\", 42); }");
    assert!(result.is_err());

    // print_err expects string, not bool
    let result = yorum::typecheck("fn f() { print_err(true); }");
    assert!(result.is_err());
}

#[test]
fn test_file_io_pure_rejected() {
    // file_read is impure
    let result = yorum::typecheck("pure fn f() -> string { return file_read(\"x\"); }");
    assert!(result.is_err());

    // file_write is impure
    let result = yorum::typecheck("pure fn f() -> bool { return file_write(\"x\", \"y\"); }");
    assert!(result.is_err());
}

// ── Phase 1e: Process interaction ───────────────────────

#[test]
fn test_exit_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   exit(1);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("trunc i64"));
    assert!(ir.contains("call void @exit(i32"));
}

#[test]
fn test_args_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let a: [string] = args();\n\
         \x20   let n: int = len(a);\n\
         \x20   return n;\n\
         }\n",
    );
    assert!(ir.contains("@__yorum_argc"));
    assert!(ir.contains("@__yorum_argv"));
    assert!(ir.contains("@memcpy"));
}

#[test]
fn test_main_has_argc_argv() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("define i64 @main(i32 %__argc, ptr %__argv)"));
}

#[test]
fn test_args_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20   let a: [string] = args();\n\
         \x20   let first: string = a[0];\n\
         \x20   print_str(first);\n\
         \x20   return 0;\n\
         }\n",
    );
}

#[test]
fn test_exit_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20   exit(0);\n\
         \x20   return 0;\n\
         }\n",
    );
}

#[test]
fn test_process_type_errors() {
    // exit expects int, not string
    let result = yorum::typecheck("fn f() { exit(\"bad\"); }");
    assert!(result.is_err());

    // args takes no arguments
    let result = yorum::typecheck("fn f() -> int { let a: [string] = args(42); return 0; }");
    assert!(result.is_err());
}

#[test]
fn test_process_pure_rejected() {
    let result = yorum::typecheck("pure fn f() { exit(0); }");
    assert!(result.is_err());

    let result = yorum::typecheck("pure fn f() -> int { let a: [string] = args(); return 0; }");
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  P1f: HashMap tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_map_new_compiles() {
    let ir = compile(
        "fn main() -> int {\
           let m: Map = map_new();\
           return 0;\
         }",
    );
    assert!(ir.contains("@map_new"));
}

#[test]
fn test_map_set_compiles() {
    let ir = compile(
        "fn main() -> int {\
           let m: Map = map_new();\
           map_set(m, \"hello\", 42);\
           return 0;\
         }",
    );
    assert!(ir.contains("@map_set"));
}

#[test]
fn test_map_get_compiles() {
    let ir = compile(
        "fn main() -> int {\
           let m: Map = map_new();\
           map_set(m, \"x\", 10);\
           let v: int = map_get(m, \"x\");\
           print_int(v);\
           return 0;\
         }",
    );
    assert!(ir.contains("@map_get"));
}

#[test]
fn test_map_has_compiles() {
    let ir = compile(
        "fn main() -> int {\
           let m: Map = map_new();\
           map_set(m, \"key\", 1);\
           let found: bool = map_has(m, \"key\");\
           print_bool(found);\
           return 0;\
         }",
    );
    assert!(ir.contains("@map_has"));
}

#[test]
fn test_map_typecheck() {
    parse_and_check(
        "fn main() -> int {\
           let m: Map = map_new();\
           map_set(m, \"a\", 1);\
           let v: int = map_get(m, \"a\");\
           let b: bool = map_has(m, \"a\");\
           return 0;\
         }",
    );
}

#[test]
fn test_map_type_errors() {
    // map_set with wrong key type
    let result = yorum::typecheck(
        "fn main() -> int {\
           let m: Map = map_new();\
           map_set(m, 42, 1);\
           return 0;\
         }",
    );
    assert!(result.is_err());

    // map_get with wrong key type
    let result = yorum::typecheck(
        "fn main() -> int {\
           let m: Map = map_new();\
           let v: int = map_get(m, 42);\
           return 0;\
         }",
    );
    assert!(result.is_err());

    // map_get returns int, not string
    let result = yorum::typecheck(
        "fn main() -> int {\
           let m: Map = map_new();\
           let v: string = map_get(m, \"x\");\
           return 0;\
         }",
    );
    assert!(result.is_err());

    // map_new takes no arguments
    let result = yorum::typecheck(
        "fn main() -> int {\
           let m: Map = map_new(10);\
           return 0;\
         }",
    );
    assert!(result.is_err());
}

#[test]
fn test_map_pure_rejected() {
    // map_new is impure (allocates)
    let result = yorum::typecheck("pure fn f() -> Map { return map_new(); }");
    assert!(result.is_err());

    // map_set is impure
    let result = yorum::typecheck("pure fn f(m: Map) { map_set(m, \"k\", 1); }");
    assert!(result.is_err());
}

#[test]
fn test_map_ir_contains_helpers() {
    let ir = compile(
        "fn main() -> int {\
           let m: Map = map_new();\
           return 0;\
         }",
    );
    assert!(ir.contains("@__yorum_hash_string"));
    assert!(ir.contains("@__yorum_map_find_slot"));
    assert!(ir.contains("@__yorum_map_grow"));
    assert!(ir.contains("define ptr @map_new"));
    assert!(ir.contains("define void @map_set"));
    assert!(ir.contains("define i64 @map_get"));
    assert!(ir.contains("define i1 @map_has"));
}

// ═══════════════════════════════════════════════════════════════
//  v0.6 Standard Library — Math builtins
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_math_abs_int() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let x: int = abs_int(-5);\n\
         \x20 print_int(x);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @abs_int"));
}

#[test]
fn test_math_abs_float() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let x: float = abs_float(-3.14);\n\
         \x20 print_float(x);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call double @abs_float"));
}

#[test]
fn test_math_min_max_int() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let a: int = min_int(3, 7);\n\
         \x20 let b: int = max_int(3, 7);\n\
         \x20 print_int(a);\n\
         \x20 print_int(b);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @min_int"));
    assert!(ir.contains("call i64 @max_int"));
}

#[test]
fn test_math_min_max_float() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let a: float = min_float(1.5, 2.5);\n\
         \x20 let b: float = max_float(1.5, 2.5);\n\
         \x20 print_float(a);\n\
         \x20 print_float(b);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call double @min_float"));
    assert!(ir.contains("call double @max_float"));
}

#[test]
fn test_math_sqrt_pow() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let s: float = sqrt(16.0);\n\
         \x20 let p: float = pow(2.0, 10.0);\n\
         \x20 print_float(s);\n\
         \x20 print_float(p);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call double @llvm.sqrt.f64"));
    assert!(ir.contains("call double @llvm.pow.f64"));
}

#[test]
fn test_math_trig() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let a: float = sin(0.0);\n\
         \x20 let b: float = cos(0.0);\n\
         \x20 let c: float = tan(0.0);\n\
         \x20 print_float(a);\n\
         \x20 print_float(b);\n\
         \x20 print_float(c);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call double @sin"));
    assert!(ir.contains("call double @cos"));
    assert!(ir.contains("call double @tan"));
}

#[test]
fn test_math_floor_ceil_round() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let a: float = floor(3.7);\n\
         \x20 let b: float = ceil(3.2);\n\
         \x20 let c: float = round(3.5);\n\
         \x20 print_float(a);\n\
         \x20 print_float(b);\n\
         \x20 print_float(c);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call double @llvm.floor.f64"));
    assert!(ir.contains("call double @llvm.ceil.f64"));
    assert!(ir.contains("call double @llvm.round.f64"));
}

#[test]
fn test_math_log_exp() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let a: float = log(2.718);\n\
         \x20 let b: float = log10(100.0);\n\
         \x20 let c: float = exp(1.0);\n\
         \x20 print_float(a);\n\
         \x20 print_float(b);\n\
         \x20 print_float(c);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call double @log"));
    assert!(ir.contains("call double @log10"));
    assert!(ir.contains("call double @exp"));
}

#[test]
fn test_math_pure_in_pure_fn() {
    // Math builtins are pure, so they should work in pure functions
    parse_and_check(
        "pure fn f(x: int) -> int { return abs_int(x); }\n\
         pure fn g(x: float) -> float { return sqrt(x); }\n\
         pure fn h(a: int, b: int) -> int { return min_int(a, b); }\n",
    );
}

#[test]
fn test_math_ir_definitions() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let x: int = abs_int(1);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("define i64 @abs_int"));
    assert!(ir.contains("define double @abs_float"));
    assert!(ir.contains("define i64 @min_int"));
    assert!(ir.contains("define i64 @max_int"));
    assert!(ir.contains("define double @min_float"));
    assert!(ir.contains("define double @max_float"));
    // sqrt, pow, floor, ceil, round are inlined as LLVM intrinsic calls
    // (no wrapper functions) to avoid C library name collisions
    assert!(ir.contains("declare double @llvm.sqrt.f64"));
    assert!(ir.contains("declare double @llvm.pow.f64"));
    assert!(ir.contains("declare double @llvm.floor.f64"));
    assert!(ir.contains("declare double @llvm.ceil.f64"));
    assert!(ir.contains("declare double @llvm.round.f64"));
    // sin, cos, tan, log, log10, exp are external declarations, not definitions
    assert!(ir.contains("declare double @sin"));
    assert!(ir.contains("declare double @cos"));
    assert!(ir.contains("declare double @tan"));
    assert!(ir.contains("declare double @log(double)"));
    assert!(ir.contains("declare double @log10"));
    assert!(ir.contains("declare double @exp"));
}

// ═══════════════════════════════════════════════════════════════
//  v0.6 Standard Library — String utility builtins
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_str_contains_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let b: bool = str_contains(\"hello world\", \"world\");\n\
         \x20 print_bool(b);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @str_contains"));
}

#[test]
fn test_str_index_of_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let i: int = str_index_of(\"hello\", \"ll\");\n\
         \x20 print_int(i);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @str_index_of"));
}

#[test]
fn test_str_starts_ends_with() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let a: bool = str_starts_with(\"hello\", \"hel\");\n\
         \x20 let b: bool = str_ends_with(\"hello\", \"llo\");\n\
         \x20 print_bool(a);\n\
         \x20 print_bool(b);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @str_starts_with"));
    assert!(ir.contains("call i1 @str_ends_with"));
}

#[test]
fn test_str_trim_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let s: string = str_trim(\"  hello  \");\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @str_trim"));
}

#[test]
fn test_str_replace_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let s: string = str_replace(\"hello world\", \"world\", \"yorum\");\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @str_replace"));
}

#[test]
fn test_str_split_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let parts: [string] = str_split(\"a,b,c\", \",\");\n\
         \x20 print_int(len(parts));\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @str_split"));
}

#[test]
fn test_str_to_upper_lower() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let a: string = str_to_upper(\"hello\");\n\
         \x20 let b: string = str_to_lower(\"HELLO\");\n\
         \x20 print_str(a);\n\
         \x20 print_str(b);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @str_to_upper"));
    assert!(ir.contains("call ptr @str_to_lower"));
}

#[test]
fn test_str_repeat_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let s: string = str_repeat(\"ab\", 3);\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @str_repeat"));
}

#[test]
fn test_str_contains_pure() {
    // str_contains is pure
    parse_and_check("pure fn f(s: string) -> bool { return str_contains(s, \"x\"); }\n");
}

#[test]
fn test_str_split_impure() {
    // str_split is impure (allocates array)
    let result =
        yorum::typecheck("pure fn f(s: string) -> [string] { return str_split(s, \",\"); }");
    assert!(result.is_err());
}

#[test]
fn test_str_utility_ir_definitions() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let b: bool = str_contains(\"a\", \"b\");\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("define i1 @str_contains"));
    assert!(ir.contains("define i64 @str_index_of"));
    assert!(ir.contains("define i1 @str_starts_with"));
    assert!(ir.contains("define i1 @str_ends_with"));
    assert!(ir.contains("define ptr @str_trim"));
    assert!(ir.contains("define ptr @str_replace"));
    assert!(ir.contains("define ptr @str_split"));
    assert!(ir.contains("define ptr @str_to_upper"));
    assert!(ir.contains("define ptr @str_to_lower"));
    assert!(ir.contains("define ptr @str_repeat"));
}

// ═══════════════════════════════════════════════════════════════
//  v0.6 Standard Library — Collection utility builtins
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_slice_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let arr: [int] = [1, 2, 3, 4, 5];\n\
         \x20 let s: [int] = slice(arr, 1, 4);\n\
         \x20 print_int(len(s));\n\
         \x20 return 0;\n\
         }\n",
    );
    // slice is inlined, check for its characteristic pattern
    assert!(ir.contains("@malloc"));
    assert!(ir.contains("@memcpy"));
}

#[test]
fn test_concat_arrays_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let a: [int] = [1, 2];\n\
         \x20 let b: [int] = [3, 4];\n\
         \x20 let c: [int] = concat_arrays(a, b);\n\
         \x20 print_int(len(c));\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("@malloc"));
}

#[test]
fn test_reverse_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let arr: [int] = [1, 2, 3];\n\
         \x20 let rev: [int] = reverse(arr);\n\
         \x20 print_int(len(rev));\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("rev.loop"));
}

#[test]
fn test_contains_int_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let arr: [int] = [1, 2, 3];\n\
         \x20 let b: bool = contains_int(arr, 2);\n\
         \x20 print_bool(b);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @contains_int"));
}

#[test]
fn test_contains_str_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let arr: [string] = [\"hello\", \"world\"];\n\
         \x20 let b: bool = contains_str(arr, \"world\");\n\
         \x20 print_bool(b);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @contains_str"));
}

#[test]
fn test_sort_int_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let arr: [int] = [3, 1, 2];\n\
         \x20 let sorted: [int] = sort_int(arr);\n\
         \x20 print_int(len(sorted));\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @sort_int"));
}

#[test]
fn test_sort_str_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let arr: [string] = [\"c\", \"a\", \"b\"];\n\
         \x20 let sorted: [string] = sort_str(arr);\n\
         \x20 print_int(len(sorted));\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @sort_str"));
}

#[test]
fn test_map_size_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let m: Map = map_new();\n\
         \x20 map_set(m, \"a\", 1);\n\
         \x20 let s: int = map_size(m);\n\
         \x20 print_int(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @map_size"));
}

#[test]
fn test_map_remove_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let m: Map = map_new();\n\
         \x20 map_set(m, \"a\", 1);\n\
         \x20 let removed: bool = map_remove(m, \"a\");\n\
         \x20 print_bool(removed);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @map_remove"));
}

#[test]
fn test_map_keys_values_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let m: Map = map_new();\n\
         \x20 map_set(m, \"a\", 1);\n\
         \x20 let keys: [string] = map_keys(m);\n\
         \x20 let vals: [int] = map_values(m);\n\
         \x20 print_int(len(keys));\n\
         \x20 print_int(len(vals));\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @map_keys"));
    assert!(ir.contains("call ptr @map_values"));
}

#[test]
fn test_contains_int_pure() {
    // contains_int is pure
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let arr: [int] = [1, 2, 3];\n\
         \x20 let b: bool = contains_int(arr, 2);\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_slice_type_error() {
    // slice requires array argument
    let result = yorum::typecheck("fn f(x: int) { let y: int = slice(x, 0, 1); }");
    assert!(result.is_err());
}

#[test]
fn test_concat_arrays_type_mismatch() {
    let result = yorum::typecheck(
        "fn f() {\n\
         \x20 let a: [int] = [1];\n\
         \x20 let b: [string] = [\"x\"];\n\
         \x20 let c: [int] = concat_arrays(a, b);\n\
         }",
    );
    assert!(result.is_err());
}

#[test]
fn test_collection_ir_definitions() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let m: Map = map_new();\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("define i1 @contains_int"));
    assert!(ir.contains("define i1 @contains_str"));
    assert!(ir.contains("define ptr @sort_int"));
    assert!(ir.contains("define ptr @sort_str"));
    assert!(ir.contains("define i64 @map_size"));
    assert!(ir.contains("define i1 @map_remove"));
    assert!(ir.contains("define ptr @map_keys"));
    assert!(ir.contains("define ptr @map_values"));
}

// ═══════════════════════════════════════════════════════════════
//  v0.6 Standard Library — Enhanced I/O builtins
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_file_exists_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let b: bool = file_exists(\"test.txt\");\n\
         \x20 print_bool(b);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @file_exists"));
}

#[test]
fn test_file_append_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let ok: bool = file_append(\"log.txt\", \"hello\\n\");\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i1 @file_append"));
}

#[test]
fn test_read_line_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let s: string = read_line();\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @read_line"));
}

#[test]
fn test_print_flush_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 print_flush(\"prompt> \");\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call void @print_flush"));
}

#[test]
fn test_env_get_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let home: string = env_get(\"HOME\");\n\
         \x20 print_str(home);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @env_get"));
}

#[test]
fn test_time_ms_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let t: int = time_ms();\n\
         \x20 print_int(t);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @time_ms"));
}

#[test]
fn test_io_builtins_impure() {
    // All enhanced I/O builtins are impure
    let result = yorum::typecheck("pure fn f() -> bool { return file_exists(\"x\"); }");
    assert!(result.is_err());
    let result = yorum::typecheck("pure fn f() -> string { return read_line(); }");
    assert!(result.is_err());
    let result = yorum::typecheck("pure fn f() -> int { return time_ms(); }");
    assert!(result.is_err());
}

#[test]
fn test_io_ir_definitions() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let b: bool = file_exists(\"x\");\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("define i1 @file_exists"));
    assert!(ir.contains("define i1 @file_append"));
    assert!(ir.contains("define ptr @read_line"));
    assert!(ir.contains("define void @print_flush"));
    assert!(ir.contains("define ptr @env_get"));
    assert!(ir.contains("define i64 @time_ms"));
}

// ═══════════════════════════════════════════════════════════════
//  LSP diagnostics API tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_check_diagnostics_valid_program() {
    let diags = yorum::check_diagnostics(
        "module test;\n\
         fn main() -> int {\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(
        diags.is_empty(),
        "expected no diagnostics for valid program"
    );
}

#[test]
fn test_check_diagnostics_type_error() {
    let diags = yorum::check_diagnostics(
        "module test;\n\
         fn main() -> int {\n\
         \x20   let x: int = true;\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(!diags.is_empty(), "expected diagnostics for type error");
    assert!(diags[0].message.contains("cannot assign"));
    assert!(matches!(
        diags[0].severity,
        yorum::DiagnosticSeverity::Error
    ));
}

#[test]
fn test_check_diagnostics_parse_error() {
    let diags = yorum::check_diagnostics("fn {{{");
    assert!(!diags.is_empty(), "expected diagnostics for parse error");
}

#[test]
fn test_check_with_symbols_returns_symbol_table() {
    let (diags, symbols) = yorum::check_with_symbols(
        "module test;\n\
         fn add(a: int, b: int) -> int {\n\
         \x20   return a + b;\n\
         }\n\
         fn main() -> int {\n\
         \x20   let x: int = add(1, 2);\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(diags.is_empty());
    let sym = symbols.expect("should have symbol table");

    // Should have function definitions
    let fn_defs: Vec<_> = sym
        .definitions
        .iter()
        .filter(|d| matches!(d.kind, yorum::compiler::typechecker::SymbolKind::Function))
        .collect();
    assert!(fn_defs.len() >= 2, "should have at least 2 function defs");

    // Should have parameter definitions
    let param_defs: Vec<_> = sym
        .definitions
        .iter()
        .filter(|d| matches!(d.kind, yorum::compiler::typechecker::SymbolKind::Parameter))
        .collect();
    assert!(
        param_defs.len() >= 2,
        "should have at least 2 param defs (a, b)"
    );

    // Should have variable definitions
    let var_defs: Vec<_> = sym
        .definitions
        .iter()
        .filter(|d| matches!(d.kind, yorum::compiler::typechecker::SymbolKind::Variable))
        .collect();
    assert!(!var_defs.is_empty(), "should have at least 1 var def (x)");

    // Should have references
    assert!(!sym.references.is_empty(), "should have symbol references");

    // Check that add() call has a reference with def_span pointing to the function
    let add_refs: Vec<_> = sym
        .references
        .iter()
        .filter(|r| r.def_name == "add")
        .collect();
    assert!(!add_refs.is_empty(), "should have reference to 'add'");
    assert!(
        add_refs.iter().any(|r| r.def_span.is_some()),
        "add ref should have def_span"
    );
}

#[test]
fn test_check_with_symbols_parse_error_returns_none() {
    let (diags, symbols) = yorum::check_with_symbols("fn {{{");
    assert!(!diags.is_empty());
    assert!(
        symbols.is_none(),
        "symbol table should be None on parse error"
    );
}

#[test]
fn test_check_with_symbols_struct_def() {
    let (diags, symbols) = yorum::check_with_symbols(
        "module test;\n\
         struct Point {\n\
         \x20   x: int,\n\
         \x20   y: int,\n\
         }\n\
         fn main() -> int { return 0; }\n",
    );
    assert!(diags.is_empty());
    let sym = symbols.unwrap();
    let struct_defs: Vec<_> = sym
        .definitions
        .iter()
        .filter(|d| matches!(d.kind, yorum::compiler::typechecker::SymbolKind::Struct))
        .collect();
    assert!(!struct_defs.is_empty());
    assert!(struct_defs[0].type_desc.contains("Point"));
}

#[test]
fn test_check_with_symbols_enum_def() {
    let (diags, symbols) = yorum::check_with_symbols(
        "module test;\n\
         enum Color { Red, Green, Blue }\n\
         fn main() -> int { return 0; }\n",
    );
    assert!(diags.is_empty());
    let sym = symbols.unwrap();
    let enum_defs: Vec<_> = sym
        .definitions
        .iter()
        .filter(|d| matches!(d.kind, yorum::compiler::typechecker::SymbolKind::Enum))
        .collect();
    assert!(!enum_defs.is_empty());
    assert!(enum_defs[0].type_desc.contains("Color"));
}

#[test]
fn test_check_diagnostics_with_type_errors_still_returns_symbols() {
    let (diags, symbols) = yorum::check_with_symbols(
        "module test;\n\
         fn main() -> int {\n\
         \x20   let x: int = true;\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(!diags.is_empty(), "should have type error");
    // Symbol table should still be present even with errors
    let sym = symbols.expect("should have symbol table even with errors");
    assert!(!sym.definitions.is_empty());
}

// ═══════════════════════════════════════════════════════════════
//  Ownership checker tests (v0.8)
// ═══════════════════════════════════════════════════════════════

/// Helper: compile_to_ir should fail with a specific ownership error message
fn expect_ownership_error(source: &str, expected_msg: &str) {
    let result = yorum::compile_to_ir(source);
    assert!(
        result.is_err(),
        "expected ownership error but compilation succeeded"
    );
    let err = result.unwrap_err();
    assert!(
        err.contains(expected_msg),
        "expected error containing '{}', got: {}",
        expected_msg,
        err
    );
}

// ── Move tracking ────────────────────────────────────────────

#[test]
fn test_use_after_move_struct() {
    expect_ownership_error(
        "struct Foo { x: int }\n\
         fn f() -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   let b: Foo = a;\n\
         \x20   return a.x;\n\
         }\n",
        "use of moved variable 'a'",
    );
}

#[test]
fn test_double_move() {
    expect_ownership_error(
        "struct Foo { x: int }\n\
         fn f() -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   let b: Foo = a;\n\
         \x20   let c: Foo = a;\n\
         \x20   return 0;\n\
         }\n",
        "use of moved variable 'a'",
    );
}

#[test]
fn test_copy_types_freely_reusable() {
    // int, float, bool, char, string are copy — reuse is fine
    compile(
        "fn f(x: int) -> int {\n\
         \x20   let a: int = x;\n\
         \x20   let b: int = x;\n\
         \x20   return x;\n\
         }\n",
    );
}

#[test]
fn test_string_is_copy() {
    compile(
        "fn f(s: string) -> string {\n\
         \x20   let a: string = s;\n\
         \x20   let b: string = s;\n\
         \x20   return s;\n\
         }\n",
    );
}

#[test]
fn test_reassign_after_move() {
    compile(
        "struct Foo { x: int }\n\
         fn f() -> int {\n\
         \x20   let mut a: Foo = Foo { x: 1 };\n\
         \x20   let b: Foo = a;\n\
         \x20   a = Foo { x: 2 };\n\
         \x20   return a.x;\n\
         }\n",
    );
}

#[test]
fn test_move_in_return() {
    // Returning a moved variable should error
    expect_ownership_error(
        "struct Foo { x: int }\n\
         fn f() -> Foo {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   let b: Foo = a;\n\
         \x20   return a;\n\
         }\n",
        "use of moved variable 'a'",
    );
}

#[test]
fn test_move_in_assign() {
    // Assigning from a moved variable should error
    expect_ownership_error(
        "struct Foo { x: int }\n\
         fn f() -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   let b: Foo = a;\n\
         \x20   let mut c: Foo = Foo { x: 3 };\n\
         \x20   c = a;\n\
         \x20   return 0;\n\
         }\n",
        "use of moved variable 'a'",
    );
}

// ── Branch merging ───────────────────────────────────────────

#[test]
fn test_move_in_both_if_branches() {
    expect_ownership_error(
        "struct Foo { x: int }\n\
         fn f(cond: bool) -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   if cond {\n\
         \x20       let b: Foo = a;\n\
         \x20   } else {\n\
         \x20       let c: Foo = a;\n\
         \x20   }\n\
         \x20   return a.x;\n\
         }\n",
        "use of moved variable 'a'",
    );
}

#[test]
fn test_move_in_one_branch_only() {
    // Conservative: moved in one branch = moved after the if
    expect_ownership_error(
        "struct Foo { x: int }\n\
         fn f(cond: bool) -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   if cond {\n\
         \x20       let b: Foo = a;\n\
         \x20   } else {\n\
         \x20       let x: int = 0;\n\
         \x20   }\n\
         \x20   return a.x;\n\
         }\n",
        "use of moved variable 'a'",
    );
}

#[test]
fn test_no_move_in_either_branch() {
    compile(
        "struct Foo { x: int }\n\
         fn f(cond: bool) -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   if cond {\n\
         \x20       let x: int = 1;\n\
         \x20   } else {\n\
         \x20       let y: int = 2;\n\
         \x20   }\n\
         \x20   return a.x;\n\
         }\n",
    );
}

#[test]
fn test_move_in_if_no_else() {
    // Conservative: conditional move = moved
    expect_ownership_error(
        "struct Foo { x: int }\n\
         fn f(cond: bool) -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   if cond {\n\
         \x20       let b: Foo = a;\n\
         \x20   }\n\
         \x20   return a.x;\n\
         }\n",
        "use of moved variable 'a'",
    );
}

#[test]
fn test_match_arm_move_tracking() {
    expect_ownership_error(
        "struct Foo { x: int }\n\
         enum Choice { A, B }\n\
         fn f(c: Choice) -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   match c {\n\
         \x20       A => { let b: Foo = a; }\n\
         \x20       B => { let x: int = 0; }\n\
         \x20   }\n\
         \x20   return a.x;\n\
         }\n",
        "use of moved variable 'a'",
    );
}

// ── Loop safety ──────────────────────────────────────────────

#[test]
fn test_move_outer_var_in_while_loop() {
    expect_ownership_error(
        "struct Foo { x: int }\n\
         fn f() -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   let mut i: int = 0;\n\
         \x20   while i < 3 {\n\
         \x20       let b: Foo = a;\n\
         \x20       i = i + 1;\n\
         \x20   }\n\
         \x20   return 0;\n\
         }\n",
        "cannot move 'a' inside a loop",
    );
}

#[test]
fn test_move_outer_var_in_for_loop() {
    expect_ownership_error(
        "struct Foo { x: int }\n\
         fn f() -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   let arr: [int] = [1, 2, 3];\n\
         \x20   for x in arr {\n\
         \x20       let b: Foo = a;\n\
         \x20   }\n\
         \x20   return 0;\n\
         }\n",
        "cannot move 'a' inside a loop",
    );
}

#[test]
fn test_move_loop_local_var_ok() {
    // Variables defined inside the loop body are fresh each iteration
    compile(
        "struct Foo { x: int }\n\
         fn f() -> int {\n\
         \x20   let mut i: int = 0;\n\
         \x20   while i < 3 {\n\
         \x20       let a: Foo = Foo { x: i };\n\
         \x20       let b: Foo = a;\n\
         \x20       i = i + 1;\n\
         \x20   }\n\
         \x20   return 0;\n\
         }\n",
    );
}

#[test]
fn test_copy_type_in_loop_ok() {
    // Copy types are fine to use repeatedly in loops
    compile(
        "fn f() -> int {\n\
         \x20   let x: int = 42;\n\
         \x20   let mut sum: int = 0;\n\
         \x20   let mut i: int = 0;\n\
         \x20   while i < 3 {\n\
         \x20       sum = sum + x;\n\
         \x20       i = i + 1;\n\
         \x20   }\n\
         \x20   return sum;\n\
         }\n",
    );
}

// ── Non-moving contexts ──────────────────────────────────────

#[test]
fn test_method_calls_dont_move() {
    compile(
        "struct Point { x: int, y: int }\n\
         impl Point {\n\
         \x20   fn get_x(self: &Point) -> int { return self.x; }\n\
         \x20   fn get_y(self: &Point) -> int { return self.y; }\n\
         }\n\
         fn main() -> int {\n\
         \x20   let p: Point = Point { x: 3, y: 4 };\n\
         \x20   let a: int = p.get_x();\n\
         \x20   let b: int = p.get_y();\n\
         \x20   return a + b;\n\
         }\n",
    );
}

#[test]
fn test_field_access_doesnt_move() {
    compile(
        "struct Point { x: int, y: int }\n\
         fn main() -> int {\n\
         \x20   let p: Point = Point { x: 3, y: 4 };\n\
         \x20   let a: int = p.x;\n\
         \x20   let b: int = p.y;\n\
         \x20   return a + b;\n\
         }\n",
    );
}

#[test]
fn test_array_indexing_doesnt_move() {
    compile(
        "fn main() -> int {\n\
         \x20   let arr: [int] = [10, 20, 30];\n\
         \x20   let a: int = arr[0];\n\
         \x20   let b: int = arr[1];\n\
         \x20   let c: int = arr[2];\n\
         \x20   return a + b + c;\n\
         }\n",
    );
}

#[test]
fn test_function_args_dont_move() {
    compile(
        "struct Foo { x: int }\n\
         fn use_foo(f: Foo) -> int { return f.x; }\n\
         fn main() -> int {\n\
         \x20   let a: Foo = Foo { x: 42 };\n\
         \x20   let b: int = use_foo(a);\n\
         \x20   return a.x;\n\
         }\n",
    );
}

#[test]
fn test_enum_use_after_move() {
    expect_ownership_error(
        "enum MyOption { Some(int), None }\n\
         fn f() -> int {\n\
         \x20   let a: MyOption = Some(42);\n\
         \x20   let b: MyOption = a;\n\
         \x20   let c: MyOption = a;\n\
         \x20   return 0;\n\
         }\n",
        "use of moved variable 'a'",
    );
}

#[test]
fn test_closure_doesnt_move_outer() {
    compile(
        "fn main() -> int {\n\
         \x20   let x: int = 42;\n\
         \x20   let f: fn(int) -> int = |y: int| -> int { return y + x; };\n\
         \x20   return f(0) + x;\n\
         }\n",
    );
}

#[test]
fn test_spawn_doesnt_move_outer() {
    compile(
        "fn main() -> int {\n\
         \x20   let x: int = 10;\n\
         \x20   let t: Task<int> = spawn { return x; };\n\
         \x20   let r: int = t.join();\n\
         \x20   return r + x;\n\
         }\n",
    );
}

#[test]
fn test_task_must_join_with_span() {
    // Task must-join error should have a real span, not synthetic 0:0
    let result = yorum::compile_to_ir(
        "fn main() -> int {\n\
         \x20   let t: Task<int> = spawn { return 42; };\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("must be joined"));
    // The span should not be 0:0 (synthetic)
    assert!(
        !err.contains("0:0"),
        "task error should have real span, not synthetic 0:0"
    );
}

#[test]
fn test_multiple_moves_different_vars() {
    // Moving different variables is fine
    compile(
        "struct Foo { x: int }\n\
         fn f() -> int {\n\
         \x20   let a: Foo = Foo { x: 1 };\n\
         \x20   let b: Foo = Foo { x: 2 };\n\
         \x20   let c: Foo = a;\n\
         \x20   let d: Foo = b;\n\
         \x20   return c.x + d.x;\n\
         }\n",
    );
}

// ═══════════════════════════════════════════════════════════════
//  Networking builtins — TCP
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_tcp_connect_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_connect(\"127.0.0.1\", 8080);\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_tcp_connect_wrong_args() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_connect(8080, \"127.0.0.1\");\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_tcp_listen_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_listen(\"0.0.0.0\", 9090);\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_tcp_accept_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_listen(\"0.0.0.0\", 9090);\n\
         \x20 let client: int = tcp_accept(fd);\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_tcp_send_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_connect(\"127.0.0.1\", 80);\n\
         \x20 let sent: int = tcp_send(fd, \"hello\");\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_tcp_recv_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_connect(\"127.0.0.1\", 80);\n\
         \x20 let data: string = tcp_recv(fd, 1024);\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_tcp_close_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_connect(\"127.0.0.1\", 80);\n\
         \x20 tcp_close(fd);\n\
         \x20 return 0;\n\
         }\n",
    );
}

// ═══════════════════════════════════════════════════════════════
//  Networking builtins — UDP
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_udp_socket_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let fd: int = udp_socket();\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_udp_bind_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let fd: int = udp_socket();\n\
         \x20 let r: int = udp_bind(fd, \"0.0.0.0\", 5000);\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_udp_send_to_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let fd: int = udp_socket();\n\
         \x20 let sent: int = udp_send_to(fd, \"hello\", \"127.0.0.1\", 5000);\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_udp_recv_from_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let fd: int = udp_socket();\n\
         \x20 let data: string = udp_recv_from(fd, 1024);\n\
         \x20 return 0;\n\
         }\n",
    );
}

// ═══════════════════════════════════════════════════════════════
//  Networking builtins — DNS + HTTP
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_dns_resolve_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let ip: string = dns_resolve(\"localhost\");\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_http_request_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let resp: string = http_request(\"GET\", \"http://example.com\", \"\", \"\");\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_http_get_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let resp: string = http_get(\"http://example.com\");\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_http_post_typecheck() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let resp: string = http_post(\"http://example.com/api\", \"{{}}\");\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_networking_in_pure_fn_rejected() {
    let result = yorum::typecheck("pure fn f() -> int { return tcp_connect(\"x\", 80); }");
    assert!(result.is_err());
    let result = yorum::typecheck("pure fn f() -> int { return udp_socket(); }");
    assert!(result.is_err());
    let result = yorum::typecheck("pure fn f() -> string { return dns_resolve(\"x\"); }");
    assert!(result.is_err());
    let result = yorum::typecheck("pure fn f() -> string { return http_get(\"http://x\"); }");
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  Networking builtins — IR compilation
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_tcp_connect_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_connect(\"127.0.0.1\", 8080);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @tcp_connect"));
    assert!(ir.contains("define i64 @tcp_connect(ptr %host, i64 %port)"));
}

#[test]
fn test_tcp_listen_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_listen(\"0.0.0.0\", 9090);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @tcp_listen"));
    assert!(ir.contains("define i64 @tcp_listen(ptr %host, i64 %port)"));
}

#[test]
fn test_tcp_send_recv_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_connect(\"127.0.0.1\", 80);\n\
         \x20 let sent: int = tcp_send(fd, \"GET / HTTP/1.0\\r\\n\\r\\n\");\n\
         \x20 let resp: string = tcp_recv(fd, 4096);\n\
         \x20 tcp_close(fd);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @tcp_send"));
    assert!(ir.contains("call ptr @tcp_recv"));
    assert!(ir.contains("call void @tcp_close"));
}

#[test]
fn test_udp_socket_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let fd: int = udp_socket();\n\
         \x20 tcp_close(fd);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @udp_socket"));
    assert!(ir.contains("define i64 @udp_socket()"));
}

#[test]
fn test_udp_send_to_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let fd: int = udp_socket();\n\
         \x20 let sent: int = udp_send_to(fd, \"hello\", \"127.0.0.1\", 5000);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @udp_send_to"));
}

#[test]
fn test_dns_resolve_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let ip: string = dns_resolve(\"localhost\");\n\
         \x20 print_str(ip);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @dns_resolve"));
    assert!(ir.contains("define ptr @dns_resolve(ptr %hostname)"));
}

#[test]
fn test_http_request_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let r: string = http_request(\"GET\", \"http://example.com\", \"\", \"\");\n\
         \x20 print_str(r);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @http_request"));
    assert!(ir.contains("define ptr @http_request(ptr %method, ptr %url, ptr %headers, ptr %body)"));
}

#[test]
fn test_http_get_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let r: string = http_get(\"http://example.com\");\n\
         \x20 print_str(r);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @http_get"));
    assert!(ir.contains("define ptr @http_get(ptr %url)"));
}

#[test]
fn test_http_post_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let r: string = http_post(\"http://example.com/api\", \"data\");\n\
         \x20 print_str(r);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call ptr @http_post"));
    assert!(ir.contains("define ptr @http_post(ptr %url, ptr %body)"));
}

#[test]
fn test_networking_combined_compiles() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let server: int = tcp_listen(\"0.0.0.0\", 8080);\n\
         \x20 if server < 0 { return 1; }\n\
         \x20 let client: int = tcp_accept(server);\n\
         \x20 if client < 0 {\n\
         \x20   tcp_close(server);\n\
         \x20   return 1;\n\
         \x20 }\n\
         \x20 let data: string = tcp_recv(client, 1024);\n\
         \x20 let sent: int = tcp_send(client, \"HTTP/1.0 200 OK\\r\\n\\r\\nHello\");\n\
         \x20 tcp_close(client);\n\
         \x20 tcp_close(server);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("call i64 @tcp_listen"));
    assert!(ir.contains("call i64 @tcp_accept"));
    assert!(ir.contains("call ptr @tcp_recv"));
    assert!(ir.contains("call i64 @tcp_send"));
    assert!(ir.contains("call void @tcp_close"));
}

#[test]
fn test_networking_with_ownership() {
    // Networking with struct ownership — fd is int (copy type), no move issues
    compile(
        "struct Server { fd: int }\n\
         fn start() -> Server {\n\
         \x20 let fd: int = tcp_listen(\"0.0.0.0\", 3000);\n\
         \x20 return Server { fd: fd };\n\
         }\n\
         fn main() -> int {\n\
         \x20 let s: Server = start();\n\
         \x20 tcp_close(s.fd);\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_networking_ir_definitions() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let fd: int = tcp_connect(\"x\", 80);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("define i64 @tcp_connect"));
    assert!(ir.contains("define i64 @tcp_listen"));
    assert!(ir.contains("define i64 @tcp_accept"));
    assert!(ir.contains("define i64 @tcp_send"));
    assert!(ir.contains("define ptr @tcp_recv"));
    assert!(ir.contains("define void @tcp_close"));
    assert!(ir.contains("define i64 @udp_socket"));
    assert!(ir.contains("define i64 @udp_bind"));
    assert!(ir.contains("define i64 @udp_send_to"));
    assert!(ir.contains("define ptr @udp_recv_from"));
    assert!(ir.contains("define ptr @dns_resolve"));
    assert!(ir.contains("define ptr @http_request"));
    assert!(ir.contains("define ptr @http_get"));
    assert!(ir.contains("define ptr @http_post"));
}

// ═══════════════════════════════════════════════════════════════
//  v1.0 — Robustness tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_empty_input_compiles() {
    // Empty / whitespace-only input should not panic — it compiles to a valid (empty) module
    let result = yorum::compile_to_ir("");
    assert!(result.is_ok());
    let result2 = yorum::compile_to_ir("   \n\n  ");
    assert!(result2.is_ok());
}

#[test]
fn test_unterminated_string_error() {
    let result = yorum::compile_to_ir("fn f() -> string { return \"hello; }");
    assert!(result.is_err());
}

#[test]
fn test_unterminated_block_comment() {
    // Unterminated block comment — lexer should not panic
    let result = yorum::compile_to_ir("/* unclosed comment\nfn f() -> int { return 0; }");
    // Whether this errors or not, the key invariant is no panic
    let _ = result;
}

#[test]
fn test_invalid_token_error() {
    let result = yorum::compile_to_ir("fn f() -> int { let x: int = @; return x; }");
    assert!(result.is_err());
    let result2 = yorum::compile_to_ir("fn f() -> int { let x: int = $; return x; }");
    assert!(result2.is_err());
}

#[test]
fn test_missing_semicolon_error() {
    let result = yorum::compile_to_ir("fn f() -> int { return 1 }");
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  v1.0 — Parser error tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_parse_error_unclosed_brace() {
    let result = yorum::compile_to_ir("fn f() -> int { return 1;");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("parse error"));
}

#[test]
fn test_parse_error_unclosed_paren() {
    let result = yorum::compile_to_ir("fn f(x: int -> int { return x; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected ')'"));
}

#[test]
fn test_parse_error_missing_type_annotation() {
    let result = yorum::compile_to_ir("fn f() -> int { let x = 1; return x; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected ':'"));
}

#[test]
fn test_parse_error_missing_fn_name() {
    let result = yorum::compile_to_ir("fn () -> int { return 0; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected identifier"));
}

#[test]
fn test_parse_error_keyword_as_variable() {
    let result = yorum::compile_to_ir("fn f() -> int { let while: int = 1; return while; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected identifier"));
}

#[test]
fn test_parse_error_double_arrow() {
    let result = yorum::compile_to_ir("fn f() -> -> int { return 0; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected type"));
}

#[test]
fn test_parse_error_missing_return_type() {
    let result = yorum::compile_to_ir("fn f() { return 1; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected '->'"));
}

#[test]
fn test_parse_error_struct_missing_field_type() {
    let result = yorum::compile_to_ir("struct S { x; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected ':'"));
}

#[test]
fn test_parse_error_array_unclosed() {
    let result = yorum::compile_to_ir("fn f() -> [int] { let a: [int] = [1, 2; return a; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected ']'"));
}

#[test]
fn test_parse_error_if_missing_brace() {
    let result = yorum::compile_to_ir("fn f(x: int) -> int { if x > 0 return x; return 0; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected '{'"));
}

#[test]
fn test_parse_error_while_missing_body() {
    let result = yorum::compile_to_ir("fn f() -> int { while true return 0; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("expected '{'"));
}

#[test]
fn test_parse_error_empty_enum_accepted() {
    // Empty enum is accepted by the parser — no variants, but valid syntax
    let result = yorum::compile_to_ir("enum E { }\nfn main() -> int { return 0; }");
    assert!(result.is_ok());
}

#[test]
fn test_parse_error_match_no_arms_accepted() {
    // Match with no arms is accepted — potentially unreachable at runtime
    let result =
        yorum::compile_to_ir("fn f(x: int) -> int { match x { } }\nfn main() -> int { return 0; }");
    assert!(result.is_ok());
}

#[test]
fn test_parse_error_duplicate_function() {
    let result = yorum::compile_to_ir("fn f() -> int { return 1; }\nfn f() -> int { return 2; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("already defined"));
}

#[test]
fn test_parse_error_nested_function_rejected() {
    // Functions cannot be nested — should fail at parse or type-check level
    let result = yorum::compile_to_ir(
        "fn outer() -> int { fn inner() -> int { return 1; } return inner(); }",
    );
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  v1.0 — Module system tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_module_pub_struct_accessible() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    std::fs::write(
        src.join("shapes.yrm"),
        "module shapes;\n\
         pub struct Point { x: int, y: int }\n\
         pub fn sum_xy(x: int, y: int) -> int { return x + y; }\n",
    )
    .unwrap();

    std::fs::write(
        src.join("main.yrm"),
        "module main;\n\
         use shapes;\n\
         fn main() -> int {\n\
         \x20   let p: Point = Point { x: 1, y: 2 };\n\
         \x20   let s: int = sum_xy(p.x, p.y);\n\
         \x20   print_int(s);\n\
         \x20   return 0;\n\
         }\n",
    )
    .unwrap();

    let ir = yorum::compile_project(dir.path()).expect("pub struct/fn should be accessible");
    assert!(ir.contains("define i64 @main"));
    assert!(ir.contains("shapes__Point"));
}

#[test]
fn test_module_private_fn_rejected() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    std::fs::write(
        src.join("secret.yrm"),
        "module secret;\n\
         fn hidden() -> int { return 42; }\n",
    )
    .unwrap();

    std::fs::write(
        src.join("main.yrm"),
        "module main;\n\
         use secret;\n\
         fn main() -> int { return hidden(); }\n",
    )
    .unwrap();

    let result = yorum::compile_project(dir.path());
    assert!(result.is_err());
}

#[test]
fn test_module_pub_enum_accessible() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    std::fs::write(
        src.join("types.yrm"),
        "module types;\n\
         pub enum Color { Red, Green, Blue }\n\
         pub fn make_red() -> int { return 0; }\n",
    )
    .unwrap();

    std::fs::write(
        src.join("main.yrm"),
        "module main;\n\
         use types;\n\
         fn main() -> int {\n\
         \x20   let x: int = make_red();\n\
         \x20   return x;\n\
         }\n",
    )
    .unwrap();

    let ir = yorum::compile_project(dir.path()).expect("pub enum module should compile");
    assert!(ir.contains("types__Color") || ir.contains("types__make_red"));
}

#[test]
fn test_module_multiple_files() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    std::fs::write(
        src.join("math.yrm"),
        "module math;\n\
         pub fn add(a: int, b: int) -> int { return a + b; }\n",
    )
    .unwrap();

    std::fs::write(
        src.join("str_utils.yrm"),
        "module str_utils;\n\
         pub fn greeting() -> string { return \"hello\"; }\n",
    )
    .unwrap();

    std::fs::write(
        src.join("main.yrm"),
        "module main;\n\
         use math;\n\
         use str_utils;\n\
         fn main() -> int {\n\
         \x20   let x: int = add(1, 2);\n\
         \x20   let s: string = greeting();\n\
         \x20   print_int(x);\n\
         \x20   return 0;\n\
         }\n",
    )
    .unwrap();

    let ir = yorum::compile_project(dir.path()).expect("3-file project should compile");
    assert!(ir.contains("@math__add"));
    assert!(ir.contains("@str_utils__greeting"));
}

#[test]
fn test_module_use_statement() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    std::fs::write(
        src.join("utils.yrm"),
        "module utils;\n\
         pub fn double(x: int) -> int { return x * 2; }\n",
    )
    .unwrap();

    std::fs::write(
        src.join("main.yrm"),
        "module main;\n\
         use utils;\n\
         fn main() -> int { return double(21); }\n",
    )
    .unwrap();

    let ir = yorum::compile_project(dir.path()).expect("use statement should work");
    assert!(ir.contains("@utils__double"));
}

#[test]
fn test_module_wrong_module_decl() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    // File is lib.yrm but declares module wrong
    std::fs::write(
        src.join("lib.yrm"),
        "module wrong;\n\
         pub fn f() -> int { return 1; }\n",
    )
    .unwrap();

    std::fs::write(
        src.join("main.yrm"),
        "module main;\n\
         fn main() -> int { return 0; }\n",
    )
    .unwrap();

    let result = yorum::compile_project(dir.path());
    assert!(result.is_err());
}

#[test]
fn test_module_missing_main() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    // Only a library module, no main
    std::fs::write(
        src.join("lib.yrm"),
        "module lib;\n\
         pub fn f() -> int { return 1; }\n",
    )
    .unwrap();

    let result = yorum::compile_project(dir.path());
    // Missing main module should produce an error
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  v1.0 — Control flow tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_nested_while_loops_compile() {
    compile(
        "fn matrix_sum() -> int {\n\
         \x20   let mut sum: int = 0;\n\
         \x20   let mut i: int = 0;\n\
         \x20   while i < 3 {\n\
         \x20       let mut j: int = 0;\n\
         \x20       while j < 3 {\n\
         \x20           sum = sum + i * 3 + j;\n\
         \x20           j = j + 1;\n\
         \x20       }\n\
         \x20       i = i + 1;\n\
         \x20   }\n\
         \x20   return sum;\n\
         }\n",
    );
}

#[test]
fn test_while_with_array_push() {
    compile(
        "fn build_array() -> [int] {\n\
         \x20   let mut arr: [int] = [0];\n\
         \x20   let mut i: int = 1;\n\
         \x20   while i < 5 {\n\
         \x20       push(arr, i);\n\
         \x20       i = i + 1;\n\
         \x20   }\n\
         \x20   return arr;\n\
         }\n",
    );
}

#[test]
fn test_for_loop_over_array() {
    compile(
        "fn sum_array() -> int {\n\
         \x20   let arr: [int] = [1, 2, 3];\n\
         \x20   let mut sum: int = 0;\n\
         \x20   for x in arr {\n\
         \x20       sum = sum + x;\n\
         \x20   }\n\
         \x20   return sum;\n\
         }\n",
    );
}

#[test]
fn test_if_without_else() {
    compile(
        "fn maybe_print(x: int) -> int {\n\
         \x20   if x > 0 {\n\
         \x20       print_int(x);\n\
         \x20   }\n\
         \x20   return 0;\n\
         }\n",
    );
}

#[test]
fn test_nested_if_else_if() {
    compile(
        "fn classify(x: int) -> int {\n\
         \x20   if x > 100 {\n\
         \x20       return 3;\n\
         \x20   } else if x > 10 {\n\
         \x20       return 2;\n\
         \x20   } else if x > 0 {\n\
         \x20       return 1;\n\
         \x20   } else {\n\
         \x20       return 0;\n\
         \x20   }\n\
         }\n",
    );
}

#[test]
fn test_match_wildcard_only() {
    compile(
        "fn always_zero(x: int) -> int {\n\
         \x20   match x {\n\
         \x20       _ => { return 0; }\n\
         \x20   }\n\
         }\n",
    );
}

#[test]
fn test_match_multiple_patterns() {
    compile(
        "fn describe(x: int) -> int {\n\
         \x20   match x {\n\
         \x20       0 => { return 0; }\n\
         \x20       1 => { return 1; }\n\
         \x20       2 => { return 2; }\n\
         \x20       _ => { return 99; }\n\
         \x20   }\n\
         }\n",
    );
}

// ═══════════════════════════════════════════════════════════════
//  v1.0 — Struct/impl/trait tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_struct_many_fields() {
    compile(
        "struct Record {\n\
         \x20   a: int, b: int, c: int, d: int,\n\
         \x20   e: int, f: int, g: int, h: int\n\
         }\n\
         fn main() -> int {\n\
         \x20   let r: Record = Record { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8 };\n\
         \x20   return r.a + r.h;\n\
         }\n",
    );
}

#[test]
fn test_struct_nested_structs() {
    compile(
        "struct Inner { value: int }\n\
         struct Outer { inner: Inner, label: int }\n\
         fn main() -> int {\n\
         \x20   let i: Inner = Inner { value: 42 };\n\
         \x20   let o: Outer = Outer { inner: i, label: 7 };\n\
         \x20   print_int(o.label);\n\
         \x20   return 0;\n\
         }\n",
    );
}

#[test]
fn test_impl_multiple_methods() {
    compile(
        "struct Counter { value: int }\n\
         impl Counter {\n\
         \x20   fn get(self: &Counter) -> int { return self.value; }\n\
         \x20   fn is_zero(self: &Counter) -> bool { return self.value == 0; }\n\
         \x20   fn double(self: &Counter) -> int { return self.value * 2; }\n\
         }\n\
         fn main() -> int {\n\
         \x20   let c: Counter = Counter { value: 5 };\n\
         \x20   print_int(c.get());\n\
         \x20   print_int(c.double());\n\
         \x20   return 0;\n\
         }\n",
    );
}

#[test]
fn test_trait_multiple_methods() {
    compile(
        "struct Vec2 { x: int, y: int }\n\
         trait Geometric {\n\
         \x20   fn area(self: &Self) -> int;\n\
         \x20   fn perimeter(self: &Self) -> int;\n\
         }\n\
         impl Geometric for Vec2 {\n\
         \x20   fn area(self: &Vec2) -> int { return self.x * self.y; }\n\
         \x20   fn perimeter(self: &Vec2) -> int { return 2 * (self.x + self.y); }\n\
         }\n\
         fn main() -> int {\n\
         \x20   let v: Vec2 = Vec2 { x: 3, y: 4 };\n\
         \x20   return v.area() + v.perimeter();\n\
         }\n",
    );
}

#[test]
fn test_trait_impl_missing_method_error() {
    // Trait requires two methods but impl only provides one
    let result = yorum::typecheck(
        "struct Foo { x: int }\n\
         trait Bar {\n\
         \x20   fn baz(self: &Self) -> int;\n\
         \x20   fn qux(self: &Self) -> int;\n\
         }\n\
         impl Bar for Foo {\n\
         \x20   fn baz(self: &Foo) -> int { return self.x; }\n\
         }\n",
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("missing method"));
}

// ═══════════════════════════════════════════════════════════════
//  v1.0 — Example program compilation tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_example_hello_compiles() {
    let source = include_str!("../examples/hello.yrm");
    yorum::compile_to_ir(source).expect("examples/hello.yrm should compile");
}

#[test]
fn test_example_fibonacci_compiles() {
    let source = include_str!("../examples/fibonacci.yrm");
    yorum::compile_to_ir(source).expect("examples/fibonacci.yrm should compile");
}

#[test]
fn test_example_structs_compiles() {
    let source = include_str!("../examples/structs.yrm");
    yorum::compile_to_ir(source).expect("examples/structs.yrm should compile");
}

#[test]
fn test_example_pattern_match_compiles() {
    let source = include_str!("../examples/pattern_match.yrm");
    yorum::compile_to_ir(source).expect("examples/pattern_match.yrm should compile");
}

#[test]
fn test_example_effects_compiles() {
    let source = include_str!("../examples/effects.yrm");
    yorum::compile_to_ir(source).expect("examples/effects.yrm should compile");
}

#[test]
fn test_example_methods_compiles() {
    let source = include_str!("../examples/methods.yrm");
    yorum::compile_to_ir(source).expect("examples/methods.yrm should compile");
}

#[test]
fn test_example_traits_compiles() {
    let source = include_str!("../examples/traits.yrm");
    yorum::compile_to_ir(source).expect("examples/traits.yrm should compile");
}

#[test]
fn test_example_generics_compiles() {
    let source = include_str!("../examples/generics.yrm");
    yorum::compile_to_ir(source).expect("examples/generics.yrm should compile");
}

#[test]
fn test_example_closures_compiles() {
    let source = include_str!("../examples/closures.yrm");
    yorum::compile_to_ir(source).expect("examples/closures.yrm should compile");
}

#[test]
fn test_example_arrays_compiles() {
    let source = include_str!("../examples/arrays.yrm");
    yorum::compile_to_ir(source).expect("examples/arrays.yrm should compile");
}

#[test]
fn test_example_strings_compiles() {
    let source = include_str!("../examples/strings.yrm");
    yorum::compile_to_ir(source).expect("examples/strings.yrm should compile");
}

#[test]
fn test_example_concurrency_compiles() {
    let source = include_str!("../examples/concurrency.yrm");
    yorum::compile_to_ir(source).expect("examples/concurrency.yrm should compile");
}

#[test]
fn test_example_contracts_compiles() {
    let source = include_str!("../examples/contracts.yrm");
    yorum::compile_to_ir(source).expect("examples/contracts.yrm should compile");
}

#[test]
fn test_example_chars_compiles() {
    let source = include_str!("../examples/chars.yrm");
    yorum::compile_to_ir(source).expect("examples/chars.yrm should compile");
}

#[test]
fn test_example_string_ops_compiles() {
    let source = include_str!("../examples/string_ops.yrm");
    yorum::compile_to_ir(source).expect("examples/string_ops.yrm should compile");
}

#[test]
fn test_example_tcp_echo_server_compiles() {
    let source = include_str!("../examples/tcp_echo_server.yrm");
    yorum::compile_to_ir(source).expect("examples/tcp_echo_server.yrm should compile");
}

#[test]
fn test_example_tcp_echo_client_compiles() {
    let source = include_str!("../examples/tcp_echo_client.yrm");
    yorum::compile_to_ir(source).expect("examples/tcp_echo_client.yrm should compile");
}

// ═══════════════════════════════════════════════════════════════
//  Regression tests for codebase review bug fixes
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_generic_elseif_chain() {
    let ir = compile(
        "fn identity<T>(x: T) -> T { return x; }\n\
         fn main() -> int {\n\
         \x20   let a: int = identity(1);\n\
         \x20   if a == 1 {\n\
         \x20       print_int(1);\n\
         \x20   } else if a == 2 {\n\
         \x20       print_int(identity(2));\n\
         \x20   } else {\n\
         \x20       print_int(identity(3));\n\
         \x20   }\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("define i64 @identity__int"));
}

#[test]
fn test_module_elseif_rewrite() {
    let dir = tempfile::tempdir().unwrap();
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).unwrap();

    std::fs::write(
        dir.path().join("yorum.toml"),
        "[package]\nname = \"testproj\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();

    std::fs::write(
        src.join("helpers.yrm"),
        "module helpers;\n\
         pub fn classify(x: int) -> int {\n\
         \x20   if x < 0 {\n\
         \x20       return 0;\n\
         \x20   } else if x == 0 {\n\
         \x20       return 1;\n\
         \x20   } else {\n\
         \x20       return 2;\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    std::fs::write(
        src.join("main.yrm"),
        "module main;\n\
         use helpers;\n\
         fn main() -> int {\n\
         \x20   let r: int = classify(5);\n\
         \x20   print_int(r);\n\
         \x20   return 0;\n\
         }\n",
    )
    .unwrap();

    let ir = yorum::compile_project(dir.path()).expect("module elseif should compile");
    assert!(ir.contains("@helpers__classify"));
}

#[test]
fn test_short_circuit_and() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let a: bool = true;\n\
         \x20   let b: bool = false;\n\
         \x20   if a and b {\n\
         \x20       print_int(1);\n\
         \x20   }\n\
         \x20   return 0;\n\
         }\n",
    );
    // Short-circuit `and` should produce phi nodes and branch labels
    assert!(ir.contains("and_rhs"));
    assert!(ir.contains("and_merge"));
    assert!(ir.contains("phi i1"));
}

#[test]
fn test_short_circuit_or() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let a: bool = true;\n\
         \x20   let b: bool = false;\n\
         \x20   if a or b {\n\
         \x20       print_int(1);\n\
         \x20   }\n\
         \x20   return 0;\n\
         }\n",
    );
    // Short-circuit `or` should produce phi nodes and branch labels
    assert!(ir.contains("or_rhs"));
    assert!(ir.contains("or_merge"));
    assert!(ir.contains("phi i1"));
}

#[test]
fn test_return_unit_bare() {
    let ir = compile(
        "fn do_nothing() -> unit {\n\
         \x20   return;\n\
         }\n\
         fn main() -> int {\n\
         \x20   do_nothing();\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("define void @do_nothing"));
    assert!(ir.contains("ret void"));
}

#[test]
fn test_slice_in_pure_fn() {
    parse_and_check(
        "pure fn first_two(arr: [int]) -> [int] {\n\
         \x20   return slice(arr, 0, 2);\n\
         }\n\
         fn main() -> int { return 0; }\n",
    );
}

#[test]
fn test_reverse_in_pure_fn() {
    parse_and_check(
        "pure fn reversed(arr: [int]) -> [int] {\n\
         \x20   return reverse(arr);\n\
         }\n\
         fn main() -> int { return 0; }\n",
    );
}

#[test]
fn test_concat_arrays_in_pure_fn() {
    parse_and_check(
        "pure fn joined(a: [int], b: [int]) -> [int] {\n\
         \x20   return concat_arrays(a, b);\n\
         }\n\
         fn main() -> int { return 0; }\n",
    );
}

#[test]
fn test_map_remove_tombstone() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let m: Map = map_new();\n\
         \x20   map_set(m, \"key1\", 10);\n\
         \x20   map_set(m, \"key2\", 20);\n\
         \x20   map_remove(m, \"key1\");\n\
         \x20   let v: int = map_get(m, \"key2\");\n\
         \x20   print_int(v);\n\
         \x20   return 0;\n\
         }\n",
    );
    // Verify tombstone-based removal is emitted (flag=2 instead of flag=0)
    assert!(ir.contains("store i8 2"));
}

#[test]
fn test_spawn_with_captures() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let x: int = 42;\n\
         \x20   let t: Task<unit> = spawn {\n\
         \x20       print_int(x);\n\
         \x20   };\n\
         \x20   t.join();\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("@__spawn_"));
    assert!(ir.contains("call i32 @pthread_create"));
}

// ═══════════════════════════════════════════════════════════════
//  Codegen P1 bug fix tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_short_circuit_phi_after_branching_function() {
    // Bug 1: emit_function didn't reset current_block, so the first and/or
    // PHI in the second function could reference a label from the first function.
    let ir = compile(
        "fn branchy(x: int) -> int {\n\
         \x20   if x > 0 {\n\
         \x20       return 1;\n\
         \x20   } else {\n\
         \x20       return 0;\n\
         \x20   }\n\
         }\n\
         fn main() -> int {\n\
         \x20   let a: bool = true and false;\n\
         \x20   print_bool(a);\n\
         \x20   return 0;\n\
         }\n",
    );
    // Extract main function body
    let main_start = ir.find("define i64 @main(").expect("main not found");
    let main_ir = &ir[main_start..];
    // Every PHI predecessor in main must reference labels within main (e.g., entry, and.rhs, and.merge)
    // and NOT labels from branchy (e.g., ifcont, then, else)
    for line in main_ir.lines() {
        if line.contains("phi ") {
            assert!(
                !line.contains("ifcont") && !line.contains("%then") && !line.contains("%else"),
                "PHI in main references label from branchy: {}",
                line
            );
        }
    }
}

#[test]
fn test_map_tombstone_churn() {
    // Bug 2: Without tombstone tracking in load factor, insert/remove churn
    // could fill all slots with tombstones, causing infinite probe in find_slot.
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let m: Map = map_new();\n\
         \x20   map_set(m, \"a\", 1);\n\
         \x20   map_remove(m, \"a\");\n\
         \x20   map_set(m, \"b\", 2);\n\
         \x20   map_remove(m, \"b\");\n\
         \x20   map_set(m, \"c\", 3);\n\
         \x20   let v: int = map_get(m, \"c\");\n\
         \x20   print_int(v);\n\
         \x20   return 0;\n\
         }\n",
    );
    // map struct is now 48 bytes (was 40) to hold tombstone counter at offset 40
    assert!(
        ir.contains("call ptr @malloc(i64 48)"),
        "map_new should allocate 48 bytes for map struct"
    );
    // map_remove should increment tombstone counter at offset 40
    assert!(
        ir.contains("getelementptr i8, ptr %map, i64 40"),
        "tombstone counter should be tracked at offset 40"
    );
}

#[test]
fn test_spawn_env_alignment() {
    // Bug 3: Spawn env malloc used sum of raw field sizes, ignoring alignment.
    // For { i1, i64 } this gave 9 bytes but LLVM struct is 16.
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let flag: bool = true;\n\
         \x20   let t: Task<unit> = spawn {\n\
         \x20       print_bool(flag);\n\
         \x20   };\n\
         \x20   t.join();\n\
         \x20   return 0;\n\
         }\n",
    );
    // Should use LLVM sizeof idiom (getelementptr from null + ptrtoint)
    // instead of hardcoded byte count
    assert!(
        ir.contains("getelementptr %__spawn_env_"),
        "spawn env should use LLVM sizeof idiom with named struct type"
    );
    assert!(
        ir.contains("ptrtoint ptr"),
        "spawn env should use ptrtoint for size calculation"
    );
}

#[test]
fn test_unit_let_binding_no_invalid_ir() {
    // Unit-typed let bindings must not emit `alloca void` or `store void`,
    // which are invalid LLVM IR.
    let ir = compile(
        "fn do_nothing() -> unit {\n\
         \x20   return;\n\
         }\n\
         fn main() -> int {\n\
         \x20   let x: unit = do_nothing();\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(
        !ir.contains("alloca void"),
        "unit let binding must not emit alloca void"
    );
    assert!(
        !ir.contains("store void"),
        "unit let binding must not emit store void"
    );
    // The void call should still be emitted for side effects
    assert!(ir.contains("call void @do_nothing()"));
}

// ═══════════════════════════════════════════════════════════════
//  v1.1 — Compound assignment operators
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_compound_add() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut x: int = 10;\n\
         \x20   x += 5;\n\
         \x20   return x;\n\
         }\n",
    );
    // Desugared to x = x + 5, so we expect an add instruction
    assert!(ir.contains("add i64"));
}

#[test]
fn test_compound_all_ops() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut a: int = 10;\n\
         \x20   a += 1;\n\
         \x20   a -= 2;\n\
         \x20   a *= 3;\n\
         \x20   a /= 4;\n\
         \x20   a %= 5;\n\
         \x20   return a;\n\
         }\n",
    );
    assert!(ir.contains("add i64"));
    assert!(ir.contains("sub i64"));
    assert!(ir.contains("mul i64"));
    assert!(ir.contains("sdiv i64"));
    assert!(ir.contains("srem i64"));
}

#[test]
fn test_compound_array_index() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut arr: [int] = [1, 2, 3];\n\
         \x20   arr[0] += 10;\n\
         \x20   return arr[0];\n\
         }\n",
    );
    assert!(ir.contains("add i64"));
}

#[test]
fn test_compound_field() {
    let ir = compile(
        "struct Point { x: int, y: int }\n\
         fn main() -> int {\n\
         \x20   let mut p: Point = Point { x: 1, y: 2 };\n\
         \x20   p.x += 10;\n\
         \x20   return p.x;\n\
         }\n",
    );
    assert!(ir.contains("add i64"));
}

#[test]
fn test_compound_type_error() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   let mut x: int = 5;\n\
         \x20   x += true;\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  v1.1 — Bitwise operators
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_bitwise_and_or_xor() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let a: int = 5 & 3;\n\
         \x20   let b: int = 5 | 3;\n\
         \x20   let c: int = 5 ^ 3;\n\
         \x20   return a;\n\
         }\n",
    );
    assert!(ir.contains("and i64"));
    assert!(ir.contains("or i64"));
    assert!(ir.contains("xor i64"));
}

#[test]
fn test_bitwise_shift() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let x: int = 1;\n\
         \x20   let y: int = 3;\n\
         \x20   let a: int = x << y;\n\
         \x20   let z: int = 16;\n\
         \x20   let w: int = 2;\n\
         \x20   let b: int = z >> w;\n\
         \x20   return a;\n\
         }\n",
    );
    assert!(ir.contains("shl i64"));
    assert!(ir.contains("ashr i64"));
}

#[test]
fn test_bitwise_precedence() {
    // a + b & c should parse as (a + b) & c since + binds tighter than &
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let a: int = 1;\n\
         \x20   let b: int = 2;\n\
         \x20   let c: int = 3;\n\
         \x20   let r: int = a + b & c;\n\
         \x20   return r;\n\
         }\n",
    );
    // The add should appear before the and in the IR
    let add_pos = ir.find("add i64").unwrap();
    let and_pos = ir.rfind("and i64").unwrap();
    assert!(add_pos < and_pos, "add should appear before bitwise and");
}

#[test]
fn test_bitwise_type_error() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   let x: int = 1.0 & 2;\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_shift_not_confused_with_generics() {
    // Ensure >> in expression context is parsed as shift, not two Gt tokens
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let x: int = 16;\n\
         \x20   let y: int = x >> 2;\n\
         \x20   return y;\n\
         }\n",
    );
    assert!(ir.contains("ashr i64"));
}

// ═══════════════════════════════════════════════════════════════
//  v1.1 — break and continue
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_break_in_while() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut x: int = 0;\n\
         \x20   while true {\n\
         \x20       x += 1;\n\
         \x20       if x == 5 { break; }\n\
         \x20   }\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(ir.contains("br label %while.end"));
}

#[test]
fn test_continue_in_while() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut x: int = 0;\n\
         \x20   let mut i: int = 0;\n\
         \x20   while i < 10 {\n\
         \x20       i += 1;\n\
         \x20       if i == 5 { continue; }\n\
         \x20       x += 1;\n\
         \x20   }\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(ir.contains("br label %while.cond"));
}

#[test]
fn test_break_in_for() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let arr: [int] = [1, 2, 3, 4, 5];\n\
         \x20   let mut sum: int = 0;\n\
         \x20   for x in arr {\n\
         \x20       if x == 4 { break; }\n\
         \x20       sum += x;\n\
         \x20   }\n\
         \x20   return sum;\n\
         }\n",
    );
    assert!(ir.contains("br label %for.end"));
}

#[test]
fn test_continue_in_for() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let arr: [int] = [1, 2, 3, 4, 5];\n\
         \x20   let mut sum: int = 0;\n\
         \x20   for x in arr {\n\
         \x20       if x == 3 { continue; }\n\
         \x20       sum += x;\n\
         \x20   }\n\
         \x20   return sum;\n\
         }\n",
    );
    // continue jumps to for.inc (the increment block)
    assert!(ir.contains("br label %for.inc"));
}

#[test]
fn test_break_outside_loop_error() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   break;\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_continue_outside_loop_error() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   continue;\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_nested_break() {
    // Inner break should not exit outer loop
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut count: int = 0;\n\
         \x20   let mut i: int = 0;\n\
         \x20   while i < 3 {\n\
         \x20       let mut j: int = 0;\n\
         \x20       while j < 3 {\n\
         \x20           if j == 1 { break; }\n\
         \x20           count += 1;\n\
         \x20           j += 1;\n\
         \x20       }\n\
         \x20       i += 1;\n\
         \x20   }\n\
         \x20   return count;\n\
         }\n",
    );
    // Should have two while.end labels (one for inner, one for outer)
    let count = ir.matches("while.end").count();
    assert!(
        count >= 2,
        "expected at least 2 while.end labels for nested loops"
    );
}

// ═══════════════════════════════════════════════════════════════
//  v1.1 — Range expressions (for i in 0..n)
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_for_range_basic() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let mut sum: int = 0;\n\
         \x20   for i in 0..10 {\n\
         \x20       sum += i;\n\
         \x20   }\n\
         \x20   return sum;\n\
         }\n",
    );
    // Should have for.cond, for.body, for.inc, for.end labels
    assert!(ir.contains("for.cond"));
    assert!(ir.contains("for.body"));
    assert!(ir.contains("for.inc"));
    assert!(ir.contains("for.end"));
    // Should compare with icmp slt
    assert!(ir.contains("icmp slt i64"));
}

#[test]
fn test_for_range_variables() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let n: int = 5;\n\
         \x20   let mut sum: int = 0;\n\
         \x20   for i in 0..n {\n\
         \x20       sum += i;\n\
         \x20   }\n\
         \x20   return sum;\n\
         }\n",
    );
    assert!(ir.contains("for.cond"));
    assert!(ir.contains("icmp slt i64"));
}

#[test]
fn test_for_range_expression_bounds() {
    // 0..n + 1 should parse as 0..(n + 1) since + binds tighter than ..
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let n: int = 5;\n\
         \x20   let mut sum: int = 0;\n\
         \x20   for i in 0..n + 1 {\n\
         \x20       sum += i;\n\
         \x20   }\n\
         \x20   return sum;\n\
         }\n",
    );
    // The add for n+1 should happen before the loop comparison
    assert!(ir.contains("for.cond"));
}

#[test]
fn test_for_range_type_error() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   for i in 0..true {\n\
         \x20       print_int(i);\n\
         \x20   }\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_range_outside_for_error() {
    // Range is not a standalone expression — using it as a value should fail
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20   let x: int = 0..10;\n\
         \x20   return x;\n\
         }\n",
    );
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  String interpolation tests (v1.2)
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_string_interp_basic() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let name: string = \"world\";\n\
         \x20 let s: string = \"hello {name}\";\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("@str_concat"));
}

#[test]
fn test_string_interp_int() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let x: int = 42;\n\
         \x20 let s: string = \"x = {x}\";\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("@int_to_str"));
    assert!(ir.contains("@str_concat"));
}

#[test]
fn test_string_interp_float() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let pi: float = 3.14;\n\
         \x20 let s: string = \"pi = {pi}\";\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("@float_to_str"));
}

#[test]
fn test_string_interp_bool() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let flag: bool = true;\n\
         \x20 let s: string = \"flag = {flag}\";\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("@bool_to_str"));
}

#[test]
fn test_string_interp_escape_braces() {
    // {{x}} should produce literal {x} — a plain string, not an interpolation
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let s: string = \"{{x}}\";\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    // Verify at AST level: should be a StringLit, not desugared
    let json = parse_to_json(
        "fn main() -> int {\n\
         \x20 let s: string = \"{{x}}\";\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(json.contains("{x}"));
}

#[test]
fn test_string_interp_complex_expr() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let a: int = 3;\n\
         \x20 let b: int = 4;\n\
         \x20 let s: string = \"sum = {a + b}\";\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("@str_concat"));
    assert!(ir.contains("@int_to_str"));
}

#[test]
fn test_float_to_str_builtin() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let s: string = float_to_str(3.14);\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("@float_to_str"));
}

#[test]
fn test_bool_to_str_builtin() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let s: string = bool_to_str(true);\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("@bool_to_str"));
}

// ── Tuple type tests ──────────────────────────────────────────────

#[test]
fn test_tuple_literal() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let t: (int, string) = (42, \"hello\");\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("%tuple.int.string = type { i64, ptr }"));
}

#[test]
fn test_tuple_field_access() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let t: (int, string) = (42, \"hello\");\n\
         \x20 let x: int = t.0;\n\
         \x20 let s: string = t.1;\n\
         \x20 print_int(x);\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("getelementptr %tuple.int.string"));
}

#[test]
fn test_tuple_destructure() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let t: (int, string) = (42, \"hello\");\n\
         \x20 let (a, b): (int, string) = t;\n\
         \x20 print_int(a);\n\
         \x20 print_str(b);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("getelementptr %tuple.int.string"));
}

#[test]
fn test_tuple_nested() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let inner: (bool, char) = (true, 'x');\n\
         \x20 let t: (int, (bool, char)) = (1, inner);\n\
         \x20 let x: int = t.0;\n\
         \x20 print_int(x);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("%tuple.int.tuple.bool.char = type"));
    assert!(ir.contains("%tuple.bool.char = type { i1, i8 }"));
}

#[test]
fn test_tuple_function_param_return() {
    let ir = compile(
        "fn swap(t: (int, int)) -> (int, int) {\n\
         \x20 let a: int = t.0;\n\
         \x20 let b: int = t.1;\n\
         \x20 return (b, a);\n\
         }\n\
         fn main() -> int {\n\
         \x20 let t: (int, int) = (1, 2);\n\
         \x20 let swapped: (int, int) = swap(t);\n\
         \x20 print_int(swapped.0);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("define %tuple.int.int @swap"));
}

#[test]
fn test_tuple_type_error() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20 let t: (int, string) = (42, 42);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_tuple_destructure_arity_mismatch() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20 let t: (int, string) = (42, \"hello\");\n\
         \x20 let (a, b, c): (int, string) = t;\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  Option<T> & Result<T, E> prelude types
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_option_some_none() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let x: Option<int> = Some(42);\n\
         \x20 match x {\n\
         \x20   Some(val) => { return val; }\n\
         \x20   None => { return 0; }\n\
         \x20 }\n\
         }\n",
    );
    assert!(ir.contains("%Option__int"));
    assert!(ir.contains("ret i64"));
}

#[test]
fn test_option_unwrap() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let x: Option<int> = Some(7);\n\
         \x20 let val: int = x.unwrap();\n\
         \x20 return val;\n\
         }\n",
    );
    assert!(ir.contains("%Option__int"));
    assert!(ir.contains("@abort"));
}

#[test]
fn test_option_is_some_is_none() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let x: Option<int> = Some(1);\n\
         \x20 let y: Option<int> = None;\n\
         \x20 if x.is_some() and y.is_none() {\n\
         \x20   return 1;\n\
         \x20 }\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("icmp eq i32"));
}

#[test]
fn test_result_ok_err() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let r: Result<int, string> = Ok(42);\n\
         \x20 match r {\n\
         \x20   Ok(val) => { return val; }\n\
         \x20   Err(msg) => { return 0; }\n\
         \x20 }\n\
         }\n",
    );
    assert!(ir.contains("%Result__int__string"));
}

#[test]
fn test_result_unwrap() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let r: Result<int, string> = Ok(5);\n\
         \x20 let val: int = r.unwrap();\n\
         \x20 return val;\n\
         }\n",
    );
    assert!(ir.contains("%Result__int__string"));
    assert!(ir.contains("@abort"));
}

#[test]
fn test_result_is_ok_is_err() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let r: Result<int, string> = Ok(1);\n\
         \x20 if r.is_ok() {\n\
         \x20   return 1;\n\
         \x20 }\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(ir.contains("icmp eq i32"));
}

#[test]
fn test_option_generic_monomorphize() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let a: Option<int> = Some(42);\n\
         \x20 let b: Option<string> = Some(\"hello\");\n\
         \x20 return a.unwrap();\n\
         }\n",
    );
    assert!(ir.contains("%Option__int"));
    assert!(ir.contains("%Option__string"));
}

#[test]
fn test_option_in_function_return() {
    let ir = compile(
        "fn find(arr: [int], target: int) -> Option<int> {\n\
         \x20 let mut i: int = 0;\n\
         \x20 while i < len(arr) {\n\
         \x20   if arr[i] == target {\n\
         \x20     return Some(i);\n\
         \x20   }\n\
         \x20   i = i + 1;\n\
         \x20 }\n\
         \x20 return None;\n\
         }\n\
         fn main() -> int {\n\
         \x20 let arr: [int] = [10, 20, 30];\n\
         \x20 let result: Option<int> = find(arr, 20);\n\
         \x20 match result {\n\
         \x20   Some(idx) => { return idx; }\n\
         \x20   None => { return -1; }\n\
         \x20 }\n\
         }\n",
    );
    assert!(ir.contains("define %Option__int @find"));
    assert!(ir.contains("%Option__int"));
}

// ═══════════════════════════════════════════════════════════════
//  v1.2 bug-fix regression tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_string_interp_variable() {
    // Fix 1: to_str must be in fn_ret_types so expr_llvm_type returns "ptr" (not "i64")
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let x: int = 42;\n\
         \x20 let s: string = \"x = {x}\";\n\
         \x20 print_str(s);\n\
         \x20 return 0;\n\
         }\n",
    );
    // str_concat should receive ptr arguments, not i64
    assert!(ir.contains("call ptr @str_concat(ptr"));
}

#[test]
fn test_tuple_from_function_return() {
    // Fix 2: tuple let binding must handle function-call RHS (value, not pointer)
    let ir = compile(
        "fn swap(a: int, b: int) -> (int, int) {\n\
         \x20 return (b, a);\n\
         }\n\
         fn main() -> int {\n\
         \x20 let t: (int, int) = swap(1, 2);\n\
         \x20 return t.0;\n\
         }\n",
    );
    assert!(ir.contains("define %tuple.int.int @swap"));
}

#[test]
fn test_tuple_array() {
    // Fix 3: tuple type names must be consistent (semantic names, not LLVM names)
    let ir = compile(
        "fn main() -> int {\n\
         \x20 let arr: [(int, string)] = [(1, \"a\"), (2, \"b\")];\n\
         \x20 return 0;\n\
         }\n",
    );
    // The type must use semantic names consistently
    assert!(ir.contains("%tuple.int.string = type"));
}

#[test]
fn test_option_type_arg_mismatch() {
    // Fix 4: Option<int> must reject Some("hello") which infers as Option<string>
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20 let x: Option<int> = Some(\"hello\");\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_interp_trailing_tokens() {
    // Fix 5: interpolation must reject trailing tokens in expressions
    let result = yorum::compile_to_ir(
        "fn main() -> int {\n\
         \x20 let s: string = \"bad {1 2}\";\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

// ═══════════════════════════════════════════════════════════════
//  v1.3 — Match exhaustiveness checking
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_match_exhaustive_missing_variant() {
    let result = yorum::typecheck(
        "enum Color { Red, Green, Blue }\n\
         fn main() -> int {\n\
         \x20 let c: Color = Red;\n\
         \x20 match c {\n\
         \x20   Red => { print_int(1); }\n\
         \x20   Green => { print_int(2); }\n\
         \x20 }\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
    let msg = result.unwrap_err();
    assert!(msg.contains("non-exhaustive match"));
    assert!(msg.contains("Blue"));
}

#[test]
fn test_match_exhaustive_all_variants() {
    parse_and_check(
        "enum Color { Red, Green, Blue }\n\
         fn main() -> int {\n\
         \x20 let c: Color = Red;\n\
         \x20 match c {\n\
         \x20   Red => { print_int(1); }\n\
         \x20   Green => { print_int(2); }\n\
         \x20   Blue => { print_int(3); }\n\
         \x20 }\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_match_exhaustive_wildcard() {
    parse_and_check(
        "enum Color { Red, Green, Blue }\n\
         fn main() -> int {\n\
         \x20 let c: Color = Red;\n\
         \x20 match c {\n\
         \x20   Red => { print_int(1); }\n\
         \x20   _ => { print_int(0); }\n\
         \x20 }\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_match_exhaustive_binding_catchall() {
    parse_and_check(
        "enum Color { Red, Green, Blue }\n\
         fn main() -> int {\n\
         \x20 let c: Color = Red;\n\
         \x20 match c {\n\
         \x20   Red => { print_int(1); }\n\
         \x20   other => { print_int(0); }\n\
         \x20 }\n\
         \x20 return 0;\n\
         }\n",
    );
}

#[test]
fn test_match_exhaustive_option() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let x: Option<int> = Some(42);\n\
         \x20 match x {\n\
         \x20   Some(v) => { print_int(v); }\n\
         \x20   None => { print_int(0); }\n\
         \x20 }\n\
         \x20 return 0;\n\
         }\n",
    );
}

// ═══════════════════════════════════════════════════════════════
//  v1.3 — Generic Map<K, V>
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_map_generic_bare_backward_compat() {
    // Bare `Map` should still work as Map<string, int>
    let ir = compile(
        "fn main() -> int {\
           let m: Map = map_new();\
           map_set(m, \"hello\", 42);\
           let v: int = map_get(m, \"hello\");\
           return v;\
         }",
    );
    assert!(ir.contains("map_new"));
    assert!(ir.contains("map_set"));
    assert!(ir.contains("map_get"));
}

#[test]
fn test_map_generic_explicit_string_int() {
    // Explicit Map<string, int> should compile
    let ir = compile(
        "fn main() -> int {\
           let m: Map<string, int> = map_new();\
           map_set(m, \"key\", 99);\
           let v: int = map_get(m, \"key\");\
           return v;\
         }",
    );
    assert!(ir.contains("map_new"));
}

#[test]
fn test_map_generic_keys_values_types() {
    // map_keys returns [string], map_values returns [int] for Map<string, int>
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let m: Map<string, int> = map_new();\n\
         \x20 map_set(m, \"a\", 1);\n\
         \x20 let ks: [string] = map_keys(m);\n\
         \x20 let vs: [int] = map_values(m);\n\
         \x20 return len(vs);\n\
         }\n",
    );
}

#[test]
fn test_map_generic_size_has_remove() {
    // map_size, map_has, map_remove work with Map<string, int>
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let m: Map<string, int> = map_new();\n\
         \x20 map_set(m, \"x\", 10);\n\
         \x20 let s: int = map_size(m);\n\
         \x20 let h: bool = map_has(m, \"x\");\n\
         \x20 let r: bool = map_remove(m, \"x\");\n\
         \x20 return s;\n\
         }\n",
    );
}

#[test]
fn test_map_generic_float_key_error() {
    // float is not a hashable key type
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20 let m: Map<float, int> = map_new();\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("hashable"));
}

#[test]
fn test_map_generic_wrong_key_type_error() {
    // map_set with int key on Map<string, int> should error
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20 let m: Map<string, int> = map_new();\n\
         \x20 map_set(m, 42, 10);\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_map_generic_wrong_value_type_error() {
    // map_set with string value on Map<string, int> should error
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20 let m: Map<string, int> = map_new();\n\
         \x20 map_set(m, \"key\", \"not_int\");\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_map_generic_int_string() {
    // Map<int, string> — int keys
    let ir = compile(
        "fn main() -> int {\
           let m: Map<int, string> = map_new();\
           map_set(m, 42, \"hello\");\
           let v: string = map_get(m, 42);\
           return 0;\
         }",
    );
    assert!(ir.contains("map_new__int__string"));
    assert!(ir.contains("map_set__int__string"));
    assert!(ir.contains("map_get__int__string"));
}

#[test]
fn test_map_generic_char_bool() {
    // Map<char, bool> — char keys
    let ir = compile(
        "fn main() -> int {\
           let m: Map<char, bool> = map_new();\
           map_set(m, 'a', true);\
           let v: bool = map_get(m, 'a');\
           let h: bool = map_has(m, 'b');\
           return 0;\
         }",
    );
    assert!(ir.contains("map_new__char__bool"));
}

// ═══════════════════════════════════════════════════════════════
//  v1.3 — Set<T>
// ═══════════════════════════════════════════════════════════════

#[test]
fn test_set_int_add_has_remove() {
    let ir = compile(
        "fn main() -> int {\
           let s: Set<int> = set_new();\
           set_add(s, 42);\
           set_add(s, 99);\
           let h: bool = set_has(s, 42);\
           let r: bool = set_remove(s, 42);\
           return 0;\
         }",
    );
    assert!(ir.contains("set_new__int"));
    assert!(ir.contains("set_add__int"));
    assert!(ir.contains("set_has__int"));
    assert!(ir.contains("set_remove__int"));
}

#[test]
fn test_set_size_and_values() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20 let s: Set<string> = set_new();\n\
         \x20 set_add(s, \"hello\");\n\
         \x20 let sz: int = set_size(s);\n\
         \x20 let vals: [string] = set_values(s);\n\
         \x20 return sz;\n\
         }\n",
    );
}

#[test]
fn test_set_float_not_hashable() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20 let s: Set<float> = set_new();\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("hashable"));
}

#[test]
fn test_set_wrong_element_type() {
    let result = yorum::typecheck(
        "fn main() -> int {\n\
         \x20 let s: Set<int> = set_new();\n\
         \x20 set_add(s, \"not_int\");\n\
         \x20 return 0;\n\
         }\n",
    );
    assert!(result.is_err());
}

#[test]
fn test_set_string_compiles() {
    let ir = compile(
        "fn main() -> int {\
           let s: Set<string> = set_new();\
           set_add(s, \"hello\");\
           set_add(s, \"world\");\
           let sz: int = set_size(s);\
           return sz;\
         }",
    );
    assert!(ir.contains("set_new__string"));
    assert!(ir.contains("set_add__string"));
    assert!(ir.contains("set_size__string"));
}

// ── ? operator tests ─────────────────────────────────────

#[test]
fn test_try_operator_option_in_option_fn() {
    let ir = compile(
        "fn get_val() -> Option<int> {\
           return Some(42);\
         }\
         fn try_it() -> Option<int> {\
           let v: int = get_val()?;\
           return Some(v + 1);\
         }\
         fn main() -> int {\
           let r: Option<int> = try_it();\
           return 0;\
         }",
    );
    assert!(ir.contains("try.ok"));
    assert!(ir.contains("try.err"));
}

#[test]
fn test_try_operator_result_in_result_fn() {
    let ir = compile(
        "fn parse_num() -> Result<int, string> {\
           return Ok(10);\
         }\
         fn compute() -> Result<int, string> {\
           let n: int = parse_num()?;\
           return Ok(n * 2);\
         }\
         fn main() -> int {\
           let r: Result<int, string> = compute();\
           return 0;\
         }",
    );
    assert!(ir.contains("try.ok"));
    assert!(ir.contains("try.err"));
}

#[test]
fn test_try_operator_chaining() {
    // Multiple ? operators in one function
    let ir = compile(
        "fn a() -> Option<int> {\
           return Some(1);\
         }\
         fn b() -> Option<int> {\
           return Some(2);\
         }\
         fn chain() -> Option<int> {\
           let x: int = a()?;\
           let y: int = b()?;\
           return Some(x + y);\
         }\
         fn main() -> int {\
           let r: Option<int> = chain();\
           return 0;\
         }",
    );
    // Should have two try.ok/try.err pairs
    let ok_count = ir.matches("try.ok").count();
    assert!(
        ok_count >= 2,
        "expected at least 2 try.ok labels, got {}",
        ok_count
    );
}

#[test]
fn test_try_operator_error_not_option_or_result() {
    let result = yorum::typecheck(
        "fn foo() -> int {\
           let x: int = 42;\
           let y: int = x?;\
           return y;\
         }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("? operator") || err.contains("Option") || err.contains("Result"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn test_try_operator_error_wrong_return_type() {
    // ? on Option inside a function returning int (not Option)
    let result = yorum::typecheck(
        "fn get_opt() -> Option<int> {\
           return Some(1);\
         }\
         fn bad() -> int {\
           let v: int = get_opt()?;\
           return v;\
         }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("return") || err.contains("Option") || err.contains("?"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn test_try_operator_result_e_type_mismatch() {
    // ? on Result<int, string> inside fn returning Result<int, int> — E types differ
    let result = yorum::typecheck(
        "fn get_res() -> Result<int, string> {\
           return Ok(1);\
         }\
         fn bad() -> Result<int, int> {\
           let v: int = get_res()?;\
           return Ok(v);\
         }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("mismatch") || err.contains("error type") || err.contains("Result"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn test_try_operator_none_propagation() {
    // Verify the IR for Option ? has proper None construction and ret
    let ir = compile(
        "fn maybe() -> Option<int> {\
           return None;\
         }\
         fn use_it() -> Option<int> {\
           let v: int = maybe()?;\
           return Some(v);\
         }\
         fn main() -> int {\
           let r: Option<int> = use_it();\
           return 0;\
         }",
    );
    // The error path should store tag 1 (None) and ret
    assert!(ir.contains("store i32 1"));
    assert!(ir.contains("try.err"));
}

// ── Bug fix regression tests (v1.3) ─────────────────────

#[test]
fn test_map_generic_format_string_length() {
    // Bug: @.fmt.map_key_generic was declared as [22 x i8] but the string
    // "map_get: key not found\0" is 23 bytes. Caused LLVM validation failure.
    let ir = compile(
        "fn main() -> int {\
           let m: Map<int, string> = map_new();\
           map_set(m, 1, \"hello\");\
           let v: string = map_get(m, 1);\
           return 0;\
         }",
    );
    assert!(ir.contains("[23 x i8]"));
    assert!(!ir.contains("[22 x i8] c\"map_get: key not found"));
}

#[test]
fn test_map_set_named_type_is_ptr() {
    // Bug: Type::Named("Map__string__int") fell to i64 fallback in llvm_type()
    // because it wasn't in struct_layouts or enum_layouts. Should be ptr.
    let ir = compile(
        "fn main() -> int {\
           let m: Map<string, int> = map_new();\
           map_set(m, \"key\", 42);\
           return 0;\
         }",
    );
    // The map variable alloca should be ptr (not i64)
    // map_new returns ptr, store into alloca should be: store ptr %tN, ptr %m.addr
    assert!(
        ir.contains("store ptr %"),
        "map variable should be stored as ptr, not i64"
    );
}

#[test]
fn test_set_named_type_is_ptr() {
    // Same bug as above but for Set
    let ir = compile(
        "fn main() -> int {\
           let s: Set<int> = set_new();\
           set_add(s, 42);\
           return 0;\
         }",
    );
    assert!(ir.contains("alloca ptr"));
}

#[test]
fn test_collection_calls_in_if_block() {
    // Bug: map/set builtin calls in nested blocks (if/while/for/match) were
    // not rewritten to mangled names because var_types wasn't propagated.
    let ir = compile(
        "fn main() -> int {\
           let s: Set<int> = set_new();\
           set_add(s, 1);\
           if true {\
             set_add(s, 2);\
             let sz: int = set_size(s);\
           }\
           return 0;\
         }",
    );
    // All set calls should be mangled, no unmangled set_add/set_size remaining
    assert!(!ir.contains("call ptr @set_add(") && !ir.contains("call i64 @set_size("));
    assert!(ir.contains("set_add__int"));
    assert!(ir.contains("set_size__int"));
}

#[test]
fn test_collection_calls_in_while_block() {
    let ir = compile(
        "fn main() -> int {\
           let m: Map<int, string> = map_new();\
           let mut i: int = 0;\
           while i < 3 {\
             map_set(m, i, \"val\");\
             i += 1;\
           }\
           return map_size(m);\
         }",
    );
    assert!(ir.contains("map_set__int__string"));
    assert!(ir.contains("map_size__int__string"));
}

#[test]
fn test_collection_calls_in_for_block() {
    let ir = compile(
        "fn main() -> int {\
           let s: Set<int> = set_new();\
           for i in 0..5 {\
             set_add(s, i);\
           }\
           return set_size(s);\
         }",
    );
    assert!(ir.contains("set_add__int"));
    assert!(ir.contains("set_size__int"));
}

#[test]
fn test_collection_calls_in_match_block() {
    let ir = compile(
        "fn main() -> int {\
           let s: Set<int> = set_new();\
           let x: int = 1;\
           match x {\
             1 => { set_add(s, 10); },\
             _ => { set_add(s, 20); },\
           }\
           return set_size(s);\
         }",
    );
    assert!(ir.contains("set_add__int"));
    assert!(ir.contains("set_size__int"));
}

// ═══════════════════════════════════════════════════════════════
//  v1.4 — Effect system enforcement
// ═══════════════════════════════════════════════════════════════

// ── Validation tests ──

#[test]
fn test_effects_unknown_effect_name_rejected() {
    let result = yorum::typecheck("fn foo() -> unit effects badeffect { print_str(\"hi\"); }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("unknown effect 'badeffect'"));
}

#[test]
fn test_effects_pure_fn_with_effects_clause_rejected() {
    let result = yorum::typecheck("pure fn foo() -> int effects io { return 1; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("pure function cannot have an 'effects' clause"));
}

#[test]
fn test_effects_valid_effect_names_accepted() {
    parse_and_check(
        "fn foo() -> unit effects io, fs, net, time, env, concurrency { }\n\
         fn main() -> int { foo(); return 0; }",
    );
}

// ── Per-effect category tests ──

#[test]
fn test_effects_io_allowed_when_declared() {
    parse_and_check(
        "fn foo() -> unit effects io { print_str(\"hello\"); }\n\
         fn main() -> int { foo(); return 0; }",
    );
}

#[test]
fn test_effects_io_rejected_when_missing() {
    let result = yorum::typecheck(
        "fn foo() -> unit effects fs { print_str(\"hello\"); }\n\
         fn main() -> int { foo(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'io' effect"));
    assert!(err.contains("print_str"));
}

#[test]
fn test_effects_fs_allowed_when_declared() {
    parse_and_check(
        "fn foo() -> unit effects fs { let x0: bool = file_exists(\"test.txt\"); }\n\
         fn main() -> int { foo(); return 0; }",
    );
}

#[test]
fn test_effects_fs_rejected_when_missing() {
    let result = yorum::typecheck(
        "fn foo() -> unit effects io { let s0: string = file_read(\"test.txt\"); }\n\
         fn main() -> int { foo(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'fs' effect"));
    assert!(err.contains("file_read"));
}

#[test]
fn test_effects_net_allowed_when_declared() {
    parse_and_check(
        "fn foo() -> unit effects net { let s0: string = dns_resolve(\"example.com\"); }\n\
         fn main() -> int { foo(); return 0; }",
    );
}

#[test]
fn test_effects_net_rejected_when_missing() {
    let result = yorum::typecheck(
        "fn foo() -> unit effects io { let s0: string = http_get(\"http://example.com\"); }\n\
         fn main() -> int { foo(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'net' effect"));
    assert!(err.contains("http_get"));
}

#[test]
fn test_effects_time_allowed_when_declared() {
    parse_and_check(
        "fn foo() -> int effects time { return time_ms(); }\n\
         fn main() -> int { return foo(); }",
    );
}

#[test]
fn test_effects_time_rejected_when_missing() {
    let result = yorum::typecheck(
        "fn foo() -> int effects io { return time_ms(); }\n\
         fn main() -> int { return foo(); }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'time' effect"));
    assert!(err.contains("time_ms"));
}

#[test]
fn test_effects_env_allowed_when_declared() {
    parse_and_check(
        "fn foo() -> unit effects env { let s0: string = env_get(\"PATH\"); }\n\
         fn main() -> int { foo(); return 0; }",
    );
}

#[test]
fn test_effects_env_rejected_when_missing() {
    let result = yorum::typecheck(
        "fn foo() -> unit effects io { exit(1); }\n\
         fn main() -> int { foo(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'env' effect"));
    assert!(err.contains("exit"));
}

// ── Multiple effects ──

#[test]
fn test_effects_multiple_effects_allowed() {
    parse_and_check(
        "fn foo() -> unit effects io, fs {\n\
         \x20   print_str(\"reading file\");\n\
         \x20   let s0: string = file_read(\"data.txt\");\n\
         }\n\
         fn main() -> int { foo(); return 0; }",
    );
}

#[test]
fn test_effects_multiple_effects_partial_missing() {
    let result = yorum::typecheck(
        "fn foo() -> unit effects io {\n\
         \x20   print_str(\"reading file\");\n\
         \x20   let s0: string = file_read(\"data.txt\");\n\
         }\n\
         fn main() -> int { foo(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'fs' effect"));
}

// ── Propagation tests ──

#[test]
fn test_effects_callee_effects_must_be_subset() {
    let result = yorum::typecheck(
        "fn printer() -> unit effects io { print_str(\"hi\"); }\n\
         fn wrapper() -> unit effects fs { printer(); }\n\
         fn main() -> int { wrapper(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'io' effect"));
    assert!(err.contains("printer"));
}

#[test]
fn test_effects_superset_is_ok() {
    parse_and_check(
        "fn printer() -> unit effects io { print_str(\"hi\"); }\n\
         fn wrapper() -> unit effects io, fs { printer(); }\n\
         fn main() -> int { wrapper(); return 0; }",
    );
}

#[test]
fn test_effects_transitive_propagation_error() {
    let result = yorum::typecheck(
        "fn b() -> unit effects io { print_str(\"b\"); }\n\
         fn a() -> unit effects fs { b(); }\n\
         fn main() -> int { a(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'io' effect"));
}

// ── Inference tests ──

#[test]
fn test_effects_inference_from_builtin() {
    parse_and_check(
        "fn unchecked_printer() -> unit { print_str(\"hello\"); }\n\
         fn caller() -> unit effects io { unchecked_printer(); }\n\
         fn main() -> int { caller(); return 0; }",
    );
}

#[test]
fn test_effects_inference_rejects_missing() {
    let result = yorum::typecheck(
        "fn unchecked_printer() -> unit { print_str(\"hello\"); }\n\
         fn caller() -> unit effects fs { unchecked_printer(); }\n\
         fn main() -> int { caller(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'io' effect"));
}

#[test]
fn test_effects_transitive_inference() {
    parse_and_check(
        "fn b() -> unit { print_str(\"b\"); }\n\
         fn a() -> unit { b(); }\n\
         fn caller() -> unit effects io { a(); }\n\
         fn main() -> int { caller(); return 0; }",
    );
}

#[test]
fn test_effects_transitive_inference_rejects() {
    let result = yorum::typecheck(
        "fn b() -> unit { print_str(\"b\"); }\n\
         fn a() -> unit { b(); }\n\
         fn caller() -> unit effects fs { a(); }\n\
         fn main() -> int { caller(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'io' effect"));
}

// ── Backward compatibility tests ──

#[test]
fn test_effects_unchecked_fn_compiles_freely() {
    parse_and_check(
        "fn do_everything() -> unit {\n\
         \x20   print_str(\"hello\");\n\
         \x20   let s0: string = env_get(\"PATH\");\n\
         }\n\
         fn main() -> int { do_everything(); return 0; }",
    );
}

#[test]
fn test_effects_main_always_unchecked() {
    parse_and_check(
        "fn main() -> int {\n\
         \x20   print_str(\"hello\");\n\
         \x20   let s0: string = env_get(\"HOME\");\n\
         \x20   return 0;\n\
         }",
    );
}

#[test]
fn test_effects_memory_ops_need_no_effect() {
    parse_and_check(
        "fn foo() -> int effects io {\n\
         \x20   let mut arr: [int] = [1, 2, 3];\n\
         \x20   push(arr, 4);\n\
         \x20   let n0: int = abs_int(-5);\n\
         \x20   let s0: string = str_concat(\"a\", \"b\");\n\
         \x20   print_int(len(arr));\n\
         \x20   return 0;\n\
         }\n\
         fn main() -> int { return foo(); }",
    );
}

#[test]
fn test_effects_empty_effects_clause() {
    parse_and_check(
        "fn pure_ish() -> int effects {\n\
         \x20   let x: int = abs_int(-5);\n\
         \x20   return x;\n\
         }\n\
         fn main() -> int { return pure_ish(); }",
    );
}

#[test]
fn test_effects_empty_effects_rejects_io() {
    let result = yorum::typecheck(
        "fn pure_ish() -> unit effects { print_str(\"hello\"); }\n\
         fn main() -> int { pure_ish(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'io' effect"));
}

// ── Closure tests ──

#[test]
fn test_effects_closure_inherits_enclosing_effects() {
    parse_and_check(
        "fn foo() -> unit effects io {\n\
         \x20   let f: fn() -> unit = || -> unit { print_str(\"hello\"); };\n\
         \x20   f();\n\
         }\n\
         fn main() -> int { foo(); return 0; }",
    );
}

#[test]
fn test_effects_closure_rejects_missing_effect() {
    let result = yorum::typecheck(
        "fn foo() -> unit effects fs {\n\
         \x20   let f: fn() -> unit = || -> unit { print_str(\"hello\"); };\n\
         \x20   f();\n\
         }\n\
         fn main() -> int { foo(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'io' effect"));
}

// ── Concurrency effect tests ──

#[test]
fn test_effects_concurrency_allowed() {
    let ir = compile(
        "fn worker() -> unit effects concurrency {\n\
         \x20   let ch: Chan<int> = chan();\n\
         \x20   send(ch, 42);\n\
         }\n\
         fn main() -> int { worker(); return 0; }",
    );
    assert!(ir.contains("define void @worker"));
}

#[test]
fn test_effects_concurrency_rejected_when_missing() {
    let result = yorum::typecheck(
        "fn worker() -> unit effects io {\n\
         \x20   let ch: Chan<int> = chan();\n\
         \x20   send(ch, 42);\n\
         }\n\
         fn main() -> int { worker(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'concurrency' effect"));
}

#[test]
fn test_effects_spawn_requires_concurrency() {
    let result = yorum::typecheck(
        "fn worker() -> unit effects io {\n\
         \x20   let t: Task<int> = spawn { return 42; };\n\
         \x20   let r0: int = t.join();\n\
         }\n\
         fn main() -> int { worker(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'concurrency' effect"));
}

// ── args() effect test ──

#[test]
fn test_effects_args_requires_env() {
    let result = yorum::typecheck(
        "fn foo() -> unit effects io {\n\
         \x20   let a0: [string] = args();\n\
         }\n\
         fn main() -> int { foo(); return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("'env' effect"));
    assert!(err.contains("args"));
}

// ── Compile-time only (no IR changes) ──

#[test]
fn test_effects_compiles_to_same_ir() {
    let ir = compile(
        "fn greet() -> unit effects io { print_str(\"hello\"); }\n\
         fn main() -> int { greet(); return 0; }",
    );
    assert!(ir.contains("define void @greet()"));
    assert!(ir.contains("call void @print_str"));
    assert!(!ir.contains("effect"));
}

// ── Bug fix regression tests ──

#[test]
fn test_effects_method_call_enforces_effects() {
    // Bug 1: method calls bypassed effect enforcement
    let result = yorum::typecheck(
        "struct Printer { id: int }\n\
         impl Printer {\n\
         \x20   fn print(self: &Printer) -> unit effects io { print_str(\"hello\"); }\n\
         }\n\
         fn caller() -> unit effects fs {\n\
         \x20   let p: Printer = Printer { id: 1 };\n\
         \x20   p.print();\n\
         }\n\
         fn main() -> int { caller(); return 0; }",
    );
    assert!(result.is_err(), "expected error but got Ok: {:?}", result);
    let err = result.unwrap_err();
    assert!(err.contains("'io' effect"), "got: {}", err);
}

#[test]
fn test_effects_method_call_allowed_when_declared() {
    parse_and_check(
        "struct Printer { id: int }\n\
         impl Printer {\n\
         \x20   fn print(self: &Printer) -> unit effects io { print_str(\"hello\"); }\n\
         }\n\
         fn caller() -> unit effects io {\n\
         \x20   let p: Printer = Printer { id: 1 };\n\
         \x20   p.print();\n\
         }\n\
         fn main() -> int { caller(); return 0; }",
    );
}

#[test]
fn test_effects_method_call_purity_enforced() {
    // Bug 1 also: pure function calling impure method was allowed
    let result = yorum::typecheck(
        "struct Svc { id: int }\n\
         impl Svc {\n\
         \x20   fn do_io(self: &Svc) -> unit { print_str(\"hello\"); }\n\
         }\n\
         pure fn bad(s: &Svc) -> unit {\n\
         \x20   s.do_io();\n\
         }\n\
         fn main() -> int { return 0; }",
    );
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("pure function cannot call impure method"));
}

#[test]
fn test_effects_inference_includes_method_calls() {
    // Bug 2: unchecked wrapper calling io method was inferred as effect-free
    let result = yorum::typecheck(
        "struct Logger { id: int }\n\
         impl Logger {\n\
         \x20   fn log(self: &Logger) -> unit effects io { print_str(\"log\"); }\n\
         }\n\
         fn unchecked_wrapper(l: &Logger) -> unit { l.log(); }\n\
         fn caller() -> unit effects fs {\n\
         \x20   let lg: Logger = Logger { id: 1 };\n\
         \x20   unchecked_wrapper(lg);\n\
         }\n\
         fn main() -> int { caller(); return 0; }",
    );
    assert!(result.is_err(), "expected error but got Ok: {:?}", result);
    let err = result.unwrap_err();
    assert!(err.contains("'io' effect"), "got: {}", err);
}

#[test]
fn test_effects_main_unknown_effect_rejected() {
    // Bug 3: unknown effect names on main were silently accepted
    let result = yorum::typecheck("fn main() -> int effects madeup { return 0; }");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("unknown effect 'madeup'"));
}

// ═══════════════════════════════════════════════════════════════
//  v1.5 — Tooling & Developer Experience
// ═══════════════════════════════════════════════════════════════

// ── yorum run ────────────────────────────────────────────────

#[test]
fn test_yorum_run_hello() {
    // Skip on Windows — the IR declares networking/pthread symbols that require
    // platform-specific libraries (ws2_32, pthreads) not available in CI.
    if cfg!(target_os = "windows") {
        eprintln!("skipping yorum run test on Windows");
        return;
    }

    let status = std::process::Command::new("cargo")
        .args(["run", "--", "run", "examples/hello.yrm"])
        .status();
    match status {
        Ok(s) => assert!(s.success(), "yorum run examples/hello.yrm failed"),
        Err(e) => {
            eprintln!("skipping yorum run test: {}", e);
        }
    }
}

// ── LSP completions helpers ──────────────────────────────────

#[test]
fn test_lsp_extract_prefix() {
    use yorum::lsp::server::extract_prefix;
    assert_eq!(extract_prefix("let x = pri"), "pri");
    assert_eq!(extract_prefix("print_"), "print_");
    assert_eq!(extract_prefix("  fn "), "");
    assert_eq!(extract_prefix("hello"), "hello");
}

#[test]
fn test_lsp_extract_last_ident() {
    use yorum::lsp::server::extract_last_ident;
    assert_eq!(extract_last_ident("point."), "point");
    assert_eq!(extract_last_ident("self.x."), "x");
}

#[test]
fn test_lsp_levenshtein() {
    use yorum::lsp::server::levenshtein_distance;
    assert_eq!(levenshtein_distance("print_int", "print_int"), 0);
    assert_eq!(levenshtein_distance("prnt_int", "print_int"), 1);
    assert_eq!(levenshtein_distance("kitten", "sitting"), 3);
    assert_eq!(levenshtein_distance("", "abc"), 3);
}

#[test]
fn test_lsp_extract_quoted_name() {
    use yorum::lsp::server::extract_quoted_name;
    assert_eq!(
        extract_quoted_name("undefined variable 'foo'"),
        Some("foo".to_string())
    );
    assert_eq!(
        extract_quoted_name("undefined function 'bar'"),
        Some("bar".to_string())
    );
    assert_eq!(extract_quoted_name("no quotes here"), None);
}

#[test]
fn test_lsp_find_closest_name() {
    use yorum::lsp::server::find_closest_name;
    let candidates: Vec<String> = vec!["print_int", "print_str", "read_line"]
        .into_iter()
        .map(String::from)
        .collect();
    assert_eq!(
        find_closest_name("prnt_int", &candidates),
        Some("print_int".to_string())
    );
    assert_eq!(find_closest_name("xyz_abc", &candidates), None);
}

// ── LSP completions ─────────────────────────────────────────

#[test]
fn test_lsp_builtin_function_list() {
    let builtins = yorum::builtin_function_list();
    assert!(!builtins.is_empty());
    let names: Vec<&str> = builtins.iter().map(|(n, _)| n.as_str()).collect();
    assert!(names.contains(&"print_int"));
    assert!(names.contains(&"str_len"));
}

// ── Debug info ──────────────────────────────────────────────

#[test]
fn test_debug_info_present() {
    let source = "fn main() -> int {\n  print_int(42);\n  return 0;\n}\n";
    let ir = yorum::compile_to_ir_with_options(source, "test.yrm", true).unwrap();
    assert!(ir.contains("!llvm.dbg.cu"), "missing !llvm.dbg.cu");
    assert!(ir.contains("!DICompileUnit"), "missing !DICompileUnit");
    assert!(ir.contains("!DISubprogram"), "missing !DISubprogram");
    assert!(ir.contains("!DIFile"), "missing !DIFile");
    assert!(
        ir.contains("test.yrm"),
        "filename not found in debug metadata"
    );
}

#[test]
fn test_debug_info_absent() {
    let ir = compile("fn main() -> int { return 0; }\n");
    assert!(
        !ir.contains("!DICompileUnit"),
        "debug info present when not requested"
    );
}

#[test]
fn test_debug_info_with_options_false() {
    let source = "fn main() -> int { return 0; }\n";
    let ir = yorum::compile_to_ir_with_options(source, "test.yrm", false).unwrap();
    assert!(
        !ir.contains("!DICompileUnit"),
        "debug info present when debug=false"
    );
}

#[test]
fn test_debug_info_multiple_functions() {
    let source = "fn helper(x: int) -> int { return x + 1; }\n\
                  fn main() -> int { print_int(helper(41)); return 0; }\n";
    let ir = yorum::compile_to_ir_with_options(source, "multi.yrm", true).unwrap();
    // Should have two DISubprogram entries (helper + main)
    let sp_count = ir.matches("!DISubprogram").count();
    assert!(
        sp_count >= 2,
        "expected >= 2 DISubprogram entries, got {}",
        sp_count
    );
}

// ── REPL helpers ────────────────────────────────────────────

#[test]
fn test_compile_to_ir_with_options_basic() {
    let source = "fn main() -> int { return 42; }\n";
    let ir = yorum::compile_to_ir_with_options(source, "basic.yrm", false).unwrap();
    assert!(ir.contains("define i64 @main"));
}

// ═══════════════════════════════════════════════════════════════
//  Formatter tests
// ═══════════════════════════════════════════════════════════════

fn format(source: &str) -> String {
    yorum::format_source(source).expect("format failed")
}

#[test]
fn test_fmt_basic_fn() {
    let input = "fn main()->int{return 0;}";
    let output = format(input);
    assert!(output.contains("fn main() -> int {"));
    assert!(output.contains("    return 0;"));
    assert!(output.contains("}"));
}

#[test]
fn test_fmt_contracts_brace_on_own_line() {
    let input = "pure fn fib(n: int) -> int requires n >= 0 ensures result >= 0 { return n; }\n";
    let output = format(input);
    // Contracts should be indented, brace on its own line
    assert!(output.contains("    requires n >= 0"));
    assert!(output.contains("    ensures result >= 0"));
    // The { should be on its own line (not on same line as ensures)
    let lines: Vec<&str> = output.lines().collect();
    let brace_line = lines.iter().find(|l| l.trim() == "{");
    assert!(
        brace_line.is_some(),
        "expected '{{' on its own line after contracts"
    );
}

#[test]
fn test_fmt_compound_assign() {
    let input = "fn main() -> int { let mut x: int = 0; x += 1; x -= 2; x *= 3; x /= 4; x %= 5; return x; }\n";
    let output = format(input);
    assert!(output.contains("x += 1;"));
    assert!(output.contains("x -= 2;"));
    assert!(output.contains("x *= 3;"));
    assert!(output.contains("x /= 4;"));
    assert!(output.contains("x %= 5;"));
}

#[test]
fn test_fmt_interp_string() {
    let input = "fn main() -> int { let name: string = \"Alice\"; print_str(\"hello {name}\"); return 0; }\n";
    let output = format(input);
    assert!(
        output.contains("\"hello {name}\""),
        "interpolation should be preserved, got: {}",
        output
    );
}

#[test]
fn test_fmt_comment_preserved() {
    let input = "// greeting\nfn main() -> int { return 0; }\n";
    let output = format(input);
    assert!(output.contains("// greeting"));
}

#[test]
fn test_fmt_block_comment_preserved() {
    let input = "/* block comment */\nfn main() -> int { return 0; }\n";
    let output = format(input);
    assert!(output.contains("/* block comment */"));
}

#[test]
fn test_fmt_already_formatted() {
    let input = "fn main() -> int {\n    return 0;\n}\n";
    let output = format(input);
    assert_eq!(output, input);
}

#[test]
fn test_fmt_enum() {
    let input = "enum Dir { North, South, East, West }\nfn main() -> int { return 0; }\n";
    let output = format(input);
    assert!(output.contains("    North,"));
    assert!(output.contains("    South,"));
    assert!(output.contains("    East,"));
    // Last variant has no trailing comma
    assert!(output.contains("    West\n"));
}

#[test]
fn test_fmt_match() {
    let input = "fn main() -> int { match 1 { 1 => { return 1; }, _ => { return 0; }, } }\n";
    let output = format(input);
    assert!(output.contains("1 => {"));
    assert!(output.contains("_ => {"));
}

#[test]
fn test_fmt_blank_lines_between_decls() {
    let input = "fn foo() -> int { return 1; }\nfn bar() -> int { return 2; }\n";
    let output = format(input);
    // Should have exactly one blank line between declarations
    assert!(output.contains("}\n\nfn bar"));
}

#[test]
fn test_fmt_effects() {
    let input = "fn display(x: int) -> unit effects io { print_int(x); }\n";
    let output = format(input);
    assert!(output.contains("    effects io"));
    // Brace on own line after effects
    let lines: Vec<&str> = output.lines().collect();
    let brace_line = lines.iter().find(|l| l.trim() == "{");
    assert!(brace_line.is_some());
}

#[test]
fn test_fmt_idempotent() {
    // Various inputs: format twice should give same result
    let inputs = vec![
        "fn main() -> int { let x: int = 1 + 2 * 3; return x; }\n",
        "// comment\nfn main() -> int { return 0; }\n",
        "struct Point { x: int, y: int }\nfn main() -> int { return 0; }\n",
        "enum Color { Red, Green, Blue }\nfn main() -> int { return 0; }\n",
        "fn main() -> int { let mut x: int = 0; x += 1; return x; }\n",
        "fn foo(n: int) -> int requires n > 0 { return n; }\nfn main() -> int { return 0; }\n",
    ];
    for input in inputs {
        let first = format(input);
        let second = format(&first);
        assert_eq!(first, second, "not idempotent for input:\n{}", input);
    }
}

#[test]
fn test_fmt_struct() {
    let input = "struct Point{x:int,y:int}\nfn main() -> int { return 0; }\n";
    let output = format(input);
    assert!(output.contains("struct Point { x: int, y: int }"));
}

#[test]
fn test_fmt_if_else() {
    let input = "fn main() -> int { if true { return 1; } else { return 0; } }\n";
    let output = format(input);
    // K&R style: } else {
    assert!(output.contains("} else {"));
}

#[test]
fn test_fmt_for_loop() {
    let input = "fn main() -> int { for i in 0..10 { print_int(i); } return 0; }\n";
    let output = format(input);
    assert!(output.contains("for i in 0..10 {"));
}

#[test]
fn test_fmt_compile_after_format() {
    // Format then compile — formatted output should compile
    let input = "fn main() -> int { let x: int = 42; print_int(x); return 0; }\n";
    let formatted = format(input);
    yorum::compile_to_ir(&formatted).expect("formatted code should compile");
}

#[test]
fn test_fmt_closure() {
    let input = "fn main() -> int { let f: fn(int) -> int = |x: int| -> int { return x * 2; }; return f(21); }\n";
    let output = format(input);
    assert!(output.contains("|x: int| -> int {"));
}

#[test]
fn test_fmt_trait() {
    let input = "trait Foo { fn bar(self: &Self) -> int; }\nfn main() -> int { return 0; }\n";
    let output = format(input);
    assert!(output.contains("trait Foo {"));
    assert!(output.contains("    fn bar(self: &Self) -> int;"));
}

#[test]
fn test_fmt_impl() {
    let input = "struct S { x: int }\nimpl S { fn get(self: &S) -> int { return self.x; } }\nfn main() -> int { return 0; }\n";
    let output = format(input);
    assert!(output.contains("impl S {"));
    assert!(output.contains("    fn get(self: &S) -> int {"));
}

#[test]
fn test_fmt_escaped_braces_in_string() {
    // String with literal { and } should round-trip correctly
    let input = "fn main() -> int { print_str(\"Use {{expr}} for interpolation\"); return 0; }\n";
    let output = format(input);
    assert!(
        output.contains("\"Use {{expr}} for interpolation\""),
        "escaped braces should be preserved, got: {}",
        output
    );
    // Formatted code should compile
    yorum::compile_to_ir(&output).expect("formatted code with escaped braces should compile");
}

#[test]
fn test_fmt_trait_method_unit_return() {
    // Trait methods returning unit must keep `-> unit`
    let input = "trait Logger { fn log(self: &Self) -> unit; }\nfn main() -> int { return 0; }\n";
    let output = format(input);
    assert!(
        output.contains("-> unit;"),
        "trait method should preserve -> unit, got: {}",
        output
    );
    // Formatted output must reparse
    let reparsed = format(&output);
    assert_eq!(output, reparsed, "trait method unit return not idempotent");
}

#[test]
fn test_fmt_singleton_tuple_literal() {
    // (x,) must keep trailing comma so it doesn't become a grouped expression
    let input = "fn main() -> int { let t: (int,) = (42,); return t.0; }\n";
    let output = format(input);
    assert!(
        output.contains("(42,)"),
        "singleton tuple literal should have trailing comma, got: {}",
        output
    );
    yorum::compile_to_ir(&output).expect("formatted singleton tuple should compile");
}

#[test]
fn test_fmt_singleton_tuple_type() {
    // (int,) must keep trailing comma in type position
    let input = "fn main() -> int { let t: (int,) = (42,); return t.0; }\n";
    let output = format(input);
    assert!(
        output.contains(": (int,)"),
        "singleton tuple type should have trailing comma, got: {}",
        output
    );
}

#[test]
fn test_fmt_impl_method_comments() {
    // Comments above methods in impl blocks should stay attached
    let input = "\
struct S { x: int }

impl S {
    // returns x
    fn get(self: &S) -> int {
        return self.x;
    }
}

fn main() -> int { return 0; }
";
    let output = format(input);
    // Comment must appear inside the impl, before the method
    let impl_start = output.find("impl S {").unwrap();
    let comment_pos = output.find("// returns x").unwrap();
    let method_pos = output.find("fn get(self: &S)").unwrap();
    assert!(
        comment_pos > impl_start && comment_pos < method_pos,
        "comment should be inside impl and before method, got:\n{}",
        output
    );
}

#[test]
fn test_fmt_block_end_comment() {
    // Comments before closing brace should stay inside the block
    let input = "\
fn main() -> int {
    let x: int = 42;
    // end of function
}
";
    let output = format(input);
    let brace_pos = output.rfind('}').unwrap();
    let comment_pos = output.find("// end of function").unwrap();
    assert!(
        comment_pos < brace_pos,
        "block-end comment should stay inside the function, got:\n{}",
        output
    );
}

#[test]
fn test_fmt_trailing_line_comment() {
    let input = "\
fn main() -> int {
    let x: int = 15;   // fifteen
    return x;
}
";
    let output = format(input);
    assert!(
        output.contains("let x: int = 15;  // fifteen"),
        "trailing line comment should stay on same line, got:\n{}",
        output
    );
}

#[test]
fn test_fmt_trailing_block_comment() {
    let input = "\
fn main() -> int {
    let x: int = 15;   /* fifteen */
    return x;
}
";
    let output = format(input);
    assert!(
        output.contains("let x: int = 15;  /* fifteen */"),
        "trailing block comment should stay on same line, got:\n{}",
        output
    );
}

#[test]
fn test_fmt_trailing_then_leading_comment() {
    let input = "\
fn main() -> int {
    let x: int = 15;   // fifteen
    // next stmt
    return x;
}
";
    let output = format(input);
    assert!(
        output.contains("let x: int = 15;  // fifteen"),
        "trailing comment should stay inline, got:\n{}",
        output
    );
    assert!(
        output.contains("    // next stmt\n    return x;"),
        "leading comment should stay on its own line, got:\n{}",
        output
    );
}

#[test]
fn test_fmt_trailing_comment_idempotent() {
    let input = "\
fn main() -> int {
    let x: int = 15;  // fifteen
    return x;
}
";
    let first = format(input);
    let second = format(&first);
    assert_eq!(
        first, second,
        "trailing comment format should be idempotent"
    );
}

#[test]
fn test_fmt_trailing_comment_blank_line() {
    let input = "\
fn main() -> int {
    let x: int = 15;   // fifteen

    return x;
}
";
    let output = format(input);
    assert!(
        output.contains("let x: int = 15;  // fifteen"),
        "trailing comment should stay inline, got:\n{}",
        output
    );
    assert!(
        output.contains("// fifteen\n\n    return"),
        "blank line after stmt with trailing comment should be preserved, got:\n{}",
        output
    );
}

#[test]
fn test_fmt_multiline_block_not_trailing() {
    let input = "\
fn main() -> int {
    let x: int = 15; /* multi
line */
    return x;
}
";
    let output = format(input);
    // Multi-line block comment should NOT be inlined as trailing
    assert!(
        !output.contains("let x: int = 15;  /* multi"),
        "multi-line block comment should not be trailing, got:\n{}",
        output
    );
}

#[test]
fn test_fmt_trailing_comment_struct_field() {
    let input = "\
struct Config {
    width: int,  // pixels
    height: int,  // pixels
    name: string,
    debug: bool
}

fn main() -> unit {}
";
    let output = format(input);
    assert!(
        output.contains("width: int,  // pixels"),
        "struct field trailing comment should be preserved, got:\n{}",
        output
    );
    assert!(
        output.contains("height: int,  // pixels"),
        "struct field trailing comment should be preserved, got:\n{}",
        output
    );
}

#[test]
fn test_fmt_trailing_comment_enum_variant() {
    let input = "\
enum Color {
    Red,   // primary
    Green,   // primary
    Blue
}

fn main() -> unit {}
";
    let output = format(input);
    assert!(
        output.contains("Red,  // primary"),
        "enum variant trailing comment should be preserved, got:\n{}",
        output
    );
    assert!(
        output.contains("Green,  // primary"),
        "enum variant trailing comment should be preserved, got:\n{}",
        output
    );
}

#[test]
fn test_fmt_trailing_comment_not_pulled_into_compact_fn() {
    // Comment after `}` must attach to the function, not the inner `return 1;`
    let input = "\
fn a() -> int { return 1; } // note

fn main() -> int { return 0; }
";
    let output = format(input);
    assert!(
        !output.contains("return 1;  // note"),
        "comment after }} must not attach to inner statement, got:\n{}",
        output
    );
    assert!(
        output.contains("}  // note"),
        "comment should attach to the closing brace line, got:\n{}",
        output
    );
}

#[test]
fn test_fmt_trailing_comment_not_pulled_into_compact_enum() {
    // Comment after `}` must attach to the enum, not variant A
    let input = "\
enum E { A, B } // note

fn main() -> unit {}
";
    let output = format(input);
    assert!(
        !output.contains("A,  // note") && !output.contains("B  // note"),
        "comment after }} must not attach to inner variant, got:\n{}",
        output
    );
}

// ═══════════════════════════════════════════════════════════════
//  v1.7 — Performance & optimization tests
// ═══════════════════════════════════════════════════════════════

// ── Inline hint annotations ─────────────────────────────────

#[test]
fn test_inline_hint_pure_small_fn() {
    let ir = compile(
        "pure fn double(x: int) -> int { return x * 2; }\n\
         fn main() -> int { return double(21); }\n",
    );
    assert!(
        ir.contains("#0"),
        "small pure fn should get #0 attribute, got:\n{}",
        ir
    );
    assert!(
        ir.contains("alwaysinline"),
        "IR should contain alwaysinline attribute"
    );
}

#[test]
fn test_no_inline_impure_fn() {
    let ir = compile(
        "fn greet() -> unit { print_str(\"hello\"); }\n\
         fn main() -> int { greet(); return 0; }\n",
    );
    assert!(
        !ir.contains("define void @greet()") || !ir.contains("#0"),
        "impure fn should not get #0 attribute"
    );
}

#[test]
fn test_no_inline_large_pure_fn() {
    let ir = compile(
        "pure fn big(a: int, b: int) -> int {\n\
         \x20   let x: int = a + b;\n\
         \x20   let y: int = x * 2;\n\
         \x20   let z: int = y - a;\n\
         \x20   let w: int = z + 1;\n\
         \x20   return w;\n\
         }\n\
         fn main() -> int { return big(1, 2); }\n",
    );
    // big has 5 statements, should not get #0
    assert!(
        !ir.contains("define i64 @big(") || !ir.contains("@big(i64 %a, i64 %b) #0"),
        "large pure fn (>3 stmts) should not get inline hint"
    );
}

#[test]
fn test_no_inline_fn_with_ensures() {
    let ir = compile(
        "pure fn safe_add(a: int, b: int) -> int\n\
         \x20   ensures result > 0\n\
         {\n\
         \x20   return a + b;\n\
         }\n\
         fn main() -> int { return safe_add(1, 2); }\n",
    );
    assert!(
        !ir.contains("@safe_add(i64 %a, i64 %b) #0"),
        "fn with ensures should not get inline hint"
    );
}

// ── Constant folding ────────────────────────────────────────

#[test]
fn test_const_fold_arithmetic() {
    let ir = compile("fn main() -> int { return 2 + 3; }\n");
    assert!(
        ir.contains("ret i64 5"),
        "2 + 3 should be folded to 5, got:\n{}",
        ir
    );
}

#[test]
fn test_const_fold_nested() {
    let ir = compile("fn main() -> int { return (10 - 3) * 2; }\n");
    assert!(
        ir.contains("ret i64 14"),
        "(10 - 3) * 2 should fold to 14, got:\n{}",
        ir
    );
}

#[test]
fn test_const_fold_comparison() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   if 3 > 2 { return 1; }\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(
        ir.contains("br i1 true"),
        "3 > 2 should fold to true, got:\n{}",
        ir
    );
}

#[test]
fn test_no_fold_division_by_zero() {
    let ir = compile("fn main() -> int { return 10 / 0; }\n");
    assert!(
        ir.contains("sdiv i64"),
        "10 / 0 should not be folded, got:\n{}",
        ir
    );
}

#[test]
fn test_no_fold_variables() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let x: int = 5;\n\
         \x20   return x + 3;\n\
         }\n",
    );
    assert!(
        ir.contains("add i64"),
        "x + 3 should not be folded (x is a variable), got:\n{}",
        ir
    );
}

// ── Tail call optimization ──────────────────────────────────

#[test]
fn test_tail_call_recursive() {
    let ir = compile(
        "fn factorial(n: int, acc: int) -> int {\n\
         \x20   if n < 2 { return acc; }\n\
         \x20   return factorial(n - 1, acc * n);\n\
         }\n\
         fn main() -> int { return factorial(5, 1); }\n",
    );
    assert!(
        ir.contains("tail call i64 @factorial"),
        "tail-recursive call should get tail hint, got:\n{}",
        ir
    );
}

#[test]
fn test_no_tail_call_with_ensures() {
    let ir = compile(
        "fn helper(n: int) -> int { return n + 1; }\n\
         fn checked(n: int) -> int\n\
         \x20   ensures result > 0\n\
         {\n\
         \x20   return helper(n);\n\
         }\n\
         fn main() -> int { return checked(5); }\n",
    );
    // With ensures, the return value goes through result.addr — no tail call
    assert!(
        !ir.contains("tail call i64 @helper"),
        "fn with ensures should not get tail call, got:\n{}",
        ir
    );
}

#[test]
fn test_no_tail_call_non_tail_position() {
    let ir = compile(
        "fn helper(n: int) -> int { return n + 1; }\n\
         fn wrapper(n: int) -> int {\n\
         \x20   let x: int = helper(n);\n\
         \x20   return x;\n\
         }\n\
         fn main() -> int { return wrapper(5); }\n",
    );
    // `let x = helper(n)` is not in tail position — return x is just a load
    assert!(
        !ir.contains("tail call i64 @helper"),
        "non-tail-position call should not get tail hint, got:\n{}",
        ir
    );
}

// ── Sort algorithm (heap sort) ──────────────────────────────

#[test]
fn test_sort_is_heapsort() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let a: [int] = [3, 1, 2];\n\
         \x20   let b: [int] = sort_int(a);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(
        ir.contains("heap sort"),
        "sort_int should use heap sort, got:\n{}",
        ir
    );
    assert!(
        !ir.contains("insertion sort"),
        "sort_int should not use insertion sort"
    );
}

// ── Dead code elimination ───────────────────────────────────

#[test]
fn test_dce_removes_unused_fn() {
    let ir = compile(
        "fn unused() -> int { return 42; }\n\
         fn main() -> int { return 0; }\n",
    );
    assert!(
        !ir.contains("@unused"),
        "unused function should be removed by DCE, got:\n{}",
        ir
    );
}

#[test]
fn test_dce_keeps_used_fn() {
    let ir = compile(
        "fn helper() -> int { return 42; }\n\
         fn main() -> int { return helper(); }\n",
    );
    assert!(
        ir.contains("define i64 @helper"),
        "called function should be kept by DCE, got:\n{}",
        ir
    );
}

#[test]
fn test_dce_transitive_reachability() {
    let ir = compile(
        "fn a() -> int { return b(); }\n\
         fn b() -> int { return 42; }\n\
         fn c() -> int { return 99; }\n\
         fn main() -> int { return a(); }\n",
    );
    assert!(
        ir.contains("define i64 @a"),
        "a should be kept (called by main)"
    );
    assert!(
        ir.contains("define i64 @b"),
        "b should be kept (called by a)"
    );
    assert!(
        !ir.contains("define i64 @c("),
        "c should be removed (unreachable)"
    );
}

#[test]
fn test_dce_removes_unused_struct() {
    let ir = compile(
        "struct Dead { x: int }\n\
         fn main() -> int { return 0; }\n",
    );
    assert!(
        !ir.contains("%Dead"),
        "unused struct should be removed by DCE, got:\n{}",
        ir
    );
}

#[test]
fn test_dce_keeps_enum_used_by_variant() {
    let ir = compile(
        "fn main() -> int {\n\
         \x20   let x: Option<int> = Some(42);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(
        ir.contains("Option__int"),
        "Option enum used via Some(42) should be kept, got:\n{}",
        ir
    );
}

// ---- P1 fix: constant folding i64::MIN / -1 must not panic the compiler ----

#[test]
fn test_const_fold_no_panic_on_int_min_div() {
    // i64::MIN / -1 overflows in two's complement — must not crash the compiler
    let ir = compile("fn main() -> int { return (-9223372036854775807 - 1) / -1; }\n");
    // Should produce IR (not panic). The division falls through to runtime.
    assert!(
        ir.contains("sdiv") || ir.contains("ret i64"),
        "i64::MIN / -1 should compile without panic, got:\n{}",
        ir
    );
}

#[test]
fn test_const_fold_no_panic_on_int_min_mod() {
    // i64::MIN % -1 also overflows — must not crash
    let ir = compile("fn main() -> int { return (-9223372036854775807 - 1) % -1; }\n");
    assert!(
        ir.contains("srem") || ir.contains("ret i64"),
        "i64::MIN %% -1 should compile without panic, got:\n{}",
        ir
    );
}

// ---- P1 fix: DCE must include contract expressions in reachability ----

#[test]
fn test_dce_keeps_fn_used_in_ensures() {
    let ir = compile(
        "fn is_positive(x: int) -> bool { return x > 0; }\n\
         fn compute() -> int ensures is_positive(result) { return 42; }\n\
         fn main() -> int { return compute(); }\n",
    );
    assert!(
        ir.contains("define i1 @is_positive("),
        "is_positive used in ensures should be kept, got:\n{}",
        ir
    );
}

#[test]
fn test_dce_keeps_fn_used_in_requires() {
    let ir = compile(
        "fn valid(x: int) -> bool { return x >= 0; }\n\
         fn process(n: int) -> int requires valid(n) { return n + 1; }\n\
         fn main() -> int { return process(5); }\n",
    );
    assert!(
        ir.contains("define i1 @valid("),
        "valid used in requires should be kept, got:\n{}",
        ir
    );
}

// ---- P1 fix: DCE must mark enum impl methods reachable ----

#[test]
fn test_dce_keeps_enum_impl_methods() {
    let ir = compile(
        "enum Color { Red, Green, Blue }\n\
         impl Color {\n\
         \x20   fn tag(self: &Color) -> int { return 1; }\n\
         }\n\
         fn main() -> int {\n\
         \x20   let c: Color = Red;\n\
         \x20   let t: int = c.tag();\n\
         \x20   return t;\n\
         }\n",
    );
    assert!(
        ir.contains("@Color_tag"),
        "Enum impl method Color.tag should be kept, got:\n{}",
        ir
    );
}
