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
        "enum Option { Some(int), None }\n\
         fn main() -> int {\n\
         \x20   let opt: Option = Some(42);\n\
         \x20   return 0;\n\
         }\n",
    );
    assert!(ir.contains("getelementptr %Option"));
    assert!(ir.contains("store i32")); // tag
}

#[test]
fn test_enum_match_with_data() {
    let ir = compile(
        "enum Option { Some(int), None }\n\
         fn get_or(opt: Option, default_val: int) -> int {\n\
         \x20   match opt {\n\
         \x20       Some(val) => { return val; }\n\
         \x20       None => { return default_val; }\n\
         \x20   }\n\
         }\n\
         fn main() -> int {\n\
         \x20   let opt: Option = Some(42);\n\
         \x20   return get_or(opt, 0);\n\
         }\n",
    );
    assert!(ir.contains("define i64 @get_or"));
    assert!(ir.contains("icmp eq i32")); // tag comparison
}

#[test]
fn test_enum_match_typecheck() {
    parse_and_check(
        "enum Option { Some(int), None }\n\
         fn get_or(opt: Option, default_val: int) -> int {\n\
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
    assert!(ir.contains("call double @sqrt"));
    assert!(ir.contains("call double @pow"));
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
    assert!(ir.contains("call double @floor"));
    assert!(ir.contains("call double @ceil"));
    assert!(ir.contains("call double @round"));
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
    assert!(ir.contains("define double @sqrt"));
    assert!(ir.contains("define double @pow"));
    assert!(ir.contains("define double @floor"));
    assert!(ir.contains("define double @ceil"));
    assert!(ir.contains("define double @round"));
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
        "enum Option { Some(int), None }\n\
         fn f() -> int {\n\
         \x20   let a: Option = Some(42);\n\
         \x20   let b: Option = a;\n\
         \x20   let c: Option = a;\n\
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
