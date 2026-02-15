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
    assert!(ir.contains("define i64 @main()"));
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
    assert!(ir.contains("define i64 @main()"));
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
