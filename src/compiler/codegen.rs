//! LLVM IR code generation for the Yorum language.
//!
//! Emits textual LLVM IR that can be compiled by `llc` + `clang` or consumed
//! by any LLVM-based toolchain.  Uses the alloca/load/store pattern so that
//! LLVM's mem2reg pass promotes stack slots to SSA registers automatically.

use crate::compiler::ast::*;
use std::collections::{HashMap, HashSet};
use std::fmt;

// ═══════════════════════════════════════════════════════════════
//  Helpers
// ═══════════════════════════════════════════════════════════════

/// Map an LLVM type name to the semantic name used in tuple type identifiers.
fn llvm_to_semantic_name(t: &str) -> String {
    match t {
        "i64" => "int".to_string(),
        "double" => "float".to_string(),
        "i1" => "bool".to_string(),
        "i8" => "char".to_string(),
        "ptr" => "string".to_string(),
        "void" => "unit".to_string(),
        other => other.strip_prefix('%').unwrap_or(other).to_string(),
    }
}

// ═══════════════════════════════════════════════════════════════
//  Error type
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct CodegenError {
    pub message: String,
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "codegen error: {}", self.message)
    }
}

impl std::error::Error for CodegenError {}

// ═══════════════════════════════════════════════════════════════
//  Internal types
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
struct VarSlot {
    ptr: String,     // LLVM alloca name (e.g., "%x.addr")
    llvm_ty: String, // LLVM type string (e.g., "i64")
}

/// Tracks per-variable string buffer metadata for capacity-aware str_concat.
/// When `cap == 0`, the data pointer is not an owned heap buffer (e.g. a global
/// literal or a fresh allocation from a function).  The first inline concat
/// transitions it to a tracked buffer.
#[derive(Debug, Clone)]
struct StringBufInfo {
    len_ptr: String, // alloca for tracked length (i64)
    cap_ptr: String, // alloca for tracked capacity (i64), 0 = uninitialized
}

#[derive(Debug, Clone)]
struct StructLayout {
    #[allow(dead_code)]
    name: String,
    fields: Vec<(String, Type)>,
    llvm_ty: String,
}

#[derive(Debug, Clone)]
struct EnumLayout {
    #[allow(dead_code)]
    name: String,
    variants: Vec<(String, Vec<Type>)>,
    #[allow(dead_code)]
    tag_count: usize,
}

// ═══════════════════════════════════════════════════════════════
//  Code generator
// ═══════════════════════════════════════════════════════════════

pub struct Codegen {
    // Output buffers
    preamble: String,
    type_defs: String,
    globals: String,
    body: String,

    // Counters for generating unique names
    temp_counter: u32,
    label_counter: u32,
    string_counter: u32,
    closure_counter: u32,
    spawn_counter: u32,
    contract_string_counter: u32,

    // Symbol tables
    vars: Vec<HashMap<String, VarSlot>>,
    struct_layouts: HashMap<String, StructLayout>,
    enum_layouts: HashMap<String, EnumLayout>,
    fn_ret_types: HashMap<String, Type>,

    // Closure support
    deferred_fns: Vec<String>,
    closure_var_types: HashMap<String, Type>,

    // Array support: tracks the LLVM element type for each array variable
    array_elem_types: HashMap<String, String>,

    // Tuple support: tracks LLVM element types for each tuple type name
    tuple_elem_types: HashMap<String, Vec<String>>,
    // Tracks AST-level Type for variables (used for tuple field access)
    var_ast_types: HashMap<String, Type>,

    // State within a function
    current_fn_ret_ty: Option<String>,
    block_terminated: bool,
    current_block: String,

    // Contract support
    current_fn_contracts: Vec<Contract>,
    current_fn_name: String,

    // Loop control flow: (continue_label, break_label)
    loop_labels: Vec<(String, String)>,

    // Expected enum type hint for variant constructor disambiguation
    current_expected_enum: Option<String>,

    // Track which map/set helper suffixes have been emitted
    emitted_map_helpers: HashSet<String>,

    // Spawn context: (env_type_name, result_field_idx) — when set, `return`
    // statements store the value in the env result slot and emit `ret ptr null`
    spawn_return_ctx: Option<(String, usize)>,

    // Tracks task variable → (env_type_name, result_field_idx) so .join()
    // can load the result from the correct env struct field
    task_env_info: HashMap<String, (String, usize)>,

    // Set by emit_spawn, consumed by emit_let to populate task_env_info
    last_spawn_env_info: Option<(String, usize)>,

    // Inline hint: at least one function was annotated with #0
    has_inline_fns: bool,

    // Tail call hint: set by emit_return when expression is a simple tail call
    tail_call_hint: bool,

    // String buffer optimization: per-variable {len, cap} tracking
    string_buf_vars: HashMap<String, StringBufInfo>,

    // Debug info (DWARF)
    debug_enabled: bool,
    debug_filename: String,
    debug_directory: String,
    debug_metadata_counter: u32,
    debug_metadata: Vec<String>,
    debug_cu_id: u32,
    debug_file_id: u32,
    debug_current_subprogram: u32,
    debug_basic_types: HashMap<String, u32>,
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            preamble: String::new(),
            type_defs: String::new(),
            globals: String::new(),
            body: String::new(),
            temp_counter: 0,
            label_counter: 0,
            string_counter: 0,
            closure_counter: 0,
            spawn_counter: 0,
            contract_string_counter: 0,
            vars: Vec::new(),
            struct_layouts: HashMap::new(),
            enum_layouts: HashMap::new(),
            fn_ret_types: HashMap::new(),
            deferred_fns: Vec::new(),
            closure_var_types: HashMap::new(),
            array_elem_types: HashMap::new(),
            tuple_elem_types: HashMap::new(),
            var_ast_types: HashMap::new(),
            current_fn_ret_ty: None,
            block_terminated: false,
            current_block: "entry".to_string(),
            current_fn_contracts: Vec::new(),
            current_fn_name: String::new(),
            loop_labels: Vec::new(),
            current_expected_enum: None,
            emitted_map_helpers: HashSet::new(),
            spawn_return_ctx: None,
            task_env_info: HashMap::new(),
            last_spawn_env_info: None,
            has_inline_fns: false,
            tail_call_hint: false,
            string_buf_vars: HashMap::new(),
            debug_enabled: false,
            debug_filename: String::new(),
            debug_directory: String::new(),
            debug_metadata_counter: 0,
            debug_metadata: Vec::new(),
            debug_cu_id: 0,
            debug_file_id: 0,
            debug_current_subprogram: 0,
            debug_basic_types: HashMap::new(),
        }
    }

    /// Enable DWARF debug info emission.
    pub fn enable_debug(&mut self, filename: &str) {
        self.debug_enabled = true;
        let path = std::path::Path::new(filename);
        self.debug_filename = path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| filename.to_string());
        self.debug_directory = path
            .parent()
            .map(|p| {
                if p.as_os_str().is_empty() {
                    ".".to_string()
                } else {
                    p.to_string_lossy().to_string()
                }
            })
            .unwrap_or_else(|| ".".to_string());
    }

    fn fresh_debug_id(&mut self) -> u32 {
        let id = self.debug_metadata_counter;
        self.debug_metadata_counter += 1;
        id
    }

    fn emit_debug_preamble(&mut self) {
        // !0 = DIFile
        self.debug_file_id = self.fresh_debug_id();
        self.debug_metadata.push(format!(
            "!{} = !DIFile(filename: \"{}\", directory: \"{}\")",
            self.debug_file_id, self.debug_filename, self.debug_directory
        ));

        // !1 = DICompileUnit
        self.debug_cu_id = self.fresh_debug_id();
        let version = env!("CARGO_PKG_VERSION");
        self.debug_metadata.push(format!(
            "!{} = distinct !DICompileUnit(language: DW_LANG_C, file: !{}, producer: \"yorum {}\", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)",
            self.debug_cu_id, self.debug_file_id, version
        ));

        // Basic types
        let types = [
            ("int", "i64", 64, 5),      // DW_ATE_signed
            ("float", "double", 64, 4), // DW_ATE_float
            ("bool", "i1", 8, 2),       // DW_ATE_boolean
            ("char", "i8", 8, 6),       // DW_ATE_signed_char
        ];
        for (name, _llvm, size, encoding) in &types {
            let id = self.fresh_debug_id();
            self.debug_metadata.push(format!(
                "!{} = !DIBasicType(name: \"{}\", size: {}, encoding: DW_ATE_{})",
                id,
                name,
                size,
                match *encoding {
                    2 => "boolean",
                    4 => "float",
                    5 => "signed",
                    6 => "signed_char",
                    _ => "signed",
                }
            ));
            self.debug_basic_types.insert(name.to_string(), id);
        }
    }

    fn emit_debug_subprogram(&mut self, name: &str, line: u32) -> u32 {
        // Subroutine type (void for simplicity)
        let sr_type_id = self.fresh_debug_id();
        self.debug_metadata
            .push(format!("!{} = !DISubroutineType(types: !{{}})", sr_type_id));

        let sp_id = self.fresh_debug_id();
        self.debug_metadata.push(format!(
            "!{} = distinct !DISubprogram(name: \"{}\", scope: !{}, file: !{}, line: {}, type: !{}, scopeLine: {}, unit: !{}, spFlags: DISPFlagDefinition)",
            sp_id, name, self.debug_file_id, self.debug_file_id, line, sr_type_id, line, self.debug_cu_id
        ));
        self.debug_current_subprogram = sp_id;
        sp_id
    }

    #[allow(dead_code)]
    fn emit_debug_location(&mut self, line: u32, col: u32) -> u32 {
        let id = self.fresh_debug_id();
        self.debug_metadata.push(format!(
            "!{} = !DILocation(line: {}, column: {}, scope: !{})",
            id, line, col, self.debug_current_subprogram
        ));
        id
    }

    #[allow(dead_code)]
    fn debug_suffix_for_line(&mut self, line: u32, col: u32) -> String {
        if !self.debug_enabled || line == 0 {
            return String::new();
        }
        let id = self.emit_debug_location(line, col);
        format!(", !dbg !{}", id)
    }

    /// Generate LLVM IR for the entire program. Returns the IR as a string.
    pub fn generate(&mut self, program: &Program) -> Result<String, CodegenError> {
        self.emit_preamble(program);
        if self.debug_enabled {
            self.emit_debug_preamble();
        }
        self.register_types(program);
        self.emit_type_defs();
        self.emit_builtin_decls();
        self.emit_builtin_helpers();

        // Register built-in function return types
        self.fn_ret_types
            .insert("print_int".to_string(), Type::Unit);
        self.fn_ret_types
            .insert("print_float".to_string(), Type::Unit);
        self.fn_ret_types
            .insert("print_bool".to_string(), Type::Unit);
        self.fn_ret_types
            .insert("print_str".to_string(), Type::Unit);
        self.fn_ret_types.insert("str_len".to_string(), Type::Int);
        self.fn_ret_types
            .insert("str_concat".to_string(), Type::Str);
        self.fn_ret_types.insert("str_eq".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("print_char".to_string(), Type::Unit);
        self.fn_ret_types
            .insert("char_to_int".to_string(), Type::Int);
        self.fn_ret_types
            .insert("int_to_char".to_string(), Type::Char);
        self.fn_ret_types
            .insert("int_to_float".to_string(), Type::Float);
        self.fn_ret_types
            .insert("float_to_int".to_string(), Type::Int);
        self.fn_ret_types
            .insert("int_to_str".to_string(), Type::Str);
        self.fn_ret_types
            .insert("str_to_int".to_string(), Type::Int);
        self.fn_ret_types
            .insert("str_charAt".to_string(), Type::Char);
        self.fn_ret_types.insert("str_sub".to_string(), Type::Str);
        self.fn_ret_types
            .insert("str_from_char".to_string(), Type::Str);
        self.fn_ret_types
            .insert("char_is_alpha".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("char_is_digit".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("char_is_whitespace".to_string(), Type::Bool);
        self.fn_ret_types.insert("file_read".to_string(), Type::Str);
        self.fn_ret_types
            .insert("file_write".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("print_err".to_string(), Type::Unit);
        self.fn_ret_types.insert(
            "map_new".to_string(),
            Type::Generic("Map".to_string(), vec![Type::Str, Type::Int]),
        );
        self.fn_ret_types.insert("map_set".to_string(), Type::Unit);
        self.fn_ret_types.insert("map_get".to_string(), Type::Int);
        self.fn_ret_types.insert("map_has".to_string(), Type::Bool);
        // Math builtins
        self.fn_ret_types.insert("abs_int".to_string(), Type::Int);
        self.fn_ret_types
            .insert("abs_float".to_string(), Type::Float);
        self.fn_ret_types.insert("min_int".to_string(), Type::Int);
        self.fn_ret_types.insert("max_int".to_string(), Type::Int);
        self.fn_ret_types
            .insert("min_float".to_string(), Type::Float);
        self.fn_ret_types
            .insert("max_float".to_string(), Type::Float);
        self.fn_ret_types.insert("sqrt".to_string(), Type::Float);
        self.fn_ret_types.insert("pow".to_string(), Type::Float);
        self.fn_ret_types.insert("sin".to_string(), Type::Float);
        self.fn_ret_types.insert("cos".to_string(), Type::Float);
        self.fn_ret_types.insert("tan".to_string(), Type::Float);
        self.fn_ret_types.insert("floor".to_string(), Type::Float);
        self.fn_ret_types.insert("ceil".to_string(), Type::Float);
        self.fn_ret_types.insert("round".to_string(), Type::Float);
        self.fn_ret_types.insert("log".to_string(), Type::Float);
        self.fn_ret_types.insert("log10".to_string(), Type::Float);
        self.fn_ret_types.insert("exp".to_string(), Type::Float);
        // String utility builtins
        self.fn_ret_types
            .insert("str_contains".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("str_index_of".to_string(), Type::Int);
        self.fn_ret_types
            .insert("str_starts_with".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("str_ends_with".to_string(), Type::Bool);
        self.fn_ret_types.insert("str_trim".to_string(), Type::Str);
        self.fn_ret_types
            .insert("str_replace".to_string(), Type::Str);
        self.fn_ret_types
            .insert("str_to_upper".to_string(), Type::Str);
        self.fn_ret_types
            .insert("str_to_lower".to_string(), Type::Str);
        self.fn_ret_types
            .insert("str_repeat".to_string(), Type::Str);
        self.fn_ret_types
            .insert("str_split".to_string(), Type::Array(Box::new(Type::Str)));
        // Collection utility builtins
        self.fn_ret_types
            .insert("contains_int".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("contains_str".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("sort_int".to_string(), Type::Array(Box::new(Type::Int)));
        self.fn_ret_types
            .insert("sort_str".to_string(), Type::Array(Box::new(Type::Str)));
        // Map utility builtins (backward compat for string->int maps)
        self.fn_ret_types.insert("map_size".to_string(), Type::Int);
        self.fn_ret_types
            .insert("map_remove".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("map_keys".to_string(), Type::Array(Box::new(Type::Str)));
        self.fn_ret_types
            .insert("map_values".to_string(), Type::Array(Box::new(Type::Int)));
        // Mangled map builtins for monomorphized Map<string, int>
        self.fn_ret_types.insert(
            "map_new__string__int".to_string(),
            Type::Generic("Map".to_string(), vec![Type::Str, Type::Int]),
        );
        self.fn_ret_types
            .insert("map_set__string__int".to_string(), Type::Unit);
        self.fn_ret_types
            .insert("map_get__string__int".to_string(), Type::Int);
        self.fn_ret_types
            .insert("map_has__string__int".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("map_size__string__int".to_string(), Type::Int);
        self.fn_ret_types
            .insert("map_remove__string__int".to_string(), Type::Bool);
        self.fn_ret_types.insert(
            "map_keys__string__int".to_string(),
            Type::Array(Box::new(Type::Str)),
        );
        self.fn_ret_types.insert(
            "map_values__string__int".to_string(),
            Type::Array(Box::new(Type::Int)),
        );
        // Set builtins (registered when set helpers are emitted)
        // set_new, set_add, set_has, set_remove, set_size, set_values
        // Enhanced I/O builtins
        self.fn_ret_types
            .insert("file_exists".to_string(), Type::Bool);
        self.fn_ret_types
            .insert("file_append".to_string(), Type::Bool);
        self.fn_ret_types.insert("read_line".to_string(), Type::Str);
        self.fn_ret_types
            .insert("print_flush".to_string(), Type::Unit);
        self.fn_ret_types.insert("env_get".to_string(), Type::Str);
        self.fn_ret_types.insert("time_ms".to_string(), Type::Int);
        // Networking builtins
        self.fn_ret_types
            .insert("tcp_connect".to_string(), Type::Int);
        self.fn_ret_types
            .insert("tcp_listen".to_string(), Type::Int);
        self.fn_ret_types
            .insert("tcp_accept".to_string(), Type::Int);
        self.fn_ret_types.insert("tcp_send".to_string(), Type::Int);
        self.fn_ret_types.insert("tcp_recv".to_string(), Type::Str);
        self.fn_ret_types
            .insert("tcp_close".to_string(), Type::Unit);
        self.fn_ret_types
            .insert("udp_socket".to_string(), Type::Int);
        self.fn_ret_types.insert("udp_bind".to_string(), Type::Int);
        self.fn_ret_types
            .insert("udp_send_to".to_string(), Type::Int);
        self.fn_ret_types
            .insert("udp_recv_from".to_string(), Type::Str);
        self.fn_ret_types
            .insert("dns_resolve".to_string(), Type::Str);
        self.fn_ret_types
            .insert("http_request".to_string(), Type::Str);
        self.fn_ret_types.insert("http_get".to_string(), Type::Str);
        self.fn_ret_types.insert("http_post".to_string(), Type::Str);
        // String conversion builtins
        self.fn_ret_types
            .insert("float_to_str".to_string(), Type::Str);
        self.fn_ret_types
            .insert("bool_to_str".to_string(), Type::Str);
        self.fn_ret_types.insert("to_str".to_string(), Type::Str);

        // Register function return types (including impl methods with mangled names)
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) => {
                    self.fn_ret_types
                        .insert(f.name.clone(), f.return_type.clone());
                }
                Declaration::Impl(i) => {
                    for method in &i.methods {
                        let mangled = format!("{}_{}", i.target_type, method.name);
                        self.fn_ret_types
                            .insert(mangled, method.return_type.clone());
                    }
                }
                _ => {}
            }
        }

        // Scan for non-string-int map instantiations and emit helpers
        self.emit_generic_map_set_helpers(program);

        // Emit each function and impl methods
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) => {
                    self.emit_function(f)?;
                }
                Declaration::Impl(i) => {
                    for method in &i.methods {
                        self.emit_method(&i.target_type, method)?;
                    }
                }
                _ => {}
            }
        }

        // Assemble final output
        let mut output = String::new();
        output.push_str(&self.preamble);
        output.push('\n');
        output.push_str(&self.type_defs);
        output.push('\n');
        output.push_str(&self.globals);
        output.push('\n');
        output.push_str(&self.body);

        // Append inline hint attributes
        if self.has_inline_fns {
            output.push_str("attributes #0 = { alwaysinline }\n");
        }

        // Append debug metadata
        if self.debug_enabled {
            output.push('\n');
            // Module flags for DWARF
            let dwarf_ver_id = self.fresh_debug_id();
            let debug_info_ver_id = self.fresh_debug_id();
            output.push_str(&format!("!llvm.dbg.cu = !{{!{}}}\n", self.debug_cu_id));
            output.push_str(&format!(
                "!llvm.module.flags = !{{!{}, !{}}}\n",
                dwarf_ver_id, debug_info_ver_id
            ));
            output.push_str(&format!(
                "!{} = !{{i32 7, !\"Dwarf Version\", i32 5}}\n",
                dwarf_ver_id
            ));
            output.push_str(&format!(
                "!{} = !{{i32 2, !\"Debug Info Version\", i32 3}}\n",
                debug_info_ver_id
            ));
            for md in &self.debug_metadata {
                output.push_str(md);
                output.push('\n');
            }
        }

        Ok(output)
    }

    // ── Preamble ─────────────────────────────────────────────

    fn emit_preamble(&mut self, program: &Program) {
        let module_name = program.module_name.as_deref().unwrap_or("yorum_module");
        self.preamble.push_str(&format!(
            "; ModuleID = '{}'\n\
             source_filename = \"{}.yrm\"\n\n",
            module_name, module_name
        ));
    }

    fn emit_builtin_decls(&mut self) {
        self.globals.push_str("; ── External declarations ──\n");
        self.globals.push_str("declare i32 @printf(ptr, ...)\n");
        self.globals.push_str("declare i32 @puts(ptr)\n");
        self.globals.push_str("declare ptr @malloc(i64)\n");
        self.globals.push_str("declare void @free(ptr)\n");
        self.globals.push_str("declare void @abort()\n");
        self.globals.push_str("declare i64 @strlen(ptr)\n");
        self.globals.push_str("declare ptr @strcpy(ptr, ptr)\n");
        self.globals.push_str("declare ptr @strcat(ptr, ptr)\n");
        self.globals.push_str("declare i32 @strcmp(ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_create(ptr, ptr, ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_join(i64, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_mutex_init(ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_mutex_lock(ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_mutex_unlock(ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_mutex_destroy(ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_cond_init(ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_cond_wait(ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_cond_signal(ptr)\n");
        self.globals
            .push_str("declare i32 @pthread_cond_destroy(ptr)\n");
        self.globals
            .push_str("declare ptr @memcpy(ptr, ptr, i64)\n");
        self.globals
            .push_str("declare i32 @snprintf(ptr, i64, ptr, ...)\n");
        self.globals.push_str("declare i64 @atol(ptr)\n");
        self.globals.push_str("declare i32 @putchar(i32)\n");
        self.globals.push_str("declare ptr @realloc(ptr, i64)\n");
        self.globals.push_str("declare ptr @fopen(ptr, ptr)\n");
        self.globals.push_str("declare i32 @fclose(ptr)\n");
        self.globals.push_str("declare i32 @fseek(ptr, i64, i32)\n");
        self.globals.push_str("declare i64 @ftell(ptr)\n");
        self.globals
            .push_str("declare i64 @fread(ptr, i64, i64, ptr)\n");
        self.globals
            .push_str("declare i64 @fwrite(ptr, i64, i64, ptr)\n");
        self.globals.push_str("declare i64 @write(i32, ptr, i64)\n");
        self.globals.push_str("declare void @exit(i32)\n");
        self.globals
            .push_str("declare ptr @memset(ptr, i32, i64)\n");
        // Math intrinsics/libm
        self.globals
            .push_str("declare double @llvm.fabs.f64(double)\n");
        self.globals
            .push_str("declare double @llvm.sqrt.f64(double)\n");
        self.globals
            .push_str("declare double @llvm.pow.f64(double, double)\n");
        self.globals
            .push_str("declare double @llvm.floor.f64(double)\n");
        self.globals
            .push_str("declare double @llvm.ceil.f64(double)\n");
        self.globals
            .push_str("declare double @llvm.round.f64(double)\n");
        self.globals
            .push_str("declare double @llvm.minnum.f64(double, double)\n");
        self.globals
            .push_str("declare double @llvm.maxnum.f64(double, double)\n");
        self.globals.push_str("declare double @sin(double)\n");
        self.globals.push_str("declare double @cos(double)\n");
        self.globals.push_str("declare double @tan(double)\n");
        self.globals.push_str("declare double @log(double)\n");
        self.globals.push_str("declare double @log10(double)\n");
        self.globals.push_str("declare double @exp(double)\n");
        // String utility externals
        self.globals.push_str("declare ptr @strstr(ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @strncmp(ptr, ptr, i64)\n");
        // Enhanced I/O externals
        self.globals.push_str("declare i32 @access(ptr, i32)\n");
        self.globals.push_str("declare ptr @getenv(ptr)\n");
        self.globals.push_str("declare i32 @fflush(ptr)\n");
        self.globals.push_str("declare ptr @fgets(ptr, i32, ptr)\n");
        self.globals
            .push_str("declare i32 @gettimeofday(ptr, ptr)\n");
        if cfg!(target_os = "macos") {
            self.globals.push_str("@__stdinp = external global ptr\n");
        } else {
            self.globals.push_str("@stdin = external global ptr\n");
        }
        // Networking externals
        self.globals
            .push_str("declare i32 @socket(i32, i32, i32)\n");
        self.globals
            .push_str("declare i32 @connect(i32, ptr, i32)\n");
        self.globals.push_str("declare i32 @bind(i32, ptr, i32)\n");
        self.globals.push_str("declare i32 @listen(i32, i32)\n");
        self.globals
            .push_str("declare i32 @accept(i32, ptr, ptr)\n");
        self.globals
            .push_str("declare i32 @setsockopt(i32, i32, i32, ptr, i32)\n");
        self.globals.push_str("declare i32 @close(i32)\n");
        self.globals.push_str("declare i64 @read(i32, ptr, i64)\n");
        self.globals
            .push_str("declare i32 @getaddrinfo(ptr, ptr, ptr, ptr)\n");
        self.globals.push_str("declare void @freeaddrinfo(ptr)\n");
        self.globals
            .push_str("declare ptr @inet_ntop(i32, ptr, ptr, i32)\n");
        self.globals
            .push_str("declare i64 @sendto(i32, ptr, i64, i32, ptr, i32)\n");
        self.globals
            .push_str("declare i64 @recvfrom(i32, ptr, i64, i32, ptr, ptr)\n\n");
        // Globals for command-line arguments (stored by main)
        self.globals
            .push_str("@__yorum_argc = internal global i32 0\n");
        self.globals
            .push_str("@__yorum_argv = internal global ptr null\n\n");

        // Format strings (%lld\n\0 = 6 bytes, %f\n\0 = 4 bytes)
        self.globals
            .push_str("@.fmt.int = private unnamed_addr constant [6 x i8] c\"%lld\\0A\\00\"\n");
        self.globals
            .push_str("@.fmt.float = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\"\n");
        self.globals
            .push_str("@.fmt.true = private unnamed_addr constant [6 x i8] c\"true\\0A\\00\"\n");
        self.globals
            .push_str("@.fmt.false = private unnamed_addr constant [7 x i8] c\"false\\0A\\00\"\n");
        self.globals.push_str(
            "@.fmt.bounds = private unnamed_addr constant [40 x i8] c\"array index out of bounds: %lld >= %lld\\00\"\n",
        );
        self.globals.push_str(
            "@.fmt.contract = private unnamed_addr constant [24 x i8] c\"contract violation: %s\\0A\\00\"\n",
        );
        self.globals
            .push_str("@.fmt.lld = private unnamed_addr constant [5 x i8] c\"%lld\\00\"\n");
        self.globals.push_str(
            "@.fmt.pop_empty = private unnamed_addr constant [22 x i8] c\"pop from empty array\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.fmt.map_key = private unnamed_addr constant [25 x i8] c\"map key not found: '%s'\\0A\\00\"\n",
        );
        self.globals
            .push_str("@.str.r = private unnamed_addr constant [2 x i8] c\"r\\00\"\n");
        self.globals
            .push_str("@.str.w = private unnamed_addr constant [2 x i8] c\"w\\00\"\n");
        self.globals
            .push_str("@.str.newline = private unnamed_addr constant [1 x i8] c\"\\0A\"\n");
        self.globals
            .push_str("@.str.a = private unnamed_addr constant [2 x i8] c\"a\\00\"\n");
        self.globals
            .push_str("@.fmt.str = private unnamed_addr constant [3 x i8] c\"%s\\00\"\n");
        // Networking constants
        self.globals
            .push_str("@.str.empty = private unnamed_addr constant [1 x i8] c\"\\00\"\n");
        // String conversion format strings
        self.globals
            .push_str("@.fmt.g = private unnamed_addr constant [3 x i8] c\"%g\\00\"\n");
        self.globals
            .push_str("@.str.true = private unnamed_addr constant [5 x i8] c\"true\\00\"\n");
        self.globals
            .push_str("@.str.false = private unnamed_addr constant [6 x i8] c\"false\\00\"\n");
        self.globals.push_str(
            "@.str.http_req_line = private unnamed_addr constant [15 x i8] c\"%s %s HTTP/1.0\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_host_hdr = private unnamed_addr constant [30 x i8] c\"\\0D\\0AHost: %s\\0D\\0AConnection: close\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_cl_hdr = private unnamed_addr constant [23 x i8] c\"\\0D\\0AContent-Length: %lld\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_crlf2 = private unnamed_addr constant [5 x i8] c\"\\0D\\0A\\0D\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_crlf = private unnamed_addr constant [3 x i8] c\"\\0D\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_sep = private unnamed_addr constant [5 x i8] c\"\\0D\\0A\\0D\\0A\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_get_method = private unnamed_addr constant [4 x i8] c\"GET\\00\"\n",
        );
        self.globals.push_str(
            "@.str.http_post_method = private unnamed_addr constant [5 x i8] c\"POST\\00\"\n",
        );
        self.globals.push('\n');
    }

    fn emit_builtin_helpers(&mut self) {
        // print_int
        self.body.push_str(
            "define void @print_int(i64 %x) {\n\
             entry:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.int, i64 %x)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // print_float
        self.body.push_str(
            "define void @print_float(double %x) {\n\
             entry:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.float, double %x)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // print_bool
        self.body.push_str(
            "define void @print_bool(i1 %x) {\n\
             entry:\n\
             \x20 br i1 %x, label %is_true, label %is_false\n\
             is_true:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.true)\n\
             \x20 ret void\n\
             is_false:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.false)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // print_str — prints a global string pointer
        self.body.push_str(
            "define void @print_str(ptr %s) {\n\
             entry:\n\
             \x20 call i32 @puts(ptr %s)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // __yorum_bounds_check — aborts on out-of-bounds access
        self.body.push_str(
            "define void @__yorum_bounds_check(i64 %idx, i64 %len) {\n\
             entry:\n\
             \x20 %neg = icmp slt i64 %idx, 0\n\
             \x20 br i1 %neg, label %fail, label %check_upper\n\
             check_upper:\n\
             \x20 %oob = icmp sge i64 %idx, %len\n\
             \x20 br i1 %oob, label %fail, label %ok\n\
             fail:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.bounds, i64 %idx, i64 %len)\n\
             \x20 call void @abort()\n\
             \x20 unreachable\n\
             ok:\n\
             \x20 ret void\n\
             }\n\n",
        );
        // str_len — returns length of a string
        self.body.push_str(
            "define i64 @str_len(ptr %s) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 ret i64 %len\n\
             }\n\n",
        );
        // str_concat — concatenates two strings into a new heap-allocated string
        // Uses memcpy instead of strcpy/strcat to avoid redundant strlen scans
        self.body.push_str(
            "define ptr @str_concat(ptr %a, ptr %b) {\n\
             entry:\n\
             \x20 %la = call i64 @strlen(ptr %a)\n\
             \x20 %lb = call i64 @strlen(ptr %b)\n\
             \x20 %sum = add i64 %la, %lb\n\
             \x20 %total = add i64 %sum, 1\n\
             \x20 %buf = call ptr @malloc(i64 %total)\n\
             \x20 call ptr @memcpy(ptr %buf, ptr %a, i64 %la)\n\
             \x20 %dest = getelementptr i8, ptr %buf, i64 %la\n\
             \x20 call ptr @memcpy(ptr %dest, ptr %b, i64 %lb)\n\
             \x20 %end = getelementptr i8, ptr %buf, i64 %sum\n\
             \x20 store i8 0, ptr %end\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_eq — compares two strings for equality
        self.body.push_str(
            "define i1 @str_eq(ptr %a, ptr %b) {\n\
             entry:\n\
             \x20 %cmp = call i32 @strcmp(ptr %a, ptr %b)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n\
             \x20 ret i1 %eq\n\
             }\n\n",
        );
        // __yorum_contract_fail — prints error message and aborts
        self.body.push_str(
            "define void @__yorum_contract_fail(ptr %msg) {\n\
             entry:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.contract, ptr %msg)\n\
             \x20 call void @abort()\n\
             \x20 unreachable\n\
             }\n\n",
        );

        // __yorum_chan_create — creates a channel (mutex + condvar + value slot + ready flag)
        // Channel layout: { mutex (64 bytes), condvar (64 bytes), value (8 bytes), ready (i32) }
        // Total: 144 bytes (padded)
        self.body.push_str(
            "define ptr @__yorum_chan_create() {\n\
             entry:\n\
             \x20 %ch = call ptr @malloc(i64 144)\n\
             \x20 call i32 @pthread_mutex_init(ptr %ch, ptr null)\n\
             \x20 %cond = getelementptr i8, ptr %ch, i64 64\n\
             \x20 call i32 @pthread_cond_init(ptr %cond, ptr null)\n\
             \x20 %ready = getelementptr i8, ptr %ch, i64 136\n\
             \x20 store i32 0, ptr %ready\n\
             \x20 ret ptr %ch\n\
             }\n\n",
        );
        // __yorum_chan_send — waits until slot is empty, stores value, and signals
        self.body.push_str(
            "define void @__yorum_chan_send(ptr %ch, i64 %val) {\n\
             entry:\n\
             \x20 call i32 @pthread_mutex_lock(ptr %ch)\n\
             \x20 br label %send_wait\n\
             send_wait:\n\
             \x20 %ready = getelementptr i8, ptr %ch, i64 136\n\
             \x20 %r = load i32, ptr %ready\n\
             \x20 %is_full = icmp eq i32 %r, 1\n\
             \x20 br i1 %is_full, label %send_do_wait, label %send_ready\n\
             send_do_wait:\n\
             \x20 %cond0 = getelementptr i8, ptr %ch, i64 64\n\
             \x20 call i32 @pthread_cond_wait(ptr %cond0, ptr %ch)\n\
             \x20 br label %send_wait\n\
             send_ready:\n\
             \x20 %slot = getelementptr i8, ptr %ch, i64 128\n\
             \x20 store i64 %val, ptr %slot\n\
             \x20 %ready2 = getelementptr i8, ptr %ch, i64 136\n\
             \x20 store i32 1, ptr %ready2\n\
             \x20 %cond = getelementptr i8, ptr %ch, i64 64\n\
             \x20 call i32 @pthread_cond_signal(ptr %cond)\n\
             \x20 call i32 @pthread_mutex_unlock(ptr %ch)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // __yorum_chan_recv — waits for value and returns it
        self.body.push_str(
            "define i64 @__yorum_chan_recv(ptr %ch) {\n\
             entry:\n\
             \x20 call i32 @pthread_mutex_lock(ptr %ch)\n\
             \x20 br label %wait_loop\n\
             wait_loop:\n\
             \x20 %ready = getelementptr i8, ptr %ch, i64 136\n\
             \x20 %r = load i32, ptr %ready\n\
             \x20 %is_ready = icmp eq i32 %r, 1\n\
             \x20 br i1 %is_ready, label %done, label %do_wait\n\
             do_wait:\n\
             \x20 %cond = getelementptr i8, ptr %ch, i64 64\n\
             \x20 call i32 @pthread_cond_wait(ptr %cond, ptr %ch)\n\
             \x20 br label %wait_loop\n\
             done:\n\
             \x20 %slot = getelementptr i8, ptr %ch, i64 128\n\
             \x20 %val = load i64, ptr %slot\n\
             \x20 store i32 0, ptr %ready\n\
             \x20 %cond2 = getelementptr i8, ptr %ch, i64 64\n\
             \x20 call i32 @pthread_cond_signal(ptr %cond2)\n\
             \x20 call i32 @pthread_mutex_unlock(ptr %ch)\n\
             \x20 ret i64 %val\n\
             }\n\n",
        );

        // print_char — prints a single character
        self.body.push_str(
            "define void @print_char(i8 %c) {\n\
             entry:\n\
             \x20 %ext = zext i8 %c to i32\n\
             \x20 call i32 @putchar(i32 %ext)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // char_to_int — zero-extends i8 to i64
        self.body.push_str(
            "define i64 @char_to_int(i8 %c) {\n\
             entry:\n\
             \x20 %ext = zext i8 %c to i64\n\
             \x20 ret i64 %ext\n\
             }\n\n",
        );
        // int_to_char — truncates i64 to i8
        self.body.push_str(
            "define i8 @int_to_char(i64 %n) {\n\
             entry:\n\
             \x20 %trunc = trunc i64 %n to i8\n\
             \x20 ret i8 %trunc\n\
             }\n\n",
        );
        // int_to_float — converts i64 to double
        self.body.push_str(
            "define double @int_to_float(i64 %n) {\n\
             entry:\n\
             \x20 %f = sitofp i64 %n to double\n\
             \x20 ret double %f\n\
             }\n\n",
        );
        // float_to_int — converts double to i64
        self.body.push_str(
            "define i64 @float_to_int(double %f) {\n\
             entry:\n\
             \x20 %n = fptosi double %f to i64\n\
             \x20 ret i64 %n\n\
             }\n\n",
        );
        // int_to_str — converts i64 to heap-allocated decimal string
        self.body.push_str(
            "define ptr @int_to_str(i64 %n) {\n\
             entry:\n\
             \x20 %buf = call ptr @malloc(i64 24)\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %buf, i64 24, ptr @.fmt.lld, i64 %n)\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // float_to_str — converts double to heap-allocated string using %g
        self.body.push_str(
            "define ptr @float_to_str(double %n) {\n\
             entry:\n\
             \x20 %buf = call ptr @malloc(i64 32)\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %buf, i64 32, ptr @.fmt.g, double %n)\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // bool_to_str — converts i1 to "true" or "false"
        self.body.push_str(
            "define ptr @bool_to_str(i1 %b) {\n\
             entry:\n\
             \x20 br i1 %b, label %is.true, label %is.false\n\
             is.true:\n\
             \x20 ret ptr @.str.true\n\
             is.false:\n\
             \x20 ret ptr @.str.false\n\
             }\n\n",
        );
        // str_to_int — parses string to i64
        self.body.push_str(
            "define i64 @str_to_int(ptr %s) {\n\
             entry:\n\
             \x20 %n = call i64 @atol(ptr %s)\n\
             \x20 ret i64 %n\n\
             }\n\n",
        );
        // str_charAt — index into string with bounds check, returns i8
        self.body.push_str(
            "define i8 @str_charAt(ptr %s, i64 %i) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 call void @__yorum_bounds_check(i64 %i, i64 %len)\n\
             \x20 %ptr = getelementptr i8, ptr %s, i64 %i\n\
             \x20 %c = load i8, ptr %ptr\n\
             \x20 ret i8 %c\n\
             }\n\n",
        );
        // str_sub — extract substring: str_sub(s, start, len) -> string
        self.body.push_str(
            "define ptr @str_sub(ptr %s, i64 %start, i64 %len) {\n\
             entry:\n\
             \x20 %total = add i64 %len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %total)\n\
             \x20 %src = getelementptr i8, ptr %s, i64 %start\n\
             \x20 call ptr @memcpy(ptr %buf, ptr %src, i64 %len)\n\
             \x20 %end = getelementptr i8, ptr %buf, i64 %len\n\
             \x20 store i8 0, ptr %end\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_from_char — convert a char to a single-character string
        self.body.push_str(
            "define ptr @str_from_char(i8 %c) {\n\
             entry:\n\
             \x20 %buf = call ptr @malloc(i64 2)\n\
             \x20 store i8 %c, ptr %buf\n\
             \x20 %end = getelementptr i8, ptr %buf, i64 1\n\
             \x20 store i8 0, ptr %end\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // char_is_alpha — returns true if c is a-z or A-Z
        self.body.push_str(
            "define i1 @char_is_alpha(i8 %c) {\n\
             entry:\n\
             \x20 %ge_a = icmp sge i8 %c, 97\n\
             \x20 %le_z = icmp sle i8 %c, 122\n\
             \x20 %lower = and i1 %ge_a, %le_z\n\
             \x20 %ge_A = icmp sge i8 %c, 65\n\
             \x20 %le_Z = icmp sle i8 %c, 90\n\
             \x20 %upper = and i1 %ge_A, %le_Z\n\
             \x20 %result = or i1 %lower, %upper\n\
             \x20 ret i1 %result\n\
             }\n\n",
        );
        // char_is_digit — returns true if c is 0-9
        self.body.push_str(
            "define i1 @char_is_digit(i8 %c) {\n\
             entry:\n\
             \x20 %ge_0 = icmp sge i8 %c, 48\n\
             \x20 %le_9 = icmp sle i8 %c, 57\n\
             \x20 %result = and i1 %ge_0, %le_9\n\
             \x20 ret i1 %result\n\
             }\n\n",
        );
        // char_is_whitespace — returns true if c is space, tab, newline, or carriage return
        self.body.push_str(
            "define i1 @char_is_whitespace(i8 %c) {\n\
             entry:\n\
             \x20 %is_space = icmp eq i8 %c, 32\n\
             \x20 %is_tab = icmp eq i8 %c, 9\n\
             \x20 %is_nl = icmp eq i8 %c, 10\n\
             \x20 %is_cr = icmp eq i8 %c, 13\n\
             \x20 %or1 = or i1 %is_space, %is_tab\n\
             \x20 %or2 = or i1 %or1, %is_nl\n\
             \x20 %result = or i1 %or2, %is_cr\n\
             \x20 ret i1 %result\n\
             }\n\n",
        );
        // file_read — reads entire file into a heap-allocated string
        self.body.push_str(
            "define ptr @file_read(ptr %path) {\n\
             entry:\n\
             \x20 %f = call ptr @fopen(ptr %path, ptr @.str.r)\n\
             \x20 %is_null = icmp eq ptr %f, null\n\
             \x20 br i1 %is_null, label %fail, label %opened\n\
             fail:\n\
             \x20 %empty = call ptr @malloc(i64 1)\n\
             \x20 store i8 0, ptr %empty\n\
             \x20 ret ptr %empty\n\
             opened:\n\
             \x20 call i32 @fseek(ptr %f, i64 0, i32 2)\n\
             \x20 %size = call i64 @ftell(ptr %f)\n\
             \x20 call i32 @fseek(ptr %f, i64 0, i32 0)\n\
             \x20 %buf_size = add i64 %size, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_size)\n\
             \x20 call i64 @fread(ptr %buf, i64 1, i64 %size, ptr %f)\n\
             \x20 %end = getelementptr i8, ptr %buf, i64 %size\n\
             \x20 store i8 0, ptr %end\n\
             \x20 call i32 @fclose(ptr %f)\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // file_write — writes string to file, returns true on success
        self.body.push_str(
            "define i1 @file_write(ptr %path, ptr %content) {\n\
             entry:\n\
             \x20 %f = call ptr @fopen(ptr %path, ptr @.str.w)\n\
             \x20 %is_null = icmp eq ptr %f, null\n\
             \x20 br i1 %is_null, label %fail, label %opened\n\
             fail:\n\
             \x20 ret i1 0\n\
             opened:\n\
             \x20 %len = call i64 @strlen(ptr %content)\n\
             \x20 %written = call i64 @fwrite(ptr %content, i64 1, i64 %len, ptr %f)\n\
             \x20 call i32 @fclose(ptr %f)\n\
             \x20 %ok = icmp eq i64 %written, %len\n\
             \x20 ret i1 %ok\n\
             }\n\n",
        );
        // print_err — writes string to stderr (fd 2) with newline
        self.body.push_str(
            "define void @print_err(ptr %s) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 call i64 @write(i32 2, ptr %s, i64 %len)\n\
             \x20 call i64 @write(i32 2, ptr @.str.newline, i64 1)\n\
             \x20 ret void\n\
             }\n\n",
        );

        // ── HashMap helpers ──
        // Map struct layout (40 bytes):
        //   offset 0:  ptr keys     (array of ptr to C strings)
        //   offset 8:  ptr values   (array of i64)
        //   offset 16: ptr flags    (array of i8: 0=empty, 1=occupied)
        //   offset 24: i64 capacity
        //   offset 32: i64 size

        // __yorum_hash_string — FNV-1a hash
        self.body.push_str(
            "define i64 @__yorum_hash_string(ptr %s) {\n\
             entry:\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cont ]\n\
             \x20 %h = phi i64 [ -3750763034362895579, %entry ], [ %h3, %cont ]\n\
             \x20 %cp = getelementptr i8, ptr %s, i64 %i\n\
             \x20 %c = load i8, ptr %cp\n\
             \x20 %done = icmp eq i8 %c, 0\n\
             \x20 br i1 %done, label %end, label %cont\n\
             cont:\n\
             \x20 %cv = zext i8 %c to i64\n\
             \x20 %h2 = xor i64 %h, %cv\n\
             \x20 %h3 = mul i64 %h2, 1099511628211\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             end:\n\
             \x20 ret i64 %h\n\
             }\n\n",
        );

        // __yorum_map_find_slot — find slot for key (used by set/get/has)
        // Returns index of matching slot or first empty slot.
        // Continues probing past tombstones (flag==2) to maintain chain integrity.
        // %map = ptr to map struct, %key = ptr to C string
        // Also takes %cap = capacity for convenience.
        self.body.push_str(
            "define i64 @__yorum_map_find_slot(ptr %map, ptr %key, i64 %cap) {\n\
             entry:\n\
             \x20 %hash = call i64 @__yorum_hash_string(ptr %key)\n\
             \x20 %mask = sub i64 %cap, 1\n\
             \x20 %start = and i64 %hash, %mask\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %probe\n\
             probe:\n\
             \x20 %idx = phi i64 [ %start, %entry ], [ %next, %advance ]\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %idx\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %is_empty = icmp eq i8 %flag, 0\n\
             \x20 br i1 %is_empty, label %done, label %check_tombstone\n\
             check_tombstone:\n\
             \x20 %is_tomb = icmp eq i8 %flag, 2\n\
             \x20 br i1 %is_tomb, label %advance, label %check_key\n\
             check_key:\n\
             \x20 %kp = getelementptr ptr, ptr %keys_p, i64 %idx\n\
             \x20 %k = load ptr, ptr %kp\n\
             \x20 %cmp = call i32 @strcmp(ptr %k, ptr %key)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n\
             \x20 br i1 %eq, label %done, label %advance\n\
             advance:\n\
             \x20 %next_raw = add i64 %idx, 1\n\
             \x20 %next = and i64 %next_raw, %mask\n\
             \x20 br label %probe\n\
             done:\n\
             \x20 ret i64 %idx\n\
             }\n\n",
        );

        // __yorum_map_grow — double capacity and rehash
        self.body.push_str(
            "define void @__yorum_map_grow(ptr %map) {\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %old_cap = load i64, ptr %cap_p\n\
             \x20 %new_cap = mul i64 %old_cap, 2\n\
             \x20 ; allocate new arrays\n\
             \x20 %kb = mul i64 %new_cap, 8\n\
             \x20 %vb = mul i64 %new_cap, 8\n\
             \x20 %new_keys = call ptr @malloc(i64 %kb)\n\
             \x20 %new_vals = call ptr @malloc(i64 %vb)\n\
             \x20 %new_flags = call ptr @malloc(i64 %new_cap)\n\
             \x20 call ptr @memset(ptr %new_flags, i32 0, i64 %new_cap)\n\
             \x20 ; load old arrays\n\
             \x20 %old_keys = load ptr, ptr %map\n\
             \x20 %vals_p = getelementptr i8, ptr %map, i64 8\n\
             \x20 %old_vals = load ptr, ptr %vals_p\n\
             \x20 %flags_p = getelementptr i8, ptr %map, i64 16\n\
             \x20 %old_flags = load ptr, ptr %flags_p\n\
             \x20 ; store new arrays and capacity\n\
             \x20 store ptr %new_keys, ptr %map\n\
             \x20 store ptr %new_vals, ptr %vals_p\n\
             \x20 store ptr %new_flags, ptr %flags_p\n\
             \x20 store i64 %new_cap, ptr %cap_p\n\
             \x20 %tomb_p = getelementptr i8, ptr %map, i64 40\n\
             \x20 store i64 0, ptr %tomb_p\n\
             \x20 br label %rehash_loop\n\
             rehash_loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %rehash_cont ]\n\
             \x20 %cmp = icmp slt i64 %i, %old_cap\n\
             \x20 br i1 %cmp, label %rehash_body, label %rehash_done\n\
             rehash_body:\n\
             \x20 %ofp = getelementptr i8, ptr %old_flags, i64 %i\n\
             \x20 %of = load i8, ptr %ofp\n\
             \x20 %occ = icmp eq i8 %of, 1\n\
             \x20 br i1 %occ, label %rehash_insert, label %rehash_cont\n\
             rehash_insert:\n\
             \x20 %okp = getelementptr ptr, ptr %old_keys, i64 %i\n\
             \x20 %ok = load ptr, ptr %okp\n\
             \x20 %ovp = getelementptr i64, ptr %old_vals, i64 %i\n\
             \x20 %ov = load i64, ptr %ovp\n\
             \x20 %slot = call i64 @__yorum_map_find_slot(ptr %map, ptr %ok, i64 %new_cap)\n\
             \x20 %nkp = getelementptr ptr, ptr %new_keys, i64 %slot\n\
             \x20 store ptr %ok, ptr %nkp\n\
             \x20 %nvp = getelementptr i64, ptr %new_vals, i64 %slot\n\
             \x20 store i64 %ov, ptr %nvp\n\
             \x20 %nfp = getelementptr i8, ptr %new_flags, i64 %slot\n\
             \x20 store i8 1, ptr %nfp\n\
             \x20 br label %rehash_cont\n\
             rehash_cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %rehash_loop\n\
             rehash_done:\n\
             \x20 call void @free(ptr %old_keys)\n\
             \x20 call void @free(ptr %old_vals)\n\
             \x20 call void @free(ptr %old_flags)\n\
             \x20 ret void\n\
             }\n\n",
        );

        // map_new — allocate and initialize a new hash map (capacity 16)
        // Map struct layout (48 bytes):
        //   offset 0:  ptr   keys
        //   offset 8:  ptr   vals
        //   offset 16: ptr   flags
        //   offset 24: i64   capacity
        //   offset 32: i64   size (occupied count)
        //   offset 40: i64   tombstones
        self.body.push_str(
            "define ptr @map_new() {\n\
             entry:\n\
             \x20 %map = call ptr @malloc(i64 48)\n\
             \x20 %keys = call ptr @malloc(i64 128)\n\
             \x20 %vals = call ptr @malloc(i64 128)\n\
             \x20 %flags = call ptr @malloc(i64 16)\n\
             \x20 call ptr @memset(ptr %flags, i32 0, i64 16)\n\
             \x20 store ptr %keys, ptr %map\n\
             \x20 %vp = getelementptr i8, ptr %map, i64 8\n\
             \x20 store ptr %vals, ptr %vp\n\
             \x20 %fp = getelementptr i8, ptr %map, i64 16\n\
             \x20 store ptr %flags, ptr %fp\n\
             \x20 %cp = getelementptr i8, ptr %map, i64 24\n\
             \x20 store i64 16, ptr %cp\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 store i64 0, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %map, i64 40\n\
             \x20 store i64 0, ptr %tp\n\
             \x20 ret ptr %map\n\
             }\n\n",
        );

        // map_set — insert or update key-value pair
        // Accepts empty (flag==0) or tombstone (flag==2) slots for insertion.
        // Load factor check includes tombstones to prevent infinite probe loops.
        self.body.push_str(
            "define void @map_set(ptr %map, ptr %key, i64 %val) {\n\
             entry:\n\
             \x20 ; check load factor: (size + tombstones)*4 >= cap*3 → grow\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %map, i64 40\n\
             \x20 %tombs = load i64, ptr %tp\n\
             \x20 %used = add i64 %size, %tombs\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %s4 = mul i64 %used, 4\n\
             \x20 %c3 = mul i64 %cap, 3\n\
             \x20 %need_grow = icmp sge i64 %s4, %c3\n\
             \x20 br i1 %need_grow, label %grow, label %find\n\
             grow:\n\
             \x20 call void @__yorum_map_grow(ptr %map)\n\
             \x20 br label %find\n\
             find:\n\
             \x20 %cap2 = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot(ptr %map, ptr %key, i64 %cap2)\n\
             \x20 ; check if slot is occupied (flag==1 → update) or empty/tombstone (insert)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fslot = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fslot\n\
             \x20 %is_occupied = icmp eq i8 %flag, 1\n\
             \x20 br i1 %is_occupied, label %update, label %insert\n\
             insert:\n\
             \x20 ; copy key string\n\
             \x20 %klen = call i64 @strlen(ptr %key)\n\
             \x20 %kbuf_sz = add i64 %klen, 1\n\
             \x20 %kbuf = call ptr @malloc(i64 %kbuf_sz)\n\
             \x20 call ptr @strcpy(ptr %kbuf, ptr %key)\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %kslot = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 store ptr %kbuf, ptr %kslot\n\
             \x20 store i8 1, ptr %fslot\n\
             \x20 %new_size = add i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 br label %store_val\n\
             update:\n\
             \x20 br label %store_val\n\
             store_val:\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %vslot = getelementptr i64, ptr %vals_p, i64 %slot\n\
             \x20 store i64 %val, ptr %vslot\n\
             \x20 ret void\n\
             }\n\n",
        );

        // map_get — look up value by key (aborts if not found)
        self.body.push_str(
            "define i64 @map_get(ptr %map, ptr %key) {\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot(ptr %map, ptr %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 br i1 %found, label %ok, label %fail\n\
             fail:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.map_key, ptr %key)\n\
             \x20 call void @abort()\n\
             \x20 unreachable\n\
             ok:\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %vp = getelementptr i64, ptr %vals_p, i64 %slot\n\
             \x20 %v = load i64, ptr %vp\n\
             \x20 ret i64 %v\n\
             }\n\n",
        );

        // map_has — check if key exists
        self.body.push_str(
            "define i1 @map_has(ptr %map, ptr %key) {\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot(ptr %map, ptr %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 ret i1 %found\n\
             }\n\n",
        );

        // ── Math builtins ──

        // abs_int — absolute value of int
        self.body.push_str(
            "define i64 @abs_int(i64 %x) {\n\
             entry:\n\
             \x20 %neg = icmp slt i64 %x, 0\n\
             \x20 %pos = sub i64 0, %x\n\
             \x20 %result = select i1 %neg, i64 %pos, i64 %x\n\
             \x20 ret i64 %result\n\
             }\n\n",
        );
        // abs_float — absolute value of float
        self.body.push_str(
            "define double @abs_float(double %x) {\n\
             entry:\n\
             \x20 %result = call double @llvm.fabs.f64(double %x)\n\
             \x20 ret double %result\n\
             }\n\n",
        );
        // min_int
        self.body.push_str(
            "define i64 @min_int(i64 %a, i64 %b) {\n\
             entry:\n\
             \x20 %cmp = icmp slt i64 %a, %b\n\
             \x20 %result = select i1 %cmp, i64 %a, i64 %b\n\
             \x20 ret i64 %result\n\
             }\n\n",
        );
        // max_int
        self.body.push_str(
            "define i64 @max_int(i64 %a, i64 %b) {\n\
             entry:\n\
             \x20 %cmp = icmp sgt i64 %a, %b\n\
             \x20 %result = select i1 %cmp, i64 %a, i64 %b\n\
             \x20 ret i64 %result\n\
             }\n\n",
        );
        // min_float
        self.body.push_str(
            "define double @min_float(double %a, double %b) {\n\
             entry:\n\
             \x20 %result = call double @llvm.minnum.f64(double %a, double %b)\n\
             \x20 ret double %result\n\
             }\n\n",
        );
        // max_float
        self.body.push_str(
            "define double @max_float(double %a, double %b) {\n\
             entry:\n\
             \x20 %result = call double @llvm.maxnum.f64(double %a, double %b)\n\
             \x20 ret double %result\n\
             }\n\n",
        );
        // sqrt, pow, floor, ceil, round — these use LLVM intrinsics and are
        // inlined at call sites (see emit_expr Call handler) to avoid defining
        // wrapper functions whose names clash with C library symbols (e.g.,
        // @pow wrapping @llvm.pow.f64 which LLVM lowers to C's pow → infinite
        // recursion). No wrapper functions emitted here.

        // sin, cos, tan, log, log10, exp — these are just external libm
        // declarations (already in emit_builtin_decls), called directly by the
        // standard call dispatch path. No wrapper functions needed.

        // ── String utility builtins ──

        // str_contains — check if substring exists
        self.body.push_str(
            "define i1 @str_contains(ptr %s, ptr %sub) {\n\
             entry:\n\
             \x20 %p = call ptr @strstr(ptr %s, ptr %sub)\n\
             \x20 %found = icmp ne ptr %p, null\n\
             \x20 ret i1 %found\n\
             }\n\n",
        );
        // str_index_of — find index of substring, returns -1 if not found
        self.body.push_str(
            "define i64 @str_index_of(ptr %s, ptr %sub) {\n\
             entry:\n\
             \x20 %p = call ptr @strstr(ptr %s, ptr %sub)\n\
             \x20 %is_null = icmp eq ptr %p, null\n\
             \x20 br i1 %is_null, label %not_found, label %found\n\
             not_found:\n\
             \x20 ret i64 -1\n\
             found:\n\
             \x20 %si = ptrtoint ptr %s to i64\n\
             \x20 %pi = ptrtoint ptr %p to i64\n\
             \x20 %idx = sub i64 %pi, %si\n\
             \x20 ret i64 %idx\n\
             }\n\n",
        );
        // str_starts_with — check if string starts with prefix
        self.body.push_str(
            "define i1 @str_starts_with(ptr %s, ptr %prefix) {\n\
             entry:\n\
             \x20 %plen = call i64 @strlen(ptr %prefix)\n\
             \x20 %cmp = call i32 @strncmp(ptr %s, ptr %prefix, i64 %plen)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n\
             \x20 ret i1 %eq\n\
             }\n\n",
        );
        // str_ends_with — check if string ends with suffix
        self.body.push_str(
            "define i1 @str_ends_with(ptr %s, ptr %suffix) {\n\
             entry:\n\
             \x20 %slen = call i64 @strlen(ptr %s)\n\
             \x20 %suflen = call i64 @strlen(ptr %suffix)\n\
             \x20 %too_short = icmp slt i64 %slen, %suflen\n\
             \x20 br i1 %too_short, label %no, label %check\n\
             no:\n\
             \x20 ret i1 0\n\
             check:\n\
             \x20 %offset = sub i64 %slen, %suflen\n\
             \x20 %tail = getelementptr i8, ptr %s, i64 %offset\n\
             \x20 %cmp = call i32 @strcmp(ptr %tail, ptr %suffix)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n\
             \x20 ret i1 %eq\n\
             }\n\n",
        );
        // str_trim — trim leading and trailing whitespace
        self.body.push_str(
            "define ptr @str_trim(ptr %s) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 br label %skip_leading\n\
             skip_leading:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %leading_cont ]\n\
             \x20 %done_l = icmp sge i64 %i, %len\n\
             \x20 br i1 %done_l, label %empty, label %check_leading\n\
             check_leading:\n\
             \x20 %cp = getelementptr i8, ptr %s, i64 %i\n\
             \x20 %c = load i8, ptr %cp\n\
             \x20 %is_sp = icmp eq i8 %c, 32\n\
             \x20 %is_tab = icmp eq i8 %c, 9\n\
             \x20 %is_nl = icmp eq i8 %c, 10\n\
             \x20 %is_cr = icmp eq i8 %c, 13\n\
             \x20 %w1 = or i1 %is_sp, %is_tab\n\
             \x20 %w2 = or i1 %w1, %is_nl\n\
             \x20 %is_ws = or i1 %w2, %is_cr\n\
             \x20 br i1 %is_ws, label %leading_cont, label %find_end\n\
             leading_cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %skip_leading\n\
             empty:\n\
             \x20 %e = call ptr @malloc(i64 1)\n\
             \x20 store i8 0, ptr %e\n\
             \x20 ret ptr %e\n\
             find_end:\n\
             \x20 %start = phi i64 [ %i, %check_leading ]\n\
             \x20 %last_init = sub i64 %len, 1\n\
             \x20 br label %skip_trailing\n\
             skip_trailing:\n\
             \x20 %j = phi i64 [ %last_init, %find_end ], [ %j_prev, %trailing_cont ]\n\
             \x20 %done_t = icmp slt i64 %j, %start\n\
             \x20 br i1 %done_t, label %empty, label %check_trailing\n\
             check_trailing:\n\
             \x20 %cp2 = getelementptr i8, ptr %s, i64 %j\n\
             \x20 %c2 = load i8, ptr %cp2\n\
             \x20 %is_sp2 = icmp eq i8 %c2, 32\n\
             \x20 %is_tab2 = icmp eq i8 %c2, 9\n\
             \x20 %is_nl2 = icmp eq i8 %c2, 10\n\
             \x20 %is_cr2 = icmp eq i8 %c2, 13\n\
             \x20 %w3 = or i1 %is_sp2, %is_tab2\n\
             \x20 %w4 = or i1 %w3, %is_nl2\n\
             \x20 %is_ws2 = or i1 %w4, %is_cr2\n\
             \x20 br i1 %is_ws2, label %trailing_cont, label %copy\n\
             trailing_cont:\n\
             \x20 %j_prev = sub i64 %j, 1\n\
             \x20 br label %skip_trailing\n\
             copy:\n\
             \x20 %end = phi i64 [ %j, %check_trailing ]\n\
             \x20 %new_len = sub i64 %end, %start\n\
             \x20 %new_len1 = add i64 %new_len, 1\n\
             \x20 %buf_sz = add i64 %new_len1, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 %src = getelementptr i8, ptr %s, i64 %start\n\
             \x20 call ptr @memcpy(ptr %buf, ptr %src, i64 %new_len1)\n\
             \x20 %term = getelementptr i8, ptr %buf, i64 %new_len1\n\
             \x20 store i8 0, ptr %term\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_replace — replace all occurrences of 'from' with 'to'
        self.body.push_str(
            "define ptr @str_replace(ptr %s, ptr %from, ptr %to) {\n\
             entry:\n\
             \x20 %slen = call i64 @strlen(ptr %s)\n\
             \x20 %flen = call i64 @strlen(ptr %from)\n\
             \x20 %tlen = call i64 @strlen(ptr %to)\n\
             \x20 ; allocate generous buffer: slen * (tlen+1) + 1\n\
             \x20 %max1 = add i64 %tlen, 1\n\
             \x20 %max2 = add i64 %slen, 1\n\
             \x20 %max3 = mul i64 %max2, %max1\n\
             \x20 %buf_sz = add i64 %max3, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 store i8 0, ptr %buf\n\
             \x20 %is_empty_from = icmp eq i64 %flen, 0\n\
             \x20 br i1 %is_empty_from, label %just_copy, label %loop\n\
             just_copy:\n\
             \x20 call ptr @strcpy(ptr %buf, ptr %s)\n\
             \x20 ret ptr %buf\n\
             loop:\n\
             \x20 %cur = phi ptr [ %s, %entry ], [ %after, %replace ]\n\
             \x20 %found = call ptr @strstr(ptr %cur, ptr %from)\n\
             \x20 %is_null = icmp eq ptr %found, null\n\
             \x20 br i1 %is_null, label %done, label %replace\n\
             replace:\n\
             \x20 ; append chars before match\n\
             \x20 %ci = ptrtoint ptr %cur to i64\n\
             \x20 %fi = ptrtoint ptr %found to i64\n\
             \x20 %prefix_len = sub i64 %fi, %ci\n\
             \x20 %blen = call i64 @strlen(ptr %buf)\n\
             \x20 %dst = getelementptr i8, ptr %buf, i64 %blen\n\
             \x20 call ptr @memcpy(ptr %dst, ptr %cur, i64 %prefix_len)\n\
             \x20 %dst_end = getelementptr i8, ptr %dst, i64 %prefix_len\n\
             \x20 store i8 0, ptr %dst_end\n\
             \x20 ; append replacement\n\
             \x20 call ptr @strcat(ptr %buf, ptr %to)\n\
             \x20 %after = getelementptr i8, ptr %found, i64 %flen\n\
             \x20 br label %loop\n\
             done:\n\
             \x20 ; append remaining string\n\
             \x20 call ptr @strcat(ptr %buf, ptr %cur)\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_split — split string by delimiter, returns array of strings
        self.body.push_str(
            "define ptr @str_split(ptr %s, ptr %delim) {\n\
             entry:\n\
             \x20 %dlen = call i64 @strlen(ptr %delim)\n\
             \x20 ; allocate fat pointer { ptr, i64, i64 }\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 ; initial capacity 8\n\
             \x20 %data = call ptr @malloc(i64 64)\n\
             \x20 %d_gep = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %d_gep\n\
             \x20 %l_gep = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %l_gep\n\
             \x20 %c_gep = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 8, ptr %c_gep\n\
             \x20 %is_empty_delim = icmp eq i64 %dlen, 0\n\
             \x20 br i1 %is_empty_delim, label %whole, label %split_loop\n\
             whole:\n\
             \x20 ; empty delimiter: return array with original string\n\
             \x20 %sdup = call i64 @strlen(ptr %s)\n\
             \x20 %sdup_sz = add i64 %sdup, 1\n\
             \x20 %sdup_buf = call ptr @malloc(i64 %sdup_sz)\n\
             \x20 call ptr @strcpy(ptr %sdup_buf, ptr %s)\n\
             \x20 store ptr %sdup_buf, ptr %data\n\
             \x20 store i64 1, ptr %l_gep\n\
             \x20 ret ptr %fat\n\
             split_loop:\n\
             \x20 %cur = phi ptr [ %s, %entry ], [ %next, %store1 ]\n\
             \x20 %found = call ptr @strstr(ptr %cur, ptr %delim)\n\
             \x20 %is_null = icmp eq ptr %found, null\n\
             \x20 br i1 %is_null, label %last_part, label %add_part\n\
             add_part:\n\
             \x20 %ci = ptrtoint ptr %cur to i64\n\
             \x20 %fi = ptrtoint ptr %found to i64\n\
             \x20 %plen = sub i64 %fi, %ci\n\
             \x20 %pbuf_sz = add i64 %plen, 1\n\
             \x20 %pbuf = call ptr @malloc(i64 %pbuf_sz)\n\
             \x20 call ptr @memcpy(ptr %pbuf, ptr %cur, i64 %plen)\n\
             \x20 %pterm = getelementptr i8, ptr %pbuf, i64 %plen\n\
             \x20 store i8 0, ptr %pterm\n\
             \x20 ; push to array (grow if needed)\n\
             \x20 %len1 = load i64, ptr %l_gep\n\
             \x20 %cap1 = load i64, ptr %c_gep\n\
             \x20 %need1 = icmp eq i64 %len1, %cap1\n\
             \x20 br i1 %need1, label %grow1, label %store1\n\
             grow1:\n\
             \x20 %nc1 = shl i64 %cap1, 1\n\
             \x20 %nb1 = mul i64 %nc1, 8\n\
             \x20 %d1 = load ptr, ptr %d_gep\n\
             \x20 %nd1 = call ptr @realloc(ptr %d1, i64 %nb1)\n\
             \x20 store ptr %nd1, ptr %d_gep\n\
             \x20 store i64 %nc1, ptr %c_gep\n\
             \x20 br label %store1\n\
             store1:\n\
             \x20 %d2 = load ptr, ptr %d_gep\n\
             \x20 %slot1 = getelementptr ptr, ptr %d2, i64 %len1\n\
             \x20 store ptr %pbuf, ptr %slot1\n\
             \x20 %nl1 = add i64 %len1, 1\n\
             \x20 store i64 %nl1, ptr %l_gep\n\
             \x20 %next = getelementptr i8, ptr %found, i64 %dlen\n\
             \x20 br label %split_loop\n\
             last_part:\n\
             \x20 %last_cur = phi ptr [ %cur, %split_loop ]\n\
             \x20 %llen = call i64 @strlen(ptr %last_cur)\n\
             \x20 %lbuf_sz = add i64 %llen, 1\n\
             \x20 %lbuf = call ptr @malloc(i64 %lbuf_sz)\n\
             \x20 call ptr @strcpy(ptr %lbuf, ptr %last_cur)\n\
             \x20 ; push last part\n\
             \x20 %len2 = load i64, ptr %l_gep\n\
             \x20 %cap2 = load i64, ptr %c_gep\n\
             \x20 %need2 = icmp eq i64 %len2, %cap2\n\
             \x20 br i1 %need2, label %grow2, label %store2\n\
             grow2:\n\
             \x20 %nc2 = shl i64 %cap2, 1\n\
             \x20 %nb2 = mul i64 %nc2, 8\n\
             \x20 %d3 = load ptr, ptr %d_gep\n\
             \x20 %nd2 = call ptr @realloc(ptr %d3, i64 %nb2)\n\
             \x20 store ptr %nd2, ptr %d_gep\n\
             \x20 store i64 %nc2, ptr %c_gep\n\
             \x20 br label %store2\n\
             store2:\n\
             \x20 %d4 = load ptr, ptr %d_gep\n\
             \x20 %slot2 = getelementptr ptr, ptr %d4, i64 %len2\n\
             \x20 store ptr %lbuf, ptr %slot2\n\
             \x20 %nl2 = add i64 %len2, 1\n\
             \x20 store i64 %nl2, ptr %l_gep\n\
             \x20 ret ptr %fat\n\
             }\n\n",
        );
        // str_to_upper — convert string to uppercase
        self.body.push_str(
            "define ptr @str_to_upper(ptr %s) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 %buf_sz = add i64 %len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cont ]\n\
             \x20 %done = icmp sge i64 %i, %buf_sz\n\
             \x20 br i1 %done, label %end, label %body\n\
             body:\n\
             \x20 %sp = getelementptr i8, ptr %s, i64 %i\n\
             \x20 %c = load i8, ptr %sp\n\
             \x20 %ge_a = icmp sge i8 %c, 97\n\
             \x20 %le_z = icmp sle i8 %c, 122\n\
             \x20 %is_lower = and i1 %ge_a, %le_z\n\
             \x20 %upper = sub i8 %c, 32\n\
             \x20 %out = select i1 %is_lower, i8 %upper, i8 %c\n\
             \x20 %dp = getelementptr i8, ptr %buf, i64 %i\n\
             \x20 store i8 %out, ptr %dp\n\
             \x20 br label %cont\n\
             cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             end:\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_to_lower — convert string to lowercase
        self.body.push_str(
            "define ptr @str_to_lower(ptr %s) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 %buf_sz = add i64 %len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cont ]\n\
             \x20 %done = icmp sge i64 %i, %buf_sz\n\
             \x20 br i1 %done, label %end, label %body\n\
             body:\n\
             \x20 %sp = getelementptr i8, ptr %s, i64 %i\n\
             \x20 %c = load i8, ptr %sp\n\
             \x20 %ge_A = icmp sge i8 %c, 65\n\
             \x20 %le_Z = icmp sle i8 %c, 90\n\
             \x20 %is_upper = and i1 %ge_A, %le_Z\n\
             \x20 %lower = add i8 %c, 32\n\
             \x20 %out = select i1 %is_upper, i8 %lower, i8 %c\n\
             \x20 %dp = getelementptr i8, ptr %buf, i64 %i\n\
             \x20 store i8 %out, ptr %dp\n\
             \x20 br label %cont\n\
             cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             end:\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // str_repeat — repeat string n times
        self.body.push_str(
            "define ptr @str_repeat(ptr %s, i64 %n) {\n\
             entry:\n\
             \x20 %len = call i64 @strlen(ptr %s)\n\
             \x20 %total = mul i64 %len, %n\n\
             \x20 %buf_sz = add i64 %total, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 store i8 0, ptr %buf\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cat ]\n\
             \x20 %done = icmp sge i64 %i, %n\n\
             \x20 br i1 %done, label %end, label %cat\n\
             cat:\n\
             \x20 call ptr @strcat(ptr %buf, ptr %s)\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             end:\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );

        // ── Collection utility builtins ──

        // contains_int — linear scan of [int] array for a value
        self.body.push_str(
            "define i1 @contains_int(ptr %arr, i64 %val) {\n\
             entry:\n\
             \x20 %l_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 1\n\
             \x20 %len = load i64, ptr %l_gep\n\
             \x20 %d_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 0\n\
             \x20 %data = load ptr, ptr %d_gep\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cont ]\n\
             \x20 %done = icmp sge i64 %i, %len\n\
             \x20 br i1 %done, label %not_found, label %check\n\
             check:\n\
             \x20 %ep = getelementptr i64, ptr %data, i64 %i\n\
             \x20 %e = load i64, ptr %ep\n\
             \x20 %eq = icmp eq i64 %e, %val\n\
             \x20 br i1 %eq, label %found, label %cont\n\
             cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             found:\n\
             \x20 ret i1 1\n\
             not_found:\n\
             \x20 ret i1 0\n\
             }\n\n",
        );
        // contains_str — linear scan of [string] array using strcmp
        self.body.push_str(
            "define i1 @contains_str(ptr %arr, ptr %val) {\n\
             entry:\n\
             \x20 %l_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 1\n\
             \x20 %len = load i64, ptr %l_gep\n\
             \x20 %d_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 0\n\
             \x20 %data = load ptr, ptr %d_gep\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %cont ]\n\
             \x20 %done = icmp sge i64 %i, %len\n\
             \x20 br i1 %done, label %not_found, label %check\n\
             check:\n\
             \x20 %ep = getelementptr ptr, ptr %data, i64 %i\n\
             \x20 %e = load ptr, ptr %ep\n\
             \x20 %cmp = call i32 @strcmp(ptr %e, ptr %val)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n\
             \x20 br i1 %eq, label %found, label %cont\n\
             cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             found:\n\
             \x20 ret i1 1\n\
             not_found:\n\
             \x20 ret i1 0\n\
             }\n\n",
        );
        // sort_int — copy array and in-place heap sort, O(n log n)
        // Uses sift-down heapify with alloca-based loop variables
        self.body.push_str(
            "define ptr @sort_int(ptr %arr) {\n\
             entry:\n\
             \x20 %l_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 1\n\
             \x20 %len = load i64, ptr %l_gep\n\
             \x20 %d_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 0\n\
             \x20 %data = load ptr, ptr %d_gep\n\
             \x20 ; copy data\n\
             \x20 %bytes = mul i64 %len, 8\n\
             \x20 %new_data = call ptr @malloc(i64 %bytes)\n\
             \x20 call ptr @memcpy(ptr %new_data, ptr %data, i64 %bytes)\n\
             \x20 ; heap sort phase 1: build max-heap\n\
             \x20 %bi_addr = alloca i64\n\
             \x20 %half = sdiv i64 %len, 2\n\
             \x20 %start = sub i64 %half, 1\n\
             \x20 store i64 %start, ptr %bi_addr\n\
             \x20 br label %build_loop\n\
             build_loop:\n\
             \x20 %bi = load i64, ptr %bi_addr\n\
             \x20 %b_done = icmp slt i64 %bi, 0\n\
             \x20 br i1 %b_done, label %extract_init, label %build_sift\n\
             build_sift:\n\
             \x20 %root_addr = alloca i64\n\
             \x20 store i64 %bi, ptr %root_addr\n\
             \x20 br label %sift_b\n\
             sift_b:\n\
             \x20 %sb_r = load i64, ptr %root_addr\n\
             \x20 %sb_left1 = add i64 %sb_r, %sb_r\n\
             \x20 %sb_left = add i64 %sb_left1, 1\n\
             \x20 %sb_right = add i64 %sb_left, 1\n\
             \x20 ; start with largest = root\n\
             \x20 %lg_addr = alloca i64\n\
             \x20 store i64 %sb_r, ptr %lg_addr\n\
             \x20 ; check left child\n\
             \x20 %sb_lvalid = icmp slt i64 %sb_left, %len\n\
             \x20 br i1 %sb_lvalid, label %sift_b_lcmp, label %sift_b_rchk\n\
             sift_b_lcmp:\n\
             \x20 %sb_lg1 = load i64, ptr %lg_addr\n\
             \x20 %sb_lp = getelementptr i64, ptr %new_data, i64 %sb_left\n\
             \x20 %sb_lv = load i64, ptr %sb_lp\n\
             \x20 %sb_bp = getelementptr i64, ptr %new_data, i64 %sb_lg1\n\
             \x20 %sb_bv = load i64, ptr %sb_bp\n\
             \x20 %sb_lgt = icmp sgt i64 %sb_lv, %sb_bv\n\
             \x20 br i1 %sb_lgt, label %sift_b_lset, label %sift_b_rchk\n\
             sift_b_lset:\n\
             \x20 store i64 %sb_left, ptr %lg_addr\n\
             \x20 br label %sift_b_rchk\n\
             sift_b_rchk:\n\
             \x20 %sb_rvalid = icmp slt i64 %sb_right, %len\n\
             \x20 br i1 %sb_rvalid, label %sift_b_rcmp, label %sift_b_chk\n\
             sift_b_rcmp:\n\
             \x20 %sb_lg2 = load i64, ptr %lg_addr\n\
             \x20 %sb_rp = getelementptr i64, ptr %new_data, i64 %sb_right\n\
             \x20 %sb_rv = load i64, ptr %sb_rp\n\
             \x20 %sb_b3p = getelementptr i64, ptr %new_data, i64 %sb_lg2\n\
             \x20 %sb_b3v = load i64, ptr %sb_b3p\n\
             \x20 %sb_rgt = icmp sgt i64 %sb_rv, %sb_b3v\n\
             \x20 br i1 %sb_rgt, label %sift_b_rset, label %sift_b_chk\n\
             sift_b_rset:\n\
             \x20 store i64 %sb_right, ptr %lg_addr\n\
             \x20 br label %sift_b_chk\n\
             sift_b_chk:\n\
             \x20 %sb_largest = load i64, ptr %lg_addr\n\
             \x20 %sb_r2 = load i64, ptr %root_addr\n\
             \x20 %sb_changed = icmp ne i64 %sb_largest, %sb_r2\n\
             \x20 br i1 %sb_changed, label %sift_b_swap, label %build_cont\n\
             sift_b_swap:\n\
             \x20 %sb_r3 = load i64, ptr %root_addr\n\
             \x20 %sb_rp2 = getelementptr i64, ptr %new_data, i64 %sb_r3\n\
             \x20 %sb_rv2 = load i64, ptr %sb_rp2\n\
             \x20 %sb_lg3 = load i64, ptr %lg_addr\n\
             \x20 %sb_lp2 = getelementptr i64, ptr %new_data, i64 %sb_lg3\n\
             \x20 %sb_lv2 = load i64, ptr %sb_lp2\n\
             \x20 store i64 %sb_lv2, ptr %sb_rp2\n\
             \x20 store i64 %sb_rv2, ptr %sb_lp2\n\
             \x20 store i64 %sb_lg3, ptr %root_addr\n\
             \x20 br label %sift_b\n\
             build_cont:\n\
             \x20 %bi2 = load i64, ptr %bi_addr\n\
             \x20 %bi_next = sub i64 %bi2, 1\n\
             \x20 store i64 %bi_next, ptr %bi_addr\n\
             \x20 br label %build_loop\n\
             extract_init:\n\
             \x20 %end_addr = alloca i64\n\
             \x20 %len_m1 = sub i64 %len, 1\n\
             \x20 store i64 %len_m1, ptr %end_addr\n\
             \x20 br label %extract_loop\n\
             extract_loop:\n\
             \x20 %end = load i64, ptr %end_addr\n\
             \x20 %e_done = icmp sle i64 %end, 0\n\
             \x20 br i1 %e_done, label %build_fat, label %extract_swap\n\
             extract_swap:\n\
             \x20 %e0p = getelementptr i64, ptr %new_data, i64 0\n\
             \x20 %e0v = load i64, ptr %e0p\n\
             \x20 %end2 = load i64, ptr %end_addr\n\
             \x20 %eep = getelementptr i64, ptr %new_data, i64 %end2\n\
             \x20 %eev = load i64, ptr %eep\n\
             \x20 store i64 %eev, ptr %e0p\n\
             \x20 store i64 %e0v, ptr %eep\n\
             \x20 ; sift down [0, end)\n\
             \x20 %se_root = alloca i64\n\
             \x20 store i64 0, ptr %se_root\n\
             \x20 br label %sift_e\n\
             sift_e:\n\
             \x20 %se_r = load i64, ptr %se_root\n\
             \x20 %se_left1 = add i64 %se_r, %se_r\n\
             \x20 %se_left = add i64 %se_left1, 1\n\
             \x20 %se_right = add i64 %se_left, 1\n\
             \x20 %se_lg_addr = alloca i64\n\
             \x20 store i64 %se_r, ptr %se_lg_addr\n\
             \x20 %se_end = load i64, ptr %end_addr\n\
             \x20 %se_lvalid = icmp slt i64 %se_left, %se_end\n\
             \x20 br i1 %se_lvalid, label %sift_e_lcmp, label %sift_e_rchk\n\
             sift_e_lcmp:\n\
             \x20 %se_lg1 = load i64, ptr %se_lg_addr\n\
             \x20 %se_lp = getelementptr i64, ptr %new_data, i64 %se_left\n\
             \x20 %se_lv = load i64, ptr %se_lp\n\
             \x20 %se_bp = getelementptr i64, ptr %new_data, i64 %se_lg1\n\
             \x20 %se_bv = load i64, ptr %se_bp\n\
             \x20 %se_lgt = icmp sgt i64 %se_lv, %se_bv\n\
             \x20 br i1 %se_lgt, label %sift_e_lset, label %sift_e_rchk\n\
             sift_e_lset:\n\
             \x20 store i64 %se_left, ptr %se_lg_addr\n\
             \x20 br label %sift_e_rchk\n\
             sift_e_rchk:\n\
             \x20 %se_end2 = load i64, ptr %end_addr\n\
             \x20 %se_rvalid = icmp slt i64 %se_right, %se_end2\n\
             \x20 br i1 %se_rvalid, label %sift_e_rcmp, label %sift_e_chk\n\
             sift_e_rcmp:\n\
             \x20 %se_lg2 = load i64, ptr %se_lg_addr\n\
             \x20 %se_rp = getelementptr i64, ptr %new_data, i64 %se_right\n\
             \x20 %se_rv = load i64, ptr %se_rp\n\
             \x20 %se_b3p = getelementptr i64, ptr %new_data, i64 %se_lg2\n\
             \x20 %se_b3v = load i64, ptr %se_b3p\n\
             \x20 %se_rgt = icmp sgt i64 %se_rv, %se_b3v\n\
             \x20 br i1 %se_rgt, label %sift_e_rset, label %sift_e_chk\n\
             sift_e_rset:\n\
             \x20 store i64 %se_right, ptr %se_lg_addr\n\
             \x20 br label %sift_e_chk\n\
             sift_e_chk:\n\
             \x20 %se_largest = load i64, ptr %se_lg_addr\n\
             \x20 %se_r2 = load i64, ptr %se_root\n\
             \x20 %se_changed = icmp ne i64 %se_largest, %se_r2\n\
             \x20 br i1 %se_changed, label %sift_e_swap, label %sift_e_done\n\
             sift_e_swap:\n\
             \x20 %se_r3 = load i64, ptr %se_root\n\
             \x20 %se_rp2 = getelementptr i64, ptr %new_data, i64 %se_r3\n\
             \x20 %se_rv2 = load i64, ptr %se_rp2\n\
             \x20 %se_lg3 = load i64, ptr %se_lg_addr\n\
             \x20 %se_lp2 = getelementptr i64, ptr %new_data, i64 %se_lg3\n\
             \x20 %se_lv2 = load i64, ptr %se_lp2\n\
             \x20 store i64 %se_lv2, ptr %se_rp2\n\
             \x20 store i64 %se_rv2, ptr %se_lp2\n\
             \x20 store i64 %se_lg3, ptr %se_root\n\
             \x20 br label %sift_e\n\
             sift_e_done:\n\
             \x20 %end3 = load i64, ptr %end_addr\n\
             \x20 %end_next = sub i64 %end3, 1\n\
             \x20 store i64 %end_next, ptr %end_addr\n\
             \x20 br label %extract_loop\n\
             build_fat:\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %fd = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %new_data, ptr %fd\n\
             \x20 %fl = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 %len, ptr %fl\n\
             \x20 %fc = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %len, ptr %fc\n\
             \x20 ret ptr %fat\n\
             }\n\n",
        );
        // sort_str — copy array and in-place heap sort using strcmp, O(n log n)
        self.body.push_str(
            "define ptr @sort_str(ptr %arr) {\n\
             entry:\n\
             \x20 %l_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 1\n\
             \x20 %len = load i64, ptr %l_gep\n\
             \x20 %d_gep = getelementptr { ptr, i64, i64 }, ptr %arr, i32 0, i32 0\n\
             \x20 %data = load ptr, ptr %d_gep\n\
             \x20 ; copy data\n\
             \x20 %bytes = mul i64 %len, 8\n\
             \x20 %new_data = call ptr @malloc(i64 %bytes)\n\
             \x20 call ptr @memcpy(ptr %new_data, ptr %data, i64 %bytes)\n\
             \x20 ; heap sort phase 1: build max-heap\n\
             \x20 %bi_addr = alloca i64\n\
             \x20 %half = sdiv i64 %len, 2\n\
             \x20 %start = sub i64 %half, 1\n\
             \x20 store i64 %start, ptr %bi_addr\n\
             \x20 br label %build_loop\n\
             build_loop:\n\
             \x20 %bi = load i64, ptr %bi_addr\n\
             \x20 %b_done = icmp slt i64 %bi, 0\n\
             \x20 br i1 %b_done, label %extract_init, label %build_sift\n\
             build_sift:\n\
             \x20 %root_addr = alloca i64\n\
             \x20 store i64 %bi, ptr %root_addr\n\
             \x20 br label %sift_b\n\
             sift_b:\n\
             \x20 %sb_r = load i64, ptr %root_addr\n\
             \x20 %sb_left1 = add i64 %sb_r, %sb_r\n\
             \x20 %sb_left = add i64 %sb_left1, 1\n\
             \x20 %sb_right = add i64 %sb_left, 1\n\
             \x20 %lg_addr = alloca i64\n\
             \x20 store i64 %sb_r, ptr %lg_addr\n\
             \x20 %sb_lvalid = icmp slt i64 %sb_left, %len\n\
             \x20 br i1 %sb_lvalid, label %sift_b_lcmp, label %sift_b_rchk\n\
             sift_b_lcmp:\n\
             \x20 %sb_lg1 = load i64, ptr %lg_addr\n\
             \x20 %sb_lp = getelementptr ptr, ptr %new_data, i64 %sb_left\n\
             \x20 %sb_lv = load ptr, ptr %sb_lp\n\
             \x20 %sb_bp = getelementptr ptr, ptr %new_data, i64 %sb_lg1\n\
             \x20 %sb_bv = load ptr, ptr %sb_bp\n\
             \x20 %sb_cmp = call i32 @strcmp(ptr %sb_lv, ptr %sb_bv)\n\
             \x20 %sb_lgt = icmp sgt i32 %sb_cmp, 0\n\
             \x20 br i1 %sb_lgt, label %sift_b_lset, label %sift_b_rchk\n\
             sift_b_lset:\n\
             \x20 store i64 %sb_left, ptr %lg_addr\n\
             \x20 br label %sift_b_rchk\n\
             sift_b_rchk:\n\
             \x20 %sb_rvalid = icmp slt i64 %sb_right, %len\n\
             \x20 br i1 %sb_rvalid, label %sift_b_rcmp, label %sift_b_chk\n\
             sift_b_rcmp:\n\
             \x20 %sb_lg2 = load i64, ptr %lg_addr\n\
             \x20 %sb_rp = getelementptr ptr, ptr %new_data, i64 %sb_right\n\
             \x20 %sb_rv = load ptr, ptr %sb_rp\n\
             \x20 %sb_b3p = getelementptr ptr, ptr %new_data, i64 %sb_lg2\n\
             \x20 %sb_b3v = load ptr, ptr %sb_b3p\n\
             \x20 %sb_cmp2 = call i32 @strcmp(ptr %sb_rv, ptr %sb_b3v)\n\
             \x20 %sb_rgt = icmp sgt i32 %sb_cmp2, 0\n\
             \x20 br i1 %sb_rgt, label %sift_b_rset, label %sift_b_chk\n\
             sift_b_rset:\n\
             \x20 store i64 %sb_right, ptr %lg_addr\n\
             \x20 br label %sift_b_chk\n\
             sift_b_chk:\n\
             \x20 %sb_largest = load i64, ptr %lg_addr\n\
             \x20 %sb_r2 = load i64, ptr %root_addr\n\
             \x20 %sb_changed = icmp ne i64 %sb_largest, %sb_r2\n\
             \x20 br i1 %sb_changed, label %sift_b_swap, label %build_cont\n\
             sift_b_swap:\n\
             \x20 %sb_r3 = load i64, ptr %root_addr\n\
             \x20 %sb_rp2 = getelementptr ptr, ptr %new_data, i64 %sb_r3\n\
             \x20 %sb_rv2 = load ptr, ptr %sb_rp2\n\
             \x20 %sb_lg3 = load i64, ptr %lg_addr\n\
             \x20 %sb_lp2 = getelementptr ptr, ptr %new_data, i64 %sb_lg3\n\
             \x20 %sb_lv2 = load ptr, ptr %sb_lp2\n\
             \x20 store ptr %sb_lv2, ptr %sb_rp2\n\
             \x20 store ptr %sb_rv2, ptr %sb_lp2\n\
             \x20 store i64 %sb_lg3, ptr %root_addr\n\
             \x20 br label %sift_b\n\
             build_cont:\n\
             \x20 %bi2 = load i64, ptr %bi_addr\n\
             \x20 %bi_next = sub i64 %bi2, 1\n\
             \x20 store i64 %bi_next, ptr %bi_addr\n\
             \x20 br label %build_loop\n\
             extract_init:\n\
             \x20 %end_addr = alloca i64\n\
             \x20 %len_m1 = sub i64 %len, 1\n\
             \x20 store i64 %len_m1, ptr %end_addr\n\
             \x20 br label %extract_loop\n\
             extract_loop:\n\
             \x20 %end = load i64, ptr %end_addr\n\
             \x20 %e_done = icmp sle i64 %end, 0\n\
             \x20 br i1 %e_done, label %build_fat, label %extract_swap\n\
             extract_swap:\n\
             \x20 %e0p = getelementptr ptr, ptr %new_data, i64 0\n\
             \x20 %e0v = load ptr, ptr %e0p\n\
             \x20 %end2 = load i64, ptr %end_addr\n\
             \x20 %eep = getelementptr ptr, ptr %new_data, i64 %end2\n\
             \x20 %eev = load ptr, ptr %eep\n\
             \x20 store ptr %eev, ptr %e0p\n\
             \x20 store ptr %e0v, ptr %eep\n\
             \x20 %se_root = alloca i64\n\
             \x20 store i64 0, ptr %se_root\n\
             \x20 br label %sift_e\n\
             sift_e:\n\
             \x20 %se_r = load i64, ptr %se_root\n\
             \x20 %se_left1 = add i64 %se_r, %se_r\n\
             \x20 %se_left = add i64 %se_left1, 1\n\
             \x20 %se_right = add i64 %se_left, 1\n\
             \x20 %se_lg_addr = alloca i64\n\
             \x20 store i64 %se_r, ptr %se_lg_addr\n\
             \x20 %se_end = load i64, ptr %end_addr\n\
             \x20 %se_lvalid = icmp slt i64 %se_left, %se_end\n\
             \x20 br i1 %se_lvalid, label %sift_e_lcmp, label %sift_e_rchk\n\
             sift_e_lcmp:\n\
             \x20 %se_lg1 = load i64, ptr %se_lg_addr\n\
             \x20 %se_lp = getelementptr ptr, ptr %new_data, i64 %se_left\n\
             \x20 %se_lv = load ptr, ptr %se_lp\n\
             \x20 %se_bp = getelementptr ptr, ptr %new_data, i64 %se_lg1\n\
             \x20 %se_bv = load ptr, ptr %se_bp\n\
             \x20 %se_cmp = call i32 @strcmp(ptr %se_lv, ptr %se_bv)\n\
             \x20 %se_lgt = icmp sgt i32 %se_cmp, 0\n\
             \x20 br i1 %se_lgt, label %sift_e_lset, label %sift_e_rchk\n\
             sift_e_lset:\n\
             \x20 store i64 %se_left, ptr %se_lg_addr\n\
             \x20 br label %sift_e_rchk\n\
             sift_e_rchk:\n\
             \x20 %se_end2 = load i64, ptr %end_addr\n\
             \x20 %se_rvalid = icmp slt i64 %se_right, %se_end2\n\
             \x20 br i1 %se_rvalid, label %sift_e_rcmp, label %sift_e_chk\n\
             sift_e_rcmp:\n\
             \x20 %se_lg2 = load i64, ptr %se_lg_addr\n\
             \x20 %se_rp = getelementptr ptr, ptr %new_data, i64 %se_right\n\
             \x20 %se_rv = load ptr, ptr %se_rp\n\
             \x20 %se_b3p = getelementptr ptr, ptr %new_data, i64 %se_lg2\n\
             \x20 %se_b3v = load ptr, ptr %se_b3p\n\
             \x20 %se_cmp2 = call i32 @strcmp(ptr %se_rv, ptr %se_b3v)\n\
             \x20 %se_rgt = icmp sgt i32 %se_cmp2, 0\n\
             \x20 br i1 %se_rgt, label %sift_e_rset, label %sift_e_chk\n\
             sift_e_rset:\n\
             \x20 store i64 %se_right, ptr %se_lg_addr\n\
             \x20 br label %sift_e_chk\n\
             sift_e_chk:\n\
             \x20 %se_largest = load i64, ptr %se_lg_addr\n\
             \x20 %se_r2 = load i64, ptr %se_root\n\
             \x20 %se_changed = icmp ne i64 %se_largest, %se_r2\n\
             \x20 br i1 %se_changed, label %sift_e_swap, label %sift_e_done\n\
             sift_e_swap:\n\
             \x20 %se_r3 = load i64, ptr %se_root\n\
             \x20 %se_rp2 = getelementptr ptr, ptr %new_data, i64 %se_r3\n\
             \x20 %se_rv2 = load ptr, ptr %se_rp2\n\
             \x20 %se_lg3 = load i64, ptr %se_lg_addr\n\
             \x20 %se_lp2 = getelementptr ptr, ptr %new_data, i64 %se_lg3\n\
             \x20 %se_lv2 = load ptr, ptr %se_lp2\n\
             \x20 store ptr %se_lv2, ptr %se_rp2\n\
             \x20 store ptr %se_rv2, ptr %se_lp2\n\
             \x20 store i64 %se_lg3, ptr %se_root\n\
             \x20 br label %sift_e\n\
             sift_e_done:\n\
             \x20 %end3 = load i64, ptr %end_addr\n\
             \x20 %end_next = sub i64 %end3, 1\n\
             \x20 store i64 %end_next, ptr %end_addr\n\
             \x20 br label %extract_loop\n\
             build_fat:\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %fd = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %new_data, ptr %fd\n\
             \x20 %fl = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 %len, ptr %fl\n\
             \x20 %fc = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %len, ptr %fc\n\
             \x20 ret ptr %fat\n\
             }\n\n",
        );

        // ── Map utility builtins ──

        // map_size — load size field from map struct
        self.body.push_str(
            "define i64 @map_size(ptr %map) {\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 ret i64 %size\n\
             }\n\n",
        );
        // map_remove — find and mark slot as tombstone (flag=2), decrement size
        // Uses tombstone to preserve linear probing chains.
        self.body.push_str(
            "define i1 @map_remove(ptr %map, ptr %key) {\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot(ptr %map, ptr %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 br i1 %found, label %remove, label %done\n\
             remove:\n\
             \x20 store i8 2, ptr %fp\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %new_size = sub i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 %tomb_p = getelementptr i8, ptr %map, i64 40\n\
             \x20 %tombs = load i64, ptr %tomb_p\n\
             \x20 %new_tombs = add i64 %tombs, 1\n\
             \x20 store i64 %new_tombs, ptr %tomb_p\n\
             \x20 ; free the key string\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %kp = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 %k = load ptr, ptr %kp\n\
             \x20 call void @free(ptr %k)\n\
             \x20 ret i1 1\n\
             done:\n\
             \x20 ret i1 0\n\
             }\n\n",
        );
        // map_keys — collect all keys into a [string] array
        self.body.push_str(
            "define ptr @map_keys(ptr %map) {\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 ; allocate result array\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %dbytes = mul i64 %size, 8\n\
             \x20 %data = call ptr @malloc(i64 %dbytes)\n\
             \x20 %fd = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %fd\n\
             \x20 %fl = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %fl\n\
             \x20 %fc = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %size, ptr %fc\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %loop_cont ]\n\
             \x20 %out_idx = phi i64 [ 0, %entry ], [ %out_next, %loop_cont ]\n\
             \x20 %done = icmp sge i64 %i, %cap\n\
             \x20 br i1 %done, label %finish, label %check\n\
             check:\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %i\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %occ = icmp eq i8 %flag, 1\n\
             \x20 br i1 %occ, label %copy_key, label %skip\n\
             copy_key:\n\
             \x20 %kp = getelementptr ptr, ptr %keys_p, i64 %i\n\
             \x20 %k = load ptr, ptr %kp\n\
             \x20 %dp = getelementptr ptr, ptr %data, i64 %out_idx\n\
             \x20 store ptr %k, ptr %dp\n\
             \x20 %out_inc = add i64 %out_idx, 1\n\
             \x20 br label %loop_cont\n\
             skip:\n\
             \x20 br label %loop_cont\n\
             loop_cont:\n\
             \x20 %out_next = phi i64 [ %out_inc, %copy_key ], [ %out_idx, %skip ]\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             finish:\n\
             \x20 store i64 %out_idx, ptr %fl\n\
             \x20 ret ptr %fat\n\
             }\n\n",
        );
        // map_values — collect all values into an [int] array
        self.body.push_str(
            "define ptr @map_values(ptr %map) {\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 ; allocate result array\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %dbytes = mul i64 %size, 8\n\
             \x20 %data = call ptr @malloc(i64 %dbytes)\n\
             \x20 %fd = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %fd\n\
             \x20 %fl = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %fl\n\
             \x20 %fc = getelementptr { ptr, i64, i64 }, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %size, ptr %fc\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %loop_cont ]\n\
             \x20 %out_idx = phi i64 [ 0, %entry ], [ %out_next, %loop_cont ]\n\
             \x20 %done = icmp sge i64 %i, %cap\n\
             \x20 br i1 %done, label %finish, label %check\n\
             check:\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %i\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %occ = icmp eq i8 %flag, 1\n\
             \x20 br i1 %occ, label %copy_val, label %skip\n\
             copy_val:\n\
             \x20 %vp = getelementptr i64, ptr %vals_p, i64 %i\n\
             \x20 %v = load i64, ptr %vp\n\
             \x20 %dp = getelementptr i64, ptr %data, i64 %out_idx\n\
             \x20 store i64 %v, ptr %dp\n\
             \x20 %out_inc = add i64 %out_idx, 1\n\
             \x20 br label %loop_cont\n\
             skip:\n\
             \x20 br label %loop_cont\n\
             loop_cont:\n\
             \x20 %out_next = phi i64 [ %out_inc, %copy_val ], [ %out_idx, %skip ]\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             finish:\n\
             \x20 store i64 %out_idx, ptr %fl\n\
             \x20 ret ptr %fat\n\
             }\n\n",
        );

        // ── Map<string, int> aliases for monomorphized names ──
        // The hardcoded helpers above use unmangled names (map_new, map_set, etc.)
        // After monomorphization, calls use mangled names (map_new__string__int, etc.)
        // Emit LLVM aliases so the mangled names resolve to the existing definitions.
        let map_fns = [
            ("map_new", "map_new__string__int", "ptr ()"),
            ("map_set", "map_set__string__int", "void (ptr, ptr, i64)"),
            ("map_get", "map_get__string__int", "i64 (ptr, ptr)"),
            ("map_has", "map_has__string__int", "i1 (ptr, ptr)"),
            ("map_size", "map_size__string__int", "i64 (ptr)"),
            ("map_remove", "map_remove__string__int", "i1 (ptr, ptr)"),
            ("map_keys", "map_keys__string__int", "ptr (ptr)"),
            ("map_values", "map_values__string__int", "ptr (ptr)"),
        ];
        for (orig, mangled, _ty) in &map_fns {
            self.body
                .push_str(&format!("@{} = alias ptr, ptr @{}\n", mangled, orig));
        }
        self.body.push('\n');

        // ── Enhanced I/O builtins ──

        // file_exists — check if file exists using access()
        self.body.push_str(
            "define i1 @file_exists(ptr %path) {\n\
             entry:\n\
             \x20 %rc = call i32 @access(ptr %path, i32 0)\n\
             \x20 %ok = icmp eq i32 %rc, 0\n\
             \x20 ret i1 %ok\n\
             }\n\n",
        );
        // file_append — append content to file
        self.body.push_str(
            "define i1 @file_append(ptr %path, ptr %content) {\n\
             entry:\n\
             \x20 %f = call ptr @fopen(ptr %path, ptr @.str.a)\n\
             \x20 %is_null = icmp eq ptr %f, null\n\
             \x20 br i1 %is_null, label %fail, label %opened\n\
             fail:\n\
             \x20 ret i1 0\n\
             opened:\n\
             \x20 %len = call i64 @strlen(ptr %content)\n\
             \x20 %written = call i64 @fwrite(ptr %content, i64 1, i64 %len, ptr %f)\n\
             \x20 call i32 @fclose(ptr %f)\n\
             \x20 %ok = icmp eq i64 %written, %len\n\
             \x20 ret i1 %ok\n\
             }\n\n",
        );
        // read_line — read a line from stdin (up to 4096 chars)
        let stdin_sym = if cfg!(target_os = "macos") {
            "@__stdinp"
        } else {
            "@stdin"
        };
        self.body.push_str(&format!(
            "define ptr @read_line() {{\n\
             entry:\n\
             \x20 %buf = call ptr @malloc(i64 4096)\n\
             \x20 %sin = load ptr, ptr {stdin_sym}\n\
             \x20 %result = call ptr @fgets(ptr %buf, i32 4096, ptr %sin)\n\
             \x20 %is_null = icmp eq ptr %result, null\n\
             \x20 br i1 %is_null, label %eof, label %got_line\n\
             eof:\n\
             \x20 store i8 0, ptr %buf\n\
             \x20 ret ptr %buf\n\
             got_line:\n\
             \x20 ; strip trailing newline if present\n\
             \x20 %len = call i64 @strlen(ptr %buf)\n\
             \x20 %has_len = icmp sgt i64 %len, 0\n\
             \x20 br i1 %has_len, label %check_nl, label %done\n\
             check_nl:\n\
             \x20 %last_idx = sub i64 %len, 1\n\
             \x20 %last_p = getelementptr i8, ptr %buf, i64 %last_idx\n\
             \x20 %last_c = load i8, ptr %last_p\n\
             \x20 %is_nl = icmp eq i8 %last_c, 10\n\
             \x20 br i1 %is_nl, label %strip_nl, label %done\n\
             strip_nl:\n\
             \x20 store i8 0, ptr %last_p\n\
             \x20 br label %done\n\
             done:\n\
             \x20 ret ptr %buf\n\
             }}\n\n",
            stdin_sym = stdin_sym,
        ));
        // print_flush — print string without newline and flush
        self.body.push_str(
            "define void @print_flush(ptr %s) {\n\
             entry:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.str, ptr %s)\n\
             \x20 call i32 @fflush(ptr null)\n\
             \x20 ret void\n\
             }\n\n",
        );
        // env_get — get environment variable, returns "" if not set
        self.body.push_str(
            "define ptr @env_get(ptr %name) {\n\
             entry:\n\
             \x20 %val = call ptr @getenv(ptr %name)\n\
             \x20 %is_null = icmp eq ptr %val, null\n\
             \x20 br i1 %is_null, label %not_set, label %found\n\
             not_set:\n\
             \x20 %empty = call ptr @malloc(i64 1)\n\
             \x20 store i8 0, ptr %empty\n\
             \x20 ret ptr %empty\n\
             found:\n\
             \x20 ; copy the value to a heap buffer (getenv result is not ours to keep)\n\
             \x20 %len = call i64 @strlen(ptr %val)\n\
             \x20 %buf_sz = add i64 %len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 call ptr @strcpy(ptr %buf, ptr %val)\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );
        // time_ms — get current time in milliseconds using gettimeofday
        // gettimeofday fills a { i64 sec, i64 usec } struct (on 64-bit)
        self.body.push_str(
            "define i64 @time_ms() {\n\
             entry:\n\
             \x20 %tv = alloca { i64, i64 }\n\
             \x20 call i32 @gettimeofday(ptr %tv, ptr null)\n\
             \x20 %sec_p = getelementptr { i64, i64 }, ptr %tv, i32 0, i32 0\n\
             \x20 %sec = load i64, ptr %sec_p\n\
             \x20 %usec_p = getelementptr { i64, i64 }, ptr %tv, i32 0, i32 1\n\
             \x20 %usec = load i64, ptr %usec_p\n\
             \x20 %ms_sec = mul i64 %sec, 1000\n\
             \x20 %ms_usec = sdiv i64 %usec, 1000\n\
             \x20 %ms = add i64 %ms_sec, %ms_usec\n\
             \x20 ret i64 %ms\n\
             }\n\n",
        );

        self.emit_networking_helpers();
    }

    fn emit_networking_helpers(&mut self) {
        // Platform-specific constants
        let sol_socket = if cfg!(target_os = "macos") {
            "65535"
        } else {
            "1"
        };
        let so_reuseaddr = if cfg!(target_os = "macos") { "4" } else { "2" };

        // ── sockaddr_in filling ──
        // macOS: { i8 len=16, i8 family=2, i16 port, i32 addr, [8 x i8] zero } = 16 bytes
        // Linux: { i16 family=2, i16 port, i32 addr, [8 x i8] zero } = 16 bytes
        //
        // We use a flat [16 x i8] buffer and store bytes directly for portability.

        // tcp_connect(host: string, port: int) -> int
        // Uses getaddrinfo for host resolution, then socket + connect
        self.body.push_str(
            "define i64 @tcp_connect(ptr %host, i64 %port) {\n\
             entry:\n\
             \x20 %port_buf = alloca [16 x i8]\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %port_buf, i64 16, ptr @.fmt.lld, i64 %port)\n\
             \x20 %hints = alloca [48 x i8]\n\
             \x20 call ptr @memset(ptr %hints, i32 0, i64 48)\n\
             \x20 %ai_family_p = getelementptr i8, ptr %hints, i64 4\n\
             \x20 store i32 0, ptr %ai_family_p\n\
             \x20 %ai_socktype_p = getelementptr i8, ptr %hints, i64 8\n\
             \x20 store i32 1, ptr %ai_socktype_p\n\
             \x20 %result_p = alloca ptr\n\
             \x20 %gai = call i32 @getaddrinfo(ptr %host, ptr %port_buf, ptr %hints, ptr %result_p)\n\
             \x20 %gai_fail = icmp ne i32 %gai, 0\n\
             \x20 br i1 %gai_fail, label %fail, label %resolved\n\
             resolved:\n\
             \x20 %res = load ptr, ptr %result_p\n\
             \x20 %ai_family_res_p = getelementptr i8, ptr %res, i64 4\n\
             \x20 %ai_family_val = load i32, ptr %ai_family_res_p\n\
             \x20 %ai_socktype_res_p = getelementptr i8, ptr %res, i64 8\n\
             \x20 %ai_socktype_val = load i32, ptr %ai_socktype_res_p\n\
             \x20 %ai_protocol_p = getelementptr i8, ptr %res, i64 12\n\
             \x20 %ai_protocol_val = load i32, ptr %ai_protocol_p\n\
             \x20 %fd = call i32 @socket(i32 %ai_family_val, i32 %ai_socktype_val, i32 %ai_protocol_val)\n\
             \x20 %fd64 = sext i32 %fd to i64\n\
             \x20 %fd_fail = icmp slt i32 %fd, 0\n\
             \x20 br i1 %fd_fail, label %free_fail, label %do_connect\n\
             do_connect:\n\
             \x20 %ai_addrlen_p = getelementptr i8, ptr %res, i64 16\n\
             \x20 %ai_addrlen = load i32, ptr %ai_addrlen_p\n\
             \x20 %ai_addr_p = getelementptr i8, ptr %res, i64 32\n\
             \x20 %ai_addr = load ptr, ptr %ai_addr_p\n\
             \x20 %conn = call i32 @connect(i32 %fd, ptr %ai_addr, i32 %ai_addrlen)\n\
             \x20 call void @freeaddrinfo(ptr %res)\n\
             \x20 %conn_fail = icmp ne i32 %conn, 0\n\
             \x20 br i1 %conn_fail, label %close_fail, label %success\n\
             success:\n\
             \x20 ret i64 %fd64\n\
             close_fail:\n\
             \x20 call i32 @close(i32 %fd)\n\
             \x20 ret i64 -1\n\
             free_fail:\n\
             \x20 call void @freeaddrinfo(ptr %res)\n\
             \x20 ret i64 -1\n\
             fail:\n\
             \x20 ret i64 -1\n\
             }\n\n",
        );

        // tcp_listen(host: string, port: int) -> int
        self.body.push_str(&format!(
            "define i64 @tcp_listen(ptr %host, i64 %port) {{\n\
             entry:\n\
             \x20 %fd = call i32 @socket(i32 2, i32 1, i32 0)\n\
             \x20 %fd64 = sext i32 %fd to i64\n\
             \x20 %fd_fail = icmp slt i32 %fd, 0\n\
             \x20 br i1 %fd_fail, label %fail, label %set_reuse\n\
             set_reuse:\n\
             \x20 %one = alloca i32\n\
             \x20 store i32 1, ptr %one\n\
             \x20 call i32 @setsockopt(i32 %fd, i32 {sol_socket}, i32 {so_reuseaddr}, ptr %one, i32 4)\n\
             \x20 %addr = alloca [16 x i8]\n\
             \x20 call ptr @memset(ptr %addr, i32 0, i64 16)\n\
             {sockaddr_fill}\
             \x20 %port32 = trunc i64 %port to i32\n\
             \x20 %port_n = call i32 @__yorum_htons(i32 %port32)\n\
             \x20 %port16 = trunc i32 %port_n to i16\n\
             \x20 %port_p = getelementptr i8, ptr %addr, i64 {port_offset}\n\
             \x20 store i16 %port16, ptr %port_p\n\
             \x20 %bind_r = call i32 @bind(i32 %fd, ptr %addr, i32 16)\n\
             \x20 %bind_fail = icmp ne i32 %bind_r, 0\n\
             \x20 br i1 %bind_fail, label %close_fail, label %do_listen\n\
             do_listen:\n\
             \x20 %listen_r = call i32 @listen(i32 %fd, i32 128)\n\
             \x20 %listen_fail = icmp ne i32 %listen_r, 0\n\
             \x20 br i1 %listen_fail, label %close_fail, label %success\n\
             success:\n\
             \x20 ret i64 %fd64\n\
             close_fail:\n\
             \x20 call i32 @close(i32 %fd)\n\
             \x20 ret i64 -1\n\
             fail:\n\
             \x20 ret i64 -1\n\
             }}\n\n",
            sol_socket = sol_socket,
            so_reuseaddr = so_reuseaddr,
            sockaddr_fill = if cfg!(target_os = "macos") {
                "\x20 %len_p = getelementptr i8, ptr %addr, i64 0\n\
                 \x20 store i8 16, ptr %len_p\n\
                 \x20 %fam_p = getelementptr i8, ptr %addr, i64 1\n\
                 \x20 store i8 2, ptr %fam_p\n"
            } else {
                "\x20 %fam_p = getelementptr i8, ptr %addr, i64 0\n\
                 \x20 store i16 2, ptr %fam_p\n"
            },
            port_offset = 2,
        ));

        // tcp_accept(fd: int) -> int
        self.body.push_str(
            "define i64 @tcp_accept(i64 %fd) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %client = call i32 @accept(i32 %fd32, ptr null, ptr null)\n\
             \x20 %client64 = sext i32 %client to i64\n\
             \x20 ret i64 %client64\n\
             }\n\n",
        );

        // tcp_send(fd: int, data: string) -> int
        self.body.push_str(
            "define i64 @tcp_send(i64 %fd, ptr %data) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %len = call i64 @strlen(ptr %data)\n\
             \x20 %sent = call i64 @write(i32 %fd32, ptr %data, i64 %len)\n\
             \x20 ret i64 %sent\n\
             }\n\n",
        );

        // tcp_recv(fd: int, max_len: int) -> string
        self.body.push_str(
            "define ptr @tcp_recv(i64 %fd, i64 %max_len) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %buf_sz = add i64 %max_len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 %n = call i64 @read(i32 %fd32, ptr %buf, i64 %max_len)\n\
             \x20 %fail = icmp sle i64 %n, 0\n\
             \x20 br i1 %fail, label %empty, label %ok\n\
             ok:\n\
             \x20 %end_p = getelementptr i8, ptr %buf, i64 %n\n\
             \x20 store i8 0, ptr %end_p\n\
             \x20 ret ptr %buf\n\
             empty:\n\
             \x20 store i8 0, ptr %buf\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );

        // tcp_close(fd: int) -> unit
        self.body.push_str(
            "define void @tcp_close(i64 %fd) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 call i32 @close(i32 %fd32)\n\
             \x20 ret void\n\
             }\n\n",
        );

        // udp_socket() -> int
        self.body.push_str(
            "define i64 @udp_socket() {\n\
             entry:\n\
             \x20 %fd = call i32 @socket(i32 2, i32 2, i32 0)\n\
             \x20 %fd64 = sext i32 %fd to i64\n\
             \x20 ret i64 %fd64\n\
             }\n\n",
        );

        // udp_bind(fd: int, host: string, port: int) -> int
        self.body.push_str(&format!(
            "define i64 @udp_bind(i64 %fd, ptr %host, i64 %port) {{\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %addr = alloca [16 x i8]\n\
             \x20 call ptr @memset(ptr %addr, i32 0, i64 16)\n\
             {sockaddr_fill}\
             \x20 %port32 = trunc i64 %port to i32\n\
             \x20 %port_n = call i32 @__yorum_htons(i32 %port32)\n\
             \x20 %port16 = trunc i32 %port_n to i16\n\
             \x20 %port_p = getelementptr i8, ptr %addr, i64 2\n\
             \x20 store i16 %port16, ptr %port_p\n\
             \x20 %r = call i32 @bind(i32 %fd32, ptr %addr, i32 16)\n\
             \x20 %r64 = sext i32 %r to i64\n\
             \x20 ret i64 %r64\n\
             }}\n\n",
            sockaddr_fill = if cfg!(target_os = "macos") {
                "\x20 %len_p = getelementptr i8, ptr %addr, i64 0\n\
                 \x20 store i8 16, ptr %len_p\n\
                 \x20 %fam_p = getelementptr i8, ptr %addr, i64 1\n\
                 \x20 store i8 2, ptr %fam_p\n"
            } else {
                "\x20 %fam_p = getelementptr i8, ptr %addr, i64 0\n\
                 \x20 store i16 2, ptr %fam_p\n"
            },
        ));

        // udp_send_to(fd: int, data: string, host: string, port: int) -> int
        // Uses getaddrinfo for host resolution
        self.body.push_str(
            "define i64 @udp_send_to(i64 %fd, ptr %data, ptr %host, i64 %port) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %port_buf = alloca [16 x i8]\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %port_buf, i64 16, ptr @.fmt.lld, i64 %port)\n\
             \x20 %hints = alloca [48 x i8]\n\
             \x20 call ptr @memset(ptr %hints, i32 0, i64 48)\n\
             \x20 %ai_family_p = getelementptr i8, ptr %hints, i64 4\n\
             \x20 store i32 0, ptr %ai_family_p\n\
             \x20 %ai_socktype_p = getelementptr i8, ptr %hints, i64 8\n\
             \x20 store i32 2, ptr %ai_socktype_p\n\
             \x20 %result_p = alloca ptr\n\
             \x20 %gai = call i32 @getaddrinfo(ptr %host, ptr %port_buf, ptr %hints, ptr %result_p)\n\
             \x20 %gai_fail = icmp ne i32 %gai, 0\n\
             \x20 br i1 %gai_fail, label %fail, label %resolved\n\
             resolved:\n\
             \x20 %res = load ptr, ptr %result_p\n\
             \x20 %ai_addrlen_p = getelementptr i8, ptr %res, i64 16\n\
             \x20 %ai_addrlen = load i32, ptr %ai_addrlen_p\n\
             \x20 %ai_addr_p = getelementptr i8, ptr %res, i64 32\n\
             \x20 %ai_addr = load ptr, ptr %ai_addr_p\n\
             \x20 %len = call i64 @strlen(ptr %data)\n\
             \x20 %sent = call i64 @sendto(i32 %fd32, ptr %data, i64 %len, i32 0, ptr %ai_addr, i32 %ai_addrlen)\n\
             \x20 call void @freeaddrinfo(ptr %res)\n\
             \x20 ret i64 %sent\n\
             fail:\n\
             \x20 ret i64 -1\n\
             }\n\n",
        );

        // udp_recv_from(fd: int, max_len: int) -> string
        self.body.push_str(
            "define ptr @udp_recv_from(i64 %fd, i64 %max_len) {\n\
             entry:\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 %buf_sz = add i64 %max_len, 1\n\
             \x20 %buf = call ptr @malloc(i64 %buf_sz)\n\
             \x20 %n = call i64 @recvfrom(i32 %fd32, ptr %buf, i64 %max_len, i32 0, ptr null, ptr null)\n\
             \x20 %fail = icmp sle i64 %n, 0\n\
             \x20 br i1 %fail, label %empty, label %ok\n\
             ok:\n\
             \x20 %end_p = getelementptr i8, ptr %buf, i64 %n\n\
             \x20 store i8 0, ptr %end_p\n\
             \x20 ret ptr %buf\n\
             empty:\n\
             \x20 store i8 0, ptr %buf\n\
             \x20 ret ptr %buf\n\
             }\n\n",
        );

        // dns_resolve(hostname: string) -> string
        self.body.push_str(
            "define ptr @dns_resolve(ptr %hostname) {\n\
             entry:\n\
             \x20 %hints = alloca [48 x i8]\n\
             \x20 call ptr @memset(ptr %hints, i32 0, i64 48)\n\
             \x20 %ai_family_p = getelementptr i8, ptr %hints, i64 4\n\
             \x20 store i32 2, ptr %ai_family_p\n\
             \x20 %ai_socktype_p = getelementptr i8, ptr %hints, i64 8\n\
             \x20 store i32 1, ptr %ai_socktype_p\n\
             \x20 %result_p = alloca ptr\n\
             \x20 %gai = call i32 @getaddrinfo(ptr %hostname, ptr null, ptr %hints, ptr %result_p)\n\
             \x20 %gai_fail = icmp ne i32 %gai, 0\n\
             \x20 br i1 %gai_fail, label %fail, label %resolved\n\
             resolved:\n\
             \x20 %res = load ptr, ptr %result_p\n\
             \x20 %ai_addr_p = getelementptr i8, ptr %res, i64 32\n\
             \x20 %ai_addr = load ptr, ptr %ai_addr_p\n\
             \x20 %sin_addr_p = getelementptr i8, ptr %ai_addr, i64 4\n\
             \x20 %buf = call ptr @malloc(i64 46)\n\
             \x20 %r = call ptr @inet_ntop(i32 2, ptr %sin_addr_p, ptr %buf, i32 46)\n\
             \x20 call void @freeaddrinfo(ptr %res)\n\
             \x20 %ntop_fail = icmp eq ptr %r, null\n\
             \x20 br i1 %ntop_fail, label %fail_free, label %success\n\
             success:\n\
             \x20 ret ptr %buf\n\
             fail_free:\n\
             \x20 call void @free(ptr %buf)\n\
             \x20 br label %fail\n\
             fail:\n\
             \x20 %empty = call ptr @malloc(i64 1)\n\
             \x20 store i8 0, ptr %empty\n\
             \x20 ret ptr %empty\n\
             }\n\n",
        );

        // __yorum_parse_url — internal helper, not user-facing
        // Parses "http://host:port/path" or "http://host/path"
        // Stores host, port, path via pointer params
        // Returns 0 on success, -1 on failure
        self.body.push_str(
            "define i32 @__yorum_parse_url(ptr %url, ptr %host_out, ptr %port_out, ptr %path_out) {\n\
             entry:\n\
             \x20 %url_len = call i64 @strlen(ptr %url)\n\
             \x20 ; Skip past \"http://\" (7 chars) or \"https://\" (8 chars)\n\
             \x20 %c0 = getelementptr i8, ptr %url, i64 4\n\
             \x20 %c0v = load i8, ptr %c0\n\
             \x20 %is_s = icmp eq i8 %c0v, 115\n\
             \x20 %skip = select i1 %is_s, i64 8, i64 7\n\
             \x20 %start = getelementptr i8, ptr %url, i64 %skip\n\
             \x20 %rest_len = sub i64 %url_len, %skip\n\
             \x20 ; Find '/' in rest to separate host:port from path\n\
             \x20 %slash_idx = call i64 @__yorum_find_char(ptr %start, i64 %rest_len, i8 47)\n\
             \x20 %no_slash = icmp slt i64 %slash_idx, 0\n\
             \x20 %host_part_len = select i1 %no_slash, i64 %rest_len, i64 %slash_idx\n\
             \x20 ; Extract path\n\
             \x20 br i1 %no_slash, label %default_path, label %has_path\n\
             has_path:\n\
             \x20 %path_start = getelementptr i8, ptr %start, i64 %slash_idx\n\
             \x20 %path_len = sub i64 %rest_len, %slash_idx\n\
             \x20 %path_buf = call ptr @malloc(i64 %path_len)\n\
             \x20 %path_len_plus1 = add i64 %path_len, 1\n\
             \x20 call ptr @memcpy(ptr %path_buf, ptr %path_start, i64 %path_len_plus1)\n\
             \x20 store ptr %path_buf, ptr %path_out\n\
             \x20 br label %parse_host\n\
             default_path:\n\
             \x20 %def_path = call ptr @malloc(i64 2)\n\
             \x20 store i8 47, ptr %def_path\n\
             \x20 %dp1 = getelementptr i8, ptr %def_path, i64 1\n\
             \x20 store i8 0, ptr %dp1\n\
             \x20 store ptr %def_path, ptr %path_out\n\
             \x20 br label %parse_host\n\
             parse_host:\n\
             \x20 ; Find ':' in host part to separate host from port\n\
             \x20 %colon_idx = call i64 @__yorum_find_char(ptr %start, i64 %host_part_len, i8 58)\n\
             \x20 %no_colon = icmp slt i64 %colon_idx, 0\n\
             \x20 br i1 %no_colon, label %default_port, label %has_port\n\
             has_port:\n\
             \x20 ; Host is start[0..colon_idx]\n\
             \x20 %h_len = add i64 %colon_idx, 1\n\
             \x20 %h_buf = call ptr @malloc(i64 %h_len)\n\
             \x20 call ptr @memcpy(ptr %h_buf, ptr %start, i64 %colon_idx)\n\
             \x20 %h_end = getelementptr i8, ptr %h_buf, i64 %colon_idx\n\
             \x20 store i8 0, ptr %h_end\n\
             \x20 store ptr %h_buf, ptr %host_out\n\
             \x20 ; Port is start[colon_idx+1..host_part_len]\n\
             \x20 %port_off = add i64 %colon_idx, 1\n\
             \x20 %port_start = getelementptr i8, ptr %start, i64 %port_off\n\
             \x20 %port_val = call i64 @atol(ptr %port_start)\n\
             \x20 store i64 %port_val, ptr %port_out\n\
             \x20 ret i32 0\n\
             default_port:\n\
             \x20 ; Host is start[0..host_part_len]\n\
             \x20 %h2_len = add i64 %host_part_len, 1\n\
             \x20 %h2_buf = call ptr @malloc(i64 %h2_len)\n\
             \x20 call ptr @memcpy(ptr %h2_buf, ptr %start, i64 %host_part_len)\n\
             \x20 %h2_end = getelementptr i8, ptr %h2_buf, i64 %host_part_len\n\
             \x20 store i8 0, ptr %h2_end\n\
             \x20 store ptr %h2_buf, ptr %host_out\n\
             \x20 store i64 80, ptr %port_out\n\
             \x20 ret i32 0\n\
             }\n\n",
        );

        // __yorum_find_char — internal helper: find first occurrence of char in buffer
        // Returns index or -1
        self.body.push_str(
            "define i64 @__yorum_find_char(ptr %buf, i64 %len, i8 %ch) {\n\
             entry:\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [0, %entry], [%i_next, %cont]\n\
             \x20 %done = icmp sge i64 %i, %len\n\
             \x20 br i1 %done, label %not_found, label %check\n\
             check:\n\
             \x20 %p = getelementptr i8, ptr %buf, i64 %i\n\
             \x20 %v = load i8, ptr %p\n\
             \x20 %eq = icmp eq i8 %v, %ch\n\
             \x20 br i1 %eq, label %found, label %cont\n\
             cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             found:\n\
             \x20 ret i64 %i\n\
             not_found:\n\
             \x20 ret i64 -1\n\
             }\n\n",
        );

        // http_request(method: string, url: string, headers: string, body: string) -> string
        self.body.push_str(
            "define ptr @http_request(ptr %method, ptr %url, ptr %headers, ptr %body) {\n\
             entry:\n\
             \x20 %host_p = alloca ptr\n\
             \x20 %port_p = alloca i64\n\
             \x20 %path_p = alloca ptr\n\
             \x20 %parse_r = call i32 @__yorum_parse_url(ptr %url, ptr %host_p, ptr %port_p, ptr %path_p)\n\
             \x20 %host = load ptr, ptr %host_p\n\
             \x20 %port = load i64, ptr %port_p\n\
             \x20 %path = load ptr, ptr %path_p\n\
             \x20 ; Connect\n\
             \x20 %fd = call i64 @tcp_connect(ptr %host, i64 %port)\n\
             \x20 %fd_fail = icmp slt i64 %fd, 0\n\
             \x20 br i1 %fd_fail, label %fail, label %connected\n\
             connected:\n\
             \x20 ; Build request into a 4096-byte buffer\n\
             \x20 %req = call ptr @malloc(i64 8192)\n\
             \x20 ; \"METHOD /path HTTP/1.0\"\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %req, i64 8192, ptr @.str.http_req_line, ptr %method, ptr %path)\n\
             \x20 ; \"\\r\\nHost: host\\r\\nConnection: close\"\n\
             \x20 %hdr_buf = alloca [512 x i8]\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %hdr_buf, i64 512, ptr @.str.http_host_hdr, ptr %host)\n\
             \x20 call ptr @strcat(ptr %req, ptr %hdr_buf)\n\
             \x20 ; Append user headers if non-empty\n\
             \x20 %hdr_len = call i64 @strlen(ptr %headers)\n\
             \x20 %has_hdrs = icmp sgt i64 %hdr_len, 0\n\
             \x20 br i1 %has_hdrs, label %add_hdrs, label %check_body\n\
             add_hdrs:\n\
             \x20 call ptr @strcat(ptr %req, ptr @.str.http_crlf)\n\
             \x20 call ptr @strcat(ptr %req, ptr %headers)\n\
             \x20 br label %check_body\n\
             check_body:\n\
             \x20 %body_len = call i64 @strlen(ptr %body)\n\
             \x20 %has_body = icmp sgt i64 %body_len, 0\n\
             \x20 br i1 %has_body, label %add_cl, label %finish_req\n\
             add_cl:\n\
             \x20 ; Content-Length header\n\
             \x20 %cl_buf = alloca [64 x i8]\n\
             \x20 call i32 (ptr, i64, ptr, ...) @snprintf(ptr %cl_buf, i64 64, ptr @.str.http_cl_hdr, i64 %body_len)\n\
             \x20 call ptr @strcat(ptr %req, ptr %cl_buf)\n\
             \x20 br label %finish_req\n\
             finish_req:\n\
             \x20 ; \\r\\n\\r\\n\n\
             \x20 call ptr @strcat(ptr %req, ptr @.str.http_crlf2)\n\
             \x20 ; Append body if present\n\
             \x20 %has_body2 = icmp sgt i64 %body_len, 0\n\
             \x20 br i1 %has_body2, label %add_body, label %send_req\n\
             add_body:\n\
             \x20 call ptr @strcat(ptr %req, ptr %body)\n\
             \x20 br label %send_req\n\
             send_req:\n\
             \x20 %req_len = call i64 @strlen(ptr %req)\n\
             \x20 %fd32 = trunc i64 %fd to i32\n\
             \x20 call i64 @write(i32 %fd32, ptr %req, i64 %req_len)\n\
             \x20 call void @free(ptr %req)\n\
             \x20 ; Read response in a loop\n\
             \x20 %resp_cap_init = add i64 0, 4096\n\
             \x20 %resp_buf = call ptr @malloc(i64 %resp_cap_init)\n\
             \x20 br label %read_loop\n\
             read_loop:\n\
             \x20 %resp_off = phi i64 [0, %send_req], [%new_off, %grow_done]\n\
             \x20 %resp_ptr = phi ptr [%resp_buf, %send_req], [%new_ptr, %grow_done]\n\
             \x20 %resp_cap = phi i64 [4096, %send_req], [%new_cap, %grow_done]\n\
             \x20 %space = sub i64 %resp_cap, %resp_off\n\
             \x20 %read_dst = getelementptr i8, ptr %resp_ptr, i64 %resp_off\n\
             \x20 %n = call i64 @read(i32 %fd32, ptr %read_dst, i64 %space)\n\
             \x20 %read_done = icmp sle i64 %n, 0\n\
             \x20 br i1 %read_done, label %read_end, label %got_data\n\
             got_data:\n\
             \x20 %new_off = add i64 %resp_off, %n\n\
             \x20 %need_grow = icmp sge i64 %new_off, %resp_cap\n\
             \x20 br i1 %need_grow, label %grow, label %grow_done\n\
             grow:\n\
             \x20 %doubled = mul i64 %resp_cap, 2\n\
             \x20 %grown = call ptr @realloc(ptr %resp_ptr, i64 %doubled)\n\
             \x20 br label %grow_done\n\
             grow_done:\n\
             \x20 %new_ptr = phi ptr [%resp_ptr, %got_data], [%grown, %grow]\n\
             \x20 %new_cap = phi i64 [%resp_cap, %got_data], [%doubled, %grow]\n\
             \x20 br label %read_loop\n\
             read_end:\n\
             \x20 call i32 @close(i32 %fd32)\n\
             \x20 ; Null-terminate\n\
             \x20 %term_p = getelementptr i8, ptr %resp_ptr, i64 %resp_off\n\
             \x20 store i8 0, ptr %term_p\n\
             \x20 ; Find \\r\\n\\r\\n to strip headers\n\
             \x20 %sep = call ptr @strstr(ptr %resp_ptr, ptr @.str.http_sep)\n\
             \x20 %no_sep = icmp eq ptr %sep, null\n\
             \x20 br i1 %no_sep, label %return_all, label %strip_headers\n\
             strip_headers:\n\
             \x20 %body_start = getelementptr i8, ptr %sep, i64 4\n\
             \x20 %body_sz = call i64 @strlen(ptr %body_start)\n\
             \x20 %body_buf_sz = add i64 %body_sz, 1\n\
             \x20 %body_buf = call ptr @malloc(i64 %body_buf_sz)\n\
             \x20 call ptr @memcpy(ptr %body_buf, ptr %body_start, i64 %body_buf_sz)\n\
             \x20 call void @free(ptr %resp_ptr)\n\
             \x20 ret ptr %body_buf\n\
             return_all:\n\
             \x20 ret ptr %resp_ptr\n\
             fail:\n\
             \x20 %empty = call ptr @malloc(i64 1)\n\
             \x20 store i8 0, ptr %empty\n\
             \x20 ret ptr %empty\n\
             }\n\n",
        );

        // http_get(url: string) -> string
        self.body.push_str(
            "define ptr @http_get(ptr %url) {\n\
             entry:\n\
             \x20 %r = call ptr @http_request(ptr @.str.http_get_method, ptr %url, ptr @.str.empty, ptr @.str.empty)\n\
             \x20 ret ptr %r\n\
             }\n\n",
        );

        // http_post(url: string, body: string) -> string
        self.body.push_str(
            "define ptr @http_post(ptr %url, ptr %body) {\n\
             entry:\n\
             \x20 %r = call ptr @http_request(ptr @.str.http_post_method, ptr %url, ptr @.str.empty, ptr %body)\n\
             \x20 ret ptr %r\n\
             }\n\n",
        );

        // htons helper — byte-swap for network byte order
        self.body.push_str(
            "define i32 @__yorum_htons(i32 %x) {\n\
             entry:\n\
             \x20 %lo = and i32 %x, 255\n\
             \x20 %hi = lshr i32 %x, 8\n\
             \x20 %hi_masked = and i32 %hi, 255\n\
             \x20 %lo_shift = shl i32 %lo, 8\n\
             \x20 %result = or i32 %lo_shift, %hi_masked\n\
             \x20 ret i32 %result\n\
             }\n\n",
        );
    }

    // ── Generic Map/Set helper emission ──────────────────────

    /// Scan the program for Map/Set instantiations and emit helpers for each.
    fn emit_generic_map_set_helpers(&mut self, program: &Program) {
        let mut map_suffixes: Vec<(String, String, String)> = Vec::new(); // (suffix, key_llvm, val_llvm)
        let mut set_suffixes: Vec<(String, String)> = Vec::new(); // (suffix, elem_llvm)
                                                                  // Scan all let bindings for Map__* and Set__* types
        for decl in &program.declarations {
            let fns = match decl {
                Declaration::Function(f) => vec![f],
                Declaration::Impl(i) => i.methods.iter().collect(),
                _ => continue,
            };
            for f in fns {
                Self::collect_map_set_suffixes_from_block(
                    &f.body,
                    &mut map_suffixes,
                    &mut set_suffixes,
                );
            }
        }
        // Emit map helpers (deduplicated)
        let mut seen: HashSet<String> = HashSet::new();
        for (suffix, key_llvm, val_llvm) in &map_suffixes {
            if suffix == "string__int" {
                continue; // Already have hardcoded helpers + aliases
            }
            if seen.contains(suffix) {
                continue;
            }
            seen.insert(suffix.clone());
            self.emit_map_helpers_for_suffix(suffix, key_llvm, val_llvm);
        }
        // Emit set helpers (deduplicated)
        let mut seen_set: HashSet<String> = HashSet::new();
        for (suffix, elem_llvm) in &set_suffixes {
            if seen_set.contains(suffix) {
                continue;
            }
            seen_set.insert(suffix.clone());
            self.emit_set_helpers_for_suffix(suffix, elem_llvm);
        }
    }

    fn collect_map_set_suffixes_from_block(
        block: &Block,
        map_out: &mut Vec<(String, String, String)>,
        set_out: &mut Vec<(String, String)>,
    ) {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Let(s) => {
                    if let Type::Named(ref name) = s.ty {
                        if let Some(suffix) = name.strip_prefix("Map__") {
                            let parts: Vec<&str> = suffix.splitn(2, "__").collect();
                            if parts.len() == 2 {
                                let key_llvm = Self::yorum_name_to_llvm(parts[0]);
                                let val_llvm = Self::yorum_name_to_llvm(parts[1]);
                                map_out.push((suffix.to_string(), key_llvm, val_llvm));
                            }
                        } else if let Some(suffix) = name.strip_prefix("Set__") {
                            let elem_llvm = Self::yorum_name_to_llvm(suffix);
                            set_out.push((suffix.to_string(), elem_llvm));
                        }
                    }
                }
                Stmt::If(s) => {
                    Self::collect_map_set_suffixes_from_block(&s.then_block, map_out, set_out);
                    if let Some(ref else_branch) = s.else_branch {
                        match else_branch.as_ref() {
                            ElseBranch::ElseIf(elif) => {
                                let if_stmt = Stmt::If(elif.clone());
                                Self::collect_map_set_suffixes_from_block(
                                    &Block {
                                        stmts: vec![if_stmt],
                                        span: elif.span,
                                    },
                                    map_out,
                                    set_out,
                                );
                            }
                            ElseBranch::Else(b) => {
                                Self::collect_map_set_suffixes_from_block(b, map_out, set_out);
                            }
                        }
                    }
                }
                Stmt::While(s) => {
                    Self::collect_map_set_suffixes_from_block(&s.body, map_out, set_out);
                }
                Stmt::For(s) => {
                    Self::collect_map_set_suffixes_from_block(&s.body, map_out, set_out);
                }
                Stmt::Match(s) => {
                    for arm in &s.arms {
                        Self::collect_map_set_suffixes_from_block(&arm.body, map_out, set_out);
                    }
                }
                _ => {}
            }
        }
    }

    fn yorum_name_to_llvm(name: &str) -> String {
        match name {
            "int" => "i64".to_string(),
            "float" => "double".to_string(),
            "bool" => "i1".to_string(),
            "char" => "i8".to_string(),
            "string" => "ptr".to_string(),
            _ => "ptr".to_string(), // Named types are ptr
        }
    }

    fn key_elem_size(key_llvm: &str) -> usize {
        match key_llvm {
            "i64" | "ptr" | "double" => 8,
            "i8" => 1,
            "i1" => 1,
            _ => 8,
        }
    }

    fn val_elem_size(val_llvm: &str) -> usize {
        match val_llvm {
            "i64" | "ptr" | "double" => 8,
            "i8" => 1,
            "i1" => 1,
            _ => 8,
        }
    }

    /// Emit all map helper functions for a specific (K, V) type pair.
    fn emit_map_helpers_for_suffix(&mut self, suffix: &str, key_llvm: &str, val_llvm: &str) {
        if self.emitted_map_helpers.contains(suffix) {
            return;
        }
        self.emitted_map_helpers.insert(suffix.to_string());

        let key_size = Self::key_elem_size(key_llvm);
        let val_size = Self::val_elem_size(val_llvm);
        let key_is_string = key_llvm == "ptr";
        let parts: Vec<&str> = suffix.splitn(2, "__").collect();
        let key_name = parts[0];

        // Emit hash function for this key type (if not already a string hash)
        let hash_fn = if key_is_string {
            "@__yorum_hash_string".to_string()
        } else {
            let hash_name = format!("__yorum_hash_{}", key_name);
            self.emit_hash_function(&hash_name, key_llvm);
            format!("@{}", hash_name)
        };

        // Emit find_slot function
        self.emit_find_slot(suffix, key_llvm, &hash_fn, key_is_string, key_size);

        // Emit grow function
        self.emit_map_grow(suffix, key_llvm, val_llvm, key_size, val_size);

        // Emit map_new
        self.body.push_str(&format!(
            "define ptr @map_new__{}() {{\n\
             entry:\n\
             \x20 %map = call ptr @malloc(i64 48)\n\
             \x20 %kb = mul i64 16, {}\n\
             \x20 %keys = call ptr @malloc(i64 %kb)\n\
             \x20 %vb = mul i64 16, {}\n\
             \x20 %vals = call ptr @malloc(i64 %vb)\n\
             \x20 %flags = call ptr @malloc(i64 16)\n\
             \x20 call ptr @memset(ptr %flags, i32 0, i64 16)\n\
             \x20 store ptr %keys, ptr %map\n\
             \x20 %vp = getelementptr i8, ptr %map, i64 8\n\
             \x20 store ptr %vals, ptr %vp\n\
             \x20 %fp = getelementptr i8, ptr %map, i64 16\n\
             \x20 store ptr %flags, ptr %fp\n\
             \x20 %cp = getelementptr i8, ptr %map, i64 24\n\
             \x20 store i64 16, ptr %cp\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 store i64 0, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %map, i64 40\n\
             \x20 store i64 0, ptr %tp\n\
             \x20 ret ptr %map\n\
             }}\n\n",
            suffix, key_size, val_size
        ));

        // Emit map_set
        let key_copy = if key_is_string {
            "\x20 ; copy key string\n\
             \x20 %klen = call i64 @strlen(ptr %key)\n\
             \x20 %kbuf_sz = add i64 %klen, 1\n\
             \x20 %kbuf = call ptr @malloc(i64 %kbuf_sz)\n\
             \x20 call ptr @strcpy(ptr %kbuf, ptr %key)\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %kslot = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 store ptr %kbuf, ptr %kslot\n"
                .to_string()
        } else {
            format!(
                "\x20 %keys_p = load ptr, ptr %map\n\
                 \x20 %kslot = getelementptr {}, ptr %keys_p, i64 %slot\n\
                 \x20 store {} %key, ptr %kslot\n",
                key_llvm, key_llvm
            )
        };

        self.body.push_str(&format!(
            "define void @map_set__{suffix}(ptr %map, {key_llvm} %key, {val_llvm} %val) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %map, i64 40\n\
             \x20 %tombs = load i64, ptr %tp\n\
             \x20 %used = add i64 %size, %tombs\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %s4 = mul i64 %used, 4\n\
             \x20 %c3 = mul i64 %cap, 3\n\
             \x20 %need_grow = icmp sge i64 %s4, %c3\n\
             \x20 br i1 %need_grow, label %grow, label %find\n\
             grow:\n\
             \x20 call void @__yorum_map_grow__{suffix}(ptr %map)\n\
             \x20 br label %find\n\
             find:\n\
             \x20 %cap2 = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %key, i64 %cap2)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fslot = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fslot\n\
             \x20 %is_occupied = icmp eq i8 %flag, 1\n\
             \x20 br i1 %is_occupied, label %update, label %insert\n\
             insert:\n\
             {key_copy}\
             \x20 store i8 1, ptr %fslot\n\
             \x20 %new_size = add i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 br label %store_val\n\
             update:\n\
             \x20 br label %store_val\n\
             store_val:\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %vslot = getelementptr {val_llvm}, ptr %vals_p, i64 %slot\n\
             \x20 store {val_llvm} %val, ptr %vslot\n\
             \x20 ret void\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            val_llvm = val_llvm,
            key_copy = key_copy,
        ));

        // Emit map_get
        self.body.push_str(&format!(
            "define {val_llvm} @map_get__{suffix}(ptr %map, {key_llvm} %key) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 br i1 %found, label %ok, label %fail\n\
             fail:\n\
             \x20 call i32 (ptr, ...) @printf(ptr @.fmt.map_key_generic)\n\
             \x20 call void @abort()\n\
             \x20 unreachable\n\
             ok:\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %vp = getelementptr {val_llvm}, ptr %vals_p, i64 %slot\n\
             \x20 %v = load {val_llvm}, ptr %vp\n\
             \x20 ret {val_llvm} %v\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            val_llvm = val_llvm,
        ));

        // Emit map_has
        self.body.push_str(&format!(
            "define i1 @map_has__{suffix}(ptr %map, {key_llvm} %key) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 ret i1 %found\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
        ));

        // Emit map_size (same for all types)
        self.body.push_str(&format!(
            "define i64 @map_size__{suffix}(ptr %map) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 ret i64 %size\n\
             }}\n\n",
            suffix = suffix,
        ));

        // Emit map_remove
        let key_free = if key_is_string {
            "\x20 %keys_p = load ptr, ptr %map\n\
             \x20 %kp = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 %k = load ptr, ptr %kp\n\
             \x20 call void @free(ptr %k)\n"
                .to_string()
        } else {
            String::new()
        };

        self.body.push_str(&format!(
            "define i1 @map_remove__{suffix}(ptr %map, {key_llvm} %key) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 br i1 %found, label %remove, label %done\n\
             remove:\n\
             \x20 store i8 2, ptr %fp\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %new_size = sub i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 %tomb_p = getelementptr i8, ptr %map, i64 40\n\
             \x20 %tombs = load i64, ptr %tomb_p\n\
             \x20 %new_tombs = add i64 %tombs, 1\n\
             \x20 store i64 %new_tombs, ptr %tomb_p\n\
             {key_free}\
             \x20 ret i1 1\n\
             done:\n\
             \x20 ret i1 0\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            key_free = key_free,
        ));

        // Emit map_keys — returns [K] array
        self.body.push_str(&format!(
            "define ptr @map_keys__{suffix}(ptr %map) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %dbytes = mul i64 %size, {key_size}\n\
             \x20 %data = call ptr @malloc(i64 %dbytes)\n\
             \x20 %fd = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %fd\n\
             \x20 %fl = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %fl\n\
             \x20 %fc = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %size, ptr %fc\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %loop_cont ]\n\
             \x20 %out_idx = phi i64 [ 0, %entry ], [ %out_next, %loop_cont ]\n\
             \x20 %done = icmp sge i64 %i, %cap\n\
             \x20 br i1 %done, label %finish, label %check\n\
             check:\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %i\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %occ = icmp eq i8 %flag, 1\n\
             \x20 br i1 %occ, label %copy_key, label %skip\n\
             copy_key:\n\
             \x20 %kp = getelementptr {key_llvm}, ptr %keys_p, i64 %i\n\
             \x20 %k = load {key_llvm}, ptr %kp\n\
             \x20 %dp = getelementptr {key_llvm}, ptr %data, i64 %out_idx\n\
             \x20 store {key_llvm} %k, ptr %dp\n\
             \x20 %out_inc = add i64 %out_idx, 1\n\
             \x20 br label %loop_cont\n\
             skip:\n\
             \x20 br label %loop_cont\n\
             loop_cont:\n\
             \x20 %out_next = phi i64 [ %out_inc, %copy_key ], [ %out_idx, %skip ]\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             finish:\n\
             \x20 store i64 %out_idx, ptr %fl\n\
             \x20 ret ptr %fat\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            key_size = key_size,
        ));

        // Emit map_values — returns [V] array
        self.body.push_str(&format!(
            "define ptr @map_values__{suffix}(ptr %map) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %map, i64 32\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %dbytes = mul i64 %size, {val_size}\n\
             \x20 %data = call ptr @malloc(i64 %dbytes)\n\
             \x20 %fd = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %fd\n\
             \x20 %fl = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %fl\n\
             \x20 %fc = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %size, ptr %fc\n\
             \x20 %vals_pp = getelementptr i8, ptr %map, i64 8\n\
             \x20 %vals_p = load ptr, ptr %vals_pp\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %loop_cont ]\n\
             \x20 %out_idx = phi i64 [ 0, %entry ], [ %out_next, %loop_cont ]\n\
             \x20 %done = icmp sge i64 %i, %cap\n\
             \x20 br i1 %done, label %finish, label %check\n\
             check:\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %i\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %occ = icmp eq i8 %flag, 1\n\
             \x20 br i1 %occ, label %copy_val, label %skip\n\
             copy_val:\n\
             \x20 %vp = getelementptr {val_llvm}, ptr %vals_p, i64 %i\n\
             \x20 %v = load {val_llvm}, ptr %vp\n\
             \x20 %dp = getelementptr {val_llvm}, ptr %data, i64 %out_idx\n\
             \x20 store {val_llvm} %v, ptr %dp\n\
             \x20 %out_inc = add i64 %out_idx, 1\n\
             \x20 br label %loop_cont\n\
             skip:\n\
             \x20 br label %loop_cont\n\
             loop_cont:\n\
             \x20 %out_next = phi i64 [ %out_inc, %copy_val ], [ %out_idx, %skip ]\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             finish:\n\
             \x20 store i64 %out_idx, ptr %fl\n\
             \x20 ret ptr %fat\n\
             }}\n\n",
            suffix = suffix,
            val_llvm = val_llvm,
            val_size = val_size,
        ));

        // Register fn_ret_types for all mangled names
        let key_name_str = parts[0];
        let val_name_str = parts[1];
        let key_type = Self::yorum_name_to_ast_type(key_name_str);
        let val_type = Self::yorum_name_to_ast_type(val_name_str);

        self.fn_ret_types.insert(
            format!("map_new__{}", suffix),
            Type::Generic("Map".to_string(), vec![key_type.clone(), val_type.clone()]),
        );
        self.fn_ret_types
            .insert(format!("map_set__{}", suffix), Type::Unit);
        self.fn_ret_types
            .insert(format!("map_get__{}", suffix), val_type.clone());
        self.fn_ret_types
            .insert(format!("map_has__{}", suffix), Type::Bool);
        self.fn_ret_types
            .insert(format!("map_size__{}", suffix), Type::Int);
        self.fn_ret_types
            .insert(format!("map_remove__{}", suffix), Type::Bool);
        self.fn_ret_types.insert(
            format!("map_keys__{}", suffix),
            Type::Array(Box::new(key_type)),
        );
        self.fn_ret_types.insert(
            format!("map_values__{}", suffix),
            Type::Array(Box::new(val_type)),
        );

        // Add format string for generic map key error
        if !self.globals.contains("@.fmt.map_key_generic") {
            self.globals.push_str(
                "@.fmt.map_key_generic = private constant [23 x i8] c\"map_get: key not found\\00\"\n",
            );
        }
    }

    fn yorum_name_to_ast_type(name: &str) -> Type {
        match name {
            "int" => Type::Int,
            "float" => Type::Float,
            "bool" => Type::Bool,
            "char" => Type::Char,
            "string" => Type::Str,
            _ => Type::Named(name.to_string()),
        }
    }

    fn emit_hash_function(&mut self, name: &str, key_llvm: &str) {
        // For non-string keys: bit-mix hash
        match key_llvm {
            "i64" => {
                self.body.push_str(&format!(
                    "define i64 @{}(i64 %x) {{\n\
                     entry:\n\
                     \x20 %x1 = xor i64 %x, -4658895280553007687\n\
                     \x20 %x2 = mul i64 %x1, -7723592293110705685\n\
                     \x20 %x3 = lshr i64 %x2, 33\n\
                     \x20 %x4 = xor i64 %x2, %x3\n\
                     \x20 %x5 = mul i64 %x4, -4658895280553007687\n\
                     \x20 ret i64 %x5\n\
                     }}\n\n",
                    name
                ));
            }
            "i8" => {
                self.body.push_str(&format!(
                    "define i64 @{}(i8 %x) {{\n\
                     entry:\n\
                     \x20 %x64 = zext i8 %x to i64\n\
                     \x20 %x1 = xor i64 %x64, -4658895280553007687\n\
                     \x20 %x2 = mul i64 %x1, -7723592293110705685\n\
                     \x20 ret i64 %x2\n\
                     }}\n\n",
                    name
                ));
            }
            "i1" => {
                self.body.push_str(&format!(
                    "define i64 @{}(i1 %x) {{\n\
                     entry:\n\
                     \x20 %x64 = zext i1 %x to i64\n\
                     \x20 %x1 = xor i64 %x64, -4658895280553007687\n\
                     \x20 %x2 = mul i64 %x1, -7723592293110705685\n\
                     \x20 ret i64 %x2\n\
                     }}\n\n",
                    name
                ));
            }
            _ => {
                // Fallback: treat as i64 (shouldn't happen for hashable types)
                self.body.push_str(&format!(
                    "define i64 @{}(i64 %x) {{\n\
                     entry:\n\
                     \x20 ret i64 %x\n\
                     }}\n\n",
                    name
                ));
            }
        }
    }

    fn emit_find_slot(
        &mut self,
        suffix: &str,
        key_llvm: &str,
        hash_fn: &str,
        key_is_string: bool,
        key_size: usize,
    ) {
        let _ = key_size;
        let key_compare = if key_is_string {
            "\x20 %k = load ptr, ptr %kp\n\
             \x20 %cmp = call i32 @strcmp(ptr %k, ptr %key)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n"
                .to_string()
        } else {
            format!(
                "\x20 %k = load {}, ptr %kp\n\
                 \x20 %eq = icmp eq {} %k, %key\n",
                key_llvm, key_llvm
            )
        };

        self.body.push_str(&format!(
            "define i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %key, i64 %cap) {{\n\
             entry:\n\
             \x20 %hash = call i64 {hash_fn}({key_llvm} %key)\n\
             \x20 %mask = sub i64 %cap, 1\n\
             \x20 %start = and i64 %hash, %mask\n\
             \x20 %keys_p = load ptr, ptr %map\n\
             \x20 %flags_pp = getelementptr i8, ptr %map, i64 16\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %probe\n\
             probe:\n\
             \x20 %idx = phi i64 [ %start, %entry ], [ %next, %advance ]\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %idx\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %is_empty = icmp eq i8 %flag, 0\n\
             \x20 br i1 %is_empty, label %done, label %check_tombstone\n\
             check_tombstone:\n\
             \x20 %is_tomb = icmp eq i8 %flag, 2\n\
             \x20 br i1 %is_tomb, label %advance, label %check_key\n\
             check_key:\n\
             \x20 %kp = getelementptr {key_llvm}, ptr %keys_p, i64 %idx\n\
             {key_compare}\
             \x20 br i1 %eq, label %done, label %advance\n\
             advance:\n\
             \x20 %next_raw = add i64 %idx, 1\n\
             \x20 %next = and i64 %next_raw, %mask\n\
             \x20 br label %probe\n\
             done:\n\
             \x20 ret i64 %idx\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            hash_fn = hash_fn,
            key_compare = key_compare,
        ));
    }

    fn emit_map_grow(
        &mut self,
        suffix: &str,
        key_llvm: &str,
        val_llvm: &str,
        key_size: usize,
        val_size: usize,
    ) {
        self.body.push_str(&format!(
            "define void @__yorum_map_grow__{suffix}(ptr %map) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %map, i64 24\n\
             \x20 %old_cap = load i64, ptr %cap_p\n\
             \x20 %new_cap = mul i64 %old_cap, 2\n\
             \x20 %kb = mul i64 %new_cap, {key_size}\n\
             \x20 %vb = mul i64 %new_cap, {val_size}\n\
             \x20 %new_keys = call ptr @malloc(i64 %kb)\n\
             \x20 %new_vals = call ptr @malloc(i64 %vb)\n\
             \x20 %new_flags = call ptr @malloc(i64 %new_cap)\n\
             \x20 call ptr @memset(ptr %new_flags, i32 0, i64 %new_cap)\n\
             \x20 %old_keys = load ptr, ptr %map\n\
             \x20 %vals_p = getelementptr i8, ptr %map, i64 8\n\
             \x20 %old_vals = load ptr, ptr %vals_p\n\
             \x20 %flags_p = getelementptr i8, ptr %map, i64 16\n\
             \x20 %old_flags = load ptr, ptr %flags_p\n\
             \x20 store ptr %new_keys, ptr %map\n\
             \x20 store ptr %new_vals, ptr %vals_p\n\
             \x20 store ptr %new_flags, ptr %flags_p\n\
             \x20 store i64 %new_cap, ptr %cap_p\n\
             \x20 %tomb_p = getelementptr i8, ptr %map, i64 40\n\
             \x20 store i64 0, ptr %tomb_p\n\
             \x20 br label %rehash_loop\n\
             rehash_loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %rehash_cont ]\n\
             \x20 %cmp = icmp slt i64 %i, %old_cap\n\
             \x20 br i1 %cmp, label %rehash_body, label %rehash_done\n\
             rehash_body:\n\
             \x20 %ofp = getelementptr i8, ptr %old_flags, i64 %i\n\
             \x20 %of = load i8, ptr %ofp\n\
             \x20 %occ = icmp eq i8 %of, 1\n\
             \x20 br i1 %occ, label %rehash_insert, label %rehash_cont\n\
             rehash_insert:\n\
             \x20 %okp = getelementptr {key_llvm}, ptr %old_keys, i64 %i\n\
             \x20 %ok = load {key_llvm}, ptr %okp\n\
             \x20 %ovp = getelementptr {val_llvm}, ptr %old_vals, i64 %i\n\
             \x20 %ov = load {val_llvm}, ptr %ovp\n\
             \x20 %slot = call i64 @__yorum_map_find_slot__{suffix}(ptr %map, {key_llvm} %ok, i64 %new_cap)\n\
             \x20 %nkp = getelementptr {key_llvm}, ptr %new_keys, i64 %slot\n\
             \x20 store {key_llvm} %ok, ptr %nkp\n\
             \x20 %nvp = getelementptr {val_llvm}, ptr %new_vals, i64 %slot\n\
             \x20 store {val_llvm} %ov, ptr %nvp\n\
             \x20 %nfp = getelementptr i8, ptr %new_flags, i64 %slot\n\
             \x20 store i8 1, ptr %nfp\n\
             \x20 br label %rehash_cont\n\
             rehash_cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %rehash_loop\n\
             rehash_done:\n\
             \x20 call void @free(ptr %old_keys)\n\
             \x20 call void @free(ptr %old_vals)\n\
             \x20 call void @free(ptr %old_flags)\n\
             \x20 ret void\n\
             }}\n\n",
            suffix = suffix,
            key_llvm = key_llvm,
            val_llvm = val_llvm,
            key_size = key_size,
            val_size = val_size,
        ));
    }

    /// Emit all set helper functions for a specific T type.
    /// Set struct layout (40 bytes):
    ///   offset 0:  ptr   keys
    ///   offset 8:  ptr   flags
    ///   offset 16: i64   capacity
    ///   offset 24: i64   size
    ///   offset 32: i64   tombstones
    fn emit_set_helpers_for_suffix(&mut self, suffix: &str, elem_llvm: &str) {
        let elem_size = Self::key_elem_size(elem_llvm);
        let elem_is_string = elem_llvm == "ptr";

        // Reuse hash function from map if already emitted, or emit new one
        let hash_fn = if elem_is_string {
            "@__yorum_hash_string".to_string()
        } else {
            let hash_name = format!("__yorum_hash_{}", suffix);
            // Check if hash already emitted (might share with Map)
            if !self.body.contains(&format!("@{}", hash_name)) {
                self.emit_hash_function(&hash_name, elem_llvm);
            }
            format!("@{}", hash_name)
        };

        // Emit find_slot for set
        let key_compare = if elem_is_string {
            "\x20 %k = load ptr, ptr %kp\n\
             \x20 %cmp = call i32 @strcmp(ptr %k, ptr %key)\n\
             \x20 %eq = icmp eq i32 %cmp, 0\n"
                .to_string()
        } else {
            format!(
                "\x20 %k = load {}, ptr %kp\n\
                 \x20 %eq = icmp eq {} %k, %key\n",
                elem_llvm, elem_llvm
            )
        };

        self.body.push_str(&format!(
            "define i64 @__yorum_set_find_slot__{suffix}(ptr %set, {elem_llvm} %key, i64 %cap) {{\n\
             entry:\n\
             \x20 %hash = call i64 {hash_fn}({elem_llvm} %key)\n\
             \x20 %mask = sub i64 %cap, 1\n\
             \x20 %start = and i64 %hash, %mask\n\
             \x20 %keys_p = load ptr, ptr %set\n\
             \x20 %flags_pp = getelementptr i8, ptr %set, i64 8\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %probe\n\
             probe:\n\
             \x20 %idx = phi i64 [ %start, %entry ], [ %next, %advance ]\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %idx\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %is_empty = icmp eq i8 %flag, 0\n\
             \x20 br i1 %is_empty, label %done, label %check_tombstone\n\
             check_tombstone:\n\
             \x20 %is_tomb = icmp eq i8 %flag, 2\n\
             \x20 br i1 %is_tomb, label %advance, label %check_key\n\
             check_key:\n\
             \x20 %kp = getelementptr {elem_llvm}, ptr %keys_p, i64 %idx\n\
             {key_compare}\
             \x20 br i1 %eq, label %done, label %advance\n\
             advance:\n\
             \x20 %next_raw = add i64 %idx, 1\n\
             \x20 %next = and i64 %next_raw, %mask\n\
             \x20 br label %probe\n\
             done:\n\
             \x20 ret i64 %idx\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
            hash_fn = hash_fn,
            key_compare = key_compare,
        ));

        // Emit set grow
        self.body.push_str(&format!(
            "define void @__yorum_set_grow__{suffix}(ptr %set) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %set, i64 16\n\
             \x20 %old_cap = load i64, ptr %cap_p\n\
             \x20 %new_cap = mul i64 %old_cap, 2\n\
             \x20 %kb = mul i64 %new_cap, {elem_size}\n\
             \x20 %new_keys = call ptr @malloc(i64 %kb)\n\
             \x20 %new_flags = call ptr @malloc(i64 %new_cap)\n\
             \x20 call ptr @memset(ptr %new_flags, i32 0, i64 %new_cap)\n\
             \x20 %old_keys = load ptr, ptr %set\n\
             \x20 %flags_p = getelementptr i8, ptr %set, i64 8\n\
             \x20 %old_flags = load ptr, ptr %flags_p\n\
             \x20 store ptr %new_keys, ptr %set\n\
             \x20 store ptr %new_flags, ptr %flags_p\n\
             \x20 store i64 %new_cap, ptr %cap_p\n\
             \x20 %tomb_p = getelementptr i8, ptr %set, i64 32\n\
             \x20 store i64 0, ptr %tomb_p\n\
             \x20 br label %rehash_loop\n\
             rehash_loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %rehash_cont ]\n\
             \x20 %cmp = icmp slt i64 %i, %old_cap\n\
             \x20 br i1 %cmp, label %rehash_body, label %rehash_done\n\
             rehash_body:\n\
             \x20 %ofp = getelementptr i8, ptr %old_flags, i64 %i\n\
             \x20 %of = load i8, ptr %ofp\n\
             \x20 %occ = icmp eq i8 %of, 1\n\
             \x20 br i1 %occ, label %rehash_insert, label %rehash_cont\n\
             rehash_insert:\n\
             \x20 %okp = getelementptr {elem_llvm}, ptr %old_keys, i64 %i\n\
             \x20 %ok = load {elem_llvm}, ptr %okp\n\
             \x20 %slot = call i64 @__yorum_set_find_slot__{suffix}(ptr %set, {elem_llvm} %ok, i64 %new_cap)\n\
             \x20 %nkp = getelementptr {elem_llvm}, ptr %new_keys, i64 %slot\n\
             \x20 store {elem_llvm} %ok, ptr %nkp\n\
             \x20 %nfp = getelementptr i8, ptr %new_flags, i64 %slot\n\
             \x20 store i8 1, ptr %nfp\n\
             \x20 br label %rehash_cont\n\
             rehash_cont:\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %rehash_loop\n\
             rehash_done:\n\
             \x20 call void @free(ptr %old_keys)\n\
             \x20 call void @free(ptr %old_flags)\n\
             \x20 ret void\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
            elem_size = elem_size,
        ));

        // set_new — allocate 40-byte set struct with capacity 16
        self.body.push_str(&format!(
            "define ptr @set_new__{suffix}() {{\n\
             entry:\n\
             \x20 %set = call ptr @malloc(i64 40)\n\
             \x20 %kb = mul i64 16, {elem_size}\n\
             \x20 %keys = call ptr @malloc(i64 %kb)\n\
             \x20 %flags = call ptr @malloc(i64 16)\n\
             \x20 call ptr @memset(ptr %flags, i32 0, i64 16)\n\
             \x20 store ptr %keys, ptr %set\n\
             \x20 %fp = getelementptr i8, ptr %set, i64 8\n\
             \x20 store ptr %flags, ptr %fp\n\
             \x20 %cp = getelementptr i8, ptr %set, i64 16\n\
             \x20 store i64 16, ptr %cp\n\
             \x20 %sp = getelementptr i8, ptr %set, i64 24\n\
             \x20 store i64 0, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %set, i64 32\n\
             \x20 store i64 0, ptr %tp\n\
             \x20 ret ptr %set\n\
             }}\n\n",
            suffix = suffix,
            elem_size = elem_size,
        ));

        // set_add — insert element into set
        let key_copy = if elem_is_string {
            "\x20 %klen = call i64 @strlen(ptr %key)\n\
             \x20 %kbuf_sz = add i64 %klen, 1\n\
             \x20 %kbuf = call ptr @malloc(i64 %kbuf_sz)\n\
             \x20 call ptr @strcpy(ptr %kbuf, ptr %key)\n\
             \x20 %keys_p = load ptr, ptr %set\n\
             \x20 %kslot = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 store ptr %kbuf, ptr %kslot\n"
                .to_string()
        } else {
            format!(
                "\x20 %keys_p = load ptr, ptr %set\n\
                 \x20 %kslot = getelementptr {}, ptr %keys_p, i64 %slot\n\
                 \x20 store {} %key, ptr %kslot\n",
                elem_llvm, elem_llvm
            )
        };

        self.body.push_str(&format!(
            "define void @set_add__{suffix}(ptr %set, {elem_llvm} %key) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %set, i64 24\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %tp = getelementptr i8, ptr %set, i64 32\n\
             \x20 %tombs = load i64, ptr %tp\n\
             \x20 %used = add i64 %size, %tombs\n\
             \x20 %cap_p = getelementptr i8, ptr %set, i64 16\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %s4 = mul i64 %used, 4\n\
             \x20 %c3 = mul i64 %cap, 3\n\
             \x20 %need_grow = icmp sge i64 %s4, %c3\n\
             \x20 br i1 %need_grow, label %grow, label %find\n\
             grow:\n\
             \x20 call void @__yorum_set_grow__{suffix}(ptr %set)\n\
             \x20 br label %find\n\
             find:\n\
             \x20 %cap2 = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_set_find_slot__{suffix}(ptr %set, {elem_llvm} %key, i64 %cap2)\n\
             \x20 %flags_pp = getelementptr i8, ptr %set, i64 8\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fslot = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fslot\n\
             \x20 %is_occupied = icmp eq i8 %flag, 1\n\
             \x20 br i1 %is_occupied, label %done, label %insert\n\
             insert:\n\
             {key_copy}\
             \x20 store i8 1, ptr %fslot\n\
             \x20 %new_size = add i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 br label %done\n\
             done:\n\
             \x20 ret void\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
            key_copy = key_copy,
        ));

        // set_has — check if element exists
        self.body.push_str(&format!(
            "define i1 @set_has__{suffix}(ptr %set, {elem_llvm} %key) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %set, i64 16\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_set_find_slot__{suffix}(ptr %set, {elem_llvm} %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %set, i64 8\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 ret i1 %found\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
        ));

        // set_remove — mark slot as tombstone
        let key_free = if elem_is_string {
            "\x20 %keys_p = load ptr, ptr %set\n\
             \x20 %kp = getelementptr ptr, ptr %keys_p, i64 %slot\n\
             \x20 %k = load ptr, ptr %kp\n\
             \x20 call void @free(ptr %k)\n"
                .to_string()
        } else {
            String::new()
        };

        self.body.push_str(&format!(
            "define i1 @set_remove__{suffix}(ptr %set, {elem_llvm} %key) {{\n\
             entry:\n\
             \x20 %cap_p = getelementptr i8, ptr %set, i64 16\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %slot = call i64 @__yorum_set_find_slot__{suffix}(ptr %set, {elem_llvm} %key, i64 %cap)\n\
             \x20 %flags_pp = getelementptr i8, ptr %set, i64 8\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %slot\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %found = icmp eq i8 %flag, 1\n\
             \x20 br i1 %found, label %remove, label %done\n\
             remove:\n\
             \x20 store i8 2, ptr %fp\n\
             \x20 %sp = getelementptr i8, ptr %set, i64 24\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %new_size = sub i64 %size, 1\n\
             \x20 store i64 %new_size, ptr %sp\n\
             \x20 %tomb_p = getelementptr i8, ptr %set, i64 32\n\
             \x20 %tombs = load i64, ptr %tomb_p\n\
             \x20 %new_tombs = add i64 %tombs, 1\n\
             \x20 store i64 %new_tombs, ptr %tomb_p\n\
             {key_free}\
             \x20 ret i1 1\n\
             done:\n\
             \x20 ret i1 0\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
            key_free = key_free,
        ));

        // set_size — load size field
        self.body.push_str(&format!(
            "define i64 @set_size__{suffix}(ptr %set) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %set, i64 24\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 ret i64 %size\n\
             }}\n\n",
            suffix = suffix,
        ));

        // set_values — collect all elements into [T] array
        self.body.push_str(&format!(
            "define ptr @set_values__{suffix}(ptr %set) {{\n\
             entry:\n\
             \x20 %sp = getelementptr i8, ptr %set, i64 24\n\
             \x20 %size = load i64, ptr %sp\n\
             \x20 %cap_p = getelementptr i8, ptr %set, i64 16\n\
             \x20 %cap = load i64, ptr %cap_p\n\
             \x20 %fat = call ptr @malloc(i64 24)\n\
             \x20 %dbytes = mul i64 %size, {elem_size}\n\
             \x20 %data = call ptr @malloc(i64 %dbytes)\n\
             \x20 %fd = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 0\n\
             \x20 store ptr %data, ptr %fd\n\
             \x20 %fl = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 1\n\
             \x20 store i64 0, ptr %fl\n\
             \x20 %fc = getelementptr {{ ptr, i64, i64 }}, ptr %fat, i32 0, i32 2\n\
             \x20 store i64 %size, ptr %fc\n\
             \x20 %keys_p = load ptr, ptr %set\n\
             \x20 %flags_pp = getelementptr i8, ptr %set, i64 8\n\
             \x20 %flags_p = load ptr, ptr %flags_pp\n\
             \x20 br label %loop\n\
             loop:\n\
             \x20 %i = phi i64 [ 0, %entry ], [ %i_next, %loop_cont ]\n\
             \x20 %out_idx = phi i64 [ 0, %entry ], [ %out_next, %loop_cont ]\n\
             \x20 %done = icmp sge i64 %i, %cap\n\
             \x20 br i1 %done, label %finish, label %check\n\
             check:\n\
             \x20 %fp = getelementptr i8, ptr %flags_p, i64 %i\n\
             \x20 %flag = load i8, ptr %fp\n\
             \x20 %occ = icmp eq i8 %flag, 1\n\
             \x20 br i1 %occ, label %copy_key, label %skip\n\
             copy_key:\n\
             \x20 %kp = getelementptr {elem_llvm}, ptr %keys_p, i64 %i\n\
             \x20 %k = load {elem_llvm}, ptr %kp\n\
             \x20 %dp = getelementptr {elem_llvm}, ptr %data, i64 %out_idx\n\
             \x20 store {elem_llvm} %k, ptr %dp\n\
             \x20 %out_inc = add i64 %out_idx, 1\n\
             \x20 br label %loop_cont\n\
             skip:\n\
             \x20 br label %loop_cont\n\
             loop_cont:\n\
             \x20 %out_next = phi i64 [ %out_inc, %copy_key ], [ %out_idx, %skip ]\n\
             \x20 %i_next = add i64 %i, 1\n\
             \x20 br label %loop\n\
             finish:\n\
             \x20 store i64 %out_idx, ptr %fl\n\
             \x20 ret ptr %fat\n\
             }}\n\n",
            suffix = suffix,
            elem_llvm = elem_llvm,
            elem_size = elem_size,
        ));

        // Register fn_ret_types for all mangled set names
        let elem_type = Self::yorum_name_to_ast_type(suffix);

        self.fn_ret_types.insert(
            format!("set_new__{}", suffix),
            Type::Generic("Set".to_string(), vec![elem_type.clone()]),
        );
        self.fn_ret_types
            .insert(format!("set_add__{}", suffix), Type::Unit);
        self.fn_ret_types
            .insert(format!("set_has__{}", suffix), Type::Bool);
        self.fn_ret_types
            .insert(format!("set_remove__{}", suffix), Type::Bool);
        self.fn_ret_types
            .insert(format!("set_size__{}", suffix), Type::Int);
        self.fn_ret_types.insert(
            format!("set_values__{}", suffix),
            Type::Array(Box::new(elem_type)),
        );
    }

    // ── Type registration ────────────────────────────────────

    fn register_types(&mut self, program: &Program) {
        for decl in &program.declarations {
            match decl {
                Declaration::Struct(s) => {
                    let field_types: Vec<String> =
                        s.fields.iter().map(|f| self.llvm_type(&f.ty)).collect();
                    let llvm_ty = format!("{{ {} }}", field_types.join(", "));
                    self.struct_layouts.insert(
                        s.name.clone(),
                        StructLayout {
                            name: s.name.clone(),
                            fields: s
                                .fields
                                .iter()
                                .map(|f| (f.name.clone(), f.ty.clone()))
                                .collect(),
                            llvm_ty,
                        },
                    );
                }
                Declaration::Enum(e) => {
                    self.enum_layouts.insert(
                        e.name.clone(),
                        EnumLayout {
                            name: e.name.clone(),
                            variants: e
                                .variants
                                .iter()
                                .map(|v| (v.name.clone(), v.fields.clone()))
                                .collect(),
                            tag_count: e.variants.len(),
                        },
                    );
                }
                _ => {}
            }
        }
    }

    fn emit_type_defs(&mut self) {
        self.type_defs.push_str("; ── Type definitions ──\n");
        for (name, layout) in &self.struct_layouts {
            self.type_defs
                .push_str(&format!("%{} = type {}\n", name, layout.llvm_ty));
        }
        // Enums are represented as { i32, <largest_payload> }
        // For simple enums (no data), just i32
        for (name, layout) in &self.enum_layouts {
            let has_data = layout.variants.iter().any(|(_, fields)| !fields.is_empty());
            if has_data {
                // Find the largest variant payload size
                let max_payload = layout
                    .variants
                    .iter()
                    .map(|(_, fields)| fields.iter().map(|t| self.type_size(t)).sum::<usize>())
                    .max()
                    .unwrap_or(0);
                let payload_bytes = if max_payload == 0 { 8 } else { max_payload };
                self.type_defs.push_str(&format!(
                    "%{} = type {{ i32, [{} x i8] }}\n",
                    name, payload_bytes
                ));
            } else {
                self.type_defs
                    .push_str(&format!("%{} = type {{ i32 }}\n", name));
            }
        }
        self.type_defs.push('\n');
    }

    // ── Function emission ────────────────────────────────────

    /// Check if a function qualifies for the `alwaysinline` LLVM attribute.
    fn should_inline(f: &FnDecl) -> bool {
        f.is_pure
            && f.name != "main"
            && f.body.stmts.len() <= 3
            && !f
                .contracts
                .iter()
                .any(|c| matches!(c, Contract::Requires(_) | Contract::Ensures(_)))
    }

    fn emit_function(&mut self, f: &FnDecl) -> Result<(), CodegenError> {
        self.temp_counter = 0;
        self.label_counter = 0;
        self.block_terminated = false;
        self.current_block = "entry".to_string();
        self.current_fn_contracts = f.contracts.clone();
        self.current_fn_name = f.name.clone();
        self.string_buf_vars.clear();

        // Ensure tuple type definitions exist for params and return type
        if let Type::Tuple(ref types) = f.return_type {
            self.ensure_tuple_type(types);
        }
        for p in &f.params {
            if let Type::Tuple(ref types) = p.ty {
                self.ensure_tuple_type(types);
            }
        }

        let ret_ty = self.llvm_type(&f.return_type);
        self.current_fn_ret_ty = Some(ret_ty.clone());

        let mut params: Vec<String> = f
            .params
            .iter()
            .map(|p| format!("{} %{}", self.llvm_type(&p.ty), p.name))
            .collect();

        // main() gets argc/argv parameters to capture command-line args
        if f.name == "main" {
            params.push("i32 %__argc".to_string());
            params.push("ptr %__argv".to_string());
        }

        let dbg_suffix = if self.debug_enabled {
            let line = if f.span.line > 0 { f.span.line } else { 1 };
            let sp_id = self.emit_debug_subprogram(&f.name, line);
            format!(" !dbg !{}", sp_id)
        } else {
            String::new()
        };

        let inline_attr = if Self::should_inline(f) {
            self.has_inline_fns = true;
            " #0"
        } else {
            ""
        };

        self.body.push_str(&format!(
            "define {} @{}({}){}{} {{\n",
            ret_ty,
            f.name,
            params.join(", "),
            dbg_suffix,
            inline_attr
        ));
        self.body.push_str("entry:\n");

        // Store argc/argv to globals at the start of main
        if f.name == "main" {
            self.emit_line("store i32 %__argc, ptr @__yorum_argc");
            self.emit_line("store ptr %__argv, ptr @__yorum_argv");
        }

        self.push_scope();

        // Alloca for each parameter
        for param in &f.params {
            // Array params are passed as ptr to { ptr, i64, i64 } — use directly
            if let Type::Array(ref inner) = param.ty {
                let elem_llvm_ty = self.llvm_type(inner);
                self.define_var(
                    &param.name,
                    &format!("%{}", param.name),
                    "{ ptr, i64, i64 }",
                );
                self.array_elem_types
                    .insert(param.name.clone(), elem_llvm_ty);
                continue;
            }
            let ty = self.llvm_type(&param.ty);
            let ptr = self.fresh_temp();
            self.emit_line(&format!("{} = alloca {}", ptr, ty));
            self.emit_line(&format!("store {} %{}, ptr {}", ty, param.name, ptr));
            self.define_var(&param.name, &ptr, &ty);

            // Track fn-typed params for indirect calls
            if let Type::Fn(_, _) = &param.ty {
                self.closure_var_types
                    .insert(param.name.clone(), param.ty.clone());
            }
            // Track tuple-typed params for field access
            if let Type::Tuple(_) = &param.ty {
                self.var_ast_types
                    .insert(param.name.clone(), param.ty.clone());
            }
        }

        // Emit requires checks after parameter allocas
        let contracts = f.contracts.clone();
        let fn_name = f.name.clone();
        self.emit_requires_checks(&contracts, &fn_name)?;

        // Alloca for result variable (used by ensures checks)
        if Self::has_ensures(&contracts) && ret_ty != "void" {
            let result_ptr = "%result.addr".to_string();
            self.emit_line(&format!("{} = alloca {}", result_ptr, ret_ty));
            self.emit_line(&format!("store {} 0, ptr {}", ret_ty, result_ptr));
            self.define_var("result", &result_ptr, &ret_ty);
        }

        // Emit body statements
        self.emit_block(&f.body)?;

        // If the block didn't terminate, add a default return
        if !self.block_terminated {
            if ret_ty == "void" {
                self.emit_line("ret void");
            } else {
                // Emit ensures checks for default return
                if Self::has_ensures(&contracts) {
                    self.emit_line(&format!("store {} 0, ptr %result.addr", ret_ty));
                    self.emit_ensures_checks(&contracts, &fn_name)?;
                }
                self.emit_line(&format!("ret {} 0", ret_ty));
            }
        }

        self.pop_scope();
        self.body.push_str("}\n\n");
        self.current_fn_ret_ty = None;

        // Emit any deferred closure functions
        for deferred in self.deferred_fns.drain(..) {
            self.body.push_str(&deferred);
        }

        Ok(())
    }

    fn emit_method(&mut self, target_type: &str, f: &FnDecl) -> Result<(), CodegenError> {
        self.temp_counter = 0;
        self.label_counter = 0;
        self.block_terminated = false;
        self.current_fn_contracts = f.contracts.clone();

        let mangled_name = format!("{}_{}", target_type, f.name);
        self.current_fn_name = mangled_name.clone();
        let ret_ty = self.llvm_type(&f.return_type);
        self.current_fn_ret_ty = Some(ret_ty.clone());

        let params: Vec<String> = f
            .params
            .iter()
            .map(|p| {
                let ty = self.llvm_type(&p.ty);
                // For self params that are references to structs, pass as ptr
                if matches!(&p.ty, Type::Ref(inner) | Type::MutRef(inner) if matches!(inner.as_ref(), Type::Named(_)))
                {
                    format!("ptr %{}", p.name)
                } else {
                    format!("{} %{}", ty, p.name)
                }
            })
            .collect();

        let dbg_suffix = if self.debug_enabled {
            let line = if f.span.line > 0 { f.span.line } else { 1 };
            let sp_id = self.emit_debug_subprogram(&mangled_name, line);
            format!(" !dbg !{}", sp_id)
        } else {
            String::new()
        };

        let inline_attr = if Self::should_inline(f) {
            self.has_inline_fns = true;
            " #0"
        } else {
            ""
        };

        self.body.push_str(&format!(
            "define {} @{}({}){}{} {{\n",
            ret_ty,
            mangled_name,
            params.join(", "),
            dbg_suffix,
            inline_attr
        ));
        self.body.push_str("entry:\n");

        self.push_scope();

        for param in &f.params {
            // For self: &StructType — store the pointer itself
            if let Type::Ref(inner) | Type::MutRef(inner) = &param.ty {
                if let Type::Named(sname) = inner.as_ref() {
                    let llvm_ty = format!("%{}", sname);
                    // self is already a ptr, use it directly
                    self.define_var(&param.name, &format!("%{}", param.name), &llvm_ty);
                    continue;
                }
            }
            let ty = self.llvm_type(&param.ty);
            let ptr = self.fresh_temp();
            self.emit_line(&format!("{} = alloca {}", ptr, ty));
            self.emit_line(&format!("store {} %{}, ptr {}", ty, param.name, ptr));
            self.define_var(&param.name, &ptr, &ty);
        }

        self.emit_block(&f.body)?;

        if !self.block_terminated {
            if ret_ty == "void" {
                self.emit_line("ret void");
            } else {
                self.emit_line(&format!("ret {} 0", ret_ty));
            }
        }

        self.pop_scope();
        self.body.push_str("}\n\n");
        self.current_fn_ret_ty = None;

        // Emit any deferred closure functions
        for deferred in self.deferred_fns.drain(..) {
            self.body.push_str(&deferred);
        }

        Ok(())
    }

    fn emit_block(&mut self, block: &Block) -> Result<(), CodegenError> {
        for stmt in &block.stmts {
            if self.block_terminated {
                break; // Dead code after return/branch
            }
            self.emit_stmt(stmt)?;
        }
        Ok(())
    }

    // ── Statement emission ───────────────────────────────────

    fn emit_stmt(&mut self, stmt: &Stmt) -> Result<(), CodegenError> {
        match stmt {
            Stmt::Let(s) => self.emit_let(s),
            Stmt::Assign(s) => self.emit_assign(s),
            Stmt::Return(s) => self.emit_return(s),
            Stmt::If(s) => self.emit_if(s),
            Stmt::While(s) => self.emit_while(s),
            Stmt::For(s) => self.emit_for(s),
            Stmt::Match(s) => self.emit_match(s),
            Stmt::Expr(s) => {
                self.emit_expr(&s.expr)?;
                Ok(())
            }
            Stmt::Break(_) => {
                if let Some((_, break_label)) = self.loop_labels.last() {
                    let break_label = break_label.clone();
                    self.emit_line(&format!("br label %{}", break_label));
                    self.block_terminated = true;
                }
                Ok(())
            }
            Stmt::Continue(_) => {
                if let Some((continue_label, _)) = self.loop_labels.last() {
                    let continue_label = continue_label.clone();
                    self.emit_line(&format!("br label %{}", continue_label));
                    self.block_terminated = true;
                }
                Ok(())
            }
        }
    }

    fn emit_let(&mut self, s: &LetStmt) -> Result<(), CodegenError> {
        let ty = self.llvm_type(&s.ty);

        // Unit-typed let bindings: evaluate the RHS for side effects only.
        // `alloca void` and `store void` are invalid LLVM IR.
        if s.ty == Type::Unit {
            self.emit_expr(&s.value)?;
            return Ok(());
        }

        // For struct-typed variables, StructInit already creates an alloca and
        // fills in the fields.  Use that alloca directly as the variable slot
        // instead of trying to store a whole struct as a value.
        if let Type::Named(ref name) = s.ty {
            if self.struct_layouts.contains_key(name) {
                let returns_ptr = self.expr_returns_ptr(&s.value);
                let val = self.emit_expr(&s.value)?;
                if returns_ptr {
                    // StructInit: reuse alloca directly
                    self.define_var(&s.name, &val, &ty);
                } else {
                    // Function call or variable: value returned, need alloca + store
                    let ptr = self.fresh_temp();
                    self.emit_line(&format!("{} = alloca {}", ptr, ty));
                    self.emit_line(&format!("store {} {}, ptr {}", ty, val, ptr));
                    self.define_var(&s.name, &ptr, &ty);
                }
                return Ok(());
            }
            // Enum-typed let
            if self.enum_layouts.contains_key(name) {
                self.current_expected_enum = Some(name.clone());
                let returns_ptr = self.expr_returns_ptr(&s.value);
                let val = self.emit_expr(&s.value)?;
                self.current_expected_enum = None;
                if returns_ptr {
                    // Variant constructor: reuse alloca directly
                    self.define_var(&s.name, &val, &ty);
                } else {
                    // Function call or variable: value returned, need alloca + store
                    let ptr = self.fresh_temp();
                    self.emit_line(&format!("{} = alloca {}", ptr, ty));
                    self.emit_line(&format!("store {} {}, ptr {}", ty, val, ptr));
                    self.define_var(&s.name, &ptr, &ty);
                }
                return Ok(());
            }
        }

        // For tuple-typed variables, TupleLit already creates an alloca.
        // Reuse that alloca directly as the variable slot.
        if let Type::Tuple(ref elem_types) = s.ty {
            let returns_ptr = self.expr_returns_ptr(&s.value);
            let val = self.emit_expr(&s.value)?;
            let val_ptr = if returns_ptr {
                val
            } else {
                // Use fresh_temp() to avoid duplicate names when multiple
                // tuple let bindings or destructures exist in one function.
                let ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca {}", ptr, ty));
                self.emit_line(&format!("store {} {}, ptr {}", ty, val, ptr));
                ptr
            };

            // Handle destructuring: let (a, b): (int, string) = expr;
            if let Some(ref names) = s.destructure {
                let tuple_name = self.tuple_type_name(elem_types);
                // Ensure the tuple type is defined
                let llvm_elem_types: Vec<String> =
                    elem_types.iter().map(|t| self.llvm_type(t)).collect();
                let llvm_fields = llvm_elem_types.join(", ");
                let type_def = format!("%{} = type {{ {} }}\n", tuple_name, llvm_fields);
                if !self.type_defs.contains(&format!("%{} = type", tuple_name)) {
                    self.type_defs.push_str(&type_def);
                }
                self.tuple_elem_types
                    .insert(tuple_name.clone(), llvm_elem_types.clone());

                for (i, name) in names.iter().enumerate() {
                    let elem_ty = &llvm_elem_types[i];
                    let gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                        gep, tuple_name, val_ptr, i
                    ));
                    let loaded = self.fresh_temp();
                    self.emit_line(&format!("{} = load {}, ptr {}", loaded, elem_ty, gep));
                    let ptr = self.fresh_temp();
                    self.emit_line(&format!("{} = alloca {}", ptr, elem_ty));
                    self.emit_line(&format!("store {} {}, ptr {}", elem_ty, loaded, ptr));
                    self.define_var(name, &ptr, elem_ty);
                    self.var_ast_types
                        .insert(name.clone(), elem_types[i].clone());
                }
                return Ok(());
            }

            self.define_var(&s.name, &val_ptr, &ty);
            self.var_ast_types.insert(s.name.clone(), s.ty.clone());
            return Ok(());
        }

        // For array-typed variables, ArrayLit creates a { ptr, i64, i64 } alloca.
        // Reuse that alloca directly.
        if let Type::Array(ref inner) = s.ty {
            let elem_llvm_ty = self.llvm_type(inner);
            let val_ptr = self.emit_expr(&s.value)?;
            self.define_var(&s.name, &val_ptr, "{ ptr, i64, i64 }");
            self.array_elem_types.insert(s.name.clone(), elem_llvm_ty);
            return Ok(());
        }

        // Track fn-typed variables for indirect calls
        if let Type::Fn(_, _) = &s.ty {
            self.closure_var_types.insert(s.name.clone(), s.ty.clone());
        }

        let ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", ptr, ty));

        let val = self.emit_expr(&s.value)?;
        self.emit_line(&format!("store {} {}, ptr {}", ty, val, ptr));
        self.define_var(&s.name, &ptr, &ty);

        // String buffer tracking: create len/cap allocas for string variables
        if s.ty == Type::Str {
            let len_ptr = self.fresh_temp();
            self.emit_line(&format!("{} = alloca i64", len_ptr));
            let cap_ptr = self.fresh_temp();
            self.emit_line(&format!("{} = alloca i64", cap_ptr));

            // Determine initial length from string literal, else 0
            let init_len = if let ExprKind::Literal(Literal::String(ref lit)) = s.value.kind {
                lit.len() as i64
            } else {
                0
            };
            self.emit_line(&format!("store i64 {}, ptr {}", init_len, len_ptr));
            // cap = 0 means "not an owned heap buffer yet"
            self.emit_line(&format!("store i64 0, ptr {}", cap_ptr));
            self.string_buf_vars
                .insert(ptr.clone(), StringBufInfo { len_ptr, cap_ptr });
        }

        // If the RHS was a spawn, record the env info for .join() to use
        if let Some(env_info) = self.last_spawn_env_info.take() {
            self.task_env_info.insert(s.name.clone(), env_info);
        }

        Ok(())
    }

    fn emit_assign(&mut self, s: &AssignStmt) -> Result<(), CodegenError> {
        match &s.target.kind {
            ExprKind::Ident(name) => {
                let slot = self
                    .lookup_var(name)
                    .ok_or_else(|| CodegenError {
                        message: format!("undefined variable '{}'", name),
                    })?
                    .clone();

                // Detect s = str_concat(s, literal) for capacity-aware inline concat.
                // Only applied when the suffix is a string literal (provably non-aliasing).
                if let ExprKind::Call(callee, args) = &s.value.kind {
                    if let ExprKind::Ident(fn_name) = &callee.kind {
                        if fn_name == "str_concat"
                            && args.len() == 2
                            && matches!(&args[0].kind, ExprKind::Ident(a) if a == name)
                            && matches!(&args[1].kind, ExprKind::Literal(Literal::String(_)))
                            && self.string_buf_vars.contains_key(&slot.ptr)
                        {
                            return self.emit_str_concat_inplace(name, &args[1]);
                        }
                    }
                }

                // If assigning a new (non-concat) value to a tracked string var,
                // reset len/cap to 0 so the next inline concat re-initializes
                if slot.llvm_ty == "ptr" {
                    if let Some(buf_info) = self.string_buf_vars.get(&slot.ptr) {
                        let len_ptr = buf_info.len_ptr.clone();
                        let cap_ptr = buf_info.cap_ptr.clone();
                        let val = self.emit_expr(&s.value)?;
                        self.emit_line(&format!("store ptr {}, ptr {}", val, slot.ptr));
                        self.emit_line(&format!("store i64 0, ptr {}", len_ptr));
                        self.emit_line(&format!("store i64 0, ptr {}", cap_ptr));
                        return Ok(());
                    }
                }
                let val = self.emit_expr(&s.value)?;
                // Unit-typed assignment: evaluate RHS for side effects only.
                // `store void` is invalid LLVM IR.
                if slot.llvm_ty == "void" {
                    return Ok(());
                }
                self.emit_line(&format!("store {} {}, ptr {}", slot.llvm_ty, val, slot.ptr));
                Ok(())
            }
            ExprKind::FieldAccess(obj, field) => {
                let obj_ptr = self.emit_expr_ptr(obj)?;
                let struct_name = self.expr_struct_name(obj)?;
                let (idx, field_ty) = self.struct_field_index(&struct_name, field)?;
                let fty = self.llvm_type(&field_ty);

                let gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                    gep, struct_name, obj_ptr, idx
                ));
                let val = self.emit_expr(&s.value)?;
                self.emit_line(&format!("store {} {}, ptr {}", fty, val, gep));
                Ok(())
            }
            ExprKind::Index(arr_expr, idx_expr) => {
                // arr[idx] = value
                let arr_name = if let ExprKind::Ident(name) = &arr_expr.kind {
                    name.clone()
                } else {
                    return Err(CodegenError {
                        message: "index assignment target must be a variable".to_string(),
                    });
                };
                let elem_ty = self
                    .array_elem_types
                    .get(&arr_name)
                    .cloned()
                    .unwrap_or_else(|| "i64".to_string());
                let arr_ptr = self.emit_expr_ptr(arr_expr)?;

                // Load data pointer from { ptr, i64, i64 }
                let data_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                    data_gep, arr_ptr
                ));
                let data_ptr = self.fresh_temp();
                self.emit_line(&format!("{} = load ptr, ptr {}", data_ptr, data_gep));

                // Load length for bounds check
                let len_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                    len_gep, arr_ptr
                ));
                let len_val = self.fresh_temp();
                self.emit_line(&format!("{} = load i64, ptr {}", len_val, len_gep));

                let idx_val = self.emit_expr(idx_expr)?;
                self.emit_line(&format!(
                    "call void @__yorum_bounds_check(i64 {}, i64 {})",
                    idx_val, len_val
                ));

                let elem_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {}, ptr {}, i64 {}",
                    elem_gep, elem_ty, data_ptr, idx_val
                ));
                let val = self.emit_expr(&s.value)?;
                self.emit_line(&format!("store {} {}, ptr {}", elem_ty, val, elem_gep));
                Ok(())
            }
            _ => Err(CodegenError {
                message: "invalid assignment target".to_string(),
            }),
        }
    }

    /// Emit inline capacity-aware string concatenation for `name = str_concat(name, suffix)`.
    ///
    /// Instead of calling `@str_concat` (which mallocs a new buffer every time),
    /// this tracks {len, cap} per variable and uses realloc for amortized O(1) appends.
    fn emit_str_concat_inplace(
        &mut self,
        name: &str,
        suffix_expr: &Expr,
    ) -> Result<(), CodegenError> {
        let slot = self
            .lookup_var(name)
            .ok_or_else(|| CodegenError {
                message: format!("undefined variable '{}'", name),
            })?
            .clone();
        let buf_info = self.string_buf_vars.get(&slot.ptr).unwrap().clone();

        // Load current data, len, cap
        let data = self.fresh_temp();
        self.emit_line(&format!("{} = load ptr, ptr {}", data, slot.ptr));
        let len = self.fresh_temp();
        self.emit_line(&format!("{} = load i64, ptr {}", len, buf_info.len_ptr));
        let cap = self.fresh_temp();
        self.emit_line(&format!("{} = load i64, ptr {}", cap, buf_info.cap_ptr));

        // Evaluate suffix and get its length
        let suffix_val = self.emit_expr(suffix_expr)?;
        let suffix_len = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call i64 @strlen(ptr {})",
            suffix_len, suffix_val
        ));

        // Compute new_len and need (new_len + 1 for null terminator)
        let new_len = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, {}", new_len, len, suffix_len));
        let need = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", need, new_len));

        // Branch on cap == 0 (uninitialized: literal or function result)
        let cap_zero = self.fresh_temp();
        self.emit_line(&format!("{} = icmp eq i64 {}, 0", cap_zero, cap));
        let lbl_init = self.fresh_label("sbuf_init");
        let lbl_check_grow = self.fresh_label("sbuf_check_grow");
        let lbl_grow = self.fresh_label("sbuf_grow");
        let lbl_append = self.fresh_label("sbuf_append");
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cap_zero, lbl_init, lbl_check_grow
        ));
        self.block_terminated = true;

        // --- init_buf: first time, malloc a new buffer and copy old data ---
        self.emit_label(&lbl_init);
        // Compute actual data length first (tracked len may be 0 for non-literal init)
        let old_len = self.fresh_temp();
        self.emit_line(&format!("{} = call i64 @strlen(ptr {})", old_len, data));
        let real_new_len = self.fresh_temp();
        self.emit_line(&format!(
            "{} = add i64 {}, {}",
            real_new_len, old_len, suffix_len
        ));
        let real_need = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", real_need, real_new_len));
        // init_cap = max(real_need * 2, 64)
        let need_x2 = self.fresh_temp();
        self.emit_line(&format!("{} = mul i64 {}, 2", need_x2, real_need));
        let cmp_64 = self.fresh_temp();
        self.emit_line(&format!("{} = icmp ugt i64 {}, 64", cmp_64, need_x2));
        let init_cap = self.fresh_temp();
        self.emit_line(&format!(
            "{} = select i1 {}, i64 {}, i64 64",
            init_cap, cmp_64, need_x2
        ));
        let init_buf = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call ptr @malloc(i64 {})",
            init_buf, init_cap
        ));
        // Copy old data into new buffer
        self.emit_line(&format!(
            "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
            init_buf, data, old_len
        ));
        // Store updated data, len, cap
        self.emit_line(&format!("store ptr {}, ptr {}", init_buf, slot.ptr));
        self.emit_line(&format!("store i64 {}, ptr {}", init_cap, buf_info.cap_ptr));
        // Append suffix at old_len offset
        let init_dest = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            init_dest, init_buf, old_len
        ));
        self.emit_line(&format!(
            "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
            init_dest, suffix_val, suffix_len
        ));
        let init_end = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            init_end, init_buf, real_new_len
        ));
        self.emit_line(&format!("store i8 0, ptr {}", init_end));
        self.emit_line(&format!(
            "store i64 {}, ptr {}",
            real_new_len, buf_info.len_ptr
        ));
        self.emit_line(&format!("br label %{}", lbl_append));
        self.block_terminated = true;

        // --- check_grow: cap > 0, check if need > cap ---
        self.emit_label(&lbl_check_grow);
        let need_grow = self.fresh_temp();
        self.emit_line(&format!("{} = icmp ugt i64 {}, {}", need_grow, need, cap));
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            need_grow, lbl_grow, lbl_append
        ));
        self.block_terminated = true;

        // --- grow: realloc to max(need, cap * 2) ---
        self.emit_label(&lbl_grow);
        let cap_x2 = self.fresh_temp();
        self.emit_line(&format!("{} = mul i64 {}, 2", cap_x2, cap));
        let cmp_grow = self.fresh_temp();
        self.emit_line(&format!("{} = icmp ugt i64 {}, {}", cmp_grow, need, cap_x2));
        let grow_cap = self.fresh_temp();
        self.emit_line(&format!(
            "{} = select i1 {}, i64 {}, i64 {}",
            grow_cap, cmp_grow, need, cap_x2
        ));
        let grow_buf = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call ptr @realloc(ptr {}, i64 {})",
            grow_buf, data, grow_cap
        ));
        self.emit_line(&format!("store ptr {}, ptr {}", grow_buf, slot.ptr));
        self.emit_line(&format!("store i64 {}, ptr {}", grow_cap, buf_info.cap_ptr));
        // Append suffix at len offset
        let grow_dest = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            grow_dest, grow_buf, len
        ));
        self.emit_line(&format!(
            "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
            grow_dest, suffix_val, suffix_len
        ));
        let grow_end = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            grow_end, grow_buf, new_len
        ));
        self.emit_line(&format!("store i8 0, ptr {}", grow_end));
        self.emit_line(&format!("store i64 {}, ptr {}", new_len, buf_info.len_ptr));
        self.emit_line(&format!("br label %{}", lbl_append));
        self.block_terminated = true;

        // --- append: no-grow path, cap > 0 and need <= cap ---
        self.emit_label(&lbl_append);
        // Reload data from alloca (may have changed in init or grow)
        let final_data = self.fresh_temp();
        self.emit_line(&format!("{} = load ptr, ptr {}", final_data, slot.ptr));
        let final_len = self.fresh_temp();
        self.emit_line(&format!(
            "{} = load i64, ptr {}",
            final_len, buf_info.len_ptr
        ));
        // Check if we already handled this in init/grow (len was updated there)
        let already_done = self.fresh_temp();
        self.emit_line(&format!(
            "{} = icmp ne i64 {}, {}",
            already_done, final_len, len
        ));
        let lbl_do_append = self.fresh_label("sbuf_do_append");
        let lbl_done = self.fresh_label("sbuf_done");
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            already_done, lbl_done, lbl_do_append
        ));
        self.block_terminated = true;

        // --- do_append: the no-realloc/no-init path ---
        self.emit_label(&lbl_do_append);
        let app_dest = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            app_dest, final_data, len
        ));
        self.emit_line(&format!(
            "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
            app_dest, suffix_val, suffix_len
        ));
        let app_end = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr i8, ptr {}, i64 {}",
            app_end, final_data, new_len
        ));
        self.emit_line(&format!("store i8 0, ptr {}", app_end));
        self.emit_line(&format!("store i64 {}, ptr {}", new_len, buf_info.len_ptr));
        self.emit_line(&format!("br label %{}", lbl_done));
        self.block_terminated = true;

        // --- done ---
        self.emit_label(&lbl_done);

        Ok(())
    }

    fn emit_return(&mut self, s: &ReturnStmt) -> Result<(), CodegenError> {
        // In spawn context, the user's `return X` stores the result into the
        // env struct's result slot and emits `ret ptr null` for the pthread
        // wrapper ABI. The actual result is read by .join() from the env.
        if let Some((ref env_type, result_idx)) = self.spawn_return_ctx.clone() {
            let val = self.emit_expr(&s.value)?;
            let result_gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr %env, i32 0, i32 {}",
                result_gep, env_type, result_idx
            ));
            self.emit_line(&format!("store i64 {}, ptr {}", val, result_gep));
            self.emit_line("ret ptr null");
            self.block_terminated = true;
            return Ok(());
        }

        let ret_ty = self.current_fn_ret_ty.clone().unwrap_or("void".to_string());
        if ret_ty == "void" {
            self.emit_line("ret void");
        } else {
            // Tail call optimization: detect `return f(args)` pattern
            let is_tail_call = {
                let contracts = &self.current_fn_contracts;
                let no_ensures = !Self::has_ensures(contracts);
                let is_simple_call = matches!(
                    &s.value.kind,
                    ExprKind::Call(callee, _) if matches!(&callee.kind, ExprKind::Ident(_))
                );
                let no_ptr_load = !(ret_ty.starts_with('%') && self.expr_returns_ptr(&s.value));
                no_ensures && is_simple_call && no_ptr_load
            };
            if is_tail_call {
                self.tail_call_hint = true;
            }

            // Set expected enum hint for return value (e.g., return Some(42);)
            if let Some(stripped) = ret_ty.strip_prefix('%') {
                if self.enum_layouts.contains_key(stripped) {
                    self.current_expected_enum = Some(stripped.to_string());
                }
            }
            let val = self.emit_expr(&s.value)?;
            self.current_expected_enum = None;
            self.tail_call_hint = false;

            // Enum/struct variant constructors return alloca pointers (ptr type),
            // but `ret %EnumType %ptr` is invalid — need to load the value first
            let val = if ret_ty.starts_with('%') && self.expr_returns_ptr(&s.value) {
                let loaded = self.fresh_temp();
                self.emit_line(&format!("{} = load {}, ptr {}", loaded, ret_ty, val));
                loaded
            } else {
                val
            };

            // Emit ensures checks before the actual return
            let contracts = self.current_fn_contracts.clone();
            let fn_name = self.current_fn_name.clone();
            if Self::has_ensures(&contracts) {
                // Store the return value into the result slot
                self.emit_line(&format!("store {} {}, ptr %result.addr", ret_ty, val));
                self.emit_ensures_checks(&contracts, &fn_name)?;
                // Reload the value (it hasn't changed, but keeps IR correct)
                let reloaded = self.fresh_temp();
                self.emit_line(&format!("{} = load {}, ptr %result.addr", reloaded, ret_ty));
                self.emit_line(&format!("ret {} {}", ret_ty, reloaded));
            } else {
                self.emit_line(&format!("ret {} {}", ret_ty, val));
            }
        }
        self.block_terminated = true;
        Ok(())
    }

    fn emit_if(&mut self, s: &IfStmt) -> Result<(), CodegenError> {
        let cond = self.emit_expr(&s.condition)?;
        let then_label = self.fresh_label("then");
        let else_label = self.fresh_label("else");
        let merge_label = self.fresh_label("ifcont");

        let has_else = s.else_branch.is_some();
        let target_else = if has_else { &else_label } else { &merge_label };

        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cond, then_label, target_else
        ));

        // Then block
        self.emit_label(&then_label);
        self.block_terminated = false;
        self.push_scope();
        self.emit_block(&s.then_block)?;
        self.pop_scope();
        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", merge_label));
        }
        let then_terminated = self.block_terminated;

        // Else block
        if let Some(else_branch) = &s.else_branch {
            self.emit_label(&else_label);
            self.block_terminated = false;
            self.push_scope();
            match else_branch.as_ref() {
                ElseBranch::Else(block) => {
                    self.emit_block(block)?;
                }
                ElseBranch::ElseIf(elif) => {
                    self.emit_if(elif)?;
                }
            }
            self.pop_scope();
            if !self.block_terminated {
                self.emit_line(&format!("br label %{}", merge_label));
            }
            let else_terminated = self.block_terminated;

            // Merge block — only needed if at least one branch falls through
            if !then_terminated || !else_terminated {
                self.emit_label(&merge_label);
                self.block_terminated = false;
            } else {
                // Both branches returned; emit merge block as dead code landing pad
                self.emit_label(&merge_label);
                self.block_terminated = false;
            }
        } else {
            // No else branch
            self.emit_label(&merge_label);
            self.block_terminated = false;
        }

        Ok(())
    }

    fn emit_while(&mut self, s: &WhileStmt) -> Result<(), CodegenError> {
        let cond_label = self.fresh_label("while.cond");
        let body_label = self.fresh_label("while.body");
        let end_label = self.fresh_label("while.end");

        self.emit_line(&format!("br label %{}", cond_label));

        // Condition check
        self.emit_label(&cond_label);
        self.block_terminated = false;
        let cond = self.emit_expr(&s.condition)?;
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cond, body_label, end_label
        ));

        // Loop body
        self.emit_label(&body_label);
        self.block_terminated = false;
        self.loop_labels
            .push((cond_label.clone(), end_label.clone()));
        self.push_scope();
        self.emit_block(&s.body)?;
        self.pop_scope();
        self.loop_labels.pop();
        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", cond_label));
        }

        // End
        self.emit_label(&end_label);
        self.block_terminated = false;
        Ok(())
    }

    fn emit_for(&mut self, s: &ForStmt) -> Result<(), CodegenError> {
        // Range-based for loop: for i in start..end
        if let ExprKind::Range(ref start, ref end) = s.iterable.kind {
            return self.emit_for_range(&s.var_name, start, end, false, &s.body);
        }
        // Inclusive range: for i in start..=end
        if let ExprKind::RangeInclusive(ref start, ref end) = s.iterable.kind {
            return self.emit_for_range(&s.var_name, start, end, true, &s.body);
        }
        // .iter() on arrays: delegate to array for-loop codegen.
        // Only rewrite when the receiver is a known array variable (or literal),
        // so that user-defined iter() methods on structs fall through to normal codegen.
        if let ExprKind::MethodCall(ref receiver, ref method, ref args) = s.iterable.kind {
            if method == "iter" && args.is_empty() {
                let is_array_receiver = match &receiver.kind {
                    ExprKind::Ident(name) => self.array_elem_types.contains_key(name),
                    ExprKind::ArrayLit(_) => true,
                    _ => false,
                };
                if is_array_receiver {
                    let arr_for = ForStmt {
                        var_name: s.var_name.clone(),
                        iterable: *receiver.clone(),
                        body: s.body.clone(),
                        span: s.span,
                    };
                    return self.emit_for(&arr_for);
                }
            }
        }

        // Evaluate the iterable (array fat pointer)
        let arr_val = self.emit_expr(&s.iterable)?;

        // Determine the array variable name to look up elem type
        let elem_ty = if let ExprKind::Ident(name) = &s.iterable.kind {
            self.array_elem_types
                .get(name)
                .cloned()
                .unwrap_or_else(|| "i64".to_string())
        } else {
            "i64".to_string()
        };

        // Load data ptr and length from { ptr, i64, i64 }
        let data_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
            data_gep, arr_val
        ));
        let data_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = load ptr, ptr {}", data_ptr, data_gep));

        let len_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
            len_gep, arr_val
        ));
        let len_val = self.fresh_temp();
        self.emit_line(&format!("{} = load i64, ptr {}", len_val, len_gep));

        // Alloca for index counter
        let idx_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca i64", idx_ptr));
        self.emit_line(&format!("store i64 0, ptr {}", idx_ptr));

        // Alloca for loop variable — use fresh_temp() to avoid duplicate
        // names when the same variable name is used in multiple for loops.
        let var_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", var_ptr, elem_ty));

        let cond_label = self.fresh_label("for.cond");
        let body_label = self.fresh_label("for.body");
        let inc_label = self.fresh_label("for.inc");
        let end_label = self.fresh_label("for.end");

        self.emit_line(&format!("br label %{}", cond_label));

        // Condition: idx < len
        self.emit_label(&cond_label);
        self.block_terminated = false;
        let cur_idx = self.fresh_temp();
        self.emit_line(&format!("{} = load i64, ptr {}", cur_idx, idx_ptr));
        let cmp = self.fresh_temp();
        self.emit_line(&format!("{} = icmp slt i64 {}, {}", cmp, cur_idx, len_val));
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cmp, body_label, end_label
        ));

        // Body
        self.emit_label(&body_label);
        self.block_terminated = false;

        self.push_scope();

        // Load element at current index into loop variable
        let elem_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {}, ptr {}, i64 {}",
            elem_gep, elem_ty, data_ptr, cur_idx
        ));
        let elem_val = self.fresh_temp();
        self.emit_line(&format!(
            "{} = load {}, ptr {}",
            elem_val, elem_ty, elem_gep
        ));
        self.emit_line(&format!("store {} {}, ptr {}", elem_ty, elem_val, var_ptr));

        self.define_var(&s.var_name, &var_ptr, &elem_ty);

        // Push loop labels: continue → for.inc, break → for.end
        self.loop_labels
            .push((inc_label.clone(), end_label.clone()));
        self.emit_block(&s.body)?;
        self.loop_labels.pop();

        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", inc_label));
        }

        self.pop_scope();

        // Increment block
        self.emit_label(&inc_label);
        self.block_terminated = false;
        let next_idx_tmp = self.fresh_temp();
        let cur_idx2 = self.fresh_temp();
        self.emit_line(&format!("{} = load i64, ptr {}", cur_idx2, idx_ptr));
        self.emit_line(&format!("{} = add i64 {}, 1", next_idx_tmp, cur_idx2));
        self.emit_line(&format!("store i64 {}, ptr {}", next_idx_tmp, idx_ptr));
        self.emit_line(&format!("br label %{}", cond_label));

        self.emit_label(&end_label);
        self.block_terminated = false;
        Ok(())
    }

    fn emit_for_range(
        &mut self,
        var_name: &str,
        start_expr: &Expr,
        end_expr: &Expr,
        inclusive: bool,
        body: &Block,
    ) -> Result<(), CodegenError> {
        // Evaluate start and end bounds
        let start_val = self.emit_expr(start_expr)?;
        let end_val = self.emit_expr(end_expr)?;

        // Alloca for the loop counter (also the loop variable)
        // Use fresh_temp() to avoid duplicate names when the same variable
        // name is used in multiple for-range loops in one function.
        let var_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca i64", var_ptr));
        self.emit_line(&format!("store i64 {}, ptr {}", start_val, var_ptr));

        let cond_label = self.fresh_label("for.cond");
        let body_label = self.fresh_label("for.body");
        let inc_label = self.fresh_label("for.inc");
        let end_label = self.fresh_label("for.end");

        self.emit_line(&format!("br label %{}", cond_label));

        // Condition: counter < end (exclusive) or counter <= end (inclusive)
        self.emit_label(&cond_label);
        self.block_terminated = false;
        let cur_val = self.fresh_temp();
        self.emit_line(&format!("{} = load i64, ptr {}", cur_val, var_ptr));
        let cmp = self.fresh_temp();
        let cmp_op = if inclusive { "sle" } else { "slt" };
        self.emit_line(&format!(
            "{} = icmp {} i64 {}, {}",
            cmp, cmp_op, cur_val, end_val
        ));
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cmp, body_label, end_label
        ));

        // Body
        self.emit_label(&body_label);
        self.block_terminated = false;

        self.push_scope();
        self.define_var(var_name, &var_ptr, "i64");

        self.loop_labels
            .push((inc_label.clone(), end_label.clone()));
        self.emit_block(body)?;
        self.loop_labels.pop();

        if !self.block_terminated {
            self.emit_line(&format!("br label %{}", inc_label));
        }

        self.pop_scope();

        // Increment block
        self.emit_label(&inc_label);
        self.block_terminated = false;
        let cur_val2 = self.fresh_temp();
        self.emit_line(&format!("{} = load i64, ptr {}", cur_val2, var_ptr));
        if inclusive {
            // Guard against overflow: if counter == end, this was the last
            // iteration — exit instead of incrementing (which would wrap
            // i64::MAX to i64::MIN and loop forever).
            let at_end = self.fresh_temp();
            self.emit_line(&format!(
                "{} = icmp eq i64 {}, {}",
                at_end, cur_val2, end_val
            ));
            let inc_do_label = self.fresh_label("for.inc.do");
            self.emit_line(&format!(
                "br i1 {}, label %{}, label %{}",
                at_end, end_label, inc_do_label
            ));
            self.emit_label(&inc_do_label);
            self.block_terminated = false;
        }
        let next_val = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 {}, 1", next_val, cur_val2));
        self.emit_line(&format!("store i64 {}, ptr {}", next_val, var_ptr));
        self.emit_line(&format!("br label %{}", cond_label));

        self.emit_label(&end_label);
        self.block_terminated = false;
        Ok(())
    }

    fn emit_match(&mut self, s: &MatchStmt) -> Result<(), CodegenError> {
        // Determine if the subject is an enum type
        let subject_enum_name = self.expr_enum_name(&s.subject);
        let is_enum = subject_enum_name.is_some();

        // For enums, we need the alloca pointer (not the loaded value) for GEP
        let subject_val = if is_enum {
            self.emit_expr_ptr(&s.subject)?
        } else {
            self.emit_expr(&s.subject)?
        };
        let merge_label = self.fresh_label("match.end");

        // For enums: extract tag from alloca via GEP
        let tag_val = if is_enum {
            let tag_gep = self.fresh_temp();
            let enum_name = subject_enum_name.as_ref().unwrap();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr {}, i32 0, i32 0",
                tag_gep, enum_name, subject_val
            ));
            let tag = self.fresh_temp();
            self.emit_line(&format!("{} = load i32, ptr {}", tag, tag_gep));
            Some(tag)
        } else {
            None
        };

        let compare_val = if let Some(ref tv) = tag_val {
            tv.clone()
        } else {
            subject_val.clone()
        };

        let mut arm_labels: Vec<String> = Vec::new();
        let mut check_labels: Vec<String> = Vec::new();
        for _ in &s.arms {
            arm_labels.push(self.fresh_label("match.arm"));
            check_labels.push(self.fresh_label("match.check"));
        }
        let default_label = self.fresh_label("match.default");

        // Emit cascading comparisons
        for (i, arm) in s.arms.iter().enumerate() {
            let next = if i + 1 < s.arms.len() {
                check_labels[i + 1].clone()
            } else {
                default_label.clone()
            };
            match &arm.pattern {
                Pattern::Literal(Literal::Int(n), _) => {
                    let cmp = self.fresh_temp();
                    self.emit_line(&format!("{} = icmp eq i64 {}, {}", cmp, compare_val, n));
                    self.emit_line(&format!(
                        "br i1 {}, label %{}, label %{}",
                        cmp, arm_labels[i], next
                    ));
                    if i + 1 < s.arms.len() {
                        self.emit_label(&next);
                        self.block_terminated = false;
                    }
                }
                Pattern::Literal(Literal::Char(c), _) => {
                    let cmp = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = icmp eq i8 {}, {}",
                        cmp, compare_val, *c as u8
                    ));
                    self.emit_line(&format!(
                        "br i1 {}, label %{}, label %{}",
                        cmp, arm_labels[i], next
                    ));
                    if i + 1 < s.arms.len() {
                        self.emit_label(&next);
                        self.block_terminated = false;
                    }
                }
                Pattern::Wildcard(_) => {
                    self.emit_line(&format!("br label %{}", arm_labels[i]));
                }
                Pattern::Binding(bname, _) => {
                    // Check if this binding name is actually a no-data enum variant
                    if is_enum {
                        if let Ok(tag) = self.variant_tag(bname) {
                            let cmp = self.fresh_temp();
                            self.emit_line(&format!(
                                "{} = icmp eq i32 {}, {}",
                                cmp, compare_val, tag
                            ));
                            self.emit_line(&format!(
                                "br i1 {}, label %{}, label %{}",
                                cmp, arm_labels[i], next
                            ));
                            if i + 1 < s.arms.len() {
                                self.emit_label(&next);
                                self.block_terminated = false;
                            }
                        } else {
                            self.emit_line(&format!("br label %{}", arm_labels[i]));
                        }
                    } else {
                        self.emit_line(&format!("br label %{}", arm_labels[i]));
                    }
                }
                Pattern::Variant(vname, _, _) => {
                    let tag = self.variant_tag(vname)?;
                    let cmp = self.fresh_temp();
                    self.emit_line(&format!("{} = icmp eq i32 {}, {}", cmp, compare_val, tag));
                    self.emit_line(&format!(
                        "br i1 {}, label %{}, label %{}",
                        cmp, arm_labels[i], next
                    ));
                    if i + 1 < s.arms.len() {
                        self.emit_label(&next);
                        self.block_terminated = false;
                    }
                }
                _ => {
                    self.emit_line(&format!("br label %{}", arm_labels[i]));
                }
            }
        }

        // Default (unreachable if match is exhaustive)
        self.emit_label(&default_label);
        self.emit_line("unreachable");

        // Emit arm bodies
        let enum_name_clone = subject_enum_name.clone();
        for (i, arm) in s.arms.iter().enumerate() {
            self.emit_label(&arm_labels[i]);
            self.block_terminated = false;

            self.push_scope();

            // Bind pattern variables
            match &arm.pattern {
                Pattern::Binding(name, _) => {
                    // Don't bind if the name matches a no-data enum variant
                    let is_variant = is_enum && self.variant_tag(name).is_ok();
                    if !is_variant {
                        let ty_str = if is_enum {
                            "i32".to_string()
                        } else {
                            "i64".to_string()
                        };
                        let val = &compare_val;
                        let ptr = self.fresh_temp();
                        self.emit_line(&format!("{} = alloca {}", ptr, ty_str));
                        self.emit_line(&format!("store {} {}, ptr {}", ty_str, val, ptr));
                        self.define_var(name, &ptr, &ty_str);
                    }
                }
                Pattern::Variant(vname, sub_patterns, _) => {
                    // Extract payload from enum and bind sub-patterns
                    if let Some(ref ename) = enum_name_clone {
                        let layout = self.enum_layouts.get(ename).cloned();
                        if let Some(layout) = layout {
                            let variant_fields: Vec<Type> = layout
                                .variants
                                .iter()
                                .find(|(n, _)| n == vname)
                                .map(|(_, f)| f.clone())
                                .unwrap_or_default();

                            if !variant_fields.is_empty() {
                                // GEP into payload bytes
                                let payload_gep = self.fresh_temp();
                                self.emit_line(&format!(
                                    "{} = getelementptr %{}, ptr {}, i32 0, i32 1",
                                    payload_gep, ename, subject_val
                                ));

                                // Extract each field from payload and bind to sub-patterns
                                let mut byte_offset = 0usize;
                                for (fi, field_ty) in variant_fields.iter().enumerate() {
                                    let field_llvm_ty = self.llvm_type(field_ty);
                                    if fi < sub_patterns.len() {
                                        if let Pattern::Binding(bname, _) = &sub_patterns[fi] {
                                            let field_ptr = self.fresh_temp();
                                            self.emit_line(&format!(
                                                "{} = getelementptr [0 x i8], ptr {}, i64 0, i64 {}",
                                                field_ptr, payload_gep, byte_offset
                                            ));
                                            let field_val = self.fresh_temp();
                                            self.emit_line(&format!(
                                                "{} = load {}, ptr {}",
                                                field_val, field_llvm_ty, field_ptr
                                            ));
                                            let bind_ptr = self.fresh_temp();
                                            self.emit_line(&format!(
                                                "{} = alloca {}",
                                                bind_ptr, field_llvm_ty
                                            ));
                                            self.emit_line(&format!(
                                                "store {} {}, ptr {}",
                                                field_llvm_ty, field_val, bind_ptr
                                            ));
                                            self.define_var(bname, &bind_ptr, &field_llvm_ty);
                                        }
                                    }
                                    byte_offset += self.llvm_type_size(&field_llvm_ty);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }

            self.emit_block(&arm.body)?;
            if !self.block_terminated {
                self.emit_line(&format!("br label %{}", merge_label));
            }
            self.pop_scope();
        }

        self.emit_label(&merge_label);
        self.block_terminated = false;
        Ok(())
    }

    fn llvm_type_size(&self, llvm_ty: &str) -> usize {
        match llvm_ty {
            "i64" => 8,
            "double" => 8,
            "i1" => 1,
            "i8" => 1,
            "ptr" => 8,
            "i32" => 4,
            _ => {
                // Handle struct types like %Token
                if let Some(name) = llvm_ty.strip_prefix('%') {
                    if let Some(layout) = self.struct_layouts.get(name) {
                        return layout.fields.iter().map(|(_, t)| self.type_size(t)).sum();
                    }
                }
                8
            }
        }
    }

    fn is_aggregate_type(llvm_ty: &str) -> bool {
        llvm_ty.starts_with('%')
    }

    // ── Expression emission ──────────────────────────────────
    // Each `emit_expr` returns the LLVM value name (SSA temp or literal).

    // ── Constant folding ─────────────────────────────────────

    /// Try to evaluate a constant expression at compile time.
    /// Returns the LLVM literal string if fully foldable, None otherwise.
    fn try_const_fold(expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Literal(Literal::Int(n)) => Some(n.to_string()),
            ExprKind::Literal(Literal::Bool(b)) => {
                Some(if *b { "true" } else { "false" }.to_string())
            }
            ExprKind::Unary(UnaryOp::Neg, operand) => {
                if let Some(val) = Self::try_const_fold(operand) {
                    val.parse::<i64>()
                        .ok()
                        .map(|n| n.wrapping_neg().to_string())
                } else {
                    None
                }
            }
            ExprKind::Unary(UnaryOp::Not, operand) => {
                match Self::try_const_fold(operand)?.as_str() {
                    "true" => Some("false".to_string()),
                    "false" => Some("true".to_string()),
                    _ => None,
                }
            }
            ExprKind::Binary(lhs, op, rhs) => {
                let l = Self::try_const_fold(lhs)?;
                let r = Self::try_const_fold(rhs)?;
                // Try int arithmetic
                if let (Ok(li), Ok(ri)) = (l.parse::<i64>(), r.parse::<i64>()) {
                    return Self::fold_binary_int(li, op, ri);
                }
                // Try bool logic
                let lb = match l.as_str() {
                    "true" => Some(true),
                    "false" => Some(false),
                    _ => None,
                };
                let rb = match r.as_str() {
                    "true" => Some(true),
                    "false" => Some(false),
                    _ => None,
                };
                if let (Some(lb), Some(rb)) = (lb, rb) {
                    return match op {
                        BinOp::And => Some(if lb && rb { "true" } else { "false" }.to_string()),
                        BinOp::Or => Some(if lb || rb { "true" } else { "false" }.to_string()),
                        BinOp::Eq => Some(if lb == rb { "true" } else { "false" }.to_string()),
                        BinOp::NotEq => Some(if lb != rb { "true" } else { "false" }.to_string()),
                        _ => None,
                    };
                }
                None
            }
            _ => None,
        }
    }

    /// Fold a binary operation on two integer constants.
    fn fold_binary_int(l: i64, op: &BinOp, r: i64) -> Option<String> {
        match op {
            BinOp::Add => Some(l.wrapping_add(r).to_string()),
            BinOp::Sub => Some(l.wrapping_sub(r).to_string()),
            BinOp::Mul => Some(l.wrapping_mul(r).to_string()),
            BinOp::Div => {
                if r == 0 {
                    None
                } else {
                    l.checked_div(r).map(|v| v.to_string())
                }
            }
            BinOp::Mod => {
                if r == 0 {
                    None
                } else {
                    l.checked_rem(r).map(|v| v.to_string())
                }
            }
            BinOp::BitAnd => Some((l & r).to_string()),
            BinOp::BitOr => Some((l | r).to_string()),
            BinOp::BitXor => Some((l ^ r).to_string()),
            BinOp::Shl => Some(l.wrapping_shl(r as u32).to_string()),
            BinOp::Shr => Some(l.wrapping_shr(r as u32).to_string()),
            BinOp::Eq => Some(if l == r { "true" } else { "false" }.to_string()),
            BinOp::NotEq => Some(if l != r { "true" } else { "false" }.to_string()),
            BinOp::Lt => Some(if l < r { "true" } else { "false" }.to_string()),
            BinOp::Gt => Some(if l > r { "true" } else { "false" }.to_string()),
            BinOp::LtEq => Some(if l <= r { "true" } else { "false" }.to_string()),
            BinOp::GtEq => Some(if l >= r { "true" } else { "false" }.to_string()),
            BinOp::And | BinOp::Or => None, // and/or on ints not applicable
        }
    }

    // ── Expression emission ─────────────────────────────────

    fn emit_expr(&mut self, expr: &Expr) -> Result<String, CodegenError> {
        // Constant folding: evaluate compile-time constant expressions
        if let Some(folded) = Self::try_const_fold(expr) {
            return Ok(folded);
        }

        match &expr.kind {
            ExprKind::Literal(lit) => self.emit_literal(lit),

            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name).cloned() {
                    // Array variables: return the pointer directly (arrays are reference types)
                    if slot.llvm_ty == "{ ptr, i64, i64 }" {
                        return Ok(slot.ptr.clone());
                    }
                    let tmp = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = load {}, ptr {}",
                        tmp, slot.llvm_ty, slot.ptr
                    ));
                    Ok(tmp)
                } else if let Some(enum_name) = self.find_enum_for_variant(name) {
                    // Data-less enum variant (e.g., None)
                    self.emit_enum_variant_constructor(&enum_name, name, &[])
                } else {
                    Err(CodegenError {
                        message: format!("undefined variable '{}'", name),
                    })
                }
            }

            ExprKind::Binary(lhs, op, rhs) => {
                // Short-circuit evaluation for `and` and `or`
                if *op == BinOp::And {
                    let l = self.emit_expr(lhs)?;
                    let rhs_label = self.fresh_label("and_rhs");
                    let merge_label = self.fresh_label("and_merge");
                    let lhs_block = self.current_block_label();
                    self.emit_line(&format!(
                        "br i1 {}, label %{}, label %{}",
                        l, rhs_label, merge_label
                    ));
                    self.emit_label(&rhs_label);
                    self.block_terminated = false;
                    let r = self.emit_expr(rhs)?;
                    let rhs_block = self.current_block_label();
                    self.emit_line(&format!("br label %{}", merge_label));
                    self.emit_label(&merge_label);
                    self.block_terminated = false;
                    let result = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = phi i1 [ false, %{} ], [ {}, %{} ]",
                        result, lhs_block, r, rhs_block
                    ));
                    return Ok(result);
                }
                if *op == BinOp::Or {
                    let l = self.emit_expr(lhs)?;
                    let rhs_label = self.fresh_label("or_rhs");
                    let merge_label = self.fresh_label("or_merge");
                    let lhs_block = self.current_block_label();
                    self.emit_line(&format!(
                        "br i1 {}, label %{}, label %{}",
                        l, merge_label, rhs_label
                    ));
                    self.emit_label(&rhs_label);
                    self.block_terminated = false;
                    let r = self.emit_expr(rhs)?;
                    let rhs_block = self.current_block_label();
                    self.emit_line(&format!("br label %{}", merge_label));
                    self.emit_label(&merge_label);
                    self.block_terminated = false;
                    let result = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = phi i1 [ true, %{} ], [ {}, %{} ]",
                        result, lhs_block, r, rhs_block
                    ));
                    return Ok(result);
                }

                let l = self.emit_expr(lhs)?;
                let r = self.emit_expr(rhs)?;
                let tmp = self.fresh_temp();

                // Determine the operand type from the LHS
                let is_float = self.expr_is_float(lhs);
                let is_char = !is_float && self.expr_is_char(lhs);
                // Integer type string: "i8" for char, "i64" for int
                let ity = if is_char { "i8" } else { "i64" };

                let instr = match (op, is_float) {
                    (BinOp::Add, false) => format!("{} = add {} {}, {}", tmp, ity, l, r),
                    (BinOp::Add, true) => format!("{} = fadd double {}, {}", tmp, l, r),
                    (BinOp::Sub, false) => format!("{} = sub {} {}, {}", tmp, ity, l, r),
                    (BinOp::Sub, true) => format!("{} = fsub double {}, {}", tmp, l, r),
                    (BinOp::Mul, false) => format!("{} = mul {} {}, {}", tmp, ity, l, r),
                    (BinOp::Mul, true) => format!("{} = fmul double {}, {}", tmp, l, r),
                    (BinOp::Div, false) => format!("{} = sdiv {} {}, {}", tmp, ity, l, r),
                    (BinOp::Div, true) => format!("{} = fdiv double {}, {}", tmp, l, r),
                    (BinOp::Mod, false) => format!("{} = srem {} {}, {}", tmp, ity, l, r),
                    (BinOp::Mod, true) => format!("{} = frem double {}, {}", tmp, l, r),
                    (BinOp::Eq, false) => format!("{} = icmp eq {} {}, {}", tmp, ity, l, r),
                    (BinOp::Eq, true) => format!("{} = fcmp oeq double {}, {}", tmp, l, r),
                    (BinOp::NotEq, false) => format!("{} = icmp ne {} {}, {}", tmp, ity, l, r),
                    (BinOp::NotEq, true) => format!("{} = fcmp one double {}, {}", tmp, l, r),
                    (BinOp::Lt, false) => format!("{} = icmp slt {} {}, {}", tmp, ity, l, r),
                    (BinOp::Lt, true) => format!("{} = fcmp olt double {}, {}", tmp, l, r),
                    (BinOp::Gt, false) => format!("{} = icmp sgt {} {}, {}", tmp, ity, l, r),
                    (BinOp::Gt, true) => format!("{} = fcmp ogt double {}, {}", tmp, l, r),
                    (BinOp::LtEq, false) => format!("{} = icmp sle {} {}, {}", tmp, ity, l, r),
                    (BinOp::LtEq, true) => format!("{} = fcmp ole double {}, {}", tmp, l, r),
                    (BinOp::GtEq, false) => format!("{} = icmp sge {} {}, {}", tmp, ity, l, r),
                    (BinOp::GtEq, true) => format!("{} = fcmp oge double {}, {}", tmp, l, r),
                    (BinOp::BitAnd, _) => format!("{} = and i64 {}, {}", tmp, l, r),
                    (BinOp::BitOr, _) => format!("{} = or i64 {}, {}", tmp, l, r),
                    (BinOp::BitXor, _) => format!("{} = xor i64 {}, {}", tmp, l, r),
                    (BinOp::Shl, _) => format!("{} = shl i64 {}, {}", tmp, l, r),
                    (BinOp::Shr, _) => format!("{} = ashr i64 {}, {}", tmp, l, r),
                    (BinOp::And, _) | (BinOp::Or, _) => unreachable!(),
                };
                self.emit_line(&instr);
                Ok(tmp)
            }

            ExprKind::Unary(op, operand) => {
                let val = self.emit_expr(operand)?;
                let tmp = self.fresh_temp();
                let is_float = self.expr_is_float(operand);
                match op {
                    UnaryOp::Neg if is_float => {
                        self.emit_line(&format!("{} = fneg double {}", tmp, val));
                    }
                    UnaryOp::Neg => {
                        self.emit_line(&format!("{} = sub i64 0, {}", tmp, val));
                    }
                    UnaryOp::Not => {
                        self.emit_line(&format!("{} = xor i1 {}, true", tmp, val));
                    }
                }
                Ok(tmp)
            }

            ExprKind::Call(callee, args) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    // Built-in len() for arrays
                    if name == "len" && args.len() == 1 {
                        let arr_ptr = self.emit_expr_ptr(&args[0])?;
                        let len_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                            len_gep, arr_ptr
                        ));
                        let len_val = self.fresh_temp();
                        self.emit_line(&format!("{} = load i64, ptr {}", len_val, len_gep));
                        return Ok(len_val);
                    }
                    // Built-in concurrency functions
                    if name == "chan" {
                        return self.emit_chan_create();
                    }
                    if name == "send" && args.len() == 2 {
                        return self.emit_chan_send(&args[0], &args[1]);
                    }
                    if name == "recv" && args.len() == 1 {
                        return self.emit_chan_recv(&args[0]);
                    }

                    // Built-in push() for dynamic arrays
                    if name == "push" && args.len() == 2 {
                        let arr_ptr = self.emit_expr(&args[0])?;
                        let elem_ty = if let ExprKind::Ident(arr_name) = &args[0].kind {
                            self.array_elem_types
                                .get(arr_name)
                                .cloned()
                                .unwrap_or_else(|| "i64".to_string())
                        } else {
                            "i64".to_string()
                        };
                        let elem_size = self.llvm_type_size(&elem_ty);

                        // Load data, length, capacity
                        let data_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                            data_gep, arr_ptr
                        ));
                        let data_ptr = self.fresh_temp();
                        self.emit_line(&format!("{} = load ptr, ptr {}", data_ptr, data_gep));

                        let len_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                            len_gep, arr_ptr
                        ));
                        let len_val = self.fresh_temp();
                        self.emit_line(&format!("{} = load i64, ptr {}", len_val, len_gep));

                        let cap_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 2",
                            cap_gep, arr_ptr
                        ));
                        let cap_val = self.fresh_temp();
                        self.emit_line(&format!("{} = load i64, ptr {}", cap_val, cap_gep));

                        // Check if need to grow: len == cap
                        let need_grow = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = icmp eq i64 {}, {}",
                            need_grow, len_val, cap_val
                        ));
                        let grow_label = self.fresh_label("push.grow");
                        let store_label = self.fresh_label("push.store");
                        self.emit_line(&format!(
                            "br i1 {}, label %{}, label %{}",
                            need_grow, grow_label, store_label
                        ));

                        // Grow block: realloc with doubled capacity (min 4)
                        self.emit_label(&grow_label);
                        self.block_terminated = false;
                        let doubled = self.fresh_temp();
                        self.emit_line(&format!("{} = shl i64 {}, 1", doubled, cap_val));
                        let is_zero = self.fresh_temp();
                        self.emit_line(&format!("{} = icmp eq i64 {}, 0", is_zero, cap_val));
                        let new_cap = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = select i1 {}, i64 4, i64 {}",
                            new_cap, is_zero, doubled
                        ));
                        let new_bytes = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = mul i64 {}, {}",
                            new_bytes, new_cap, elem_size
                        ));
                        let new_data = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call ptr @realloc(ptr {}, i64 {})",
                            new_data, data_ptr, new_bytes
                        ));
                        self.emit_line(&format!("store ptr {}, ptr {}", new_data, data_gep));
                        self.emit_line(&format!("store i64 {}, ptr {}", new_cap, cap_gep));
                        self.emit_line(&format!("br label %{}", store_label));

                        // Store block: store element at data[len], increment length
                        self.emit_label(&store_label);
                        self.block_terminated = false;
                        let cur_data = self.fresh_temp();
                        self.emit_line(&format!("{} = load ptr, ptr {}", cur_data, data_gep));
                        let is_agg = Self::is_aggregate_type(&elem_ty);
                        let val = if is_agg {
                            self.emit_expr_ptr(&args[1])?
                        } else {
                            self.emit_expr(&args[1])?
                        };
                        let elem_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {}, ptr {}, i64 {}",
                            elem_gep, elem_ty, cur_data, len_val
                        ));
                        if Self::is_aggregate_type(&elem_ty) {
                            self.emit_line(&format!(
                                "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
                                elem_gep, val, elem_size
                            ));
                        } else {
                            self.emit_line(&format!("store {} {}, ptr {}", elem_ty, val, elem_gep));
                        }
                        let new_len = self.fresh_temp();
                        self.emit_line(&format!("{} = add i64 {}, 1", new_len, len_val));
                        self.emit_line(&format!("store i64 {}, ptr {}", new_len, len_gep));
                        return Ok("void".to_string());
                    }

                    // Built-in pop() for dynamic arrays
                    if name == "pop" && args.len() == 1 {
                        let arr_ptr = self.emit_expr(&args[0])?;
                        let elem_ty = if let ExprKind::Ident(arr_name) = &args[0].kind {
                            self.array_elem_types
                                .get(arr_name)
                                .cloned()
                                .unwrap_or_else(|| "i64".to_string())
                        } else {
                            "i64".to_string()
                        };

                        // Load length
                        let len_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                            len_gep, arr_ptr
                        ));
                        let len_val = self.fresh_temp();
                        self.emit_line(&format!("{} = load i64, ptr {}", len_val, len_gep));

                        // Check not empty: len > 0
                        let is_empty = self.fresh_temp();
                        self.emit_line(&format!("{} = icmp eq i64 {}, 0", is_empty, len_val));
                        let fail_label = self.fresh_label("pop.fail");
                        let ok_label = self.fresh_label("pop.ok");
                        self.emit_line(&format!(
                            "br i1 {}, label %{}, label %{}",
                            is_empty, fail_label, ok_label
                        ));

                        // Fail block: print error and abort
                        self.emit_label(&fail_label);
                        self.block_terminated = false;
                        self.emit_line("call i32 (ptr, ...) @printf(ptr @.fmt.pop_empty)");
                        self.emit_line("call void @abort()");
                        self.emit_line("unreachable");

                        // OK block: decrement length, load last element
                        self.emit_label(&ok_label);
                        self.block_terminated = false;
                        let new_len = self.fresh_temp();
                        self.emit_line(&format!("{} = sub i64 {}, 1", new_len, len_val));
                        self.emit_line(&format!("store i64 {}, ptr {}", new_len, len_gep));

                        let data_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                            data_gep, arr_ptr
                        ));
                        let data_ptr = self.fresh_temp();
                        self.emit_line(&format!("{} = load ptr, ptr {}", data_ptr, data_gep));
                        let elem_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {}, ptr {}, i64 {}",
                            elem_gep, elem_ty, data_ptr, new_len
                        ));
                        if Self::is_aggregate_type(&elem_ty) {
                            // For aggregate types, copy to a local alloca and return pointer
                            let tmp_alloca = self.fresh_temp();
                            self.emit_line(&format!("{} = alloca {}", tmp_alloca, elem_ty));
                            let sz = self.llvm_type_size(&elem_ty);
                            self.emit_line(&format!(
                                "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
                                tmp_alloca, elem_gep, sz
                            ));
                            return Ok(tmp_alloca);
                        } else {
                            let val = self.fresh_temp();
                            self.emit_line(&format!(
                                "{} = load {}, ptr {}",
                                val, elem_ty, elem_gep
                            ));
                            return Ok(val);
                        }
                    }

                    // Built-in exit() — truncate i64 to i32 and call C exit
                    if name == "exit" && args.len() == 1 {
                        let code = self.emit_expr(&args[0])?;
                        let code32 = self.fresh_temp();
                        self.emit_line(&format!("{} = trunc i64 {} to i32", code32, code));
                        self.emit_line(&format!("call void @exit(i32 {})", code32));
                        self.emit_line("unreachable");
                        self.block_terminated = true;
                        return Ok("void".to_string());
                    }

                    // Built-in args() — build [string] from stored argc/argv
                    if name == "args" && args.is_empty() {
                        let argc32 = self.fresh_temp();
                        self.emit_line(&format!("{} = load i32, ptr @__yorum_argc", argc32));
                        let argc = self.fresh_temp();
                        self.emit_line(&format!("{} = zext i32 {} to i64", argc, argc32));
                        let argv = self.fresh_temp();
                        self.emit_line(&format!("{} = load ptr, ptr @__yorum_argv", argv));
                        // Allocate data array and copy argv pointers
                        let bytes = self.fresh_temp();
                        self.emit_line(&format!("{} = mul i64 {}, 8", bytes, argc));
                        let data = self.fresh_temp();
                        self.emit_line(&format!("{} = call ptr @malloc(i64 {})", data, bytes));
                        self.emit_line(&format!(
                            "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
                            data, argv, bytes
                        ));
                        // Build { ptr, i64, i64 } fat pointer
                        let fat = self.fresh_temp();
                        self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat));
                        let d_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                            d_gep, fat
                        ));
                        self.emit_line(&format!("store ptr {}, ptr {}", data, d_gep));
                        let l_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                            l_gep, fat
                        ));
                        self.emit_line(&format!("store i64 {}, ptr {}", argc, l_gep));
                        let c_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 2",
                            c_gep, fat
                        ));
                        self.emit_line(&format!("store i64 {}, ptr {}", argc, c_gep));
                        return Ok(fat);
                    }

                    // Built-in slice(arr, start, end) — returns new array
                    if name == "slice" && args.len() == 3 {
                        let arr_ptr = self.emit_expr(&args[0])?;
                        let elem_ty = if let ExprKind::Ident(arr_name) = &args[0].kind {
                            self.array_elem_types
                                .get(arr_name)
                                .cloned()
                                .unwrap_or_else(|| "i64".to_string())
                        } else {
                            "i64".to_string()
                        };
                        let elem_size = self.llvm_type_size(&elem_ty);
                        let start = self.emit_expr(&args[1])?;
                        let end = self.emit_expr(&args[2])?;

                        // new_len = end - start
                        let new_len = self.fresh_temp();
                        self.emit_line(&format!("{} = sub i64 {}, {}", new_len, end, start));
                        // allocate new data
                        let new_bytes = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = mul i64 {}, {}",
                            new_bytes, new_len, elem_size
                        ));
                        let new_data = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call ptr @malloc(i64 {})",
                            new_data, new_bytes
                        ));
                        // memcpy from arr.data[start]
                        let data_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                            data_gep, arr_ptr
                        ));
                        let data_ptr = self.fresh_temp();
                        self.emit_line(&format!("{} = load ptr, ptr {}", data_ptr, data_gep));
                        let src = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {}, ptr {}, i64 {}",
                            src, elem_ty, data_ptr, start
                        ));
                        self.emit_line(&format!(
                            "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
                            new_data, src, new_bytes
                        ));
                        // build fat pointer
                        let fat = self.fresh_temp();
                        self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat));
                        let d_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                            d_gep, fat
                        ));
                        self.emit_line(&format!("store ptr {}, ptr {}", new_data, d_gep));
                        let l_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                            l_gep, fat
                        ));
                        self.emit_line(&format!("store i64 {}, ptr {}", new_len, l_gep));
                        let c_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 2",
                            c_gep, fat
                        ));
                        self.emit_line(&format!("store i64 {}, ptr {}", new_len, c_gep));
                        return Ok(fat);
                    }

                    // Built-in concat_arrays(a, b) — returns new array
                    if name == "concat_arrays" && args.len() == 2 {
                        let arr_a = self.emit_expr(&args[0])?;
                        let elem_ty = if let ExprKind::Ident(arr_name) = &args[0].kind {
                            self.array_elem_types
                                .get(arr_name)
                                .cloned()
                                .unwrap_or_else(|| "i64".to_string())
                        } else {
                            "i64".to_string()
                        };
                        let elem_size = self.llvm_type_size(&elem_ty);
                        let arr_b = self.emit_expr(&args[1])?;

                        // load lengths
                        let la_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                            la_gep, arr_a
                        ));
                        let len_a = self.fresh_temp();
                        self.emit_line(&format!("{} = load i64, ptr {}", len_a, la_gep));
                        let lb_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                            lb_gep, arr_b
                        ));
                        let len_b = self.fresh_temp();
                        self.emit_line(&format!("{} = load i64, ptr {}", len_b, lb_gep));
                        let total_len = self.fresh_temp();
                        self.emit_line(&format!("{} = add i64 {}, {}", total_len, len_a, len_b));
                        // allocate data
                        let total_bytes = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = mul i64 {}, {}",
                            total_bytes, total_len, elem_size
                        ));
                        let new_data = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call ptr @malloc(i64 {})",
                            new_data, total_bytes
                        ));
                        // copy a's data
                        let da_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                            da_gep, arr_a
                        ));
                        let data_a = self.fresh_temp();
                        self.emit_line(&format!("{} = load ptr, ptr {}", data_a, da_gep));
                        let bytes_a = self.fresh_temp();
                        self.emit_line(&format!("{} = mul i64 {}, {}", bytes_a, len_a, elem_size));
                        self.emit_line(&format!(
                            "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
                            new_data, data_a, bytes_a
                        ));
                        // copy b's data after a
                        let db_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                            db_gep, arr_b
                        ));
                        let data_b = self.fresh_temp();
                        self.emit_line(&format!("{} = load ptr, ptr {}", data_b, db_gep));
                        let bytes_b = self.fresh_temp();
                        self.emit_line(&format!("{} = mul i64 {}, {}", bytes_b, len_b, elem_size));
                        let dst = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {}, ptr {}, i64 {}",
                            dst, elem_ty, new_data, len_a
                        ));
                        self.emit_line(&format!(
                            "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
                            dst, data_b, bytes_b
                        ));
                        // build fat pointer
                        let fat = self.fresh_temp();
                        self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat));
                        let d_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                            d_gep, fat
                        ));
                        self.emit_line(&format!("store ptr {}, ptr {}", new_data, d_gep));
                        let l_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                            l_gep, fat
                        ));
                        self.emit_line(&format!("store i64 {}, ptr {}", total_len, l_gep));
                        let c_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 2",
                            c_gep, fat
                        ));
                        self.emit_line(&format!("store i64 {}, ptr {}", total_len, c_gep));
                        return Ok(fat);
                    }

                    // Built-in reverse(arr) — returns new reversed array
                    if name == "reverse" && args.len() == 1 {
                        let arr_ptr = self.emit_expr(&args[0])?;
                        let elem_ty = if let ExprKind::Ident(arr_name) = &args[0].kind {
                            self.array_elem_types
                                .get(arr_name)
                                .cloned()
                                .unwrap_or_else(|| "i64".to_string())
                        } else {
                            "i64".to_string()
                        };
                        let elem_size = self.llvm_type_size(&elem_ty);
                        let is_agg = Self::is_aggregate_type(&elem_ty);

                        // load length and data
                        let len_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                            len_gep, arr_ptr
                        ));
                        let len_val = self.fresh_temp();
                        self.emit_line(&format!("{} = load i64, ptr {}", len_val, len_gep));
                        let data_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                            data_gep, arr_ptr
                        ));
                        let data_ptr = self.fresh_temp();
                        self.emit_line(&format!("{} = load ptr, ptr {}", data_ptr, data_gep));
                        // allocate new data
                        let total_bytes = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = mul i64 {}, {}",
                            total_bytes, len_val, elem_size
                        ));
                        let new_data = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = call ptr @malloc(i64 {})",
                            new_data, total_bytes
                        ));
                        // Use alloca-based loop counter (avoids phi nodes)
                        let counter = self.fresh_temp();
                        self.emit_line(&format!("{} = alloca i64", counter));
                        self.emit_line(&format!("store i64 0, ptr {}", counter));
                        let loop_label = self.fresh_label("rev.loop");
                        let body_label = self.fresh_label("rev.body");
                        let done_label = self.fresh_label("rev.done");
                        self.emit_line(&format!("br label %{}", loop_label));
                        self.emit_label(&loop_label);
                        self.block_terminated = false;
                        let i = self.fresh_temp();
                        self.emit_line(&format!("{} = load i64, ptr {}", i, counter));
                        let cmp = self.fresh_temp();
                        self.emit_line(&format!("{} = icmp slt i64 {}, {}", cmp, i, len_val));
                        self.emit_line(&format!(
                            "br i1 {}, label %{}, label %{}",
                            cmp, body_label, done_label
                        ));
                        self.emit_label(&body_label);
                        self.block_terminated = false;
                        // dst_idx = len - 1 - i
                        let len_m1 = self.fresh_temp();
                        self.emit_line(&format!("{} = sub i64 {}, 1", len_m1, len_val));
                        let dst_idx = self.fresh_temp();
                        self.emit_line(&format!("{} = sub i64 {}, {}", dst_idx, len_m1, i));
                        let src_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {}, ptr {}, i64 {}",
                            src_gep, elem_ty, data_ptr, i
                        ));
                        let dst_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {}, ptr {}, i64 {}",
                            dst_gep, elem_ty, new_data, dst_idx
                        ));
                        if is_agg {
                            self.emit_line(&format!(
                                "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
                                dst_gep, src_gep, elem_size
                            ));
                        } else {
                            let val = self.fresh_temp();
                            self.emit_line(&format!("{} = load {}, ptr {}", val, elem_ty, src_gep));
                            self.emit_line(&format!("store {} {}, ptr {}", elem_ty, val, dst_gep));
                        }
                        let i_next = self.fresh_temp();
                        self.emit_line(&format!("{} = add i64 {}, 1", i_next, i));
                        self.emit_line(&format!("store i64 {}, ptr {}", i_next, counter));
                        self.emit_line(&format!("br label %{}", loop_label));
                        self.emit_label(&done_label);
                        self.block_terminated = false;
                        // build fat pointer
                        let fat = self.fresh_temp();
                        self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat));
                        let d_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                            d_gep, fat
                        ));
                        self.emit_line(&format!("store ptr {}, ptr {}", new_data, d_gep));
                        let l_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                            l_gep, fat
                        ));
                        self.emit_line(&format!("store i64 {}, ptr {}", len_val, l_gep));
                        let c_gep = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 2",
                            c_gep, fat
                        ));
                        self.emit_line(&format!("store i64 {}, ptr {}", len_val, c_gep));
                        return Ok(fat);
                    }

                    // Built-in to_str() — polymorphic string conversion
                    if name == "to_str" && args.len() == 1 {
                        return self.emit_to_str(&args[0]);
                    }

                    // Math builtins using LLVM intrinsics — called inline to
                    // avoid wrapper functions that clash with C library names
                    // (e.g., @pow wrapper calling @llvm.pow.f64 which LLVM
                    // lowers back to C's pow → infinite recursion).
                    match name.as_str() {
                        "sqrt" if args.len() == 1 => {
                            let val = self.emit_expr(&args[0])?;
                            let tmp = self.fresh_temp();
                            self.emit_line(&format!(
                                "{} = call double @llvm.sqrt.f64(double {})",
                                tmp, val
                            ));
                            return Ok(tmp);
                        }
                        "pow" if args.len() == 2 => {
                            let base = self.emit_expr(&args[0])?;
                            let exp = self.emit_expr(&args[1])?;
                            let tmp = self.fresh_temp();
                            self.emit_line(&format!(
                                "{} = call double @llvm.pow.f64(double {}, double {})",
                                tmp, base, exp
                            ));
                            return Ok(tmp);
                        }
                        "floor" if args.len() == 1 => {
                            let val = self.emit_expr(&args[0])?;
                            let tmp = self.fresh_temp();
                            self.emit_line(&format!(
                                "{} = call double @llvm.floor.f64(double {})",
                                tmp, val
                            ));
                            return Ok(tmp);
                        }
                        "ceil" if args.len() == 1 => {
                            let val = self.emit_expr(&args[0])?;
                            let tmp = self.fresh_temp();
                            self.emit_line(&format!(
                                "{} = call double @llvm.ceil.f64(double {})",
                                tmp, val
                            ));
                            return Ok(tmp);
                        }
                        "round" if args.len() == 1 => {
                            let val = self.emit_expr(&args[0])?;
                            let tmp = self.fresh_temp();
                            self.emit_line(&format!(
                                "{} = call double @llvm.round.f64(double {})",
                                tmp, val
                            ));
                            return Ok(tmp);
                        }
                        _ => {}
                    }

                    // Check if this is an enum variant constructor
                    if let Some(enum_name) = self.find_enum_for_variant(name) {
                        return self.emit_enum_variant_constructor(&enum_name, name, args);
                    }
                    // Check if this is a known function (direct call)
                    if self.fn_ret_types.contains_key(name.as_str()) {
                        let mut arg_strs = Vec::new();
                        for arg in args {
                            let val = self.emit_expr(arg)?;
                            let ty = self.expr_llvm_type(arg);
                            arg_strs.push(format!("{} {}", ty, val));
                        }

                        let ret_ty = self.fn_ret_types[name.as_str()].clone();
                        let llvm_ret = self.llvm_type(&ret_ty);
                        let call_prefix = if self.tail_call_hint {
                            "tail call"
                        } else {
                            "call"
                        };

                        if llvm_ret == "void" {
                            self.emit_line(&format!(
                                "{} void @{}({})",
                                call_prefix,
                                name,
                                arg_strs.join(", ")
                            ));
                            Ok("void".to_string())
                        } else {
                            let tmp = self.fresh_temp();
                            self.emit_line(&format!(
                                "{} = {} {} @{}({})",
                                tmp,
                                call_prefix,
                                llvm_ret,
                                name,
                                arg_strs.join(", ")
                            ));
                            Ok(tmp)
                        }
                    } else {
                        // Indirect call through a fn-typed variable
                        self.emit_indirect_call(callee, args)
                    }
                } else {
                    // Indirect call through a complex expression
                    self.emit_indirect_call(callee, args)
                }
            }

            ExprKind::FieldAccess(obj, field) => {
                let obj_ptr = self.emit_expr_ptr(obj)?;
                let struct_name = self.expr_struct_name(obj)?;

                // Tuple field access: t.0, t.1, ...
                if struct_name.starts_with("tuple.") {
                    let idx: usize = field.parse().map_err(|_| CodegenError {
                        message: format!("invalid tuple field '{}'", field),
                    })?;
                    let elem_types = self
                        .tuple_elem_types
                        .get(&struct_name)
                        .ok_or_else(|| CodegenError {
                            message: format!("unknown tuple type '{}'", struct_name),
                        })?
                        .clone();
                    if idx >= elem_types.len() {
                        return Err(CodegenError {
                            message: format!(
                                "tuple index {} out of bounds (tuple has {} elements)",
                                idx,
                                elem_types.len()
                            ),
                        });
                    }
                    let fty = &elem_types[idx];
                    let gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                        gep, struct_name, obj_ptr, idx
                    ));
                    let val = self.fresh_temp();
                    self.emit_line(&format!("{} = load {}, ptr {}", val, fty, gep));
                    return Ok(val);
                }

                let (idx, field_ty) = self.struct_field_index(&struct_name, field)?;
                let fty = self.llvm_type(&field_ty);

                let gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                    gep, struct_name, obj_ptr, idx
                ));
                let val = self.fresh_temp();
                self.emit_line(&format!("{} = load {}, ptr {}", val, fty, gep));
                Ok(val)
            }

            ExprKind::MethodCall(receiver, method_name, args) => {
                // Handle .join() on Task values
                if method_name == "join" {
                    return self.emit_task_join(receiver);
                }

                // Handle Option/Result methods
                if let Ok(ref ename) = self.expr_struct_name(receiver) {
                    if let Some(layout) = self.enum_layouts.get(ename).cloned() {
                        match method_name.as_str() {
                            "unwrap" => {
                                return self.emit_option_unwrap(receiver, ename, &layout);
                            }
                            "unwrap_err" => {
                                return self.emit_result_unwrap_err(receiver, ename, &layout);
                            }
                            "is_some" | "is_ok" => {
                                return self.emit_enum_tag_check(receiver, ename, 0);
                            }
                            "is_none" | "is_err" => {
                                return self.emit_enum_tag_check(receiver, ename, 1);
                            }
                            _ => {}
                        }
                    }
                }

                let struct_name = self.expr_struct_name(receiver)?;
                let recv_ptr = self.emit_expr_ptr(receiver)?;
                let mangled = format!("{}_{}", struct_name, method_name);

                let mut arg_strs = vec![format!("ptr {}", recv_ptr)];
                for arg in args {
                    let val = self.emit_expr(arg)?;
                    let ty = self.expr_llvm_type(arg);
                    arg_strs.push(format!("{} {}", ty, val));
                }

                let ret_ty = self
                    .fn_ret_types
                    .get(&mangled)
                    .cloned()
                    .unwrap_or(Type::Unit);
                let llvm_ret = self.llvm_type(&ret_ty);

                if llvm_ret == "void" {
                    self.emit_line(&format!("call void @{}({})", mangled, arg_strs.join(", ")));
                    Ok("void".to_string())
                } else {
                    let tmp = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = call {} @{}({})",
                        tmp,
                        llvm_ret,
                        mangled,
                        arg_strs.join(", ")
                    ));
                    Ok(tmp)
                }
            }

            ExprKind::Closure(closure) => self.emit_closure(closure),

            ExprKind::Spawn(block) => self.emit_spawn(block),

            ExprKind::TupleLit(elements) => self.emit_tuple_lit(elements),

            ExprKind::Range(_, _) | ExprKind::RangeInclusive(_, _) => Err(CodegenError {
                message: "range expression is only valid in for loops".to_string(),
            })?,

            ExprKind::Try(inner) => self.emit_try_expr(inner),

            ExprKind::StructInit(name, fields) => {
                let _layout =
                    self.struct_layouts
                        .get(name)
                        .cloned()
                        .ok_or_else(|| CodegenError {
                            message: format!("undefined struct '{}'", name),
                        })?;
                let ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca %{}", ptr, name));

                for fi in fields {
                    let (idx, field_ty) = self.struct_field_index(name, &fi.name)?;
                    let fty = self.llvm_type(&field_ty);
                    let val = self.emit_expr(&fi.value)?;
                    let gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                        gep, name, ptr, idx
                    ));
                    self.emit_line(&format!("store {} {}, ptr {}", fty, val, gep));
                }

                // Return the pointer as the struct value
                Ok(ptr)
            }

            ExprKind::ArrayLit(elements) => {
                let count = elements.len();
                if count == 0 {
                    // Empty array: { null, 0 }
                    let ptr = self.fresh_temp();
                    self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", ptr));
                    let d_gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                        d_gep, ptr
                    ));
                    self.emit_line(&format!("store ptr null, ptr {}", d_gep));
                    let l_gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                        l_gep, ptr
                    ));
                    self.emit_line(&format!("store i64 0, ptr {}", l_gep));
                    let c_gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 2",
                        c_gep, ptr
                    ));
                    self.emit_line(&format!("store i64 0, ptr {}", c_gep));
                    return Ok(ptr);
                }
                // Determine element type from first element
                let elem_ty = self.expr_llvm_type(&elements[0]);
                let elem_size = self.llvm_type_size(&elem_ty);
                let total_size = elem_size * count;

                // malloc data
                let data_ptr = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = call ptr @malloc(i64 {})",
                    data_ptr, total_size
                ));

                // Store each element
                let is_aggregate = Self::is_aggregate_type(&elem_ty);
                for (i, elem) in elements.iter().enumerate() {
                    let val = if is_aggregate {
                        self.emit_expr_ptr(elem)?
                    } else {
                        self.emit_expr(elem)?
                    };
                    let gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr {}, ptr {}, i64 {}",
                        gep, elem_ty, data_ptr, i
                    ));
                    if is_aggregate {
                        self.emit_line(&format!(
                            "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
                            gep, val, elem_size
                        ));
                    } else {
                        self.emit_line(&format!("store {} {}, ptr {}", elem_ty, val, gep));
                    }
                }

                // Build { ptr, i64, i64 } fat pointer on stack
                let fat_ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat_ptr));
                let d_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                    d_gep, fat_ptr
                ));
                self.emit_line(&format!("store ptr {}, ptr {}", data_ptr, d_gep));
                let l_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                    l_gep, fat_ptr
                ));
                self.emit_line(&format!("store i64 {}, ptr {}", count, l_gep));
                let c_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 2",
                    c_gep, fat_ptr
                ));
                self.emit_line(&format!("store i64 {}, ptr {}", count, c_gep));

                Ok(fat_ptr)
            }

            ExprKind::Index(arr_expr, idx_expr) => {
                // Determine element type
                let elem_ty = if let ExprKind::Ident(name) = &arr_expr.kind {
                    self.array_elem_types
                        .get(name)
                        .cloned()
                        .unwrap_or_else(|| "i64".to_string())
                } else {
                    "i64".to_string()
                };

                let arr_ptr = self.emit_expr_ptr(arr_expr)?;

                // Load data pointer from { ptr, i64, i64 }
                let data_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 0",
                    data_gep, arr_ptr
                ));
                let data_ptr = self.fresh_temp();
                self.emit_line(&format!("{} = load ptr, ptr {}", data_ptr, data_gep));

                // Load length for bounds check
                let len_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 1",
                    len_gep, arr_ptr
                ));
                let len_val = self.fresh_temp();
                self.emit_line(&format!("{} = load i64, ptr {}", len_val, len_gep));

                let idx_val = self.emit_expr(idx_expr)?;

                // Bounds check
                self.emit_line(&format!(
                    "call void @__yorum_bounds_check(i64 {}, i64 {})",
                    idx_val, len_val
                ));

                // GEP to element and load
                let elem_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr {}, ptr {}, i64 {}",
                    elem_gep, elem_ty, data_ptr, idx_val
                ));
                if Self::is_aggregate_type(&elem_ty) {
                    // For aggregate types, copy to a local alloca and return pointer
                    let tmp_alloca = self.fresh_temp();
                    self.emit_line(&format!("{} = alloca {}", tmp_alloca, elem_ty));
                    let sz = self.llvm_type_size(&elem_ty);
                    self.emit_line(&format!(
                        "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
                        tmp_alloca, elem_gep, sz
                    ));
                    Ok(tmp_alloca)
                } else {
                    let val = self.fresh_temp();
                    self.emit_line(&format!("{} = load {}, ptr {}", val, elem_ty, elem_gep));
                    Ok(val)
                }
            }
        }
    }

    /// Emit an expression and return its alloca pointer (for lvalues).
    fn emit_expr_ptr(&mut self, expr: &Expr) -> Result<String, CodegenError> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                let slot = self.lookup_var(name).ok_or_else(|| CodegenError {
                    message: format!("undefined variable '{}'", name),
                })?;
                Ok(slot.ptr.clone())
            }
            ExprKind::StructInit(_, _) | ExprKind::TupleLit(_) => {
                // StructInit/TupleLit emit_expr already returns an alloca pointer
                self.emit_expr(expr)
            }
            ExprKind::Index(arr_expr, _) => {
                // Index on aggregate types already returns an alloca pointer
                // (the codegen does memcpy to a temp alloca for struct elements)
                let val = self.emit_expr(expr)?;
                // Check if element type is aggregate
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    if let Some(elem_ty) = self.array_elem_types.get(name) {
                        if Self::is_aggregate_type(elem_ty) {
                            return Ok(val); // already a pointer to temp alloca
                        }
                    }
                }
                Ok(val)
            }
            _ => {
                // For complex expressions that return aggregate types (e.g., function calls
                // returning structs), store to a temp alloca and return the alloca pointer
                let val = self.emit_expr(expr)?;
                let llvm_ty = self.expr_llvm_type(expr);
                if Self::is_aggregate_type(&llvm_ty) {
                    let tmp = self.fresh_temp();
                    self.emit_line(&format!("{} = alloca {}", tmp, llvm_ty));
                    self.emit_line(&format!("store {} {}, ptr {}", llvm_ty, val, tmp));
                    Ok(tmp)
                } else {
                    Ok(val)
                }
            }
        }
    }

    fn emit_contract_string(&mut self, msg: &str) -> String {
        let id = self.contract_string_counter;
        self.contract_string_counter += 1;
        let escaped = self.escape_llvm_string(msg);
        // Count actual bytes after escaping: each \XX escape is 1 byte
        let byte_len = self.count_escaped_bytes(&escaped) + 1; // +1 for null terminator
        let global_name = format!("@.str.contract.{}", id);
        self.globals.push_str(&format!(
            "{} = private unnamed_addr constant [{} x i8] c\"{}\\00\"\n",
            global_name, byte_len, escaped
        ));
        global_name
    }

    fn count_escaped_bytes(&self, escaped: &str) -> usize {
        let mut count = 0;
        let mut chars = escaped.chars();
        while let Some(c) = chars.next() {
            count += 1;
            if c == '\\' {
                // Skip the two hex digits of the escape sequence
                chars.next();
                chars.next();
            }
        }
        count
    }

    fn emit_requires_checks(
        &mut self,
        contracts: &[Contract],
        fn_name: &str,
    ) -> Result<(), CodegenError> {
        for contract in contracts {
            if let Contract::Requires(expr) = contract {
                let val = self.emit_expr(expr)?;
                let ok_label = self.fresh_label("req_ok");
                let fail_label = self.fresh_label("req_fail");
                self.emit_line(&format!(
                    "br i1 {}, label %{}, label %{}",
                    val, ok_label, fail_label
                ));
                self.emit_label(&fail_label);
                let msg = self.emit_contract_string(&format!("requires clause in '{}'", fn_name));
                self.emit_line(&format!("call void @__yorum_contract_fail(ptr {})", msg));
                self.emit_line("unreachable");
                self.emit_label(&ok_label);
                self.block_terminated = false;
            }
        }
        Ok(())
    }

    fn emit_ensures_checks(
        &mut self,
        contracts: &[Contract],
        fn_name: &str,
    ) -> Result<(), CodegenError> {
        for contract in contracts {
            if let Contract::Ensures(expr) = contract {
                let val = self.emit_expr(expr)?;
                let ok_label = self.fresh_label("ens_ok");
                let fail_label = self.fresh_label("ens_fail");
                self.emit_line(&format!(
                    "br i1 {}, label %{}, label %{}",
                    val, ok_label, fail_label
                ));
                self.emit_label(&fail_label);
                let msg = self.emit_contract_string(&format!("ensures clause in '{}'", fn_name));
                self.emit_line(&format!("call void @__yorum_contract_fail(ptr {})", msg));
                self.emit_line("unreachable");
                self.emit_label(&ok_label);
                self.block_terminated = false;
            }
        }
        Ok(())
    }

    fn has_ensures(contracts: &[Contract]) -> bool {
        contracts.iter().any(|c| matches!(c, Contract::Ensures(_)))
    }

    fn emit_literal(&mut self, lit: &Literal) -> Result<String, CodegenError> {
        match lit {
            Literal::Int(n) => Ok(n.to_string()),
            Literal::Float(f) => {
                // LLVM requires hex float format for exact representation
                Ok(format!("{:.6e}", f))
            }
            Literal::Bool(b) => Ok(if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }),
            Literal::Char(c) => Ok((*c as u8).to_string()),
            Literal::Unit => Ok("void".to_string()),
            Literal::String(s) => {
                // Emit as a global constant
                let id = self.string_counter;
                self.string_counter += 1;
                let escaped = self.escape_llvm_string(s);
                let len = s.len() + 1; // +1 for null terminator
                let global_name = format!("@.str.{}", id);
                self.globals.push_str(&format!(
                    "{} = private unnamed_addr constant [{} x i8] c\"{}\\00\"\n",
                    global_name, len, escaped
                ));
                Ok(global_name)
            }
        }
    }

    // ── Enum variant constructor ──────────────────────────────

    fn emit_enum_variant_constructor(
        &mut self,
        enum_name: &str,
        variant_name: &str,
        args: &[Expr],
    ) -> Result<String, CodegenError> {
        let tag = self.variant_tag(variant_name)?;

        // Alloca the enum type
        let ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca %{}", ptr, enum_name));

        // Set the tag
        let tag_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr %{}, ptr {}, i32 0, i32 0",
            tag_gep, enum_name, ptr
        ));
        self.emit_line(&format!("store i32 {}, ptr {}", tag, tag_gep));

        // Store arguments into payload bytes
        if !args.is_empty() {
            let payload_gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr {}, i32 0, i32 1",
                payload_gep, enum_name, ptr
            ));

            let mut byte_offset = 0usize;
            for arg in args {
                let val = self.emit_expr(arg)?;
                let arg_ty = self.expr_llvm_type(arg);
                let field_ptr = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr [0 x i8], ptr {}, i64 0, i64 {}",
                    field_ptr, payload_gep, byte_offset
                ));
                self.emit_line(&format!("store {} {}, ptr {}", arg_ty, val, field_ptr));
                byte_offset += self.llvm_type_size(&arg_ty);
            }
        }

        Ok(ptr)
    }

    // ── Tuple helpers ─────────────────────────────────────

    /// Generate a stable LLVM type name for a tuple type.
    fn tuple_type_name(&self, types: &[Type]) -> String {
        let parts: Vec<String> = types
            .iter()
            .map(|t| match t {
                Type::Int => "int".to_string(),
                Type::Float => "float".to_string(),
                Type::Bool => "bool".to_string(),
                Type::Char => "char".to_string(),
                Type::Str => "string".to_string(),
                Type::Unit => "unit".to_string(),
                Type::Named(n) => n.clone(),
                Type::Tuple(inner) => self.tuple_type_name(inner),
                _ => format!("{}", t),
            })
            .collect();
        format!("tuple.{}", parts.join("."))
    }

    /// Ensure a tuple type definition is emitted and element types are tracked.
    fn ensure_tuple_type(&mut self, types: &[Type]) {
        let type_name = self.tuple_type_name(types);
        if self.tuple_elem_types.contains_key(&type_name) {
            return;
        }
        let llvm_elem_types: Vec<String> = types.iter().map(|t| self.llvm_type(t)).collect();
        let llvm_fields = llvm_elem_types.join(", ");
        let type_def = format!("%{} = type {{ {} }}\n", type_name, llvm_fields);
        if !self.type_defs.contains(&format!("%{} = type", type_name)) {
            self.type_defs.push_str(&type_def);
        }
        self.tuple_elem_types.insert(type_name, llvm_elem_types);
    }

    /// Emit a tuple literal: alloca the tuple struct, store each element.
    fn emit_tuple_lit(&mut self, elements: &[Expr]) -> Result<String, CodegenError> {
        // Compute element LLVM types
        let elem_types: Vec<String> = elements.iter().map(|e| self.expr_llvm_type(e)).collect();
        let type_name = format!(
            "tuple.{}",
            elem_types
                .iter()
                .map(|t| llvm_to_semantic_name(t))
                .collect::<Vec<_>>()
                .join(".")
        );

        // Ensure the tuple type is defined and element types are tracked
        let llvm_fields = elem_types.join(", ");
        let type_def = format!("%{} = type {{ {} }}\n", type_name, llvm_fields);
        if !self.type_defs.contains(&format!("%{} = type", type_name)) {
            self.type_defs.push_str(&type_def);
        }
        self.tuple_elem_types
            .insert(type_name.clone(), elem_types.clone());

        // Alloca the tuple
        let ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca %{}", ptr, type_name));

        // Store each element
        for (i, elem) in elements.iter().enumerate() {
            let val = self.emit_expr(elem)?;
            let gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                gep, type_name, ptr, i
            ));
            self.emit_line(&format!("store {} {}, ptr {}", elem_types[i], val, gep));
        }

        Ok(ptr)
    }

    // ── Try (?) operator ─────────────────────────────────────

    fn emit_try_expr(&mut self, inner: &Expr) -> Result<String, CodegenError> {
        // 1. Determine the enum type of the inner expression
        let inner_llvm_ty = self.expr_llvm_type(inner);
        let enum_name = inner_llvm_ty
            .strip_prefix('%')
            .ok_or_else(|| CodegenError {
                message: format!("? operator on non-enum type '{}'", inner_llvm_ty),
            })?
            .to_string();

        let is_option = enum_name.starts_with("Option__");
        let is_result = enum_name.starts_with("Result__");
        if !is_option && !is_result {
            return Err(CodegenError {
                message: format!("? operator on non-Option/Result type '{}'", enum_name),
            });
        }

        // 2. Emit inner expression and ensure we have a pointer for GEP
        let inner_val = self.emit_expr(inner)?;
        let enum_ptr = if self.expr_returns_ptr(inner) {
            inner_val
        } else {
            let ptr = self.fresh_temp();
            self.emit_line(&format!("{} = alloca %{}", ptr, enum_name));
            self.emit_line(&format!("store %{} {}, ptr {}", enum_name, inner_val, ptr));
            ptr
        };

        // 3. Extract tag (offset 0)
        let tag_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr %{}, ptr {}, i32 0, i32 0",
            tag_gep, enum_name, enum_ptr
        ));
        let tag_val = self.fresh_temp();
        self.emit_line(&format!("{} = load i32, ptr {}", tag_val, tag_gep));

        // 4. Branch: tag 0 = Some/Ok (success), tag != 0 = None/Err (early return)
        let success_label = self.fresh_label("try.ok");
        let error_label = self.fresh_label("try.err");
        let cmp = self.fresh_temp();
        self.emit_line(&format!("{} = icmp eq i32 {}, 0", cmp, tag_val));
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cmp, success_label, error_label
        ));

        // 5. Error path: early return None or Err(e)
        self.emit_label(&error_label);
        self.block_terminated = false;

        let ret_ty = self
            .current_fn_ret_ty
            .clone()
            .unwrap_or_else(|| "void".to_string());
        let ret_enum_name = ret_ty.strip_prefix('%').unwrap_or(&ret_ty).to_string();

        if is_option {
            // Construct None for the function's return type and ret
            let none_ptr = self.fresh_temp();
            self.emit_line(&format!("{} = alloca %{}", none_ptr, ret_enum_name));
            let none_tag_gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr {}, i32 0, i32 0",
                none_tag_gep, ret_enum_name, none_ptr
            ));
            self.emit_line(&format!("store i32 1, ptr {}", none_tag_gep));
            let none_val = self.fresh_temp();
            self.emit_line(&format!("{} = load {}, ptr {}", none_val, ret_ty, none_ptr));
            self.emit_line(&format!("ret {} {}", ret_ty, none_val));
        } else {
            // Extract Err payload from inner Result, construct Err(e) for return type
            let inner_payload_gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr {}, i32 0, i32 1",
                inner_payload_gep, enum_name, enum_ptr
            ));

            let err_variant_fields = self
                .enum_layouts
                .get(&enum_name)
                .and_then(|layout| layout.variants.iter().find(|(vn, _)| vn == "Err"))
                .map(|(_, fields)| fields.clone())
                .unwrap_or_default();

            if !err_variant_fields.is_empty() {
                let err_llvm_ty = self.llvm_type(&err_variant_fields[0]);

                // Load Err payload from inner
                let err_field_ptr = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr [0 x i8], ptr {}, i64 0, i64 0",
                    err_field_ptr, inner_payload_gep
                ));
                let err_val = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = load {}, ptr {}",
                    err_val, err_llvm_ty, err_field_ptr
                ));

                // Construct Err(e) for the return type
                let ret_err_ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca %{}", ret_err_ptr, ret_enum_name));
                let ret_tag_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr %{}, ptr {}, i32 0, i32 0",
                    ret_tag_gep, ret_enum_name, ret_err_ptr
                ));
                self.emit_line(&format!("store i32 1, ptr {}", ret_tag_gep));
                let ret_payload_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr %{}, ptr {}, i32 0, i32 1",
                    ret_payload_gep, ret_enum_name, ret_err_ptr
                ));
                let ret_err_field_ptr = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr [0 x i8], ptr {}, i64 0, i64 0",
                    ret_err_field_ptr, ret_payload_gep
                ));
                self.emit_line(&format!(
                    "store {} {}, ptr {}",
                    err_llvm_ty, err_val, ret_err_field_ptr
                ));
                let ret_val = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = load {}, ptr {}",
                    ret_val, ret_ty, ret_err_ptr
                ));
                self.emit_line(&format!("ret {} {}", ret_ty, ret_val));
            } else {
                // Err variant with no payload (unusual but handle gracefully)
                let ret_err_ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca %{}", ret_err_ptr, ret_enum_name));
                let ret_tag_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr %{}, ptr {}, i32 0, i32 0",
                    ret_tag_gep, ret_enum_name, ret_err_ptr
                ));
                self.emit_line(&format!("store i32 1, ptr {}", ret_tag_gep));
                let ret_val = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = load {}, ptr {}",
                    ret_val, ret_ty, ret_err_ptr
                ));
                self.emit_line(&format!("ret {} {}", ret_ty, ret_val));
            }
        }
        self.block_terminated = true;

        // 6. Success path: extract T payload from Some/Ok
        self.emit_label(&success_label);
        self.block_terminated = false;

        let ok_payload_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr %{}, ptr {}, i32 0, i32 1",
            ok_payload_gep, enum_name, enum_ptr
        ));
        let ok_field_ptr = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr [0 x i8], ptr {}, i64 0, i64 0",
            ok_field_ptr, ok_payload_gep
        ));

        let ok_variant_name = if is_option { "Some" } else { "Ok" };
        let ok_variant_fields = self
            .enum_layouts
            .get(&enum_name)
            .and_then(|layout| layout.variants.iter().find(|(vn, _)| vn == ok_variant_name))
            .map(|(_, fields)| fields.clone())
            .unwrap_or_default();

        if !ok_variant_fields.is_empty() {
            let t_llvm_ty = self.llvm_type(&ok_variant_fields[0]);
            let t_val = self.fresh_temp();
            self.emit_line(&format!(
                "{} = load {}, ptr {}",
                t_val, t_llvm_ty, ok_field_ptr
            ));
            Ok(t_val)
        } else {
            Ok("0".to_string())
        }
    }

    // ── to_str polymorphic dispatch ────────────────────────

    fn emit_to_str(&mut self, arg: &Expr) -> Result<String, CodegenError> {
        let arg_llvm_ty = self.expr_llvm_type(arg);
        let val = self.emit_expr(arg)?;
        match arg_llvm_ty.as_str() {
            "i64" => {
                // int → int_to_str
                let tmp = self.fresh_temp();
                self.emit_line(&format!("{} = call ptr @int_to_str(i64 {})", tmp, val));
                Ok(tmp)
            }
            "double" => {
                // float → float_to_str
                let tmp = self.fresh_temp();
                self.emit_line(&format!("{} = call ptr @float_to_str(double {})", tmp, val));
                Ok(tmp)
            }
            "i1" => {
                // bool → bool_to_str
                let tmp = self.fresh_temp();
                self.emit_line(&format!("{} = call ptr @bool_to_str(i1 {})", tmp, val));
                Ok(tmp)
            }
            "i8" => {
                // char → str_from_char
                let tmp = self.fresh_temp();
                self.emit_line(&format!("{} = call ptr @str_from_char(i8 {})", tmp, val));
                Ok(tmp)
            }
            "ptr" => {
                // string → identity (already a ptr to char data)
                Ok(val)
            }
            _ => {
                // Unsupported type: return empty string
                Ok("@.str.empty".to_string())
            }
        }
    }

    // ── Closure helpers ──────────────────────────────────────

    fn emit_closure(&mut self, closure: &ClosureExpr) -> Result<String, CodegenError> {
        let closure_id = self.closure_counter;
        self.closure_counter += 1;
        let closure_fn_name = format!("__closure_{}", closure_id);
        let env_type_name = format!("__env_{}", closure_id);

        // Find captured variables
        let captures = self.find_captures(&closure.body, &closure.params);

        // Build env struct type
        let env_field_types: Vec<String> = captures
            .iter()
            .map(|(_, slot)| slot.llvm_ty.clone())
            .collect();
        let env_llvm_ty = if env_field_types.is_empty() {
            "{ i8 }".to_string() // dummy field for empty env
        } else {
            format!("{{ {} }}", env_field_types.join(", "))
        };

        // Add env type definition
        self.type_defs
            .push_str(&format!("%{} = type {}\n", env_type_name, env_llvm_ty));

        // Emit the closure function (deferred)
        self.emit_closure_function(&closure_fn_name, &env_type_name, &captures, closure)?;

        // At the closure site: alloca env, fill captures
        let env_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca %{}", env_ptr, env_type_name));

        for (i, (cap_name, cap_slot)) in captures.iter().enumerate() {
            let gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                gep, env_type_name, env_ptr, i
            ));
            let val = self.fresh_temp();
            self.emit_line(&format!(
                "{} = load {}, ptr {}",
                val, cap_slot.llvm_ty, cap_slot.ptr
            ));
            self.emit_line(&format!("store {} {}, ptr {}", cap_slot.llvm_ty, val, gep));
            let _ = cap_name; // used above via cap_slot
        }

        // Build { ptr, ptr } closure pair
        let pair = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {{ ptr, ptr }}", pair));

        let fn_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, ptr }}, ptr {}, i32 0, i32 0",
            fn_gep, pair
        ));
        self.emit_line(&format!("store ptr @{}, ptr {}", closure_fn_name, fn_gep));

        let env_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, ptr }}, ptr {}, i32 0, i32 1",
            env_gep, pair
        ));
        self.emit_line(&format!("store ptr {}, ptr {}", env_ptr, env_gep));

        // Return ptr to the pair
        Ok(pair)
    }

    fn emit_closure_function(
        &mut self,
        closure_fn_name: &str,
        env_type_name: &str,
        captures: &[(String, VarSlot)],
        closure: &ClosureExpr,
    ) -> Result<(), CodegenError> {
        // Save current function state
        let saved_body = std::mem::take(&mut self.body);
        let saved_temp = self.temp_counter;
        let saved_label = self.label_counter;
        let saved_terminated = self.block_terminated;
        let saved_block = std::mem::replace(&mut self.current_block, "entry".to_string());
        let saved_ret_ty = self.current_fn_ret_ty.take();
        let saved_vars = std::mem::take(&mut self.vars);

        self.temp_counter = 0;
        self.label_counter = 0;
        self.block_terminated = false;

        let ret_ty = self.llvm_type(&closure.return_type);
        self.current_fn_ret_ty = Some(ret_ty.clone());

        // Build param list: ptr %env, then explicit params
        let mut params_str = vec!["ptr %env".to_string()];
        for p in &closure.params {
            params_str.push(format!("{} %{}", self.llvm_type(&p.ty), p.name));
        }

        self.body.push_str(&format!(
            "define {} @{}({}) {{\n",
            ret_ty,
            closure_fn_name,
            params_str.join(", ")
        ));
        self.body.push_str("entry:\n");

        self.push_scope();

        // Load captures from env struct
        for (i, (cap_name, cap_slot)) in captures.iter().enumerate() {
            let gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr %env, i32 0, i32 {}",
                gep, env_type_name, i
            ));
            let ptr = format!("%{}.cap", cap_name);
            self.emit_line(&format!("{} = alloca {}", ptr, cap_slot.llvm_ty));
            let val = self.fresh_temp();
            self.emit_line(&format!("{} = load {}, ptr {}", val, cap_slot.llvm_ty, gep));
            self.emit_line(&format!("store {} {}, ptr {}", cap_slot.llvm_ty, val, ptr));
            self.define_var(cap_name, &ptr, &cap_slot.llvm_ty);
        }

        // Alloca for explicit params
        for param in &closure.params {
            let ty = self.llvm_type(&param.ty);
            let ptr = self.fresh_temp();
            self.emit_line(&format!("{} = alloca {}", ptr, ty));
            self.emit_line(&format!("store {} %{}, ptr {}", ty, param.name, ptr));
            self.define_var(&param.name, &ptr, &ty);
        }

        // Emit closure body
        self.emit_block(&closure.body)?;

        if !self.block_terminated {
            if ret_ty == "void" {
                self.emit_line("ret void");
            } else {
                self.emit_line(&format!("ret {} 0", ret_ty));
            }
        }

        self.pop_scope();
        self.body.push_str("}\n\n");

        // Save the closure function IR
        let closure_fn_ir = std::mem::take(&mut self.body);
        self.deferred_fns.push(closure_fn_ir);

        // Restore saved state
        self.body = saved_body;
        self.temp_counter = saved_temp;
        self.label_counter = saved_label;
        self.block_terminated = saved_terminated;
        self.current_block = saved_block;
        self.current_fn_ret_ty = saved_ret_ty;
        self.vars = saved_vars;

        Ok(())
    }

    // ── Spawn/Join/Channel codegen ───────────────────────────

    fn emit_spawn(&mut self, block: &Block) -> Result<String, CodegenError> {
        let spawn_id = self.spawn_counter;
        self.spawn_counter += 1;
        let wrapper_fn_name = format!("__spawn_{}", spawn_id);
        let env_type_name = format!("__spawn_env_{}", spawn_id);

        // Find captured variables (empty params — no closure params for spawn)
        let captures = self.find_captures(block, &[]);

        // Build env struct: { captures..., result_slot (i64) }
        let mut env_field_types: Vec<String> = captures
            .iter()
            .map(|(_, slot)| slot.llvm_ty.clone())
            .collect();
        let result_field_idx = env_field_types.len();
        env_field_types.push("i64".to_string()); // result slot
        let env_llvm_ty = format!("{{ {} }}", env_field_types.join(", "));

        // Add env type definition
        self.type_defs
            .push_str(&format!("%{} = type {}\n", env_type_name, env_llvm_ty));

        // Emit the spawn wrapper function (deferred)
        self.emit_spawn_function(
            &wrapper_fn_name,
            &env_type_name,
            &captures,
            result_field_idx,
            block,
        )?;

        // At spawn site: malloc env, fill captures
        // Use LLVM sizeof idiom to get struct size with proper alignment padding
        let env_ptr = self.fresh_temp();
        let size_ptr = self.fresh_temp();
        let size_val = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr %{}, ptr null, i32 1",
            size_ptr, env_type_name
        ));
        self.emit_line(&format!("{} = ptrtoint ptr {} to i64", size_val, size_ptr));
        self.emit_line(&format!("{} = call ptr @malloc(i64 {})", env_ptr, size_val));

        for (i, (_cap_name, cap_slot)) in captures.iter().enumerate() {
            let gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                gep, env_type_name, env_ptr, i
            ));
            let val = self.fresh_temp();
            self.emit_line(&format!(
                "{} = load {}, ptr {}",
                val, cap_slot.llvm_ty, cap_slot.ptr
            ));
            self.emit_line(&format!("store {} {}, ptr {}", cap_slot.llvm_ty, val, gep));
        }

        // Malloc task control block: { i64 (pthread_t), ptr (env) }
        let tcb = self.fresh_temp();
        self.emit_line(&format!("{} = call ptr @malloc(i64 16)", tcb));

        // Call pthread_create
        let thread_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ i64, ptr }}, ptr {}, i32 0, i32 0",
            thread_gep, tcb
        ));
        self.emit_line(&format!(
            "call i32 @pthread_create(ptr {}, ptr null, ptr @{}, ptr {})",
            thread_gep, wrapper_fn_name, env_ptr
        ));

        // Store env ptr in TCB
        let env_field_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ i64, ptr }}, ptr {}, i32 0, i32 1",
            env_field_gep, tcb
        ));
        self.emit_line(&format!("store ptr {}, ptr {}", env_ptr, env_field_gep));

        // Record env info so .join() can load the result from the correct field
        self.last_spawn_env_info = Some((env_type_name, result_field_idx));

        Ok(tcb)
    }

    fn emit_spawn_function(
        &mut self,
        wrapper_fn_name: &str,
        env_type_name: &str,
        captures: &[(String, VarSlot)],
        result_field_idx: usize,
        block: &Block,
    ) -> Result<(), CodegenError> {
        // Save current function state
        let saved_body = std::mem::take(&mut self.body);
        let saved_temp = self.temp_counter;
        let saved_label = self.label_counter;
        let saved_terminated = self.block_terminated;
        let saved_block = std::mem::replace(&mut self.current_block, "entry".to_string());
        let saved_ret_ty = self.current_fn_ret_ty.take();
        let saved_vars = std::mem::take(&mut self.vars);
        let saved_contracts = std::mem::take(&mut self.current_fn_contracts);
        let saved_fn_name = std::mem::take(&mut self.current_fn_name);

        self.temp_counter = 0;
        self.label_counter = 0;
        self.block_terminated = false;
        self.current_fn_ret_ty = Some("ptr".to_string());
        self.current_fn_contracts = Vec::new();
        self.current_fn_name = wrapper_fn_name.to_string();

        self.body
            .push_str(&format!("define ptr @{}(ptr %env) {{\n", wrapper_fn_name));
        self.body.push_str("entry:\n");

        self.push_scope();

        // Load captures from env struct
        for (i, (cap_name, cap_slot)) in captures.iter().enumerate() {
            let gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr %env, i32 0, i32 {}",
                gep, env_type_name, i
            ));
            let ptr = format!("%{}.cap", cap_name);
            self.emit_line(&format!("{} = alloca {}", ptr, cap_slot.llvm_ty));
            let val = self.fresh_temp();
            self.emit_line(&format!("{} = load {}, ptr {}", val, cap_slot.llvm_ty, gep));
            self.emit_line(&format!("store {} {}, ptr {}", cap_slot.llvm_ty, val, ptr));
            self.define_var(cap_name, &ptr, &cap_slot.llvm_ty);
        }

        // Set spawn context so emit_return stores into result slot and returns null
        self.spawn_return_ctx = Some((env_type_name.to_string(), result_field_idx));
        self.emit_block(block)?;
        self.spawn_return_ctx = None;

        if !self.block_terminated {
            // Store default result 0 and return
            let result_gep = self.fresh_temp();
            self.emit_line(&format!(
                "{} = getelementptr %{}, ptr %env, i32 0, i32 {}",
                result_gep, env_type_name, result_field_idx
            ));
            self.emit_line(&format!("store i64 0, ptr {}", result_gep));
            self.emit_line("ret ptr null");
        }

        self.pop_scope();
        self.body.push_str("}\n\n");

        // Save the spawn function IR
        let spawn_fn_ir = std::mem::take(&mut self.body);
        self.deferred_fns.push(spawn_fn_ir);

        // Restore saved state
        self.body = saved_body;
        self.temp_counter = saved_temp;
        self.label_counter = saved_label;
        self.block_terminated = saved_terminated;
        self.current_block = saved_block;
        self.current_fn_ret_ty = saved_ret_ty;
        self.vars = saved_vars;
        self.current_fn_contracts = saved_contracts;
        self.current_fn_name = saved_fn_name;

        Ok(())
    }

    fn emit_task_join(&mut self, receiver: &Expr) -> Result<String, CodegenError> {
        // Load task control block pointer
        let tcb = self.emit_expr(receiver)?;

        // Load pthread_t from TCB field 0
        let thread_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ i64, ptr }}, ptr {}, i32 0, i32 0",
            thread_gep, tcb
        ));
        let thread = self.fresh_temp();
        self.emit_line(&format!("{} = load i64, ptr {}", thread, thread_gep));

        // Call pthread_join
        self.emit_line(&format!("call i32 @pthread_join(i64 {}, ptr null)", thread));

        // Load env ptr from TCB field 1
        let env_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ i64, ptr }}, ptr {}, i32 0, i32 1",
            env_gep, tcb
        ));
        let env_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = load ptr, ptr {}", env_ptr, env_gep));

        // Load the result from the env struct's result slot.
        // The receiver variable name maps to (env_type_name, result_field_idx)
        // recorded when the task was created via emit_spawn.
        if let ExprKind::Ident(ref task_name) = receiver.kind {
            if let Some((ref env_type, result_idx)) = self.task_env_info.get(task_name).cloned() {
                let result_gep = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
                    result_gep, env_type, env_ptr, result_idx
                ));
                let result = self.fresh_temp();
                self.emit_line(&format!("{} = load i64, ptr {}", result, result_gep));
                return Ok(result);
            }
        }

        // Fallback: no env info available (shouldn't happen in well-typed code)
        let result = self.fresh_temp();
        self.emit_line(&format!("{} = add i64 0, 0", result));

        Ok(result)
    }

    // ── Option/Result method codegen ─────────────────────────────

    /// Emit `.unwrap()` for Option/Result: check tag == 0 (Some/Ok), extract payload, abort if wrong.
    fn emit_option_unwrap(
        &mut self,
        receiver: &Expr,
        enum_name: &str,
        layout: &EnumLayout,
    ) -> Result<String, CodegenError> {
        let ptr = self.emit_expr_ptr(receiver)?;
        // Load tag
        let tag_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr %{}, ptr {}, i32 0, i32 0",
            tag_gep, enum_name, ptr
        ));
        let tag = self.fresh_temp();
        self.emit_line(&format!("{} = load i32, ptr {}", tag, tag_gep));

        // Check tag == 0 (Some/Ok variant)
        let cmp = self.fresh_temp();
        self.emit_line(&format!("{} = icmp eq i32 {}, 0", cmp, tag));
        let ok_label = self.fresh_label("unwrap_ok");
        let fail_label = self.fresh_label("unwrap_fail");
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cmp, ok_label, fail_label
        ));

        // Fail: abort
        self.emit_label(&fail_label);
        self.emit_line("call void @abort()");
        self.emit_line("unreachable");

        // Ok: extract payload
        self.emit_label(&ok_label);
        // Get payload type from first variant's first field
        let payload_ty = if let Some((_, fields)) = layout.variants.first() {
            if let Some(field_ty) = fields.first() {
                self.llvm_type(field_ty)
            } else {
                "i64".to_string()
            }
        } else {
            "i64".to_string()
        };
        let payload_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr %{}, ptr {}, i32 0, i32 1",
            payload_gep, enum_name, ptr
        ));
        let val = self.fresh_temp();
        self.emit_line(&format!(
            "{} = load {}, ptr {}",
            val, payload_ty, payload_gep
        ));
        Ok(val)
    }

    /// Emit `.unwrap_err()` for Result: check tag == 1 (Err), extract payload.
    fn emit_result_unwrap_err(
        &mut self,
        receiver: &Expr,
        enum_name: &str,
        layout: &EnumLayout,
    ) -> Result<String, CodegenError> {
        let ptr = self.emit_expr_ptr(receiver)?;
        // Load tag
        let tag_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr %{}, ptr {}, i32 0, i32 0",
            tag_gep, enum_name, ptr
        ));
        let tag = self.fresh_temp();
        self.emit_line(&format!("{} = load i32, ptr {}", tag, tag_gep));

        // Check tag == 1 (Err variant)
        let cmp = self.fresh_temp();
        self.emit_line(&format!("{} = icmp eq i32 {}, 1", cmp, tag));
        let ok_label = self.fresh_label("unwrap_err_ok");
        let fail_label = self.fresh_label("unwrap_err_fail");
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cmp, ok_label, fail_label
        ));

        // Fail: abort
        self.emit_label(&fail_label);
        self.emit_line("call void @abort()");
        self.emit_line("unreachable");

        // Ok: extract Err payload
        self.emit_label(&ok_label);
        let payload_ty = if let Some((_, fields)) = layout.variants.get(1) {
            if let Some(field_ty) = fields.first() {
                self.llvm_type(field_ty)
            } else {
                "i64".to_string()
            }
        } else {
            "i64".to_string()
        };
        let payload_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr %{}, ptr {}, i32 0, i32 1",
            payload_gep, enum_name, ptr
        ));
        let val = self.fresh_temp();
        self.emit_line(&format!(
            "{} = load {}, ptr {}",
            val, payload_ty, payload_gep
        ));
        Ok(val)
    }

    /// Emit `.is_some()`/`.is_none()`/`.is_ok()`/`.is_err()`: compare tag against expected value.
    fn emit_enum_tag_check(
        &mut self,
        receiver: &Expr,
        enum_name: &str,
        expected_tag: u32,
    ) -> Result<String, CodegenError> {
        let ptr = self.emit_expr_ptr(receiver)?;
        let tag_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr %{}, ptr {}, i32 0, i32 0",
            tag_gep, enum_name, ptr
        ));
        let tag = self.fresh_temp();
        self.emit_line(&format!("{} = load i32, ptr {}", tag, tag_gep));
        let cmp = self.fresh_temp();
        self.emit_line(&format!("{} = icmp eq i32 {}, {}", cmp, tag, expected_tag));
        Ok(cmp)
    }

    fn emit_chan_create(&mut self) -> Result<String, CodegenError> {
        let ch = self.fresh_temp();
        self.emit_line(&format!("{} = call ptr @__yorum_chan_create()", ch));
        Ok(ch)
    }

    fn emit_chan_send(&mut self, ch_expr: &Expr, val_expr: &Expr) -> Result<String, CodegenError> {
        let ch = self.emit_expr(ch_expr)?;
        let val = self.emit_expr(val_expr)?;
        self.emit_line(&format!(
            "call void @__yorum_chan_send(ptr {}, i64 {})",
            ch, val
        ));
        Ok("void".to_string())
    }

    fn emit_chan_recv(&mut self, ch_expr: &Expr) -> Result<String, CodegenError> {
        let ch = self.emit_expr(ch_expr)?;
        let result = self.fresh_temp();
        self.emit_line(&format!(
            "{} = call i64 @__yorum_chan_recv(ptr {})",
            result, ch
        ));
        Ok(result)
    }

    fn emit_indirect_call(&mut self, callee: &Expr, args: &[Expr]) -> Result<String, CodegenError> {
        // Emit the callee to get the closure pair pointer
        let closure_ptr = self.emit_expr(callee)?;

        // Extract fn_ptr from { ptr, ptr } pair
        let fn_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, ptr }}, ptr {}, i32 0, i32 0",
            fn_gep, closure_ptr
        ));
        let fn_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = load ptr, ptr {}", fn_ptr, fn_gep));

        // Extract env_ptr
        let env_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, ptr }}, ptr {}, i32 0, i32 1",
            env_gep, closure_ptr
        ));
        let env_ptr = self.fresh_temp();
        self.emit_line(&format!("{} = load ptr, ptr {}", env_ptr, env_gep));

        // Emit arguments
        let mut arg_strs = vec![format!("ptr {}", env_ptr)];
        for arg in args {
            let val = self.emit_expr(arg)?;
            let ty = self.expr_llvm_type(arg);
            arg_strs.push(format!("{} {}", ty, val));
        }

        // Determine return type from closure_var_types
        let ret_type = if let ExprKind::Ident(name) = &callee.kind {
            if let Some(Type::Fn(_, ret)) = self.closure_var_types.get(name) {
                self.llvm_type(ret)
            } else {
                "i64".to_string() // fallback
            }
        } else {
            "i64".to_string() // fallback
        };

        if ret_type == "void" {
            self.emit_line(&format!("call void {}({})", fn_ptr, arg_strs.join(", ")));
            Ok("void".to_string())
        } else {
            let tmp = self.fresh_temp();
            self.emit_line(&format!(
                "{} = call {} {}({})",
                tmp,
                ret_type,
                fn_ptr,
                arg_strs.join(", ")
            ));
            Ok(tmp)
        }
    }

    fn find_captures(&self, body: &Block, params: &[Param]) -> Vec<(String, VarSlot)> {
        let param_names: std::collections::HashSet<String> =
            params.iter().map(|p| p.name.clone()).collect();
        let mut ident_names = std::collections::HashSet::new();
        Self::collect_idents_from_block(body, &param_names, &mut ident_names);

        let mut captures = Vec::new();
        for name in ident_names {
            if let Some(slot) = self.lookup_var(&name) {
                captures.push((name, slot.clone()));
            }
        }
        captures.sort_by(|a, b| a.0.cmp(&b.0));
        captures
    }

    fn collect_idents_from_block(
        block: &Block,
        locals: &std::collections::HashSet<String>,
        names: &mut std::collections::HashSet<String>,
    ) {
        let mut local_scope = locals.clone();
        for stmt in &block.stmts {
            Self::collect_idents_from_stmt(stmt, &mut local_scope, names);
        }
    }

    fn collect_idents_from_stmt(
        stmt: &Stmt,
        locals: &mut std::collections::HashSet<String>,
        names: &mut std::collections::HashSet<String>,
    ) {
        match stmt {
            Stmt::Let(s) => {
                Self::collect_idents_from_expr(&s.value, locals, names);
                locals.insert(s.name.clone());
            }
            Stmt::Assign(s) => {
                Self::collect_idents_from_expr(&s.target, locals, names);
                Self::collect_idents_from_expr(&s.value, locals, names);
            }
            Stmt::Return(s) => {
                Self::collect_idents_from_expr(&s.value, locals, names);
            }
            Stmt::If(s) => {
                Self::collect_idents_from_expr(&s.condition, locals, names);
                Self::collect_idents_from_block(&s.then_block, locals, names);
                if let Some(eb) = &s.else_branch {
                    match eb.as_ref() {
                        ElseBranch::ElseIf(elif) => {
                            Self::collect_idents_from_stmt(&Stmt::If(elif.clone()), locals, names);
                        }
                        ElseBranch::Else(block) => {
                            Self::collect_idents_from_block(block, locals, names);
                        }
                    }
                }
            }
            Stmt::While(s) => {
                Self::collect_idents_from_expr(&s.condition, locals, names);
                Self::collect_idents_from_block(&s.body, locals, names);
            }
            Stmt::For(s) => {
                Self::collect_idents_from_expr(&s.iterable, locals, names);
                locals.insert(s.var_name.clone());
                Self::collect_idents_from_block(&s.body, locals, names);
            }
            Stmt::Match(s) => {
                Self::collect_idents_from_expr(&s.subject, locals, names);
                for arm in &s.arms {
                    Self::collect_idents_from_block(&arm.body, locals, names);
                }
            }
            Stmt::Expr(s) => {
                Self::collect_idents_from_expr(&s.expr, locals, names);
            }
            Stmt::Break(_) | Stmt::Continue(_) => {}
        }
    }

    fn collect_idents_from_expr(
        expr: &Expr,
        locals: &std::collections::HashSet<String>,
        names: &mut std::collections::HashSet<String>,
    ) {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if !locals.contains(name) {
                    names.insert(name.clone());
                }
            }
            ExprKind::Binary(l, _, r) => {
                Self::collect_idents_from_expr(l, locals, names);
                Self::collect_idents_from_expr(r, locals, names);
            }
            ExprKind::Unary(_, operand) => {
                Self::collect_idents_from_expr(operand, locals, names);
            }
            ExprKind::Call(callee, args) => {
                Self::collect_idents_from_expr(callee, locals, names);
                for arg in args {
                    Self::collect_idents_from_expr(arg, locals, names);
                }
            }
            ExprKind::FieldAccess(obj, _) => {
                Self::collect_idents_from_expr(obj, locals, names);
            }
            ExprKind::MethodCall(recv, _, args) => {
                Self::collect_idents_from_expr(recv, locals, names);
                for arg in args {
                    Self::collect_idents_from_expr(arg, locals, names);
                }
            }
            ExprKind::Index(arr, idx) => {
                Self::collect_idents_from_expr(arr, locals, names);
                Self::collect_idents_from_expr(idx, locals, names);
            }
            ExprKind::StructInit(_, fields) => {
                for fi in fields {
                    Self::collect_idents_from_expr(&fi.value, locals, names);
                }
            }
            ExprKind::ArrayLit(elements) | ExprKind::TupleLit(elements) => {
                for elem in elements {
                    Self::collect_idents_from_expr(elem, locals, names);
                }
            }
            ExprKind::Closure(c) => {
                let mut inner_locals = locals.clone();
                for p in &c.params {
                    inner_locals.insert(p.name.clone());
                }
                Self::collect_idents_from_block(&c.body, &inner_locals, names);
            }
            ExprKind::Spawn(block) => {
                Self::collect_idents_from_block(block, locals, names);
            }
            ExprKind::Range(start, end) | ExprKind::RangeInclusive(start, end) => {
                Self::collect_idents_from_expr(start, locals, names);
                Self::collect_idents_from_expr(end, locals, names);
            }
            ExprKind::Try(inner) => {
                Self::collect_idents_from_expr(inner, locals, names);
            }
            ExprKind::Literal(_) => {}
        }
    }

    // ── Type helpers ─────────────────────────────────────────

    fn llvm_type(&self, ty: &Type) -> String {
        match ty {
            Type::Int => "i64".to_string(),
            Type::Float => "double".to_string(),
            Type::Bool => "i1".to_string(),
            Type::Char => "i8".to_string(),
            Type::Str => "ptr".to_string(),
            Type::Unit => "void".to_string(),
            Type::Named(name) => {
                if self.struct_layouts.contains_key(name) || self.enum_layouts.contains_key(name) {
                    format!("%{}", name)
                } else if name.starts_with("Map__") || name.starts_with("Set__") {
                    "ptr".to_string()
                } else {
                    "i64".to_string() // fallback
                }
            }
            Type::Tuple(types) => {
                let name = self.tuple_type_name(types);
                format!("%{}", name)
            }
            Type::Array(_) => "ptr".to_string(),
            Type::Ref(_) | Type::MutRef(_) | Type::Own(_) => "ptr".to_string(),
            Type::SelfType | Type::TypeVar(_) => {
                debug_assert!(
                    false,
                    "Type::SelfType/TypeVar should be resolved before codegen"
                );
                "i64".to_string()
            }
            Type::Generic(name, _) => {
                if name == "Map" || name == "Set" {
                    "ptr".to_string()
                } else {
                    format!("%{}", name)
                }
            }
            Type::Fn(_, _) => "ptr".to_string(),
            Type::Task(_) => "ptr".to_string(),
            Type::Chan(_) => "ptr".to_string(),
        }
    }

    fn type_size(&self, ty: &Type) -> usize {
        match ty {
            Type::Int => 8,
            Type::Float => 8,
            Type::Bool => 1,
            Type::Char => 1,
            Type::Str => 8,
            Type::Unit => 0,
            Type::Named(name) => {
                if let Some(layout) = self.struct_layouts.get(name) {
                    layout.fields.iter().map(|(_, t)| self.type_size(t)).sum()
                } else {
                    8
                }
            }
            Type::SelfType | Type::TypeVar(_) => {
                debug_assert!(
                    false,
                    "Type::SelfType/TypeVar should be resolved before codegen"
                );
                8
            }
            Type::Generic(_, _) | Type::Fn(_, _) => 8,
            _ => 8,
        }
    }

    /// Heuristic to determine if an expression produces a float value.
    fn expr_is_float(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Literal(Literal::Float(_)) => true,
            ExprKind::Literal(Literal::Int(_)) => false,
            ExprKind::Literal(Literal::Bool(_)) => false,
            ExprKind::Literal(Literal::Char(_)) => false,
            ExprKind::Literal(Literal::String(_)) => false,
            ExprKind::Literal(Literal::Unit) => false,
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    slot.llvm_ty == "double"
                } else {
                    false
                }
            }
            ExprKind::Binary(lhs, op, _) => {
                // Comparison operators always produce bool
                match op {
                    BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::LtEq
                    | BinOp::GtEq
                    | BinOp::And
                    | BinOp::Or
                    | BinOp::BitAnd
                    | BinOp::BitOr
                    | BinOp::BitXor
                    | BinOp::Shl
                    | BinOp::Shr => false,
                    _ => self.expr_is_float(lhs),
                }
            }
            ExprKind::Unary(UnaryOp::Not, _) => false,
            ExprKind::Unary(_, operand) => self.expr_is_float(operand),
            ExprKind::Call(callee, _) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    if let Some(ret_ty) = self.fn_ret_types.get(name.as_str()) {
                        return *ret_ty == Type::Float;
                    }
                }
                false
            }
            ExprKind::FieldAccess(obj, field) => {
                if let Ok(sname) = self.expr_struct_name(obj) {
                    // Tuple field access
                    if sname.starts_with("tuple.") {
                        if let Ok(idx) = field.parse::<usize>() {
                            if let Some(elem_types) = self.tuple_elem_types.get(&sname) {
                                if idx < elem_types.len() {
                                    return elem_types[idx] == "double";
                                }
                            }
                        }
                        return false;
                    }
                    if let Ok((_, fty)) = self.struct_field_index(&sname, field) {
                        return fty == Type::Float;
                    }
                }
                false
            }
            ExprKind::MethodCall(receiver, method_name, _) => {
                if let Ok(sname) = self.expr_struct_name(receiver) {
                    let mangled = format!("{}_{}", sname, method_name);
                    if let Some(ret_ty) = self.fn_ret_types.get(&mangled) {
                        return *ret_ty == Type::Float;
                    }
                }
                false
            }
            ExprKind::ArrayLit(_) => false,
            ExprKind::Index(arr_expr, _) => {
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    if let Some(elem_ty) = self.array_elem_types.get(name) {
                        return elem_ty == "double";
                    }
                }
                false
            }
            ExprKind::Closure(_) => false,
            _ => false,
        }
    }

    /// Heuristic to determine if an expression produces a char (i8) value.
    fn expr_is_char(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Literal(Literal::Char(_)) => true,
            ExprKind::Literal(_) => false,
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    slot.llvm_ty == "i8"
                } else {
                    false
                }
            }
            ExprKind::Call(callee, _) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    if let Some(ret_ty) = self.fn_ret_types.get(name.as_str()) {
                        return *ret_ty == Type::Char;
                    }
                }
                false
            }
            ExprKind::Index(arr_expr, _) => {
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    if let Some(elem_ty) = self.array_elem_types.get(name) {
                        return elem_ty == "i8";
                    }
                }
                false
            }
            _ => false,
        }
    }

    fn expr_llvm_type(&self, expr: &Expr) -> String {
        match &expr.kind {
            ExprKind::Literal(Literal::Int(_)) => "i64".to_string(),
            ExprKind::Literal(Literal::Float(_)) => "double".to_string(),
            ExprKind::Literal(Literal::Bool(_)) => "i1".to_string(),
            ExprKind::Literal(Literal::Char(_)) => "i8".to_string(),
            ExprKind::Literal(Literal::String(_)) => "ptr".to_string(),
            ExprKind::Literal(Literal::Unit) => "void".to_string(),
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    // Array idents are returned as raw pointers by emit_expr (reference semantics)
                    if slot.llvm_ty == "{ ptr, i64, i64 }" {
                        "ptr".to_string()
                    } else {
                        slot.llvm_ty.clone()
                    }
                } else {
                    "i64".to_string()
                }
            }
            ExprKind::Binary(_, op, _) => match op {
                BinOp::Eq
                | BinOp::NotEq
                | BinOp::Lt
                | BinOp::Gt
                | BinOp::LtEq
                | BinOp::GtEq
                | BinOp::And
                | BinOp::Or => "i1".to_string(),
                BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::Shl | BinOp::Shr => {
                    "i64".to_string()
                }
                _ => {
                    if self.expr_is_float(expr) {
                        "double".to_string()
                    } else {
                        "i64".to_string()
                    }
                }
            },
            ExprKind::Unary(UnaryOp::Not, _) => "i1".to_string(),
            ExprKind::Unary(_, operand) => self.expr_llvm_type(operand),
            ExprKind::Call(callee, _) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    if let Some(ret_ty) = self.fn_ret_types.get(name.as_str()) {
                        return self.llvm_type(ret_ty);
                    }
                }
                "i64".to_string()
            }
            ExprKind::FieldAccess(obj, field) => {
                if let Ok(sname) = self.expr_struct_name(obj) {
                    // Tuple field access
                    if sname.starts_with("tuple.") {
                        if let Ok(idx) = field.parse::<usize>() {
                            if let Some(elem_types) = self.tuple_elem_types.get(&sname) {
                                if idx < elem_types.len() {
                                    return elem_types[idx].clone();
                                }
                            }
                        }
                        return "i64".to_string();
                    }
                    if let Ok((_, fty)) = self.struct_field_index(&sname, field) {
                        return self.llvm_type(&fty);
                    }
                }
                "i64".to_string()
            }
            ExprKind::MethodCall(receiver, method_name, _) => {
                // Option/Result boolean methods are special-cased in emit_expr
                // and return i1 (icmp result), not registered in fn_ret_types
                match method_name.as_str() {
                    "is_some" | "is_none" | "is_ok" | "is_err" => return "i1".to_string(),
                    _ => {}
                }
                if let Ok(sname) = self.expr_struct_name(receiver) {
                    let mangled = format!("{}_{}", sname, method_name);
                    if let Some(ret_ty) = self.fn_ret_types.get(&mangled) {
                        return self.llvm_type(ret_ty);
                    }
                }
                "i64".to_string()
            }
            ExprKind::ArrayLit(_) => "{ ptr, i64, i64 }".to_string(),
            ExprKind::Index(arr_expr, _) => {
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    if let Some(elem_ty) = self.array_elem_types.get(name) {
                        return elem_ty.clone();
                    }
                }
                "i64".to_string()
            }
            ExprKind::StructInit(name, _) => format!("%{}", name),
            ExprKind::Closure(_) => "ptr".to_string(),
            ExprKind::TupleLit(elements) => {
                // Infer element types and construct the tuple type name using semantic names
                // (matching emit_tuple_lit and tuple_type_name conventions)
                let elem_types: Vec<String> =
                    elements.iter().map(|e| self.expr_llvm_type(e)).collect();
                let semantic_name = elem_types
                    .iter()
                    .map(|t| llvm_to_semantic_name(t))
                    .collect::<Vec<_>>()
                    .join(".");
                format!("%tuple.{}", semantic_name)
            }
            ExprKind::Spawn(_) => "ptr".to_string(),
            ExprKind::Range(_, _) | ExprKind::RangeInclusive(_, _) => "i64".to_string(),
            ExprKind::Try(inner) => {
                // The result type of ? is T from Option<T> or T from Result<T, E>
                let inner_ty = self.expr_llvm_type(inner);
                if let Some(enum_name) = inner_ty.strip_prefix('%') {
                    if let Some(layout) = self.enum_layouts.get(enum_name) {
                        // First variant is Some/Ok — its first field is T
                        if let Some((_, fields)) = layout.variants.first() {
                            if !fields.is_empty() {
                                return self.llvm_type(&fields[0]);
                            }
                        }
                    }
                }
                "i64".to_string()
            }
        }
    }

    fn expr_struct_name(&self, expr: &Expr) -> Result<String, CodegenError> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    let ty = &slot.llvm_ty;
                    if let Some(stripped) = ty.strip_prefix('%') {
                        Ok(stripped.to_string())
                    } else {
                        Err(CodegenError {
                            message: format!("'{}' is not a struct", name),
                        })
                    }
                } else {
                    Err(CodegenError {
                        message: format!("undefined variable '{}'", name),
                    })
                }
            }
            ExprKind::Index(arr_expr, _) => {
                // Array indexing: element type is stored in array_elem_types
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    if let Some(elem_ty) = self.array_elem_types.get(name) {
                        if let Some(stripped) = elem_ty.strip_prefix('%') {
                            return Ok(stripped.to_string());
                        }
                    }
                }
                Err(CodegenError {
                    message: "cannot determine struct name from index expression".to_string(),
                })
            }
            ExprKind::StructInit(name, _) => Ok(name.clone()),
            ExprKind::Call(callee, _) => {
                // Function call: look up return type from function signatures
                if let ExprKind::Ident(fn_name) = &callee.kind {
                    if let Some(Type::Named(name)) = self.fn_ret_types.get(fn_name) {
                        return Ok(name.clone());
                    }
                }
                Err(CodegenError {
                    message: "cannot determine struct name from call expression".to_string(),
                })
            }
            _ => Err(CodegenError {
                message: "cannot determine struct name from expression".to_string(),
            }),
        }
    }

    fn struct_field_index(
        &self,
        struct_name: &str,
        field_name: &str,
    ) -> Result<(usize, Type), CodegenError> {
        let layout = self
            .struct_layouts
            .get(struct_name)
            .ok_or_else(|| CodegenError {
                message: format!("undefined struct '{}'", struct_name),
            })?;
        for (i, (fname, fty)) in layout.fields.iter().enumerate() {
            if fname == field_name {
                return Ok((i, fty.clone()));
            }
        }
        Err(CodegenError {
            message: format!("struct '{}' has no field '{}'", struct_name, field_name),
        })
    }

    fn find_enum_for_variant(&self, variant_name: &str) -> Option<String> {
        // Use the expected enum hint first for disambiguation
        if let Some(ref expected) = self.current_expected_enum {
            if let Some(layout) = self.enum_layouts.get(expected) {
                if layout.variants.iter().any(|(vn, _)| vn == variant_name) {
                    return Some(expected.clone());
                }
            }
        }
        // Fallback: search all enums
        for (ename, layout) in &self.enum_layouts {
            for (vname, _) in &layout.variants {
                if vname == variant_name {
                    return Some(ename.clone());
                }
            }
        }
        None
    }

    fn expr_enum_name(&self, expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if let Some(slot) = self.lookup_var(name) {
                    if let Some(stripped) = slot.llvm_ty.strip_prefix('%') {
                        if self.enum_layouts.contains_key(stripped) {
                            return Some(stripped.to_string());
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Check if emit_expr for this expression returns a pointer to an aggregate
    /// (enum variant constructor, struct init, tuple lit) rather than a loaded value.
    fn expr_returns_ptr(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::StructInit(_, _) | ExprKind::TupleLit(_) => true,
            ExprKind::Call(callee, _) => {
                // Enum variant constructors (e.g., Some(42)) return alloca pointers
                if let ExprKind::Ident(name) = &callee.kind {
                    self.find_enum_for_variant(name).is_some()
                } else {
                    false
                }
            }
            ExprKind::Ident(name) => {
                // Data-less enum variant (e.g., None) returns alloca pointer
                if self.lookup_var(name).is_some() {
                    false // regular variable, emit_expr loads it
                } else {
                    self.find_enum_for_variant(name).is_some()
                }
            }
            ExprKind::Index(arr_expr, _) => {
                // Array index of aggregate types returns alloca pointer (via memcpy)
                if let ExprKind::Ident(name) = &arr_expr.kind {
                    self.array_elem_types
                        .get(name)
                        .map(|ty| Self::is_aggregate_type(ty))
                        .unwrap_or(false)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn variant_tag(&self, variant_name: &str) -> Result<i32, CodegenError> {
        for layout in self.enum_layouts.values() {
            for (i, (vname, _)) in layout.variants.iter().enumerate() {
                if vname == variant_name {
                    return Ok(i as i32);
                }
            }
        }
        Err(CodegenError {
            message: format!("unknown variant '{}'", variant_name),
        })
    }

    // ── Variable scope management ────────────────────────────

    fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.vars.pop();
    }

    fn define_var(&mut self, name: &str, ptr: &str, llvm_ty: &str) {
        if let Some(scope) = self.vars.last_mut() {
            scope.insert(
                name.to_string(),
                VarSlot {
                    ptr: ptr.to_string(),
                    llvm_ty: llvm_ty.to_string(),
                },
            );
        }
    }

    fn lookup_var(&self, name: &str) -> Option<&VarSlot> {
        for scope in self.vars.iter().rev() {
            if let Some(slot) = scope.get(name) {
                return Some(slot);
            }
        }
        None
    }

    // ── Output helpers ───────────────────────────────────────

    fn emit_line(&mut self, line: &str) {
        self.body.push_str("  ");
        self.body.push_str(line);
        self.body.push('\n');
    }

    fn emit_label(&mut self, label: &str) {
        self.body.push_str(label);
        self.body.push_str(":\n");
        self.block_terminated = false;
        self.current_block = label.to_string();
    }

    fn current_block_label(&self) -> String {
        self.current_block.clone()
    }

    fn fresh_temp(&mut self) -> String {
        let n = self.temp_counter;
        self.temp_counter += 1;
        format!("%.t{}", n)
    }

    fn fresh_label(&mut self, prefix: &str) -> String {
        let n = self.label_counter;
        self.label_counter += 1;
        format!("{}.{}", prefix, n)
    }

    fn escape_llvm_string(&self, s: &str) -> String {
        let mut out = String::new();
        for c in s.chars() {
            match c {
                '\n' => out.push_str("\\0A"),
                '\t' => out.push_str("\\09"),
                '\r' => out.push_str("\\0D"),
                '\\' => out.push_str("\\5C"),
                '"' => out.push_str("\\22"),
                '\0' => out.push_str("\\00"),
                c if c.is_ascii() && !c.is_ascii_control() => out.push(c),
                c => {
                    // Encode as UTF-8 hex bytes
                    let mut buf = [0u8; 4];
                    let encoded = c.encode_utf8(&mut buf);
                    for b in encoded.bytes() {
                        out.push_str(&format!("\\{:02X}", b));
                    }
                }
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::lexer::Lexer;
    use crate::compiler::parser::Parser;

    fn compile_to_ir(input: &str) -> String {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();
        let mut codegen = Codegen::new();
        codegen.generate(&program).unwrap()
    }

    #[test]
    fn test_simple_function() {
        let ir = compile_to_ir("fn main() -> int { return 42; }");
        assert!(ir.contains("define i64 @main(i32 %__argc, ptr %__argv)"));
        assert!(ir.contains("ret i64 42"));
    }

    #[test]
    fn test_arithmetic() {
        let ir = compile_to_ir("fn add(a: int, b: int) -> int { return a + b; }");
        assert!(ir.contains("define i64 @add(i64 %a, i64 %b)"));
        assert!(ir.contains("add i64"));
    }

    #[test]
    fn test_if_else() {
        let ir = compile_to_ir(
            "fn abs(x: int) -> int { if x < 0 { return 0 - x; } else { return x; } }",
        );
        assert!(ir.contains("icmp slt"));
        assert!(ir.contains("br i1"));
    }

    #[test]
    fn test_while_loop() {
        let ir = compile_to_ir(
            "fn countdown(n: int) -> int { let mut x: int = n; while x > 0 { x = x - 1; } return x; }",
        );
        assert!(ir.contains("while.cond"));
        assert!(ir.contains("while.body"));
        assert!(ir.contains("while.end"));
    }

    #[test]
    fn test_function_call() {
        let ir = compile_to_ir(
            "fn double(x: int) -> int { return x * 2; } fn main() -> int { return double(21); }",
        );
        assert!(ir.contains("call i64 @double(i64 21)"));
    }
}
