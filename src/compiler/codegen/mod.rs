//! LLVM IR code generation for the Yorum language.
//!
//! Emits textual LLVM IR that can be compiled by `llc` + `clang` or consumed
//! by any LLVM-based toolchain.  Uses the alloca/load/store pattern so that
//! LLVM's mem2reg pass promotes stack slots to SSA registers automatically.

use crate::compiler::ast::*;
use std::collections::{HashMap, HashSet};
use std::fmt;

mod builtins;
mod pipelines;
mod statements;
mod types;

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
pub(crate) struct VarSlot {
    pub(crate) ptr: String,     // LLVM alloca name (e.g., "%x.addr")
    pub(crate) llvm_ty: String, // LLVM type string (e.g., "i64")
}

/// A single step in an iterator pipeline (.map or .filter).
pub(crate) enum IterStep<'a> {
    Map(&'a ClosureExpr),
    Filter(&'a ClosureExpr),
    Enumerate,
    Zip(&'a Expr),
    Take(&'a Expr),
    Skip(&'a Expr),
    Chain(&'a Expr),
    TakeWhile(&'a ClosureExpr),
    Rev,
    FlatMap(&'a ClosureExpr),
    Flatten,
}

impl IterStep<'_> {
    pub(crate) fn name(&self) -> &'static str {
        match self {
            IterStep::Map(_) => "map",
            IterStep::Filter(_) => "filter",
            IterStep::Enumerate => "enumerate",
            IterStep::Zip(_) => "zip",
            IterStep::Take(_) => "take",
            IterStep::Skip(_) => "skip",
            IterStep::Chain(_) => "chain",
            IterStep::TakeWhile(_) => "take_while",
            IterStep::Rev => "rev",
            IterStep::FlatMap(_) => "flat_map",
            IterStep::Flatten => "flatten",
        }
    }
}

/// Pre-emitted flat_map/flatten info for nested loop emission.
pub(crate) struct FlatMapInfo {
    pub(crate) closure_info: Option<ClosureInfo>, // None for flatten
    pub(crate) inner_elem_ty: String,             // LLVM type of flattened elements
    pub(crate) step_index: usize,                 // position in steps array
}

/// A fully extracted iterator pipeline: array/range source + ordered steps.
pub(crate) struct IterPipeline<'a> {
    pub(crate) source: &'a Expr, // source expression (receiver of .iter())
    pub(crate) steps: Vec<IterStep<'a>>, // ordered pipeline steps
    pub(crate) is_range_source: bool, // true if source is Range/RangeInclusive/RangeFrom
    pub(crate) is_unbounded_range: bool, // true if source is RangeFrom
}

/// A terminator at the end of a pipeline (standalone expression, not for-loop).
pub(crate) enum PipelineTerminator<'a> {
    Reduce(&'a ClosureExpr),
    Fold(&'a Expr, &'a ClosureExpr),
    Collect,
    Find(&'a ClosureExpr),
    Any(&'a ClosureExpr),
    All(&'a ClosureExpr),
    Sum,
    Count,
    Position(&'a ClosureExpr),
}

/// A fully extracted terminated pipeline: array/range source + steps + terminator.
pub(crate) struct TerminatedPipeline<'a> {
    pub(crate) source: &'a Expr,
    pub(crate) steps: Vec<IterStep<'a>>,
    pub(crate) terminator: PipelineTerminator<'a>,
    pub(crate) is_range_source: bool, // true if source is Range/RangeInclusive/RangeFrom
    pub(crate) is_unbounded_range: bool, // true if source is RangeFrom
}

/// Pre-emitted closure info for a pipeline step.
pub(crate) struct ClosureInfo {
    pub(crate) fn_ptr: String,
    pub(crate) env_ptr: String,
    pub(crate) ret_ty: String,
}

/// Pre-emitted zip data source info.
pub(crate) struct ZipInfo {
    pub(crate) data_ptr: String,
    pub(crate) len_val: String,
    pub(crate) idx_ptr: String,
    pub(crate) elem_ty: String,
    /// For range-based zip: (start_val, end_val, inclusive)
    pub(crate) range_zip: Option<(String, bool)>,
}

/// Pre-emitted chain data source info.
pub(crate) struct ChainInfo {
    pub(crate) data_ptr: String,  // data pointer of second array
    pub(crate) first_len: String, // length of first source (for conditional load)
    pub(crate) elem_ty: String,   // element LLVM type
}

/// Info for Map.iter() — values array for implicit zip with keys.
pub(crate) struct MapIterInfo {
    pub(crate) val_data_ptr: String, // data pointer of values array
    pub(crate) val_elem_ty: String,  // LLVM type of value elements
    pub(crate) key_elem_ty: String,  // LLVM type of key elements
    pub(crate) tuple_name: String,   // tuple type name (e.g., "tuple.string.int")
}

/// Shared state returned by `emit_pipeline_loop_header`.
/// After the call, the IR cursor is at the start of the **body** block.
pub(crate) struct PipelineLoopParts {
    pub(crate) cond_label: String,
    pub(crate) end_label: String,
    pub(crate) current_val: String,
}

/// Shared context for pipeline code generation, produced by the preamble and consumed
/// by loop header, steps, and terminators.
pub(crate) struct PipelineContext {
    pub data_ptr: String,
    pub len_val: String,
    pub idx_ptr: String,
    pub src_elem_ty: String,
    pub final_elem_ty: String,
    pub closure_infos: Vec<Option<ClosureInfo>>,
    pub zip_infos: Vec<Option<ZipInfo>>,
    pub take_skip_ptrs: Vec<Option<String>>,
    pub enumerate_ptrs: Vec<Option<String>>,
    pub is_range_source: bool,
    pub is_unbounded_range: bool,
    pub range_end: Option<(String, bool)>,
    pub chain_infos: Vec<Option<ChainInfo>>,
    pub map_iter_info: Option<MapIterInfo>,
    pub flat_map_info: Option<FlatMapInfo>,
}

/// Tracks per-variable string buffer metadata for capacity-aware str_concat.
/// When `cap == 0`, the data pointer is not an owned heap buffer (e.g. a global
/// literal or a fresh allocation from a function).  The first inline concat
/// transitions it to a tracked buffer.
#[derive(Debug, Clone)]
pub(crate) struct StringBufInfo {
    pub(crate) len_ptr: String, // alloca for tracked length (i64)
    pub(crate) cap_ptr: String, // alloca for tracked capacity (i64), 0 = uninitialized
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

    /// Loop vars provably bounded: var_name -> array_name
    /// When `for i in 0..len(arr)` is detected and body doesn't mutate arr,
    /// bounds checks for `arr[i]` can be safely elided.
    bounded_loop_vars: HashMap<String, String>,

    /// Variables known to equal len(array): var_name -> array_name.
    /// Populated from `let n = len(arr)` (immutable) and `let arr = [val; n]` (n immutable).
    len_aliases: HashMap<String, String>,

    /// Names of immutable bindings (function params + non-mut let bindings).
    /// Used to verify len_aliases are safe (can't be reassigned).
    immutable_bindings: HashSet<String>,

    /// Variables provably non-negative at current program point.
    /// Populated from non-negative literal inits and len() calls.
    non_negative_vars: HashSet<String>,

    /// Saved snapshots for scope-aware push/pop of len_aliases,
    /// immutable_bindings, and non_negative_vars.
    len_alias_scopes: Vec<HashMap<String, String>>,
    immutable_binding_scopes: Vec<HashSet<String>>,
    non_negative_scopes: Vec<HashSet<String>>,

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
            bounded_loop_vars: HashMap::new(),
            len_aliases: HashMap::new(),
            immutable_bindings: HashSet::new(),
            non_negative_vars: HashSet::new(),
            len_alias_scopes: Vec::new(),
            immutable_binding_scopes: Vec::new(),
            non_negative_scopes: Vec::new(),
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

        // Emit type definitions after all functions (some enum layouts are
        // lazily registered during codegen, e.g. Option for pipeline terminators)
        self.emit_type_defs();

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
        self.bounded_loop_vars.clear();
        self.len_aliases.clear();
        self.immutable_bindings.clear();
        self.non_negative_vars.clear();
        self.len_alias_scopes.clear();
        self.immutable_binding_scopes.clear();
        self.non_negative_scopes.clear();

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
            let ptr = self.emit_alloca_store(&ty, &format!("%{}", param.name));
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
            // Track Set/Map-typed params for collection iteration inference
            match &param.ty {
                Type::Named(ref name) if name.starts_with("Set__") || name.starts_with("Map__") => {
                    self.var_ast_types
                        .insert(param.name.clone(), param.ty.clone());
                }
                Type::Generic(ref gname, ref args) if gname == "Set" || gname == "Map" => {
                    // Monomorphizer doesn't rewrite Set/Map param types to Named;
                    // store as Named with mangled name for pipeline detection
                    let suffixes: Vec<String> = args.iter().map(Self::mangle_type_suffix).collect();
                    let mangled = format!("{}__{}", gname, suffixes.join("__"));
                    self.var_ast_types
                        .insert(param.name.clone(), Type::Named(mangled));
                }
                _ => {}
            }
        }

        // Function params are immutable bindings (for len_alias tracking)
        for param in &f.params {
            self.immutable_bindings.insert(param.name.clone());
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
            let ptr = self.emit_alloca_store(&ty, &format!("%{}", param.name));
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
        // Invalidate len_aliases for any arrays mutated by this statement.
        // This ensures stale aliases (e.g. after push/pop between alias
        // definition and while-loop) are removed before we reach emit_while.
        self.invalidate_len_aliases_for_stmt(stmt);

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

    // ── Expression emission ──────────────────────────────────
    // Each `emit_expr` returns the LLVM value name (SSA temp or literal).

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
                    let tmp = self.emit_load(&slot.llvm_ty, &slot.ptr);
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
                    self.emit_cond_branch(&l, &rhs_label, &merge_label);
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
                    self.emit_cond_branch(&l, &merge_label, &rhs_label);
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
                        let len_val = self.emit_fat_ptr_field_load(&arr_ptr, 1, "i64");
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
                        let data_ptr = self.emit_fat_ptr_field_load(&arr_ptr, 0, "ptr");
                        let len_val = self.emit_fat_ptr_field_load(&arr_ptr, 1, "i64");
                        let cap_val = self.emit_fat_ptr_field_load(&arr_ptr, 2, "i64");

                        // Check if need to grow: len == cap
                        let need_grow = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = icmp eq i64 {}, {}",
                            need_grow, len_val, cap_val
                        ));
                        let grow_label = self.fresh_label("push.grow");
                        let store_label = self.fresh_label("push.store");
                        self.emit_cond_branch(&need_grow, &grow_label, &store_label);

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
                        self.emit_fat_ptr_field_store(&arr_ptr, 0, "ptr", &new_data);
                        self.emit_fat_ptr_field_store(&arr_ptr, 2, "i64", &new_cap);
                        self.emit_line(&format!("br label %{}", store_label));

                        // Store block: store element at data[len], increment length
                        self.emit_label(&store_label);
                        self.block_terminated = false;
                        let cur_data = self.emit_fat_ptr_field_load(&arr_ptr, 0, "ptr");
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
                            self.emit_memcpy(&elem_gep, &val, &elem_size.to_string());
                        } else {
                            self.emit_line(&format!("store {} {}, ptr {}", elem_ty, val, elem_gep));
                        }
                        let new_len = self.fresh_temp();
                        self.emit_line(&format!("{} = add i64 {}, 1", new_len, len_val));
                        self.emit_fat_ptr_field_store(&arr_ptr, 1, "i64", &new_len);
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
                        let len_val = self.emit_fat_ptr_field_load(&arr_ptr, 1, "i64");

                        // Check not empty: len > 0
                        let is_empty = self.fresh_temp();
                        self.emit_line(&format!("{} = icmp eq i64 {}, 0", is_empty, len_val));
                        let fail_label = self.fresh_label("pop.fail");
                        let ok_label = self.fresh_label("pop.ok");
                        self.emit_cond_branch(&is_empty, &fail_label, &ok_label);

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
                        self.emit_fat_ptr_field_store(&arr_ptr, 1, "i64", &new_len);

                        let data_ptr = self.emit_fat_ptr_field_load(&arr_ptr, 0, "ptr");
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
                            self.emit_memcpy(&tmp_alloca, &elem_gep, &sz.to_string());
                            return Ok(tmp_alloca);
                        } else {
                            let val = self.emit_load(&elem_ty, &elem_gep);
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
                        let argc32 = self.emit_load("i32", "@__yorum_argc");
                        let argc = self.fresh_temp();
                        self.emit_line(&format!("{} = zext i32 {} to i64", argc, argc32));
                        let argv = self.emit_load("ptr", "@__yorum_argv");
                        // Allocate data array and copy argv pointers
                        let bytes = self.fresh_temp();
                        self.emit_line(&format!("{} = mul i64 {}, 8", bytes, argc));
                        let data = self.fresh_temp();
                        self.emit_line(&format!("{} = call ptr @malloc(i64 {})", data, bytes));
                        self.emit_memcpy(&data, &argv, &bytes);
                        // Build { ptr, i64, i64 } fat pointer
                        let fat = self.fresh_temp();
                        self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat));
                        self.emit_fat_ptr_init(&fat, &data, &argc, &argc);
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
                        let data_ptr = self.emit_fat_ptr_field_load(&arr_ptr, 0, "ptr");
                        let src = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {}, ptr {}, i64 {}",
                            src, elem_ty, data_ptr, start
                        ));
                        self.emit_memcpy(&new_data, &src, &new_bytes);
                        // build fat pointer
                        let fat = self.fresh_temp();
                        self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat));
                        self.emit_fat_ptr_init(&fat, &new_data, &new_len, &new_len);
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
                        let len_a = self.emit_fat_ptr_field_load(&arr_a, 1, "i64");
                        let len_b = self.emit_fat_ptr_field_load(&arr_b, 1, "i64");
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
                        let data_a = self.emit_fat_ptr_field_load(&arr_a, 0, "ptr");
                        let bytes_a = self.fresh_temp();
                        self.emit_line(&format!("{} = mul i64 {}, {}", bytes_a, len_a, elem_size));
                        self.emit_memcpy(&new_data, &data_a, &bytes_a);
                        // copy b's data after a
                        let data_b = self.emit_fat_ptr_field_load(&arr_b, 0, "ptr");
                        let bytes_b = self.fresh_temp();
                        self.emit_line(&format!("{} = mul i64 {}, {}", bytes_b, len_b, elem_size));
                        let dst = self.fresh_temp();
                        self.emit_line(&format!(
                            "{} = getelementptr {}, ptr {}, i64 {}",
                            dst, elem_ty, new_data, len_a
                        ));
                        self.emit_memcpy(&dst, &data_b, &bytes_b);
                        // build fat pointer
                        let fat = self.fresh_temp();
                        self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat));
                        self.emit_fat_ptr_init(&fat, &new_data, &total_len, &total_len);
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
                        let len_val = self.emit_fat_ptr_field_load(&arr_ptr, 1, "i64");
                        let data_ptr = self.emit_fat_ptr_field_load(&arr_ptr, 0, "ptr");
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
                        let counter = self.emit_alloca_store("i64", "0");
                        let loop_label = self.fresh_label("rev.loop");
                        let body_label = self.fresh_label("rev.body");
                        let done_label = self.fresh_label("rev.done");
                        self.emit_line(&format!("br label %{}", loop_label));
                        self.emit_label(&loop_label);
                        self.block_terminated = false;
                        let i = self.emit_load("i64", &counter);
                        let cmp = self.fresh_temp();
                        self.emit_line(&format!("{} = icmp slt i64 {}, {}", cmp, i, len_val));
                        self.emit_cond_branch(&cmp, &body_label, &done_label);
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
                            self.emit_memcpy(&dst_gep, &src_gep, &elem_size.to_string());
                        } else {
                            let val = self.emit_load(&elem_ty, &src_gep);
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
                        self.emit_fat_ptr_init(&fat, &new_data, &len_val, &len_val);
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
                    let val = self.emit_struct_field_load(&struct_name, &obj_ptr, idx, fty);
                    return Ok(val);
                }

                let (idx, field_ty) = self.struct_field_index(&struct_name, field)?;
                let fty = self.llvm_type(&field_ty);
                let val = self.emit_struct_field_load(&struct_name, &obj_ptr, idx, &fty);
                Ok(val)
            }

            ExprKind::MethodCall(receiver, method_name, args) => {
                // Handle pipeline terminators (collect, fold, any, all, find, reduce)
                if let Some(pipeline) = self.try_extract_terminated_pipeline(expr) {
                    return self.emit_pipeline_terminator(&pipeline);
                }

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

                // Handle .iter() on arrays: if the receiver is not a struct,
                // it's an array .iter() — just evaluate the receiver (a no-op
                // that returns the array fat pointer). This covers all array
                // receiver forms: idents, literals, field accesses, calls, etc.
                if method_name == "iter"
                    && args.is_empty()
                    && self.expr_struct_name(receiver).is_err()
                {
                    return self.emit_expr(receiver);
                }

                // Handle .clear() on arrays: set length to 0 without deallocating
                if method_name == "clear"
                    && args.is_empty()
                    && self.expr_struct_name(receiver).is_err()
                {
                    let arr_ptr = self.emit_expr(receiver)?;
                    self.emit_fat_ptr_field_store(&arr_ptr, 1, "i64", "0");
                    return Ok("void".to_string());
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

            ExprKind::Range(_, _) | ExprKind::RangeInclusive(_, _) | ExprKind::RangeFrom(_) => {
                Err(CodegenError {
                    message: "range expression is only valid in for loops".to_string(),
                })?
            }

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
                    self.emit_struct_field_store(name, &ptr, idx, &fty, &val);
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
                    self.emit_fat_ptr_init(&ptr, "null", "0", "0");
                    return Ok(ptr);
                }
                // Determine element type from first element
                // Arrays use reference semantics (ptr to fat pointer), so when
                // elements are arrays, use ptr as the element type.
                let raw_elem_ty = self.expr_llvm_type(&elements[0]);
                let is_array_of_arrays = raw_elem_ty == "{ ptr, i64, i64 }";
                let elem_ty = if is_array_of_arrays {
                    "ptr".to_string()
                } else {
                    raw_elem_ty
                };
                // Ensure tuple type is registered before size computation;
                // expr_llvm_type infers the name but doesn't register it.
                if let ExprKind::TupleLit(ref tuple_elems) = elements[0].kind {
                    let tuple_llvm_types: Vec<String> =
                        tuple_elems.iter().map(|e| self.expr_llvm_type(e)).collect();
                    let tuple_name = elem_ty.trim_start_matches('%');
                    self.ensure_pipeline_tuple_type(tuple_name, &tuple_llvm_types);
                }
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
                        self.emit_memcpy(&gep, &val, &elem_size.to_string());
                    } else if is_array_of_arrays {
                        // Inner array fat pointers are stack allocas; heap-copy so
                        // they survive if the outer array escapes the current frame.
                        let heap_fp = self.fresh_temp();
                        self.emit_line(&format!("{} = call ptr @malloc(i64 24)", heap_fp));
                        self.emit_memcpy(&heap_fp, &val, "24");
                        self.emit_line(&format!("store ptr {}, ptr {}", heap_fp, gep));
                    } else {
                        self.emit_line(&format!("store {} {}, ptr {}", elem_ty, val, gep));
                    }
                }

                // Build { ptr, i64, i64 } fat pointer on stack
                let fat_ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat_ptr));
                let count_str = count.to_string();
                self.emit_fat_ptr_init(&fat_ptr, &data_ptr, &count_str, &count_str);

                Ok(fat_ptr)
            }

            ExprKind::ArrayRepeat(value_expr, count_expr) => {
                // Evaluate count (runtime i64)
                let count_val = self.emit_expr(count_expr)?;

                // Determine element type
                let elem_ty = self.expr_llvm_type(value_expr);
                let elem_size = self.llvm_type_size(&elem_ty);

                // Compute total bytes = count * elem_size
                let total_bytes = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = mul i64 {}, {}",
                    total_bytes, count_val, elem_size
                ));

                // malloc(total_bytes)
                let data_ptr = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = call ptr @malloc(i64 {})",
                    data_ptr, total_bytes
                ));

                // Check for zero-fill fast path: literal 0, 0.0, or false
                let is_zero_fill = match &value_expr.kind {
                    ExprKind::Literal(Literal::Int(0)) => true,
                    ExprKind::Literal(Literal::Float(f)) => *f == 0.0,
                    ExprKind::Literal(Literal::Bool(false)) => true,
                    _ => false,
                };

                if is_zero_fill {
                    // memset to zero
                    self.emit_line(&format!(
                        "call ptr @memset(ptr {}, i32 0, i64 {})",
                        data_ptr, total_bytes
                    ));
                } else {
                    // Fill loop: evaluate value once, then loop storing it
                    let is_aggregate = Self::is_aggregate_type(&elem_ty);
                    let val = if is_aggregate {
                        self.emit_expr_ptr(value_expr)?
                    } else {
                        self.emit_expr(value_expr)?
                    };

                    let loop_var = self.fresh_temp();
                    self.emit_line(&format!("{} = alloca i64", loop_var));
                    self.emit_line(&format!("store i64 0, ptr {}", loop_var));

                    let cond_label = self.fresh_label("repeat.cond");
                    let body_label = self.fresh_label("repeat.body");
                    let end_label = self.fresh_label("repeat.end");

                    self.emit_line(&format!("br label %{}", cond_label));

                    self.emit_label(&cond_label);
                    self.block_terminated = false;
                    let idx = self.emit_load("i64", &loop_var);
                    let cmp = self.fresh_temp();
                    self.emit_line(&format!("{} = icmp slt i64 {}, {}", cmp, idx, count_val));
                    self.emit_cond_branch(&cmp, &body_label, &end_label);

                    self.emit_label(&body_label);
                    self.block_terminated = false;
                    let gep = self.fresh_temp();
                    self.emit_line(&format!(
                        "{} = getelementptr {}, ptr {}, i64 {}",
                        gep, elem_ty, data_ptr, idx
                    ));
                    if is_aggregate {
                        self.emit_memcpy(&gep, &val, &elem_size.to_string());
                    } else {
                        self.emit_line(&format!("store {} {}, ptr {}", elem_ty, val, gep));
                    }
                    let next_idx = self.fresh_temp();
                    self.emit_line(&format!("{} = add i64 {}, 1", next_idx, idx));
                    self.emit_line(&format!("store i64 {}, ptr {}", next_idx, loop_var));
                    self.emit_line(&format!("br label %{}", cond_label));

                    self.emit_label(&end_label);
                    self.block_terminated = false;
                }

                // Build fat pointer with len = count, cap = count
                let fat_ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca {{ ptr, i64, i64 }}", fat_ptr));
                self.emit_fat_ptr_init(&fat_ptr, &data_ptr, &count_val, &count_val);

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

                // Load data pointer and length from fat pointer
                let (data_ptr, len_val) = self.emit_fat_ptr_load(&arr_ptr);

                let idx_val = self.emit_expr(idx_expr)?;

                // Bounds check (elide if index is a provably bounded loop var)
                let elide = if let ExprKind::Ident(idx_name) = &idx_expr.kind {
                    if let Some(bounded_arr) = self.bounded_loop_vars.get(idx_name) {
                        if let ExprKind::Ident(arr_name) = &arr_expr.kind {
                            bounded_arr == arr_name
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };
                if !elide {
                    self.emit_line(&format!(
                        "call void @__yorum_bounds_check(i64 {}, i64 {})",
                        idx_val, len_val
                    ));
                }

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
                    self.emit_memcpy(&tmp_alloca, &elem_gep, &sz.to_string());
                    Ok(tmp_alloca)
                } else {
                    let val = self.emit_load(&elem_ty, &elem_gep);
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
                // returning structs), store to a temp alloca and return the alloca pointer.
                // If the expression already returns an alloca pointer (e.g., pipeline
                // terminators like find/position/reduce), use it directly.
                let val = self.emit_expr(expr)?;
                if self.expr_returns_ptr(expr) {
                    Ok(val)
                } else {
                    let llvm_ty = self.expr_llvm_type(expr);
                    if Self::is_aggregate_type(&llvm_ty) {
                        let tmp = self.emit_alloca_store(&llvm_ty, &val);
                        Ok(tmp)
                    } else {
                        Ok(val)
                    }
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
                self.emit_cond_branch(&val, &ok_label, &fail_label);
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
                self.emit_cond_branch(&val, &ok_label, &fail_label);
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
        self.emit_struct_field_store(enum_name, &ptr, 0, "i32", &tag.to_string());

        // Store arguments into payload bytes
        if !args.is_empty() {
            let payload_gep = self.emit_struct_gep(enum_name, &ptr, 1);

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
        let tag_val = self.emit_struct_field_load(&enum_name, &enum_ptr, 0, "i32");

        // 4. Branch: tag 0 = Some/Ok (success), tag != 0 = None/Err (early return)
        let success_label = self.fresh_label("try.ok");
        let error_label = self.fresh_label("try.err");
        let cmp = self.fresh_temp();
        self.emit_line(&format!("{} = icmp eq i32 {}, 0", cmp, tag_val));
        self.emit_cond_branch(&cmp, &success_label, &error_label);

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
            self.emit_struct_field_store(&ret_enum_name, &none_ptr, 0, "i32", "1");
            let none_val = self.emit_load(&ret_ty, &none_ptr);
            self.emit_line(&format!("ret {} {}", ret_ty, none_val));
        } else {
            // Extract Err payload from inner Result, construct Err(e) for return type
            let inner_payload_gep = self.emit_struct_gep(&enum_name, &enum_ptr, 1);

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
                let err_val = self.emit_load(&err_llvm_ty, &err_field_ptr);

                // Construct Err(e) for the return type
                let ret_err_ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca %{}", ret_err_ptr, ret_enum_name));
                self.emit_struct_field_store(&ret_enum_name, &ret_err_ptr, 0, "i32", "1");
                let ret_payload_gep = self.emit_struct_gep(&ret_enum_name, &ret_err_ptr, 1);
                let ret_err_field_ptr = self.fresh_temp();
                self.emit_line(&format!(
                    "{} = getelementptr [0 x i8], ptr {}, i64 0, i64 0",
                    ret_err_field_ptr, ret_payload_gep
                ));
                self.emit_line(&format!(
                    "store {} {}, ptr {}",
                    err_llvm_ty, err_val, ret_err_field_ptr
                ));
                let ret_val = self.emit_load(&ret_ty, &ret_err_ptr);
                self.emit_line(&format!("ret {} {}", ret_ty, ret_val));
            } else {
                // Err variant with no payload (unusual but handle gracefully)
                let ret_err_ptr = self.fresh_temp();
                self.emit_line(&format!("{} = alloca %{}", ret_err_ptr, ret_enum_name));
                self.emit_struct_field_store(&ret_enum_name, &ret_err_ptr, 0, "i32", "1");
                let ret_val = self.emit_load(&ret_ty, &ret_err_ptr);
                self.emit_line(&format!("ret {} {}", ret_ty, ret_val));
            }
        }
        self.block_terminated = true;

        // 6. Success path: extract T payload from Some/Ok
        self.emit_label(&success_label);
        self.block_terminated = false;

        let ok_payload_gep = self.emit_struct_gep(&enum_name, &enum_ptr, 1);
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
            let t_val = self.emit_load(&t_llvm_ty, &ok_field_ptr);
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
            let val = self.emit_load(&cap_slot.llvm_ty, &cap_slot.ptr);
            self.emit_struct_field_store(&env_type_name, &env_ptr, i, &cap_slot.llvm_ty, &val);
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
        // Save while-elision analysis state (closure is a separate function scope)
        let saved_len_aliases = std::mem::take(&mut self.len_aliases);
        let saved_immutable_bindings = std::mem::take(&mut self.immutable_bindings);
        let saved_non_negative_vars = std::mem::take(&mut self.non_negative_vars);
        let saved_len_alias_scopes = std::mem::take(&mut self.len_alias_scopes);
        let saved_immutable_binding_scopes = std::mem::take(&mut self.immutable_binding_scopes);
        let saved_non_negative_scopes = std::mem::take(&mut self.non_negative_scopes);
        let saved_bounded_loop_vars = std::mem::take(&mut self.bounded_loop_vars);

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
            let val = self.emit_struct_field_load(env_type_name, "%env", i, &cap_slot.llvm_ty);
            let ptr = format!("%{}.cap", cap_name);
            self.emit_line(&format!("{} = alloca {}", ptr, cap_slot.llvm_ty));
            self.emit_line(&format!("store {} {}, ptr {}", cap_slot.llvm_ty, val, ptr));
            self.define_var(cap_name, &ptr, &cap_slot.llvm_ty);
        }

        // Alloca for explicit params
        for param in &closure.params {
            let ty = self.llvm_type(&param.ty);
            let ptr = self.emit_alloca_store(&ty, &format!("%{}", param.name));
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
        self.len_aliases = saved_len_aliases;
        self.immutable_bindings = saved_immutable_bindings;
        self.non_negative_vars = saved_non_negative_vars;
        self.len_alias_scopes = saved_len_alias_scopes;
        self.immutable_binding_scopes = saved_immutable_binding_scopes;
        self.non_negative_scopes = saved_non_negative_scopes;
        self.bounded_loop_vars = saved_bounded_loop_vars;

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
            let val = self.emit_load(&cap_slot.llvm_ty, &cap_slot.ptr);
            self.emit_struct_field_store(&env_type_name, &env_ptr, i, &cap_slot.llvm_ty, &val);
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
        // Save while-elision analysis state (spawn is a separate function scope)
        let saved_len_aliases = std::mem::take(&mut self.len_aliases);
        let saved_immutable_bindings = std::mem::take(&mut self.immutable_bindings);
        let saved_non_negative_vars = std::mem::take(&mut self.non_negative_vars);
        let saved_len_alias_scopes = std::mem::take(&mut self.len_alias_scopes);
        let saved_immutable_binding_scopes = std::mem::take(&mut self.immutable_binding_scopes);
        let saved_non_negative_scopes = std::mem::take(&mut self.non_negative_scopes);
        let saved_bounded_loop_vars = std::mem::take(&mut self.bounded_loop_vars);

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
            let val = self.emit_struct_field_load(env_type_name, "%env", i, &cap_slot.llvm_ty);
            let ptr = format!("%{}.cap", cap_name);
            self.emit_line(&format!("{} = alloca {}", ptr, cap_slot.llvm_ty));
            self.emit_line(&format!("store {} {}, ptr {}", cap_slot.llvm_ty, val, ptr));
            self.define_var(cap_name, &ptr, &cap_slot.llvm_ty);
        }

        // Set spawn context so emit_return stores into result slot and returns null
        self.spawn_return_ctx = Some((env_type_name.to_string(), result_field_idx));
        self.emit_block(block)?;
        self.spawn_return_ctx = None;

        if !self.block_terminated {
            // Store default result 0 and return
            self.emit_struct_field_store(env_type_name, "%env", result_field_idx, "i64", "0");
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
        self.len_aliases = saved_len_aliases;
        self.immutable_bindings = saved_immutable_bindings;
        self.non_negative_vars = saved_non_negative_vars;
        self.len_alias_scopes = saved_len_alias_scopes;
        self.immutable_binding_scopes = saved_immutable_binding_scopes;
        self.non_negative_scopes = saved_non_negative_scopes;
        self.bounded_loop_vars = saved_bounded_loop_vars;

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
        let thread = self.emit_load("i64", &thread_gep);

        // Call pthread_join
        self.emit_line(&format!("call i32 @pthread_join(i64 {}, ptr null)", thread));

        // Load env ptr from TCB field 1
        let env_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ i64, ptr }}, ptr {}, i32 0, i32 1",
            env_gep, tcb
        ));
        let env_ptr = self.emit_load("ptr", &env_gep);

        // Load the result from the env struct's result slot.
        // The receiver variable name maps to (env_type_name, result_field_idx)
        // recorded when the task was created via emit_spawn.
        if let ExprKind::Ident(ref task_name) = receiver.kind {
            if let Some((ref env_type, result_idx)) = self.task_env_info.get(task_name).cloned() {
                let result = self.emit_struct_field_load(env_type, &env_ptr, result_idx, "i64");
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
        let tag = self.emit_struct_field_load(enum_name, &ptr, 0, "i32");

        // Check tag == 0 (Some/Ok variant)
        let cmp = self.fresh_temp();
        self.emit_line(&format!("{} = icmp eq i32 {}, 0", cmp, tag));
        let ok_label = self.fresh_label("unwrap_ok");
        let fail_label = self.fresh_label("unwrap_fail");
        self.emit_cond_branch(&cmp, &ok_label, &fail_label);

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
        let val = self.emit_struct_field_load(enum_name, &ptr, 1, &payload_ty);
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
        let tag = self.emit_struct_field_load(enum_name, &ptr, 0, "i32");

        // Check tag == 1 (Err variant)
        let cmp = self.fresh_temp();
        self.emit_line(&format!("{} = icmp eq i32 {}, 1", cmp, tag));
        let ok_label = self.fresh_label("unwrap_err_ok");
        let fail_label = self.fresh_label("unwrap_err_fail");
        self.emit_cond_branch(&cmp, &ok_label, &fail_label);

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
        let val = self.emit_struct_field_load(enum_name, &ptr, 1, &payload_ty);
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
        let tag = self.emit_struct_field_load(enum_name, &ptr, 0, "i32");
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
        let fn_ptr = self.emit_load("ptr", &fn_gep);

        // Extract env_ptr
        let env_gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, ptr }}, ptr {}, i32 0, i32 1",
            env_gep, closure_ptr
        ));
        let env_ptr = self.emit_load("ptr", &env_gep);

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
            ExprKind::ArrayRepeat(val, count) => {
                Self::collect_idents_from_expr(val, locals, names);
                Self::collect_idents_from_expr(count, locals, names);
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
            ExprKind::RangeFrom(start) => {
                Self::collect_idents_from_expr(start, locals, names);
            }
            ExprKind::Try(inner) => {
                Self::collect_idents_from_expr(inner, locals, names);
            }
            ExprKind::Literal(_) => {}
        }
    }

    // ── Bounds check elision helpers ────────────────────────

    /// Check whether a loop body mutates the given array (push/pop/reassignment).
    /// Index assignment (`arr[i] = val`) does NOT count as mutation since it
    /// doesn't change length.
    pub(crate) fn body_mutates_array(body: &Block, array_name: &str) -> bool {
        for stmt in &body.stmts {
            if Self::stmt_mutates_array(stmt, array_name) {
                return true;
            }
        }
        false
    }

    fn stmt_mutates_array(stmt: &Stmt, array_name: &str) -> bool {
        match stmt {
            Stmt::Let(s) => {
                // Shadowing the array name
                if s.name == array_name {
                    return true;
                }
                Self::expr_mutates_array(&s.value, array_name)
            }
            Stmt::Assign(s) => {
                // Direct reassignment: `arr = ...`
                if let ExprKind::Ident(name) = &s.target.kind {
                    if name == array_name {
                        return true;
                    }
                }
                // Index assignment (arr[i] = val) is safe — doesn't change length
                Self::expr_mutates_array(&s.target, array_name)
                    || Self::expr_mutates_array(&s.value, array_name)
            }
            Stmt::Expr(s) => Self::expr_mutates_array(&s.expr, array_name),
            Stmt::Return(s) => Self::expr_mutates_array(&s.value, array_name),
            Stmt::If(s) => {
                Self::expr_mutates_array(&s.condition, array_name)
                    || Self::block_mutates_array(&s.then_block, array_name)
                    || s.else_branch
                        .as_ref()
                        .is_some_and(|branch| Self::else_branch_mutates_array(branch, array_name))
            }
            Stmt::While(s) => {
                Self::expr_mutates_array(&s.condition, array_name)
                    || Self::block_mutates_array(&s.body, array_name)
            }
            Stmt::For(s) => {
                Self::expr_mutates_array(&s.iterable, array_name)
                    || Self::block_mutates_array(&s.body, array_name)
            }
            Stmt::Match(s) => {
                Self::expr_mutates_array(&s.subject, array_name)
                    || s.arms
                        .iter()
                        .any(|arm| Self::block_mutates_array(&arm.body, array_name))
            }
            Stmt::Break(_) | Stmt::Continue(_) => false,
        }
    }

    fn block_mutates_array(block: &Block, array_name: &str) -> bool {
        block
            .stmts
            .iter()
            .any(|s| Self::stmt_mutates_array(s, array_name))
    }

    fn else_branch_mutates_array(branch: &ElseBranch, array_name: &str) -> bool {
        match branch {
            ElseBranch::Else(block) => Self::block_mutates_array(block, array_name),
            ElseBranch::ElseIf(if_stmt) => {
                Self::expr_mutates_array(&if_stmt.condition, array_name)
                    || Self::block_mutates_array(&if_stmt.then_block, array_name)
                    || if_stmt
                        .else_branch
                        .as_ref()
                        .is_some_and(|b| Self::else_branch_mutates_array(b, array_name))
            }
        }
    }

    fn expr_mutates_array(expr: &Expr, array_name: &str) -> bool {
        match &expr.kind {
            ExprKind::Call(callee, args) => {
                // push(arr, val) or pop(arr)
                if let ExprKind::Ident(fn_name) = &callee.kind {
                    if (fn_name == "push" || fn_name == "pop") && !args.is_empty() {
                        if let ExprKind::Ident(arg_name) = &args[0].kind {
                            if arg_name == array_name {
                                return true;
                            }
                        }
                    }
                }
                Self::expr_mutates_array(callee, array_name)
                    || args.iter().any(|a| Self::expr_mutates_array(a, array_name))
            }
            ExprKind::Binary(l, _, r) => {
                Self::expr_mutates_array(l, array_name) || Self::expr_mutates_array(r, array_name)
            }
            ExprKind::Unary(_, inner) => Self::expr_mutates_array(inner, array_name),
            ExprKind::FieldAccess(obj, _) => Self::expr_mutates_array(obj, array_name),
            ExprKind::MethodCall(recv, method, args) => {
                // .clear() on the target array mutates its length
                if method == "clear" {
                    if let ExprKind::Ident(name) = &recv.kind {
                        if name == array_name {
                            return true;
                        }
                    }
                }
                Self::expr_mutates_array(recv, array_name)
                    || args.iter().any(|a| Self::expr_mutates_array(a, array_name))
            }
            ExprKind::Index(arr, idx) => {
                Self::expr_mutates_array(arr, array_name)
                    || Self::expr_mutates_array(idx, array_name)
            }
            ExprKind::StructInit(_, fields) => fields
                .iter()
                .any(|f| Self::expr_mutates_array(&f.value, array_name)),
            ExprKind::ArrayLit(elems) | ExprKind::TupleLit(elems) => elems
                .iter()
                .any(|e| Self::expr_mutates_array(e, array_name)),
            ExprKind::ArrayRepeat(val, count) => {
                Self::expr_mutates_array(val, array_name)
                    || Self::expr_mutates_array(count, array_name)
            }
            ExprKind::Closure(_) => false, // Closure captures don't mutate in this context
            ExprKind::Spawn(block) => Self::block_mutates_array(block, array_name),
            ExprKind::Range(s, e) | ExprKind::RangeInclusive(s, e) => {
                Self::expr_mutates_array(s, array_name) || Self::expr_mutates_array(e, array_name)
            }
            ExprKind::RangeFrom(s) => Self::expr_mutates_array(s, array_name),
            ExprKind::Try(inner) => Self::expr_mutates_array(inner, array_name),
            ExprKind::Literal(_) | ExprKind::Ident(_) => false,
        }
    }

    /// Check whether a statement may mutate the length of any array
    /// (e.g. push/pop/clear), regardless of identifier name.
    ///
    /// Used to conservatively invalidate all len aliases when aliasing means
    /// we cannot map the mutation back to a single source identifier.
    fn stmt_has_length_mutation(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Let(s) => Self::expr_has_length_mutation(&s.value),
            Stmt::Assign(s) => {
                Self::expr_has_length_mutation(&s.target)
                    || Self::expr_has_length_mutation(&s.value)
            }
            Stmt::Expr(s) => Self::expr_has_length_mutation(&s.expr),
            Stmt::Return(s) => Self::expr_has_length_mutation(&s.value),
            Stmt::If(s) => {
                Self::expr_has_length_mutation(&s.condition)
                    || s.then_block
                        .stmts
                        .iter()
                        .any(Self::stmt_has_length_mutation)
                    || s.else_branch
                        .as_ref()
                        .is_some_and(|branch| Self::else_has_length_mutation(branch))
            }
            Stmt::While(s) => {
                Self::expr_has_length_mutation(&s.condition)
                    || s.body.stmts.iter().any(Self::stmt_has_length_mutation)
            }
            Stmt::For(s) => {
                Self::expr_has_length_mutation(&s.iterable)
                    || s.body.stmts.iter().any(Self::stmt_has_length_mutation)
            }
            Stmt::Match(s) => {
                Self::expr_has_length_mutation(&s.subject)
                    || s.arms
                        .iter()
                        .any(|arm| arm.body.stmts.iter().any(Self::stmt_has_length_mutation))
            }
            Stmt::Break(_) | Stmt::Continue(_) => false,
        }
    }

    fn else_has_length_mutation(branch: &ElseBranch) -> bool {
        match branch {
            ElseBranch::Else(block) => block.stmts.iter().any(Self::stmt_has_length_mutation),
            ElseBranch::ElseIf(if_stmt) => {
                Self::expr_has_length_mutation(&if_stmt.condition)
                    || if_stmt
                        .then_block
                        .stmts
                        .iter()
                        .any(Self::stmt_has_length_mutation)
                    || if_stmt
                        .else_branch
                        .as_ref()
                        .is_some_and(|branch| Self::else_has_length_mutation(branch))
            }
        }
    }

    fn expr_has_length_mutation(expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Call(callee, args) => {
                if let ExprKind::Ident(fn_name) = &callee.kind {
                    if (fn_name == "push" || fn_name == "pop") && !args.is_empty() {
                        return true;
                    }
                }
                Self::expr_has_length_mutation(callee)
                    || args.iter().any(Self::expr_has_length_mutation)
            }
            ExprKind::MethodCall(recv, method, args) => {
                if method == "clear" {
                    return true;
                }
                Self::expr_has_length_mutation(recv)
                    || args.iter().any(Self::expr_has_length_mutation)
            }
            ExprKind::Binary(lhs, _, rhs) => {
                Self::expr_has_length_mutation(lhs) || Self::expr_has_length_mutation(rhs)
            }
            ExprKind::Unary(_, inner) => Self::expr_has_length_mutation(inner),
            ExprKind::FieldAccess(obj, _) => Self::expr_has_length_mutation(obj),
            ExprKind::Index(arr, idx) => {
                Self::expr_has_length_mutation(arr) || Self::expr_has_length_mutation(idx)
            }
            ExprKind::StructInit(_, fields) => fields
                .iter()
                .any(|f| Self::expr_has_length_mutation(&f.value)),
            ExprKind::ArrayLit(elems) | ExprKind::TupleLit(elems) => {
                elems.iter().any(Self::expr_has_length_mutation)
            }
            ExprKind::ArrayRepeat(val, count) => {
                Self::expr_has_length_mutation(val) || Self::expr_has_length_mutation(count)
            }
            ExprKind::Closure(_) => false,
            ExprKind::Spawn(block) => block.stmts.iter().any(Self::stmt_has_length_mutation),
            ExprKind::Range(s, e) | ExprKind::RangeInclusive(s, e) => {
                Self::expr_has_length_mutation(s) || Self::expr_has_length_mutation(e)
            }
            ExprKind::RangeFrom(s) => Self::expr_has_length_mutation(s),
            ExprKind::Try(inner) => Self::expr_has_length_mutation(inner),
            ExprKind::Literal(_) | ExprKind::Ident(_) => false,
        }
    }

    // ── While-loop bounds check elision helpers ──────────────

    /// Check if an expression is provably non-negative (>= 0).
    /// Conservative: returns true only for patterns we can statically guarantee.
    pub(crate) fn expr_is_provably_non_negative(
        expr: &Expr,
        non_neg_vars: &HashSet<String>,
    ) -> bool {
        match &expr.kind {
            ExprKind::Literal(Literal::Int(n)) => *n >= 0,
            ExprKind::Ident(name) => non_neg_vars.contains(name),
            ExprKind::Call(callee, args) => {
                // len() always returns >= 0
                if let ExprKind::Ident(fn_name) = &callee.kind {
                    if fn_name == "len" && args.len() == 1 {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    /// Check if an assignment `name = value` preserves non-negativity.
    /// Returns true for patterns like `name = name + <non_neg>` (desugared +=).
    pub(crate) fn assign_preserves_non_negative(
        _name: &str,
        value: &Expr,
        non_neg_vars: &HashSet<String>,
    ) -> bool {
        // General: value is provably non-negative
        Self::expr_is_provably_non_negative(value, non_neg_vars)
    }

    fn is_array_binding_name(&self, name: &str) -> bool {
        self.lookup_var(name)
            .is_some_and(|slot| slot.llvm_ty == "{ ptr, i64, i64 }")
    }

    /// Detect unknown calls that receive an in-scope array variable directly.
    /// Such calls may mutate array length via aliases (e.g. helper(arr_alias)).
    fn expr_has_unknown_call_with_array_ident_arg(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Call(callee, args) => {
                let is_known_non_mutating =
                    matches!(&callee.kind, ExprKind::Ident(name) if name == "len");
                let has_array_ident_arg = args.iter().any(|arg| {
                    if let ExprKind::Ident(name) = &arg.kind {
                        self.is_array_binding_name(name)
                    } else {
                        false
                    }
                });
                (!is_known_non_mutating && has_array_ident_arg)
                    || self.expr_has_unknown_call_with_array_ident_arg(callee)
                    || args
                        .iter()
                        .any(|arg| self.expr_has_unknown_call_with_array_ident_arg(arg))
            }
            ExprKind::MethodCall(recv, _, args) => {
                self.expr_has_unknown_call_with_array_ident_arg(recv)
                    || args
                        .iter()
                        .any(|arg| self.expr_has_unknown_call_with_array_ident_arg(arg))
            }
            ExprKind::Binary(lhs, _, rhs) => {
                self.expr_has_unknown_call_with_array_ident_arg(lhs)
                    || self.expr_has_unknown_call_with_array_ident_arg(rhs)
            }
            ExprKind::Unary(_, inner) => self.expr_has_unknown_call_with_array_ident_arg(inner),
            ExprKind::FieldAccess(obj, _) => self.expr_has_unknown_call_with_array_ident_arg(obj),
            ExprKind::Index(arr, idx) => {
                self.expr_has_unknown_call_with_array_ident_arg(arr)
                    || self.expr_has_unknown_call_with_array_ident_arg(idx)
            }
            ExprKind::StructInit(_, fields) => fields
                .iter()
                .any(|field| self.expr_has_unknown_call_with_array_ident_arg(&field.value)),
            ExprKind::ArrayLit(elems) | ExprKind::TupleLit(elems) => elems
                .iter()
                .any(|elem| self.expr_has_unknown_call_with_array_ident_arg(elem)),
            ExprKind::ArrayRepeat(val, count) => {
                self.expr_has_unknown_call_with_array_ident_arg(val)
                    || self.expr_has_unknown_call_with_array_ident_arg(count)
            }
            ExprKind::Closure(_) => false,
            ExprKind::Spawn(block) => block
                .stmts
                .iter()
                .any(|stmt| self.stmt_has_indirect_array_length_mutation_risk(stmt)),
            ExprKind::Range(s, e) | ExprKind::RangeInclusive(s, e) => {
                self.expr_has_unknown_call_with_array_ident_arg(s)
                    || self.expr_has_unknown_call_with_array_ident_arg(e)
            }
            ExprKind::RangeFrom(s) => self.expr_has_unknown_call_with_array_ident_arg(s),
            ExprKind::Try(inner) => self.expr_has_unknown_call_with_array_ident_arg(inner),
            ExprKind::Literal(_) | ExprKind::Ident(_) => false,
        }
    }

    fn else_has_indirect_array_length_mutation_risk(&self, branch: &ElseBranch) -> bool {
        match branch {
            ElseBranch::Else(block) => block
                .stmts
                .iter()
                .any(|stmt| self.stmt_has_indirect_array_length_mutation_risk(stmt)),
            ElseBranch::ElseIf(if_stmt) => {
                self.expr_has_unknown_call_with_array_ident_arg(&if_stmt.condition)
                    || if_stmt
                        .then_block
                        .stmts
                        .iter()
                        .any(|stmt| self.stmt_has_indirect_array_length_mutation_risk(stmt))
                    || if_stmt.else_branch.as_ref().is_some_and(|branch| {
                        self.else_has_indirect_array_length_mutation_risk(branch)
                    })
            }
        }
    }

    /// Conservative safety check for alias-sensitive array length analysis.
    ///
    /// Returns true when a statement can invalidate facts like `n = len(arr)` or
    /// make `while i < len(arr)` elision unsafe through aliases/indirect effects.
    fn stmt_has_indirect_array_length_mutation_risk(&self, stmt: &Stmt) -> bool {
        if Self::stmt_has_length_mutation(stmt) {
            return true;
        }

        match stmt {
            Stmt::Let(s) => self.expr_has_unknown_call_with_array_ident_arg(&s.value),
            Stmt::Assign(s) => {
                if let ExprKind::Ident(name) = &s.target.kind {
                    if self.is_array_binding_name(name) {
                        return true;
                    }
                }
                self.expr_has_unknown_call_with_array_ident_arg(&s.target)
                    || self.expr_has_unknown_call_with_array_ident_arg(&s.value)
            }
            Stmt::Expr(s) => self.expr_has_unknown_call_with_array_ident_arg(&s.expr),
            Stmt::Return(s) => self.expr_has_unknown_call_with_array_ident_arg(&s.value),
            Stmt::If(s) => {
                self.expr_has_unknown_call_with_array_ident_arg(&s.condition)
                    || s.then_block
                        .stmts
                        .iter()
                        .any(|stmt| self.stmt_has_indirect_array_length_mutation_risk(stmt))
                    || s.else_branch.as_ref().is_some_and(|branch| {
                        self.else_has_indirect_array_length_mutation_risk(branch)
                    })
            }
            Stmt::While(s) => {
                self.expr_has_unknown_call_with_array_ident_arg(&s.condition)
                    || s.body
                        .stmts
                        .iter()
                        .any(|stmt| self.stmt_has_indirect_array_length_mutation_risk(stmt))
            }
            Stmt::For(s) => {
                self.expr_has_unknown_call_with_array_ident_arg(&s.iterable)
                    || s.body
                        .stmts
                        .iter()
                        .any(|stmt| self.stmt_has_indirect_array_length_mutation_risk(stmt))
            }
            Stmt::Match(s) => {
                self.expr_has_unknown_call_with_array_ident_arg(&s.subject)
                    || s.arms.iter().any(|arm| {
                        arm.body
                            .stmts
                            .iter()
                            .any(|stmt| self.stmt_has_indirect_array_length_mutation_risk(stmt))
                    })
            }
            Stmt::Break(_) | Stmt::Continue(_) => false,
        }
    }

    /// Invalidate len_aliases for any arrays mutated by a statement.
    /// Catches `push(arr, ..)`, `pop(arr)`, `arr = ..` between alias and loop.
    fn invalidate_len_aliases_for_stmt(&mut self, stmt: &Stmt) {
        // Conservative alias-aware fallback: if this statement can indirectly
        // affect array length facts, drop all len aliases.
        if self.stmt_has_indirect_array_length_mutation_risk(stmt) {
            self.len_aliases.clear();
            return;
        }

        let affected: Vec<String> = self
            .len_aliases
            .iter()
            .filter(|(_, arr)| Self::stmt_mutates_array(stmt, arr))
            .map(|(alias, _)| alias.clone())
            .collect();
        for alias in affected {
            self.len_aliases.remove(&alias);
        }
    }

    /// Detect `while var < bound` pattern where bound == len(arr).
    /// Returns Some((loop_var, array_name)) if elision is safe.
    /// Requires the loop variable to be provably non-negative (lower bound)
    /// and that body modifications preserve non-negativity.
    fn detect_bounded_while(
        &self,
        condition: &Expr,
        body: &Block,
        len_aliases: &HashMap<String, String>,
        non_negative_vars: &HashSet<String>,
    ) -> Option<(String, String)> {
        // Condition must be Binary(Ident(var), Lt, rhs)
        let (var_name, arr_name) = match &condition.kind {
            ExprKind::Binary(lhs, BinOp::Lt, rhs) => {
                let var_name = if let ExprKind::Ident(name) = &lhs.kind {
                    name.clone()
                } else {
                    return None;
                };

                let arr_name = match &rhs.kind {
                    // Direct: while var < len(arr)
                    ExprKind::Call(callee, args) => {
                        if let ExprKind::Ident(fn_name) = &callee.kind {
                            if fn_name == "len" && args.len() == 1 {
                                if let ExprKind::Ident(name) = &args[0].kind {
                                    name.clone()
                                } else {
                                    return None;
                                }
                            } else {
                                return None;
                            }
                        } else {
                            return None;
                        }
                    }
                    // Indirect: while var < n where n aliases len(arr)
                    ExprKind::Ident(bound_name) => {
                        let arr = len_aliases.get(bound_name)?;
                        if Self::body_reassigns_var(body, bound_name) {
                            return None;
                        }
                        arr.clone()
                    }
                    _ => return None,
                };
                (var_name, arr_name)
            }
            _ => return None,
        };

        // Loop variable must be provably non-negative (lower bound safety)
        if !non_negative_vars.contains(&var_name) {
            return None;
        }

        // Body modifications must preserve non-negativity across iterations
        if !Self::body_preserves_non_negative(body, &var_name, non_negative_vars) {
            return None;
        }

        // Body must not mutate array length (including alias/indirect risks)
        if body
            .stmts
            .iter()
            .any(|stmt| self.stmt_has_indirect_array_length_mutation_risk(stmt))
        {
            return None;
        }

        // Body must not directly mutate/shadow the guarded array name
        if Self::body_mutates_array(body, &arr_name) {
            return None;
        }

        // All arr[var] accesses must precede all var modifications
        if !Self::accesses_precede_modifications(body, &var_name, &arr_name) {
            return None;
        }

        Some((var_name, arr_name))
    }

    /// Check that all modifications of `var_name` in the body preserve
    /// non-negativity (e.g. `var = var + <non_neg_expr>`).
    ///
    /// Uses a "loop-stable" snapshot: variables modified anywhere in the body
    /// (other than var_name itself) are excluded from non_neg_vars, because
    /// a later reassignment (e.g. `step = -1`) would make them negative on
    /// subsequent iterations even if they started non-negative.
    fn body_preserves_non_negative(
        body: &Block,
        var_name: &str,
        non_neg_vars: &HashSet<String>,
    ) -> bool {
        // Compute stable snapshot: exclude vars modified in the body
        // (except var_name, which is the variable we're analyzing).
        let stable: HashSet<String> = non_neg_vars
            .iter()
            .filter(|name| *name == var_name || !Self::body_reassigns_var(body, name))
            .cloned()
            .collect();

        for stmt in &body.stmts {
            if !Self::stmt_preserves_non_negative(stmt, var_name, &stable) {
                return false;
            }
        }
        true
    }

    fn is_small_non_negative_literal(expr: &Expr, max: i64) -> bool {
        if let ExprKind::Literal(Literal::Int(n)) = &expr.kind {
            *n >= 0 && *n <= max
        } else {
            false
        }
    }

    /// Bounded-while specific non-negativity preservation.
    ///
    /// Allows only wrap-safe self-updates (`+0`, `+1`, `*0`, `*1`) in addition
    /// to the general non-negative assignment proof.
    fn assign_preserves_non_negative_bounded_while(
        name: &str,
        value: &Expr,
        non_neg_vars: &HashSet<String>,
    ) -> bool {
        if let ExprKind::Binary(lhs, BinOp::Add, rhs) = &value.kind {
            if let ExprKind::Ident(lhs_name) = &lhs.kind {
                if lhs_name == name && Self::is_small_non_negative_literal(rhs, 1) {
                    return true;
                }
            }
            if let ExprKind::Ident(rhs_name) = &rhs.kind {
                if rhs_name == name && Self::is_small_non_negative_literal(lhs, 1) {
                    return true;
                }
            }
        }
        if let ExprKind::Binary(lhs, BinOp::Mul, rhs) = &value.kind {
            if let ExprKind::Ident(lhs_name) = &lhs.kind {
                if lhs_name == name && Self::is_small_non_negative_literal(rhs, 1) {
                    return true;
                }
            }
            if let ExprKind::Ident(rhs_name) = &rhs.kind {
                if rhs_name == name && Self::is_small_non_negative_literal(lhs, 1) {
                    return true;
                }
            }
        }
        Self::assign_preserves_non_negative(name, value, non_neg_vars)
    }

    fn stmt_preserves_non_negative(
        stmt: &Stmt,
        var_name: &str,
        non_neg_vars: &HashSet<String>,
    ) -> bool {
        match stmt {
            Stmt::Assign(s) => {
                if let ExprKind::Ident(name) = &s.target.kind {
                    if name == var_name {
                        return Self::assign_preserves_non_negative_bounded_while(
                            var_name,
                            &s.value,
                            non_neg_vars,
                        );
                    }
                }
                true
            }
            Stmt::Let(s) => {
                // Shadowing the loop var breaks our tracking
                if s.name == var_name {
                    return false;
                }
                true
            }
            Stmt::If(s) => {
                Self::body_preserves_non_negative(&s.then_block, var_name, non_neg_vars)
                    && s.else_branch.as_ref().is_none_or(|b| {
                        Self::else_preserves_non_negative(b, var_name, non_neg_vars)
                    })
            }
            Stmt::While(s) => Self::body_preserves_non_negative(&s.body, var_name, non_neg_vars),
            Stmt::For(s) => {
                if s.var_name == var_name {
                    return false;
                }
                Self::body_preserves_non_negative(&s.body, var_name, non_neg_vars)
            }
            Stmt::Match(s) => s
                .arms
                .iter()
                .all(|arm| Self::body_preserves_non_negative(&arm.body, var_name, non_neg_vars)),
            Stmt::Expr(_) | Stmt::Return(_) | Stmt::Break(_) | Stmt::Continue(_) => true,
        }
    }

    fn else_preserves_non_negative(
        branch: &ElseBranch,
        var_name: &str,
        non_neg_vars: &HashSet<String>,
    ) -> bool {
        match branch {
            ElseBranch::Else(block) => {
                Self::body_preserves_non_negative(block, var_name, non_neg_vars)
            }
            ElseBranch::ElseIf(if_stmt) => {
                Self::body_preserves_non_negative(&if_stmt.then_block, var_name, non_neg_vars)
                    && if_stmt.else_branch.as_ref().is_none_or(|b| {
                        Self::else_preserves_non_negative(b, var_name, non_neg_vars)
                    })
            }
        }
    }

    /// Check if body reassigns a variable (direct assignment or shadowing let).
    fn body_reassigns_var(body: &Block, var_name: &str) -> bool {
        body.stmts
            .iter()
            .any(|s| Self::stmt_reassigns_var(s, var_name))
    }

    fn stmt_reassigns_var(stmt: &Stmt, var_name: &str) -> bool {
        match stmt {
            Stmt::Let(s) => {
                if s.name == var_name {
                    return true;
                }
                false
            }
            Stmt::Assign(s) => {
                if let ExprKind::Ident(name) = &s.target.kind {
                    if name == var_name {
                        return true;
                    }
                }
                false
            }
            Stmt::If(s) => {
                Self::body_reassigns_var(&s.then_block, var_name)
                    || s.else_branch
                        .as_ref()
                        .is_some_and(|b| Self::else_branch_reassigns_var(b, var_name))
            }
            Stmt::While(s) => Self::body_reassigns_var(&s.body, var_name),
            Stmt::For(s) => Self::body_reassigns_var(&s.body, var_name),
            Stmt::Match(s) => s
                .arms
                .iter()
                .any(|arm| Self::body_reassigns_var(&arm.body, var_name)),
            Stmt::Expr(_) | Stmt::Return(_) | Stmt::Break(_) | Stmt::Continue(_) => false,
        }
    }

    fn else_branch_reassigns_var(branch: &ElseBranch, var_name: &str) -> bool {
        match branch {
            ElseBranch::Else(block) => Self::body_reassigns_var(block, var_name),
            ElseBranch::ElseIf(if_stmt) => {
                Self::body_reassigns_var(&if_stmt.then_block, var_name)
                    || if_stmt
                        .else_branch
                        .as_ref()
                        .is_some_and(|b| Self::else_branch_reassigns_var(b, var_name))
            }
        }
    }

    /// Verify that all arr[var] accesses precede all var modifications in the body.
    /// This ensures the loop condition `var < bound` still holds at access time.
    fn accesses_precede_modifications(body: &Block, var_name: &str, arr_name: &str) -> bool {
        let mut var_modified = false;
        for stmt in &body.stmts {
            let accesses = Self::stmt_accesses_arr_with_var(stmt, arr_name, var_name);
            let modifies = Self::stmt_modifies_var(stmt, var_name);
            // If a single statement both accesses and modifies, be conservative
            if accesses && modifies {
                return false;
            }
            // If var was already modified and we access arr[var], unsafe
            if var_modified && accesses {
                return false;
            }
            if modifies {
                var_modified = true;
            }
        }
        true
    }

    /// Check if a statement contains an `arr[var]` index access pattern.
    fn stmt_accesses_arr_with_var(stmt: &Stmt, arr_name: &str, var_name: &str) -> bool {
        match stmt {
            Stmt::Let(s) => Self::expr_accesses_arr_with_var(&s.value, arr_name, var_name),
            Stmt::Assign(s) => {
                Self::expr_accesses_arr_with_var(&s.target, arr_name, var_name)
                    || Self::expr_accesses_arr_with_var(&s.value, arr_name, var_name)
            }
            Stmt::Expr(s) => Self::expr_accesses_arr_with_var(&s.expr, arr_name, var_name),
            Stmt::Return(s) => Self::expr_accesses_arr_with_var(&s.value, arr_name, var_name),
            Stmt::If(s) => {
                Self::expr_accesses_arr_with_var(&s.condition, arr_name, var_name)
                    || s.then_block
                        .stmts
                        .iter()
                        .any(|st| Self::stmt_accesses_arr_with_var(st, arr_name, var_name))
                    || s.else_branch.as_ref().is_some_and(|b| {
                        Self::else_branch_accesses_arr_with_var(b, arr_name, var_name)
                    })
            }
            Stmt::While(s) => {
                Self::expr_accesses_arr_with_var(&s.condition, arr_name, var_name)
                    || s.body
                        .stmts
                        .iter()
                        .any(|st| Self::stmt_accesses_arr_with_var(st, arr_name, var_name))
            }
            Stmt::For(s) => {
                Self::expr_accesses_arr_with_var(&s.iterable, arr_name, var_name)
                    || s.body
                        .stmts
                        .iter()
                        .any(|st| Self::stmt_accesses_arr_with_var(st, arr_name, var_name))
            }
            Stmt::Match(s) => {
                Self::expr_accesses_arr_with_var(&s.subject, arr_name, var_name)
                    || s.arms.iter().any(|arm| {
                        arm.body
                            .stmts
                            .iter()
                            .any(|st| Self::stmt_accesses_arr_with_var(st, arr_name, var_name))
                    })
            }
            Stmt::Break(_) | Stmt::Continue(_) => false,
        }
    }

    fn else_branch_accesses_arr_with_var(
        branch: &ElseBranch,
        arr_name: &str,
        var_name: &str,
    ) -> bool {
        match branch {
            ElseBranch::Else(block) => block
                .stmts
                .iter()
                .any(|st| Self::stmt_accesses_arr_with_var(st, arr_name, var_name)),
            ElseBranch::ElseIf(if_stmt) => {
                Self::expr_accesses_arr_with_var(&if_stmt.condition, arr_name, var_name)
                    || if_stmt
                        .then_block
                        .stmts
                        .iter()
                        .any(|st| Self::stmt_accesses_arr_with_var(st, arr_name, var_name))
                    || if_stmt.else_branch.as_ref().is_some_and(|b| {
                        Self::else_branch_accesses_arr_with_var(b, arr_name, var_name)
                    })
            }
        }
    }

    fn expr_accesses_arr_with_var(expr: &Expr, arr_name: &str, var_name: &str) -> bool {
        match &expr.kind {
            ExprKind::Index(arr_expr, idx_expr) => {
                // Check if this is arr[var]
                let is_target = matches!(&arr_expr.kind, ExprKind::Ident(a) if a == arr_name)
                    && matches!(&idx_expr.kind, ExprKind::Ident(v) if v == var_name);
                if is_target {
                    return true;
                }
                Self::expr_accesses_arr_with_var(arr_expr, arr_name, var_name)
                    || Self::expr_accesses_arr_with_var(idx_expr, arr_name, var_name)
            }
            ExprKind::Binary(l, _, r) => {
                Self::expr_accesses_arr_with_var(l, arr_name, var_name)
                    || Self::expr_accesses_arr_with_var(r, arr_name, var_name)
            }
            ExprKind::Unary(_, inner) => {
                Self::expr_accesses_arr_with_var(inner, arr_name, var_name)
            }
            ExprKind::Call(callee, args) => {
                Self::expr_accesses_arr_with_var(callee, arr_name, var_name)
                    || args
                        .iter()
                        .any(|a| Self::expr_accesses_arr_with_var(a, arr_name, var_name))
            }
            ExprKind::FieldAccess(obj, _) => {
                Self::expr_accesses_arr_with_var(obj, arr_name, var_name)
            }
            ExprKind::MethodCall(recv, _, args) => {
                Self::expr_accesses_arr_with_var(recv, arr_name, var_name)
                    || args
                        .iter()
                        .any(|a| Self::expr_accesses_arr_with_var(a, arr_name, var_name))
            }
            ExprKind::StructInit(_, fields) => fields
                .iter()
                .any(|f| Self::expr_accesses_arr_with_var(&f.value, arr_name, var_name)),
            ExprKind::ArrayLit(elems) | ExprKind::TupleLit(elems) => elems
                .iter()
                .any(|e| Self::expr_accesses_arr_with_var(e, arr_name, var_name)),
            ExprKind::ArrayRepeat(val, count) => {
                Self::expr_accesses_arr_with_var(val, arr_name, var_name)
                    || Self::expr_accesses_arr_with_var(count, arr_name, var_name)
            }
            ExprKind::Range(s, e) | ExprKind::RangeInclusive(s, e) => {
                Self::expr_accesses_arr_with_var(s, arr_name, var_name)
                    || Self::expr_accesses_arr_with_var(e, arr_name, var_name)
            }
            ExprKind::RangeFrom(s) => Self::expr_accesses_arr_with_var(s, arr_name, var_name),
            ExprKind::Try(inner) => Self::expr_accesses_arr_with_var(inner, arr_name, var_name),
            ExprKind::Spawn(block) => block
                .stmts
                .iter()
                .any(|st| Self::stmt_accesses_arr_with_var(st, arr_name, var_name)),
            ExprKind::Closure(_) | ExprKind::Literal(_) | ExprKind::Ident(_) => false,
        }
    }

    /// Check if a statement modifies the given variable (assignment or shadowing let).
    fn stmt_modifies_var(stmt: &Stmt, var_name: &str) -> bool {
        match stmt {
            Stmt::Let(s) => {
                if s.name == var_name {
                    return true;
                }
                false
            }
            Stmt::Assign(s) => {
                if let ExprKind::Ident(name) = &s.target.kind {
                    if name == var_name {
                        return true;
                    }
                }
                // Recurse into nested blocks in the value expression
                Self::expr_contains_var_modification(&s.value, var_name)
            }
            Stmt::If(s) => {
                Self::body_reassigns_var(&s.then_block, var_name)
                    || s.else_branch
                        .as_ref()
                        .is_some_and(|b| Self::else_branch_reassigns_var(b, var_name))
            }
            Stmt::While(s) => Self::body_reassigns_var(&s.body, var_name),
            Stmt::For(s) => s.var_name == var_name || Self::body_reassigns_var(&s.body, var_name),
            Stmt::Match(s) => s
                .arms
                .iter()
                .any(|arm| Self::body_reassigns_var(&arm.body, var_name)),
            Stmt::Expr(s) => Self::expr_contains_var_modification(&s.expr, var_name),
            Stmt::Return(_) | Stmt::Break(_) | Stmt::Continue(_) => false,
        }
    }

    /// Check if an expression contains statements that modify a variable
    /// (e.g. closures or spawn blocks with assignments).
    fn expr_contains_var_modification(expr: &Expr, var_name: &str) -> bool {
        match &expr.kind {
            ExprKind::Spawn(block) => Self::body_reassigns_var(block, var_name),
            ExprKind::Closure(c) => Self::body_reassigns_var(&c.body, var_name),
            _ => false,
        }
    }

    // ── Variable scope management ────────────────────────────

    fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
        self.len_alias_scopes.push(self.len_aliases.clone());
        self.immutable_binding_scopes
            .push(self.immutable_bindings.clone());
        self.non_negative_scopes
            .push(self.non_negative_vars.clone());
    }

    fn pop_scope(&mut self) {
        self.vars.pop();
        if let Some(saved) = self.len_alias_scopes.pop() {
            self.len_aliases = saved;
        }
        if let Some(saved) = self.immutable_binding_scopes.pop() {
            self.immutable_bindings = saved;
        }
        if let Some(saved) = self.non_negative_scopes.pop() {
            // Use intersection: a name is non-negative only if it survived
            // BOTH the inner scope (wasn't invalidated by e.g. `i = -1`)
            // AND was in the outer scope (isn't an inner-only `let` binding).
            // This preserves invalidations from inner assignments to outer vars.
            self.non_negative_vars = self
                .non_negative_vars
                .intersection(&saved)
                .cloned()
                .collect();
        }
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

    /// Ensure an Option enum layout exists for the given LLVM element type.
    /// Called by pipeline terminators (position, find, reduce) that construct
    /// Option values at the codegen level, since the monomorphizer may not have
    /// created the declaration if the result is never bound to a typed variable.
    pub(crate) fn ensure_option_enum_for_llvm_type(&mut self, llvm_ty: &str) {
        let option_name = self.yorum_type_to_option_name(llvm_ty);
        if !self.enum_layouts.contains_key(&option_name) {
            let inner_type = match llvm_ty {
                "i64" => Type::Int,
                "double" => Type::Float,
                "i1" => Type::Bool,
                "i8" => Type::Char,
                "ptr" => Type::Str,
                _ => Type::Int, // fallback
            };
            self.enum_layouts.insert(
                option_name.clone(),
                EnumLayout {
                    name: option_name,
                    variants: vec![
                        ("Some".to_string(), vec![inner_type]),
                        ("None".to_string(), vec![]),
                    ],
                    tag_count: 2,
                },
            );
        }
    }

    /// GEP into a named struct field `%StructName`. Returns the GEP pointer temp.
    fn emit_struct_gep(&mut self, struct_name: &str, ptr: &str, field_idx: usize) -> String {
        let gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr %{}, ptr {}, i32 0, i32 {}",
            gep, struct_name, ptr, field_idx
        ));
        gep
    }

    /// GEP + load from a named struct field. Returns the loaded SSA value.
    fn emit_struct_field_load(
        &mut self,
        struct_name: &str,
        ptr: &str,
        field_idx: usize,
        field_ty: &str,
    ) -> String {
        let gep = self.emit_struct_gep(struct_name, ptr, field_idx);
        let val = self.fresh_temp();
        self.emit_line(&format!("{} = load {}, ptr {}", val, field_ty, gep));
        val
    }

    /// GEP + store into a named struct field. Returns the GEP temp.
    fn emit_struct_field_store(
        &mut self,
        struct_name: &str,
        ptr: &str,
        field_idx: usize,
        field_ty: &str,
        val: &str,
    ) -> String {
        let gep = self.emit_struct_gep(struct_name, ptr, field_idx);
        self.emit_line(&format!("store {} {}, ptr {}", field_ty, val, gep));
        gep
    }

    /// Load a single field from an array fat pointer `{ ptr, i64, i64 }`.
    /// field: 0=data(ptr), 1=len(i64), 2=cap(i64). Returns the loaded SSA value.
    fn emit_fat_ptr_field_load(&mut self, ptr: &str, field: u32, ty: &str) -> String {
        let gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 {}",
            gep, ptr, field
        ));
        let val = self.fresh_temp();
        self.emit_line(&format!("{} = load {}, ptr {}", val, ty, gep));
        val
    }

    /// Store a value to a single field of an array fat pointer `{ ptr, i64, i64 }`.
    /// Returns the GEP temp (useful if the caller needs to re-store later).
    fn emit_fat_ptr_field_store(&mut self, ptr: &str, field: u32, ty: &str, val: &str) -> String {
        let gep = self.fresh_temp();
        self.emit_line(&format!(
            "{} = getelementptr {{ ptr, i64, i64 }}, ptr {}, i32 0, i32 {}",
            gep, ptr, field
        ));
        self.emit_line(&format!("store {} {}, ptr {}", ty, val, gep));
        gep
    }

    /// Load data pointer and length from an array fat pointer. Returns (data_ptr, len_val).
    fn emit_fat_ptr_load(&mut self, ptr: &str) -> (String, String) {
        let data = self.emit_fat_ptr_field_load(ptr, 0, "ptr");
        let len = self.emit_fat_ptr_field_load(ptr, 1, "i64");
        (data, len)
    }

    /// Store data, length, and capacity into a pre-allocated fat pointer.
    fn emit_fat_ptr_init(&mut self, fat_ptr: &str, data: &str, len: &str, cap: &str) {
        self.emit_fat_ptr_field_store(fat_ptr, 0, "ptr", data);
        self.emit_fat_ptr_field_store(fat_ptr, 1, "i64", len);
        self.emit_fat_ptr_field_store(fat_ptr, 2, "i64", cap);
    }

    /// `load TY, ptr PTR` — returns the loaded SSA value.
    fn emit_load(&mut self, ty: &str, ptr: &str) -> String {
        let val = self.fresh_temp();
        self.emit_line(&format!("{} = load {}, ptr {}", val, ty, ptr));
        val
    }

    /// `alloca TY` + `store TY VAL, ptr ALLOCA` — returns the alloca pointer.
    fn emit_alloca_store(&mut self, ty: &str, val: &str) -> String {
        let ptr = self.fresh_temp();
        self.emit_line(&format!("{} = alloca {}", ptr, ty));
        self.emit_line(&format!("store {} {}, ptr {}", ty, val, ptr));
        ptr
    }

    /// `call ptr @memcpy(ptr DST, ptr SRC, i64 SIZE)`
    fn emit_memcpy(&mut self, dst: &str, src: &str, size: &str) {
        self.emit_line(&format!(
            "call ptr @memcpy(ptr {}, ptr {}, i64 {})",
            dst, src, size
        ));
    }

    /// `br i1 COND, label %TRUE_LBL, label %FALSE_LBL`
    fn emit_cond_branch(&mut self, cond: &str, true_lbl: &str, false_lbl: &str) {
        self.emit_line(&format!(
            "br i1 {}, label %{}, label %{}",
            cond, true_lbl, false_lbl
        ));
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
