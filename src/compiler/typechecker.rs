use crate::compiler::ast::*;
use crate::compiler::span::Span;
use std::collections::{HashMap, HashSet};
use std::fmt;

// ═══════════════════════════════════════════════════════════════
//  Error type
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type error at {}: {}", self.span, self.message)
    }
}

impl std::error::Error for TypeError {}

// ═══════════════════════════════════════════════════════════════
//  Symbol table (for LSP hover / go-to-definition)
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub definitions: Vec<SymbolDef>,
    pub references: Vec<SymbolRef>,
}

#[derive(Debug, Clone)]
pub struct SymbolDef {
    pub name: String,
    pub kind: SymbolKind,
    pub type_desc: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SymbolRef {
    pub span: Span,
    pub resolved_type: String,
    pub def_name: String,
    pub def_span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Function,
    Variable,
    Parameter,
    Struct,
    Enum,
    #[allow(dead_code)]
    Field,
}

// ═══════════════════════════════════════════════════════════════
//  Type environment
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
struct FnSig {
    params: Vec<Type>,
    ret: Type,
    #[allow(dead_code)]
    is_pure: bool,
    type_params: Vec<String>,
    span: Span,
    /// None = unchecked (no effects clause), Some(vec![]) = no effects, Some(vec!["io"]) = io only
    effects: Option<Vec<String>>,
}

const VALID_EFFECTS: &[&str] = &["io", "fs", "net", "time", "env", "concurrency"];

/// Returns the effect categories for a builtin function name.
/// Returns empty vec for pure/memory-only builtins (no external effects).
/// Returns None for unknown names (not a builtin).
fn builtin_effects(name: &str) -> Option<Vec<&'static str>> {
    match name {
        // io
        "print_int" | "print_float" | "print_bool" | "print_str" | "print_char" | "print_err"
        | "read_line" | "print_flush" => Some(vec!["io"]),
        // fs
        "file_read" | "file_write" | "file_exists" | "file_append" => Some(vec!["fs"]),
        // net
        "tcp_connect" | "tcp_listen" | "tcp_accept" | "tcp_send" | "tcp_recv" | "tcp_close"
        | "udp_socket" | "udp_bind" | "udp_send_to" | "udp_recv_from" | "dns_resolve"
        | "http_request" | "http_get" | "http_post" => Some(vec!["net"]),
        // time
        "time_ms" => Some(vec!["time"]),
        // env
        "env_get" | "exit" | "args" => Some(vec!["env"]),
        // concurrency
        "chan" | "send" | "recv" | "spawn" => Some(vec!["concurrency"]),
        // Pure / memory-only builtins — no external effects
        "str_len" | "str_concat" | "str_eq" | "str_charAt" | "str_sub" | "str_from_char"
        | "char_to_int" | "int_to_char" | "int_to_float" | "float_to_int" | "int_to_str"
        | "str_to_int" | "char_is_alpha" | "char_is_digit" | "char_is_whitespace"
        | "float_to_str" | "bool_to_str" | "to_str" | "abs_int" | "abs_float" | "min_int"
        | "max_int" | "min_float" | "max_float" | "sqrt" | "pow" | "sin" | "cos" | "tan"
        | "floor" | "ceil" | "round" | "log" | "log10" | "exp" | "str_contains"
        | "str_index_of" | "str_starts_with" | "str_ends_with" | "str_trim" | "str_replace"
        | "str_to_upper" | "str_to_lower" | "str_repeat" | "str_split" | "contains_int"
        | "contains_str" | "sort_int" | "sort_str" | "len" | "push" | "pop" | "slice"
        | "concat_arrays" | "reverse" | "map_new" | "map_set" | "map_get" | "map_has"
        | "map_size" | "map_remove" | "map_keys" | "map_values" | "set_new" | "set_add"
        | "set_has" | "set_remove" | "set_size" | "set_values" => Some(vec![]),
        _ => None,
    }
}

#[derive(Debug, Clone)]
struct StructInfo {
    fields: Vec<(String, Type)>,
    type_params: Vec<String>,
}

#[derive(Debug, Clone)]
struct EnumInfo {
    variants: Vec<(String, Vec<Type>)>,
    type_params: Vec<String>,
}

#[derive(Debug, Clone)]
struct VarInfo {
    ty: Type,
    is_mut: bool,
    def_span: Span,
}

#[derive(Debug, Clone)]
struct MethodInfo {
    mangled_name: String,
    sig: FnSig,
    #[allow(dead_code)]
    self_param_ty: Type,
}

#[derive(Debug, Clone)]
struct TraitInfo {
    methods: Vec<(String, FnSig)>,
}

// ═══════════════════════════════════════════════════════════════
//  Type checker
// ═══════════════════════════════════════════════════════════════

pub struct TypeChecker {
    scopes: Vec<HashMap<String, VarInfo>>,
    functions: HashMap<String, FnSig>,
    structs: HashMap<String, StructInfo>,
    enums: HashMap<String, EnumInfo>,
    methods: HashMap<String, Vec<MethodInfo>>,
    traits: HashMap<String, TraitInfo>,
    trait_impls: HashMap<(String, String), bool>,
    type_param_scope: Vec<HashSet<String>>,
    current_self_type: Option<Type>,
    current_fn_ret: Option<Type>,
    current_fn_is_pure: bool,
    current_fn_effects: Option<Vec<String>>,
    inferred_effects: HashMap<String, Vec<String>>,
    loop_depth: usize,
    errors: Vec<TypeError>,
    collect_symbols: bool,
    symbol_table: SymbolTable,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut checker = Self {
            scopes: Vec::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            methods: HashMap::new(),
            traits: HashMap::new(),
            trait_impls: HashMap::new(),
            type_param_scope: Vec::new(),
            current_self_type: None,
            current_fn_ret: None,
            current_fn_is_pure: false,
            current_fn_effects: None,
            inferred_effects: HashMap::new(),
            loop_depth: 0,
            errors: Vec::new(),
            collect_symbols: false,
            symbol_table: SymbolTable {
                definitions: Vec::new(),
                references: Vec::new(),
            },
        };
        checker.register_builtins();
        checker
    }

    pub fn new_with_symbols() -> Self {
        let mut checker = Self::new();
        checker.collect_symbols = true;
        checker
    }

    pub fn take_symbol_table(&mut self) -> SymbolTable {
        std::mem::replace(
            &mut self.symbol_table,
            SymbolTable {
                definitions: Vec::new(),
                references: Vec::new(),
            },
        )
    }

    /// Return a list of (name, signature_string) for all builtin functions.
    pub fn builtin_names() -> Vec<(String, String)> {
        let checker = TypeChecker::new();
        checker
            .functions
            .iter()
            .map(|(name, sig)| {
                let params = sig
                    .params
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(", ");
                (
                    name.clone(),
                    format!("fn {}({}) -> {}", name, params, sig.ret),
                )
            })
            .collect()
    }

    fn register_builtins(&mut self) {
        let builtin =
            |params: Vec<Type>, ret: Type, is_pure: bool, effects: Option<Vec<String>>| FnSig {
                params,
                ret,
                is_pure,
                type_params: Vec::new(),
                span: Span::synthetic(),
                effects,
            };
        let eff_io = || Some(vec!["io".to_string()]);
        let eff_fs = || Some(vec!["fs".to_string()]);
        let eff_net = || Some(vec!["net".to_string()]);
        let eff_time = || Some(vec!["time".to_string()]);
        let eff_env = || Some(vec!["env".to_string()]);
        let eff_none = || Some(vec![]);

        // I/O builtins
        self.functions.insert(
            "print_int".to_string(),
            builtin(vec![Type::Int], Type::Unit, false, eff_io()),
        );
        self.functions.insert(
            "print_float".to_string(),
            builtin(vec![Type::Float], Type::Unit, false, eff_io()),
        );
        self.functions.insert(
            "print_bool".to_string(),
            builtin(vec![Type::Bool], Type::Unit, false, eff_io()),
        );
        self.functions.insert(
            "print_str".to_string(),
            builtin(vec![Type::Str], Type::Unit, false, eff_io()),
        );
        self.functions.insert(
            "print_char".to_string(),
            builtin(vec![Type::Char], Type::Unit, false, eff_io()),
        );
        self.functions.insert(
            "print_err".to_string(),
            builtin(vec![Type::Str], Type::Unit, false, eff_io()),
        );
        self.functions.insert(
            "read_line".to_string(),
            builtin(vec![], Type::Str, false, eff_io()),
        );
        self.functions.insert(
            "print_flush".to_string(),
            builtin(vec![Type::Str], Type::Unit, false, eff_io()),
        );
        // Pure string/char builtins
        self.functions.insert(
            "str_len".to_string(),
            builtin(vec![Type::Str], Type::Int, true, eff_none()),
        );
        self.functions.insert(
            "str_concat".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Str, false, eff_none()),
        );
        self.functions.insert(
            "str_eq".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, true, eff_none()),
        );
        // Type casting builtins
        self.functions.insert(
            "char_to_int".to_string(),
            builtin(vec![Type::Char], Type::Int, true, eff_none()),
        );
        self.functions.insert(
            "int_to_char".to_string(),
            builtin(vec![Type::Int], Type::Char, true, eff_none()),
        );
        self.functions.insert(
            "int_to_float".to_string(),
            builtin(vec![Type::Int], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "float_to_int".to_string(),
            builtin(vec![Type::Float], Type::Int, true, eff_none()),
        );
        self.functions.insert(
            "int_to_str".to_string(),
            builtin(vec![Type::Int], Type::Str, true, eff_none()),
        );
        self.functions.insert(
            "str_to_int".to_string(),
            builtin(vec![Type::Str], Type::Int, true, eff_none()),
        );
        // String/char operation builtins
        self.functions.insert(
            "str_charAt".to_string(),
            builtin(vec![Type::Str, Type::Int], Type::Char, true, eff_none()),
        );
        self.functions.insert(
            "str_sub".to_string(),
            builtin(
                vec![Type::Str, Type::Int, Type::Int],
                Type::Str,
                false,
                eff_none(),
            ),
        );
        self.functions.insert(
            "str_from_char".to_string(),
            builtin(vec![Type::Char], Type::Str, false, eff_none()),
        );
        self.functions.insert(
            "char_is_alpha".to_string(),
            builtin(vec![Type::Char], Type::Bool, true, eff_none()),
        );
        self.functions.insert(
            "char_is_digit".to_string(),
            builtin(vec![Type::Char], Type::Bool, true, eff_none()),
        );
        self.functions.insert(
            "char_is_whitespace".to_string(),
            builtin(vec![Type::Char], Type::Bool, true, eff_none()),
        );
        // File I/O builtins
        self.functions.insert(
            "file_read".to_string(),
            builtin(vec![Type::Str], Type::Str, false, eff_fs()),
        );
        self.functions.insert(
            "file_write".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, false, eff_fs()),
        );
        self.functions.insert(
            "file_exists".to_string(),
            builtin(vec![Type::Str], Type::Bool, false, eff_fs()),
        );
        self.functions.insert(
            "file_append".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, false, eff_fs()),
        );
        // Process interaction builtins
        self.functions.insert(
            "exit".to_string(),
            builtin(vec![Type::Int], Type::Unit, false, eff_env()),
        );
        self.functions.insert(
            "env_get".to_string(),
            builtin(vec![Type::Str], Type::Str, false, eff_env()),
        );
        // Time
        self.functions.insert(
            "time_ms".to_string(),
            builtin(vec![], Type::Int, false, eff_time()),
        );
        // Map and Set are now generic — their builtins are special-cased in infer_expr
        // Math builtins (pure, no effects)
        self.functions.insert(
            "abs_int".to_string(),
            builtin(vec![Type::Int], Type::Int, true, eff_none()),
        );
        self.functions.insert(
            "abs_float".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "min_int".to_string(),
            builtin(vec![Type::Int, Type::Int], Type::Int, true, eff_none()),
        );
        self.functions.insert(
            "max_int".to_string(),
            builtin(vec![Type::Int, Type::Int], Type::Int, true, eff_none()),
        );
        self.functions.insert(
            "min_float".to_string(),
            builtin(
                vec![Type::Float, Type::Float],
                Type::Float,
                true,
                eff_none(),
            ),
        );
        self.functions.insert(
            "max_float".to_string(),
            builtin(
                vec![Type::Float, Type::Float],
                Type::Float,
                true,
                eff_none(),
            ),
        );
        self.functions.insert(
            "sqrt".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "pow".to_string(),
            builtin(
                vec![Type::Float, Type::Float],
                Type::Float,
                true,
                eff_none(),
            ),
        );
        self.functions.insert(
            "sin".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "cos".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "tan".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "floor".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "ceil".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "round".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "log".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "log10".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        self.functions.insert(
            "exp".to_string(),
            builtin(vec![Type::Float], Type::Float, true, eff_none()),
        );
        // String utility builtins (no effects — memory-only)
        self.functions.insert(
            "str_contains".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, true, eff_none()),
        );
        self.functions.insert(
            "str_index_of".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Int, true, eff_none()),
        );
        self.functions.insert(
            "str_starts_with".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, true, eff_none()),
        );
        self.functions.insert(
            "str_ends_with".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, true, eff_none()),
        );
        self.functions.insert(
            "str_trim".to_string(),
            builtin(vec![Type::Str], Type::Str, false, eff_none()),
        );
        self.functions.insert(
            "str_replace".to_string(),
            builtin(
                vec![Type::Str, Type::Str, Type::Str],
                Type::Str,
                false,
                eff_none(),
            ),
        );
        self.functions.insert(
            "str_to_upper".to_string(),
            builtin(vec![Type::Str], Type::Str, false, eff_none()),
        );
        self.functions.insert(
            "str_to_lower".to_string(),
            builtin(vec![Type::Str], Type::Str, false, eff_none()),
        );
        self.functions.insert(
            "str_repeat".to_string(),
            builtin(vec![Type::Str, Type::Int], Type::Str, false, eff_none()),
        );
        // Collection utility builtins (no effects — memory-only)
        self.functions.insert(
            "contains_int".to_string(),
            builtin(
                vec![Type::Array(Box::new(Type::Int)), Type::Int],
                Type::Bool,
                true,
                eff_none(),
            ),
        );
        self.functions.insert(
            "contains_str".to_string(),
            builtin(
                vec![Type::Array(Box::new(Type::Str)), Type::Str],
                Type::Bool,
                true,
                eff_none(),
            ),
        );
        self.functions.insert(
            "sort_int".to_string(),
            builtin(
                vec![Type::Array(Box::new(Type::Int))],
                Type::Array(Box::new(Type::Int)),
                false,
                eff_none(),
            ),
        );
        self.functions.insert(
            "sort_str".to_string(),
            builtin(
                vec![Type::Array(Box::new(Type::Str))],
                Type::Array(Box::new(Type::Str)),
                false,
                eff_none(),
            ),
        );
        // (map_size, map_remove, map_keys, map_values are now special-cased in infer_expr)
        // Networking — TCP
        self.functions.insert(
            "tcp_connect".to_string(),
            builtin(vec![Type::Str, Type::Int], Type::Int, false, eff_net()),
        );
        self.functions.insert(
            "tcp_listen".to_string(),
            builtin(vec![Type::Str, Type::Int], Type::Int, false, eff_net()),
        );
        self.functions.insert(
            "tcp_accept".to_string(),
            builtin(vec![Type::Int], Type::Int, false, eff_net()),
        );
        self.functions.insert(
            "tcp_send".to_string(),
            builtin(vec![Type::Int, Type::Str], Type::Int, false, eff_net()),
        );
        self.functions.insert(
            "tcp_recv".to_string(),
            builtin(vec![Type::Int, Type::Int], Type::Str, false, eff_net()),
        );
        self.functions.insert(
            "tcp_close".to_string(),
            builtin(vec![Type::Int], Type::Unit, false, eff_net()),
        );
        // Networking — UDP
        self.functions.insert(
            "udp_socket".to_string(),
            builtin(vec![], Type::Int, false, eff_net()),
        );
        self.functions.insert(
            "udp_bind".to_string(),
            builtin(
                vec![Type::Int, Type::Str, Type::Int],
                Type::Int,
                false,
                eff_net(),
            ),
        );
        self.functions.insert(
            "udp_send_to".to_string(),
            builtin(
                vec![Type::Int, Type::Str, Type::Str, Type::Int],
                Type::Int,
                false,
                eff_net(),
            ),
        );
        self.functions.insert(
            "udp_recv_from".to_string(),
            builtin(vec![Type::Int, Type::Int], Type::Str, false, eff_net()),
        );
        // Networking — DNS
        self.functions.insert(
            "dns_resolve".to_string(),
            builtin(vec![Type::Str], Type::Str, false, eff_net()),
        );
        // Networking — HTTP
        self.functions.insert(
            "http_request".to_string(),
            builtin(
                vec![Type::Str, Type::Str, Type::Str, Type::Str],
                Type::Str,
                false,
                eff_net(),
            ),
        );
        self.functions.insert(
            "http_get".to_string(),
            builtin(vec![Type::Str], Type::Str, false, eff_net()),
        );
        self.functions.insert(
            "http_post".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Str, false, eff_net()),
        );
        // String conversion builtins (for string interpolation)
        self.functions.insert(
            "float_to_str".to_string(),
            builtin(vec![Type::Float], Type::Str, true, eff_none()),
        );
        self.functions.insert(
            "bool_to_str".to_string(),
            builtin(vec![Type::Bool], Type::Str, true, eff_none()),
        );

        // Register prelude generic enums: Option<T> and Result<T, E>
        self.enums.insert(
            "Option".to_string(),
            EnumInfo {
                variants: vec![
                    ("Some".to_string(), vec![Type::TypeVar("T".to_string())]),
                    ("None".to_string(), vec![]),
                ],
                type_params: vec!["T".to_string()],
            },
        );
        self.enums.insert(
            "Result".to_string(),
            EnumInfo {
                variants: vec![
                    ("Ok".to_string(), vec![Type::TypeVar("T".to_string())]),
                    ("Err".to_string(), vec![Type::TypeVar("E".to_string())]),
                ],
                type_params: vec!["T".to_string(), "E".to_string()],
            },
        );
    }

    /// Recursive type compatibility check, treating TypeVar as wildcard.
    fn types_compatible(a: &Type, b: &Type) -> bool {
        if a == b {
            return true;
        }
        match (a, b) {
            (Type::TypeVar(_), _) | (_, Type::TypeVar(_)) => true,
            (Type::Array(inner_a), Type::Array(inner_b)) => {
                Self::types_compatible(inner_a, inner_b)
            }
            (Type::Generic(name1, args1), Type::Generic(name2, args2)) => {
                name1 == name2
                    && args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(x, y)| Self::types_compatible(x, y))
            }
            (Type::Generic(name, _), Type::Named(vname))
            | (Type::Named(vname), Type::Generic(name, _)) => name == vname,
            (Type::Tuple(elems_a), Type::Tuple(elems_b)) => {
                elems_a.len() == elems_b.len()
                    && elems_a
                        .iter()
                        .zip(elems_b.iter())
                        .all(|(x, y)| Self::types_compatible(x, y))
            }
            _ => false,
        }
    }

    /// Type-check an entire program. Returns Ok(()) or collected errors.
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<TypeError>> {
        // Pass 1: register all top-level declarations
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) => self.register_function(f),
                Declaration::Struct(s) => self.register_struct(s),
                Declaration::Enum(e) => self.register_enum(e),
                Declaration::Impl(i) => self.register_impl(i),
                Declaration::Trait(t) => self.register_trait(t),
                Declaration::Const(_) => {} // handled in pass 2
            }
        }

        // Pass 1.5: infer effects for functions without explicit `effects` clause
        self.infer_all_effects(program);

        // Pass 2: check function bodies, const values, and impl method bodies
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) => self.check_function(f),
                Declaration::Const(c) => self.check_const(c),
                Declaration::Impl(i) => {
                    self.current_self_type = Some(Type::Named(i.target_type.clone()));
                    for method in &i.methods {
                        self.check_function(method);
                    }
                    self.current_self_type = None;
                }
                _ => {}
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    // ── Registration ─────────────────────────────────────────

    fn extract_effects(contracts: &[Contract]) -> Option<Vec<String>> {
        for c in contracts {
            if let Contract::Effects(effs) = c {
                return Some(effs.clone());
            }
        }
        None
    }

    fn register_function(&mut self, f: &FnDecl) {
        let effects = Self::extract_effects(&f.contracts);
        let sig = FnSig {
            params: f.params.iter().map(|p| p.ty.clone()).collect(),
            ret: f.return_type.clone(),
            is_pure: f.is_pure,
            type_params: f.type_params.iter().map(|tp| tp.name.clone()).collect(),
            span: f.span,
            effects,
        };
        if self.collect_symbols {
            let params_desc: Vec<String> = f
                .params
                .iter()
                .map(|p| format!("{}: {}", p.name, p.ty))
                .collect();
            self.symbol_table.definitions.push(SymbolDef {
                name: f.name.clone(),
                kind: SymbolKind::Function,
                type_desc: format!(
                    "fn {}({}) -> {}",
                    f.name,
                    params_desc.join(", "),
                    f.return_type
                ),
                span: f.span,
            });
        }
        if let Some(existing) = self.functions.get(&f.name) {
            // Allow shadowing builtins (synthetic span), but reject user-defined duplicates
            if existing.span.line != 0 {
                self.errors.push(TypeError {
                    message: format!("function '{}' is already defined", f.name),
                    span: f.span,
                });
                return;
            }
        }
        self.functions.insert(f.name.clone(), sig);
    }

    fn register_struct(&mut self, s: &StructDecl) {
        let info = StructInfo {
            fields: s
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect(),
            type_params: s.type_params.iter().map(|tp| tp.name.clone()).collect(),
        };
        if self.collect_symbols {
            let fields_desc: Vec<String> = s
                .fields
                .iter()
                .map(|f| format!("{}: {}", f.name, f.ty))
                .collect();
            self.symbol_table.definitions.push(SymbolDef {
                name: s.name.clone(),
                kind: SymbolKind::Struct,
                type_desc: format!("struct {} {{ {} }}", s.name, fields_desc.join(", ")),
                span: s.span,
            });
        }
        self.structs.insert(s.name.clone(), info);
    }

    fn register_trait(&mut self, t: &TraitDecl) {
        let methods: Vec<(String, FnSig)> = t
            .methods
            .iter()
            .map(|m| {
                let sig = FnSig {
                    params: m.params.iter().map(|p| p.ty.clone()).collect(),
                    ret: m.return_type.clone(),
                    is_pure: false,
                    type_params: Vec::new(),
                    span: m.span,
                    effects: None,
                };
                (m.name.clone(), sig)
            })
            .collect();
        self.traits.insert(t.name.clone(), TraitInfo { methods });
    }

    fn register_impl(&mut self, i: &ImplDecl) {
        // Resolve Self to the target type for sig registration
        let self_ty = Type::Named(i.target_type.clone());

        for method in &i.methods {
            let mangled = format!("{}_{}", i.target_type, method.name);
            let effects = Self::extract_effects(&method.contracts);
            let sig = FnSig {
                params: method
                    .params
                    .iter()
                    .map(|p| self.resolve_self_type(&p.ty, &self_ty))
                    .collect(),
                ret: self.resolve_self_type(&method.return_type, &self_ty),
                is_pure: method.is_pure,
                type_params: method
                    .type_params
                    .iter()
                    .map(|tp| tp.name.clone())
                    .collect(),
                span: method.span,
                effects,
            };
            let self_param_ty = if let Some(p) = method.params.first() {
                self.resolve_self_type(&p.ty, &self_ty)
            } else {
                self_ty.clone()
            };
            let info = MethodInfo {
                mangled_name: mangled.clone(),
                sig: sig.clone(),
                self_param_ty,
            };
            self.methods
                .entry(i.target_type.clone())
                .or_default()
                .push(info);
            self.functions.insert(mangled, sig);
        }

        // If this is a trait impl, verify all required methods are present
        if let Some(trait_name) = &i.trait_name {
            if let Some(trait_info) = self.traits.get(trait_name).cloned() {
                let impl_method_names: Vec<&str> =
                    i.methods.iter().map(|m| m.name.as_str()).collect();
                for (required_name, _) in &trait_info.methods {
                    if !impl_method_names.contains(&required_name.as_str()) {
                        self.errors.push(TypeError {
                            message: format!(
                                "impl of '{}' for '{}' is missing method '{}'",
                                trait_name, i.target_type, required_name
                            ),
                            span: i.span,
                        });
                    }
                }
            }
            self.trait_impls
                .insert((trait_name.clone(), i.target_type.clone()), true);
        }
    }

    fn resolve_self_type(&self, ty: &Type, concrete: &Type) -> Type {
        match ty {
            Type::SelfType => concrete.clone(),
            Type::Ref(inner) => Type::Ref(Box::new(self.resolve_self_type(inner, concrete))),
            Type::MutRef(inner) => Type::MutRef(Box::new(self.resolve_self_type(inner, concrete))),
            Type::Own(inner) => Type::Own(Box::new(self.resolve_self_type(inner, concrete))),
            Type::Array(inner) => Type::Array(Box::new(self.resolve_self_type(inner, concrete))),
            Type::Fn(params, ret) => Type::Fn(
                params
                    .iter()
                    .map(|p| self.resolve_self_type(p, concrete))
                    .collect(),
                Box::new(self.resolve_self_type(ret, concrete)),
            ),
            Type::Task(inner) => Type::Task(Box::new(self.resolve_self_type(inner, concrete))),
            Type::Chan(inner) => Type::Chan(Box::new(self.resolve_self_type(inner, concrete))),
            other => other.clone(),
        }
    }

    fn register_enum(&mut self, e: &EnumDecl) {
        let info = EnumInfo {
            variants: e
                .variants
                .iter()
                .map(|v| (v.name.clone(), v.fields.clone()))
                .collect(),
            type_params: e.type_params.iter().map(|tp| tp.name.clone()).collect(),
        };
        if self.collect_symbols {
            let variants_desc: Vec<String> = e
                .variants
                .iter()
                .map(|v| {
                    if v.fields.is_empty() {
                        v.name.clone()
                    } else {
                        let fields: Vec<String> =
                            v.fields.iter().map(|f| format!("{}", f)).collect();
                        format!("{}({})", v.name, fields.join(", "))
                    }
                })
                .collect();
            self.symbol_table.definitions.push(SymbolDef {
                name: e.name.clone(),
                kind: SymbolKind::Enum,
                type_desc: format!("enum {} {{ {} }}", e.name, variants_desc.join(", ")),
                span: e.span,
            });
        }
        self.enums.insert(e.name.clone(), info);
    }

    // ── Checking ─────────────────────────────────────────────

    fn check_function(&mut self, f: &FnDecl) {
        // Push type params scope for generic functions
        if !f.type_params.is_empty() {
            let tps: HashSet<String> = f.type_params.iter().map(|tp| tp.name.clone()).collect();
            self.type_param_scope.push(tps);
        }

        self.current_fn_ret = Some(f.return_type.clone());
        self.current_fn_is_pure = f.is_pure;

        // Set up effect tracking for this function
        let effects = Self::extract_effects(&f.contracts);

        // Validate effect names (always, even on main/pure)
        if let Some(ref effs) = effects {
            for eff in effs {
                if !VALID_EFFECTS.contains(&eff.as_str()) {
                    self.errors.push(TypeError {
                        message: format!(
                            "unknown effect '{}'; valid effects are: {}",
                            eff,
                            VALID_EFFECTS.join(", ")
                        ),
                        span: f.span,
                    });
                }
            }
        }

        if f.is_pure {
            // pure fn: effects clause not allowed (pure is stricter)
            if effects.is_some() {
                self.errors.push(TypeError {
                    message: "pure function cannot have an 'effects' clause".to_string(),
                    span: f.span,
                });
            }
            self.current_fn_effects = None; // purity system handles pure fns
        } else if f.name == "main" {
            self.current_fn_effects = None; // main is always unchecked
        } else {
            self.current_fn_effects = effects;
        }

        self.push_scope();

        // Bind parameters
        for param in &f.params {
            if !self.is_valid_type(&param.ty) {
                self.errors.push(TypeError {
                    message: format!("unknown type '{}'", param.ty),
                    span: param.span,
                });
            }
            if self.collect_symbols {
                self.symbol_table.definitions.push(SymbolDef {
                    name: param.name.clone(),
                    kind: SymbolKind::Parameter,
                    type_desc: format!("{}: {}", param.name, param.ty),
                    span: param.span,
                });
            }
            self.define_with_span(&param.name, param.ty.clone(), false, param.span);
        }

        // Bind "result" for ensures clauses
        self.define("result", f.return_type.clone(), false);

        // Type-check contract expressions
        for contract in &f.contracts {
            match contract {
                Contract::Requires(expr) | Contract::Ensures(expr) => {
                    if let Some(ty) = self.infer_expr(expr) {
                        if ty != Type::Bool {
                            let kind = if matches!(contract, Contract::Requires(_)) {
                                "requires"
                            } else {
                                "ensures"
                            };
                            self.errors.push(TypeError {
                                message: format!("{} clause must be 'bool', found '{}'", kind, ty),
                                span: expr.span,
                            });
                        }
                    }
                }
                Contract::Effects(_) => {} // already handled above
            }
        }

        self.check_block(&f.body);
        self.pop_scope();
        self.current_fn_ret = None;
        self.current_fn_is_pure = false;
        self.current_fn_effects = None;

        if !f.type_params.is_empty() {
            self.type_param_scope.pop();
        }
    }

    fn check_const(&mut self, c: &ConstDecl) {
        self.push_scope();
        let ty = self.infer_expr(&c.value);
        if let Some(ty) = ty {
            if ty != c.ty {
                self.errors.push(TypeError {
                    message: format!(
                        "const '{}' declared as '{}' but initialized with '{}'",
                        c.name, c.ty, ty
                    ),
                    span: c.span,
                });
            }
        }
        self.pop_scope();
    }

    fn check_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(s) => {
                if !self.is_valid_type(&s.ty) {
                    self.errors.push(TypeError {
                        message: format!("unknown type '{}'", s.ty),
                        span: s.span,
                    });
                }
                // Validate hashable key constraint for Map<K, V> and Set<T>
                if let Type::Generic(ref name, ref args) = s.ty {
                    if name == "Map" && args.len() == 2 && !self.is_hashable_key(&args[0]) {
                        self.errors.push(TypeError {
                            message: format!(
                                "Map key type '{}' is not hashable; only int, string, char, bool are allowed",
                                args[0]
                            ),
                            span: s.span,
                        });
                    }
                    if name == "Set" && args.len() == 1 && !self.is_hashable_key(&args[0]) {
                        self.errors.push(TypeError {
                            message: format!(
                                "Set element type '{}' is not hashable; only int, string, char, bool are allowed",
                                args[0]
                            ),
                            span: s.span,
                        });
                    }
                }
                if let Some(val_ty) = self.infer_expr_with_expectation(&s.value, Some(&s.ty)) {
                    if !Self::types_compatible(&s.ty, &val_ty) {
                        self.errors.push(TypeError {
                            message: format!(
                                "cannot assign '{}' to variable of type '{}'",
                                val_ty, s.ty
                            ),
                            span: s.span,
                        });
                    }
                }

                // Handle tuple destructuring: let (a, b): (int, string) = expr;
                if let Some(names) = &s.destructure {
                    if let Type::Tuple(ref elem_types) = s.ty {
                        if names.len() != elem_types.len() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "destructuring pattern has {} names but tuple has {} elements",
                                    names.len(),
                                    elem_types.len()
                                ),
                                span: s.span,
                            });
                        } else {
                            for (name, ty) in names.iter().zip(elem_types.iter()) {
                                if self.collect_symbols {
                                    self.symbol_table.definitions.push(SymbolDef {
                                        name: name.clone(),
                                        kind: SymbolKind::Variable,
                                        type_desc: format!(
                                            "{}{}: {}",
                                            if s.is_mut { "let mut " } else { "let " },
                                            name,
                                            ty
                                        ),
                                        span: s.span,
                                    });
                                }
                                self.define_with_span(name, ty.clone(), s.is_mut, s.span);
                            }
                        }
                    } else {
                        self.errors.push(TypeError {
                            message: format!("cannot destructure non-tuple type '{}'", s.ty),
                            span: s.span,
                        });
                    }
                    return;
                }

                if self.collect_symbols {
                    self.symbol_table.definitions.push(SymbolDef {
                        name: s.name.clone(),
                        kind: SymbolKind::Variable,
                        type_desc: format!(
                            "{}{}: {}",
                            if s.is_mut { "let mut " } else { "let " },
                            s.name,
                            s.ty
                        ),
                        span: s.span,
                    });
                }
                self.define_with_span(&s.name, s.ty.clone(), s.is_mut, s.span);
            }

            Stmt::Assign(s) => {
                // Check that target is mutable
                if let ExprKind::Ident(name) = &s.target.kind {
                    if let Some(info) = self.lookup(name) {
                        if !info.is_mut {
                            self.errors.push(TypeError {
                                message: format!("cannot assign to immutable variable '{}'", name),
                                span: s.span,
                            });
                        }
                    }
                }
                // Check mutability for index assignment: arr[i] = val
                if let ExprKind::Index(arr_expr, _) = &s.target.kind {
                    if let ExprKind::Ident(name) = &arr_expr.kind {
                        if let Some(info) = self.lookup(name) {
                            if !info.is_mut {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "cannot assign to index of immutable variable '{}'",
                                        name
                                    ),
                                    span: s.span,
                                });
                            }
                        }
                    }
                }

                let target_ty = self.infer_expr(&s.target);
                let value_ty = self.infer_expr_with_expectation(&s.value, target_ty.as_ref());
                if let (Some(tt), Some(vt)) = (target_ty, value_ty) {
                    if !Self::types_compatible(&tt, &vt) {
                        self.errors.push(TypeError {
                            message: format!("cannot assign '{}' to '{}'", vt, tt),
                            span: s.span,
                        });
                    }
                }
            }

            Stmt::Return(s) => {
                if let Some(expected) = &self.current_fn_ret {
                    let expected = expected.clone();
                    if let Some(actual) =
                        self.infer_expr_with_expectation(&s.value, Some(&expected))
                    {
                        if !Self::types_compatible(&expected, &actual) {
                            self.errors.push(TypeError {
                                message: format!(
                                    "return type mismatch: expected '{}', found '{}'",
                                    expected, actual
                                ),
                                span: s.span,
                            });
                        }
                    }
                }
            }

            Stmt::If(s) => {
                if let Some(cond_ty) = self.infer_expr(&s.condition) {
                    if cond_ty != Type::Bool {
                        self.errors.push(TypeError {
                            message: format!("if condition must be 'bool', found '{}'", cond_ty),
                            span: s.condition.span,
                        });
                    }
                }
                self.push_scope();
                self.check_block(&s.then_block);
                self.pop_scope();

                if let Some(else_branch) = &s.else_branch {
                    match else_branch.as_ref() {
                        ElseBranch::ElseIf(elif) => {
                            self.check_stmt(&Stmt::If(elif.clone()));
                        }
                        ElseBranch::Else(block) => {
                            self.push_scope();
                            self.check_block(block);
                            self.pop_scope();
                        }
                    }
                }
            }

            Stmt::While(s) => {
                if let Some(cond_ty) = self.infer_expr(&s.condition) {
                    if cond_ty != Type::Bool {
                        self.errors.push(TypeError {
                            message: format!("while condition must be 'bool', found '{}'", cond_ty),
                            span: s.condition.span,
                        });
                    }
                }
                self.loop_depth += 1;
                self.push_scope();
                self.check_block(&s.body);
                self.pop_scope();
                self.loop_depth -= 1;
            }

            Stmt::For(s) => {
                if let ExprKind::Range(ref start, ref end)
                | ExprKind::RangeInclusive(ref start, ref end) = s.iterable.kind
                {
                    // Range-based for: check bounds are int, loop var is int
                    if let Some(start_ty) = self.infer_expr(start) {
                        if start_ty != Type::Int {
                            self.errors.push(TypeError {
                                message: format!("range start must be 'int', found '{}'", start_ty),
                                span: start.span,
                            });
                        }
                    }
                    if let Some(end_ty) = self.infer_expr(end) {
                        if end_ty != Type::Int {
                            self.errors.push(TypeError {
                                message: format!("range end must be 'int', found '{}'", end_ty),
                                span: end.span,
                            });
                        }
                    }
                    self.loop_depth += 1;
                    self.push_scope();
                    self.define_with_span(&s.var_name, Type::Int, false, s.span);
                    if self.collect_symbols {
                        self.symbol_table.definitions.push(SymbolDef {
                            name: s.var_name.clone(),
                            kind: SymbolKind::Variable,
                            type_desc: format!("for {}: int", s.var_name),
                            span: s.span,
                        });
                    }
                    self.check_block(&s.body);
                    self.pop_scope();
                    self.loop_depth -= 1;
                } else if let Some(elem_ty) = self.infer_for_iterable(&s.iterable) {
                    self.loop_depth += 1;
                    self.push_scope();
                    if self.collect_symbols {
                        self.symbol_table.definitions.push(SymbolDef {
                            name: s.var_name.clone(),
                            kind: SymbolKind::Variable,
                            type_desc: format!("for {}: {}", s.var_name, elem_ty),
                            span: s.span,
                        });
                    }
                    self.define_with_span(&s.var_name, elem_ty, false, s.span);
                    self.check_block(&s.body);
                    self.pop_scope();
                    self.loop_depth -= 1;
                }
            }

            Stmt::Match(s) => {
                let subject_ty = self.infer_expr(&s.subject);
                for arm in &s.arms {
                    self.push_scope();
                    self.check_pattern(&arm.pattern, &subject_ty);
                    self.check_block(&arm.body);
                    self.pop_scope();
                }
                self.check_match_exhaustiveness(&s.arms, &subject_ty, s.span);
            }

            Stmt::Expr(s) => {
                self.infer_expr(&s.expr);
            }

            Stmt::Break(s) => {
                if self.loop_depth == 0 {
                    self.errors.push(TypeError {
                        message: "'break' can only be used inside a loop".to_string(),
                        span: s.span,
                    });
                }
            }

            Stmt::Continue(s) => {
                if self.loop_depth == 0 {
                    self.errors.push(TypeError {
                        message: "'continue' can only be used inside a loop".to_string(),
                        span: s.span,
                    });
                }
            }
        }
    }

    /// Extract the enum name and type arg substitution map from a subject type.
    /// For `Type::Named("Foo")` returns `Some(("Foo", {}))`.
    /// For `Type::Generic("Option", [Int])` returns `Some(("Option", {T -> Int}))`.
    fn subject_enum_info(
        &self,
        subject_ty: &Option<Type>,
    ) -> Option<(String, HashMap<String, Type>)> {
        match subject_ty {
            Some(Type::Named(ref name)) => Some((name.clone(), HashMap::new())),
            Some(Type::Generic(ref name, ref args)) => {
                if let Some(info) = self.enums.get(name) {
                    let mut subst = HashMap::new();
                    for (i, tp) in info.type_params.iter().enumerate() {
                        if let Some(arg) = args.get(i) {
                            subst.insert(tp.clone(), arg.clone());
                        }
                    }
                    Some((name.clone(), subst))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Substitute type variables in a type using a substitution map.
    fn subst_type(ty: &Type, subst: &HashMap<String, Type>) -> Type {
        match ty {
            Type::TypeVar(name) => subst.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::Array(inner) => Type::Array(Box::new(Self::subst_type(inner, subst))),
            Type::Tuple(elems) => {
                Type::Tuple(elems.iter().map(|t| Self::subst_type(t, subst)).collect())
            }
            Type::Generic(name, args) => Type::Generic(
                name.clone(),
                args.iter().map(|t| Self::subst_type(t, subst)).collect(),
            ),
            other => other.clone(),
        }
    }

    fn check_pattern(&mut self, pattern: &Pattern, subject_ty: &Option<Type>) {
        match pattern {
            Pattern::Wildcard(_) => {}
            Pattern::Binding(name, _) => {
                // If the subject is an enum and the binding name matches a variant, don't bind
                let enum_info = self.subject_enum_info(subject_ty);
                if let Some((ref enum_name, _)) = enum_info {
                    if let Some(info) = self.enums.get(enum_name) {
                        if info.variants.iter().any(|(vn, _)| vn == name) {
                            return; // It's a no-data variant match, not a binding
                        }
                    }
                }
                // Bind with the subject type if known, otherwise default to Int
                let ty = subject_ty.clone().unwrap_or(Type::Int);
                self.define(name, ty, false);
            }
            Pattern::Literal(_, _) => {}
            Pattern::Variant(name, sub_patterns, span) => {
                let enum_info = self.subject_enum_info(subject_ty);
                let enums_snapshot = self.enums.clone();

                // Build prioritized variant list with substituted types:
                // 1. Subject enum (with type var substitution)
                // 2. Non-generic enums
                // 3. Generic enums (raw, unsubstituted - fallback)
                let mut ordered_variants: Vec<(String, Vec<Type>)> = Vec::new();
                if let Some((ref enum_name, ref subst)) = enum_info {
                    if let Some(info) = enums_snapshot.get(enum_name) {
                        for (vname, vfields) in &info.variants {
                            let substituted: Vec<Type> =
                                vfields.iter().map(|t| Self::subst_type(t, subst)).collect();
                            ordered_variants.push((vname.clone(), substituted));
                        }
                    }
                }
                for (ename, info) in &enums_snapshot {
                    if enum_info.as_ref().map(|(n, _)| n) == Some(ename) {
                        continue; // already added
                    }
                    if info.type_params.is_empty() {
                        for v in &info.variants {
                            ordered_variants.push(v.clone());
                        }
                    }
                }
                for (ename, info) in &enums_snapshot {
                    if enum_info.as_ref().map(|(n, _)| n) == Some(ename) {
                        continue; // already added as substituted
                    }
                    if !info.type_params.is_empty() {
                        for v in &info.variants {
                            ordered_variants.push(v.clone());
                        }
                    }
                }

                let mut found = false;
                for (vname, vfields) in &ordered_variants {
                    if vname == name {
                        found = true;
                        if sub_patterns.len() != vfields.len() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "variant '{}' expects {} fields, found {}",
                                    name,
                                    vfields.len(),
                                    sub_patterns.len()
                                ),
                                span: *span,
                            });
                        }
                        for (i, sp) in sub_patterns.iter().enumerate() {
                            if let Pattern::Binding(bname, _) = sp {
                                if let Some(ty) = vfields.get(i) {
                                    self.define(bname, ty.clone(), false);
                                }
                            }
                        }
                        break; // Use first match (prioritized)
                    }
                }
                if !found {
                    self.errors.push(TypeError {
                        message: format!("unknown variant '{}'", name),
                        span: *span,
                    });
                }
            }
        }
    }

    fn check_match_exhaustiveness(
        &mut self,
        arms: &[MatchArm],
        subject_ty: &Option<Type>,
        span: Span,
    ) {
        // Only check exhaustiveness for enum types
        let enum_info = self.subject_enum_info(subject_ty);
        let (enum_name, _) = match enum_info {
            Some(info) => info,
            None => return,
        };
        let all_variants: Vec<String> = match self.enums.get(&enum_name) {
            Some(info) => info.variants.iter().map(|(name, _)| name.clone()).collect(),
            None => return,
        };

        // Check if any arm is a catch-all (wildcard or binding that isn't a variant name)
        for arm in arms {
            match &arm.pattern {
                Pattern::Wildcard(_) => return,
                Pattern::Binding(name, _) => {
                    if !all_variants.contains(name) {
                        return; // It's a catch-all binding
                    }
                }
                _ => {}
            }
        }

        // Collect covered variant names
        let mut covered: HashSet<String> = HashSet::new();
        for arm in arms {
            match &arm.pattern {
                Pattern::Variant(name, _, _) => {
                    covered.insert(name.clone());
                }
                Pattern::Binding(name, _) => {
                    // Data-less variant matched as binding
                    if all_variants.contains(name) {
                        covered.insert(name.clone());
                    }
                }
                _ => {}
            }
        }

        let missing: Vec<&String> = all_variants
            .iter()
            .filter(|v| !covered.contains(*v))
            .collect();
        if !missing.is_empty() {
            let names: Vec<&str> = missing.iter().map(|s| s.as_str()).collect();
            self.errors.push(TypeError {
                message: format!(
                    "non-exhaustive match: missing variant(s) {}",
                    names.join(", ")
                ),
                span,
            });
        }
    }

    /// Check if the expression is an iterator pipeline: a `.map()` or `.filter()` chain
    /// that ultimately reaches `.iter()` as the base. Returns false for bare `.iter()`
    /// calls (no combinators) and for struct methods named `map`/`filter` that are not
    /// part of an `.iter()` pipeline — those fall through to normal method resolution.
    fn is_iter_pipeline(expr: &Expr) -> bool {
        if let ExprKind::MethodCall(ref receiver, ref method, _) = expr.kind {
            match method.as_str() {
                "map" | "filter" | "enumerate" | "zip" | "take" | "skip" | "chain"
                | "take_while" | "rev" => Self::has_iter_base(receiver),
                _ => false,
            }
        } else {
            false
        }
    }

    /// Walk a MethodCall chain looking for `.iter()` at the base.
    fn has_iter_base(expr: &Expr) -> bool {
        if let ExprKind::MethodCall(ref receiver, ref method, _) = expr.kind {
            match method.as_str() {
                "iter" | "chars" => true,
                "map" | "filter" | "enumerate" | "zip" | "take" | "skip" | "chain"
                | "take_while" | "rev" => Self::has_iter_base(receiver),
                _ => false,
            }
        } else {
            false
        }
    }

    fn is_pipeline_terminator(method: &str) -> bool {
        matches!(
            method,
            "reduce" | "fold" | "collect" | "find" | "any" | "all" | "sum" | "count" | "position"
        )
    }

    /// Infer the return type of a pipeline terminator expression.
    /// `receiver` is the pipeline chain (e.g., `arr.iter().map(f)`), `method` is the
    /// terminator name, `args` are the terminator arguments.
    fn infer_pipeline_terminator_type(
        &mut self,
        receiver: &Expr,
        method: &str,
        args: &[Expr],
        span: Span,
    ) -> Option<Type> {
        let elem_ty = self.infer_pipeline_elem_type(receiver)?;
        match method {
            "collect" => {
                if !args.is_empty() {
                    self.errors.push(TypeError {
                        message: format!("collect() takes no arguments, found {}", args.len()),
                        span,
                    });
                    return None;
                }
                Some(Type::Array(Box::new(elem_ty)))
            }
            "any" | "all" => {
                if args.len() != 1 {
                    self.errors.push(TypeError {
                        message: format!(
                            "{}() takes exactly 1 argument, found {}",
                            method,
                            args.len()
                        ),
                        span,
                    });
                    return None;
                }
                if !matches!(args[0].kind, ExprKind::Closure(_)) {
                    self.errors.push(TypeError {
                        message: format!("{}() requires an inline closure", method),
                        span: args[0].span,
                    });
                    return None;
                }
                let closure_ty = self.infer_expr(&args[0])?;
                if let Type::Fn(params, ret) = closure_ty {
                    if params.len() != 1 {
                        self.errors.push(TypeError {
                            message: format!(
                                "{}() closure must take exactly 1 parameter, found {}",
                                method,
                                params.len()
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    if params[0] != elem_ty {
                        self.errors.push(TypeError {
                            message: format!(
                                "{}() closure parameter type '{}' does not match element type '{}'",
                                method, params[0], elem_ty
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    if *ret != Type::Bool {
                        self.errors.push(TypeError {
                            message: format!(
                                "{}() closure must return bool, found '{}'",
                                method, ret
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    return Some(Type::Bool);
                }
                None
            }
            "find" => {
                if args.len() != 1 {
                    self.errors.push(TypeError {
                        message: format!("find() takes exactly 1 argument, found {}", args.len()),
                        span,
                    });
                    return None;
                }
                if !matches!(args[0].kind, ExprKind::Closure(_)) {
                    self.errors.push(TypeError {
                        message: "find() requires an inline closure".to_string(),
                        span: args[0].span,
                    });
                    return None;
                }
                let closure_ty = self.infer_expr(&args[0])?;
                if let Type::Fn(params, ret) = closure_ty {
                    if params.len() != 1 {
                        self.errors.push(TypeError {
                            message: format!(
                                "find() closure must take exactly 1 parameter, found {}",
                                params.len()
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    if params[0] != elem_ty {
                        self.errors.push(TypeError {
                            message: format!(
                                "find() closure parameter type '{}' does not match element type '{}'",
                                params[0], elem_ty
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    if *ret != Type::Bool {
                        self.errors.push(TypeError {
                            message: format!("find() closure must return bool, found '{}'", ret),
                            span: args[0].span,
                        });
                        return None;
                    }
                    return Some(Type::Generic("Option".to_string(), vec![elem_ty]));
                }
                None
            }
            "reduce" => {
                if args.len() != 1 {
                    self.errors.push(TypeError {
                        message: format!("reduce() takes exactly 1 argument, found {}", args.len()),
                        span,
                    });
                    return None;
                }
                if !matches!(args[0].kind, ExprKind::Closure(_)) {
                    self.errors.push(TypeError {
                        message: "reduce() requires an inline closure".to_string(),
                        span: args[0].span,
                    });
                    return None;
                }
                let closure_ty = self.infer_expr(&args[0])?;
                if let Type::Fn(params, ret) = closure_ty {
                    if params.len() != 2 {
                        self.errors.push(TypeError {
                            message: format!(
                                "reduce() closure must take exactly 2 parameters, found {}",
                                params.len()
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    if params[0] != elem_ty || params[1] != elem_ty {
                        self.errors.push(TypeError {
                            message: format!(
                                "reduce() closure parameters must both match element type '{}', found '({}, {})'",
                                elem_ty, params[0], params[1]
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    if *ret != elem_ty {
                        self.errors.push(TypeError {
                            message: format!(
                                "reduce() closure must return '{}', found '{}'",
                                elem_ty, ret
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    return Some(Type::Generic("Option".to_string(), vec![elem_ty]));
                }
                None
            }
            "fold" => {
                if args.len() != 2 {
                    self.errors.push(TypeError {
                        message: format!("fold() takes exactly 2 arguments, found {}", args.len()),
                        span,
                    });
                    return None;
                }
                let init_ty = self.infer_expr(&args[0])?;
                if !matches!(args[1].kind, ExprKind::Closure(_)) {
                    self.errors.push(TypeError {
                        message: "fold() requires an inline closure as second argument".to_string(),
                        span: args[1].span,
                    });
                    return None;
                }
                let closure_ty = self.infer_expr(&args[1])?;
                if let Type::Fn(params, ret) = closure_ty {
                    if params.len() != 2 {
                        self.errors.push(TypeError {
                            message: format!(
                                "fold() closure must take exactly 2 parameters, found {}",
                                params.len()
                            ),
                            span: args[1].span,
                        });
                        return None;
                    }
                    if params[0] != init_ty {
                        self.errors.push(TypeError {
                            message: format!(
                                "fold() closure first parameter type '{}' does not match init type '{}'",
                                params[0], init_ty
                            ),
                            span: args[1].span,
                        });
                        return None;
                    }
                    if params[1] != elem_ty {
                        self.errors.push(TypeError {
                            message: format!(
                                "fold() closure second parameter type '{}' does not match element type '{}'",
                                params[1], elem_ty
                            ),
                            span: args[1].span,
                        });
                        return None;
                    }
                    if *ret != init_ty {
                        self.errors.push(TypeError {
                            message: format!(
                                "fold() closure must return '{}', found '{}'",
                                init_ty, ret
                            ),
                            span: args[1].span,
                        });
                        return None;
                    }
                    return Some(init_ty);
                }
                None
            }
            "sum" => {
                if !args.is_empty() {
                    self.errors.push(TypeError {
                        message: format!("sum() takes no arguments, found {}", args.len()),
                        span,
                    });
                    return None;
                }
                if elem_ty != Type::Int && elem_ty != Type::Float {
                    self.errors.push(TypeError {
                        message: format!(
                            "sum() requires int or float elements, found '{}'",
                            elem_ty
                        ),
                        span,
                    });
                    return None;
                }
                Some(elem_ty)
            }
            "count" => {
                if !args.is_empty() {
                    self.errors.push(TypeError {
                        message: format!("count() takes no arguments, found {}", args.len()),
                        span,
                    });
                    return None;
                }
                Some(Type::Int)
            }
            "position" => {
                if args.len() != 1 {
                    self.errors.push(TypeError {
                        message: format!(
                            "position() takes exactly 1 argument, found {}",
                            args.len()
                        ),
                        span,
                    });
                    return None;
                }
                if !matches!(args[0].kind, ExprKind::Closure(_)) {
                    self.errors.push(TypeError {
                        message: "position() requires an inline closure".to_string(),
                        span: args[0].span,
                    });
                    return None;
                }
                let closure_ty = self.infer_expr(&args[0])?;
                if let Type::Fn(params, ret) = closure_ty {
                    if params.len() != 1 {
                        self.errors.push(TypeError {
                            message: format!(
                                "position() closure must take exactly 1 parameter, found {}",
                                params.len()
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    if params[0] != elem_ty {
                        self.errors.push(TypeError {
                            message: format!(
                                "position() closure parameter type '{}' does not match element type '{}'",
                                params[0], elem_ty
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    if *ret != Type::Bool {
                        self.errors.push(TypeError {
                            message: format!(
                                "position() closure must return bool, found '{}'",
                                ret
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    return Some(Type::Generic("Option".to_string(), vec![Type::Int]));
                }
                None
            }
            _ => None,
        }
    }

    /// Infer the element type for an iterator pipeline chain.
    /// Recursively walks `.iter().map(f).filter(g)` chains and validates each step.
    /// Returns `None` if the expression is not a valid pipeline.
    fn infer_pipeline_elem_type(&mut self, expr: &Expr) -> Option<Type> {
        if let ExprKind::MethodCall(ref receiver, ref method, ref args) = expr.kind {
            match method.as_str() {
                "iter" => {
                    // Range expressions: (start..end).iter() or (start..=end).iter()
                    if matches!(
                        receiver.kind,
                        ExprKind::Range(_, _) | ExprKind::RangeInclusive(_, _)
                    ) {
                        if !args.is_empty() {
                            self.errors.push(TypeError {
                                message: "iter() takes no arguments".to_string(),
                                span: expr.span,
                            });
                        }
                        // Validate range bounds are int
                        let (start, end) = match &receiver.kind {
                            ExprKind::Range(s, e) | ExprKind::RangeInclusive(s, e) => (s, e),
                            _ => unreachable!(),
                        };
                        if let Some(t) = self.infer_expr(start) {
                            if t != Type::Int {
                                self.errors.push(TypeError {
                                    message: format!("range start must be int, found '{}'", t),
                                    span: start.span,
                                });
                            }
                        }
                        if let Some(t) = self.infer_expr(end) {
                            if t != Type::Int {
                                self.errors.push(TypeError {
                                    message: format!("range end must be int, found '{}'", t),
                                    span: end.span,
                                });
                            }
                        }
                        return Some(Type::Int);
                    }
                    let recv_ty = self.infer_expr(receiver);
                    match recv_ty {
                        Some(Type::Array(inner)) => {
                            if !args.is_empty() {
                                self.errors.push(TypeError {
                                    message: "iter() takes no arguments".to_string(),
                                    span: expr.span,
                                });
                            }
                            return Some(*inner);
                        }
                        Some(Type::Generic(ref name, ref type_args))
                            if name == "Set" && type_args.len() == 1 =>
                        {
                            if !args.is_empty() {
                                self.errors.push(TypeError {
                                    message: "iter() takes no arguments".to_string(),
                                    span: expr.span,
                                });
                            }
                            return Some(type_args[0].clone());
                        }
                        Some(Type::Generic(ref name, ref type_args))
                            if name == "Map" && type_args.len() == 2 =>
                        {
                            if !args.is_empty() {
                                self.errors.push(TypeError {
                                    message: "iter() takes no arguments".to_string(),
                                    span: expr.span,
                                });
                            }
                            return Some(Type::Tuple(type_args.clone()));
                        }
                        None => return None,
                        _ => return None, // Not an array/set/map — not a pipeline base
                    }
                }
                "map" => {
                    let input_ty = self.infer_pipeline_elem_type(receiver)?;
                    if args.len() != 1 {
                        self.errors.push(TypeError {
                            message: format!(
                                "map() takes exactly 1 argument, found {}",
                                args.len()
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                    // Require inline closure — named closure variables are not
                    // supported by the fused pipeline codegen.
                    if !matches!(args[0].kind, ExprKind::Closure(_)) {
                        self.errors.push(TypeError {
                            message: "map() requires an inline closure".to_string(),
                            span: args[0].span,
                        });
                        return None;
                    }
                    let closure_ty = self.infer_expr(&args[0])?;
                    if let Type::Fn(params, ret) = closure_ty {
                        if params.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "map() closure must take exactly 1 parameter, found {}",
                                    params.len()
                                ),
                                span: args[0].span,
                            });
                            return None;
                        }
                        if params[0] != input_ty {
                            self.errors.push(TypeError {
                                message: format!(
                                    "map() closure parameter type '{}' does not match element type '{}'",
                                    params[0], input_ty
                                ),
                                span: args[0].span,
                            });
                            return None;
                        }
                        if *ret == Type::Unit {
                            self.errors.push(TypeError {
                                message: "map() closure must not return unit".to_string(),
                                span: args[0].span,
                            });
                            return None;
                        }
                        return Some(*ret);
                    }
                    self.errors.push(TypeError {
                        message: "map() argument must be a closure".to_string(),
                        span: args[0].span,
                    });
                    return None;
                }
                "filter" => {
                    let input_ty = self.infer_pipeline_elem_type(receiver)?;
                    if args.len() != 1 {
                        self.errors.push(TypeError {
                            message: format!(
                                "filter() takes exactly 1 argument, found {}",
                                args.len()
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                    // Require inline closure — named closure variables are not
                    // supported by the fused pipeline codegen.
                    if !matches!(args[0].kind, ExprKind::Closure(_)) {
                        self.errors.push(TypeError {
                            message: "filter() requires an inline closure".to_string(),
                            span: args[0].span,
                        });
                        return None;
                    }
                    let closure_ty = self.infer_expr(&args[0])?;
                    if let Type::Fn(params, ret) = closure_ty {
                        if params.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "filter() closure must take exactly 1 parameter, found {}",
                                    params.len()
                                ),
                                span: args[0].span,
                            });
                            return None;
                        }
                        if params[0] != input_ty {
                            self.errors.push(TypeError {
                                message: format!(
                                    "filter() closure parameter type '{}' does not match element type '{}'",
                                    params[0], input_ty
                                ),
                                span: args[0].span,
                            });
                            return None;
                        }
                        if *ret != Type::Bool {
                            self.errors.push(TypeError {
                                message: format!(
                                    "filter() closure must return bool, found '{}'",
                                    ret
                                ),
                                span: args[0].span,
                            });
                            return None;
                        }
                        return Some(input_ty);
                    }
                    self.errors.push(TypeError {
                        message: "filter() argument must be a closure".to_string(),
                        span: args[0].span,
                    });
                    return None;
                }
                "enumerate" => {
                    let input_ty = self.infer_pipeline_elem_type(receiver)?;
                    if !args.is_empty() {
                        self.errors.push(TypeError {
                            message: format!(
                                "enumerate() takes no arguments, found {}",
                                args.len()
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                    return Some(Type::Tuple(vec![Type::Int, input_ty]));
                }
                "zip" => {
                    let input_ty = self.infer_pipeline_elem_type(receiver)?;
                    if args.len() != 1 {
                        self.errors.push(TypeError {
                            message: format!(
                                "zip() takes exactly 1 argument, found {}",
                                args.len()
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                    // Accept both arrays and range expressions as zip arguments
                    if matches!(
                        args[0].kind,
                        ExprKind::Range(_, _) | ExprKind::RangeInclusive(_, _)
                    ) {
                        // Validate range bounds are int
                        let (start, end) = match &args[0].kind {
                            ExprKind::Range(s, e) | ExprKind::RangeInclusive(s, e) => (s, e),
                            _ => unreachable!(),
                        };
                        if let Some(t) = self.infer_expr(start) {
                            if t != Type::Int {
                                self.errors.push(TypeError {
                                    message: format!("range start must be int, found '{}'", t),
                                    span: start.span,
                                });
                            }
                        }
                        if let Some(t) = self.infer_expr(end) {
                            if t != Type::Int {
                                self.errors.push(TypeError {
                                    message: format!("range end must be int, found '{}'", t),
                                    span: end.span,
                                });
                            }
                        }
                        return Some(Type::Tuple(vec![input_ty, Type::Int]));
                    }
                    let arg_ty = self.infer_expr(&args[0])?;
                    if let Type::Array(inner) = arg_ty {
                        return Some(Type::Tuple(vec![input_ty, *inner]));
                    }
                    self.errors.push(TypeError {
                        message: format!(
                            "zip() argument must be an array or range, found '{}'",
                            arg_ty
                        ),
                        span: args[0].span,
                    });
                    return None;
                }
                "take" | "skip" => {
                    let input_ty = self.infer_pipeline_elem_type(receiver)?;
                    if args.len() != 1 {
                        self.errors.push(TypeError {
                            message: format!(
                                "{}() takes exactly 1 argument, found {}",
                                method,
                                args.len()
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                    let arg_ty = self.infer_expr(&args[0])?;
                    if arg_ty != Type::Int {
                        self.errors.push(TypeError {
                            message: format!(
                                "{}() argument must be int, found '{}'",
                                method, arg_ty
                            ),
                            span: args[0].span,
                        });
                        return None;
                    }
                    return Some(input_ty);
                }
                "chain" => {
                    // chain() must be the first combinator after .iter() —
                    // if it appeared after map/filter/etc., the conditional
                    // source selection would process garbage for indices
                    // beyond the first array's length.
                    let is_iter_base = matches!(
                        receiver.kind,
                        ExprKind::MethodCall(_, ref m, _) if m == "iter"
                    );
                    if !is_iter_base {
                        self.errors.push(TypeError {
                            message: "chain() must be the first combinator after .iter()"
                                .to_string(),
                            span: expr.span,
                        });
                        return None;
                    }
                    let input_ty = self.infer_pipeline_elem_type(receiver)?;
                    if args.len() != 1 {
                        self.errors.push(TypeError {
                            message: format!(
                                "chain() takes exactly 1 argument, found {}",
                                args.len()
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                    let arg_ty = self.infer_expr(&args[0])?;
                    if let Type::Array(inner) = arg_ty {
                        if *inner != input_ty {
                            self.errors.push(TypeError {
                                message: format!(
                                    "chain() argument element type '{}' does not match pipeline element type '{}'",
                                    inner, input_ty
                                ),
                                span: args[0].span,
                            });
                            return None;
                        }
                        return Some(input_ty);
                    }
                    self.errors.push(TypeError {
                        message: format!("chain() argument must be an array, found '{}'", arg_ty),
                        span: args[0].span,
                    });
                    return None;
                }
                "take_while" => {
                    let input_ty = self.infer_pipeline_elem_type(receiver)?;
                    if args.len() != 1 {
                        self.errors.push(TypeError {
                            message: format!(
                                "take_while() takes exactly 1 argument, found {}",
                                args.len()
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                    if !matches!(args[0].kind, ExprKind::Closure(_)) {
                        self.errors.push(TypeError {
                            message: "take_while() requires an inline closure".to_string(),
                            span: args[0].span,
                        });
                        return None;
                    }
                    let closure_ty = self.infer_expr(&args[0])?;
                    if let Type::Fn(params, ret) = closure_ty {
                        if params.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "take_while() closure must take exactly 1 parameter, found {}",
                                    params.len()
                                ),
                                span: args[0].span,
                            });
                            return None;
                        }
                        if params[0] != input_ty {
                            self.errors.push(TypeError {
                                message: format!(
                                    "take_while() closure parameter type '{}' does not match element type '{}'",
                                    params[0], input_ty
                                ),
                                span: args[0].span,
                            });
                            return None;
                        }
                        if *ret != Type::Bool {
                            self.errors.push(TypeError {
                                message: format!(
                                    "take_while() closure must return bool, found '{}'",
                                    ret
                                ),
                                span: args[0].span,
                            });
                            return None;
                        }
                        return Some(input_ty);
                    }
                    self.errors.push(TypeError {
                        message: "take_while() argument must be a closure".to_string(),
                        span: args[0].span,
                    });
                    return None;
                }
                "rev" => {
                    // rev() must be the first combinator after .iter()
                    let is_iter_base = matches!(
                        receiver.kind,
                        ExprKind::MethodCall(_, ref m, _) if m == "iter"
                    );
                    if !is_iter_base {
                        self.errors.push(TypeError {
                            message: "rev() must be the first combinator after .iter()".to_string(),
                            span: expr.span,
                        });
                        return None;
                    }
                    let input_ty = self.infer_pipeline_elem_type(receiver)?;
                    if !args.is_empty() {
                        self.errors.push(TypeError {
                            message: format!("rev() takes no arguments, found {}", args.len()),
                            span: expr.span,
                        });
                        return None;
                    }
                    return Some(input_ty);
                }
                "chars" => {
                    let recv_ty = self.infer_expr(receiver);
                    match recv_ty {
                        Some(Type::Str) => {
                            if !args.is_empty() {
                                self.errors.push(TypeError {
                                    message: "chars() takes no arguments".to_string(),
                                    span: expr.span,
                                });
                            }
                            return Some(Type::Char);
                        }
                        None => return None,
                        _ => return None,
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Infer the element type for a for-loop iterable expression.
    /// Handles iterator pipelines (.iter().map().filter() chains) first,
    /// then `.iter()` on arrays structurally (without introducing a phantom type),
    /// then falls back to general expression inference for other iterables.
    fn infer_for_iterable(&mut self, iterable: &Expr) -> Option<Type> {
        // Try iterator pipeline (.iter().map().filter() chains)
        let errors_before = self.errors.len();
        if let Some(ty) = self.infer_pipeline_elem_type(iterable) {
            return Some(ty);
        }

        // If the iterable is an iterator pipeline (.iter().map()/.filter() chain)
        // but validation failed, errors have already been reported. Don't fall
        // through to avoid confusing cascade errors. Only applies to chains that
        // actually have .iter() as the base — struct methods named map/filter
        // should fall through to normal method resolution.
        if Self::is_iter_pipeline(iterable) {
            if self.errors.len() == errors_before {
                self.errors.push(TypeError {
                    message: "pipeline combinators require .iter() on an array".to_string(),
                    span: iterable.span,
                });
            }
            return None;
        }

        // Special case: .iter() on arrays — handle before general inference
        // because arrays have no method table and infer_expr would reject the call.
        // (infer_pipeline_elem_type already handles array .iter(), but struct .iter()
        // methods fall through to here.)
        if let ExprKind::MethodCall(ref receiver, ref method, ref args) = iterable.kind {
            if method == "iter" {
                let recv_ty = self.infer_expr(receiver);
                match recv_ty {
                    Some(Type::Array(inner)) => {
                        if !args.is_empty() {
                            self.errors.push(TypeError {
                                message: "iter() takes no arguments".to_string(),
                                span: iterable.span,
                            });
                        }
                        return Some(*inner);
                    }
                    None => return None, // Receiver has errors, don't cascade
                    _ => {}              // Not an array, fall through to general inference
                }
            }
        }

        let iter_ty = self.infer_expr(iterable)?;
        if let Type::Array(inner) = iter_ty {
            return Some(*inner);
        }
        self.errors.push(TypeError {
            message: format!("for loop requires an array or range, found '{}'", iter_ty),
            span: iterable.span,
        });
        None
    }

    // ── Expression type inference ────────────────────────────

    fn infer_expr(&mut self, expr: &Expr) -> Option<Type> {
        self.infer_expr_with_expectation(expr, None)
    }

    fn infer_expr_with_expectation(
        &mut self,
        expr: &Expr,
        expected_type: Option<&Type>,
    ) -> Option<Type> {
        match &expr.kind {
            ExprKind::Literal(lit) => Some(match lit {
                Literal::Int(_) => Type::Int,
                Literal::Float(_) => Type::Float,
                Literal::Bool(_) => Type::Bool,
                Literal::Char(_) => Type::Char,
                Literal::String(_) => Type::Str,
                Literal::Unit => Type::Unit,
            }),

            ExprKind::Ident(name) => {
                if let Some(info) = self.lookup(name) {
                    let ty = info.ty.clone();
                    if self.collect_symbols {
                        let def_span = info.def_span;
                        self.symbol_table.references.push(SymbolRef {
                            span: expr.span,
                            resolved_type: format!("{}", ty),
                            def_name: name.clone(),
                            def_span: Some(def_span),
                        });
                    }
                    Some(ty)
                } else {
                    // Check if it's a data-less enum variant (e.g., None)
                    for (enum_name, info) in &self.enums {
                        for (vname, vfields) in &info.variants {
                            if vname == name && vfields.is_empty() {
                                if !info.type_params.is_empty() {
                                    // Generic enum: return with TypeVar placeholders
                                    let type_args: Vec<Type> = info
                                        .type_params
                                        .iter()
                                        .map(|tp| Type::TypeVar(tp.clone()))
                                        .collect();
                                    return Some(Type::Generic(enum_name.clone(), type_args));
                                }
                                return Some(Type::Named(enum_name.clone()));
                            }
                        }
                    }
                    self.errors.push(TypeError {
                        message: format!("undefined variable '{}'", name),
                        span: expr.span,
                    });
                    None
                }
            }

            ExprKind::Binary(lhs, op, rhs) => {
                let lt = self.infer_expr(lhs)?;
                let rt = self.infer_expr(rhs)?;
                self.check_binary_op(*op, &lt, &rt, expr.span)
            }

            ExprKind::Unary(op, operand) => {
                let ty = self.infer_expr(operand)?;
                match op {
                    UnaryOp::Neg => {
                        if ty != Type::Int && ty != Type::Float {
                            self.errors.push(TypeError {
                                message: format!("cannot negate type '{}'", ty),
                                span: expr.span,
                            });
                            None
                        } else {
                            Some(ty)
                        }
                    }
                    UnaryOp::Not => {
                        if ty != Type::Bool {
                            self.errors.push(TypeError {
                                message: format!("'not' requires 'bool', found '{}'", ty),
                                span: expr.span,
                            });
                            None
                        } else {
                            Some(Type::Bool)
                        }
                    }
                }
            }

            ExprKind::Call(callee, args) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    // Built-in len() for arrays
                    if name == "len" {
                        if args.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!("'len' expects 1 argument, found {}", args.len()),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(arg_ty) = self.infer_expr(&args[0]) {
                            if matches!(arg_ty, Type::Array(_)) {
                                return Some(Type::Int);
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "'len' requires an array argument, found '{}'",
                                        arg_ty
                                    ),
                                    span: args[0].span,
                                });
                                return None;
                            }
                        }
                        return None;
                    }
                    // Built-in concurrency functions
                    if name == "chan" {
                        self.check_call_effects("chan", expr.span);
                        if !args.is_empty() {
                            self.errors.push(TypeError {
                                message: "chan() takes no arguments".to_string(),
                                span: expr.span,
                            });
                        }
                        // Type is inferred from the let binding context
                        // For now return Chan<int> as default; the let stmt will set the real type
                        return Some(Type::Chan(Box::new(Type::Int)));
                    }
                    if name == "send" {
                        self.check_call_effects("send", expr.span);
                        if args.len() != 2 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'send' expects 2 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(ch_ty) = self.infer_expr(&args[0]) {
                            if let Type::Chan(inner) = &ch_ty {
                                if let Some(val_ty) = self.infer_expr(&args[1]) {
                                    if val_ty != **inner {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "send: channel expects '{}', found '{}'",
                                                inner, val_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                }
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "send: first argument must be a channel, found '{}'",
                                        ch_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return Some(Type::Unit);
                    }
                    if name == "recv" {
                        self.check_call_effects("recv", expr.span);
                        if args.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!("'recv' expects 1 argument, found {}", args.len()),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(ch_ty) = self.infer_expr(&args[0]) {
                            if let Type::Chan(inner) = ch_ty {
                                return Some(*inner);
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "recv: argument must be a channel, found '{}'",
                                        ch_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return None;
                    }

                    // Built-in to_str() — polymorphic string conversion
                    if name == "to_str" {
                        if args.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'to_str' expects 1 argument, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        // Type-check the argument but accept any type
                        self.infer_expr(&args[0]);
                        return Some(Type::Str);
                    }

                    // Built-in str_split() — returns [string]
                    if name == "str_split" {
                        if args.len() != 2 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'str_split' expects 2 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'str_split'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        for arg in args {
                            if let Some(ty) = self.infer_expr(arg) {
                                if ty != Type::Str {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "str_split: expected string argument, found '{}'",
                                            ty
                                        ),
                                        span: arg.span,
                                    });
                                }
                            }
                        }
                        return Some(Type::Array(Box::new(Type::Str)));
                    }

                    // Built-in args() — returns command-line arguments
                    if name == "args" {
                        if !args.is_empty() {
                            self.errors.push(TypeError {
                                message: "args() takes no arguments".to_string(),
                                span: expr.span,
                            });
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'args'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        self.check_call_effects("args", expr.span);
                        return Some(Type::Array(Box::new(Type::Str)));
                    }

                    // Built-in push() for dynamic arrays
                    if name == "push" {
                        if args.len() != 2 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'push' expects 2 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'push'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        if let Some(arr_ty) = self.infer_expr(&args[0]) {
                            if let Type::Array(inner) = &arr_ty {
                                if let Some(val_ty) = self.infer_expr(&args[1]) {
                                    if !Self::types_compatible(inner, &val_ty) {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "push: array expects '{}', found '{}'",
                                                inner, val_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                }
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "push: first argument must be an array, found '{}'",
                                        arr_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return Some(Type::Unit);
                    }
                    // Built-in pop() for dynamic arrays
                    if name == "pop" {
                        if args.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!("'pop' expects 1 argument, found {}", args.len()),
                                span: expr.span,
                            });
                            return None;
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'pop'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        if let Some(arr_ty) = self.infer_expr(&args[0]) {
                            if let Type::Array(inner) = arr_ty {
                                return Some(*inner);
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "pop: argument must be an array, found '{}'",
                                        arr_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return None;
                    }

                    // Built-in slice(arr, start, end) — returns [T] (pure)
                    if name == "slice" {
                        if args.len() != 3 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'slice' expects 3 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(arr_ty) = self.infer_expr(&args[0]) {
                            if let Type::Array(inner) = &arr_ty {
                                for arg in &args[1..] {
                                    if let Some(t) = self.infer_expr(arg) {
                                        if t != Type::Int {
                                            self.errors.push(TypeError {
                                                message: format!(
                                                    "slice: index must be int, found '{}'",
                                                    t
                                                ),
                                                span: arg.span,
                                            });
                                        }
                                    }
                                }
                                return Some(Type::Array(inner.clone()));
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "slice: first argument must be an array, found '{}'",
                                        arr_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return None;
                    }

                    // Built-in concat_arrays(a, b) — returns [T] (pure)
                    if name == "concat_arrays" {
                        if args.len() != 2 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'concat_arrays' expects 2 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(a_ty) = self.infer_expr(&args[0]) {
                            if let Type::Array(inner_a) = &a_ty {
                                if let Some(b_ty) = self.infer_expr(&args[1]) {
                                    if let Type::Array(inner_b) = &b_ty {
                                        if *inner_a != *inner_b {
                                            self.errors.push(TypeError {
                                                message: format!(
                                                    "concat_arrays: array types must match, found '[{}]' and '[{}]'",
                                                    inner_a, inner_b
                                                ),
                                                span: expr.span,
                                            });
                                        }
                                    } else {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "concat_arrays: second argument must be an array, found '{}'",
                                                b_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                }
                                return Some(Type::Array(inner_a.clone()));
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "concat_arrays: first argument must be an array, found '{}'",
                                        a_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return None;
                    }

                    // Built-in reverse(arr) — returns [T] (pure)
                    if name == "reverse" {
                        if args.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'reverse' expects 1 argument, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(arr_ty) = self.infer_expr(&args[0]) {
                            if let Type::Array(inner) = &arr_ty {
                                return Some(Type::Array(inner.clone()));
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "reverse: argument must be an array, found '{}'",
                                        arr_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return None;
                    }

                    // ── Generic Map<K, V> builtins ──
                    if name == "map_new" {
                        if !args.is_empty() {
                            self.errors.push(TypeError {
                                message: "map_new() takes no arguments".to_string(),
                                span: expr.span,
                            });
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'map_new'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        // K, V resolved by let-binding compatibility (TypeVar acts as wildcard)
                        return Some(Type::Generic(
                            "Map".to_string(),
                            vec![
                                Type::TypeVar("K".to_string()),
                                Type::TypeVar("V".to_string()),
                            ],
                        ));
                    }
                    if name == "map_set" {
                        if args.len() != 3 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'map_set' expects 3 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'map_set'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        if let Some(map_ty) = self.infer_expr(&args[0]) {
                            if let Some((k_ty, v_ty)) = self.map_kv_types(&map_ty) {
                                if let Some(key_ty) = self.infer_expr(&args[1]) {
                                    if !matches!(key_ty, Type::TypeVar(_))
                                        && !matches!(k_ty, Type::TypeVar(_))
                                        && key_ty != k_ty
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "map_set: key type mismatch, expected '{}', found '{}'",
                                                k_ty, key_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                    if !self.is_hashable_key(&key_ty)
                                        && !matches!(key_ty, Type::TypeVar(_))
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "map key type '{}' is not hashable (must be int, string, char, or bool)",
                                                key_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                }
                                if let Some(val_ty) = self.infer_expr(&args[2]) {
                                    if !matches!(val_ty, Type::TypeVar(_))
                                        && !matches!(v_ty, Type::TypeVar(_))
                                        && val_ty != v_ty
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "map_set: value type mismatch, expected '{}', found '{}'",
                                                v_ty, val_ty
                                            ),
                                            span: args[2].span,
                                        });
                                    }
                                }
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "map_set: first argument must be a Map, found '{}'",
                                        map_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return Some(Type::Unit);
                    }
                    if name == "map_get" {
                        if args.len() != 2 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'map_get' expects 2 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(map_ty) = self.infer_expr(&args[0]) {
                            if let Some((k_ty, v_ty)) = self.map_kv_types(&map_ty) {
                                if let Some(key_ty) = self.infer_expr(&args[1]) {
                                    if !matches!(key_ty, Type::TypeVar(_))
                                        && !matches!(k_ty, Type::TypeVar(_))
                                        && key_ty != k_ty
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "map_get: key type mismatch, expected '{}', found '{}'",
                                                k_ty, key_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                }
                                return Some(v_ty);
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "map_get: first argument must be a Map, found '{}'",
                                        map_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return None;
                    }
                    if name == "map_has" {
                        if args.len() != 2 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'map_has' expects 2 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(map_ty) = self.infer_expr(&args[0]) {
                            if let Some((k_ty, _)) = self.map_kv_types(&map_ty) {
                                if let Some(key_ty) = self.infer_expr(&args[1]) {
                                    if !matches!(key_ty, Type::TypeVar(_))
                                        && !matches!(k_ty, Type::TypeVar(_))
                                        && key_ty != k_ty
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "map_has: key type mismatch, expected '{}', found '{}'",
                                                k_ty, key_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                }
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "map_has: first argument must be a Map, found '{}'",
                                        map_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return Some(Type::Bool);
                    }
                    if name == "map_size" {
                        if args.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'map_size' expects 1 argument, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(map_ty) = self.infer_expr(&args[0]) {
                            if self.map_kv_types(&map_ty).is_none() {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "map_size: argument must be a Map, found '{}'",
                                        map_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return Some(Type::Int);
                    }
                    if name == "map_remove" {
                        if args.len() != 2 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'map_remove' expects 2 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'map_remove'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        if let Some(map_ty) = self.infer_expr(&args[0]) {
                            if let Some((k_ty, _)) = self.map_kv_types(&map_ty) {
                                if let Some(key_ty) = self.infer_expr(&args[1]) {
                                    if !matches!(key_ty, Type::TypeVar(_))
                                        && !matches!(k_ty, Type::TypeVar(_))
                                        && key_ty != k_ty
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "map_remove: key type mismatch, expected '{}', found '{}'",
                                                k_ty, key_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                }
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "map_remove: first argument must be a Map, found '{}'",
                                        map_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return Some(Type::Bool);
                    }
                    if name == "map_keys" {
                        if args.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'map_keys' expects 1 argument, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'map_keys'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        if let Some(map_ty) = self.infer_expr(&args[0]) {
                            if let Some((k_ty, _)) = self.map_kv_types(&map_ty) {
                                return Some(Type::Array(Box::new(k_ty)));
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "map_keys: argument must be a Map, found '{}'",
                                        map_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return None;
                    }
                    if name == "map_values" {
                        if args.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'map_values' expects 1 argument, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'map_values'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        if let Some(map_ty) = self.infer_expr(&args[0]) {
                            if let Some((_, v_ty)) = self.map_kv_types(&map_ty) {
                                return Some(Type::Array(Box::new(v_ty)));
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "map_values: argument must be a Map, found '{}'",
                                        map_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return None;
                    }

                    // ── Generic Set<T> builtins ──
                    if name == "set_new" {
                        if !args.is_empty() {
                            self.errors.push(TypeError {
                                message: "set_new() takes no arguments".to_string(),
                                span: expr.span,
                            });
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'set_new'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        return Some(Type::Generic(
                            "Set".to_string(),
                            vec![Type::TypeVar("T".to_string())],
                        ));
                    }
                    if name == "set_add" {
                        if args.len() != 2 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'set_add' expects 2 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'set_add'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        if let Some(set_ty) = self.infer_expr(&args[0]) {
                            if let Some(elem_ty) = self.set_elem_type(&set_ty) {
                                if let Some(val_ty) = self.infer_expr(&args[1]) {
                                    if !matches!(val_ty, Type::TypeVar(_))
                                        && !matches!(elem_ty, Type::TypeVar(_))
                                        && val_ty != elem_ty
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "set_add: element type mismatch, expected '{}', found '{}'",
                                                elem_ty, val_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                    if !self.is_hashable_key(&val_ty)
                                        && !matches!(val_ty, Type::TypeVar(_))
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "set element type '{}' is not hashable (must be int, string, char, or bool)",
                                                val_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                }
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "set_add: first argument must be a Set, found '{}'",
                                        set_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return Some(Type::Unit);
                    }
                    if name == "set_has" {
                        if args.len() != 2 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'set_has' expects 2 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(set_ty) = self.infer_expr(&args[0]) {
                            if let Some(elem_ty) = self.set_elem_type(&set_ty) {
                                if let Some(val_ty) = self.infer_expr(&args[1]) {
                                    if !matches!(val_ty, Type::TypeVar(_))
                                        && !matches!(elem_ty, Type::TypeVar(_))
                                        && val_ty != elem_ty
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "set_has: element type mismatch, expected '{}', found '{}'",
                                                elem_ty, val_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                }
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "set_has: first argument must be a Set, found '{}'",
                                        set_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return Some(Type::Bool);
                    }
                    if name == "set_remove" {
                        if args.len() != 2 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'set_remove' expects 2 arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'set_remove'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        if let Some(set_ty) = self.infer_expr(&args[0]) {
                            if let Some(elem_ty) = self.set_elem_type(&set_ty) {
                                if let Some(val_ty) = self.infer_expr(&args[1]) {
                                    if !matches!(val_ty, Type::TypeVar(_))
                                        && !matches!(elem_ty, Type::TypeVar(_))
                                        && val_ty != elem_ty
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "set_remove: element type mismatch, expected '{}', found '{}'",
                                                elem_ty, val_ty
                                            ),
                                            span: args[1].span,
                                        });
                                    }
                                }
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "set_remove: first argument must be a Set, found '{}'",
                                        set_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return Some(Type::Bool);
                    }
                    if name == "set_size" {
                        if args.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'set_size' expects 1 argument, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if let Some(set_ty) = self.infer_expr(&args[0]) {
                            if self.set_elem_type(&set_ty).is_none() {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "set_size: argument must be a Set, found '{}'",
                                        set_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return Some(Type::Int);
                    }
                    if name == "set_values" {
                        if args.len() != 1 {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'set_values' expects 1 argument, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        if self.current_fn_is_pure {
                            self.errors.push(TypeError {
                                message: "pure function cannot call impure function 'set_values'"
                                    .to_string(),
                                span: expr.span,
                            });
                        }
                        if let Some(set_ty) = self.infer_expr(&args[0]) {
                            if let Some(elem_ty) = self.set_elem_type(&set_ty) {
                                return Some(Type::Array(Box::new(elem_ty)));
                            } else {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "set_values: argument must be a Set, found '{}'",
                                        set_ty
                                    ),
                                    span: args[0].span,
                                });
                            }
                        }
                        return None;
                    }

                    if let Some(sig) = self.functions.get(name).cloned() {
                        if self.collect_symbols {
                            let def_span = if sig.span.line == 0 {
                                None
                            } else {
                                Some(sig.span)
                            };
                            self.symbol_table.references.push(SymbolRef {
                                span: callee.span,
                                resolved_type: format!("{}", sig.ret),
                                def_name: name.clone(),
                                def_span,
                            });
                        }
                        // Enforce purity: pure functions cannot call impure functions
                        if self.current_fn_is_pure && !sig.is_pure {
                            self.errors.push(TypeError {
                                message: format!(
                                    "pure function cannot call impure function '{}'",
                                    name
                                ),
                                span: expr.span,
                            });
                        }
                        self.check_call_effects(name, expr.span);
                        if args.len() != sig.params.len() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "'{}' expects {} arguments, found {}",
                                    name,
                                    sig.params.len(),
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }

                        // For generic functions, infer type args and substitute
                        if !sig.type_params.is_empty() {
                            let mut bindings: HashMap<String, Type> = HashMap::new();
                            for (i, arg) in args.iter().enumerate() {
                                if let Some(arg_ty) = self.infer_expr(arg) {
                                    // If param is a type param name (Named("T")), bind it
                                    if let Type::Named(ref pname) = sig.params[i] {
                                        if sig.type_params.contains(pname) {
                                            bindings.insert(pname.clone(), arg_ty);
                                        }
                                    }
                                }
                            }

                            // Infer from expected return type (target typing)
                            if let Some(expected) = expected_type {
                                self.infer_type_vars_from_expected(
                                    &sig.ret,
                                    expected,
                                    &sig.type_params,
                                    &mut bindings,
                                );
                            }

                            // Substitute return type
                            let ret = self.substitute_type_vars(&sig.ret, &bindings);
                            return Some(ret);
                        }

                        for (i, arg) in args.iter().enumerate() {
                            if let Some(arg_ty) = self.infer_expr(arg) {
                                if arg_ty != sig.params[i] {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "argument {} of '{}': expected '{}', found '{}'",
                                            i + 1,
                                            name,
                                            sig.params[i],
                                            arg_ty
                                        ),
                                        span: arg.span,
                                    });
                                }
                            }
                        }
                        Some(sig.ret)
                    } else {
                        // Check if it's an enum variant constructor
                        // Prefer non-generic (user-defined) enums over generic (prelude) enums
                        let enums_snapshot = self.enums.clone();
                        for (enum_name, info) in &enums_snapshot {
                            if info.type_params.is_empty() {
                                for (vname, vfields) in &info.variants {
                                    if vname == name && !vfields.is_empty() {
                                        return Some(Type::Named(enum_name.clone()));
                                    }
                                }
                            }
                        }
                        // Then check generic enums
                        for (enum_name, info) in &enums_snapshot {
                            if !info.type_params.is_empty() {
                                for (vname, vfields) in &info.variants {
                                    if vname == name && !vfields.is_empty() {
                                        let mut bindings: HashMap<String, Type> = HashMap::new();
                                        for (i, arg) in args.iter().enumerate() {
                                            if i < vfields.len() {
                                                if let Some(arg_ty) = self.infer_expr(arg) {
                                                    if let Type::TypeVar(ref tv) = vfields[i] {
                                                        bindings.insert(tv.clone(), arg_ty);
                                                    }
                                                }
                                            }
                                        }
                                        let type_args: Vec<Type> = info
                                            .type_params
                                            .iter()
                                            .map(|tp| {
                                                bindings
                                                    .get(tp)
                                                    .cloned()
                                                    .unwrap_or(Type::TypeVar(tp.clone()))
                                            })
                                            .collect();
                                        return Some(Type::Generic(enum_name.clone(), type_args));
                                    }
                                }
                            }
                        }
                        // Check if it's a local variable with function type (indirect call)
                        if let Some(var_info) = self.lookup(name) {
                            let var_ty = var_info.ty.clone();
                            if let Type::Fn(param_types, ret_type) = var_ty {
                                if args.len() != param_types.len() {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "'{}' expects {} arguments, found {}",
                                            name,
                                            param_types.len(),
                                            args.len()
                                        ),
                                        span: expr.span,
                                    });
                                    return None;
                                }
                                for (i, arg) in args.iter().enumerate() {
                                    if let Some(arg_ty) = self.infer_expr(arg) {
                                        if arg_ty != param_types[i] {
                                            self.errors.push(TypeError {
                                                message: format!(
                                                    "argument {} of '{}': expected '{}', found '{}'",
                                                    i + 1,
                                                    name,
                                                    param_types[i],
                                                    arg_ty
                                                ),
                                                span: arg.span,
                                            });
                                        }
                                    }
                                }
                                return Some(*ret_type);
                            }
                        }
                        self.errors.push(TypeError {
                            message: format!("undefined function '{}'", name),
                            span: expr.span,
                        });
                        None
                    }
                } else {
                    // Indirect call — callee must be a Fn type
                    let callee_ty = self.infer_expr(callee)?;
                    if let Type::Fn(param_types, ret_type) = callee_ty {
                        if args.len() != param_types.len() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "closure expects {} arguments, found {}",
                                    param_types.len(),
                                    args.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                        for (i, arg) in args.iter().enumerate() {
                            if let Some(arg_ty) = self.infer_expr(arg) {
                                if arg_ty != param_types[i] {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "argument {}: expected '{}', found '{}'",
                                            i + 1,
                                            param_types[i],
                                            arg_ty
                                        ),
                                        span: arg.span,
                                    });
                                }
                            }
                        }
                        Some(*ret_type)
                    } else {
                        self.errors.push(TypeError {
                            message: format!("'{}' is not callable", callee_ty),
                            span: expr.span,
                        });
                        None
                    }
                }
            }

            ExprKind::MethodCall(receiver, method_name, args) => {
                // Intercept pipeline terminators BEFORE receiver inference
                // (receiver is a pipeline chain that infer_expr would reject)
                if Self::is_pipeline_terminator(method_name) && Self::has_iter_base(receiver) {
                    return self.infer_pipeline_terminator_type(
                        receiver,
                        method_name,
                        args,
                        expr.span,
                    );
                }

                let recv_ty = self.infer_expr(receiver)?;

                // Handle array methods: .clear()
                if let Type::Array(_) = &recv_ty {
                    if method_name == "clear" {
                        if !args.is_empty() {
                            self.errors.push(TypeError {
                                message: format!(
                                    "clear() takes no arguments, found {}",
                                    args.len()
                                ),
                                span: expr.span,
                            });
                        }
                        return Some(Type::Unit);
                    }
                }

                // Handle .join() on Task<T>
                if let Type::Task(inner) = &recv_ty {
                    if method_name == "join" {
                        if !args.is_empty() {
                            self.errors.push(TypeError {
                                message: "join() takes no arguments".to_string(),
                                span: expr.span,
                            });
                        }
                        if self.collect_symbols {
                            self.symbol_table.references.push(SymbolRef {
                                span: expr.span,
                                resolved_type: format!("{}", inner),
                                def_name: "join".to_string(),
                                def_span: None,
                            });
                        }
                        return Some(*inner.clone());
                    } else {
                        self.errors.push(TypeError {
                            message: format!(
                                "no method '{}' found for type '{}'",
                                method_name, recv_ty
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                }

                // Handle Option<T> and Result<T, E> methods
                if let Type::Generic(ref enum_name, ref type_args) = recv_ty {
                    if enum_name == "Option" {
                        match method_name.as_str() {
                            "unwrap" => {
                                if !args.is_empty() {
                                    self.errors.push(TypeError {
                                        message: "unwrap() takes no arguments".to_string(),
                                        span: expr.span,
                                    });
                                }
                                let ret = type_args.first().cloned();
                                if self.collect_symbols {
                                    self.symbol_table.references.push(SymbolRef {
                                        span: expr.span,
                                        resolved_type: ret
                                            .as_ref()
                                            .map(|t| format!("{}", t))
                                            .unwrap_or_default(),
                                        def_name: method_name.clone(),
                                        def_span: None,
                                    });
                                }
                                return ret;
                            }
                            "is_some" | "is_none" => {
                                if !args.is_empty() {
                                    self.errors.push(TypeError {
                                        message: format!("{}() takes no arguments", method_name),
                                        span: expr.span,
                                    });
                                }
                                if self.collect_symbols {
                                    self.symbol_table.references.push(SymbolRef {
                                        span: expr.span,
                                        resolved_type: "bool".to_string(),
                                        def_name: method_name.clone(),
                                        def_span: None,
                                    });
                                }
                                return Some(Type::Bool);
                            }
                            _ => {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "no method '{}' found for type '{}'",
                                        method_name, recv_ty
                                    ),
                                    span: expr.span,
                                });
                                return None;
                            }
                        }
                    }
                    if enum_name == "Result" {
                        match method_name.as_str() {
                            "unwrap" => {
                                if !args.is_empty() {
                                    self.errors.push(TypeError {
                                        message: "unwrap() takes no arguments".to_string(),
                                        span: expr.span,
                                    });
                                }
                                let ret = type_args.first().cloned();
                                if self.collect_symbols {
                                    self.symbol_table.references.push(SymbolRef {
                                        span: expr.span,
                                        resolved_type: ret
                                            .as_ref()
                                            .map(|t| format!("{}", t))
                                            .unwrap_or_default(),
                                        def_name: method_name.clone(),
                                        def_span: None,
                                    });
                                }
                                return ret;
                            }
                            "unwrap_err" => {
                                if !args.is_empty() {
                                    self.errors.push(TypeError {
                                        message: "unwrap_err() takes no arguments".to_string(),
                                        span: expr.span,
                                    });
                                }
                                let ret = type_args.get(1).cloned();
                                if self.collect_symbols {
                                    self.symbol_table.references.push(SymbolRef {
                                        span: expr.span,
                                        resolved_type: ret
                                            .as_ref()
                                            .map(|t| format!("{}", t))
                                            .unwrap_or_default(),
                                        def_name: method_name.clone(),
                                        def_span: None,
                                    });
                                }
                                return ret;
                            }
                            "is_ok" | "is_err" => {
                                if !args.is_empty() {
                                    self.errors.push(TypeError {
                                        message: format!("{}() takes no arguments", method_name),
                                        span: expr.span,
                                    });
                                }
                                if self.collect_symbols {
                                    self.symbol_table.references.push(SymbolRef {
                                        span: expr.span,
                                        resolved_type: "bool".to_string(),
                                        def_name: method_name.clone(),
                                        def_span: None,
                                    });
                                }
                                return Some(Type::Bool);
                            }
                            _ => {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "no method '{}' found for type '{}'",
                                        method_name, recv_ty
                                    ),
                                    span: expr.span,
                                });
                                return None;
                            }
                        }
                    }
                }

                let type_name = match &recv_ty {
                    Type::Named(n) => n.clone(),
                    Type::Ref(inner) | Type::MutRef(inner) => {
                        if let Type::Named(n) = inner.as_ref() {
                            n.clone()
                        } else {
                            self.errors.push(TypeError {
                                message: format!(
                                    "method call requires a struct type, found '{}'",
                                    recv_ty
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                    }
                    _ => {
                        self.errors.push(TypeError {
                            message: format!(
                                "method call requires a struct type, found '{}'",
                                recv_ty
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                };

                let methods = self.methods.get(&type_name).cloned().unwrap_or_default();
                let method = methods.iter().find(|m| {
                    // Extract method name from mangled name: TypeName_method
                    let prefix = format!("{}_", type_name);
                    m.mangled_name.strip_prefix(&prefix) == Some(method_name)
                });

                if let Some(method) = method {
                    let sig = method.sig.clone();
                    let mangled = method.mangled_name.clone();
                    // Enforce purity: pure functions cannot call impure methods
                    if self.current_fn_is_pure && !sig.is_pure {
                        self.errors.push(TypeError {
                            message: format!(
                                "pure function cannot call impure method '{}'",
                                method_name
                            ),
                            span: expr.span,
                        });
                    }
                    self.check_call_effects(&mangled, expr.span);
                    // sig.params includes the self param; external args exclude self
                    let expected_args = sig.params.len() - 1;
                    if args.len() != expected_args {
                        self.errors.push(TypeError {
                            message: format!(
                                "method '{}' expects {} arguments, found {}",
                                method_name,
                                expected_args,
                                args.len()
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(arg_ty) = self.infer_expr(arg) {
                            if arg_ty != sig.params[i + 1] {
                                self.errors.push(TypeError {
                                    message: format!(
                                        "argument {} of '{}': expected '{}', found '{}'",
                                        i + 1,
                                        method_name,
                                        sig.params[i + 1],
                                        arg_ty
                                    ),
                                    span: arg.span,
                                });
                            }
                        }
                    }
                    if self.collect_symbols {
                        self.symbol_table.references.push(SymbolRef {
                            span: expr.span,
                            resolved_type: format!("{}", sig.ret),
                            def_name: mangled.clone(),
                            def_span: Some(sig.span),
                        });
                    }
                    Some(sig.ret)
                } else {
                    self.errors.push(TypeError {
                        message: format!(
                            "no method '{}' found for type '{}'",
                            method_name, type_name
                        ),
                        span: expr.span,
                    });
                    None
                }
            }

            ExprKind::FieldAccess(obj, field) => {
                let obj_ty = self.infer_expr(obj)?;
                // Auto-deref through references
                let obj_ty = match &obj_ty {
                    Type::Ref(inner) | Type::MutRef(inner) => inner.as_ref().clone(),
                    other => other.clone(),
                };
                // Tuple field access: t.0, t.1, ...
                if let Type::Tuple(ref types) = obj_ty {
                    if let Ok(idx) = field.parse::<usize>() {
                        if idx < types.len() {
                            if self.collect_symbols {
                                self.symbol_table.references.push(SymbolRef {
                                    span: expr.span,
                                    resolved_type: format!("{}", types[idx]),
                                    def_name: field.clone(),
                                    def_span: None,
                                });
                            }
                            return Some(types[idx].clone());
                        } else {
                            self.errors.push(TypeError {
                                message: format!(
                                    "tuple index {} out of bounds (tuple has {} elements)",
                                    idx,
                                    types.len()
                                ),
                                span: expr.span,
                            });
                            return None;
                        }
                    } else {
                        self.errors.push(TypeError {
                            message: format!(
                                "tuple fields must be numeric indices, found '{}'",
                                field
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                }
                // Extract struct name and optional type args
                let (struct_name, type_args) = match &obj_ty {
                    Type::Named(name) => (name.clone(), None),
                    Type::Generic(name, args) => (name.clone(), Some(args.clone())),
                    _ => {
                        self.errors.push(TypeError {
                            message: format!(
                                "field access requires a struct or tuple type, found '{}'",
                                obj_ty
                            ),
                            span: expr.span,
                        });
                        return None;
                    }
                };
                if let Some(info) = self.structs.get(&struct_name).cloned() {
                    if let Some((_, fty)) = info.fields.iter().find(|(n, _)| n == field) {
                        // If we have type args, substitute type params in the field type
                        let resolved = if let Some(args) = type_args {
                            let subst: HashMap<String, Type> = info
                                .type_params
                                .iter()
                                .zip(args.iter())
                                .map(|(p, a)| (p.clone(), a.clone()))
                                .collect();
                            self.substitute_type_vars(fty, &subst)
                        } else {
                            fty.clone()
                        };
                        if self.collect_symbols {
                            self.symbol_table.references.push(SymbolRef {
                                span: expr.span,
                                resolved_type: format!("{}", resolved),
                                def_name: format!("{}.{}", struct_name, field),
                                def_span: None,
                            });
                        }
                        Some(resolved)
                    } else {
                        self.errors.push(TypeError {
                            message: format!("struct '{}' has no field '{}'", struct_name, field),
                            span: expr.span,
                        });
                        None
                    }
                } else {
                    self.errors.push(TypeError {
                        message: format!("'{}' is not a struct type", struct_name),
                        span: expr.span,
                    });
                    None
                }
            }

            ExprKind::Index(arr, idx) => {
                let arr_ty = self.infer_expr(arr)?;
                let idx_ty = self.infer_expr(idx)?;
                if idx_ty != Type::Int {
                    self.errors.push(TypeError {
                        message: format!("index must be 'int', found '{}'", idx_ty),
                        span: idx.span,
                    });
                }
                if let Type::Array(inner) = arr_ty {
                    Some(*inner)
                } else {
                    self.errors.push(TypeError {
                        message: format!("cannot index into type '{}'", arr_ty),
                        span: expr.span,
                    });
                    None
                }
            }

            ExprKind::Closure(closure) => {
                self.push_scope();
                for param in &closure.params {
                    if !self.is_valid_type(&param.ty) {
                        self.errors.push(TypeError {
                            message: format!("unknown type '{}'", param.ty),
                            span: param.span,
                        });
                    }
                    self.define_with_span(&param.name, param.ty.clone(), false, param.span);
                    if self.collect_symbols {
                        self.symbol_table.definitions.push(SymbolDef {
                            name: param.name.clone(),
                            kind: SymbolKind::Parameter,
                            type_desc: format!("{}: {}", param.name, param.ty),
                            span: param.span,
                        });
                    }
                }
                let prev_ret = self.current_fn_ret.clone();
                self.current_fn_ret = Some(closure.return_type.clone());
                self.check_block(&closure.body);
                self.current_fn_ret = prev_ret;
                self.pop_scope();

                let param_types: Vec<Type> = closure.params.iter().map(|p| p.ty.clone()).collect();
                Some(Type::Fn(param_types, Box::new(closure.return_type.clone())))
            }

            ExprKind::ArrayLit(elements) => {
                if elements.is_empty() {
                    self.errors.push(TypeError {
                        message: "cannot infer type of empty array literal".to_string(),
                        span: expr.span,
                    });
                    return None;
                }
                let first_ty = self.infer_expr(&elements[0])?;
                for (i, elem) in elements.iter().enumerate().skip(1) {
                    if let Some(elem_ty) = self.infer_expr(elem) {
                        if elem_ty != first_ty {
                            self.errors.push(TypeError {
                                message: format!(
                                    "array element {} has type '{}', expected '{}'",
                                    i, elem_ty, first_ty
                                ),
                                span: elem.span,
                            });
                        }
                    }
                }
                Some(Type::Array(Box::new(first_ty)))
            }

            ExprKind::ArrayRepeat(value_expr, count_expr) => {
                let value_ty = self.infer_expr(value_expr)?;
                if let Some(count_ty) = self.infer_expr(count_expr) {
                    if count_ty != Type::Int {
                        self.errors.push(TypeError {
                            message: format!(
                                "array repeat count must be 'int', found '{}'",
                                count_ty
                            ),
                            span: count_expr.span,
                        });
                    }
                }
                Some(Type::Array(Box::new(value_ty)))
            }

            ExprKind::TupleLit(elements) => {
                if elements.is_empty() {
                    return Some(Type::Unit);
                }
                let mut elem_types = Vec::new();
                for elem in elements {
                    if let Some(ty) = self.infer_expr(elem) {
                        elem_types.push(ty);
                    } else {
                        return None;
                    }
                }
                Some(Type::Tuple(elem_types))
            }

            ExprKind::Spawn(block) => {
                // Spawn block: push scope, check block, infer return type
                if self.current_fn_is_pure {
                    self.errors.push(TypeError {
                        message: "pure function cannot use spawn".to_string(),
                        span: expr.span,
                    });
                }
                self.check_call_effects("spawn", expr.span);
                self.push_scope();
                self.check_block(block);
                // Infer the return type from the last return statement in the block
                let inner_ty = self.infer_spawn_return_type(block);
                self.pop_scope();
                Some(Type::Task(Box::new(inner_ty)))
            }

            ExprKind::Range(start, end) | ExprKind::RangeInclusive(start, end) => {
                if let Some(start_ty) = self.infer_expr(start) {
                    if start_ty != Type::Int {
                        self.errors.push(TypeError {
                            message: format!("range start must be 'int', found '{}'", start_ty),
                            span: start.span,
                        });
                    }
                }
                if let Some(end_ty) = self.infer_expr(end) {
                    if end_ty != Type::Int {
                        self.errors.push(TypeError {
                            message: format!("range end must be 'int', found '{}'", end_ty),
                            span: end.span,
                        });
                    }
                }
                // Range is not a standalone value — only valid in for loops
                self.errors.push(TypeError {
                    message: "range expression is only valid in for loops".to_string(),
                    span: expr.span,
                });
                None
            }

            ExprKind::Try(inner) => {
                // ? operator: inner must be Option<T> or Result<T, E>
                // Enclosing function must return compatible Option/Result
                let inner_ty = self.infer_expr(inner);
                match inner_ty {
                    Some(Type::Generic(ref name, ref args))
                        if name == "Option" && args.len() == 1 =>
                    {
                        // Option<T>? → T, but enclosing fn must return Option<_>
                        if let Some(ref ret) = self.current_fn_ret {
                            match ret {
                                Type::Generic(rn, _) if rn == "Option" => {}
                                _ => {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "'?' on Option<{}> requires function to return Option, found '{}'",
                                            args[0], ret
                                        ),
                                        span: expr.span,
                                    });
                                }
                            }
                        }
                        Some(args[0].clone())
                    }
                    Some(Type::Generic(ref name, ref args))
                        if name == "Result" && args.len() == 2 =>
                    {
                        // Result<T, E>? → T, but enclosing fn must return Result<_, E>
                        if let Some(ref ret) = self.current_fn_ret {
                            match ret {
                                Type::Generic(rn, rargs) if rn == "Result" && rargs.len() == 2 => {
                                    // E types must match
                                    if rargs[1] != args[1]
                                        && !matches!(rargs[1], Type::TypeVar(_))
                                        && !matches!(args[1], Type::TypeVar(_))
                                    {
                                        self.errors.push(TypeError {
                                            message: format!(
                                                "'?' error type mismatch: expression has Result<{}, {}> but function returns Result<{}, {}>",
                                                args[0], args[1], rargs[0], rargs[1]
                                            ),
                                            span: expr.span,
                                        });
                                    }
                                }
                                _ => {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "'?' on Result<{}, {}> requires function to return Result, found '{}'",
                                            args[0], args[1], ret
                                        ),
                                        span: expr.span,
                                    });
                                }
                            }
                        }
                        Some(args[0].clone())
                    }
                    Some(ty) => {
                        self.errors.push(TypeError {
                            message: format!(
                                "'?' operator requires Option or Result type, found '{}'",
                                ty
                            ),
                            span: expr.span,
                        });
                        None
                    }
                    None => None,
                }
            }

            ExprKind::StructInit(name, fields) => {
                if let Some(info) = self.structs.get(name).cloned() {
                    if self.collect_symbols {
                        let def_span = self
                            .symbol_table
                            .definitions
                            .iter()
                            .find(|d| d.name == *name && matches!(d.kind, SymbolKind::Struct))
                            .map(|d| d.span);
                        self.symbol_table.references.push(SymbolRef {
                            span: expr.span,
                            resolved_type: name.clone(),
                            def_name: name.clone(),
                            def_span,
                        });
                    }
                    // Check that all fields are provided
                    for (expected_name, expected_ty) in &info.fields {
                        if let Some(fi) = fields.iter().find(|f| &f.name == expected_name) {
                            if let Some(actual_ty) = self.infer_expr(&fi.value) {
                                // Skip type check if the expected type is a type parameter
                                let is_type_param = if let Type::Named(ref tname) = expected_ty {
                                    info.type_params.contains(tname)
                                } else {
                                    false
                                };
                                if !is_type_param && actual_ty != *expected_ty {
                                    self.errors.push(TypeError {
                                        message: format!(
                                            "field '{}' of '{}': expected '{}', found '{}'",
                                            expected_name, name, expected_ty, actual_ty
                                        ),
                                        span: fi.span,
                                    });
                                }
                            }
                        } else {
                            self.errors.push(TypeError {
                                message: format!(
                                    "missing field '{}' in struct '{}' initializer",
                                    expected_name, name
                                ),
                                span: expr.span,
                            });
                        }
                    }
                    // Check for extra fields
                    for fi in fields {
                        if !info.fields.iter().any(|(n, _)| n == &fi.name) {
                            self.errors.push(TypeError {
                                message: format!(
                                    "unknown field '{}' in struct '{}'",
                                    fi.name, name
                                ),
                                span: fi.span,
                            });
                        }
                    }
                    Some(Type::Named(name.clone()))
                } else {
                    self.errors.push(TypeError {
                        message: format!("undefined struct '{}'", name),
                        span: expr.span,
                    });
                    None
                }
            }
        }
    }

    fn check_binary_op(&mut self, op: BinOp, lt: &Type, rt: &Type, span: Span) -> Option<Type> {
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if lt != rt {
                    self.errors.push(TypeError {
                        message: format!(
                            "operands of '{}' must have same type, found '{}' and '{}'",
                            op, lt, rt
                        ),
                        span,
                    });
                    return None;
                }
                if *lt != Type::Int && *lt != Type::Float {
                    self.errors.push(TypeError {
                        message: format!(
                            "arithmetic operator '{}' requires 'int' or 'float', found '{}'",
                            op, lt
                        ),
                        span,
                    });
                    return None;
                }
                Some(lt.clone())
            }
            BinOp::Eq | BinOp::NotEq => {
                if lt != rt {
                    self.errors.push(TypeError {
                        message: format!("cannot compare '{}' and '{}' for equality", lt, rt),
                        span,
                    });
                    return None;
                }
                Some(Type::Bool)
            }
            BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                if lt != rt {
                    self.errors.push(TypeError {
                        message: format!("cannot compare '{}' and '{}'", lt, rt),
                        span,
                    });
                    return None;
                }
                if *lt != Type::Int && *lt != Type::Float && *lt != Type::Char {
                    self.errors.push(TypeError {
                        message: format!(
                            "comparison requires 'int', 'float', or 'char', found '{}'",
                            lt
                        ),
                        span,
                    });
                    return None;
                }
                Some(Type::Bool)
            }
            BinOp::And | BinOp::Or => {
                if *lt != Type::Bool || *rt != Type::Bool {
                    self.errors.push(TypeError {
                        message: format!(
                            "'{}' requires 'bool' operands, found '{}' and '{}'",
                            op, lt, rt
                        ),
                        span,
                    });
                    return None;
                }
                Some(Type::Bool)
            }
            BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::Shl | BinOp::Shr => {
                if *lt != Type::Int || *rt != Type::Int {
                    self.errors.push(TypeError {
                        message: format!(
                            "bitwise operator '{}' requires 'int' operands, found '{}' and '{}'",
                            op, lt, rt
                        ),
                        span,
                    });
                    return None;
                }
                Some(Type::Int)
            }
        }
    }

    // ── Scope management ─────────────────────────────────────

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &str, ty: Type, is_mut: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name.to_string(),
                VarInfo {
                    ty,
                    is_mut,
                    def_span: Span::synthetic(),
                },
            );
        }
    }

    fn define_with_span(&mut self, name: &str, ty: Type, is_mut: bool, span: Span) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name.to_string(),
                VarInfo {
                    ty,
                    is_mut,
                    def_span: span,
                },
            );
        }
    }

    fn lookup(&self, name: &str) -> Option<&VarInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }

    fn is_type_param(&self, name: &str) -> bool {
        for scope in self.type_param_scope.iter().rev() {
            if scope.contains(name) {
                return true;
            }
        }
        false
    }

    fn substitute_type_vars(&self, ty: &Type, bindings: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Named(name) if bindings.contains_key(name) => bindings[name].clone(),
            Type::TypeVar(name) if bindings.contains_key(name) => bindings[name].clone(),
            Type::Ref(inner) => Type::Ref(Box::new(self.substitute_type_vars(inner, bindings))),
            Type::MutRef(inner) => {
                Type::MutRef(Box::new(self.substitute_type_vars(inner, bindings)))
            }
            Type::Array(inner) => Type::Array(Box::new(self.substitute_type_vars(inner, bindings))),
            Type::Fn(params, ret) => Type::Fn(
                params
                    .iter()
                    .map(|p| self.substitute_type_vars(p, bindings))
                    .collect(),
                Box::new(self.substitute_type_vars(ret, bindings)),
            ),
            Type::Task(inner) => Type::Task(Box::new(self.substitute_type_vars(inner, bindings))),
            Type::Chan(inner) => Type::Chan(Box::new(self.substitute_type_vars(inner, bindings))),
            Type::Generic(name, args) => Type::Generic(
                name.clone(),
                args.iter()
                    .map(|t| self.substitute_type_vars(t, bindings))
                    .collect(),
            ),
            Type::Tuple(elems) => Type::Tuple(
                elems
                    .iter()
                    .map(|t| self.substitute_type_vars(t, bindings))
                    .collect(),
            ),
            other => other.clone(),
        }
    }

    fn infer_type_vars_from_expected(
        &self,
        generic: &Type,
        concrete: &Type,
        type_params: &[String],
        bindings: &mut HashMap<String, Type>,
    ) {
        match (generic, concrete) {
            (Type::Named(name), ty) => {
                if type_params.contains(name) {
                    bindings.entry(name.clone()).or_insert_with(|| ty.clone());
                }
            }
            (Type::TypeVar(name), ty) => {
                if type_params.contains(name) {
                    bindings.entry(name.clone()).or_insert_with(|| ty.clone());
                }
            }
            (Type::Generic(g_name, g_args), Type::Generic(c_name, c_args)) => {
                if g_name == c_name && g_args.len() == c_args.len() {
                    for (g, c) in g_args.iter().zip(c_args.iter()) {
                        self.infer_type_vars_from_expected(g, c, type_params, bindings);
                    }
                }
            }
            (Type::Array(g_inner), Type::Array(c_inner)) => {
                self.infer_type_vars_from_expected(g_inner, c_inner, type_params, bindings);
            }
            (Type::Tuple(g_elems), Type::Tuple(c_elems)) => {
                if g_elems.len() == c_elems.len() {
                    for (g, c) in g_elems.iter().zip(c_elems.iter()) {
                        self.infer_type_vars_from_expected(g, c, type_params, bindings);
                    }
                }
            }
            (Type::Ref(g), Type::Ref(c))
            | (Type::MutRef(g), Type::MutRef(c))
            | (Type::Own(g), Type::Own(c))
            | (Type::Task(g), Type::Task(c))
            | (Type::Chan(g), Type::Chan(c)) => {
                self.infer_type_vars_from_expected(g, c, type_params, bindings);
            }
            _ => {}
        }
    }

    // ── Effect inference ──────────────────────────────────────

    /// Collect all function call names from a block (lightweight AST walk).
    fn collect_calls_in_block(block: &Block, out: &mut Vec<String>) {
        for stmt in &block.stmts {
            Self::collect_calls_in_stmt(stmt, out);
        }
    }

    fn collect_calls_in_stmt(stmt: &Stmt, out: &mut Vec<String>) {
        match stmt {
            Stmt::Let(s) => Self::collect_calls_in_expr(&s.value, out),
            Stmt::Assign(s) => {
                Self::collect_calls_in_expr(&s.target, out);
                Self::collect_calls_in_expr(&s.value, out);
            }
            Stmt::Return(s) => Self::collect_calls_in_expr(&s.value, out),
            Stmt::If(s) => {
                Self::collect_calls_in_expr(&s.condition, out);
                Self::collect_calls_in_block(&s.then_block, out);
                if let Some(ref else_branch) = s.else_branch {
                    match else_branch.as_ref() {
                        ElseBranch::Else(block) => Self::collect_calls_in_block(block, out),
                        ElseBranch::ElseIf(if_stmt) => {
                            // Wrap in a synthetic Stmt::If and recurse
                            Self::collect_calls_in_expr(&if_stmt.condition, out);
                            Self::collect_calls_in_block(&if_stmt.then_block, out);
                            if let Some(ref inner) = if_stmt.else_branch {
                                match inner.as_ref() {
                                    ElseBranch::Else(block) => {
                                        Self::collect_calls_in_block(block, out)
                                    }
                                    ElseBranch::ElseIf(inner_if) => {
                                        let stmt = Stmt::If(inner_if.clone());
                                        Self::collect_calls_in_stmt(&stmt, out);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Stmt::While(s) => {
                Self::collect_calls_in_expr(&s.condition, out);
                Self::collect_calls_in_block(&s.body, out);
            }
            Stmt::For(s) => {
                Self::collect_calls_in_expr(&s.iterable, out);
                Self::collect_calls_in_block(&s.body, out);
            }
            Stmt::Match(s) => {
                Self::collect_calls_in_expr(&s.subject, out);
                for arm in &s.arms {
                    Self::collect_calls_in_block(&arm.body, out);
                }
            }
            Stmt::Expr(s) => Self::collect_calls_in_expr(&s.expr, out),
            Stmt::Break(_) | Stmt::Continue(_) => {}
        }
    }

    fn collect_calls_in_expr(expr: &Expr, out: &mut Vec<String>) {
        match &expr.kind {
            ExprKind::Call(callee, args) => {
                if let ExprKind::Ident(name) = &callee.kind {
                    out.push(name.clone());
                }
                Self::collect_calls_in_expr(callee, out);
                for arg in args {
                    Self::collect_calls_in_expr(arg, out);
                }
            }
            ExprKind::MethodCall(obj, method, args) => {
                // Record method call with prefix for resolution during inference
                out.push(format!("__method__{}", method));
                Self::collect_calls_in_expr(obj, out);
                for arg in args {
                    Self::collect_calls_in_expr(arg, out);
                }
            }
            ExprKind::Binary(lhs, _, rhs) => {
                Self::collect_calls_in_expr(lhs, out);
                Self::collect_calls_in_expr(rhs, out);
            }
            ExprKind::Unary(_, inner) => Self::collect_calls_in_expr(inner, out),
            ExprKind::Index(arr, idx) => {
                Self::collect_calls_in_expr(arr, out);
                Self::collect_calls_in_expr(idx, out);
            }
            ExprKind::FieldAccess(obj, _) => Self::collect_calls_in_expr(obj, out),
            ExprKind::StructInit(_, fields) => {
                for f in fields {
                    Self::collect_calls_in_expr(&f.value, out);
                }
            }
            ExprKind::ArrayLit(elems) | ExprKind::TupleLit(elems) => {
                for e in elems {
                    Self::collect_calls_in_expr(e, out);
                }
            }
            ExprKind::ArrayRepeat(val, count) => {
                Self::collect_calls_in_expr(val, out);
                Self::collect_calls_in_expr(count, out);
            }
            ExprKind::Closure(c) => Self::collect_calls_in_block(&c.body, out),
            ExprKind::Spawn(block) => {
                out.push("spawn".to_string());
                Self::collect_calls_in_block(block, out);
            }
            ExprKind::Range(start, end) | ExprKind::RangeInclusive(start, end) => {
                Self::collect_calls_in_expr(start, out);
                Self::collect_calls_in_expr(end, out);
            }
            ExprKind::Try(inner) => Self::collect_calls_in_expr(inner, out),
            ExprKind::Literal(_) | ExprKind::Ident(_) => {}
        }
    }

    /// Infer effects for all functions without explicit `effects` clause.
    /// Uses fixed-point iteration over the call graph.
    fn infer_all_effects(&mut self, program: &Program) {
        // Build fn_bodies: name -> &Block
        let mut fn_bodies: HashMap<String, &Block> = HashMap::new();
        for decl in &program.declarations {
            match decl {
                Declaration::Function(f) => {
                    fn_bodies.insert(f.name.clone(), &f.body);
                }
                Declaration::Impl(i) => {
                    for method in &i.methods {
                        let mangled = format!("{}_{}", i.target_type, method.name);
                        fn_bodies.insert(mangled, &method.body);
                    }
                }
                _ => {}
            }
        }

        // Build call graph: fn_name -> set of called function names
        let mut call_graph: HashMap<String, Vec<String>> = HashMap::new();
        for (name, body) in &fn_bodies {
            let mut calls = Vec::new();
            Self::collect_calls_in_block(body, &mut calls);
            call_graph.insert(name.clone(), calls);
        }

        // Initialize effects from declared/builtin sources
        let mut effects: HashMap<String, HashSet<String>> = HashMap::new();

        // Seed with builtin effects
        for name in call_graph.keys() {
            if let Some(sig) = self.functions.get(name) {
                if let Some(ref declared) = sig.effects {
                    effects.insert(name.clone(), declared.iter().cloned().collect());
                    continue;
                }
            }
            // For functions without declared effects, start with empty set
            effects.entry(name.clone()).or_default();
        }

        // Fixed-point iteration
        let mut changed = true;
        while changed {
            changed = false;
            for (fn_name, calls) in &call_graph {
                // Skip functions with declared effects
                if let Some(sig) = self.functions.get(fn_name) {
                    if sig.effects.is_some() {
                        continue;
                    }
                }

                let mut new_effects: HashSet<String> =
                    effects.get(fn_name).cloned().unwrap_or_default();

                for callee in calls {
                    // Resolve __method__ prefixed names to mangled function names
                    let callee_effs: HashSet<String> =
                        if let Some(method_name) = callee.strip_prefix("__method__") {
                            // Find all mangled methods matching *_<method_name>
                            let suffix = format!("_{}", method_name);
                            let mut combined = HashSet::new();
                            for (mangled, sig) in &self.functions {
                                if mangled.ends_with(&suffix) {
                                    if let Some(ref declared) = sig.effects {
                                        combined.extend(declared.iter().cloned());
                                    } else if let Some(e) = effects.get(mangled) {
                                        combined.extend(e.iter().cloned());
                                    }
                                }
                            }
                            combined
                        } else if let Some(e) = effects.get(callee) {
                            e.clone()
                        } else if let Some(be) = builtin_effects(callee) {
                            be.iter().map(|s| s.to_string()).collect()
                        } else if let Some(sig) = self.functions.get(callee) {
                            sig.effects
                                .as_ref()
                                .map(|e| e.iter().cloned().collect())
                                .unwrap_or_default()
                        } else {
                            HashSet::new()
                        };

                    for eff in callee_effs {
                        if new_effects.insert(eff) {
                            changed = true;
                        }
                    }
                }

                effects.insert(fn_name.clone(), new_effects);
            }
        }

        // Store inferred effects (only for functions without declared effects)
        for (name, effs) in effects {
            if let Some(sig) = self.functions.get(&name) {
                if sig.effects.is_some() {
                    continue;
                }
            }
            if !effs.is_empty() {
                self.inferred_effects
                    .insert(name, effs.into_iter().collect());
            }
        }
    }

    /// Check if calling `callee_name` is allowed under the current function's effect constraints.
    fn check_call_effects(&mut self, callee_name: &str, span: Span) {
        if self.current_fn_is_pure {
            return; // purity system handles pure fns
        }
        let allowed = match &self.current_fn_effects {
            None => return, // unchecked
            Some(effs) => effs.clone(),
        };

        // Look up callee effects: FnSig.effects > inferred > builtin_effects()
        let callee_effects: Vec<String> = if let Some(sig) = self.functions.get(callee_name) {
            match &sig.effects {
                Some(effs) => effs.clone(),
                None => {
                    // Check inferred effects for unchecked user functions
                    self.inferred_effects
                        .get(callee_name)
                        .cloned()
                        .unwrap_or_default()
                }
            }
        } else if let Some(effs) = builtin_effects(callee_name) {
            effs.iter().map(|s| s.to_string()).collect()
        } else {
            // Unknown function — no effect info available, allow
            return;
        };

        for eff in &callee_effects {
            if !allowed.contains(eff) {
                self.errors.push(TypeError {
                    message: format!(
                        "function requires '{}' effect to call '{}', but it is not declared",
                        eff, callee_name
                    ),
                    span,
                });
            }
        }
    }

    fn is_valid_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Int | Type::Float | Type::Bool | Type::Char | Type::Str | Type::Unit => true,
            Type::Named(name) => {
                self.structs.contains_key(name)
                    || self.enums.contains_key(name)
                    || self.is_type_param(name)
            }
            Type::Array(inner) => self.is_valid_type(inner),
            Type::Ref(inner) | Type::MutRef(inner) | Type::Own(inner) => self.is_valid_type(inner),
            Type::SelfType => self.current_self_type.is_some(),
            Type::TypeVar(_) => true,
            Type::Generic(name, _) => {
                self.structs.contains_key(name)
                    || self.enums.contains_key(name)
                    || name == "Map"
                    || name == "Set"
            }
            Type::Fn(_, _) => true,
            Type::Task(inner) => self.is_valid_type(inner),
            Type::Chan(inner) => self.is_valid_type(inner),
            Type::Tuple(types) => types.iter().all(|t| self.is_valid_type(t)),
        }
    }

    fn is_hashable_key(&self, ty: &Type) -> bool {
        matches!(ty, Type::Int | Type::Str | Type::Char | Type::Bool)
    }

    fn map_kv_types(&self, map_ty: &Type) -> Option<(Type, Type)> {
        match map_ty {
            Type::Generic(name, args) if name == "Map" && args.len() == 2 => {
                Some((args[0].clone(), args[1].clone()))
            }
            _ => None,
        }
    }

    fn set_elem_type(&self, set_ty: &Type) -> Option<Type> {
        match set_ty {
            Type::Generic(name, args) if name == "Set" && args.len() == 1 => Some(args[0].clone()),
            _ => None,
        }
    }

    fn infer_spawn_return_type(&self, block: &Block) -> Type {
        for stmt in block.stmts.iter().rev() {
            if let Stmt::Return(ret) = stmt {
                // Try to infer from the return expression
                if let Some(ty) = self.infer_expr_type_simple(&ret.value) {
                    return ty;
                }
            }
        }
        Type::Unit
    }

    /// Simple expression type inference without mutation (no error pushing).
    fn infer_expr_type_simple(&self, expr: &Expr) -> Option<Type> {
        match &expr.kind {
            ExprKind::Literal(lit) => Some(match lit {
                Literal::Int(_) => Type::Int,
                Literal::Float(_) => Type::Float,
                Literal::Bool(_) => Type::Bool,
                Literal::Char(_) => Type::Char,
                Literal::String(_) => Type::Str,
                Literal::Unit => Type::Unit,
            }),
            ExprKind::Ident(name) => self.lookup(name).map(|info| info.ty.clone()),
            ExprKind::Binary(_, op, _) => match op {
                BinOp::Eq
                | BinOp::NotEq
                | BinOp::Lt
                | BinOp::Gt
                | BinOp::LtEq
                | BinOp::GtEq
                | BinOp::And
                | BinOp::Or => Some(Type::Bool),
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                    // Infer from LHS
                    if let ExprKind::Literal(Literal::Float(_)) = &expr.kind {
                        Some(Type::Float)
                    } else {
                        Some(Type::Int)
                    }
                }
                BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::Shl | BinOp::Shr => {
                    Some(Type::Int)
                }
            },
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::lexer::Lexer;
    use crate::compiler::parser::Parser;

    fn check(input: &str) -> Result<(), Vec<TypeError>> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();
        let mut checker = TypeChecker::new();
        checker.check_program(&program)
    }

    #[test]
    fn test_valid_program() {
        assert!(check("fn main() -> int { return 0; }").is_ok());
    }

    #[test]
    fn test_type_mismatch() {
        let result = check("fn f() -> int { return true; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_undefined_variable() {
        let result = check("fn f() -> int { return x; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_arithmetic_type_check() {
        assert!(check("fn f() -> int { let x: int = 1 + 2; return x; }").is_ok());
    }

    #[test]
    fn test_bool_in_arithmetic() {
        let result = check("fn f() -> int { return true + 1; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_immutable_assign() {
        let result = check("fn f() -> int { let x: int = 1; x = 2; return x; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_mutable_assign() {
        assert!(check("fn f() -> int { let mut x: int = 1; x = 2; return x; }").is_ok());
    }

    #[test]
    fn test_struct_type_check() {
        assert!(check(
            "struct Point { x: int, y: int } fn f() -> Point { return Point { x: 1, y: 2 }; }"
        )
        .is_ok());
    }
}
