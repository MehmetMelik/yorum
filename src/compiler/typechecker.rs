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

    fn register_builtins(&mut self) {
        let builtin = |params: Vec<Type>, ret: Type, is_pure: bool| FnSig {
            params,
            ret,
            is_pure,
            type_params: Vec::new(),
            span: Span::synthetic(),
        };
        self.functions.insert(
            "print_int".to_string(),
            builtin(vec![Type::Int], Type::Unit, false),
        );
        self.functions.insert(
            "print_float".to_string(),
            builtin(vec![Type::Float], Type::Unit, false),
        );
        self.functions.insert(
            "print_bool".to_string(),
            builtin(vec![Type::Bool], Type::Unit, false),
        );
        self.functions.insert(
            "print_str".to_string(),
            builtin(vec![Type::Str], Type::Unit, false),
        );
        self.functions.insert(
            "str_len".to_string(),
            builtin(vec![Type::Str], Type::Int, true),
        );
        self.functions.insert(
            "str_concat".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Str, false),
        );
        self.functions.insert(
            "str_eq".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, true),
        );
        self.functions.insert(
            "print_char".to_string(),
            builtin(vec![Type::Char], Type::Unit, false),
        );
        // Type casting builtins
        self.functions.insert(
            "char_to_int".to_string(),
            builtin(vec![Type::Char], Type::Int, true),
        );
        self.functions.insert(
            "int_to_char".to_string(),
            builtin(vec![Type::Int], Type::Char, true),
        );
        self.functions.insert(
            "int_to_float".to_string(),
            builtin(vec![Type::Int], Type::Float, true),
        );
        self.functions.insert(
            "float_to_int".to_string(),
            builtin(vec![Type::Float], Type::Int, true),
        );
        self.functions.insert(
            "int_to_str".to_string(),
            builtin(vec![Type::Int], Type::Str, true),
        );
        self.functions.insert(
            "str_to_int".to_string(),
            builtin(vec![Type::Str], Type::Int, true),
        );
        // String/char operation builtins
        self.functions.insert(
            "str_charAt".to_string(),
            builtin(vec![Type::Str, Type::Int], Type::Char, true),
        );
        self.functions.insert(
            "str_sub".to_string(),
            builtin(vec![Type::Str, Type::Int, Type::Int], Type::Str, false),
        );
        self.functions.insert(
            "str_from_char".to_string(),
            builtin(vec![Type::Char], Type::Str, false),
        );
        self.functions.insert(
            "char_is_alpha".to_string(),
            builtin(vec![Type::Char], Type::Bool, true),
        );
        self.functions.insert(
            "char_is_digit".to_string(),
            builtin(vec![Type::Char], Type::Bool, true),
        );
        self.functions.insert(
            "char_is_whitespace".to_string(),
            builtin(vec![Type::Char], Type::Bool, true),
        );
        // File I/O builtins
        self.functions.insert(
            "file_read".to_string(),
            builtin(vec![Type::Str], Type::Str, false),
        );
        self.functions.insert(
            "file_write".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, false),
        );
        self.functions.insert(
            "print_err".to_string(),
            builtin(vec![Type::Str], Type::Unit, false),
        );
        // Process interaction builtins
        self.functions.insert(
            "exit".to_string(),
            builtin(vec![Type::Int], Type::Unit, false),
        );
        // HashMap builtins
        self.functions
            .insert("map_new".to_string(), builtin(vec![], Type::Map, false));
        self.functions.insert(
            "map_set".to_string(),
            builtin(vec![Type::Map, Type::Str, Type::Int], Type::Unit, false),
        );
        self.functions.insert(
            "map_get".to_string(),
            builtin(vec![Type::Map, Type::Str], Type::Int, true),
        );
        self.functions.insert(
            "map_has".to_string(),
            builtin(vec![Type::Map, Type::Str], Type::Bool, true),
        );
        // Math builtins
        self.functions.insert(
            "abs_int".to_string(),
            builtin(vec![Type::Int], Type::Int, true),
        );
        self.functions.insert(
            "abs_float".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "min_int".to_string(),
            builtin(vec![Type::Int, Type::Int], Type::Int, true),
        );
        self.functions.insert(
            "max_int".to_string(),
            builtin(vec![Type::Int, Type::Int], Type::Int, true),
        );
        self.functions.insert(
            "min_float".to_string(),
            builtin(vec![Type::Float, Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "max_float".to_string(),
            builtin(vec![Type::Float, Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "sqrt".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "pow".to_string(),
            builtin(vec![Type::Float, Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "sin".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "cos".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "tan".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "floor".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "ceil".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "round".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "log".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "log10".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        self.functions.insert(
            "exp".to_string(),
            builtin(vec![Type::Float], Type::Float, true),
        );
        // String utility builtins
        self.functions.insert(
            "str_contains".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, true),
        );
        self.functions.insert(
            "str_index_of".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Int, true),
        );
        self.functions.insert(
            "str_starts_with".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, true),
        );
        self.functions.insert(
            "str_ends_with".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, true),
        );
        self.functions.insert(
            "str_trim".to_string(),
            builtin(vec![Type::Str], Type::Str, false),
        );
        self.functions.insert(
            "str_replace".to_string(),
            builtin(vec![Type::Str, Type::Str, Type::Str], Type::Str, false),
        );
        self.functions.insert(
            "str_to_upper".to_string(),
            builtin(vec![Type::Str], Type::Str, false),
        );
        self.functions.insert(
            "str_to_lower".to_string(),
            builtin(vec![Type::Str], Type::Str, false),
        );
        self.functions.insert(
            "str_repeat".to_string(),
            builtin(vec![Type::Str, Type::Int], Type::Str, false),
        );
        // Collection utility builtins (type-specific)
        self.functions.insert(
            "contains_int".to_string(),
            builtin(
                vec![Type::Array(Box::new(Type::Int)), Type::Int],
                Type::Bool,
                true,
            ),
        );
        self.functions.insert(
            "contains_str".to_string(),
            builtin(
                vec![Type::Array(Box::new(Type::Str)), Type::Str],
                Type::Bool,
                true,
            ),
        );
        self.functions.insert(
            "sort_int".to_string(),
            builtin(
                vec![Type::Array(Box::new(Type::Int))],
                Type::Array(Box::new(Type::Int)),
                false,
            ),
        );
        self.functions.insert(
            "sort_str".to_string(),
            builtin(
                vec![Type::Array(Box::new(Type::Str))],
                Type::Array(Box::new(Type::Str)),
                false,
            ),
        );
        // Map utility builtins
        self.functions.insert(
            "map_size".to_string(),
            builtin(vec![Type::Map], Type::Int, true),
        );
        self.functions.insert(
            "map_remove".to_string(),
            builtin(vec![Type::Map, Type::Str], Type::Bool, false),
        );
        self.functions.insert(
            "map_keys".to_string(),
            builtin(vec![Type::Map], Type::Array(Box::new(Type::Str)), false),
        );
        self.functions.insert(
            "map_values".to_string(),
            builtin(vec![Type::Map], Type::Array(Box::new(Type::Int)), false),
        );
        // Enhanced I/O builtins
        self.functions.insert(
            "file_exists".to_string(),
            builtin(vec![Type::Str], Type::Bool, false),
        );
        self.functions.insert(
            "file_append".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Bool, false),
        );
        self.functions
            .insert("read_line".to_string(), builtin(vec![], Type::Str, false));
        self.functions.insert(
            "print_flush".to_string(),
            builtin(vec![Type::Str], Type::Unit, false),
        );
        self.functions.insert(
            "env_get".to_string(),
            builtin(vec![Type::Str], Type::Str, false),
        );
        self.functions
            .insert("time_ms".to_string(), builtin(vec![], Type::Int, false));
        // Networking — TCP
        self.functions.insert(
            "tcp_connect".to_string(),
            builtin(vec![Type::Str, Type::Int], Type::Int, false),
        );
        self.functions.insert(
            "tcp_listen".to_string(),
            builtin(vec![Type::Str, Type::Int], Type::Int, false),
        );
        self.functions.insert(
            "tcp_accept".to_string(),
            builtin(vec![Type::Int], Type::Int, false),
        );
        self.functions.insert(
            "tcp_send".to_string(),
            builtin(vec![Type::Int, Type::Str], Type::Int, false),
        );
        self.functions.insert(
            "tcp_recv".to_string(),
            builtin(vec![Type::Int, Type::Int], Type::Str, false),
        );
        self.functions.insert(
            "tcp_close".to_string(),
            builtin(vec![Type::Int], Type::Unit, false),
        );
        // Networking — UDP
        self.functions
            .insert("udp_socket".to_string(), builtin(vec![], Type::Int, false));
        self.functions.insert(
            "udp_bind".to_string(),
            builtin(vec![Type::Int, Type::Str, Type::Int], Type::Int, false),
        );
        self.functions.insert(
            "udp_send_to".to_string(),
            builtin(
                vec![Type::Int, Type::Str, Type::Str, Type::Int],
                Type::Int,
                false,
            ),
        );
        self.functions.insert(
            "udp_recv_from".to_string(),
            builtin(vec![Type::Int, Type::Int], Type::Str, false),
        );
        // Networking — DNS
        self.functions.insert(
            "dns_resolve".to_string(),
            builtin(vec![Type::Str], Type::Str, false),
        );
        // Networking — HTTP
        self.functions.insert(
            "http_request".to_string(),
            builtin(
                vec![Type::Str, Type::Str, Type::Str, Type::Str],
                Type::Str,
                false,
            ),
        );
        self.functions.insert(
            "http_get".to_string(),
            builtin(vec![Type::Str], Type::Str, false),
        );
        self.functions.insert(
            "http_post".to_string(),
            builtin(vec![Type::Str, Type::Str], Type::Str, false),
        );
        // String conversion builtins (for string interpolation)
        self.functions.insert(
            "float_to_str".to_string(),
            builtin(vec![Type::Float], Type::Str, true),
        );
        self.functions.insert(
            "bool_to_str".to_string(),
            builtin(vec![Type::Bool], Type::Str, true),
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

    fn register_function(&mut self, f: &FnDecl) {
        let sig = FnSig {
            params: f.params.iter().map(|p| p.ty.clone()).collect(),
            ret: f.return_type.clone(),
            is_pure: f.is_pure,
            type_params: f.type_params.iter().map(|tp| tp.name.clone()).collect(),
            span: f.span,
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
                Contract::Effects(_) => {}
            }
        }

        self.check_block(&f.body);
        self.pop_scope();
        self.current_fn_ret = None;
        self.current_fn_is_pure = false;

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
                if let Some(val_ty) = self.infer_expr(&s.value) {
                    // For generic types, compare base name
                    let compatible = match (&s.ty, &val_ty) {
                        (Type::Generic(name, _), Type::Named(vname)) => name == vname,
                        (Type::Generic(name1, _), Type::Generic(name2, _)) => name1 == name2,
                        _ => val_ty == s.ty,
                    };
                    if !compatible {
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
                let value_ty = self.infer_expr(&s.value);
                if let (Some(tt), Some(vt)) = (target_ty, value_ty) {
                    if tt != vt {
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
                    if let Some(actual) = self.infer_expr(&s.value) {
                        let compatible = match (&expected, &actual) {
                            (Type::Generic(name1, _), Type::Generic(name2, _)) => name1 == name2,
                            (Type::Generic(name, _), Type::Named(vname)) => name == vname,
                            _ => actual == expected,
                        };
                        if !compatible {
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
                if let ExprKind::Range(ref start, ref end) = s.iterable.kind {
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
                    self.check_block(&s.body);
                    self.pop_scope();
                    self.loop_depth -= 1;
                } else if let Some(iter_ty) = self.infer_expr(&s.iterable) {
                    if let Type::Array(inner) = iter_ty {
                        self.loop_depth += 1;
                        self.push_scope();
                        self.define_with_span(&s.var_name, *inner, false, s.span);
                        self.check_block(&s.body);
                        self.pop_scope();
                        self.loop_depth -= 1;
                    } else {
                        self.errors.push(TypeError {
                            message: format!(
                                "for loop requires an array or range, found '{}'",
                                iter_ty
                            ),
                            span: s.iterable.span,
                        });
                    }
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

    // ── Expression type inference ────────────────────────────

    fn infer_expr(&mut self, expr: &Expr) -> Option<Type> {
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
                                    if val_ty != **inner {
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
                let recv_ty = self.infer_expr(receiver)?;

                // Handle .join() on Task<T>
                if let Type::Task(inner) = &recv_ty {
                    if method_name == "join" {
                        if !args.is_empty() {
                            self.errors.push(TypeError {
                                message: "join() takes no arguments".to_string(),
                                span: expr.span,
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
                                return type_args.first().cloned();
                            }
                            "is_some" | "is_none" => {
                                if !args.is_empty() {
                                    self.errors.push(TypeError {
                                        message: format!("{}() takes no arguments", method_name),
                                        span: expr.span,
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
                                return type_args.first().cloned();
                            }
                            "unwrap_err" => {
                                if !args.is_empty() {
                                    self.errors.push(TypeError {
                                        message: "unwrap_err() takes no arguments".to_string(),
                                        span: expr.span,
                                    });
                                }
                                return type_args.get(1).cloned();
                            }
                            "is_ok" | "is_err" => {
                                if !args.is_empty() {
                                    self.errors.push(TypeError {
                                        message: format!("{}() takes no arguments", method_name),
                                        span: expr.span,
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
                        if let Some(args) = type_args {
                            let subst: HashMap<String, Type> = info
                                .type_params
                                .iter()
                                .zip(args.iter())
                                .map(|(p, a)| (p.clone(), a.clone()))
                                .collect();
                            Some(self.substitute_type_vars(fty, &subst))
                        } else {
                            Some(fty.clone())
                        }
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
                self.push_scope();
                self.check_block(block);
                // Infer the return type from the last return statement in the block
                let inner_ty = self.infer_spawn_return_type(block);
                self.pop_scope();
                Some(Type::Task(Box::new(inner_ty)))
            }

            ExprKind::Range(start, end) => {
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

            ExprKind::StructInit(name, fields) => {
                if let Some(info) = self.structs.get(name).cloned() {
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
            other => other.clone(),
        }
    }

    fn is_valid_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Int
            | Type::Float
            | Type::Bool
            | Type::Char
            | Type::Str
            | Type::Unit
            | Type::Map => true,
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
                self.structs.contains_key(name) || self.enums.contains_key(name)
            }
            Type::Fn(_, _) => true,
            Type::Task(inner) => self.is_valid_type(inner),
            Type::Chan(inner) => self.is_valid_type(inner),
            Type::Tuple(types) => types.iter().all(|t| self.is_valid_type(t)),
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
