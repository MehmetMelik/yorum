//! Abstract Syntax Tree for the Yorum language.
//!
//! Every node carries a [`Span`] for lossless round-tripping (source → AST → source).
//! All types derive `Serialize`/`Deserialize` so the entire AST can be exported as JSON.

use crate::compiler::span::Span;
use serde::{Deserialize, Serialize};

// ═══════════════════════════════════════════════════════════════
//  Top-level program
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    pub module_name: Option<String>,
    pub uses: Vec<UseDecl>,
    pub declarations: Vec<Declaration>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UseDecl {
    pub path: Vec<String>,
    pub span: Span,
}

// ═══════════════════════════════════════════════════════════════
//  Declarations
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Declaration {
    Function(FnDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Const(ConstDecl),
    Impl(ImplDecl),
    Trait(TraitDecl),
}

/// Function declaration with optional purity, contracts, and effect annotations.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FnDecl {
    pub name: String,
    pub is_pure: bool,
    pub is_pub: bool,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub contracts: Vec<Contract>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

/// Machine-readable contract clauses attached to functions.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Contract {
    Requires(Expr),
    Ensures(Expr),
    Effects(Vec<String>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDecl {
    pub name: String,
    pub is_pub: bool,
    pub type_params: Vec<TypeParam>,
    pub fields: Vec<Field>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Field {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumDecl {
    pub name: String,
    pub is_pub: bool,
    pub type_params: Vec<TypeParam>,
    pub variants: Vec<Variant>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstDecl {
    pub name: String,
    pub is_pub: bool,
    pub ty: Type,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImplDecl {
    pub target_type: String,
    pub trait_name: Option<String>,
    pub type_params: Vec<TypeParam>,
    pub methods: Vec<FnDecl>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitDecl {
    pub name: String,
    pub is_pub: bool,
    pub methods: Vec<TraitMethod>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraitMethod {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub default_body: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeParam {
    pub name: String,
    pub bounds: Vec<String>,
    pub span: Span,
}

// ═══════════════════════════════════════════════════════════════
//  Types
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Type {
    Int,
    Float,
    Bool,
    Char,
    Str,
    Unit,
    Named(String),
    Array(Box<Type>),
    Ref(Box<Type>),
    MutRef(Box<Type>),
    Own(Box<Type>),
    SelfType,
    TypeVar(String),
    Generic(String, Vec<Type>),
    Fn(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Task(Box<Type>),
    Chan(Box<Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Str => write!(f, "string"),
            Type::Unit => write!(f, "unit"),
            Type::Named(n) => write!(f, "{}", n),
            Type::Array(inner) => write!(f, "[{}]", inner),
            Type::Ref(inner) => write!(f, "&{}", inner),
            Type::MutRef(inner) => write!(f, "&mut {}", inner),
            Type::Own(inner) => write!(f, "own {}", inner),
            Type::SelfType => write!(f, "Self"),
            Type::TypeVar(name) => write!(f, "{}", name),
            Type::Generic(name, args) => {
                write!(f, "{}<", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ">")
            }
            Type::Fn(params, ret) => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ") -> {}", ret)
            }
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Task(inner) => write!(f, "Task<{}>", inner),
            Type::Chan(inner) => write!(f, "Chan<{}>", inner),
        }
    }
}

// ═══════════════════════════════════════════════════════════════
//  Statements
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Stmt {
    Let(LetStmt),
    Assign(AssignStmt),
    Return(ReturnStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Match(MatchStmt),
    Expr(ExprStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BreakStmt {
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContinueStmt {
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LetStmt {
    pub name: String,
    pub is_mut: bool,
    pub ty: Type,
    pub value: Expr,
    /// For tuple destructuring: `let (a, b): (int, string) = t;`
    pub destructure: Option<Vec<String>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssignStmt {
    pub target: Expr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReturnStmt {
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_block: Block,
    pub else_branch: Option<Box<ElseBranch>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ElseBranch {
    ElseIf(IfStmt),
    Else(Block),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForStmt {
    pub var_name: String,
    pub iterable: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchStmt {
    pub subject: Expr,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Pattern {
    Wildcard(Span),
    Binding(String, Span),
    Literal(Literal, Span),
    Variant(String, Vec<Pattern>, Span),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: Span,
}

// ═══════════════════════════════════════════════════════════════
//  Expressions
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExprKind {
    Literal(Literal),
    Ident(String),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    FieldAccess(Box<Expr>, String),
    MethodCall(Box<Expr>, String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    StructInit(String, Vec<FieldInit>),
    Closure(ClosureExpr),
    ArrayLit(Vec<Expr>),
    ArrayRepeat(Box<Expr>, Box<Expr>), // [value; count]
    TupleLit(Vec<Expr>),
    Spawn(Block),
    Range(Box<Expr>, Box<Expr>),
    RangeInclusive(Box<Expr>, Box<Expr>),
    RangeFrom(Box<Expr>),
    Try(Box<Expr>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClosureExpr {
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldInit {
    pub name: String,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Eq => write!(f, "=="),
            BinOp::NotEq => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Gt => write!(f, ">"),
            BinOp::LtEq => write!(f, "<="),
            BinOp::GtEq => write!(f, ">="),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
            BinOp::BitAnd => write!(f, "&"),
            BinOp::BitOr => write!(f, "|"),
            BinOp::BitXor => write!(f, "^"),
            BinOp::Shl => write!(f, "<<"),
            BinOp::Shr => write!(f, ">>"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
}
