use crate::compiler::span::Span;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// A part of an interpolated string literal.
#[derive(Debug, Clone, PartialEq)]
pub enum InterpPart {
    /// A literal string segment.
    Literal(String),
    /// An expression (as a sequence of tokens) to be interpolated.
    Expr(Vec<Token>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // ── Literals ──────────────────────────────────────────────
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),
    InterpStringLit(Vec<InterpPart>),
    CharLit(char),

    // ── Identifier ───────────────────────────────────────────
    Ident(String),

    // ── Keywords ─────────────────────────────────────────────
    Fn,
    Let,
    Mut,
    Return,
    If,
    Else,
    While,
    For,
    In,
    Match,
    Struct,
    Enum,
    Module,
    Use,
    Const,
    Pub,
    Pure,
    Own,
    Impl,
    Trait,
    SelfType,
    Spawn,
    Break,
    Continue,

    // Logical operators (keyword-based, no &&/||/! ambiguity)
    And,
    Or,
    Not,

    // Boolean literals
    True,
    False,

    // Contract keywords
    Requires,
    Ensures,
    Effects,

    // ── Type keywords ────────────────────────────────────────
    IntType,
    FloatType,
    BoolType,
    StringType,
    CharType,
    UnitType,

    // ── Delimiters ───────────────────────────────────────────
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]

    // ── Punctuation ──────────────────────────────────────────
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;
    Dot,       // .
    DotDot,    // ..
    DotDotEq,  // ..=
    Arrow,     // ->
    FatArrow,  // =>

    // ── Operators ────────────────────────────────────────────
    Eq,        // =
    EqEq,      // ==
    NotEq,     // !=
    Lt,        // <
    Gt,        // >
    LtEq,      // <=
    GtEq,      // >=
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Percent,   // %
    Ampersand, // &

    Pipe,   // |
    Caret,  // ^
    LShift, // <<

    // ── Compound assignment ─────────────────────────────────
    PlusEq,    // +=
    MinusEq,   // -=
    StarEq,    // *=
    SlashEq,   // /=
    PercentEq, // %=

    // ── Special ──────────────────────────────────────────────
    QuestionMark, // ?
    Underscore,   // _
    EOF,
}

impl TokenKind {
    /// Map an identifier string to its keyword token, if any.
    pub fn keyword_from_str(s: &str) -> Option<TokenKind> {
        match s {
            "fn" => Some(TokenKind::Fn),
            "let" => Some(TokenKind::Let),
            "mut" => Some(TokenKind::Mut),
            "return" => Some(TokenKind::Return),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "while" => Some(TokenKind::While),
            "for" => Some(TokenKind::For),
            "in" => Some(TokenKind::In),
            "match" => Some(TokenKind::Match),
            "struct" => Some(TokenKind::Struct),
            "enum" => Some(TokenKind::Enum),
            "module" => Some(TokenKind::Module),
            "use" => Some(TokenKind::Use),
            "const" => Some(TokenKind::Const),
            "pub" => Some(TokenKind::Pub),
            "pure" => Some(TokenKind::Pure),
            "own" => Some(TokenKind::Own),
            "impl" => Some(TokenKind::Impl),
            "trait" => Some(TokenKind::Trait),
            "Self" => Some(TokenKind::SelfType),
            "spawn" => Some(TokenKind::Spawn),
            "break" => Some(TokenKind::Break),
            "continue" => Some(TokenKind::Continue),
            "and" => Some(TokenKind::And),
            "or" => Some(TokenKind::Or),
            "not" => Some(TokenKind::Not),
            "true" => Some(TokenKind::True),
            "false" => Some(TokenKind::False),
            "requires" => Some(TokenKind::Requires),
            "ensures" => Some(TokenKind::Ensures),
            "effects" => Some(TokenKind::Effects),
            "int" => Some(TokenKind::IntType),
            "float" => Some(TokenKind::FloatType),
            "bool" => Some(TokenKind::BoolType),
            "string" => Some(TokenKind::StringType),
            "char" => Some(TokenKind::CharType),
            "unit" => Some(TokenKind::UnitType),
            _ => None,
        }
    }

    pub fn is_eof(&self) -> bool {
        matches!(self, TokenKind::EOF)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::IntLit(n) => write!(f, "{}", n),
            TokenKind::FloatLit(n) => write!(f, "{}", n),
            TokenKind::StringLit(s) => write!(f, "\"{}\"", s),
            TokenKind::InterpStringLit(_) => write!(f, "\"...{{...}}...\""),
            TokenKind::CharLit(c) => write!(f, "'{}'", c),
            TokenKind::Ident(s) => write!(f, "{}", s),
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Mut => write!(f, "mut"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::While => write!(f, "while"),
            TokenKind::For => write!(f, "for"),
            TokenKind::In => write!(f, "in"),
            TokenKind::Match => write!(f, "match"),
            TokenKind::Struct => write!(f, "struct"),
            TokenKind::Enum => write!(f, "enum"),
            TokenKind::Module => write!(f, "module"),
            TokenKind::Use => write!(f, "use"),
            TokenKind::Const => write!(f, "const"),
            TokenKind::Pub => write!(f, "pub"),
            TokenKind::Pure => write!(f, "pure"),
            TokenKind::Own => write!(f, "own"),
            TokenKind::Impl => write!(f, "impl"),
            TokenKind::Trait => write!(f, "trait"),
            TokenKind::SelfType => write!(f, "Self"),
            TokenKind::Spawn => write!(f, "spawn"),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Continue => write!(f, "continue"),
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Not => write!(f, "not"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Requires => write!(f, "requires"),
            TokenKind::Ensures => write!(f, "ensures"),
            TokenKind::Effects => write!(f, "effects"),
            TokenKind::IntType => write!(f, "int"),
            TokenKind::FloatType => write!(f, "float"),
            TokenKind::BoolType => write!(f, "bool"),
            TokenKind::StringType => write!(f, "string"),
            TokenKind::CharType => write!(f, "char"),
            TokenKind::UnitType => write!(f, "unit"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::DotDot => write!(f, ".."),
            TokenKind::DotDotEq => write!(f, "..="),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::FatArrow => write!(f, "=>"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::NotEq => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Ampersand => write!(f, "&"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::LShift => write!(f, "<<"),
            TokenKind::PlusEq => write!(f, "+="),
            TokenKind::MinusEq => write!(f, "-="),
            TokenKind::StarEq => write!(f, "*="),
            TokenKind::SlashEq => write!(f, "/="),
            TokenKind::PercentEq => write!(f, "%="),
            TokenKind::QuestionMark => write!(f, "?"),
            TokenKind::Underscore => write!(f, "_"),
            TokenKind::EOF => write!(f, "<EOF>"),
        }
    }
}
