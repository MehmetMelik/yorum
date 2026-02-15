use crate::compiler::span::Span;
use crate::compiler::token::{Token, TokenKind};
use std::fmt;

// ═══════════════════════════════════════════════════════════════
//  Error type
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "lex error at {}: {}", self.span, self.message)
    }
}

impl std::error::Error for LexError {}

// ═══════════════════════════════════════════════════════════════
//  Lexer
// ═══════════════════════════════════════════════════════════════

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: u32,
    col: u32,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    /// Tokenize the entire source into a Vec<Token>.
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token()?;
            let is_eof = tok.kind.is_eof();
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace_and_comments();

        if self.is_at_end() {
            return Ok(Token::new(
                TokenKind::EOF,
                Span::new(self.pos, self.pos, self.line, self.col),
            ));
        }

        let start = self.pos;
        let start_line = self.line;
        let start_col = self.col;
        let c = self.advance();

        let kind = match c {
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            '.' => TokenKind::Dot,
            '+' => TokenKind::Plus,
            '*' => TokenKind::Star,
            '%' => TokenKind::Percent,
            '&' => TokenKind::Ampersand,
            '|' => TokenKind::Pipe,

            '-' => {
                if self.peek() == Some('>') {
                    self.advance();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }

            '=' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::EqEq
                } else if self.peek() == Some('>') {
                    self.advance();
                    TokenKind::FatArrow
                } else {
                    TokenKind::Eq
                }
            }

            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::NotEq
                } else {
                    return Err(LexError {
                        message: format!(
                            "unexpected character '!'; use 'not' for logical negation"
                        ),
                        span: Span::new(start, self.pos, start_line, start_col),
                    });
                }
            }

            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }

            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }

            '/' => {
                // Division — comments are already stripped by skip_whitespace_and_comments
                TokenKind::Slash
            }

            '"' => return self.lex_string(start, start_line, start_col),

            c if c.is_ascii_digit() => {
                return self.lex_number(start, start_line, start_col);
            }

            '_' if !self
                .peek()
                .map_or(false, |c| c.is_ascii_alphanumeric() || c == '_') =>
            {
                TokenKind::Underscore
            }

            c if c.is_ascii_alphabetic() || c == '_' => {
                return self.lex_ident_or_keyword(start, start_line, start_col);
            }

            other => {
                return Err(LexError {
                    message: format!("unexpected character '{}'", other),
                    span: Span::new(start, self.pos, start_line, start_col),
                });
            }
        };

        Ok(Token::new(
            kind,
            Span::new(start, self.pos, start_line, start_col),
        ))
    }

    // ── Helpers ──────────────────────────────────────────────────

    fn is_at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.pos).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.source.get(self.pos + 1).copied()
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.pos];
        self.pos += 1;
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        c
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            // Skip whitespace
            while !self.is_at_end() && self.source[self.pos].is_ascii_whitespace() {
                self.advance();
            }

            if self.is_at_end() {
                return;
            }

            // Skip line comments: //
            if self.peek() == Some('/') && self.peek_next() == Some('/') {
                while !self.is_at_end() && self.peek() != Some('\n') {
                    self.advance();
                }
                continue;
            }

            // Skip block comments: /* ... */
            if self.peek() == Some('/') && self.peek_next() == Some('*') {
                self.advance(); // /
                self.advance(); // *
                let mut depth = 1;
                while !self.is_at_end() && depth > 0 {
                    if self.peek() == Some('/') && self.peek_next() == Some('*') {
                        self.advance();
                        self.advance();
                        depth += 1;
                    } else if self.peek() == Some('*') && self.peek_next() == Some('/') {
                        self.advance();
                        self.advance();
                        depth -= 1;
                    } else {
                        self.advance();
                    }
                }
                continue;
            }

            break;
        }
    }

    fn lex_string(
        &mut self,
        start: usize,
        start_line: u32,
        start_col: u32,
    ) -> Result<Token, LexError> {
        let mut value = String::new();

        loop {
            if self.is_at_end() {
                return Err(LexError {
                    message: "unterminated string literal".to_string(),
                    span: Span::new(start, self.pos, start_line, start_col),
                });
            }

            let c = self.advance();
            match c {
                '"' => break,
                '\\' => {
                    if self.is_at_end() {
                        return Err(LexError {
                            message: "unterminated escape sequence".to_string(),
                            span: Span::new(start, self.pos, start_line, start_col),
                        });
                    }
                    let escaped = self.advance();
                    match escaped {
                        'n' => value.push('\n'),
                        't' => value.push('\t'),
                        'r' => value.push('\r'),
                        '\\' => value.push('\\'),
                        '"' => value.push('"'),
                        '0' => value.push('\0'),
                        other => {
                            return Err(LexError {
                                message: format!("unknown escape sequence '\\{}'", other),
                                span: Span::new(start, self.pos, start_line, start_col),
                            });
                        }
                    }
                }
                _ => value.push(c),
            }
        }

        Ok(Token::new(
            TokenKind::StringLit(value),
            Span::new(start, self.pos, start_line, start_col),
        ))
    }

    fn lex_number(
        &mut self,
        start: usize,
        start_line: u32,
        start_col: u32,
    ) -> Result<Token, LexError> {
        // Already consumed first digit
        while !self.is_at_end() && self.peek().map_or(false, |c| c.is_ascii_digit()) {
            self.advance();
        }

        // Check for float
        if self.peek() == Some('.') && self.peek_next().map_or(false, |c| c.is_ascii_digit()) {
            self.advance(); // consume '.'
            while !self.is_at_end() && self.peek().map_or(false, |c| c.is_ascii_digit()) {
                self.advance();
            }
            let text: String = self.source[start..self.pos].iter().collect();
            let value: f64 = text.parse().map_err(|_| LexError {
                message: format!("invalid float literal '{}'", text),
                span: Span::new(start, self.pos, start_line, start_col),
            })?;
            return Ok(Token::new(
                TokenKind::FloatLit(value),
                Span::new(start, self.pos, start_line, start_col),
            ));
        }

        let text: String = self.source[start..self.pos].iter().collect();
        let value: i64 = text.parse().map_err(|_| LexError {
            message: format!("invalid integer literal '{}'", text),
            span: Span::new(start, self.pos, start_line, start_col),
        })?;

        Ok(Token::new(
            TokenKind::IntLit(value),
            Span::new(start, self.pos, start_line, start_col),
        ))
    }

    fn lex_ident_or_keyword(
        &mut self,
        start: usize,
        start_line: u32,
        start_col: u32,
    ) -> Result<Token, LexError> {
        while !self.is_at_end()
            && self
                .peek()
                .map_or(false, |c| c.is_ascii_alphanumeric() || c == '_')
        {
            self.advance();
        }

        let text: String = self.source[start..self.pos].iter().collect();
        let kind = TokenKind::keyword_from_str(&text).unwrap_or(TokenKind::Ident(text));

        Ok(Token::new(
            kind,
            Span::new(start, self.pos, start_line, start_col),
        ))
    }
}

// ═══════════════════════════════════════════════════════════════
//  Unit tests
// ═══════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Vec<TokenKind> {
        let mut lexer = Lexer::new(input);
        lexer
            .tokenize()
            .unwrap()
            .into_iter()
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn test_simple_function() {
        let tokens = lex("fn main() -> int { return 0; }");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Fn,
                TokenKind::Ident("main".into()),
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::Arrow,
                TokenKind::IntType,
                TokenKind::LBrace,
                TokenKind::Return,
                TokenKind::IntLit(0),
                TokenKind::Semicolon,
                TokenKind::RBrace,
                TokenKind::EOF,
            ]
        );
    }

    #[test]
    fn test_keywords_and_operators() {
        let tokens = lex("let mut x: int = 10 + 20;");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Let,
                TokenKind::Mut,
                TokenKind::Ident("x".into()),
                TokenKind::Colon,
                TokenKind::IntType,
                TokenKind::Eq,
                TokenKind::IntLit(10),
                TokenKind::Plus,
                TokenKind::IntLit(20),
                TokenKind::Semicolon,
                TokenKind::EOF,
            ]
        );
    }

    #[test]
    fn test_string_literal() {
        let tokens = lex(r#""hello\nworld""#);
        assert_eq!(
            tokens,
            vec![TokenKind::StringLit("hello\nworld".into()), TokenKind::EOF,]
        );
    }

    #[test]
    fn test_float_literal() {
        let tokens = lex("3.14");
        assert_eq!(tokens, vec![TokenKind::FloatLit(3.14), TokenKind::EOF]);
    }

    #[test]
    fn test_comments() {
        let tokens = lex("// this is a comment\nfn /* inline */ main");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Fn,
                TokenKind::Ident("main".into()),
                TokenKind::EOF,
            ]
        );
    }

    #[test]
    fn test_logical_operators() {
        let tokens = lex("x and y or not z");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Ident("x".into()),
                TokenKind::And,
                TokenKind::Ident("y".into()),
                TokenKind::Or,
                TokenKind::Not,
                TokenKind::Ident("z".into()),
                TokenKind::EOF,
            ]
        );
    }

    #[test]
    fn test_comparison_operators() {
        let tokens = lex("a == b != c <= d >= e");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Ident("a".into()),
                TokenKind::EqEq,
                TokenKind::Ident("b".into()),
                TokenKind::NotEq,
                TokenKind::Ident("c".into()),
                TokenKind::LtEq,
                TokenKind::Ident("d".into()),
                TokenKind::GtEq,
                TokenKind::Ident("e".into()),
                TokenKind::EOF,
            ]
        );
    }

    #[test]
    fn test_contract_keywords() {
        let tokens = lex("requires ensures effects pure");
        assert_eq!(
            tokens,
            vec![
                TokenKind::Requires,
                TokenKind::Ensures,
                TokenKind::Effects,
                TokenKind::Pure,
                TokenKind::EOF,
            ]
        );
    }
}
