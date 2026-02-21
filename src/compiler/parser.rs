use crate::compiler::ast::*;
use crate::compiler::span::Span;
use crate::compiler::token::{InterpPart, Token, TokenKind};
use std::fmt;

// ═══════════════════════════════════════════════════════════════
//  Error type
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error at {}: {}", self.span, self.message)
    }
}

impl std::error::Error for ParseError {}

// ═══════════════════════════════════════════════════════════════
//  Parser
// ═══════════════════════════════════════════════════════════════

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let start_span = self.current_span();

        // Optional module declaration
        let module_name = if self.check(&TokenKind::Module) {
            self.advance();
            let name = self.expect_ident()?;
            self.expect(&TokenKind::Semicolon)?;
            Some(name)
        } else {
            None
        };

        // Use declarations
        let mut uses = Vec::new();
        while self.check(&TokenKind::Use) {
            uses.push(self.parse_use_decl()?);
        }

        // Top-level declarations
        let mut declarations = Vec::new();
        while !self.check(&TokenKind::EOF) {
            declarations.push(self.parse_declaration()?);
        }

        let end_span = self.current_span();
        Ok(Program {
            module_name,
            uses,
            declarations,
            span: start_span.merge(end_span),
        })
    }

    // ── Use declarations ─────────────────────────────────────

    fn parse_use_decl(&mut self) -> Result<UseDecl, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Use)?;

        let mut path = vec![self.expect_ident()?];
        while self.check(&TokenKind::Dot) {
            self.advance();
            path.push(self.expect_ident()?);
        }
        self.expect(&TokenKind::Semicolon)?;

        Ok(UseDecl {
            path,
            span: start.merge(self.prev_span()),
        })
    }

    // ── Top-level declarations ───────────────────────────────

    fn parse_declaration(&mut self) -> Result<Declaration, ParseError> {
        // impl blocks and trait decls: no pub/pure prefix needed for impl
        if self.check(&TokenKind::Impl) {
            return Ok(Declaration::Impl(self.parse_impl_decl()?));
        }

        let is_pub = if self.check(&TokenKind::Pub) {
            self.advance();
            true
        } else {
            false
        };

        let is_pure = if self.check(&TokenKind::Pure) {
            self.advance();
            true
        } else {
            false
        };

        match self.peek_kind() {
            TokenKind::Fn => Ok(Declaration::Function(self.parse_fn_decl(is_pub, is_pure)?)),
            TokenKind::Struct => {
                if is_pure {
                    return Err(self.error("'pure' modifier is not valid on struct declarations"));
                }
                Ok(Declaration::Struct(self.parse_struct_decl(is_pub)?))
            }
            TokenKind::Enum => {
                if is_pure {
                    return Err(self.error("'pure' modifier is not valid on enum declarations"));
                }
                Ok(Declaration::Enum(self.parse_enum_decl(is_pub)?))
            }
            TokenKind::Const => {
                if is_pure {
                    return Err(self.error("'pure' modifier is not valid on const declarations"));
                }
                Ok(Declaration::Const(self.parse_const_decl(is_pub)?))
            }
            TokenKind::Trait => {
                if is_pure {
                    return Err(self.error("'pure' modifier is not valid on trait declarations"));
                }
                Ok(Declaration::Trait(self.parse_trait_decl(is_pub)?))
            }
            _ => Err(self.error("expected declaration (fn, struct, enum, const, or trait)")),
        }
    }

    fn parse_impl_decl(&mut self) -> Result<ImplDecl, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Impl)?;
        let type_params = self.parse_type_params()?;
        let first_name = self.expect_ident()?;

        // Check for `impl Trait for Type { ... }`
        let (target_type, trait_name) = if self.check(&TokenKind::For) {
            self.advance();
            let target = self.expect_ident()?;
            (target, Some(first_name))
        } else {
            (first_name, None)
        };

        self.expect(&TokenKind::LBrace)?;
        let mut methods = Vec::new();
        while !self.check(&TokenKind::RBrace) {
            let is_pub = if self.check(&TokenKind::Pub) {
                self.advance();
                true
            } else {
                false
            };
            let is_pure = if self.check(&TokenKind::Pure) {
                self.advance();
                true
            } else {
                false
            };
            methods.push(self.parse_fn_decl(is_pub, is_pure)?);
        }
        self.expect(&TokenKind::RBrace)?;

        Ok(ImplDecl {
            target_type,
            trait_name,
            type_params,
            methods,
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_trait_decl(&mut self, is_pub: bool) -> Result<TraitDecl, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Trait)?;
        let name = self.expect_ident()?;
        self.expect(&TokenKind::LBrace)?;

        let mut methods = Vec::new();
        while !self.check(&TokenKind::RBrace) {
            let mstart = self.current_span();
            self.expect(&TokenKind::Fn)?;
            let mname = self.expect_ident()?;

            self.expect(&TokenKind::LParen)?;
            let params = if self.check(&TokenKind::RParen) {
                self.advance();
                Vec::new()
            } else {
                let params = self.parse_param_list()?;
                self.expect(&TokenKind::RParen)?;
                params
            };

            self.expect(&TokenKind::Arrow)?;
            let return_type = self.parse_type()?;

            // Semicolon = required method, block = default implementation
            let default_body = if self.check(&TokenKind::Semicolon) {
                self.advance();
                None
            } else {
                Some(self.parse_block()?)
            };

            methods.push(TraitMethod {
                name: mname,
                params,
                return_type,
                default_body,
                span: mstart.merge(self.prev_span()),
            });
        }
        self.expect(&TokenKind::RBrace)?;

        Ok(TraitDecl {
            name,
            is_pub,
            methods,
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_type_params(&mut self) -> Result<Vec<TypeParam>, ParseError> {
        let mut type_params = Vec::new();
        if !self.check(&TokenKind::Lt) {
            return Ok(type_params);
        }
        self.advance(); // consume <

        loop {
            let start = self.current_span();
            let name = self.expect_ident()?;
            let mut bounds = Vec::new();
            if self.check(&TokenKind::Colon) {
                self.advance();
                bounds.push(self.expect_ident()?);
                while self.check(&TokenKind::Plus) {
                    self.advance();
                    bounds.push(self.expect_ident()?);
                }
            }
            type_params.push(TypeParam {
                name,
                bounds,
                span: start.merge(self.prev_span()),
            });
            if self.check(&TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(&TokenKind::Gt)?;
        Ok(type_params)
    }

    fn parse_fn_decl(&mut self, is_pub: bool, is_pure: bool) -> Result<FnDecl, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Fn)?;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;

        // Parameters
        self.expect(&TokenKind::LParen)?;
        let params = if self.check(&TokenKind::RParen) {
            self.advance();
            Vec::new()
        } else {
            let params = self.parse_param_list()?;
            self.expect(&TokenKind::RParen)?;
            params
        };

        // Return type
        self.expect(&TokenKind::Arrow)?;
        let return_type = self.parse_type()?;

        // Contract clauses
        let mut contracts = Vec::new();
        loop {
            if self.check(&TokenKind::Requires) {
                self.advance();
                let expr = self.parse_expr()?;
                contracts.push(Contract::Requires(expr));
            } else if self.check(&TokenKind::Ensures) {
                self.advance();
                let expr = self.parse_expr()?;
                contracts.push(Contract::Ensures(expr));
            } else if self.check(&TokenKind::Effects) {
                self.advance();
                // Handle empty effects list: `effects {` → empty vec
                let mut effects = Vec::new();
                if !self.check(&TokenKind::LBrace) {
                    effects.push(self.expect_ident()?);
                    while self.check(&TokenKind::Comma) {
                        self.advance();
                        // Stop if we hit the opening brace
                        if self.check(&TokenKind::LBrace) {
                            break;
                        }
                        effects.push(self.expect_ident()?);
                    }
                }
                contracts.push(Contract::Effects(effects));
            } else {
                break;
            }
        }

        // Body
        let body = self.parse_block()?;

        Ok(FnDecl {
            name,
            is_pure,
            is_pub,
            type_params,
            params,
            return_type,
            contracts,
            body,
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_param_list(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();
        params.push(self.parse_param()?);
        while self.check(&TokenKind::Comma) {
            self.advance();
            if self.check(&TokenKind::RParen) {
                break; // trailing comma
            }
            params.push(self.parse_param()?);
        }
        Ok(params)
    }

    fn parse_param(&mut self) -> Result<Param, ParseError> {
        let start = self.current_span();
        let name = self.expect_ident()?;
        self.expect(&TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok(Param {
            name,
            ty,
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_struct_decl(&mut self, is_pub: bool) -> Result<StructDecl, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Struct)?;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
        self.expect(&TokenKind::LBrace)?;

        let mut fields = Vec::new();
        while !self.check(&TokenKind::RBrace) {
            let fstart = self.current_span();
            let fname = self.expect_ident()?;
            self.expect(&TokenKind::Colon)?;
            let fty = self.parse_type()?;
            fields.push(Field {
                name: fname,
                ty: fty,
                span: fstart.merge(self.prev_span()),
            });
            if !self.check(&TokenKind::RBrace) {
                self.expect(&TokenKind::Comma)?;
            }
        }
        self.expect(&TokenKind::RBrace)?;

        Ok(StructDecl {
            name,
            is_pub,
            type_params,
            fields,
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_enum_decl(&mut self, is_pub: bool) -> Result<EnumDecl, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Enum)?;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
        self.expect(&TokenKind::LBrace)?;

        let mut variants = Vec::new();
        while !self.check(&TokenKind::RBrace) {
            let vstart = self.current_span();
            let vname = self.expect_ident()?;
            let mut vfields = Vec::new();
            if self.check(&TokenKind::LParen) {
                self.advance();
                if !self.check(&TokenKind::RParen) {
                    vfields.push(self.parse_type()?);
                    while self.check(&TokenKind::Comma) {
                        self.advance();
                        if self.check(&TokenKind::RParen) {
                            break;
                        }
                        vfields.push(self.parse_type()?);
                    }
                }
                self.expect(&TokenKind::RParen)?;
            }
            variants.push(Variant {
                name: vname,
                fields: vfields,
                span: vstart.merge(self.prev_span()),
            });
            if !self.check(&TokenKind::RBrace) {
                self.expect(&TokenKind::Comma)?;
            }
        }
        self.expect(&TokenKind::RBrace)?;

        Ok(EnumDecl {
            name,
            is_pub,
            type_params,
            variants,
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_const_decl(&mut self, is_pub: bool) -> Result<ConstDecl, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Const)?;
        let name = self.expect_ident()?;
        self.expect(&TokenKind::Colon)?;
        let ty = self.parse_type()?;
        self.expect(&TokenKind::Eq)?;
        let value = self.parse_expr()?;
        self.expect(&TokenKind::Semicolon)?;

        Ok(ConstDecl {
            name,
            is_pub,
            ty,
            value,
            span: start.merge(self.prev_span()),
        })
    }

    // ── Types ────────────────────────────────────────────────

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.peek_kind() {
            TokenKind::IntType => {
                self.advance();
                Ok(Type::Int)
            }
            TokenKind::FloatType => {
                self.advance();
                Ok(Type::Float)
            }
            TokenKind::BoolType => {
                self.advance();
                Ok(Type::Bool)
            }
            TokenKind::StringType => {
                self.advance();
                Ok(Type::Str)
            }
            TokenKind::CharType => {
                self.advance();
                Ok(Type::Char)
            }
            TokenKind::UnitType => {
                self.advance();
                Ok(Type::Unit)
            }
            TokenKind::SelfType => {
                self.advance();
                Ok(Type::SelfType)
            }
            TokenKind::Fn => {
                // fn(T, U) -> V
                self.advance();
                self.expect(&TokenKind::LParen)?;
                let mut param_types = Vec::new();
                if !self.check(&TokenKind::RParen) {
                    param_types.push(self.parse_type()?);
                    while self.check(&TokenKind::Comma) {
                        self.advance();
                        if self.check(&TokenKind::RParen) {
                            break;
                        }
                        param_types.push(self.parse_type()?);
                    }
                }
                self.expect(&TokenKind::RParen)?;
                self.expect(&TokenKind::Arrow)?;
                let ret_type = self.parse_type()?;
                Ok(Type::Fn(param_types, Box::new(ret_type)))
            }
            TokenKind::Ident(_) => {
                let name = self.expect_ident()?;
                // Built-in Map type: bare "Map" defaults to Map<string, int>
                if name == "Map" && !self.check(&TokenKind::Lt) {
                    return Ok(Type::Generic("Map".to_string(), vec![Type::Str, Type::Int]));
                }
                // Check for type args: Name<T, U>
                if self.check(&TokenKind::Lt) {
                    self.advance();
                    let mut args = vec![self.parse_type()?];
                    while self.check(&TokenKind::Comma) {
                        self.advance();
                        args.push(self.parse_type()?);
                    }
                    self.expect(&TokenKind::Gt)?;
                    // Special-case Task<T> and Chan<T>
                    if name == "Task" && args.len() == 1 {
                        Ok(Type::Task(Box::new(args.into_iter().next().unwrap())))
                    } else if name == "Chan" && args.len() == 1 {
                        Ok(Type::Chan(Box::new(args.into_iter().next().unwrap())))
                    } else {
                        Ok(Type::Generic(name, args))
                    }
                } else {
                    Ok(Type::Named(name))
                }
            }
            TokenKind::LBracket => {
                self.advance();
                let inner = self.parse_type()?;
                self.expect(&TokenKind::RBracket)?;
                Ok(Type::Array(Box::new(inner)))
            }
            TokenKind::Ampersand => {
                self.advance();
                if self.check(&TokenKind::Mut) {
                    self.advance();
                    let inner = self.parse_type()?;
                    Ok(Type::MutRef(Box::new(inner)))
                } else {
                    let inner = self.parse_type()?;
                    Ok(Type::Ref(Box::new(inner)))
                }
            }
            TokenKind::Own => {
                self.advance();
                let inner = self.parse_type()?;
                Ok(Type::Own(Box::new(inner)))
            }
            TokenKind::LParen => {
                // Tuple type: (T1, T2, ...)
                self.advance();
                let first = self.parse_type()?;
                if self.check(&TokenKind::Comma) {
                    // It's a tuple type
                    self.advance();
                    let mut types = vec![first];
                    if !self.check(&TokenKind::RParen) {
                        types.push(self.parse_type()?);
                        while self.check(&TokenKind::Comma) {
                            self.advance();
                            if self.check(&TokenKind::RParen) {
                                break;
                            }
                            types.push(self.parse_type()?);
                        }
                    }
                    self.expect(&TokenKind::RParen)?;
                    Ok(Type::Tuple(types))
                } else {
                    // Single type in parens, not a tuple — just return the type
                    self.expect(&TokenKind::RParen)?;
                    Ok(first)
                }
            }
            _ => Err(self.error("expected type")),
        }
    }

    // ── Blocks and statements ────────────────────────────────

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        while !self.check(&TokenKind::RBrace) {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(&TokenKind::RBrace)?;

        Ok(Block {
            stmts,
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match self.peek_kind() {
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Match => self.parse_match_stmt(),
            TokenKind::Break => {
                let span = self.current_span();
                self.advance();
                self.expect(&TokenKind::Semicolon)?;
                Ok(Stmt::Break(BreakStmt { span }))
            }
            TokenKind::Continue => {
                let span = self.current_span();
                self.advance();
                self.expect(&TokenKind::Semicolon)?;
                Ok(Stmt::Continue(ContinueStmt { span }))
            }
            _ => self.parse_assign_or_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Let)?;

        let is_mut = if self.check(&TokenKind::Mut) {
            self.advance();
            true
        } else {
            false
        };

        // Check for tuple destructuring: let (a, b): (int, string) = ...
        if self.check(&TokenKind::LParen) {
            self.advance();
            let mut names = vec![self.expect_ident()?];
            while self.check(&TokenKind::Comma) {
                self.advance();
                if self.check(&TokenKind::RParen) {
                    break;
                }
                names.push(self.expect_ident()?);
            }
            self.expect(&TokenKind::RParen)?;
            self.expect(&TokenKind::Colon)?;
            let ty = self.parse_type()?;
            self.expect(&TokenKind::Eq)?;
            let value = self.parse_expr()?;
            self.expect(&TokenKind::Semicolon)?;

            return Ok(Stmt::Let(LetStmt {
                name: String::new(), // unused for destructuring
                is_mut,
                ty,
                value,
                destructure: Some(names),
                span: start.merge(self.prev_span()),
            }));
        }

        let name = self.expect_ident()?;
        self.expect(&TokenKind::Colon)?;
        let ty = self.parse_type()?;
        self.expect(&TokenKind::Eq)?;
        let value = self.parse_expr()?;
        self.expect(&TokenKind::Semicolon)?;

        Ok(Stmt::Let(LetStmt {
            name,
            is_mut,
            ty,
            value,
            destructure: None,
            span: start.merge(self.prev_span()),
        }))
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Return)?;

        let value = if self.check(&TokenKind::Semicolon) {
            // return; → return unit
            Expr {
                kind: ExprKind::Literal(Literal::Unit),
                span: self.current_span(),
            }
        } else {
            self.parse_expr()?
        };
        self.expect(&TokenKind::Semicolon)?;

        Ok(Stmt::Return(ReturnStmt {
            value,
            span: start.merge(self.prev_span()),
        }))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseError> {
        Ok(Stmt::If(self.parse_if_inner()?))
    }

    fn parse_if_inner(&mut self) -> Result<IfStmt, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::If)?;
        let condition = self.parse_expr()?;
        let then_block = self.parse_block()?;

        let else_branch = if self.check(&TokenKind::Else) {
            self.advance();
            if self.check(&TokenKind::If) {
                Some(Box::new(ElseBranch::ElseIf(self.parse_if_inner()?)))
            } else {
                Some(Box::new(ElseBranch::Else(self.parse_block()?)))
            }
        } else {
            None
        };

        Ok(IfStmt {
            condition,
            then_block,
            else_branch,
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::While)?;
        let condition = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(Stmt::While(WhileStmt {
            condition,
            body,
            span: start.merge(self.prev_span()),
        }))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::For)?;
        let var_name = self.expect_ident()?;
        self.expect(&TokenKind::In)?;
        let iterable = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(Stmt::For(ForStmt {
            var_name,
            iterable,
            body,
            span: start.merge(self.prev_span()),
        }))
    }

    fn parse_match_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Match)?;
        let subject = self.parse_expr()?;
        self.expect(&TokenKind::LBrace)?;

        let mut arms = Vec::new();
        while !self.check(&TokenKind::RBrace) {
            arms.push(self.parse_match_arm()?);
            // Optional trailing comma after the arm's block
            if self.check(&TokenKind::Comma) {
                self.advance();
            }
        }
        self.expect(&TokenKind::RBrace)?;

        Ok(Stmt::Match(MatchStmt {
            subject,
            arms,
            span: start.merge(self.prev_span()),
        }))
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
        let start = self.current_span();
        let pattern = self.parse_pattern()?;
        self.expect(&TokenKind::FatArrow)?;
        let body = self.parse_block()?;

        Ok(MatchArm {
            pattern,
            body,
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.peek_kind() {
            TokenKind::Underscore => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Wildcard(span))
            }
            TokenKind::IntLit(n) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(Literal::Int(n), span))
            }
            TokenKind::True => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(Literal::Bool(true), span))
            }
            TokenKind::False => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(Literal::Bool(false), span))
            }
            TokenKind::StringLit(ref s) => {
                let span = self.current_span();
                let s = s.clone();
                self.advance();
                Ok(Pattern::Literal(Literal::String(s), span))
            }
            TokenKind::CharLit(c) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(Literal::Char(c), span))
            }
            TokenKind::Ident(_) => {
                let span = self.current_span();
                let name = self.expect_ident()?;
                // Check if this is a variant pattern: Name(pattern, ...)
                if self.check(&TokenKind::LParen) {
                    self.advance();
                    let mut sub_patterns = Vec::new();
                    if !self.check(&TokenKind::RParen) {
                        sub_patterns.push(self.parse_pattern()?);
                        while self.check(&TokenKind::Comma) {
                            self.advance();
                            if self.check(&TokenKind::RParen) {
                                break;
                            }
                            sub_patterns.push(self.parse_pattern()?);
                        }
                    }
                    self.expect(&TokenKind::RParen)?;
                    Ok(Pattern::Variant(name, sub_patterns, span))
                } else {
                    Ok(Pattern::Binding(name, span))
                }
            }
            _ => Err(self.error("expected pattern")),
        }
    }

    fn parse_assign_or_expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        let expr = self.parse_expr()?;

        if self.check(&TokenKind::Eq) {
            self.advance();
            let value = self.parse_expr()?;
            self.expect(&TokenKind::Semicolon)?;
            Ok(Stmt::Assign(AssignStmt {
                target: expr,
                value,
                span: start.merge(self.prev_span()),
            }))
        } else if let Some(op) = self.try_compound_assign() {
            self.advance();
            let rhs = self.parse_expr()?;
            self.expect(&TokenKind::Semicolon)?;
            let end = self.prev_span();
            let value = Expr {
                kind: ExprKind::Binary(Box::new(expr.clone()), op, Box::new(rhs)),
                span: start.merge(end),
            };
            Ok(Stmt::Assign(AssignStmt {
                target: expr,
                value,
                span: start.merge(end),
            }))
        } else {
            self.expect(&TokenKind::Semicolon)?;
            Ok(Stmt::Expr(ExprStmt {
                expr,
                span: start.merge(self.prev_span()),
            }))
        }
    }

    // ── Expressions (Pratt parser) ───────────────────────────

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_unary()?;

        loop {
            // Range operators `..` and `..=` have lowest precedence (1, 2)
            if self.check(&TokenKind::DotDot) || self.check(&TokenKind::DotDotEq) {
                if 1 < min_bp {
                    break;
                }
                let is_inclusive = self.check(&TokenKind::DotDotEq);
                self.advance();
                // Unbounded range: `expr..` with no RHS
                if !is_inclusive && !self.can_start_expr() {
                    let span = lhs.span.merge(self.prev_span());
                    lhs = Expr {
                        kind: ExprKind::RangeFrom(Box::new(lhs)),
                        span,
                    };
                    continue;
                }
                let rhs = self.parse_expr_bp(2)?;
                let span = lhs.span.merge(rhs.span);
                lhs = Expr {
                    kind: if is_inclusive {
                        ExprKind::RangeInclusive(Box::new(lhs), Box::new(rhs))
                    } else {
                        ExprKind::Range(Box::new(lhs), Box::new(rhs))
                    },
                    span,
                };
                continue;
            }

            if let Some((op, width)) = self.try_binop() {
                let (l_bp, r_bp) = infix_binding_power(op);
                if l_bp < min_bp {
                    break;
                }

                // Consume operator token(s)
                for _ in 0..width {
                    self.advance();
                }
                let rhs = self.parse_expr_bp(r_bp)?;
                let span = lhs.span.merge(rhs.span);
                lhs = Expr {
                    kind: ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                    span,
                };
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();

        if self.check(&TokenKind::Minus) {
            self.advance();
            let operand = self.parse_unary()?;
            let span = start.merge(operand.span);
            return Ok(Expr {
                kind: ExprKind::Unary(UnaryOp::Neg, Box::new(operand)),
                span,
            });
        }

        if self.check(&TokenKind::Not) {
            self.advance();
            let operand = self.parse_unary()?;
            let span = start.merge(operand.span);
            return Ok(Expr {
                kind: ExprKind::Unary(UnaryOp::Not, Box::new(operand)),
                span,
            });
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.check(&TokenKind::LParen) {
                // Function call
                self.advance();
                let mut args = Vec::new();
                if !self.check(&TokenKind::RParen) {
                    args.push(self.parse_expr()?);
                    while self.check(&TokenKind::Comma) {
                        self.advance();
                        if self.check(&TokenKind::RParen) {
                            break;
                        }
                        args.push(self.parse_expr()?);
                    }
                }
                self.expect(&TokenKind::RParen)?;
                let span = expr.span.merge(self.prev_span());
                expr = Expr {
                    kind: ExprKind::Call(Box::new(expr), args),
                    span,
                };
            } else if self.check(&TokenKind::Dot) {
                // Field access, method call, or tuple index
                self.advance();
                // Check for tuple index: .0, .1, .2, ...
                if let TokenKind::IntLit(idx) = self.peek_kind() {
                    self.advance();
                    let span = expr.span.merge(self.prev_span());
                    expr = Expr {
                        kind: ExprKind::FieldAccess(Box::new(expr), idx.to_string()),
                        span,
                    };
                    continue;
                }
                let field = self.expect_ident()?;
                if self.check(&TokenKind::LParen) {
                    // Method call: expr.method(args)
                    self.advance();
                    let mut args = Vec::new();
                    if !self.check(&TokenKind::RParen) {
                        args.push(self.parse_expr()?);
                        while self.check(&TokenKind::Comma) {
                            self.advance();
                            if self.check(&TokenKind::RParen) {
                                break;
                            }
                            args.push(self.parse_expr()?);
                        }
                    }
                    self.expect(&TokenKind::RParen)?;
                    let span = expr.span.merge(self.prev_span());
                    expr = Expr {
                        kind: ExprKind::MethodCall(Box::new(expr), field, args),
                        span,
                    };
                } else {
                    let span = expr.span.merge(self.prev_span());
                    expr = Expr {
                        kind: ExprKind::FieldAccess(Box::new(expr), field),
                        span,
                    };
                }
            } else if self.check(&TokenKind::LBracket) {
                // Index
                self.advance();
                let index = self.parse_expr()?;
                self.expect(&TokenKind::RBracket)?;
                let span = expr.span.merge(self.prev_span());
                expr = Expr {
                    kind: ExprKind::Index(Box::new(expr), Box::new(index)),
                    span,
                };
            } else if self.check(&TokenKind::QuestionMark) {
                // Try operator: expr?
                let q_span = self.advance().span;
                let span = expr.span.merge(q_span);
                expr = Expr {
                    kind: ExprKind::Try(Box::new(expr)),
                    span,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();

        match self.peek_kind() {
            TokenKind::IntLit(n) => {
                self.advance();
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Int(n)),
                    span: start,
                })
            }
            TokenKind::FloatLit(n) => {
                self.advance();
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Float(n)),
                    span: start,
                })
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    span: start,
                })
            }
            TokenKind::False => {
                self.advance();
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Bool(false)),
                    span: start,
                })
            }
            TokenKind::StringLit(ref s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::String(s)),
                    span: start,
                })
            }
            TokenKind::InterpStringLit(ref parts) => {
                let parts = parts.clone();
                self.advance();
                self.desugar_interp_string(&parts, start)
            }
            TokenKind::CharLit(c) => {
                self.advance();
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Char(c)),
                    span: start,
                })
            }
            TokenKind::Ident(ref name) => {
                let name = name.clone();
                self.advance();

                // Check for struct initializer: Name { field: value, ... }
                if self.check(&TokenKind::LBrace) && self.is_struct_init_lookahead() {
                    self.advance(); // consume {
                    let mut fields = Vec::new();
                    while !self.check(&TokenKind::RBrace) {
                        let fstart = self.current_span();
                        let fname = self.expect_ident()?;
                        self.expect(&TokenKind::Colon)?;
                        let fvalue = self.parse_expr()?;
                        fields.push(FieldInit {
                            name: fname,
                            value: fvalue,
                            span: fstart.merge(self.prev_span()),
                        });
                        if !self.check(&TokenKind::RBrace) {
                            self.expect(&TokenKind::Comma)?;
                        }
                    }
                    self.expect(&TokenKind::RBrace)?;
                    let span = start.merge(self.prev_span());
                    Ok(Expr {
                        kind: ExprKind::StructInit(name, fields),
                        span,
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::Ident(name),
                        span: start,
                    })
                }
            }
            TokenKind::LBracket => {
                // Array literal: [expr, expr, ...] or array repeat: [expr; count]
                self.advance();
                let mut elements = Vec::new();
                if !self.check(&TokenKind::RBracket) {
                    elements.push(self.parse_expr()?);
                    // Check for [value; count] syntax
                    if self.check(&TokenKind::Semicolon) {
                        self.advance();
                        let count_expr = self.parse_expr()?;
                        self.expect(&TokenKind::RBracket)?;
                        let span = start.merge(self.prev_span());
                        return Ok(Expr {
                            kind: ExprKind::ArrayRepeat(
                                Box::new(elements.remove(0)),
                                Box::new(count_expr),
                            ),
                            span,
                        });
                    }
                    while self.check(&TokenKind::Comma) {
                        self.advance();
                        if self.check(&TokenKind::RBracket) {
                            break;
                        }
                        elements.push(self.parse_expr()?);
                    }
                }
                self.expect(&TokenKind::RBracket)?;
                let span = start.merge(self.prev_span());
                Ok(Expr {
                    kind: ExprKind::ArrayLit(elements),
                    span,
                })
            }
            TokenKind::Spawn => {
                self.advance();
                let block = self.parse_block()?;
                let span = start.merge(self.prev_span());
                Ok(Expr {
                    kind: ExprKind::Spawn(block),
                    span,
                })
            }
            TokenKind::Pipe => self.parse_closure_expr(),
            TokenKind::LParen => {
                self.advance();
                let first = self.parse_expr()?;
                if self.check(&TokenKind::Comma) {
                    // Tuple literal: (expr, expr, ...)
                    self.advance();
                    let mut elements = vec![first];
                    if !self.check(&TokenKind::RParen) {
                        elements.push(self.parse_expr()?);
                        while self.check(&TokenKind::Comma) {
                            self.advance();
                            if self.check(&TokenKind::RParen) {
                                break;
                            }
                            elements.push(self.parse_expr()?);
                        }
                    }
                    self.expect(&TokenKind::RParen)?;
                    let span = start.merge(self.prev_span());
                    Ok(Expr {
                        kind: ExprKind::TupleLit(elements),
                        span,
                    })
                } else {
                    // Grouping: (expr)
                    self.expect(&TokenKind::RParen)?;
                    Ok(first)
                }
            }
            _ => Err(self.error(&format!("expected expression, found {}", self.peek_kind()))),
        }
    }

    /// Desugar an interpolated string `"hello {expr}"` into
    /// `str_concat(str_concat("hello ", to_str(expr)), "")` chains.
    fn desugar_interp_string(
        &mut self,
        parts: &[InterpPart],
        span: Span,
    ) -> Result<Expr, ParseError> {
        let mut acc: Option<Expr> = None;

        for part in parts {
            let part_expr = match part {
                InterpPart::Literal(s) => Expr {
                    kind: ExprKind::Literal(Literal::String(s.clone())),
                    span,
                },
                InterpPart::Expr(tokens) => {
                    // Sub-parse the expression tokens
                    let mut sub_parser = Parser::new(tokens.clone());
                    let inner_expr = sub_parser.parse_expr()?;
                    if sub_parser.peek_kind() != TokenKind::EOF {
                        return Err(ParseError {
                            message: "unexpected token in interpolation expression".to_string(),
                            span,
                        });
                    }
                    // Wrap in to_str() call
                    Expr {
                        kind: ExprKind::Call(
                            Box::new(Expr {
                                kind: ExprKind::Ident("to_str".to_string()),
                                span,
                            }),
                            vec![inner_expr],
                        ),
                        span,
                    }
                }
            };

            acc = Some(match acc {
                None => part_expr,
                Some(prev) => Expr {
                    kind: ExprKind::Call(
                        Box::new(Expr {
                            kind: ExprKind::Ident("str_concat".to_string()),
                            span,
                        }),
                        vec![prev, part_expr],
                    ),
                    span,
                },
            });
        }

        // If no parts at all, return empty string
        Ok(acc.unwrap_or(Expr {
            kind: ExprKind::Literal(Literal::String(String::new())),
            span,
        }))
    }

    fn parse_closure_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Pipe)?;

        // Parse parameters: |x: int, y: float|
        let mut params = Vec::new();
        if !self.check(&TokenKind::Pipe) {
            params.push(self.parse_param()?);
            while self.check(&TokenKind::Comma) {
                self.advance();
                if self.check(&TokenKind::Pipe) {
                    break;
                }
                params.push(self.parse_param()?);
            }
        }
        self.expect(&TokenKind::Pipe)?;

        // Return type: -> Type
        self.expect(&TokenKind::Arrow)?;
        let return_type = self.parse_type()?;

        // Body: { ... }
        let body = self.parse_block()?;

        let span = start.merge(self.prev_span());
        Ok(Expr {
            kind: ExprKind::Closure(ClosureExpr {
                params,
                return_type,
                body,
                span,
            }),
            span,
        })
    }

    /// Disambiguate struct init from a block. If the token after `{` is `Ident :`,
    /// it's a struct init. This is a deterministic 2-token lookahead.
    fn is_struct_init_lookahead(&self) -> bool {
        if self.pos + 2 < self.tokens.len() {
            matches!(self.tokens[self.pos + 1].kind, TokenKind::Ident(_))
                && matches!(self.tokens[self.pos + 2].kind, TokenKind::Colon)
        } else if self.pos + 1 < self.tokens.len() {
            // Empty struct: Name {}
            matches!(self.tokens[self.pos + 1].kind, TokenKind::RBrace)
        } else {
            false
        }
    }

    fn try_compound_assign(&self) -> Option<BinOp> {
        match self.peek_kind() {
            TokenKind::PlusEq => Some(BinOp::Add),
            TokenKind::MinusEq => Some(BinOp::Sub),
            TokenKind::StarEq => Some(BinOp::Mul),
            TokenKind::SlashEq => Some(BinOp::Div),
            TokenKind::PercentEq => Some(BinOp::Mod),
            _ => None,
        }
    }

    fn try_binop(&self) -> Option<(BinOp, usize)> {
        match self.peek_kind() {
            TokenKind::Plus => Some((BinOp::Add, 1)),
            TokenKind::Minus => Some((BinOp::Sub, 1)),
            TokenKind::Star => Some((BinOp::Mul, 1)),
            TokenKind::Slash => Some((BinOp::Div, 1)),
            TokenKind::Percent => Some((BinOp::Mod, 1)),
            TokenKind::EqEq => Some((BinOp::Eq, 1)),
            TokenKind::NotEq => Some((BinOp::NotEq, 1)),
            TokenKind::Lt => Some((BinOp::Lt, 1)),
            TokenKind::Gt => {
                // >> = two Gt tokens (avoid conflict with generic closing >>)
                if self.peek_kind_at(self.pos + 1) == TokenKind::Gt
                    && self.peek_kind_at(self.pos + 2) != TokenKind::Eq
                {
                    Some((BinOp::Shr, 2))
                } else {
                    Some((BinOp::Gt, 1))
                }
            }
            TokenKind::LtEq => Some((BinOp::LtEq, 1)),
            TokenKind::GtEq => Some((BinOp::GtEq, 1)),
            TokenKind::And => Some((BinOp::And, 1)),
            TokenKind::Or => Some((BinOp::Or, 1)),
            TokenKind::Ampersand => Some((BinOp::BitAnd, 1)),
            TokenKind::Pipe => Some((BinOp::BitOr, 1)),
            TokenKind::Caret => Some((BinOp::BitXor, 1)),
            TokenKind::LShift => Some((BinOp::Shl, 1)),
            _ => None,
        }
    }

    fn peek_kind_at(&self, pos: usize) -> TokenKind {
        self.tokens
            .get(pos)
            .map(|t| t.kind.clone())
            .unwrap_or(TokenKind::EOF)
    }

    // ── Token utilities ──────────────────────────────────────

    fn peek_kind(&self) -> TokenKind {
        self.tokens
            .get(self.pos)
            .map(|t| t.kind.clone())
            .unwrap_or(TokenKind::EOF)
    }

    /// Check whether the current token can start an expression.
    /// Used to distinguish `expr..` (RangeFrom) from `expr..expr` (Range).
    fn can_start_expr(&self) -> bool {
        matches!(
            self.peek_kind(),
            TokenKind::IntLit(_)
                | TokenKind::FloatLit(_)
                | TokenKind::StringLit(_)
                | TokenKind::InterpStringLit(_)
                | TokenKind::CharLit(_)
                | TokenKind::Ident(_)
                | TokenKind::LParen
                | TokenKind::LBracket
                | TokenKind::Minus
                | TokenKind::Not
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Pipe
                | TokenKind::Spawn
        )
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|t| t.span)
            .unwrap_or(Span::synthetic())
    }

    fn prev_span(&self) -> Span {
        if self.pos > 0 {
            self.tokens[self.pos - 1].span
        } else {
            Span::synthetic()
        }
    }

    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(&self.peek_kind()) == std::mem::discriminant(kind)
    }

    fn advance(&mut self) -> Token {
        let tok = self.tokens[self.pos].clone();
        self.pos += 1;
        tok
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(self.error(&format!(
                "expected '{}', found '{}'",
                kind,
                self.peek_kind()
            )))
        }
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        match self.peek_kind() {
            TokenKind::Ident(name) => {
                self.advance();
                Ok(name)
            }
            _ => Err(self.error(&format!(
                "expected identifier, found '{}'",
                self.peek_kind()
            ))),
        }
    }

    fn error(&self, msg: &str) -> ParseError {
        ParseError {
            message: msg.to_string(),
            span: self.current_span(),
        }
    }
}

// ── Operator binding power (precedence) ──────────────────────

/// Returns (left_bp, right_bp). Higher = tighter binding.
/// Left < Right gives left-associativity.
fn infix_binding_power(op: BinOp) -> (u8, u8) {
    match op {
        BinOp::Or => (3, 4),
        BinOp::And => (5, 6),
        BinOp::BitOr => (7, 8),
        BinOp::BitXor => (9, 10),
        BinOp::BitAnd => (11, 12),
        BinOp::Eq | BinOp::NotEq => (13, 14),
        BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => (15, 16),
        BinOp::Shl | BinOp::Shr => (17, 18),
        BinOp::Add | BinOp::Sub => (19, 20),
        BinOp::Mul | BinOp::Div | BinOp::Mod => (21, 22),
    }
}

// ═══════════════════════════════════════════════════════════════
//  Tests
// ═══════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::lexer::Lexer;

    fn parse(input: &str) -> Program {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse_program().unwrap()
    }

    #[test]
    fn test_empty_program() {
        let prog = parse("");
        assert!(prog.module_name.is_none());
        assert!(prog.declarations.is_empty());
    }

    #[test]
    fn test_module_decl() {
        let prog = parse("module main;");
        assert_eq!(prog.module_name, Some("main".to_string()));
    }

    #[test]
    fn test_simple_function() {
        let prog = parse("fn main() -> int { return 0; }");
        assert_eq!(prog.declarations.len(), 1);
        match &prog.declarations[0] {
            Declaration::Function(f) => {
                assert_eq!(f.name, "main");
                assert_eq!(f.return_type, Type::Int);
                assert!(f.params.is_empty());
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_function_with_params() {
        let prog = parse("fn add(a: int, b: int) -> int { return a + b; }");
        match &prog.declarations[0] {
            Declaration::Function(f) => {
                assert_eq!(f.params.len(), 2);
                assert_eq!(f.params[0].name, "a");
                assert_eq!(f.params[1].name, "b");
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_struct_decl() {
        let prog = parse("struct Point { x: int, y: int }");
        match &prog.declarations[0] {
            Declaration::Struct(s) => {
                assert_eq!(s.name, "Point");
                assert_eq!(s.fields.len(), 2);
            }
            _ => panic!("expected struct"),
        }
    }

    #[test]
    fn test_enum_decl() {
        let prog = parse("enum Color { Red, Green, Blue }");
        match &prog.declarations[0] {
            Declaration::Enum(e) => {
                assert_eq!(e.name, "Color");
                assert_eq!(e.variants.len(), 3);
            }
            _ => panic!("expected enum"),
        }
    }

    #[test]
    fn test_pure_function() {
        let prog = parse("pure fn add(a: int, b: int) -> int { return a + b; }");
        match &prog.declarations[0] {
            Declaration::Function(f) => {
                assert!(f.is_pure);
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_contract_clauses() {
        let prog =
            parse("fn abs(x: int) -> int requires x > -100 ensures result >= 0 { return x; }");
        match &prog.declarations[0] {
            Declaration::Function(f) => {
                assert_eq!(f.contracts.len(), 2);
                assert!(matches!(f.contracts[0], Contract::Requires(_)));
                assert!(matches!(f.contracts[1], Contract::Ensures(_)));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_if_else() {
        let prog = parse("fn f(x: int) -> int { if x > 0 { return x; } else { return 0 - x; } }");
        match &prog.declarations[0] {
            Declaration::Function(f) => {
                assert_eq!(f.body.stmts.len(), 1);
                assert!(matches!(f.body.stmts[0], Stmt::If(_)));
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_operator_precedence() {
        let prog = parse("fn f() -> int { return 1 + 2 * 3; }");
        match &prog.declarations[0] {
            Declaration::Function(f) => {
                if let Stmt::Return(ret) = &f.body.stmts[0] {
                    // Should parse as 1 + (2 * 3)
                    if let ExprKind::Binary(_, BinOp::Add, rhs) = &ret.value.kind {
                        assert!(matches!(rhs.kind, ExprKind::Binary(_, BinOp::Mul, _)));
                    } else {
                        panic!("expected addition at top level");
                    }
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_ast_json_roundtrip() {
        let prog = parse("fn main() -> int { return 42; }");
        let json = serde_json::to_string(&prog).unwrap();
        let _: Program = serde_json::from_str(&json).unwrap();
    }
}
