//! Parser for the SageSyn Agent Programming Language.
//!
//! This crate provides parsing for `.ssag` files, converting tokens into an Abstract Syntax Tree (AST).
//!
//! # Example
//!
//! ```
//! use ssag_parser::Parser;
//!
//! let source = r#"agent MyAgent { description: "Hello" }"#;
//! let program = Parser::parse(source).expect("parse error");
//!
//! println!("{:?}", program);
//! ```

pub mod ast;

pub use ast::*;
pub use ssag_lexer::Span;

use miette::{Diagnostic, SourceSpan};
use ssag_lexer::{Lexer, SpannedToken, Token};
use thiserror::Error;

/// Parser error.
#[derive(Error, Diagnostic, Debug, Clone)]
#[error("{message}")]
#[diagnostic(code(ssag::parser::error))]
pub struct ParseError {
    message: String,
    #[source_code]
    src: String,
    #[label("{message}")]
    span: SourceSpan,
}

impl ParseError {
    pub fn new(message: impl Into<String>, src: &str, span: Span) -> Self {
        Self {
            message: message.into(),
            src: src.to_string(),
            span: (span.start, span.len()).into(),
        }
    }

    pub fn expected(expected: &str, found: &Token, src: &str, span: Span) -> Self {
        Self::new(format!("expected {}, found `{}`", expected, found), src, span)
    }

    pub fn unexpected_eof(src: &str) -> Self {
        let span = Span::new(src.len(), src.len());
        Self::new("unexpected end of file", src, span)
    }
}

/// The SageSyn parser.
pub struct Parser<'src> {
    source: &'src str,
    tokens: Vec<SpannedToken>,
    pos: usize,
}

impl<'src> Parser<'src> {
    /// Create a new parser for the given source code.
    pub fn new(source: &'src str) -> Result<Self, ssag_lexer::LexerError> {
        let lexer = Lexer::new(source);
        let tokens: Vec<_> = lexer
            .filter_map(|r| r.ok())
            .filter(|t| !matches!(t.token, Token::LineComment(_) | Token::BlockComment(_)))
            .collect();

        Ok(Self {
            source,
            tokens,
            pos: 0,
        })
    }

    /// Parse the source code into a program AST.
    pub fn parse(source: &str) -> Result<Program, ParseError> {
        let mut parser = Parser::new(source).map_err(|e| {
            ParseError::new(
                format!("lexer error: {}", e),
                source,
                Span::new(0, source.len().min(1)),
            )
        })?;
        parser.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let start = self.pos;
        let mut items = Vec::new();

        while !self.is_at_end() {
            items.push(self.parse_item()?);
        }

        let span = if items.is_empty() {
            Span::new(0, 0)
        } else {
            Span::new(start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(0))
        };

        Ok(Program { items, span })
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        match self.peek_token() {
            Some(Token::Agent) => Ok(Item::Agent(self.parse_agent()?)),
            Some(Token::Skill) => Ok(Item::Skill(self.parse_skill()?)),
            Some(Token::Type) => Ok(Item::TypeDef(self.parse_type_def()?)),
            Some(Token::Fn) | Some(Token::Async) => Ok(Item::Function(self.parse_function()?)),
            Some(token) => {
                let span = self.current_span();
                Err(ParseError::new(
                    format!("expected item (agent, skill, type, fn), found `{}`", token),
                    self.source,
                    span,
                ))
            }
            None => Err(ParseError::unexpected_eof(self.source)),
        }
    }

    fn parse_agent(&mut self) -> Result<Agent, ParseError> {
        let start_span = self.expect(Token::Agent)?;
        let name = self.parse_identifier()?;
        self.expect(Token::LBrace)?;

        let mut description = None;
        let mut version = None;
        let mut model = None;
        let mut state = None;
        let mut protocols = None;
        let mut tools = Vec::new();
        let mut handlers = Vec::new();

        while !self.check(&Token::RBrace) && !self.is_at_end() {
            match self.peek_token().cloned() {
                Some(Token::Identifier(ref name)) if name == "description" => {
                    self.advance();
                    self.expect(Token::Colon)?;
                    description = Some(self.parse_string_literal()?);
                }
                Some(Token::Identifier(ref name)) if name == "version" => {
                    self.advance();
                    self.expect(Token::Colon)?;
                    version = Some(self.parse_string_literal()?);
                }
                Some(Token::Model) => {
                    self.advance();
                    self.expect(Token::Colon)?;
                    model = Some(self.parse_model_config()?);
                }
                Some(Token::State) => {
                    state = Some(self.parse_state_block()?);
                }
                Some(Token::Protocols) => {
                    self.advance();
                    self.expect(Token::Colon)?;
                    protocols = Some(self.parse_protocols_block()?);
                }
                Some(Token::Tool) => {
                    tools.push(self.parse_tool()?);
                }
                Some(Token::On) => {
                    handlers.push(self.parse_event_handler()?);
                }
                Some(token) => {
                    let span = self.current_span();
                    return Err(ParseError::new(
                        format!("unexpected token in agent body: `{}`", token),
                        self.source,
                        span,
                    ));
                }
                None => return Err(ParseError::unexpected_eof(self.source)),
            }
        }

        let end_span = self.expect(Token::RBrace)?;

        Ok(Agent {
            name,
            description,
            version,
            model,
            state,
            protocols,
            tools,
            handlers,
            span: Span::new(start_span.start, end_span.end),
        })
    }

    fn parse_skill(&mut self) -> Result<Skill, ParseError> {
        let start_span = self.expect(Token::Skill)?;
        let name = self.parse_identifier()?;
        self.expect(Token::LBrace)?;

        let mut description = None;
        let mut body = Vec::new();

        while !self.check(&Token::RBrace) && !self.is_at_end() {
            if let Some(Token::Identifier(name)) = self.peek_token() {
                if name == "description" {
                    self.advance();
                    self.expect(Token::Colon)?;
                    description = Some(self.parse_string_literal()?);
                    continue;
                }
            }
            body.push(self.parse_item()?);
        }

        let end_span = self.expect(Token::RBrace)?;

        Ok(Skill {
            name,
            description,
            body,
            span: Span::new(start_span.start, end_span.end),
        })
    }

    fn parse_type_def(&mut self) -> Result<TypeDef, ParseError> {
        let start_span = self.expect(Token::Type)?;
        let name = self.parse_identifier()?;

        let mut generics = Vec::new();
        if self.check(&Token::LAngle) {
            self.advance();
            loop {
                generics.push(self.parse_identifier()?);
                if !self.check(&Token::Comma) {
                    break;
                }
                self.advance();
            }
            self.expect(Token::RAngle)?;
        }

        let kind = if self.check(&Token::LBrace) {
            self.advance();
            let mut fields = Vec::new();
            while !self.check(&Token::RBrace) && !self.is_at_end() {
                fields.push(self.parse_field()?);
            }
            self.expect(Token::RBrace)?;
            TypeDefKind::Struct(fields)
        } else {
            self.expect(Token::Eq)?;
            let ty = self.parse_type_expr()?;
            TypeDefKind::Alias(ty)
        };

        let end = self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end);

        Ok(TypeDef {
            name,
            generics,
            kind,
            span: Span::new(start_span.start, end),
        })
    }

    fn parse_function(&mut self) -> Result<Function, ParseError> {
        let start_span = self.current_span();
        let is_async = self.check(&Token::Async);
        if is_async {
            self.advance();
        }

        self.expect(Token::Fn)?;
        let name = self.parse_identifier()?;

        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;

        let return_type = if self.check(&Token::Arrow) {
            self.advance();
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(Function {
            is_async,
            name,
            params,
            return_type,
            body,
            span: Span::new(start_span.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end)),
        })
    }

    fn parse_model_config(&mut self) -> Result<ModelConfig, ParseError> {
        let start_span = self.current_span();
        let mut provider = None;
        let mut name = None;
        let mut context_window = None;
        let mut temperature = None;

        // Simple key-value parsing for model config
        while !self.is_at_end() && !self.check(&Token::State) && !self.check(&Token::Protocols) && !self.check(&Token::Tool) && !self.check(&Token::On) && !self.check(&Token::RBrace) {
            if let Some(Token::Identifier(key)) = self.peek_token().cloned() {
                self.advance();
                self.expect(Token::Colon)?;
                match key.as_str() {
                    "provider" => provider = Some(self.parse_string_literal()?),
                    "name" => name = Some(self.parse_string_literal()?),
                    "context_window" => context_window = Some(self.parse_number_literal()?),
                    "temperature" => temperature = Some(self.parse_number_literal()?),
                    _ => {
                        // Skip unknown fields
                        self.advance();
                    }
                }
            } else {
                break;
            }
        }

        Ok(ModelConfig {
            provider,
            name,
            context_window,
            temperature,
            span: Span::new(start_span.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end)),
        })
    }

    fn parse_state_block(&mut self) -> Result<StateBlock, ParseError> {
        let start_span = self.expect(Token::State)?;
        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            fields.push(self.parse_field()?);
        }

        let end_span = self.expect(Token::RBrace)?;

        Ok(StateBlock {
            fields,
            span: Span::new(start_span.start, end_span.end),
        })
    }

    fn parse_protocols_block(&mut self) -> Result<ProtocolsBlock, ParseError> {
        let start_span = self.current_span();
        // Stub: just skip until we hit something else
        // Full implementation would parse mcp, a2a, ag_ui blocks

        Ok(ProtocolsBlock {
            mcp: None,
            a2a: None,
            ag_ui: None,
            span: start_span,
        })
    }

    fn parse_tool(&mut self) -> Result<Tool, ParseError> {
        let start_span = self.expect(Token::Tool)?;
        let name = self.parse_identifier()?;

        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;

        let return_type = if self.check(&Token::Arrow) {
            self.advance();
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect(Token::LBrace)?;

        let mut description = None;
        let mut mcp_server = None;
        let mut mcp_tool = None;
        let mut body = None;

        while !self.check(&Token::RBrace) && !self.is_at_end() {
            if let Some(Token::Identifier(key)) = self.peek_token().cloned() {
                match key.as_str() {
                    "description" => {
                        self.advance();
                        self.expect(Token::Colon)?;
                        description = Some(self.parse_string_literal()?);
                    }
                    "mcp_server" => {
                        self.advance();
                        self.expect(Token::Colon)?;
                        mcp_server = Some(self.parse_identifier()?);
                    }
                    "mcp_tool" => {
                        self.advance();
                        self.expect(Token::Colon)?;
                        mcp_tool = Some(self.parse_identifier()?);
                    }
                    _ => {
                        // Try to parse as return statement
                        if self.check(&Token::Return) {
                            // Parse body block
                            let mut stmts = Vec::new();
                            while !self.check(&Token::RBrace) && !self.is_at_end() {
                                stmts.push(self.parse_stmt()?);
                            }
                            body = Some(Block {
                                stmts,
                                span: self.current_span(),
                            });
                            break;
                        }
                        self.advance();
                    }
                }
            } else if self.check(&Token::Return) {
                let mut stmts = Vec::new();
                while !self.check(&Token::RBrace) && !self.is_at_end() {
                    stmts.push(self.parse_stmt()?);
                }
                body = Some(Block {
                    stmts,
                    span: self.current_span(),
                });
                break;
            } else {
                break;
            }
        }

        let end_span = self.expect(Token::RBrace)?;

        Ok(Tool {
            name,
            params,
            return_type,
            description,
            mcp_server,
            mcp_tool,
            body,
            span: Span::new(start_span.start, end_span.end),
        })
    }

    fn parse_event_handler(&mut self) -> Result<EventHandler, ParseError> {
        let start_span = self.expect(Token::On)?;
        let event = self.parse_identifier()?;
        let body = self.parse_block()?;

        Ok(EventHandler {
            event,
            body,
            span: Span::new(start_span.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end)),
        })
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start_span = self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();

        while !self.check(&Token::RBrace) && !self.is_at_end() {
            stmts.push(self.parse_stmt()?);
        }

        let end_span = self.expect(Token::RBrace)?;

        Ok(Block {
            stmts,
            span: Span::new(start_span.start, end_span.end),
        })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match self.peek_token() {
            Some(Token::Let) => Ok(Stmt::Let(self.parse_let_stmt()?)),
            Some(Token::Var) => Ok(Stmt::Var(self.parse_var_stmt()?)),
            Some(Token::If) => Ok(Stmt::If(self.parse_if_stmt()?)),
            Some(Token::For) => Ok(Stmt::For(self.parse_for_stmt()?)),
            Some(Token::While) => Ok(Stmt::While(self.parse_while_stmt()?)),
            Some(Token::Return) => Ok(Stmt::Return(self.parse_return_stmt()?)),
            Some(Token::Emit) => Ok(Stmt::Emit(self.parse_emit_stmt()?)),
            Some(Token::LBrace) => Ok(Stmt::Block(self.parse_block()?)),
            _ => Ok(Stmt::Expr(self.parse_expr_stmt()?)),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<LetStmt, ParseError> {
        let start_span = self.expect(Token::Let)?;
        let name = self.parse_identifier()?;

        let ty = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect(Token::Eq)?;
        let value = self.parse_expr()?;

        Ok(LetStmt {
            name,
            ty,
            value,
            span: Span::new(start_span.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end)),
        })
    }

    fn parse_var_stmt(&mut self) -> Result<VarStmt, ParseError> {
        let start_span = self.expect(Token::Var)?;
        let name = self.parse_identifier()?;

        let ty = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect(Token::Eq)?;
        let value = self.parse_expr()?;

        Ok(VarStmt {
            name,
            ty,
            value,
            span: Span::new(start_span.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end)),
        })
    }

    fn parse_if_stmt(&mut self) -> Result<IfStmt, ParseError> {
        let start_span = self.expect(Token::If)?;
        let condition = self.parse_expr()?;
        let then_block = self.parse_block()?;

        let else_block = if self.check(&Token::Else) {
            self.advance();
            if self.check(&Token::If) {
                Some(Box::new(ElseClause::ElseIf(self.parse_if_stmt()?)))
            } else {
                Some(Box::new(ElseClause::Else(self.parse_block()?)))
            }
        } else {
            None
        };

        Ok(IfStmt {
            condition,
            then_block,
            else_block,
            span: Span::new(start_span.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end)),
        })
    }

    fn parse_for_stmt(&mut self) -> Result<ForStmt, ParseError> {
        let start_span = self.expect(Token::For)?;
        let binding = self.parse_identifier()?;
        self.expect(Token::In)?;
        let iterable = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(ForStmt {
            binding,
            iterable,
            body,
            span: Span::new(start_span.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end)),
        })
    }

    fn parse_while_stmt(&mut self) -> Result<WhileStmt, ParseError> {
        let start_span = self.expect(Token::While)?;
        let condition = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(WhileStmt {
            condition,
            body,
            span: Span::new(start_span.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end)),
        })
    }

    fn parse_return_stmt(&mut self) -> Result<ReturnStmt, ParseError> {
        let start_span = self.expect(Token::Return)?;

        let value = if !self.check(&Token::RBrace) && !self.is_at_end() {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(ReturnStmt {
            value,
            span: Span::new(start_span.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end)),
        })
    }

    fn parse_emit_stmt(&mut self) -> Result<EmitStmt, ParseError> {
        let start_span = self.expect(Token::Emit)?;
        let event = self.parse_identifier()?;
        self.expect(Token::LParen)?;
        let value = self.parse_expr()?;
        self.expect(Token::RParen)?;

        Ok(EmitStmt {
            event,
            value,
            span: Span::new(start_span.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start_span.end)),
        })
    }

    fn parse_expr_stmt(&mut self) -> Result<ExprStmt, ParseError> {
        let start = self.current_span();
        let expr = self.parse_expr()?;

        Ok(ExprStmt {
            expr,
            span: Span::new(start.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start.end)),
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_or()?;

        if self.check(&Token::Eq) {
            self.advance();
            let value = self.parse_assignment()?;
            let span = Span::new(
                self.expr_span(&expr).start,
                self.expr_span(&value).end,
            );
            return Ok(Expr::Assign(AssignExpr {
                target: Box::new(expr),
                value: Box::new(value),
                span,
            }));
        }

        Ok(expr)
    }

    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_and()?;

        while self.check(&Token::OrOr) {
            self.advance();
            let right = self.parse_and()?;
            let span = Span::new(self.expr_span(&left).start, self.expr_span(&right).end);
            left = Expr::Binary(BinaryExpr {
                left: Box::new(left),
                op: BinaryOp::Or,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_equality()?;

        while self.check(&Token::AndAnd) {
            self.advance();
            let right = self.parse_equality()?;
            let span = Span::new(self.expr_span(&left).start, self.expr_span(&right).end);
            left = Expr::Binary(BinaryExpr {
                left: Box::new(left),
                op: BinaryOp::And,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_comparison()?;

        while let Some(op) = match self.peek_token() {
            Some(Token::EqEq) => Some(BinaryOp::Eq),
            Some(Token::NotEq) => Some(BinaryOp::NotEq),
            _ => None,
        } {
            self.advance();
            let right = self.parse_comparison()?;
            let span = Span::new(self.expr_span(&left).start, self.expr_span(&right).end);
            left = Expr::Binary(BinaryExpr {
                left: Box::new(left),
                op,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_term()?;

        while let Some(op) = match self.peek_token() {
            Some(Token::LAngle) => Some(BinaryOp::Lt),
            Some(Token::RAngle) => Some(BinaryOp::Gt),
            Some(Token::LtEq) => Some(BinaryOp::LtEq),
            Some(Token::GtEq) => Some(BinaryOp::GtEq),
            _ => None,
        } {
            self.advance();
            let right = self.parse_term()?;
            let span = Span::new(self.expr_span(&left).start, self.expr_span(&right).end);
            left = Expr::Binary(BinaryExpr {
                left: Box::new(left),
                op,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_factor()?;

        while let Some(op) = match self.peek_token() {
            Some(Token::Plus) => Some(BinaryOp::Add),
            Some(Token::Minus) => Some(BinaryOp::Sub),
            _ => None,
        } {
            self.advance();
            let right = self.parse_factor()?;
            let span = Span::new(self.expr_span(&left).start, self.expr_span(&right).end);
            left = Expr::Binary(BinaryExpr {
                left: Box::new(left),
                op,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?;

        while let Some(op) = match self.peek_token() {
            Some(Token::Star) => Some(BinaryOp::Mul),
            Some(Token::Slash) => Some(BinaryOp::Div),
            Some(Token::Percent) => Some(BinaryOp::Mod),
            _ => None,
        } {
            self.advance();
            let right = self.parse_unary()?;
            let span = Span::new(self.expr_span(&left).start, self.expr_span(&right).end);
            left = Expr::Binary(BinaryExpr {
                left: Box::new(left),
                op,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op) = match self.peek_token() {
            Some(Token::Bang) => Some(UnaryOp::Not),
            Some(Token::Minus) => Some(UnaryOp::Neg),
            _ => None,
        } {
            let start = self.current_span();
            self.advance();
            let operand = self.parse_unary()?;
            let span = Span::new(start.start, self.expr_span(&operand).end);
            return Ok(Expr::Unary(UnaryExpr {
                op,
                operand: Box::new(operand),
                span,
            }));
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.check(&Token::LParen) {
                self.advance();
                let mut args = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        args.push(self.parse_expr()?);
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                let end = self.expect(Token::RParen)?;
                let span = Span::new(self.expr_span(&expr).start, end.end);
                expr = Expr::Call(CallExpr {
                    callee: Box::new(expr),
                    args,
                    span,
                });
            } else if self.check(&Token::Dot) {
                self.advance();
                let property = self.parse_identifier()?;
                let span = Span::new(self.expr_span(&expr).start, property.span.end);
                expr = Expr::Member(MemberExpr {
                    object: Box::new(expr),
                    property,
                    span,
                });
            } else if self.check(&Token::LBracket) {
                self.advance();
                let index = self.parse_expr()?;
                let end = self.expect(Token::RBracket)?;
                let span = Span::new(self.expr_span(&expr).start, end.end);
                expr = Expr::Index(IndexExpr {
                    object: Box::new(expr),
                    index: Box::new(index),
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek_token().cloned() {
            Some(Token::StringLiteral(s)) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::String(StringLit { value: s, span })))
            }
            Some(Token::TemplateLiteral(s)) => {
                let span = self.current_span();
                self.advance();
                // Simple template literal handling - just treat as string for now
                Ok(Expr::Template(TemplateExpr {
                    parts: vec![TemplatePart::String(s)],
                    span,
                }))
            }
            Some(Token::NumberLiteral(n)) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::Number(NumberLit { value: n, span })))
            }
            Some(Token::True) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::Boolean(BoolLit { value: true, span })))
            }
            Some(Token::False) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::Boolean(BoolLit { value: false, span })))
            }
            Some(Token::Null) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::Null(span)))
            }
            Some(Token::Identifier(_)) => {
                let ident = self.parse_identifier()?;
                Ok(Expr::Identifier(ident))
            }
            Some(Token::Await) => {
                let start = self.current_span();
                self.advance();
                let expr = self.parse_unary()?;
                let span = Span::new(start.start, self.expr_span(&expr).end);
                Ok(Expr::Await(AwaitExpr {
                    expr: Box::new(expr),
                    span,
                }))
            }
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Some(Token::LBracket) => {
                let start = self.current_span();
                self.advance();
                let mut elements = Vec::new();
                if !self.check(&Token::RBracket) {
                    loop {
                        elements.push(self.parse_expr()?);
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                let end = self.expect(Token::RBracket)?;
                Ok(Expr::Array(ArrayExpr {
                    elements,
                    span: Span::new(start.start, end.end),
                }))
            }
            Some(Token::LBrace) => {
                let start = self.current_span();
                self.advance();
                let mut fields = Vec::new();
                if !self.check(&Token::RBrace) {
                    loop {
                        let key = self.parse_identifier()?;
                        self.expect(Token::Colon)?;
                        let value = self.parse_expr()?;
                        fields.push((key, value));
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                let end = self.expect(Token::RBrace)?;
                Ok(Expr::Record(RecordExpr {
                    fields,
                    span: Span::new(start.start, end.end),
                }))
            }
            Some(token) => {
                let span = self.current_span();
                Err(ParseError::new(
                    format!("expected expression, found `{}`", token),
                    self.source,
                    span,
                ))
            }
            None => Err(ParseError::unexpected_eof(self.source)),
        }
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        let ty = self.parse_primary_type()?;

        // Check for optional shorthand (?)
        if self.check(&Token::Question) {
            self.advance();
            return Ok(TypeExpr::Optional(Box::new(ty)));
        }

        // Check for array shorthand ([])
        if self.check(&Token::LBracket) {
            self.advance();
            self.expect(Token::RBracket)?;
            let span = match &ty {
                TypeExpr::Named(n) => n.span,
                _ => Span::default(),
            };
            return Ok(TypeExpr::Array(ArrayType {
                element: Box::new(ty),
                span,
            }));
        }

        // Check for union types (|)
        if self.check(&Token::Pipe) {
            let mut types = vec![ty];
            while self.check(&Token::Pipe) {
                self.advance();
                types.push(self.parse_primary_type()?);
            }
            return Ok(TypeExpr::Union(types));
        }

        Ok(ty)
    }

    fn parse_primary_type(&mut self) -> Result<TypeExpr, ParseError> {
        match self.peek_token().cloned() {
            Some(Token::StringType) | Some(Token::NumberType) | Some(Token::BooleanType) | Some(Token::TimestampType) => {
                let span = self.current_span();
                let name = match self.peek_token() {
                    Some(Token::StringType) => "string",
                    Some(Token::NumberType) => "number",
                    Some(Token::BooleanType) => "boolean",
                    Some(Token::TimestampType) => "timestamp",
                    _ => unreachable!(),
                };
                self.advance();
                Ok(TypeExpr::Named(NamedType {
                    name: Identifier::new(name, span),
                    args: Vec::new(),
                    span,
                }))
            }
            Some(Token::ArrayType) => {
                let span = self.current_span();
                self.advance();
                self.expect(Token::LAngle)?;
                let element = self.parse_type_expr()?;
                self.expect(Token::RAngle)?;
                Ok(TypeExpr::Array(ArrayType {
                    element: Box::new(element),
                    span,
                }))
            }
            Some(Token::RecordType) => {
                let span = self.current_span();
                self.advance();
                self.expect(Token::LAngle)?;
                let key = self.parse_type_expr()?;
                self.expect(Token::Comma)?;
                let value = self.parse_type_expr()?;
                self.expect(Token::RAngle)?;
                Ok(TypeExpr::Record(RecordType {
                    key: Box::new(key),
                    value: Box::new(value),
                    span,
                }))
            }
            Some(Token::TupleType) => {
                let span = self.current_span();
                self.advance();
                self.expect(Token::LAngle)?;
                let mut elements = Vec::new();
                loop {
                    elements.push(self.parse_type_expr()?);
                    if !self.check(&Token::Comma) {
                        break;
                    }
                    self.advance();
                }
                self.expect(Token::RAngle)?;
                Ok(TypeExpr::Tuple(TupleType { elements, span }))
            }
            Some(Token::OptionalType) => {
                self.advance();
                self.expect(Token::LAngle)?;
                let inner = self.parse_type_expr()?;
                self.expect(Token::RAngle)?;
                Ok(TypeExpr::Optional(Box::new(inner)))
            }
            Some(Token::Identifier(name)) => {
                let span = self.current_span();
                self.advance();
                let mut args = Vec::new();
                if self.check(&Token::LAngle) {
                    self.advance();
                    loop {
                        args.push(self.parse_type_expr()?);
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                    self.expect(Token::RAngle)?;
                }
                Ok(TypeExpr::Named(NamedType {
                    name: Identifier::new(name, span),
                    args,
                    span,
                }))
            }
            Some(Token::LParen) => {
                // Function type: (A, B) -> C
                let span = self.current_span();
                self.advance();
                let mut params = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        params.push(self.parse_type_expr()?);
                        if !self.check(&Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.expect(Token::RParen)?;
                self.expect(Token::Arrow)?;
                let return_type = self.parse_type_expr()?;
                Ok(TypeExpr::Function(FunctionType {
                    params,
                    return_type: Box::new(return_type),
                    span,
                }))
            }
            Some(token) => {
                let span = self.current_span();
                Err(ParseError::new(
                    format!("expected type, found `{}`", token),
                    self.source,
                    span,
                ))
            }
            None => Err(ParseError::unexpected_eof(self.source)),
        }
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                params.push(self.parse_param()?);
                if !self.check(&Token::Comma) {
                    break;
                }
                self.advance();
            }
        }
        Ok(params)
    }

    fn parse_param(&mut self) -> Result<Param, ParseError> {
        let start = self.current_span();
        let name = self.parse_identifier()?;
        self.expect(Token::Colon)?;
        let ty = self.parse_type_expr()?;

        let default = if self.check(&Token::Eq) {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Param {
            name,
            ty,
            default,
            span: Span::new(start.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start.end)),
        })
    }

    fn parse_field(&mut self) -> Result<Field, ParseError> {
        let start = self.current_span();
        let name = self.parse_identifier()?;

        let optional = self.check(&Token::Question);
        if optional {
            self.advance();
        }

        self.expect(Token::Colon)?;
        let ty = self.parse_type_expr()?;

        Ok(Field {
            name,
            ty,
            optional,
            span: Span::new(start.start, self.tokens.get(self.pos.saturating_sub(1)).map(|t| t.span.end).unwrap_or(start.end)),
        })
    }

    fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        match self.peek_token().cloned() {
            Some(Token::Identifier(name)) => {
                let span = self.current_span();
                self.advance();
                Ok(Identifier::new(name, span))
            }
            Some(token) => {
                let span = self.current_span();
                Err(ParseError::expected("identifier", &token, self.source, span))
            }
            None => Err(ParseError::unexpected_eof(self.source)),
        }
    }

    fn parse_string_literal(&mut self) -> Result<StringLit, ParseError> {
        match self.peek_token().cloned() {
            Some(Token::StringLiteral(value)) => {
                let span = self.current_span();
                self.advance();
                Ok(StringLit { value, span })
            }
            Some(token) => {
                let span = self.current_span();
                Err(ParseError::expected("string literal", &token, self.source, span))
            }
            None => Err(ParseError::unexpected_eof(self.source)),
        }
    }

    fn parse_number_literal(&mut self) -> Result<NumberLit, ParseError> {
        match self.peek_token().cloned() {
            Some(Token::NumberLiteral(value)) => {
                let span = self.current_span();
                self.advance();
                Ok(NumberLit { value, span })
            }
            Some(token) => {
                let span = self.current_span();
                Err(ParseError::expected("number literal", &token, self.source, span))
            }
            None => Err(ParseError::unexpected_eof(self.source)),
        }
    }

    // Helper methods

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|t| &t.token)
    }

    fn current_span(&self) -> Span {
        self.tokens.get(self.pos).map(|t| t.span).unwrap_or_default()
    }

    fn check(&self, token: &Token) -> bool {
        self.peek_token().map(|t| std::mem::discriminant(t) == std::mem::discriminant(token)).unwrap_or(false)
    }

    fn advance(&mut self) -> Option<&SpannedToken> {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.tokens.get(self.pos.saturating_sub(1))
    }

    fn expect(&mut self, expected: Token) -> Result<Span, ParseError> {
        if self.check(&expected) {
            let span = self.current_span();
            self.advance();
            Ok(span)
        } else {
            let span = self.current_span();
            match self.peek_token() {
                Some(found) => Err(ParseError::expected(&format!("{}", expected), found, self.source, span)),
                None => Err(ParseError::unexpected_eof(self.source)),
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn expr_span(&self, expr: &Expr) -> Span {
        match expr {
            Expr::Literal(Literal::String(s)) => s.span,
            Expr::Literal(Literal::Number(n)) => n.span,
            Expr::Literal(Literal::Boolean(b)) => b.span,
            Expr::Literal(Literal::Null(span)) => *span,
            Expr::Identifier(i) => i.span,
            Expr::Binary(b) => b.span,
            Expr::Unary(u) => u.span,
            Expr::Call(c) => c.span,
            Expr::Member(m) => m.span,
            Expr::Index(i) => i.span,
            Expr::Array(a) => a.span,
            Expr::Record(r) => r.span,
            Expr::Await(a) => a.span,
            Expr::Arrow(a) => a.span,
            Expr::Match(m) => m.span,
            Expr::Template(t) => t.span,
            Expr::Assign(a) => a.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty_agent() {
        let source = "agent MyAgent { }";
        let program = Parser::parse(source).unwrap();

        assert_eq!(program.items.len(), 1);
        match &program.items[0] {
            Item::Agent(agent) => {
                assert_eq!(agent.name.name, "MyAgent");
            }
            _ => panic!("expected agent"),
        }
    }

    #[test]
    fn test_parse_agent_with_description() {
        let source = r#"agent MyAgent { description: "A test agent" }"#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Agent(agent) => {
                assert_eq!(agent.description.as_ref().unwrap().value, "A test agent");
            }
            _ => panic!("expected agent"),
        }
    }

    #[test]
    fn test_parse_type_def() {
        let source = r#"
            type User {
                name: string
                age: number
                email?: string
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::TypeDef(typedef) => {
                assert_eq!(typedef.name.name, "User");
                match &typedef.kind {
                    TypeDefKind::Struct(fields) => {
                        assert_eq!(fields.len(), 3);
                        assert_eq!(fields[0].name.name, "name");
                        assert!(!fields[0].optional);
                        assert!(fields[2].optional);
                    }
                    _ => panic!("expected struct"),
                }
            }
            _ => panic!("expected type def"),
        }
    }

    #[test]
    fn test_parse_function() {
        let source = r#"
            fn greet(name: string) -> string {
                return `Hello, ${name}!`
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                assert_eq!(func.name.name, "greet");
                assert!(!func.is_async);
                assert_eq!(func.params.len(), 1);
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_async_function() {
        let source = r#"
            async fn fetch_data(url: string) -> Data {
                let result = await http.get(url)
                return result
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                assert!(func.is_async);
            }
            _ => panic!("expected function"),
        }
    }
}
