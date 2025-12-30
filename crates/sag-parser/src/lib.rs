//! Parser for the Sage Agent Programming Language.
//!
//! This crate provides parsing for `.sag` files, converting tokens into an Abstract Syntax Tree (AST).
//!
//! # Example
//!
//! ```
//! use sag_parser::Parser;
//!
//! let source = r#"agent MyAgent { description: "Hello" }"#;
//! let program = Parser::parse(source).expect("parse error");
//!
//! println!("{:?}", program);
//! ```

pub mod ast;

pub use ast::*;
pub use sag_lexer::Span;

use miette::{Diagnostic, LabeledSpan, SourceSpan};
use sag_lexer::{Lexer, SpannedToken, Token};
use thiserror::Error;

/// Error code category for parse errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    /// Unexpected token encountered
    UnexpectedToken,
    /// Expected a specific token but found something else
    ExpectedToken,
    /// Unexpected end of file
    UnexpectedEof,
    /// Missing delimiter (brace, paren, bracket)
    MissingDelimiter,
    /// Invalid syntax in a specific context
    InvalidSyntax,
    /// Unknown or undefined identifier
    UnknownIdentifier,
    /// Lexer error (invalid character, etc.)
    LexerError,
}

impl ParseErrorKind {
    fn code(&self) -> &'static str {
        match self {
            Self::UnexpectedToken => "sag::parser::unexpected_token",
            Self::ExpectedToken => "sag::parser::expected_token",
            Self::UnexpectedEof => "sag::parser::unexpected_eof",
            Self::MissingDelimiter => "sag::parser::missing_delimiter",
            Self::InvalidSyntax => "sag::parser::invalid_syntax",
            Self::UnknownIdentifier => "sag::parser::unknown_identifier",
            Self::LexerError => "sag::parser::lexer_error",
        }
    }
}

/// Parser error with rich diagnostic information.
#[derive(Error, Debug, Clone)]
#[error("{message}")]
pub struct ParseError {
    kind: ParseErrorKind,
    message: String,
    src: String,
    span: SourceSpan,
    help: Option<String>,
    labels: Vec<LabeledSpan>,
}

impl Diagnostic for ParseError {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(self.kind.code()))
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.src)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let mut all_labels = vec![LabeledSpan::new_with_span(
            Some(self.message.clone()),
            self.span,
        )];
        all_labels.extend(self.labels.iter().cloned());
        Some(Box::new(all_labels.into_iter()))
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.help
            .as_ref()
            .map(|h| Box::new(h.as_str()) as Box<dyn std::fmt::Display>)
    }
}

impl ParseError {
    /// Create a new parse error.
    pub fn new(message: impl Into<String>, src: &str, span: Span) -> Self {
        Self {
            kind: ParseErrorKind::InvalidSyntax,
            message: message.into(),
            src: src.to_string(),
            span: (span.start, span.len()).into(),
            help: None,
            labels: Vec::new(),
        }
    }

    /// Create an error with a specific kind.
    pub fn with_kind(
        kind: ParseErrorKind,
        message: impl Into<String>,
        src: &str,
        span: Span,
    ) -> Self {
        Self {
            kind,
            message: message.into(),
            src: src.to_string(),
            span: (span.start, span.len()).into(),
            help: None,
            labels: Vec::new(),
        }
    }

    /// Add help text to the error.
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    /// Add a secondary label at another location.
    pub fn with_label(mut self, message: impl Into<String>, span: Span) -> Self {
        self.labels.push(LabeledSpan::new_with_span(
            Some(message.into()),
            (span.start, span.len()),
        ));
        self
    }

    /// Create an "expected X, found Y" error.
    pub fn expected(expected: &str, found: &Token, src: &str, span: Span) -> Self {
        let message = format!("expected {expected}, found `{found}`");
        let mut err = Self::with_kind(ParseErrorKind::ExpectedToken, message, src, span);

        // Add contextual help for common mistakes
        err.help = match (expected, found) {
            ("identifier", Token::StringLiteral(_)) => {
                Some("identifiers don't need quotes - try removing the quotes".to_string())
            }
            (_, Token::RBrace) => {
                Some("you may have an unclosed block or extra closing brace".to_string())
            }
            ("`;`", _) | ("`,`", _) => {
                Some("this language uses commas to separate items".to_string())
            }
            _ => None,
        };

        err
    }

    /// Create an unexpected end of file error.
    pub fn unexpected_eof(src: &str) -> Self {
        let span = Span::new(src.len().saturating_sub(1), src.len());
        Self::with_kind(
            ParseErrorKind::UnexpectedEof,
            "unexpected end of file",
            src,
            span,
        )
        .with_help("the file ended unexpectedly - check for unclosed braces or brackets")
    }

    /// Create an unexpected token error.
    pub fn unexpected_token(token: &Token, context: &str, src: &str, span: Span) -> Self {
        Self::with_kind(
            ParseErrorKind::UnexpectedToken,
            format!("unexpected `{token}` in {context}"),
            src,
            span,
        )
    }

    /// Create a missing delimiter error.
    pub fn missing_delimiter(
        delimiter: &str,
        opening: Option<Span>,
        src: &str,
        span: Span,
    ) -> Self {
        let mut err = Self::with_kind(
            ParseErrorKind::MissingDelimiter,
            format!("missing closing `{delimiter}`"),
            src,
            span,
        );

        if let Some(open_span) = opening {
            err = err.with_label("opening delimiter here", open_span);
        }

        err.with_help(format!("add `{delimiter}` to close this block"))
    }

    /// Get the error message.
    pub fn message(&self) -> &str {
        &self.message
    }

    /// Get the error span.
    pub fn span(&self) -> Span {
        Span::new(self.span.offset(), self.span.offset() + self.span.len())
    }

    /// Get the error kind.
    pub fn kind(&self) -> ParseErrorKind {
        self.kind
    }
}

/// The Sage Agent parser.
pub struct Parser<'src> {
    source: &'src str,
    tokens: Vec<SpannedToken>,
    pos: usize,
}

impl<'src> Parser<'src> {
    /// Create a new parser for the given source code.
    pub fn new(source: &'src str) -> Result<Self, sag_lexer::LexerError> {
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
                format!("lexer error: {e}"),
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
            Span::new(
                start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(0),
            )
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
                Err(
                    ParseError::unexpected_token(token, "top-level scope", self.source, span)
                        .with_help("expected `agent`, `skill`, `type`, `fn`, or `async fn`"),
                )
            }
            None => Err(ParseError::unexpected_eof(self.source)),
        }
    }

    fn parse_agent(&mut self) -> Result<Agent, ParseError> {
        let start_span = self.expect(Token::Agent)?;
        let name = self.parse_identifier()?;
        let brace_span = self.expect(Token::LBrace)?;

        let mut description = None;
        let mut version = None;
        let mut model = None;
        let mut state = None;
        let mut protocols = None;
        let mut tools = Vec::new();
        let mut handlers = Vec::new();

        while !self.check(&Token::RBrace) && !self.is_at_end() {
            match self.peek_token().cloned() {
                Some(Token::Identifier(ref id)) if id == "description" => {
                    self.advance();
                    self.expect(Token::Colon)?;
                    description = Some(self.parse_string_literal()?);
                }
                Some(Token::Identifier(ref id)) if id == "version" => {
                    self.advance();
                    self.expect(Token::Colon)?;
                    version = Some(self.parse_string_literal()?);
                }
                Some(Token::Model) => {
                    self.advance();
                    // Support both `model: { ... }` and `model { ... }`
                    if self.check(&Token::Colon) {
                        self.advance();
                    }
                    // If we have a brace, parse the block
                    if self.check(&Token::LBrace) {
                        self.advance();
                        model = Some(self.parse_model_config()?);
                        self.expect(Token::RBrace)?;
                    } else {
                        model = Some(self.parse_model_config()?);
                    }
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
                    return Err(ParseError::unexpected_token(&token, "agent body", self.source, span)
                        .with_help("valid agent members: description, version, model, state, protocols, tool, on"));
                }
                None => {
                    let span = self.current_span();
                    return Err(ParseError::missing_delimiter(
                        "}",
                        Some(brace_span),
                        self.source,
                        span,
                    ));
                }
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

        let end = self
            .tokens
            .get(self.pos.saturating_sub(1))
            .map(|t| t.span.end)
            .unwrap_or(start_span.end);

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
            span: Span::new(
                start_span.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
            ),
        })
    }

    fn parse_model_config(&mut self) -> Result<ModelConfig, ParseError> {
        let start_span = self.current_span();
        let mut provider = None;
        let mut name = None;
        let mut context_window = None;
        let mut temperature = None;

        // Simple key-value parsing for model config
        while !self.is_at_end()
            && !self.check(&Token::State)
            && !self.check(&Token::Protocols)
            && !self.check(&Token::Tool)
            && !self.check(&Token::On)
            && !self.check(&Token::RBrace)
        {
            if let Some(Token::Identifier(key)) = self.peek_token().cloned() {
                self.advance();
                self.expect(Token::Colon)?;
                match key.as_str() {
                    "provider" => provider = Some(self.parse_string_or_identifier()?),
                    "name" => name = Some(self.parse_string_or_identifier()?),
                    "context_window" => context_window = Some(self.parse_number_literal()?),
                    "temperature" => temperature = Some(self.parse_number_literal()?),
                    _ => {
                        // Skip unknown fields by parsing and discarding
                        let _ = self.parse_string_or_identifier().ok();
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
            span: Span::new(
                start_span.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
            ),
        })
    }

    /// Parse either a string literal or an identifier as a string value
    fn parse_string_or_identifier(&mut self) -> Result<StringLit, ParseError> {
        let span = self.current_span();
        match self.peek_token().cloned() {
            Some(Token::StringLiteral(value)) => {
                self.advance();
                Ok(StringLit { value, span })
            }
            // Handle identifiers and hyphenated identifiers like "gemini-2.0-flash"
            Some(token) if self.is_identifier_like(&token) => {
                let mut value = self.token_to_string(&token);
                self.advance();
                // Collect hyphen-separated parts
                while self.check(&Token::Minus) {
                    self.advance();
                    if let Some(next) = self.peek_token().cloned() {
                        if self.is_identifier_like(&next) {
                            value.push('-');
                            value.push_str(&self.token_to_string(&next));
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                Ok(StringLit { value, span })
            }
            Some(token) => Err(ParseError::expected(
                "string or identifier",
                &token,
                self.source,
                span,
            )),
            None => Err(ParseError::unexpected_eof(self.source)),
        }
    }

    fn is_identifier_like(&self, token: &Token) -> bool {
        matches!(
            token,
            Token::Identifier(_)
                | Token::NumberLiteral(_)
                | Token::StringType
                | Token::NumberType
                | Token::BooleanType
                | Token::TimestampType
        )
    }

    fn token_to_string(&self, token: &Token) -> String {
        match token {
            Token::Identifier(s) => s.clone(),
            Token::NumberLiteral(n) => {
                // Preserve decimal point for version-like numbers
                if n.fract() == 0.0 {
                    format!("{n:.1}") // e.g., 2.0 -> "2.0"
                } else {
                    n.to_string()
                }
            }
            Token::StringType => "string".to_string(),
            Token::NumberType => "number".to_string(),
            Token::BooleanType => "boolean".to_string(),
            Token::TimestampType => "timestamp".to_string(),
            _ => String::new(),
        }
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
            span: Span::new(
                start_span.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
            ),
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
            span: Span::new(
                start_span.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
            ),
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
            span: Span::new(
                start_span.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
            ),
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
            span: Span::new(
                start_span.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
            ),
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
            span: Span::new(
                start_span.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
            ),
        })
    }

    fn parse_while_stmt(&mut self) -> Result<WhileStmt, ParseError> {
        let start_span = self.expect(Token::While)?;
        let condition = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(WhileStmt {
            condition,
            body,
            span: Span::new(
                start_span.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
            ),
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
            span: Span::new(
                start_span.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
            ),
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
            span: Span::new(
                start_span.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
            ),
        })
    }

    fn parse_expr_stmt(&mut self) -> Result<ExprStmt, ParseError> {
        let start = self.current_span();
        let expr = self.parse_expr()?;

        Ok(ExprStmt {
            expr,
            span: Span::new(
                start.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start.end),
            ),
        })
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
        let start = self.current_span();
        let pattern = self.parse_pattern()?;
        self.expect(Token::FatArrow)?;
        let body = self.parse_expr()?;

        Ok(MatchArm {
            pattern,
            body,
            span: Span::new(
                start.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start.end),
            ),
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.peek_token().cloned() {
            Some(Token::StringLiteral(s)) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(Literal::String(StringLit { value: s, span })))
            }
            Some(Token::NumberLiteral(n)) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(Literal::Number(NumberLit { value: n, span })))
            }
            Some(Token::True) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(Literal::Boolean(BoolLit { value: true, span })))
            }
            Some(Token::False) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(Literal::Boolean(BoolLit { value: false, span })))
            }
            Some(Token::Null) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Literal(Literal::Null(span)))
            }
            Some(Token::Underscore) => {
                let span = self.current_span();
                self.advance();
                Ok(Pattern::Wildcard(span))
            }
            Some(Token::Identifier(_)) => {
                let ident = self.parse_identifier()?;
                Ok(Pattern::Identifier(ident))
            }
            Some(token) => {
                let span = self.current_span();
                Err(ParseError::new(
                    format!("expected pattern, found `{token}`"),
                    self.source,
                    span,
                ))
            }
            None => Err(ParseError::unexpected_eof(self.source)),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_or()?;

        if self.check(&Token::Eq) {
            self.advance();
            let value = self.parse_assignment()?;
            let span = Span::new(self.expr_span(&expr).start, self.expr_span(&value).end);
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
                // Parse template literal with ${expr} interpolations
                let parts = self.parse_template_parts(&s)?;
                Ok(Expr::Template(TemplateExpr { parts, span }))
            }
            Some(Token::NumberLiteral(n)) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::Number(NumberLit { value: n, span })))
            }
            Some(Token::True) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::Boolean(BoolLit {
                    value: true,
                    span,
                })))
            }
            Some(Token::False) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::Boolean(BoolLit {
                    value: false,
                    span,
                })))
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
            // Allow 'state' keyword to be used as an identifier in expressions
            Some(Token::State) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Identifier(Identifier::new("state", span)))
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
            Some(Token::Match) => {
                let start = self.current_span();
                self.advance();
                let subject = self.parse_expr()?;
                self.expect(Token::LBrace)?;
                let mut arms = Vec::new();
                while !self.check(&Token::RBrace) && !self.is_at_end() {
                    let arm = self.parse_match_arm()?;
                    arms.push(arm);
                }
                let end = self.expect(Token::RBrace)?;
                let span = Span::new(start.start, end.end);
                Ok(Expr::Match(MatchExpr {
                    subject: Box::new(subject),
                    arms,
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
                    format!("expected expression, found `{token}`"),
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
            Some(Token::StringType)
            | Some(Token::NumberType)
            | Some(Token::BooleanType)
            | Some(Token::TimestampType) => {
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
                    format!("expected type, found `{token}`"),
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
            span: Span::new(
                start.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start.end),
            ),
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
            span: Span::new(
                start.start,
                self.tokens
                    .get(self.pos.saturating_sub(1))
                    .map(|t| t.span.end)
                    .unwrap_or(start.end),
            ),
        })
    }

    fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        let span = self.current_span();
        match self.peek_token().cloned() {
            Some(Token::Identifier(name)) => {
                self.advance();
                Ok(Identifier::new(name, span))
            }
            // Allow type keywords to be used as identifiers (e.g., field names)
            Some(Token::StringType) => {
                self.advance();
                Ok(Identifier::new("string", span))
            }
            Some(Token::NumberType) => {
                self.advance();
                Ok(Identifier::new("number", span))
            }
            Some(Token::BooleanType) => {
                self.advance();
                Ok(Identifier::new("boolean", span))
            }
            Some(Token::TimestampType) => {
                self.advance();
                Ok(Identifier::new("timestamp", span))
            }
            Some(Token::ArrayType) => {
                self.advance();
                Ok(Identifier::new("array", span))
            }
            Some(Token::RecordType) => {
                self.advance();
                Ok(Identifier::new("record", span))
            }
            Some(Token::TupleType) => {
                self.advance();
                Ok(Identifier::new("tuple", span))
            }
            Some(Token::OptionalType) => {
                self.advance();
                Ok(Identifier::new("optional", span))
            }
            // Also allow some other keywords as identifiers
            Some(Token::Model) => {
                self.advance();
                Ok(Identifier::new("model", span))
            }
            Some(Token::State) => {
                self.advance();
                Ok(Identifier::new("state", span))
            }
            Some(token) => Err(ParseError::expected(
                "identifier",
                &token,
                self.source,
                span,
            )),
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
                Err(ParseError::expected(
                    "string literal",
                    &token,
                    self.source,
                    span,
                ))
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
                Err(ParseError::expected(
                    "number literal",
                    &token,
                    self.source,
                    span,
                ))
            }
            None => Err(ParseError::unexpected_eof(self.source)),
        }
    }

    /// Parse template literal parts, extracting ${expr} interpolations.
    fn parse_template_parts(&self, template: &str) -> Result<Vec<TemplatePart>, ParseError> {
        let mut parts = Vec::new();
        let mut current_string = String::new();
        let mut chars = template.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '$' && chars.peek() == Some(&'{') {
                // Found ${, save current string part if not empty
                if !current_string.is_empty() {
                    parts.push(TemplatePart::String(current_string.clone()));
                    current_string.clear();
                }

                chars.next(); // consume '{'

                // Extract expression text up to matching '}'
                let mut expr_text = String::new();
                let mut brace_depth = 1;

                for ec in chars.by_ref() {
                    if ec == '{' {
                        brace_depth += 1;
                        expr_text.push(ec);
                    } else if ec == '}' {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            break;
                        }
                        expr_text.push(ec);
                    } else {
                        expr_text.push(ec);
                    }
                }

                // Parse the expression
                if !expr_text.is_empty() {
                    match Parser::parse_expr_string(&expr_text) {
                        Ok(expr) => parts.push(TemplatePart::Expr(Box::new(expr))),
                        Err(_) => {
                            // If parsing fails, treat as identifier (simple case)
                            let span = Span::new(0, expr_text.len());
                            parts.push(TemplatePart::Expr(Box::new(Expr::Identifier(
                                Identifier::new(expr_text, span),
                            ))));
                        }
                    }
                }
            } else {
                current_string.push(c);
            }
        }

        // Add remaining string if not empty
        if !current_string.is_empty() {
            parts.push(TemplatePart::String(current_string));
        }

        // If no parts were created, add empty string
        if parts.is_empty() {
            parts.push(TemplatePart::String(String::new()));
        }

        Ok(parts)
    }

    /// Parse a standalone expression from a string.
    pub fn parse_expr_string(source: &str) -> Result<Expr, ParseError> {
        let mut parser = Parser::new(source).map_err(|e| {
            ParseError::new(
                format!("lexer error: {e}"),
                source,
                Span::new(0, source.len()),
            )
        })?;
        parser.parse_expr()
    }

    // Helper methods

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|t| &t.token)
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|t| t.span)
            .unwrap_or_default()
    }

    fn check(&self, token: &Token) -> bool {
        self.peek_token()
            .map(|t| std::mem::discriminant(t) == std::mem::discriminant(token))
            .unwrap_or(false)
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
                Some(found) => Err(ParseError::expected(
                    &format!("{expected}"),
                    found,
                    self.source,
                    span,
                )),
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

    #[test]
    fn test_parse_agent_with_tool() {
        let source = r#"
            agent MyAgent {
                tool greet(name: string) -> string {
                    description: "Greet someone"
                    return `Hello, ${name}!`
                }
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Agent(agent) => {
                assert_eq!(agent.tools.len(), 1);
                assert_eq!(agent.tools[0].name.name, "greet");
                assert!(agent.tools[0].description.is_some());
            }
            _ => panic!("expected agent"),
        }
    }

    #[test]
    fn test_parse_agent_with_state() {
        let source = r#"
            agent MyAgent {
                state {
                    count: number
                    name?: string
                }
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Agent(agent) => {
                let state = agent.state.as_ref().expect("state should exist");
                assert_eq!(state.fields.len(), 2);
                assert!(!state.fields[0].optional);
                assert!(state.fields[1].optional);
            }
            _ => panic!("expected agent"),
        }
    }

    #[test]
    fn test_parse_agent_with_handler() {
        let source = r#"
            agent MyAgent {
                on user_message {
                    let x = 1
                    emit response(x)
                }
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Agent(agent) => {
                assert_eq!(agent.handlers.len(), 1);
                assert_eq!(agent.handlers[0].event.name, "user_message");
            }
            _ => panic!("expected agent"),
        }
    }

    #[test]
    fn test_parse_if_else_statement() {
        let source = r#"
            fn test() -> number {
                if x > 0 {
                    return 1
                } else if x < 0 {
                    return -1
                } else {
                    return 0
                }
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                assert_eq!(func.body.stmts.len(), 1);
                match &func.body.stmts[0] {
                    Stmt::If(if_stmt) => {
                        assert!(if_stmt.else_block.is_some());
                    }
                    _ => panic!("expected if statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_for_loop() {
        let source = r#"
            fn test(items: array<string>) {
                for item in items {
                    console.log(item)
                }
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.body.stmts[0] {
                    Stmt::For(for_stmt) => {
                        assert_eq!(for_stmt.binding.name, "item");
                    }
                    _ => panic!("expected for statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_while_loop() {
        let source = r#"
            fn test() {
                while x > 0 {
                    x = x - 1
                }
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.body.stmts[0] {
                    Stmt::While(_) => {}
                    _ => panic!("expected while statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_binary_expressions() {
        let source = r#"
            fn test() -> number {
                return 1 + 2 * 3 - 4 / 2
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.body.stmts[0] {
                    Stmt::Return(ret) => {
                        assert!(ret.value.is_some());
                    }
                    _ => panic!("expected return statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_comparison_expressions() {
        let source = r#"
            fn test(a: number, b: number) -> boolean {
                return a >= b && b <= 10 || a == b
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                assert_eq!(func.params.len(), 2);
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_array_literal() {
        let source = r#"
            fn test() -> array<number> {
                return [1, 2, 3, 4, 5]
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.body.stmts[0] {
                    Stmt::Return(ret) => {
                        match ret.value.as_ref().unwrap() {
                            Expr::Array(arr) => {
                                assert_eq!(arr.elements.len(), 5);
                            }
                            _ => panic!("expected array expression"),
                        }
                    }
                    _ => panic!("expected return statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_record_literal() {
        let source = r#"
            fn test() {
                let user = { name: "John", age: 30 }
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.body.stmts[0] {
                    Stmt::Let(let_stmt) => {
                        match &let_stmt.value {
                            Expr::Record(rec) => {
                                assert_eq!(rec.fields.len(), 2);
                            }
                            _ => panic!("expected record expression"),
                        }
                    }
                    _ => panic!("expected let statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_member_access() {
        let source = r#"
            fn test() {
                let x = user.name.first
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.body.stmts[0] {
                    Stmt::Let(let_stmt) => {
                        match &let_stmt.value {
                            Expr::Member(m) => {
                                assert_eq!(m.property.name, "first");
                            }
                            _ => panic!("expected member expression"),
                        }
                    }
                    _ => panic!("expected let statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_index_access() {
        let source = r#"
            fn test() {
                let x = arr[0]
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.body.stmts[0] {
                    Stmt::Let(let_stmt) => {
                        match &let_stmt.value {
                            Expr::Index(_) => {}
                            _ => panic!("expected index expression"),
                        }
                    }
                    _ => panic!("expected let statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_function_call() {
        let source = r#"
            fn test() {
                let x = foo(1, "hello", true)
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.body.stmts[0] {
                    Stmt::Let(let_stmt) => {
                        match &let_stmt.value {
                            Expr::Call(call) => {
                                assert_eq!(call.args.len(), 3);
                            }
                            _ => panic!("expected call expression"),
                        }
                    }
                    _ => panic!("expected let statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_type_alias() {
        let source = r#"
            type ID = string
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::TypeDef(typedef) => {
                assert_eq!(typedef.name.name, "ID");
                match &typedef.kind {
                    TypeDefKind::Alias(_) => {}
                    _ => panic!("expected alias"),
                }
            }
            _ => panic!("expected type def"),
        }
    }

    #[test]
    fn test_parse_union_type() {
        let source = r#"
            type Result = Success | Error | Pending
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::TypeDef(typedef) => {
                match &typedef.kind {
                    TypeDefKind::Alias(TypeExpr::Union(types)) => {
                        assert_eq!(types.len(), 3);
                    }
                    _ => panic!("expected union type"),
                }
            }
            _ => panic!("expected type def"),
        }
    }

    #[test]
    fn test_parse_generic_type() {
        let source = r#"
            type Container<T> {
                value: T
                items: array<T>
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::TypeDef(typedef) => {
                assert_eq!(typedef.generics.len(), 1);
                assert_eq!(typedef.generics[0].name, "T");
            }
            _ => panic!("expected type def"),
        }
    }

    #[test]
    fn test_parse_state_assignment() {
        let source = r#"
            agent MyAgent {
                state {
                    counter: number
                }
                on user_message {
                    state.counter = state.counter + 1
                }
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Agent(agent) => {
                assert_eq!(agent.handlers.len(), 1);
                match &agent.handlers[0].body.stmts[0] {
                    Stmt::Expr(expr_stmt) => {
                        match &expr_stmt.expr {
                            Expr::Assign(_) => {}
                            _ => panic!("expected assignment expression"),
                        }
                    }
                    _ => panic!("expected expression statement"),
                }
            }
            _ => panic!("expected agent"),
        }
    }

    #[test]
    fn test_parse_mcp_tool() {
        let source = r#"
            agent MyAgent {
                tool get_weather(city: string) -> WeatherData {
                    description: "Get weather"
                    mcp_server: weather_api
                    mcp_tool: current_weather
                }
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Agent(agent) => {
                let tool = &agent.tools[0];
                assert!(tool.mcp_server.is_some());
                assert!(tool.mcp_tool.is_some());
                assert!(tool.body.is_none());
            }
            _ => panic!("expected agent"),
        }
    }

    #[test]
    fn test_parse_var_statement() {
        let source = r#"
            fn test() {
                var x = 10
                x = x + 1
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.body.stmts[0] {
                    Stmt::Var(var_stmt) => {
                        assert_eq!(var_stmt.name.name, "x");
                    }
                    _ => panic!("expected var statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_unary_expressions() {
        let source = r#"
            fn test() -> boolean {
                return !true && -5 < 0
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                assert_eq!(func.body.stmts.len(), 1);
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_await_expression() {
        let source = r#"
            async fn test() {
                let result = await fetch_data()
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                assert!(func.is_async);
                match &func.body.stmts[0] {
                    Stmt::Let(let_stmt) => {
                        match &let_stmt.value {
                            Expr::Await(_) => {}
                            _ => panic!("expected await expression"),
                        }
                    }
                    _ => panic!("expected let statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_match_expression() {
        // match expression is parsed via let binding
        let source = r#"
            fn test(x: number) -> string {
                let result = match x {
                    0 => "zero"
                    1 => "one"
                    _ => "other"
                }
                return result
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.body.stmts[0] {
                    Stmt::Let(let_stmt) => {
                        match &let_stmt.value {
                            Expr::Match(m) => {
                                assert_eq!(m.arms.len(), 3);
                            }
                            _ => panic!("expected match expression"),
                        }
                    }
                    _ => panic!("expected let statement"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_function_type() {
        let source = r#"
            fn apply(callback: (number, number) -> number, a: number, b: number) -> number {
                return callback(a, b)
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                assert_eq!(func.params.len(), 3);
                match &func.params[0].ty {
                    TypeExpr::Function(ft) => {
                        assert_eq!(ft.params.len(), 2);
                    }
                    _ => panic!("expected function type"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_tuple_type() {
        // Test tuple type annotation (tuple literals are arrays in this language)
        let source = r#"
            fn test(data: tuple<string, number, boolean>) -> string {
                return data[0]
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match &func.params[0].ty {
                    TypeExpr::Tuple(t) => {
                        assert_eq!(t.elements.len(), 3);
                    }
                    _ => panic!("expected tuple type"),
                }
            }
            _ => panic!("expected function"),
        }
    }

    #[test]
    fn test_parse_record_type() {
        let source = r#"
            fn test() -> record<string, number> {
                return {}
            }
        "#;
        let program = Parser::parse(source).unwrap();

        match &program.items[0] {
            Item::Function(func) => {
                match func.return_type.as_ref().unwrap() {
                    TypeExpr::Record(_) => {}
                    _ => panic!("expected record type"),
                }
            }
            _ => panic!("expected function"),
        }
    }
}
