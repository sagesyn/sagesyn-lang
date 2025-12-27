//! Semantic tokens for Sage syntax highlighting.

use once_cell::sync::Lazy;
use sag_lexer::Token;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType, SemanticTokensLegend};

/// Semantic token types we support.
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,   // 0
    SemanticTokenType::TYPE,      // 1
    SemanticTokenType::FUNCTION,  // 2
    SemanticTokenType::VARIABLE,  // 3
    SemanticTokenType::STRING,    // 4
    SemanticTokenType::NUMBER,    // 5
    SemanticTokenType::COMMENT,   // 6
    SemanticTokenType::OPERATOR,  // 7
    SemanticTokenType::PROPERTY,  // 8
    SemanticTokenType::NAMESPACE, // 9
    SemanticTokenType::CLASS,     // 10
];

/// The semantic tokens legend.
pub static LEGEND: Lazy<SemanticTokensLegend> = Lazy::new(|| SemanticTokensLegend {
    token_types: TOKEN_TYPES.to_vec(),
    token_modifiers: vec![],
});

/// Map a lexer token to a semantic token type index.
pub fn token_to_semantic_type(token: &Token) -> Option<u32> {
    match token {
        // Keywords
        Token::Agent
        | Token::Tool
        | Token::Skill
        | Token::Fn
        | Token::On
        | Token::Emit
        | Token::Let
        | Token::Var
        | Token::Const
        | Token::If
        | Token::Else
        | Token::For
        | Token::In
        | Token::While
        | Token::Return
        | Token::Match
        | Token::Async
        | Token::Await
        | Token::True
        | Token::False
        | Token::Null
        | Token::Model
        | Token::State
        | Token::Protocols
        | Token::Mcp
        | Token::A2a
        | Token::AgUi
        | Token::Servers
        | Token::Transport
        | Token::Type => Some(0), // KEYWORD

        // Types
        Token::StringType
        | Token::NumberType
        | Token::BooleanType
        | Token::TimestampType
        | Token::ArrayType
        | Token::RecordType
        | Token::TupleType
        | Token::OptionalType => Some(1), // TYPE

        // Identifiers (could be functions or variables based on context)
        Token::Identifier(_) => Some(3), // VARIABLE

        // Strings
        Token::StringLiteral(_) | Token::TemplateLiteral(_) => Some(4), // STRING

        // Numbers
        Token::NumberLiteral(_) => Some(5), // NUMBER

        // Comments
        Token::LineComment(_) | Token::DocComment(_) | Token::BlockComment(_) => Some(6), // COMMENT

        // Operators
        Token::Plus
        | Token::Minus
        | Token::Star
        | Token::Slash
        | Token::Percent
        | Token::StarStar
        | Token::Eq
        | Token::EqEq
        | Token::NotEq
        | Token::LAngle
        | Token::RAngle
        | Token::LtEq
        | Token::GtEq
        | Token::AndAnd
        | Token::OrOr
        | Token::Bang
        | Token::Arrow
        | Token::FatArrow
        | Token::Pipe
        | Token::Question
        | Token::Ampersand => Some(7), // OPERATOR

        // Punctuation - don't highlight
        Token::LParen
        | Token::RParen
        | Token::LBrace
        | Token::RBrace
        | Token::LBracket
        | Token::RBracket
        | Token::Comma
        | Token::Colon
        | Token::DoubleColon
        | Token::Semicolon
        | Token::Dot
        | Token::Underscore => None,
    }
}

/// Builder for semantic tokens.
pub struct SemanticTokensBuilder {
    tokens: Vec<SemanticToken>,
    prev_line: u32,
    prev_start: u32,
}

impl SemanticTokensBuilder {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            prev_line: 0,
            prev_start: 0,
        }
    }

    /// Push a new semantic token.
    pub fn push(
        &mut self,
        line: u32,
        start: u32,
        length: u32,
        token_type: u32,
        token_modifiers: u32,
    ) {
        let delta_line = line - self.prev_line;
        let delta_start = if delta_line == 0 {
            start - self.prev_start
        } else {
            start
        };

        self.tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: token_modifiers,
        });

        self.prev_line = line;
        self.prev_start = start;
    }

    /// Build the final semantic tokens data.
    pub fn build(self) -> Vec<SemanticToken> {
        self.tokens
    }
}

impl Default for SemanticTokensBuilder {
    fn default() -> Self {
        Self::new()
    }
}
