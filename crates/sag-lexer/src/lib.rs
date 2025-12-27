//! Lexer for the Sage Agent Programming Language.
//!
//! This crate provides tokenization for `.sag` files using the `logos` lexer generator.

#![allow(unused_assignments)]
//!
//! # Example
//!
//! ```
//! use sag_lexer::Lexer;
//!
//! let source = r#"agent MyAgent { description: "Hello" }"#;
//! let lexer = Lexer::new(source);
//!
//! for token in lexer {
//!     println!("{:?}", token);
//! }
//! ```

mod token;

pub use token::{Span, SpannedToken, Token};

use logos::Logos;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

/// Lexer error.
#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
#[error("Invalid token")]
#[diagnostic(code(sag::lexer::invalid_token))]
pub struct LexerError {
    #[source_code]
    src: String,
    #[label("unexpected character")]
    span: SourceSpan,
}

impl LexerError {
    pub fn new(src: &str, span: Span) -> Self {
        Self {
            src: src.to_string(),
            span: (span.start, span.len()).into(),
        }
    }
}

/// The Sage Agent lexer.
///
/// Wraps a `logos` lexer and provides iteration over spanned tokens.
pub struct Lexer<'src> {
    source: &'src str,
    inner: logos::Lexer<'src, Token>,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer for the given source code.
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            inner: Token::lexer(source),
        }
    }

    /// Get the source code being lexed.
    pub fn source(&self) -> &'src str {
        self.source
    }

    /// Tokenize the entire source and return all tokens.
    ///
    /// Returns an error if any invalid token is encountered.
    pub fn tokenize(self) -> Result<Vec<SpannedToken>, LexerError> {
        let source = self.source;
        self.inner
            .spanned()
            .map(|(result, range)| {
                let span = Span::new(range.start, range.end);
                match result {
                    Ok(token) => Ok(SpannedToken::new(token, span)),
                    Err(()) => Err(LexerError::new(source, span)),
                }
            })
            .collect()
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<SpannedToken, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let source = self.source;
        self.inner.next().map(|result| {
            let range = self.inner.span();
            let span = Span::new(range.start, range.end);
            match result {
                Ok(token) => Ok(SpannedToken::new(token, span)),
                Err(()) => Err(LexerError::new(source, span)),
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_basic() {
        let source = "agent MyAgent { }";
        let tokens: Vec<_> = Lexer::new(source)
            .filter_map(|r| r.ok())
            .map(|t| t.token)
            .collect();

        assert_eq!(
            tokens,
            vec![
                Token::Agent,
                Token::Identifier("MyAgent".to_string()),
                Token::LBrace,
                Token::RBrace,
            ]
        );
    }

    #[test]
    fn test_lexer_with_description() {
        let source = r#"agent Test { description: "A test agent" }"#;
        let tokens = Lexer::new(source).tokenize().unwrap();

        assert_eq!(tokens.len(), 7);
        assert_eq!(tokens[0].token, Token::Agent);
        assert_eq!(tokens[1].token, Token::Identifier("Test".to_string()));
        assert_eq!(tokens[2].token, Token::LBrace);
        assert_eq!(
            tokens[3].token,
            Token::Identifier("description".to_string())
        );
        assert_eq!(tokens[4].token, Token::Colon);
        assert_eq!(
            tokens[5].token,
            Token::StringLiteral("A test agent".to_string())
        );
        assert_eq!(tokens[6].token, Token::RBrace);
    }

    #[test]
    fn test_lexer_spans() {
        let source = "agent Test";
        let tokens = Lexer::new(source).tokenize().unwrap();

        assert_eq!(tokens[0].span, Span::new(0, 5)); // "agent"
        assert_eq!(tokens[1].span, Span::new(6, 10)); // "Test"
    }

    #[test]
    fn test_lexer_comments() {
        let source = "// This is a comment\nagent";
        let tokens = Lexer::new(source).tokenize().unwrap();

        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::LineComment(_)));
        assert_eq!(tokens[1].token, Token::Agent);
    }
}
