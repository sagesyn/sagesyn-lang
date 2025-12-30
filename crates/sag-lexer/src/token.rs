//! Token definitions for the Sage Agent Programming Language.

use logos::Logos;
use std::fmt;

/// Source location span.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

/// A token with its span in the source.
#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl SpannedToken {
    pub fn new(token: Token, span: Span) -> Self {
        Self { token, span }
    }
}

/// All tokens in the Sage Agent language.
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token {
    // === Keywords ===
    #[token("agent")]
    Agent,

    #[token("skill")]
    Skill,

    #[token("type")]
    Type,

    #[token("fn")]
    Fn,

    #[token("async")]
    Async,

    #[token("await")]
    Await,

    #[token("let")]
    Let,

    #[token("var")]
    Var,

    #[token("const")]
    Const,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("match")]
    Match,

    #[token("for")]
    For,

    #[token("while")]
    While,

    #[token("in")]
    In,

    #[token("return")]
    Return,

    #[token("emit")]
    Emit,

    #[token("on")]
    On,

    #[token("tool")]
    Tool,

    #[token("state")]
    State,

    #[token("model")]
    Model,

    #[token("protocols")]
    Protocols,

    #[token("mcp")]
    Mcp,

    #[token("a2a")]
    A2a,

    #[token("ag_ui")]
    AgUi,

    #[token("servers")]
    Servers,

    #[token("transport")]
    Transport,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("null")]
    Null,

    // === Types ===
    #[token("string")]
    StringType,

    #[token("number")]
    NumberType,

    #[token("boolean")]
    BooleanType,

    #[token("timestamp")]
    TimestampType,

    #[token("array")]
    ArrayType,

    #[token("record")]
    RecordType,

    #[token("tuple")]
    TupleType,

    #[token("optional")]
    OptionalType,

    // === Punctuation ===
    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("<")]
    LAngle,

    #[token(">")]
    RAngle,

    #[token(":")]
    Colon,

    #[token("::")]
    DoubleColon,

    #[token(";")]
    Semicolon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("?")]
    Question,

    #[token("!")]
    Bang,

    #[token("->")]
    Arrow,

    #[token("=>")]
    FatArrow,

    #[token("_", priority = 3)]
    Underscore,

    // === Operators ===
    #[token("=")]
    Eq,

    #[token("==")]
    EqEq,

    #[token("!=")]
    NotEq,

    #[token("<=")]
    LtEq,

    #[token(">=")]
    GtEq,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("%")]
    Percent,

    #[token("**")]
    StarStar,

    #[token("&&")]
    AndAnd,

    #[token("||")]
    OrOr,

    #[token("|")]
    Pipe,

    #[token("&")]
    Ampersand,

    #[token("?.")]
    QuestionDot,

    #[token("??")]
    QuestionQuestion,

    #[token("..=")]
    DotDotEq,

    #[token("..")]
    DotDot,

    #[token("...")]
    DotDotDot,

    // === Error Handling Keywords ===
    #[token("try")]
    Try,

    #[token("catch")]
    Catch,

    #[token("finally")]
    Finally,

    #[token("throw")]
    Throw,

    // === Literals ===
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    StringLiteral(String),

    #[regex(r"`[^`]*`", |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    TemplateLiteral(String),

    #[regex(r"[0-9]+(\.[0-9]+)?", |lex| lex.slice().parse::<f64>().ok())]
    NumberLiteral(f64),

    // === Identifiers ===
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // === Comments ===
    #[regex(r"//[^\n]*", |lex| lex.slice()[2..].to_string())]
    LineComment(String),

    #[regex(r"///[^\n]*", |lex| lex.slice()[3..].to_string())]
    DocComment(String),

    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", |lex| {
        let s = lex.slice();
        s[2..s.len()-2].to_string()
    })]
    BlockComment(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Agent => write!(f, "agent"),
            Token::Skill => write!(f, "skill"),
            Token::Type => write!(f, "type"),
            Token::Fn => write!(f, "fn"),
            Token::Async => write!(f, "async"),
            Token::Await => write!(f, "await"),
            Token::Let => write!(f, "let"),
            Token::Var => write!(f, "var"),
            Token::Const => write!(f, "const"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Match => write!(f, "match"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::In => write!(f, "in"),
            Token::Return => write!(f, "return"),
            Token::Emit => write!(f, "emit"),
            Token::On => write!(f, "on"),
            Token::Tool => write!(f, "tool"),
            Token::State => write!(f, "state"),
            Token::Model => write!(f, "model"),
            Token::Protocols => write!(f, "protocols"),
            Token::Mcp => write!(f, "mcp"),
            Token::A2a => write!(f, "a2a"),
            Token::AgUi => write!(f, "ag_ui"),
            Token::Servers => write!(f, "servers"),
            Token::Transport => write!(f, "transport"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Null => write!(f, "null"),
            Token::StringType => write!(f, "string"),
            Token::NumberType => write!(f, "number"),
            Token::BooleanType => write!(f, "boolean"),
            Token::TimestampType => write!(f, "timestamp"),
            Token::ArrayType => write!(f, "array"),
            Token::RecordType => write!(f, "record"),
            Token::TupleType => write!(f, "tuple"),
            Token::OptionalType => write!(f, "optional"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LAngle => write!(f, "<"),
            Token::RAngle => write!(f, ">"),
            Token::Colon => write!(f, ":"),
            Token::DoubleColon => write!(f, "::"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Question => write!(f, "?"),
            Token::Bang => write!(f, "!"),
            Token::Arrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::Underscore => write!(f, "_"),
            Token::Eq => write!(f, "="),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::LtEq => write!(f, "<="),
            Token::GtEq => write!(f, ">="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::StarStar => write!(f, "**"),
            Token::AndAnd => write!(f, "&&"),
            Token::OrOr => write!(f, "||"),
            Token::Pipe => write!(f, "|"),
            Token::Ampersand => write!(f, "&"),
            Token::QuestionDot => write!(f, "?."),
            Token::QuestionQuestion => write!(f, "??"),
            Token::DotDotEq => write!(f, "..="),
            Token::DotDot => write!(f, ".."),
            Token::DotDotDot => write!(f, "..."),
            Token::Try => write!(f, "try"),
            Token::Catch => write!(f, "catch"),
            Token::Finally => write!(f, "finally"),
            Token::Throw => write!(f, "throw"),
            Token::StringLiteral(s) => write!(f, "\"{s}\""),
            Token::TemplateLiteral(s) => write!(f, "`{s}`"),
            Token::NumberLiteral(n) => write!(f, "{n}"),
            Token::Identifier(s) => write!(f, "{s}"),
            Token::LineComment(s) => write!(f, "//{s}"),
            Token::DocComment(s) => write!(f, "///{s}"),
            Token::BlockComment(s) => write!(f, "/*{s}*/"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_agent_keyword() {
        let mut lexer = Token::lexer("agent");
        assert_eq!(lexer.next(), Some(Ok(Token::Agent)));
    }

    #[test]
    fn test_string_literal() {
        let mut lexer = Token::lexer(r#""hello world""#);
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::StringLiteral("hello world".to_string())))
        );
    }

    #[test]
    fn test_number_literal() {
        let mut lexer = Token::lexer("42.5");
        assert_eq!(lexer.next(), Some(Ok(Token::NumberLiteral(42.5))));
    }

    #[test]
    fn test_identifier() {
        let mut lexer = Token::lexer("myAgent");
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::Identifier("myAgent".to_string())))
        );
    }

    #[test]
    fn test_template_literal() {
        let mut lexer = Token::lexer("`Hello ${name}`");
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::TemplateLiteral("Hello ${name}".to_string())))
        );
    }

    #[test]
    fn test_operators() {
        let mut lexer = Token::lexer("== != <= >= && ||");
        assert_eq!(lexer.next(), Some(Ok(Token::EqEq)));
        assert_eq!(lexer.next(), Some(Ok(Token::NotEq)));
        assert_eq!(lexer.next(), Some(Ok(Token::LtEq)));
        assert_eq!(lexer.next(), Some(Ok(Token::GtEq)));
        assert_eq!(lexer.next(), Some(Ok(Token::AndAnd)));
        assert_eq!(lexer.next(), Some(Ok(Token::OrOr)));
    }
}
