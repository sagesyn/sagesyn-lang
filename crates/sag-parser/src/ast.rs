//! Abstract Syntax Tree types for the Sage Agent Programming Language.

use sag_lexer::Span;

/// A complete Sage Agent program (the root AST node).
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
    pub span: Span,
}

/// Top-level items in a Sage Agent program.
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Agent(Agent),
    Skill(Skill),
    TypeDef(TypeDef),
    Function(Function),
}

/// An agent definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Agent {
    pub name: Identifier,
    pub description: Option<StringLit>,
    pub version: Option<StringLit>,
    pub model: Option<ModelConfig>,
    pub state: Option<StateBlock>,
    pub protocols: Option<ProtocolsBlock>,
    pub tools: Vec<Tool>,
    pub handlers: Vec<EventHandler>,
    pub span: Span,
}

/// A skill definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Skill {
    pub name: Identifier,
    pub description: Option<StringLit>,
    pub body: Vec<Item>,
    pub span: Span,
}

/// A type definition.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    pub name: Identifier,
    pub generics: Vec<Identifier>,
    pub kind: TypeDefKind,
    pub span: Span,
}

/// The kind of type definition.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefKind {
    /// Struct-like type with fields.
    Struct(Vec<Field>),
    /// Type alias (including union types).
    Alias(TypeExpr),
}

/// A field in a struct type.
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Identifier,
    pub ty: TypeExpr,
    pub optional: bool,
    pub span: Span,
}

/// A function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub is_async: bool,
    pub name: Identifier,
    pub params: Vec<Param>,
    pub return_type: Option<TypeExpr>,
    pub body: Block,
    pub span: Span,
}

/// A function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Identifier,
    pub ty: TypeExpr,
    pub default: Option<Expr>,
    pub span: Span,
}

/// Model configuration block.
#[derive(Debug, Clone, PartialEq)]
pub struct ModelConfig {
    pub provider: Option<StringLit>,
    pub name: Option<StringLit>,
    pub context_window: Option<NumberLit>,
    pub temperature: Option<NumberLit>,
    pub span: Span,
}

/// State block containing typed fields.
#[derive(Debug, Clone, PartialEq)]
pub struct StateBlock {
    pub fields: Vec<Field>,
    pub span: Span,
}

/// Protocols block (MCP, A2A, AG-UI).
#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolsBlock {
    pub mcp: Option<McpConfig>,
    pub a2a: Option<A2aConfig>,
    pub ag_ui: Option<AgUiConfig>,
    pub span: Span,
}

/// MCP protocol configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct McpConfig {
    pub servers: Vec<McpServer>,
    pub span: Span,
}

/// An MCP server definition.
#[derive(Debug, Clone, PartialEq)]
pub struct McpServer {
    pub name: StringLit,
    pub transport: StringLit,
    pub url: Option<StringLit>,
    pub command: Option<StringLit>,
    pub args: Option<Vec<StringLit>>,
    pub env: Option<Vec<(Identifier, StringLit)>>,
    pub span: Span,
}

/// A2A protocol configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct A2aConfig {
    pub discoverable: Option<bool>,
    pub capabilities: Option<Vec<Identifier>>,
    pub span: Span,
}

/// AG-UI protocol configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct AgUiConfig {
    pub stream_events: Option<bool>,
    pub ui_components: Option<Vec<Identifier>>,
    pub span: Span,
}

/// A tool definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Tool {
    pub name: Identifier,
    pub params: Vec<Param>,
    pub return_type: Option<TypeExpr>,
    pub description: Option<StringLit>,
    pub mcp_server: Option<Identifier>,
    pub mcp_tool: Option<Identifier>,
    pub body: Option<Block>,
    pub span: Span,
}

/// An event handler.
#[derive(Debug, Clone, PartialEq)]
pub struct EventHandler {
    pub event: Identifier,
    pub body: Block,
    pub span: Span,
}

/// A block of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

/// Statements.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let(LetStmt),
    Var(VarStmt),
    Expr(ExprStmt),
    If(IfStmt),
    For(ForStmt),
    While(WhileStmt),
    Return(ReturnStmt),
    Emit(EmitStmt),
    Block(Block),
}

/// Let binding (immutable).
#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub name: Identifier,
    pub ty: Option<TypeExpr>,
    pub value: Expr,
    pub span: Span,
}

/// Var binding (mutable).
#[derive(Debug, Clone, PartialEq)]
pub struct VarStmt {
    pub name: Identifier,
    pub ty: Option<TypeExpr>,
    pub value: Expr,
    pub span: Span,
}

/// Expression statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: Span,
}

/// If statement.
#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_block: Block,
    pub else_block: Option<Box<ElseClause>>,
    pub span: Span,
}

/// Else clause (else if or else).
#[derive(Debug, Clone, PartialEq)]
pub enum ElseClause {
    ElseIf(IfStmt),
    Else(Block),
}

/// For loop.
#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub binding: Identifier,
    pub iterable: Expr,
    pub body: Block,
    pub span: Span,
}

/// While loop.
#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Block,
    pub span: Span,
}

/// Return statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub span: Span,
}

/// Emit statement.
#[derive(Debug, Clone, PartialEq)]
pub struct EmitStmt {
    pub event: Identifier,
    pub value: Expr,
    pub span: Span,
}

/// Expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Call(CallExpr),
    Member(MemberExpr),
    Index(IndexExpr),
    Array(ArrayExpr),
    Record(RecordExpr),
    Await(AwaitExpr),
    Arrow(ArrowExpr),
    Match(MatchExpr),
    Template(TemplateExpr),
    Assign(AssignExpr),
}

/// Binary expression.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
    pub span: Span,
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
}

/// Unary expression.
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub operand: Box<Expr>,
    pub span: Span,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

/// Function call expression.
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub span: Span,
}

/// Member access expression (a.b).
#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpr {
    pub object: Box<Expr>,
    pub property: Identifier,
    pub span: Span,
}

/// Index access expression (a[b]).
#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub object: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

/// Array literal.
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpr {
    pub elements: Vec<Expr>,
    pub span: Span,
}

/// Record (object) literal.
#[derive(Debug, Clone, PartialEq)]
pub struct RecordExpr {
    pub fields: Vec<(Identifier, Expr)>,
    pub span: Span,
}

/// Await expression.
#[derive(Debug, Clone, PartialEq)]
pub struct AwaitExpr {
    pub expr: Box<Expr>,
    pub span: Span,
}

/// Arrow function expression.
#[derive(Debug, Clone, PartialEq)]
pub struct ArrowExpr {
    pub params: Vec<Param>,
    pub body: ArrowBody,
    pub span: Span,
}

/// Arrow function body.
#[derive(Debug, Clone, PartialEq)]
pub enum ArrowBody {
    Expr(Box<Expr>),
    Block(Block),
}

/// Match expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
    pub subject: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

/// Match arm.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

/// Pattern for match expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Literal),
    Identifier(Identifier),
    Wildcard(Span),
}

/// Template string expression.
#[derive(Debug, Clone, PartialEq)]
pub struct TemplateExpr {
    pub parts: Vec<TemplatePart>,
    pub span: Span,
}

/// Part of a template string.
#[derive(Debug, Clone, PartialEq)]
pub enum TemplatePart {
    String(String),
    Expr(Box<Expr>),
}

/// Assignment expression.
#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub target: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

/// Type expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    Named(NamedType),
    Array(ArrayType),
    Record(RecordType),
    Tuple(TupleType),
    Optional(Box<TypeExpr>),
    Union(Vec<TypeExpr>),
    Function(FunctionType),
}

/// Named type reference.
#[derive(Debug, Clone, PartialEq)]
pub struct NamedType {
    pub name: Identifier,
    pub args: Vec<TypeExpr>,
    pub span: Span,
}

/// Array type.
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element: Box<TypeExpr>,
    pub span: Span,
}

/// Record type.
#[derive(Debug, Clone, PartialEq)]
pub struct RecordType {
    pub key: Box<TypeExpr>,
    pub value: Box<TypeExpr>,
    pub span: Span,
}

/// Tuple type.
#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub elements: Vec<TypeExpr>,
    pub span: Span,
}

/// Function type.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<TypeExpr>,
    pub return_type: Box<TypeExpr>,
    pub span: Span,
}

/// Literals.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(StringLit),
    Number(NumberLit),
    Boolean(BoolLit),
    Null(Span),
}

/// String literal.
#[derive(Debug, Clone, PartialEq)]
pub struct StringLit {
    pub value: String,
    pub span: Span,
}

/// Number literal.
#[derive(Debug, Clone, PartialEq)]
pub struct NumberLit {
    pub value: f64,
    pub span: Span,
}

/// Boolean literal.
#[derive(Debug, Clone, PartialEq)]
pub struct BoolLit {
    pub value: bool,
    pub span: Span,
}

/// Identifier.
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

impl Identifier {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}
