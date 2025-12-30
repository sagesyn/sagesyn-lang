//! Type checker for the Sage Agent Programming Language.
//!
//! This crate provides static type checking for `.sag` programs.

use miette::{Diagnostic, LabeledSpan, SourceSpan};
use sag_parser::{
    Agent, ArrowBody, BinaryOp, Block, ElseClause, Expr, Field, ForStmt, Function, IfStmt, Item,
    LetStmt, Literal, MatchExpr, Pattern, Program, Skill, Span, Stmt, Tool, TypeDef, TypeDefKind,
    TypeExpr, UnaryOp, VarStmt, WhileStmt,
};
use std::collections::HashMap;
use thiserror::Error;

/// Error code category for type errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeErrorKind {
    /// Type mismatch between expected and actual
    TypeMismatch,
    /// Undefined type name
    UndefinedType,
    /// Undefined variable
    UndefinedVariable,
    /// Cannot call a non-function
    NotCallable,
    /// Wrong number of arguments
    ArgumentCount,
    /// Property does not exist on type
    NoSuchProperty,
    /// Cannot index into this type
    NotIndexable,
    /// Invalid operation for types
    InvalidOperation,
    /// Duplicate definition
    DuplicateDefinition,
}

impl TypeErrorKind {
    fn code(&self) -> &'static str {
        match self {
            Self::TypeMismatch => "sag::types::type_mismatch",
            Self::UndefinedType => "sag::types::undefined_type",
            Self::UndefinedVariable => "sag::types::undefined_variable",
            Self::NotCallable => "sag::types::not_callable",
            Self::ArgumentCount => "sag::types::argument_count",
            Self::NoSuchProperty => "sag::types::no_such_property",
            Self::NotIndexable => "sag::types::not_indexable",
            Self::InvalidOperation => "sag::types::invalid_operation",
            Self::DuplicateDefinition => "sag::types::duplicate_definition",
        }
    }
}

/// Type checking error with rich diagnostic information.
#[derive(Error, Debug, Clone)]
#[error("{message}")]
pub struct TypeError {
    kind: TypeErrorKind,
    message: String,
    src: String,
    span: SourceSpan,
    help: Option<String>,
    labels: Vec<LabeledSpan>,
}

impl Diagnostic for TypeError {
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

impl TypeError {
    /// Create a new type error.
    pub fn new(message: impl Into<String>, src: &str, span: Span) -> Self {
        Self {
            kind: TypeErrorKind::TypeMismatch,
            message: message.into(),
            src: src.to_string(),
            span: (span.start, span.len()).into(),
            help: None,
            labels: Vec::new(),
        }
    }

    /// Create an error with a specific kind.
    pub fn with_kind(
        kind: TypeErrorKind,
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

    /// Create a type mismatch error.
    pub fn type_mismatch(expected: &str, actual: &str, src: &str, span: Span) -> Self {
        Self::with_kind(
            TypeErrorKind::TypeMismatch,
            format!("expected `{expected}`, found `{actual}`"),
            src,
            span,
        )
    }

    /// Create an undefined type error.
    pub fn undefined_type(name: &str, src: &str, span: Span) -> Self {
        Self::with_kind(
            TypeErrorKind::UndefinedType,
            format!("undefined type `{name}`"),
            src,
            span,
        )
        .with_help("check that the type is defined or imported")
    }

    /// Create an undefined variable error.
    pub fn undefined_variable(name: &str, src: &str, span: Span, similar: Option<&str>) -> Self {
        let mut err = Self::with_kind(
            TypeErrorKind::UndefinedVariable,
            format!("undefined variable `{name}`"),
            src,
            span,
        );

        if let Some(similar_name) = similar {
            err = err.with_help(format!("did you mean `{similar_name}`?"));
        }

        err
    }

    /// Create a not callable error.
    pub fn not_callable(ty: &str, src: &str, span: Span) -> Self {
        Self::with_kind(
            TypeErrorKind::NotCallable,
            format!("type `{ty}` is not callable"),
            src,
            span,
        )
        .with_help("only functions and tools can be called")
    }

    /// Create an argument count error.
    pub fn argument_count(
        expected: usize,
        actual: usize,
        src: &str,
        span: Span,
        fn_span: Option<Span>,
    ) -> Self {
        let mut err = Self::with_kind(
            TypeErrorKind::ArgumentCount,
            format!(
                "expected {} argument{}, found {}",
                expected,
                if expected == 1 { "" } else { "s" },
                actual
            ),
            src,
            span,
        );

        if let Some(def_span) = fn_span {
            err = err.with_label("function defined here", def_span);
        }

        err
    }

    /// Create a no such property error.
    pub fn no_such_property(ty: &str, property: &str, src: &str, span: Span) -> Self {
        Self::with_kind(
            TypeErrorKind::NoSuchProperty,
            format!("type `{ty}` has no property `{property}`"),
            src,
            span,
        )
    }

    /// Create a duplicate definition error.
    pub fn duplicate_definition(name: &str, src: &str, span: Span, original: Option<Span>) -> Self {
        let mut err = Self::with_kind(
            TypeErrorKind::DuplicateDefinition,
            format!("`{name}` is already defined"),
            src,
            span,
        );

        if let Some(orig_span) = original {
            err = err.with_label("originally defined here", orig_span);
        }

        err
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
    pub fn kind(&self) -> TypeErrorKind {
        self.kind
    }
}

/// Resolved type representation.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Primitive string type.
    String,
    /// Primitive number type (f64).
    Number,
    /// Primitive boolean type.
    Boolean,
    /// Primitive null type.
    Null,
    /// Timestamp type.
    Timestamp,
    /// Array type with element type.
    Array(Box<Type>),
    /// Record (map) type with key and value types.
    Record(Box<Type>, Box<Type>),
    /// Tuple type with element types.
    Tuple(Vec<Type>),
    /// Optional type (nullable).
    Optional(Box<Type>),
    /// Union type.
    Union(Vec<Type>),
    /// Function type.
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    /// Named type (struct or alias).
    Named(String),
    /// Unknown type (for inference).
    Unknown,
    /// Error type (for error recovery).
    Error,
}

impl Type {
    /// Check if this type is assignable to another type.
    pub fn is_assignable_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Error, _) | (_, Type::Error) => true,
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (a, b) if a == b => true,
            (Type::Null, Type::Optional(_)) => true,
            (t, Type::Optional(inner)) => t.is_assignable_to(inner),
            (Type::Array(a), Type::Array(b)) => a.is_assignable_to(b),
            (Type::Record(k1, v1), Type::Record(k2, v2)) => {
                k1.is_assignable_to(k2) && v1.is_assignable_to(v2)
            }
            (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
                a.iter().zip(b.iter()).all(|(x, y)| x.is_assignable_to(y))
            }
            (t, Type::Union(types)) => types.iter().any(|u| t.is_assignable_to(u)),
            (Type::Union(types), t) => types.iter().all(|u| u.is_assignable_to(t)),
            _ => false,
        }
    }

    /// Check if this type is numeric.
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Number)
    }

    /// Check if this type is boolean.
    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Boolean)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::String => write!(f, "string"),
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::Null => write!(f, "null"),
            Type::Timestamp => write!(f, "timestamp"),
            Type::Array(inner) => write!(f, "{inner}[]"),
            Type::Record(k, v) => write!(f, "Record<{k}, {v}>"),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{t}")?;
                }
                write!(f, ")")
            }
            Type::Optional(inner) => write!(f, "{inner}?"),
            Type::Union(types) => {
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{t}")?;
                }
                Ok(())
            }
            Type::Function {
                params,
                return_type,
            } => {
                write!(f, "(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{p}")?;
                }
                write!(f, ") -> {return_type}")
            }
            Type::Named(name) => write!(f, "{name}"),
            Type::Unknown => write!(f, "unknown"),
            Type::Error => write!(f, "<error>"),
        }
    }
}

/// Type environment for tracking bindings.
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    /// Variable bindings.
    bindings: HashMap<String, Type>,
    /// Type definitions.
    types: HashMap<String, Type>,
    /// Parent environment (for scoping).
    parent: Option<Box<TypeEnv>>,
}

impl TypeEnv {
    /// Create a new empty type environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a child environment.
    pub fn child(&self) -> Self {
        Self {
            bindings: HashMap::new(),
            types: HashMap::new(),
            parent: Some(Box::new(self.clone())),
        }
    }

    /// Define a variable binding.
    pub fn define(&mut self, name: impl Into<String>, ty: Type) {
        self.bindings.insert(name.into(), ty);
    }

    /// Define a type.
    pub fn define_type(&mut self, name: impl Into<String>, ty: Type) {
        self.types.insert(name.into(), ty);
    }

    /// Look up a variable binding.
    pub fn lookup(&self, name: &str) -> Option<&Type> {
        self.bindings
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.lookup(name)))
    }

    /// Look up a type definition.
    pub fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.types
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.lookup_type(name)))
    }

    /// Find a similar variable name (for "did you mean?" suggestions).
    pub fn find_similar(&self, name: &str) -> Option<String> {
        let mut all_names: Vec<&String> = self.bindings.keys().collect();
        if let Some(ref parent) = self.parent {
            all_names.extend(parent.all_binding_names());
        }

        all_names
            .into_iter()
            .filter(|n| levenshtein(name, n) <= 2)
            .min_by_key(|n| levenshtein(name, n))
            .cloned()
    }

    /// Get all binding names (including parent scopes).
    fn all_binding_names(&self) -> Vec<&String> {
        let mut names: Vec<&String> = self.bindings.keys().collect();
        if let Some(ref parent) = self.parent {
            names.extend(parent.all_binding_names());
        }
        names
    }
}

/// Simple Levenshtein distance for similar name suggestions.
#[allow(clippy::needless_range_loop)]
fn levenshtein(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let len_a = a.len();
    let len_b = b.len();

    if len_a == 0 {
        return len_b;
    }
    if len_b == 0 {
        return len_a;
    }

    let mut matrix = vec![vec![0usize; len_b + 1]; len_a + 1];

    for i in 0..=len_a {
        matrix[i][0] = i;
    }
    for j in 0..=len_b {
        matrix[0][j] = j;
    }

    for i in 1..=len_a {
        for j in 1..=len_b {
            let cost = if a[i - 1] == b[j - 1] { 0 } else { 1 };
            matrix[i][j] = (matrix[i - 1][j] + 1)
                .min(matrix[i][j - 1] + 1)
                .min(matrix[i - 1][j - 1] + cost);
        }
    }

    matrix[len_a][len_b]
}

/// The type checker.
pub struct TypeChecker<'src> {
    source: &'src str,
    env: TypeEnv,
    errors: Vec<TypeError>,
    /// Current function's expected return type (for checking return statements).
    current_return_type: Option<Type>,
}

impl<'src> TypeChecker<'src> {
    /// Create a new type checker.
    pub fn new(source: &'src str) -> Self {
        let mut env = TypeEnv::new();

        // Register built-in types
        env.define_type("string", Type::String);
        env.define_type("number", Type::Number);
        env.define_type("boolean", Type::Boolean);
        env.define_type("timestamp", Type::Timestamp);

        Self {
            source,
            env,
            errors: Vec::new(),
            current_return_type: None,
        }
    }

    /// Check a program and return any type errors.
    pub fn check(source: &str, program: &Program) -> Result<(), Vec<TypeError>> {
        let mut checker = TypeChecker::new(source);
        checker.check_program(program);

        if checker.errors.is_empty() {
            Ok(())
        } else {
            Err(checker.errors)
        }
    }

    fn check_program(&mut self, program: &Program) {
        // First pass: register all type definitions
        for item in &program.items {
            if let Item::TypeDef(typedef) = item {
                self.register_typedef(typedef);
            }
        }

        // Second pass: register all functions and agents
        for item in &program.items {
            match item {
                Item::Function(func) => self.register_function(func),
                Item::Agent(agent) => self.register_agent(agent),
                _ => {}
            }
        }

        // Third pass: type check all items
        for item in &program.items {
            self.check_item(item);
        }
    }

    fn register_typedef(&mut self, typedef: &TypeDef) {
        let ty = match &typedef.kind {
            TypeDefKind::Struct(_fields) => {
                // For struct types, we just register it as a named type
                // The fields are checked when the type is used
                Type::Named(typedef.name.name.clone())
            }
            TypeDefKind::Alias(type_expr) => self.resolve_type(type_expr),
        };
        self.env.define_type(&typedef.name.name, ty);
    }

    fn register_function(&mut self, func: &Function) {
        let params: Vec<Type> = func
            .params
            .iter()
            .map(|p| self.resolve_type(&p.ty))
            .collect();
        let return_type = func
            .return_type
            .as_ref()
            .map(|t| self.resolve_type(t))
            .unwrap_or(Type::Null);

        let ty = Type::Function {
            params,
            return_type: Box::new(return_type),
        };
        self.env.define(&func.name.name, ty);
    }

    fn register_agent(&mut self, agent: &Agent) {
        // Register agent as a named type
        self.env
            .define_type(&agent.name.name, Type::Named(agent.name.name.clone()));
    }

    fn check_item(&mut self, item: &Item) {
        match item {
            Item::Agent(agent) => self.check_agent(agent),
            Item::Skill(skill) => self.check_skill(skill),
            Item::TypeDef(typedef) => self.check_typedef(typedef),
            Item::Function(func) => self.check_function(func),
        }
    }

    fn check_agent(&mut self, agent: &Agent) {
        // Create a child scope for the agent
        let parent_env = std::mem::take(&mut self.env);
        self.env = parent_env.child();

        // Register state fields in the agent scope
        if let Some(ref state) = agent.state {
            for field in &state.fields {
                let ty = self.resolve_type(&field.ty);
                self.env.define(&field.name.name, ty);
            }
        }

        // Register tools as callable functions in the agent scope
        for tool in &agent.tools {
            let params: Vec<Type> = tool
                .params
                .iter()
                .map(|p| self.resolve_type(&p.ty))
                .collect();
            let return_type = tool
                .return_type
                .as_ref()
                .map(|t| self.resolve_type(t))
                .unwrap_or(Type::Unknown);
            self.env.define(
                &tool.name.name,
                Type::Function {
                    params,
                    return_type: Box::new(return_type),
                },
            );
        }

        // Check tools
        for tool in &agent.tools {
            self.check_tool(tool);
        }

        // Check event handlers
        for handler in &agent.handlers {
            self.check_block(&handler.body);
        }

        // Restore parent scope
        self.env = *self.env.parent.take().unwrap();
    }

    fn check_skill(&mut self, skill: &Skill) {
        for item in &skill.body {
            self.check_item(item);
        }
    }

    fn check_typedef(&mut self, typedef: &TypeDef) {
        if let TypeDefKind::Struct(fields) = &typedef.kind {
            self.check_fields(fields);
        }
    }

    fn check_fields(&mut self, fields: &[Field]) {
        let mut seen: HashMap<&str, Span> = HashMap::new();
        for field in fields {
            if let Some(original_span) = seen.get(field.name.name.as_str()) {
                self.errors.push(TypeError::duplicate_definition(
                    &field.name.name,
                    self.source,
                    field.span,
                    Some(*original_span),
                ));
            } else {
                seen.insert(&field.name.name, field.span);
            }
        }
    }

    fn check_tool(&mut self, tool: &Tool) {
        if let Some(ref body) = tool.body {
            // Create a child scope for the tool
            let parent_env = std::mem::take(&mut self.env);
            self.env = parent_env.child();

            // Register parameters
            for param in &tool.params {
                let ty = self.resolve_type(&param.ty);
                self.env.define(&param.name.name, ty);
            }

            // Set expected return type
            let return_type = tool
                .return_type
                .as_ref()
                .map(|t| self.resolve_type(t))
                .unwrap_or(Type::Null);
            self.current_return_type = Some(return_type);

            // Check body
            self.check_block(body);

            // Reset return type
            self.current_return_type = None;

            // Restore parent scope
            self.env = *self.env.parent.take().unwrap();
        }
    }

    fn check_function(&mut self, func: &Function) {
        // Create a child scope for the function
        let parent_env = std::mem::take(&mut self.env);
        self.env = parent_env.child();

        // Register parameters
        for param in &func.params {
            let ty = self.resolve_type(&param.ty);

            // Check default value if present
            if let Some(ref default) = param.default {
                let default_ty = self.infer_expr(default);
                if !default_ty.is_assignable_to(&ty) {
                    self.errors.push(TypeError::type_mismatch(
                        &ty.to_string(),
                        &default_ty.to_string(),
                        self.source,
                        Self::expr_span(default),
                    ));
                }
            }

            self.env.define(&param.name.name, ty);
        }

        // Set expected return type
        let return_type = func
            .return_type
            .as_ref()
            .map(|t| self.resolve_type(t))
            .unwrap_or(Type::Null);
        self.current_return_type = Some(return_type);

        // Check body
        self.check_block(&func.body);

        // Reset return type
        self.current_return_type = None;

        // Restore parent scope
        self.env = *self.env.parent.take().unwrap();
    }

    fn check_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(let_stmt) => self.check_let_stmt(let_stmt),
            Stmt::Var(var_stmt) => self.check_var_stmt(var_stmt),
            Stmt::Expr(expr_stmt) => {
                self.infer_expr(&expr_stmt.expr);
            }
            Stmt::If(if_stmt) => self.check_if_stmt(if_stmt),
            Stmt::For(for_stmt) => self.check_for_stmt(for_stmt),
            Stmt::While(while_stmt) => self.check_while_stmt(while_stmt),
            Stmt::Return(ret_stmt) => self.check_return_stmt(ret_stmt),
            Stmt::Emit(emit_stmt) => {
                self.infer_expr(&emit_stmt.value);
            }
            Stmt::Block(block) => {
                let parent_env = std::mem::take(&mut self.env);
                self.env = parent_env.child();
                self.check_block(block);
                self.env = *self.env.parent.take().unwrap();
            }
        }
    }

    fn check_let_stmt(&mut self, stmt: &LetStmt) {
        let value_ty = self.infer_expr(&stmt.value);

        let declared_ty = if let Some(ref ty) = stmt.ty {
            let t = self.resolve_type(ty);
            if !value_ty.is_assignable_to(&t) {
                self.errors.push(TypeError::type_mismatch(
                    &t.to_string(),
                    &value_ty.to_string(),
                    self.source,
                    Self::expr_span(&stmt.value),
                ));
            }
            t
        } else {
            value_ty
        };

        self.env.define(&stmt.name.name, declared_ty);
    }

    fn check_var_stmt(&mut self, stmt: &VarStmt) {
        let value_ty = self.infer_expr(&stmt.value);

        let declared_ty = if let Some(ref ty) = stmt.ty {
            let t = self.resolve_type(ty);
            if !value_ty.is_assignable_to(&t) {
                self.errors.push(TypeError::type_mismatch(
                    &t.to_string(),
                    &value_ty.to_string(),
                    self.source,
                    Self::expr_span(&stmt.value),
                ));
            }
            t
        } else {
            value_ty
        };

        self.env.define(&stmt.name.name, declared_ty);
    }

    fn check_if_stmt(&mut self, stmt: &IfStmt) {
        let cond_ty = self.infer_expr(&stmt.condition);
        if !cond_ty.is_assignable_to(&Type::Boolean) {
            self.errors.push(TypeError::type_mismatch(
                "boolean",
                &cond_ty.to_string(),
                self.source,
                Self::expr_span(&stmt.condition),
            ));
        }

        // Check then block in a child scope
        let parent_env = std::mem::take(&mut self.env);
        self.env = parent_env.child();
        self.check_block(&stmt.then_block);
        self.env = *self.env.parent.take().unwrap();

        // Check else clause
        if let Some(ref else_clause) = stmt.else_block {
            match else_clause.as_ref() {
                ElseClause::ElseIf(else_if) => self.check_if_stmt(else_if),
                ElseClause::Else(block) => {
                    let parent_env = std::mem::take(&mut self.env);
                    self.env = parent_env.child();
                    self.check_block(block);
                    self.env = *self.env.parent.take().unwrap();
                }
            }
        }
    }

    fn check_for_stmt(&mut self, stmt: &ForStmt) {
        let iterable_ty = self.infer_expr(&stmt.iterable);

        // Infer element type from iterable
        let element_ty = match &iterable_ty {
            Type::Array(inner) => *inner.clone(),
            Type::String => Type::String, // Iterating over string gives chars
            _ => {
                self.errors.push(TypeError::with_kind(
                    TypeErrorKind::InvalidOperation,
                    format!("cannot iterate over `{iterable_ty}`"),
                    self.source,
                    Self::expr_span(&stmt.iterable),
                ));
                Type::Error
            }
        };

        // Create child scope with loop variable
        let parent_env = std::mem::take(&mut self.env);
        self.env = parent_env.child();
        self.env.define(&stmt.binding.name, element_ty);
        self.check_block(&stmt.body);
        self.env = *self.env.parent.take().unwrap();
    }

    fn check_while_stmt(&mut self, stmt: &WhileStmt) {
        let cond_ty = self.infer_expr(&stmt.condition);
        if !cond_ty.is_assignable_to(&Type::Boolean) {
            self.errors.push(TypeError::type_mismatch(
                "boolean",
                &cond_ty.to_string(),
                self.source,
                Self::expr_span(&stmt.condition),
            ));
        }

        // Check body in a child scope
        let parent_env = std::mem::take(&mut self.env);
        self.env = parent_env.child();
        self.check_block(&stmt.body);
        self.env = *self.env.parent.take().unwrap();
    }

    fn check_return_stmt(&mut self, stmt: &sag_parser::ReturnStmt) {
        let return_ty = stmt
            .value
            .as_ref()
            .map(|e| self.infer_expr(e))
            .unwrap_or(Type::Null);

        if let Some(ref expected) = self.current_return_type {
            if !return_ty.is_assignable_to(expected) {
                self.errors.push(TypeError::type_mismatch(
                    &expected.to_string(),
                    &return_ty.to_string(),
                    self.source,
                    stmt.span,
                ));
            }
        }
    }

    /// Infer the type of an expression.
    fn infer_expr(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Literal(lit) => self.infer_literal(lit),
            Expr::Identifier(ident) => self.infer_identifier(ident),
            Expr::Binary(bin) => self.infer_binary(bin),
            Expr::Unary(un) => self.infer_unary(un),
            Expr::Call(call) => self.infer_call(call),
            Expr::Member(mem) => self.infer_member(mem),
            Expr::Index(idx) => self.infer_index(idx),
            Expr::Array(arr) => self.infer_array(arr),
            Expr::Record(rec) => self.infer_record(rec),
            Expr::Await(aw) => self.infer_expr(&aw.expr),
            Expr::Arrow(arrow) => self.infer_arrow(arrow),
            Expr::Match(m) => self.infer_match(m),
            Expr::Template(_) => Type::String,
            Expr::Assign(assign) => self.infer_assign(assign),
        }
    }

    fn infer_literal(&self, lit: &Literal) -> Type {
        match lit {
            Literal::String(_) => Type::String,
            Literal::Number(_) => Type::Number,
            Literal::Boolean(_) => Type::Boolean,
            Literal::Null(_) => Type::Null,
        }
    }

    fn infer_identifier(&mut self, ident: &sag_parser::Identifier) -> Type {
        if let Some(ty) = self.env.lookup(&ident.name) {
            ty.clone()
        } else {
            let similar = self.env.find_similar(&ident.name);
            self.errors.push(TypeError::undefined_variable(
                &ident.name,
                self.source,
                ident.span,
                similar.as_deref(),
            ));
            Type::Error
        }
    }

    fn infer_binary(&mut self, bin: &sag_parser::BinaryExpr) -> Type {
        let left = self.infer_expr(&bin.left);
        let right = self.infer_expr(&bin.right);

        match bin.op {
            // Arithmetic operators require numbers
            BinaryOp::Add => {
                // Add can work on strings (concatenation) or numbers
                if left.is_assignable_to(&Type::String) && right.is_assignable_to(&Type::String) {
                    Type::String
                } else if left.is_numeric() && right.is_numeric() {
                    Type::Number
                } else {
                    self.errors.push(TypeError::with_kind(
                        TypeErrorKind::InvalidOperation,
                        format!("cannot add `{left}` and `{right}`"),
                        self.source,
                        bin.span,
                    ));
                    Type::Error
                }
            }
            BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod | BinaryOp::Pow => {
                if !left.is_numeric() || !right.is_numeric() {
                    self.errors.push(TypeError::with_kind(
                        TypeErrorKind::InvalidOperation,
                        format!(
                            "arithmetic operation requires numbers, found `{left}` and `{right}`"
                        ),
                        self.source,
                        bin.span,
                    ));
                }
                Type::Number
            }
            // Comparison operators return boolean
            BinaryOp::Eq | BinaryOp::NotEq => Type::Boolean,
            BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => {
                if !left.is_numeric() || !right.is_numeric() {
                    self.errors.push(TypeError::with_kind(
                        TypeErrorKind::InvalidOperation,
                        format!("comparison requires numbers, found `{left}` and `{right}`"),
                        self.source,
                        bin.span,
                    ));
                }
                Type::Boolean
            }
            // Logical operators require booleans
            BinaryOp::And | BinaryOp::Or => {
                if !left.is_boolean() {
                    self.errors.push(TypeError::type_mismatch(
                        "boolean",
                        &left.to_string(),
                        self.source,
                        Self::expr_span(&bin.left),
                    ));
                }
                if !right.is_boolean() {
                    self.errors.push(TypeError::type_mismatch(
                        "boolean",
                        &right.to_string(),
                        self.source,
                        Self::expr_span(&bin.right),
                    ));
                }
                Type::Boolean
            }
        }
    }

    fn infer_unary(&mut self, un: &sag_parser::UnaryExpr) -> Type {
        let operand = self.infer_expr(&un.operand);

        match un.op {
            UnaryOp::Not => {
                if !operand.is_boolean() {
                    self.errors.push(TypeError::type_mismatch(
                        "boolean",
                        &operand.to_string(),
                        self.source,
                        Self::expr_span(&un.operand),
                    ));
                }
                Type::Boolean
            }
            UnaryOp::Neg => {
                if !operand.is_numeric() {
                    self.errors.push(TypeError::type_mismatch(
                        "number",
                        &operand.to_string(),
                        self.source,
                        Self::expr_span(&un.operand),
                    ));
                }
                Type::Number
            }
        }
    }

    fn infer_call(&mut self, call: &sag_parser::CallExpr) -> Type {
        let callee_ty = self.infer_expr(&call.callee);

        match callee_ty {
            Type::Function {
                params,
                return_type,
            } => {
                // Check argument count
                if call.args.len() != params.len() {
                    self.errors.push(TypeError::argument_count(
                        params.len(),
                        call.args.len(),
                        self.source,
                        call.span,
                        None,
                    ));
                }

                // Check argument types
                for (arg, param_ty) in call.args.iter().zip(params.iter()) {
                    let arg_ty = self.infer_expr(arg);
                    if !arg_ty.is_assignable_to(param_ty) {
                        self.errors.push(TypeError::type_mismatch(
                            &param_ty.to_string(),
                            &arg_ty.to_string(),
                            self.source,
                            Self::expr_span(arg),
                        ));
                    }
                }

                *return_type
            }
            Type::Error => Type::Error,
            _ => {
                self.errors.push(TypeError::not_callable(
                    &callee_ty.to_string(),
                    self.source,
                    call.span,
                ));
                Type::Error
            }
        }
    }

    fn infer_member(&mut self, mem: &sag_parser::MemberExpr) -> Type {
        let object_ty = self.infer_expr(&mem.object);

        // For now, we're lenient with property access on unknown/named types
        match &object_ty {
            Type::Named(_) | Type::Unknown | Type::Error => Type::Unknown,
            Type::Record(_, value_ty) => *value_ty.clone(),
            _ => {
                self.errors.push(TypeError::no_such_property(
                    &object_ty.to_string(),
                    &mem.property.name,
                    self.source,
                    mem.span,
                ));
                Type::Error
            }
        }
    }

    fn infer_index(&mut self, idx: &sag_parser::IndexExpr) -> Type {
        let object_ty = self.infer_expr(&idx.object);
        let index_ty = self.infer_expr(&idx.index);

        match &object_ty {
            Type::Array(inner) => {
                if !index_ty.is_numeric() {
                    self.errors.push(TypeError::type_mismatch(
                        "number",
                        &index_ty.to_string(),
                        self.source,
                        Self::expr_span(&idx.index),
                    ));
                }
                *inner.clone()
            }
            Type::Record(key_ty, value_ty) => {
                if !index_ty.is_assignable_to(key_ty) {
                    self.errors.push(TypeError::type_mismatch(
                        &key_ty.to_string(),
                        &index_ty.to_string(),
                        self.source,
                        Self::expr_span(&idx.index),
                    ));
                }
                *value_ty.clone()
            }
            Type::String => {
                if !index_ty.is_numeric() {
                    self.errors.push(TypeError::type_mismatch(
                        "number",
                        &index_ty.to_string(),
                        self.source,
                        Self::expr_span(&idx.index),
                    ));
                }
                Type::String
            }
            Type::Unknown | Type::Error => Type::Unknown,
            _ => {
                self.errors.push(TypeError::with_kind(
                    TypeErrorKind::NotIndexable,
                    format!("type `{object_ty}` is not indexable"),
                    self.source,
                    idx.span,
                ));
                Type::Error
            }
        }
    }

    fn infer_array(&mut self, arr: &sag_parser::ArrayExpr) -> Type {
        if arr.elements.is_empty() {
            return Type::Array(Box::new(Type::Unknown));
        }

        // Infer element type from first element
        let first_ty = self.infer_expr(&arr.elements[0]);

        // Check all elements have compatible types
        for (i, elem) in arr.elements.iter().enumerate().skip(1) {
            let elem_ty = self.infer_expr(elem);
            if !elem_ty.is_assignable_to(&first_ty) && !first_ty.is_assignable_to(&elem_ty) {
                self.errors.push(
                    TypeError::type_mismatch(
                        &first_ty.to_string(),
                        &elem_ty.to_string(),
                        self.source,
                        Self::expr_span(elem),
                    )
                    .with_help(format!(
                        "array element {i} has type `{elem_ty}`, but expected `{first_ty}`"
                    )),
                );
            }
        }

        Type::Array(Box::new(first_ty))
    }

    fn infer_record(&mut self, rec: &sag_parser::RecordExpr) -> Type {
        // Just check all field expressions for now
        // We don't have a good way to represent record literal types
        for (_, expr) in &rec.fields {
            self.infer_expr(expr);
        }
        Type::Unknown
    }

    fn infer_arrow(&mut self, arrow: &sag_parser::ArrowExpr) -> Type {
        // Create child scope for arrow function
        let parent_env = std::mem::take(&mut self.env);
        self.env = parent_env.child();

        let params: Vec<Type> = arrow
            .params
            .iter()
            .map(|p| {
                let ty = self.resolve_type(&p.ty);
                self.env.define(&p.name.name, ty.clone());
                ty
            })
            .collect();

        let return_type = match &arrow.body {
            ArrowBody::Expr(expr) => self.infer_expr(expr),
            ArrowBody::Block(block) => {
                self.check_block(block);
                Type::Unknown // Block return type would need more analysis
            }
        };

        // Restore parent scope
        self.env = *self.env.parent.take().unwrap();

        Type::Function {
            params,
            return_type: Box::new(return_type),
        }
    }

    fn infer_match(&mut self, m: &MatchExpr) -> Type {
        let subject_ty = self.infer_expr(&m.subject);

        // All arms should have compatible types
        let mut result_ty: Option<Type> = None;

        for arm in &m.arms {
            // Check pattern (simple for now)
            match &arm.pattern {
                Pattern::Literal(lit) => {
                    let lit_ty = self.infer_literal(lit);
                    if !lit_ty.is_assignable_to(&subject_ty) {
                        self.errors.push(TypeError::type_mismatch(
                            &subject_ty.to_string(),
                            &lit_ty.to_string(),
                            self.source,
                            arm.span,
                        ));
                    }
                }
                Pattern::Identifier(ident) => {
                    // Binding pattern - introduces variable
                    self.env.define(&ident.name, subject_ty.clone());
                }
                Pattern::Wildcard(_) => {
                    // Wildcard matches anything
                }
            }

            let arm_ty = self.infer_expr(&arm.body);

            if let Some(ref prev_ty) = result_ty {
                if !arm_ty.is_assignable_to(prev_ty) && !prev_ty.is_assignable_to(&arm_ty) {
                    self.errors.push(TypeError::type_mismatch(
                        &prev_ty.to_string(),
                        &arm_ty.to_string(),
                        self.source,
                        arm.span,
                    ));
                }
            } else {
                result_ty = Some(arm_ty);
            }
        }

        result_ty.unwrap_or(Type::Unknown)
    }

    fn infer_assign(&mut self, assign: &sag_parser::AssignExpr) -> Type {
        let target_ty = self.infer_expr(&assign.target);
        let value_ty = self.infer_expr(&assign.value);

        if !value_ty.is_assignable_to(&target_ty) {
            self.errors.push(TypeError::type_mismatch(
                &target_ty.to_string(),
                &value_ty.to_string(),
                self.source,
                Self::expr_span(&assign.value),
            ));
        }

        value_ty
    }

    /// Resolve a type expression to a Type.
    pub fn resolve_type(&self, type_expr: &TypeExpr) -> Type {
        match type_expr {
            TypeExpr::Named(named) => match named.name.name.as_str() {
                "string" => Type::String,
                "number" => Type::Number,
                "boolean" => Type::Boolean,
                "timestamp" => Type::Timestamp,
                name => self
                    .env
                    .lookup_type(name)
                    .cloned()
                    .unwrap_or(Type::Named(name.to_string())),
            },
            TypeExpr::Array(arr) => Type::Array(Box::new(self.resolve_type(&arr.element))),
            TypeExpr::Record(rec) => Type::Record(
                Box::new(self.resolve_type(&rec.key)),
                Box::new(self.resolve_type(&rec.value)),
            ),
            TypeExpr::Tuple(tup) => {
                Type::Tuple(tup.elements.iter().map(|t| self.resolve_type(t)).collect())
            }
            TypeExpr::Optional(inner) => Type::Optional(Box::new(self.resolve_type(inner))),
            TypeExpr::Union(types) => {
                Type::Union(types.iter().map(|t| self.resolve_type(t)).collect())
            }
            TypeExpr::Function(func) => Type::Function {
                params: func.params.iter().map(|t| self.resolve_type(t)).collect(),
                return_type: Box::new(self.resolve_type(&func.return_type)),
            },
        }
    }

    /// Get the span of an expression.
    fn expr_span(expr: &Expr) -> Span {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::String(s) => s.span,
                Literal::Number(n) => n.span,
                Literal::Boolean(b) => b.span,
                Literal::Null(span) => *span,
            },
            Expr::Identifier(id) => id.span,
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
    use sag_parser::Parser;

    #[test]
    fn test_type_assignability() {
        assert!(Type::String.is_assignable_to(&Type::String));
        assert!(Type::Number.is_assignable_to(&Type::Number));
        assert!(!Type::String.is_assignable_to(&Type::Number));
    }

    #[test]
    fn test_optional_assignability() {
        let optional_string = Type::Optional(Box::new(Type::String));
        assert!(Type::String.is_assignable_to(&optional_string));
        assert!(Type::Null.is_assignable_to(&optional_string));
        assert!(!Type::Number.is_assignable_to(&optional_string));
    }

    #[test]
    fn test_union_assignability() {
        let string_or_number = Type::Union(vec![Type::String, Type::Number]);
        assert!(Type::String.is_assignable_to(&string_or_number));
        assert!(Type::Number.is_assignable_to(&string_or_number));
        assert!(!Type::Boolean.is_assignable_to(&string_or_number));
    }

    #[test]
    fn test_type_env() {
        let mut env = TypeEnv::new();
        env.define("x", Type::String);
        env.define_type("User", Type::Named("User".to_string()));

        assert_eq!(env.lookup("x"), Some(&Type::String));
        assert!(env.lookup("y").is_none());
    }

    #[test]
    fn test_type_env_scoping() {
        let mut parent = TypeEnv::new();
        parent.define("x", Type::String);

        let mut child = parent.child();
        child.define("y", Type::Number);

        assert_eq!(child.lookup("x"), Some(&Type::String));
        assert_eq!(child.lookup("y"), Some(&Type::Number));
        assert!(parent.lookup("y").is_none());
    }

    #[test]
    fn test_check_valid_function() {
        let source = r#"
            fn add(a: number, b: number) -> number {
                return a + b
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let result = TypeChecker::check(source, &program);
        assert!(result.is_ok(), "Expected valid program to type check");
    }

    #[test]
    fn test_check_type_mismatch() {
        let source = r#"
            fn get_name() -> string {
                return 42
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let result = TypeChecker::check(source, &program);
        assert!(result.is_err(), "Expected type error");
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, TypeErrorKind::TypeMismatch);
    }

    #[test]
    fn test_check_undefined_variable() {
        let source = r#"
            fn get_value() -> number {
                return unknown_var
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let result = TypeChecker::check(source, &program);
        assert!(result.is_err(), "Expected undefined variable error");
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, TypeErrorKind::UndefinedVariable);
    }

    #[test]
    fn test_check_let_binding() {
        let source = r#"
            fn process() -> string {
                let name: string = "test"
                return name
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let result = TypeChecker::check(source, &program);
        assert!(result.is_ok(), "Expected valid let binding");
    }

    #[test]
    fn test_check_binary_operations() {
        let source = r#"
            fn calc(x: number, y: number) -> number {
                return x + y * 2 - 1
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let result = TypeChecker::check(source, &program);
        assert!(result.is_ok(), "Expected valid binary operations");
    }

    #[test]
    fn test_check_boolean_operations() {
        let source = r#"
            fn check(a: boolean, b: boolean) -> boolean {
                return a && b || !a
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let result = TypeChecker::check(source, &program);
        assert!(result.is_ok(), "Expected valid boolean operations");
    }

    #[test]
    fn test_check_comparison_operations() {
        let source = r#"
            fn compare(x: number, y: number) -> boolean {
                return x > y && x <= 100
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let result = TypeChecker::check(source, &program);
        assert!(result.is_ok(), "Expected valid comparison operations");
    }

    #[test]
    fn test_check_function_call() {
        let source = r#"
            fn greet(name: string) -> string {
                return name
            }
            fn main() -> string {
                return greet("World")
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let result = TypeChecker::check(source, &program);
        assert!(result.is_ok(), "Expected valid function call");
    }

    #[test]
    fn test_check_wrong_argument_count() {
        let source = r#"
            fn add(a: number, b: number) -> number {
                return a + b
            }
            fn main() -> number {
                return add(1)
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let result = TypeChecker::check(source, &program);
        assert!(result.is_err(), "Expected wrong argument count error");
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, TypeErrorKind::ArgumentCount);
    }

    #[test]
    fn test_check_type_alias() {
        let source = r#"
            type UserId = string
            fn get_user(id: UserId) -> UserId {
                return id
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let result = TypeChecker::check(source, &program);
        assert!(result.is_ok(), "Expected valid type alias usage");
    }
}
