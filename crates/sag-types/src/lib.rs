//! Type checker for the Sage Agent Programming Language.
//!
//! This crate provides static type checking for `.sag` programs.

use miette::{Diagnostic, SourceSpan};
use sag_parser::{Program, TypeExpr, Span};
use std::collections::HashMap;
use thiserror::Error;

/// Type checking error.
#[derive(Error, Diagnostic, Debug, Clone)]
#[error("{message}")]
#[diagnostic(code(sag::types::error))]
pub struct TypeError {
    message: String,
    #[source_code]
    src: String,
    #[label("{message}")]
    span: SourceSpan,
}

impl TypeError {
    pub fn new(message: impl Into<String>, src: &str, span: Span) -> Self {
        Self {
            message: message.into(),
            src: src.to_string(),
            span: (span.start, span.len()).into(),
        }
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
        self.bindings.get(name).or_else(|| {
            self.parent.as_ref().and_then(|p| p.lookup(name))
        })
    }

    /// Look up a type definition.
    pub fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name).or_else(|| {
            self.parent.as_ref().and_then(|p| p.lookup_type(name))
        })
    }
}

/// The type checker.
pub struct TypeChecker<'src> {
    source: &'src str,
    env: TypeEnv,
    errors: Vec<TypeError>,
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

    fn check_program(&mut self, _program: &Program) {
        // TODO: Implement full type checking
        // This is a stub that will be expanded in future iterations
    }

    /// Resolve a type expression to a Type.
    pub fn resolve_type(&self, type_expr: &TypeExpr) -> Type {
        match type_expr {
            TypeExpr::Named(named) => {
                match named.name.name.as_str() {
                    "string" => Type::String,
                    "number" => Type::Number,
                    "boolean" => Type::Boolean,
                    "timestamp" => Type::Timestamp,
                    name => self.env.lookup_type(name).cloned().unwrap_or(Type::Named(name.to_string())),
                }
            }
            TypeExpr::Array(arr) => {
                Type::Array(Box::new(self.resolve_type(&arr.element)))
            }
            TypeExpr::Record(rec) => {
                Type::Record(
                    Box::new(self.resolve_type(&rec.key)),
                    Box::new(self.resolve_type(&rec.value)),
                )
            }
            TypeExpr::Tuple(tup) => {
                Type::Tuple(tup.elements.iter().map(|t| self.resolve_type(t)).collect())
            }
            TypeExpr::Optional(inner) => {
                Type::Optional(Box::new(self.resolve_type(inner)))
            }
            TypeExpr::Union(types) => {
                Type::Union(types.iter().map(|t| self.resolve_type(t)).collect())
            }
            TypeExpr::Function(func) => {
                Type::Function {
                    params: func.params.iter().map(|t| self.resolve_type(t)).collect(),
                    return_type: Box::new(self.resolve_type(&func.return_type)),
                }
            }
        }
    }

    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(TypeError::new(message, self.source, span));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
