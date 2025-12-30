//! Code generator for the Sage Agent Programming Language.
//!
//! This crate generates code in various target languages from an AST.

pub mod formatter;
pub mod go;
pub mod python;
pub mod typescript;

pub use formatter::{FormatConfig, Formatter};

use miette::Diagnostic;
use sag_parser::Program;
use thiserror::Error;

/// Code generation error.
#[derive(Error, Diagnostic, Debug, Clone)]
#[error("code generation error: {message}")]
#[diagnostic(code(sag::codegen::error))]
pub struct CodegenError {
    message: String,
}

impl CodegenError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

/// Compilation target.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    TypeScript,
    Python,
    Go,
}

impl std::str::FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "typescript" | "ts" => Ok(Target::TypeScript),
            "python" | "py" => Ok(Target::Python),
            "go" | "golang" => Ok(Target::Go),
            _ => Err(format!("unknown target: {s}")),
        }
    }
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Target::TypeScript => write!(f, "typescript"),
            Target::Python => write!(f, "python"),
            Target::Go => write!(f, "go"),
        }
    }
}

/// Code generator trait.
pub trait CodeGenerator {
    /// Generate code for the given program.
    fn generate(&self, program: &Program) -> Result<String, CodegenError>;
}

/// Generate code for a program.
pub fn generate(program: &Program, target: Target) -> Result<String, CodegenError> {
    match target {
        Target::TypeScript => {
            let generator = typescript::TypeScriptGenerator::new();
            generator.generate(program)
        }
        Target::Python => {
            let generator = python::PythonGenerator::new();
            generator.generate(program)
        }
        Target::Go => {
            let generator = go::GoGenerator::new();
            generator.generate(program)
        }
    }
}
