//! Sage Agent Programming Language CLI.

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, Result};
use std::fs;
use std::path::PathBuf;

use sag_codegen::{generate, Target};
use sag_parser::Parser as SagParser;
use sag_types::TypeChecker;

#[derive(Parser)]
#[command(name = "sag")]
#[command(author = "SageSyn <team@sagesyn.ai>")]
#[command(version)]
#[command(about = "Sage Agent Programming Language compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile a .sag file to a target language
    Compile {
        /// Input file path
        #[arg(value_name = "FILE")]
        file: PathBuf,

        /// Target language (typescript, python, go)
        #[arg(short, long, default_value = "typescript")]
        target: String,

        /// Output file path (defaults to input file with new extension)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Check a .sag file for errors without generating code
    Check {
        /// Input file path
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },

    /// Format a .sag file
    Fmt {
        /// Input file path
        #[arg(value_name = "FILE")]
        file: PathBuf,

        /// Write formatted output back to file
        #[arg(short, long)]
        write: bool,
    },

    /// Parse a .sag file and print the AST
    Parse {
        /// Input file path
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },

    /// Tokenize a .sag file and print tokens
    Lex {
        /// Input file path
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile { file, target, output } => {
            compile(&file, &target, output.as_ref())?;
        }
        Commands::Check { file } => {
            check(&file)?;
        }
        Commands::Fmt { file, write } => {
            format_file(&file, write)?;
        }
        Commands::Parse { file } => {
            parse(&file)?;
        }
        Commands::Lex { file } => {
            lex(&file)?;
        }
    }

    Ok(())
}

fn compile(file: &PathBuf, target_str: &str, output: Option<&PathBuf>) -> Result<()> {
    let source = fs::read_to_string(file).into_diagnostic()?;

    // Parse
    let program = SagParser::parse(&source).map_err(|e| miette::Report::new(e))?;

    // Type check
    if let Err(errors) = TypeChecker::check(&source, &program) {
        for error in errors {
            eprintln!("{:?}", miette::Report::new(error));
        }
        return Err(miette::miette!("Type checking failed"));
    }

    // Parse target
    let target: Target = target_str.parse().map_err(|e: String| miette::miette!("{}", e))?;

    // Generate code
    let code = generate(&program, target).map_err(|e| miette::miette!("{}", e))?;

    // Determine output path
    let output_path = match output {
        Some(p) => p.clone(),
        None => {
            let ext = match target {
                Target::TypeScript => "ts",
                Target::Python => "py",
                Target::Go => "go",
            };
            file.with_extension(ext)
        }
    };

    // Write output
    fs::write(&output_path, code).into_diagnostic()?;
    println!("Compiled {} -> {}", file.display(), output_path.display());

    Ok(())
}

fn check(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file).into_diagnostic()?;

    // Parse
    let program = SagParser::parse(&source).map_err(|e| miette::Report::new(e))?;

    // Type check
    if let Err(errors) = TypeChecker::check(&source, &program) {
        for error in errors {
            eprintln!("{:?}", miette::Report::new(error));
        }
        return Err(miette::miette!("Type checking failed"));
    }

    println!("✓ {} is valid", file.display());
    Ok(())
}

fn format_file(file: &PathBuf, _write: bool) -> Result<()> {
    let source = fs::read_to_string(file).into_diagnostic()?;

    // Parse
    let _program = SagParser::parse(&source).map_err(|e| miette::Report::new(e))?;

    // TODO: Implement formatter
    println!("Formatting not yet implemented");
    println!("{}", source);

    Ok(())
}

fn parse(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file).into_diagnostic()?;

    let program = SagParser::parse(&source).map_err(|e| miette::Report::new(e))?;

    println!("{:#?}", program);

    Ok(())
}

fn lex(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file).into_diagnostic()?;

    let lexer = sag_lexer::Lexer::new(&source);
    let tokens = lexer.tokenize().map_err(|e| miette::miette!("{}", e))?;

    for token in tokens {
        println!("{:?}", token);
    }

    Ok(())
}
