# SageSyn Language Compiler - Claude Code Instructions

## Project Overview

This is the Rust-based compiler for the SageSyn Agent Programming Language (`.ssag`). It compiles declarative agent definitions to TypeScript, Python, or Go.

## Tech Stack

| Component | Technology |
|-----------|------------|
| Language | Rust 2021 Edition |
| Lexer | logos |
| Error Reporting | miette |
| CLI | clap |
| Serialization | serde |

## Commands

```bash
# Build
cargo build

# Test
cargo test

# Lint
cargo clippy

# Format
cargo fmt

# Run CLI
cargo run --bin sagesyn -- compile examples/basic-agent.ssag
cargo run --bin sagesyn -- check examples/basic-agent.ssag
cargo run --bin sagesyn -- parse examples/basic-agent.ssag
cargo run --bin sagesyn -- lex examples/basic-agent.ssag
```

## Crate Structure

```
crates/
├── ssag-lexer/     # Token definitions and lexer (logos-based)
├── ssag-parser/    # AST types and recursive descent parser
├── ssag-types/     # Type system and type checker
├── ssag-codegen/   # Code generators (TypeScript, Python, Go)
└── ssag-cli/       # CLI binary (clap-based)
```

## Code Style

### Rust

- Use `thiserror` for error definitions
- Use `miette` for diagnostic reporting
- Prefer iterators over loops
- Use `#[derive(...)]` liberally
- Document public APIs with `///` comments

### Naming

- Crates: `ssag-*` (kebab-case)
- Modules: `snake_case`
- Types: `PascalCase`
- Functions: `snake_case`
- Constants: `SCREAMING_SNAKE_CASE`

### Error Handling

- Return `Result<T, E>` where `E` implements `Diagnostic`
- Use `?` for propagation
- Provide helpful error messages with source spans

## Testing

- Unit tests in same file as implementation
- Integration tests in `tests/` directory
- Use `pretty_assertions` for better diffs
- Test error cases as well as success cases

## Git Workflow

- Branch naming: `feat/feature-name`, `fix/bug-name`
- Conventional commits: `feat:`, `fix:`, `docs:`, `refactor:`, `test:`
- Run `cargo fmt` and `cargo clippy` before committing

## Adding New Features

1. Start with lexer tokens if needed (`ssag-lexer/src/token.rs`)
2. Add AST nodes (`ssag-parser/src/ast.rs`)
3. Update parser (`ssag-parser/src/lib.rs`)
4. Add type checking (`ssag-types/src/lib.rs`)
5. Update code generators (`ssag-codegen/src/*.rs`)
6. Add tests at each level
7. Update CLI if new commands needed

## Key Files

- `Cargo.toml` - Workspace manifest
- `crates/ssag-lexer/src/token.rs` - Token definitions
- `crates/ssag-parser/src/ast.rs` - AST node types
- `crates/ssag-parser/src/lib.rs` - Parser implementation
- `crates/ssag-types/src/lib.rs` - Type system
- `crates/ssag-codegen/src/typescript.rs` - TypeScript generator
- `crates/ssag-cli/src/main.rs` - CLI entry point
