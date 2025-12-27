# Sage Agent Language Compiler - Claude Code Instructions

## Project Overview

This is the Rust-based compiler for the Sage Agent Programming Language (`.sag`). It compiles declarative agent definitions to TypeScript, Python, or Go.

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
cargo run --bin sag -- compile examples/basic-agent.sag
cargo run --bin sag -- check examples/basic-agent.sag
cargo run --bin sag -- parse examples/basic-agent.sag
cargo run --bin sag -- lex examples/basic-agent.sag
```

## Crate Structure

```
crates/
├── sag-lexer/     # Token definitions and lexer (logos-based)
├── sag-parser/    # AST types and recursive descent parser
├── sag-types/     # Type system and type checker
├── sag-codegen/   # Code generators (TypeScript, Python, Go)
└── sag-cli/       # CLI binary (clap-based)
```

## Code Style

### Rust

- Use `thiserror` for error definitions
- Use `miette` for diagnostic reporting
- Prefer iterators over loops
- Use `#[derive(...)]` liberally
- Document public APIs with `///` comments

### Naming

- Crates: `sag-*` (kebab-case)
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

1. Start with lexer tokens if needed (`sag-lexer/src/token.rs`)
2. Add AST nodes (`sag-parser/src/ast.rs`)
3. Update parser (`sag-parser/src/lib.rs`)
4. Add type checking (`sag-types/src/lib.rs`)
5. Update code generators (`sag-codegen/src/*.rs`)
6. Add tests at each level
7. Update CLI if new commands needed

## Key Files

- `Cargo.toml` - Workspace manifest
- `crates/sag-lexer/src/token.rs` - Token definitions
- `crates/sag-parser/src/ast.rs` - AST node types
- `crates/sag-parser/src/lib.rs` - Parser implementation
- `crates/sag-types/src/lib.rs` - Type system
- `crates/sag-codegen/src/typescript.rs` - TypeScript generator
- `crates/sag-cli/src/main.rs` - CLI entry point
