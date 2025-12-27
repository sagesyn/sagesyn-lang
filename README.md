# Sage Agent Language Compiler

The official compiler for the Sage Agent Programming Language (`.sag`).

## Overview

Sage Agent is a declarative, event-driven, statically-typed domain-specific language for defining AI agents. This compiler transforms `.sag` files into TypeScript, Python, or Go code.

## Installation

### From Source

```bash
git clone https://github.com/sagesyn/sagesyn-lang.git
cd sagesyn-lang
cargo install --path crates/sag-cli
```

### Pre-built Binaries

Download from [Releases](https://github.com/sagesyn/sagesyn-lang/releases).

## Usage

### Compile

```bash
# Compile to TypeScript (default)
sag compile agent.sag

# Compile to specific target
sag compile agent.sag --target typescript
sag compile agent.sag --target python
sag compile agent.sag --target go

# Specify output file
sag compile agent.sag --output dist/agent.ts
```

### Check

```bash
# Type check without generating code
sag check agent.sag
```

### Parse

```bash
# Print AST
sag parse agent.sag

# Print tokens
sag lex agent.sag
```

## Language Overview

```sag
agent WeatherAgent {
  description: "Provides weather information"
  version: "1.0.0"

  model:
    provider: anthropic
    name: claude-sonnet-4
    temperature: 0.7

  state {
    last_query: optional<string>
    cache: record<string, WeatherData>
  }

  tool get_weather(city: string) -> WeatherData {
    description: "Get current weather for a city"
    mcp_server: weather_api
    mcp_tool: current_weather
  }

  on user_message {
    let weather = get_weather(message.city)
    emit response(format_weather(weather))
  }
}

type WeatherData {
  city: string
  temp: number
  conditions: string
}
```

## Project Structure

```
sagesyn-lang/
├── crates/
│   ├── sag-lexer/     # Tokenizer (logos)
│   ├── sag-parser/    # Parser & AST
│   ├── sag-types/     # Type checker
│   ├── sag-codegen/   # Code generation
│   └── sag-cli/       # CLI binary
├── examples/           # Example .sag files
└── tests/              # Integration tests
```

## Development

### Prerequisites

- Rust 1.75+
- Cargo

### Build

```bash
cargo build
```

### Test

```bash
cargo test
```

### Lint

```bash
cargo clippy
cargo fmt --check
```

## Language Features

- **Agents**: First-class agent definitions with state, tools, and event handlers
- **Types**: Static typing with primitives, arrays, records, tuples, optionals, and unions
- **Tools**: Define tools with MCP server bindings or inline implementations
- **Events**: Event-driven architecture with emit/on patterns
- **Protocols**: Native support for MCP, A2A, and AG-UI protocols
- **Async**: First-class async/await support

## Roadmap

See [ROADMAP.md](https://github.com/sagesyn/sagesyn-team/blob/main/data/roadmap.json) for the development plan.

### Phase 1 (Q1 2026): Language Foundation
- Language Specification
- Parser & AST
- TypeScript Compiler
- LSP Core

### Phase 2 (Q2 2026): Multi-Model
- Python Compiler
- Go Compiler
- Visual IDE

### Phase 3 (Q3 2026): Advanced
- MCP Protocol Support
- A2A Protocol Support
- AG-UI Protocol Support

### Phase 4 (Q4 2026): Enterprise
- Package Registry
- Cloud Compilation
- Enterprise Features

## License

Apache 2.0 - See [LICENSE](LICENSE)

## Contributing

Contributions welcome! Please read our contributing guidelines first.

## Links

- [Website](https://sagesyn.ai)
- [Documentation](https://docs.sagesyn.ai)
- [Team Plugin](https://github.com/sagesyn/sagesyn-team)
