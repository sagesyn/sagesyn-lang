# Sage Agent Language - VS Code Extension

Language support for the Sage Agent Programming Language (`.sag` files).

## Features

**Everything works out of the box - no additional installation required!**

- **Syntax Highlighting** - Full TextMate grammar for `.sag` files
- **Semantic Highlighting** - Rich token-based highlighting
- **Diagnostics** - Real-time error checking as you type
- **Hover Info** - Type information and documentation on hover
- **Auto-completion** - Keywords, types, snippets, and user-defined symbols
- **Go to Definition** - Navigate to symbol definitions (F12)
- **Find All References** - Find all usages of a symbol (Shift+F12)
- **Rename Symbol** - Rename across all references (F2)
- **Document Outline** - Navigate agents, tools, types in the outline view

The extension includes bundled Language Server binaries for:
- Linux (x64, ARM64)
- macOS (Intel, Apple Silicon)
- Windows (x64)

## Installation

### From VS Code Marketplace

1. Open VS Code
2. Go to Extensions (`Ctrl+Shift+X` / `Cmd+Shift+X`)
3. Search for "Sage Agent Language"
4. Click Install
5. Open a `.sag` file - everything works automatically!

### From VSIX File

```bash
code --install-extension sage-agent-language-0.4.0.vsix
```

## Configuration

| Setting | Description | Default |
|---------|-------------|---------|
| `sag.server.path` | Custom path to sag-lsp binary (optional) | (uses bundled) |
| `sag.trace.server` | Trace LSP communication | `off` |

## Example

```sag
agent WeatherAgent {
  description: "An agent that provides weather information"
  version: "1.0.0"

  model {
    provider: "anthropic"
    name: "claude-sonnet-4-20250514"
  }

  state {
    lastCity?: string
  }

  tool get_weather(city: string) -> WeatherData {
    description: "Get current weather for a city"
    let data = await http.get(`https://api.weather.com/${city}`)
    return data
  }

  on user_message {
    let weather = get_weather("San Francisco")
    emit response(weather)
  }
}

type WeatherData {
  temperature: number
  humidity: number
  description: string
}
```

## Supported Language Features

- **Agents** - `agent`, `tool`, `state`, `model`, `protocols`, `on`
- **Control Flow** - `if`, `else`, `for`, `while`, `match`, `return`
- **Error Handling** - `try`, `catch`, `finally`, `throw`
- **Functions** - `fn`, `async`, `await`
- **Types** - `type`, `string`, `number`, `boolean`, `array`, `record`, `optional`, `tuple`
- **Operators** - Arithmetic, comparison, logical, optional chaining (`?.`), null coalescing (`??`)
- **Patterns** - Destructuring, match expressions with guards
- **Strings** - Double-quoted and template literals with `${interpolation}`
- **Comments** - Line (`//`) and block (`/* */`)

---

## Status Bar Indicator

The status bar shows the Language Server state:
- `✓ Sage LSP` - Full LSP support is active
- `⚠ Sage (syntax only)` - LSP not available (syntax highlighting still works)

---

## Development

### Testing the Extension Locally

#### Prerequisites

- [Node.js](https://nodejs.org/) (v18 or later)
- [VS Code](https://code.visualstudio.com/)
- Rust toolchain (for building the LSP server)

#### Step 1: Build the LSP Server

From the repository root:

```bash
cargo build --release --bin sag-lsp
```

#### Step 2: Install Extension Dependencies

```bash
cd editors/vscode
npm install
```

#### Step 3: Compile the Extension

```bash
npm run compile
```

#### Step 4: Launch Extension in Debug Mode

1. Open the `editors/vscode` folder in VS Code
2. Press `F5` to launch the Extension Development Host
3. Open a `.sag` file to test

### Packaging

```bash
npm run package
```

This creates `sage-agent-language-x.x.x.vsix`.

---

## Links

- [GitHub Repository](https://github.com/sagesyn/sagesyn-lang)
- [Language Documentation](https://sagesyn.ai/docs)
- [Report Issues](https://github.com/sagesyn/sagesyn-lang/issues)

## License

MIT
