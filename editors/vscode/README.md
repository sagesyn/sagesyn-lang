# Sage Agent Language - VS Code Extension

Language support for the Sage Agent Programming Language (`.sag` files).

## Features

### Works Out of the Box
- **Syntax Highlighting** - Full TextMate grammar for `.sag` files (no additional installation required)

### With Language Server (optional)
- **Diagnostics** - Real-time error checking as you type
- **Hover Info** - Type information and documentation on hover
- **Auto-completion** - Keyword and context-aware suggestions
- **Go to Definition** - Navigate to tool and type definitions

> **Note:** The extension works without the language server! You get syntax highlighting immediately. Install `sag-lsp` for advanced features.

## Installation

### From VS Code Marketplace

1. Open VS Code
2. Go to Extensions (`Ctrl+Shift+X` / `Cmd+Shift+X`)
3. Search for "Sage Agent Language"
4. Click Install

### From VSIX File

```bash
code --install-extension sage-agent-language-0.1.1.vsix
```

## Requirements

**Syntax highlighting works immediately** - no additional installation required!

For advanced LSP features (diagnostics, completion, hover), install the `sag-lsp` binary:

### Option 1: Download Pre-built Binary

Download from [GitHub Releases](https://github.com/sagesyn/sagesyn-lang/releases) and add to your PATH.

### Option 2: Build from Source

```bash
git clone https://github.com/sagesyn/sagesyn-lang.git
cd sagesyn-lang
cargo build --release --bin sag-lsp
```

Then configure the extension:
- Open Settings (`Cmd+,` / `Ctrl+,`)
- Search for `sag.server.path`
- Set to the path of your `sag-lsp` binary

## Configuration

| Setting | Description | Default |
|---------|-------------|---------|
| `sag.server.path` | Path to sag-lsp binary | (searches PATH) |
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
- **Functions** - `fn`, `async`, `await`
- **Types** - `type`, `string`, `number`, `boolean`, `array`, `record`, `optional`, `tuple`
- **Operators** - Arithmetic, comparison, logical, assignment
- **Strings** - Double-quoted and template literals with `${interpolation}`
- **Comments** - Line (`//`) and block (`/* */`)

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

The binary will be at `target/release/sag-lsp`.

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
3. Configure `sag.server.path` in settings to point to the LSP binary
4. Open a `.sag` file to test

### Packaging

```bash
npm run package
```

This creates `sage-agent-language-x.x.x.vsix`.

---

## Troubleshooting

### "Sage Language Server (sag-lsp) not found" warning

This is normal if you haven't installed the LSP server yet. Syntax highlighting still works! To enable advanced features:
1. Install `sag-lsp` (see Requirements section above)
2. Ensure it's in your PATH, or set `sag.server.path` in settings

### Status Bar Indicator

The status bar shows the current state:
- `✓ Sage LSP` - Full LSP support is active
- `⚠ Sage (syntax only)` - LSP not available, syntax highlighting only

### Extension not activating

- Check the Output panel (`View > Output`) and select "Sage Agent Language Server"
- Ensure the file has `.sag` extension

### No syntax highlighting

- TextMate grammar provides highlighting without the LSP server
- Full semantic highlighting requires the LSP server to be running

---

## Links

- [GitHub Repository](https://github.com/sagesyn/sagesyn-lang)
- [Language Documentation](https://sagesyn.ai/docs)
- [Report Issues](https://github.com/sagesyn/sagesyn-lang/issues)

## License

MIT
