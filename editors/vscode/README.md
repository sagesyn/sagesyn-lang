# Sage Agent Language - VS Code Extension

## Testing the Extension Locally

### Prerequisites

- [Node.js](https://nodejs.org/) (v18 or later)
- [VS Code](https://code.visualstudio.com/)
- Rust toolchain (for building the LSP server)

### Step 1: Build the LSP Server

From the repository root:

```bash
cargo build --release --bin sag-lsp
```

The binary will be at `target/release/sag-lsp`.

### Step 2: Install Extension Dependencies

```bash
cd editors/vscode
npm install
```

### Step 3: Compile the Extension

```bash
npm run compile
```

If you don't have a compile script, run:

```bash
npx tsc -p .
```

### Step 4: Launch Extension in Debug Mode

1. Open the `editors/vscode` folder in VS Code:

   ```bash
   code editors/vscode
   ```

2. Press `F5` to launch the Extension Development Host

3. In the new VS Code window, configure the LSP server path:
   - Open Settings (`Cmd+,` or `Ctrl+,`)
   - Search for "sag.server.path"
   - Set it to the absolute path of `target/release/sag-lsp` in your clone

4. Open a `.sag` file to test:

   ```bash
   # Create a test file
   cat > ~/test.sag << 'EOF'
   agent HelloAgent {
     description: "A simple hello world agent"

     model {
       provider: google
       name: gemini-2.0-flash
     }

     tool greet(name: string) -> string {
       description: "Greet someone"
       return `Hello, ${name}!`
     }
   }
   EOF
   ```

### Step 5: Test Features

Once you have a `.sag` file open, test these features:

| Feature             | How to Test                                                    |
| ------------------- | -------------------------------------------------------------- |
| Syntax Highlighting | Colors should appear for keywords, strings, etc.               |
| Diagnostics         | Introduce an error (e.g., `foo: "bar"`) and see red squiggles  |
| Hover               | Hover over `agent`, `tool`, `state` keywords                   |
| Completions         | Type inside an agent body and press `Ctrl+Space`               |
| Go to Definition    | `Ctrl+Click` on a tool name                                    |

### Packaging the Extension (Optional)

To create a `.vsix` file for distribution:

```bash
npm install -g @vscode/vsce
vsce package
```

This creates `sage-agent-language-x.x.x.vsix` which can be installed via:

```bash
code --install-extension sage-agent-language-x.x.x.vsix
```

---

## Troubleshooting

### Extension not activating

- Check the Output panel (`View > Output`) and select "Sage Agent Language" from the dropdown
- Ensure the file has `.sag` extension

### LSP server not starting

- Verify the server path in settings is correct
- Check that the binary exists and is executable:

  ```bash
  ls -la target/release/sag-lsp
  ```

### No syntax highlighting

- The TextMate grammar (`syntaxes/sag.tmLanguage.json`) provides basic highlighting
- Full semantic highlighting requires the LSP server to be running

### Rebuild after changes

If you modify the extension code:

```bash
npx tsc -p .
```

Then press `Ctrl+Shift+F5` in the Extension Development Host to reload.
