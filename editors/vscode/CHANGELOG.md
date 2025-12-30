# Changelog

All notable changes to the Sage Agent Language extension will be documented in this file.

## [0.1.1] - 2024-12-30

### Added
- Match expression support in syntax highlighting
- Improved keyword recognition for `match`, `emit`, `skill`
- Better template literal highlighting with interpolation

### Fixed
- Fixed syntax highlighting for nested expressions
- Improved bracket matching in complex expressions

## [0.1.0] - 2024-12-29

### Added
- Initial release of Sage Agent Language support
- Syntax highlighting for `.sag` files
- Language Server Protocol (LSP) integration
- Support for all language keywords:
  - Agent definitions (`agent`, `tool`, `state`, `model`, `protocols`)
  - Control flow (`if`, `else`, `for`, `while`, `match`, `return`)
  - Functions (`fn`, `async`, `await`)
  - Types (`type`, `string`, `number`, `boolean`, `array`, `record`, `optional`)
- Template literal support with `${expression}` interpolation
- Comment support (line `//` and block `/* */`)
- Auto-closing pairs for brackets, quotes, and template literals
- Configurable LSP server path
- Trace logging for debugging

### Configuration
- `sag.server.path`: Path to sag-lsp binary
- `sag.trace.server`: Trace level (off/messages/verbose)
