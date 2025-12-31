//! Sage Agent Language Server
//!
//! LSP implementation for the Sage Agent Programming Language.

use dashmap::DashMap;
use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::info;

mod semantic_tokens;
mod symbols;

use semantic_tokens::{SemanticTokensBuilder, LEGEND};
use symbols::SymbolTable;

/// Document state stored for each open file.
struct Document {
    /// The document content as a rope for efficient editing.
    content: Rope,
    /// The document version.
    version: i32,
    /// Parsed program (if successful).
    program: Option<sag_parser::Program>,
    /// Symbol table for the document.
    symbols: Option<SymbolTable>,
}

/// The Sage Language Server.
struct SagLanguageServer {
    /// The LSP client for sending notifications.
    client: Client,
    /// Open documents indexed by URI.
    documents: DashMap<Url, Document>,
}

impl SagLanguageServer {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
    }

    /// Validate a document and publish diagnostics.
    async fn validate_document(&self, uri: &Url) {
        let Some(mut doc) = self.documents.get_mut(uri) else {
            return;
        };

        let source = doc.content.to_string();
        let mut diagnostics = Vec::new();

        // Parse the document
        match sag_parser::Parser::parse(&source) {
            Ok(program) => {
                // Build symbol table
                let symbols = SymbolTable::from_program(&program);
                doc.symbols = Some(symbols);
                doc.program = Some(program.clone());

                // Type check
                if let Err(errors) = sag_types::TypeChecker::check(&source, &program) {
                    for error in errors {
                        let range = span_to_range(&source, error.span());
                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::ERROR),
                            code: Some(NumberOrString::String("sag::type".to_string())),
                            source: Some("sag".to_string()),
                            message: error.message().to_string(),
                            ..Default::default()
                        });
                    }
                }
            }
            Err(error) => {
                doc.program = None;
                doc.symbols = None;
                let range = span_to_range(&source, error.span());
                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("sag::parse".to_string())),
                    source: Some("sag".to_string()),
                    message: error.message().to_string(),
                    ..Default::default()
                });
            }
        }

        drop(doc); // Release lock before async call

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }

    /// Get completions at a position.
    fn get_completions(&self, uri: &Url, position: Position) -> Vec<CompletionItem> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };

        let source = doc.content.to_string();
        let offset = position_to_offset(&source, position);

        // Get the text before cursor to determine context
        let prefix = &source[..offset.min(source.len())];
        let last_word = prefix
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect::<String>()
            .chars()
            .rev()
            .collect::<String>();

        let mut completions = Vec::new();

        // Add user-defined symbols from the document
        if let Some(symbols) = &doc.symbols {
            for (name, defs) in &symbols.definitions {
                if name.starts_with(&last_word) || last_word.is_empty() {
                    if let Some(def) = defs.first() {
                        let kind = match def.kind {
                            symbols::SymbolKind::Agent => CompletionItemKind::CLASS,
                            symbols::SymbolKind::Tool => CompletionItemKind::METHOD,
                            symbols::SymbolKind::Function => CompletionItemKind::FUNCTION,
                            symbols::SymbolKind::Type => CompletionItemKind::STRUCT,
                            symbols::SymbolKind::Field | symbols::SymbolKind::StateField => {
                                CompletionItemKind::FIELD
                            }
                            symbols::SymbolKind::Parameter | symbols::SymbolKind::Variable => {
                                CompletionItemKind::VARIABLE
                            }
                            symbols::SymbolKind::Skill => CompletionItemKind::MODULE,
                            symbols::SymbolKind::EventHandler => CompletionItemKind::EVENT,
                        };
                        completions.push(CompletionItem {
                            label: name.clone(),
                            kind: Some(kind),
                            detail: def.type_info.clone(),
                            documentation: def.doc.as_ref().map(|d| {
                                Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: d.clone(),
                                })
                            }),
                            ..Default::default()
                        });
                    }
                }
            }
        }

        // Keywords
        let keywords = [
            ("agent", "Define an agent", CompletionItemKind::KEYWORD),
            ("tool", "Define a tool", CompletionItemKind::KEYWORD),
            ("fn", "Define a function", CompletionItemKind::KEYWORD),
            ("type", "Define a type", CompletionItemKind::KEYWORD),
            ("skill", "Define a skill", CompletionItemKind::KEYWORD),
            ("on", "Event handler", CompletionItemKind::KEYWORD),
            ("emit", "Emit an event", CompletionItemKind::KEYWORD),
            ("let", "Variable binding", CompletionItemKind::KEYWORD),
            ("var", "Mutable variable", CompletionItemKind::KEYWORD),
            ("const", "Constant binding", CompletionItemKind::KEYWORD),
            ("if", "Conditional", CompletionItemKind::KEYWORD),
            ("else", "Else branch", CompletionItemKind::KEYWORD),
            ("for", "For loop", CompletionItemKind::KEYWORD),
            ("while", "While loop", CompletionItemKind::KEYWORD),
            ("match", "Pattern matching", CompletionItemKind::KEYWORD),
            ("return", "Return statement", CompletionItemKind::KEYWORD),
            ("await", "Await expression", CompletionItemKind::KEYWORD),
            ("async", "Async function", CompletionItemKind::KEYWORD),
            ("try", "Try block", CompletionItemKind::KEYWORD),
            ("catch", "Catch block", CompletionItemKind::KEYWORD),
            ("finally", "Finally block", CompletionItemKind::KEYWORD),
            ("throw", "Throw expression", CompletionItemKind::KEYWORD),
            ("model", "Model configuration", CompletionItemKind::KEYWORD),
            ("state", "Agent state", CompletionItemKind::KEYWORD),
            (
                "protocols",
                "Protocol bindings",
                CompletionItemKind::KEYWORD,
            ),
            (
                "description",
                "Description field",
                CompletionItemKind::PROPERTY,
            ),
            ("version", "Version field", CompletionItemKind::PROPERTY),
        ];

        for (name, detail, kind) in keywords {
            if name.starts_with(&last_word) || last_word.is_empty() {
                completions.push(CompletionItem {
                    label: name.to_string(),
                    kind: Some(kind),
                    detail: Some(detail.to_string()),
                    ..Default::default()
                });
            }
        }

        // Types
        let types = [
            ("string", "String type"),
            ("number", "Number type"),
            ("boolean", "Boolean type"),
            ("timestamp", "Timestamp type"),
            ("void", "Void type"),
            ("array", "Array type"),
            ("record", "Record/map type"),
            ("optional", "Optional type"),
            ("tuple", "Tuple type"),
        ];

        for (name, detail) in types {
            if name.starts_with(&last_word) || last_word.is_empty() {
                completions.push(CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::TYPE_PARAMETER),
                    detail: Some(detail.to_string()),
                    ..Default::default()
                });
            }
        }

        // Snippets for common patterns
        if last_word.is_empty() || "agent".starts_with(&last_word) {
            completions.push(CompletionItem {
                label: "agent (snippet)".to_string(),
                kind: Some(CompletionItemKind::SNIPPET),
                insert_text: Some(
                    r#"agent ${1:AgentName} {
  description: "${2:Agent description}"
  version: "1.0.0"

  tool ${3:tool_name}(${4:params}) -> ${5:ReturnType} {
    description: "${6:Tool description}"
    ${0}
  }

  on user_message {

  }
}"#
                    .to_string(),
                ),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                detail: Some("Agent template".to_string()),
                ..Default::default()
            });
        }

        if last_word.is_empty() || "tool".starts_with(&last_word) {
            completions.push(CompletionItem {
                label: "tool (snippet)".to_string(),
                kind: Some(CompletionItemKind::SNIPPET),
                insert_text: Some(
                    r#"tool ${1:name}(${2:params}) -> ${3:ReturnType} {
  description: "${4:Tool description}"
  ${0}
}"#
                    .to_string(),
                ),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                detail: Some("Tool template".to_string()),
                ..Default::default()
            });
        }

        if last_word.is_empty() || "type".starts_with(&last_word) {
            completions.push(CompletionItem {
                label: "type (snippet)".to_string(),
                kind: Some(CompletionItemKind::SNIPPET),
                insert_text: Some(
                    r#"type ${1:TypeName} {
  ${2:field}: ${3:type}
  ${0}
}"#
                    .to_string(),
                ),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                detail: Some("Type definition template".to_string()),
                ..Default::default()
            });
        }

        if last_word.is_empty() || "try".starts_with(&last_word) {
            completions.push(CompletionItem {
                label: "try-catch (snippet)".to_string(),
                kind: Some(CompletionItemKind::SNIPPET),
                insert_text: Some(
                    r#"try {
  ${1}
} catch (${2:error}) {
  ${0}
}"#
                    .to_string(),
                ),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                detail: Some("Try-catch template".to_string()),
                ..Default::default()
            });
        }

        completions
    }

    /// Get hover information at a position.
    fn get_hover(&self, uri: &Url, position: Position) -> Option<Hover> {
        let doc = self.documents.get(uri)?;
        let source = doc.content.to_string();
        let offset = position_to_offset(&source, position);

        // Try to get hover from symbol table
        if let Some(symbols) = &doc.symbols {
            if let Some(info) = symbols.get_hover_info(offset) {
                // Find the word range
                let start = source[..offset]
                    .chars()
                    .rev()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .count();
                let end = source[offset..]
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .count();

                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: info,
                    }),
                    range: Some(Range {
                        start: offset_to_position(&source, offset - start),
                        end: offset_to_position(&source, offset + end),
                    }),
                });
            }
        }

        // Fall back to built-in keyword hover
        let start = source[..offset]
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .count();
        let end = source[offset..]
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .count();

        let word_start = offset - start;
        let word_end = offset + end;
        let word = &source[word_start..word_end];

        let info = match word {
            "agent" => Some("**agent** - Define an AI agent with tools, state, and event handlers."),
            "tool" => Some("**tool** - Define a tool that the agent can use. Can have inline implementation or MCP binding."),
            "fn" => Some("**fn** - Define a standalone function."),
            "skill" => Some("**skill** - Define a reusable skill that can be shared across agents."),
            "type" => Some("**type** - Define a custom type (struct or alias)."),
            "on" => Some("**on** - Define an event handler. Events: `user_message`, `agent_start`, `agent_stop`, etc."),
            "emit" => Some("**emit** - Emit an event with a value."),
            "state" => Some("**state** - Define agent state that persists across interactions."),
            "model" => Some("**model** - Configure the LLM model for the agent."),
            "protocols" => Some("**protocols** - Configure protocol bindings (MCP, A2A, AG-UI)."),
            "try" => Some("**try** - Begin a try block for error handling."),
            "catch" => Some("**catch** - Handle errors from a try block."),
            "finally" => Some("**finally** - Code that always runs after try/catch."),
            "throw" => Some("**throw** - Throw an error."),
            "match" => Some("**match** - Pattern matching expression."),
            "string" => Some("**string** - Text type."),
            "number" => Some("**number** - Numeric type (64-bit float)."),
            "boolean" => Some("**boolean** - Boolean type (`true` or `false`)."),
            "timestamp" => Some("**timestamp** - Date/time type."),
            "array" => Some("**array<T>** - Array of elements of type T."),
            "record" => Some("**record<K, V>** - Key-value map with key type K and value type V."),
            "optional" => Some("**optional<T>** - Optional value of type T (can be null)."),
            "async" => Some("**async** - Mark a function as asynchronous."),
            "await" => Some("**await** - Wait for an async operation to complete."),
            "mcp_server" => Some("**mcp_server** - The MCP server to connect to for this tool."),
            "mcp_tool" => Some("**mcp_tool** - The tool name on the MCP server."),
            _ => None,
        };

        info.map(|content| Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content.to_string(),
            }),
            range: Some(Range {
                start: offset_to_position(&source, word_start),
                end: offset_to_position(&source, word_end),
            }),
        })
    }

    /// Compute semantic tokens for syntax highlighting.
    fn compute_semantic_tokens(&self, uri: &Url) -> Option<SemanticTokensResult> {
        let doc = self.documents.get(uri)?;
        let source = doc.content.to_string();

        // Tokenize the document
        let lexer = sag_lexer::Lexer::new(&source);
        let tokens = lexer.tokenize().ok()?;

        let mut builder = SemanticTokensBuilder::new();

        for spanned in &tokens {
            let token_type = semantic_tokens::token_to_semantic_type(&spanned.token);
            if let Some(token_type) = token_type {
                let start = offset_to_position(&source, spanned.span.start);
                let length = spanned.span.end - spanned.span.start;
                builder.push(start.line, start.character, length as u32, token_type, 0);
            }
        }

        Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: builder.build(),
        }))
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for SagLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        info!("Sage LSP server initializing");

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: LEGEND.clone(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: Some(false),
                            ..Default::default()
                        },
                    ),
                ),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "sag-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("Sage LSP server initialized");
        self.client
            .log_message(MessageType::INFO, "Sage Language Server ready")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        info!("Sage LSP server shutting down");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        let version = params.text_document.version;

        info!("Document opened: {}", uri);

        self.documents.insert(
            uri.clone(),
            Document {
                content: Rope::from_str(&content),
                version,
                program: None,
                symbols: None,
            },
        );

        self.validate_document(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;

        if let Some(mut doc) = self.documents.get_mut(&uri) {
            // Full sync - replace entire content
            if let Some(change) = params.content_changes.into_iter().next() {
                doc.content = Rope::from_str(&change.text);
                doc.version = params.text_document.version;
            }
        }

        self.validate_document(&uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        info!("Document closed: {}", uri);
        self.documents.remove(&uri);

        // Clear diagnostics
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        info!("Document saved: {}", params.text_document.uri);
        self.validate_document(&params.text_document.uri).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let completions = self.get_completions(&uri, position);

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        Ok(self.get_hover(&uri, position))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let source = doc.content.to_string();
        let offset = position_to_offset(&source, position);

        // Use symbol table for better definition lookup
        if let Some(symbols) = &doc.symbols {
            if let Some(def) = symbols.find_definition_at(offset) {
                let range = span_to_range(&source, def.span);
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range,
                })));
            }
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let source = doc.content.to_string();
        let offset = position_to_offset(&source, position);

        if let Some(symbols) = &doc.symbols {
            let refs = symbols.find_references_at(offset);
            if !refs.is_empty() {
                let locations: Vec<Location> = refs
                    .iter()
                    .map(|span| Location {
                        uri: uri.clone(),
                        range: span_to_range(&source, *span),
                    })
                    .collect();
                return Ok(Some(locations));
            }
        }

        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let source = doc.content.to_string();

        if let Some(program) = &doc.program {
            let symbols = symbols::get_document_symbols(program, &source);
            return Ok(Some(DocumentSymbolResponse::Nested(symbols)));
        }

        Ok(None)
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;

        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let source = doc.content.to_string();
        let offset = position_to_offset(&source, position);

        if let Some(symbols) = &doc.symbols {
            if let Some(def) = symbols.find_definition_at(offset) {
                let range = span_to_range(&source, def.span);
                return Ok(Some(PrepareRenameResponse::Range(range)));
            }
        }

        Ok(None)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let source = doc.content.to_string();
        let offset = position_to_offset(&source, position);

        if let Some(symbols) = &doc.symbols {
            let refs = symbols.find_references_at(offset);
            if !refs.is_empty() {
                let edits: Vec<TextEdit> = refs
                    .iter()
                    .map(|span| TextEdit {
                        range: span_to_range(&source, *span),
                        new_text: new_name.clone(),
                    })
                    .collect();

                let mut changes = std::collections::HashMap::new();
                changes.insert(uri, edits);

                return Ok(Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }));
            }
        }

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        Ok(self.compute_semantic_tokens(&uri))
    }
}

/// Convert a span to an LSP range.
fn span_to_range(source: &str, span: sag_lexer::Span) -> Range {
    Range {
        start: offset_to_position(source, span.start),
        end: offset_to_position(source, span.end),
    }
}

/// Convert an offset to an LSP position.
fn offset_to_position(source: &str, offset: usize) -> Position {
    let offset = offset.min(source.len());
    let before = &source[..offset];
    let line = before.chars().filter(|&c| c == '\n').count() as u32;
    let line_start = before.rfind('\n').map(|i| i + 1).unwrap_or(0);
    let character = (offset - line_start) as u32;
    Position { line, character }
}

/// Convert an LSP position to an offset.
fn position_to_offset(source: &str, position: Position) -> usize {
    let mut offset = 0;
    for (i, line) in source.lines().enumerate() {
        if i == position.line as usize {
            return offset + (position.character as usize).min(line.len());
        }
        offset += line.len() + 1; // +1 for newline
    }
    source.len()
}

#[tokio::main]
async fn main() {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive("sag_lsp=info".parse().unwrap()),
        )
        .with_writer(std::io::stderr)
        .init();

    info!("Starting Sage Language Server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(SagLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
