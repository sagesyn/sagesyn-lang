//! Symbol table for the Sage Language Server.
//!
//! Tracks all symbol definitions and references in a document.

use sag_parser::{
    Agent, ArrowBody, Block, BindingPattern, ElseClause, Expr, Function, Item, Param, Program,
    Skill, Stmt, Tool, TypeDef, TypeDefKind, TypeExpr,
};
use sag_lexer::Span;
use std::collections::HashMap;

/// The kind of symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Agent,
    Tool,
    Function,
    Type,
    Field,
    Parameter,
    Variable,
    Skill,
    StateField,
    EventHandler,
}

/// A symbol definition.
#[derive(Debug, Clone)]
pub struct SymbolDef {
    pub name: String,
    pub kind: SymbolKind,
    pub span: Span,
    pub type_info: Option<String>,
    pub doc: Option<String>,
}

/// A symbol reference (usage).
#[derive(Debug, Clone)]
pub struct SymbolRef {
    pub name: String,
    pub span: Span,
    pub definition_span: Option<Span>,
}

/// Symbol table for a document.
#[derive(Debug, Default)]
pub struct SymbolTable {
    /// All symbol definitions indexed by name.
    pub definitions: HashMap<String, Vec<SymbolDef>>,
    /// All symbol references.
    pub references: Vec<SymbolRef>,
    /// Definitions indexed by span for quick lookup.
    def_by_span: HashMap<(usize, usize), SymbolDef>,
}

impl SymbolTable {
    /// Create a new symbol table from a parsed program.
    pub fn from_program(program: &Program) -> Self {
        let mut table = SymbolTable::default();
        table.collect_program(program);
        table
    }

    /// Find a symbol definition at a position.
    pub fn find_definition_at(&self, offset: usize) -> Option<&SymbolDef> {
        // Check if we're on a definition
        for def in self.definitions.values().flatten() {
            if offset >= def.span.start && offset < def.span.end {
                return Some(def);
            }
        }

        // Check if we're on a reference
        for reference in &self.references {
            if offset >= reference.span.start && offset < reference.span.end {
                // Find the definition for this reference
                if let Some(def_span) = &reference.definition_span {
                    return self.def_by_span.get(&(def_span.start, def_span.end));
                }
                // Try to find by name
                if let Some(defs) = self.definitions.get(&reference.name) {
                    return defs.first();
                }
            }
        }

        None
    }

    /// Find all references to a symbol at a position.
    pub fn find_references_at(&self, offset: usize) -> Vec<Span> {
        let mut refs = Vec::new();

        // Find the symbol name at this position
        let symbol_name = self.get_symbol_name_at(offset);
        let Some(name) = symbol_name else {
            return refs;
        };

        // Add the definition
        if let Some(defs) = self.definitions.get(&name) {
            for def in defs {
                refs.push(def.span);
            }
        }

        // Add all references
        for reference in &self.references {
            if reference.name == name {
                refs.push(reference.span);
            }
        }

        refs
    }

    /// Get the symbol name at a position.
    fn get_symbol_name_at(&self, offset: usize) -> Option<String> {
        // Check definitions
        for defs in self.definitions.values() {
            for def in defs {
                if offset >= def.span.start && offset < def.span.end {
                    return Some(def.name.clone());
                }
            }
        }

        // Check references
        for reference in &self.references {
            if offset >= reference.span.start && offset < reference.span.end {
                return Some(reference.name.clone());
            }
        }

        None
    }

    /// Get hover info for a symbol at a position.
    pub fn get_hover_info(&self, offset: usize) -> Option<String> {
        if let Some(def) = self.find_definition_at(offset) {
            return Some(self.format_hover(def));
        }
        None
    }

    fn format_hover(&self, def: &SymbolDef) -> String {
        let kind_str = match def.kind {
            SymbolKind::Agent => "agent",
            SymbolKind::Tool => "tool",
            SymbolKind::Function => "function",
            SymbolKind::Type => "type",
            SymbolKind::Field => "field",
            SymbolKind::Parameter => "parameter",
            SymbolKind::Variable => "variable",
            SymbolKind::Skill => "skill",
            SymbolKind::StateField => "state field",
            SymbolKind::EventHandler => "event handler",
        };

        let mut hover = format!("**{}** `{}`", kind_str, def.name);

        if let Some(type_info) = &def.type_info {
            hover.push_str(&format!(": `{}`", type_info));
        }

        if let Some(doc) = &def.doc {
            hover.push_str(&format!("\n\n{}", doc));
        }

        hover
    }

    // --- Collection methods ---

    fn collect_program(&mut self, program: &Program) {
        for item in &program.items {
            self.collect_item(item);
        }
    }

    fn collect_item(&mut self, item: &Item) {
        match item {
            Item::Agent(agent) => self.collect_agent(agent),
            Item::Skill(skill) => self.collect_skill(skill),
            Item::TypeDef(typedef) => self.collect_typedef(typedef),
            Item::Function(func) => self.collect_function(func),
        }
    }

    fn collect_agent(&mut self, agent: &Agent) {
        self.add_definition(SymbolDef {
            name: agent.name.name.clone(),
            kind: SymbolKind::Agent,
            span: agent.name.span,
            type_info: None,
            doc: agent.description.as_ref().map(|s| s.value.clone()),
        });

        // Collect state fields
        if let Some(state) = &agent.state {
            for field in &state.fields {
                self.add_definition(SymbolDef {
                    name: field.name.name.clone(),
                    kind: SymbolKind::StateField,
                    span: field.name.span,
                    type_info: Some(self.type_expr_to_string(&field.ty)),
                    doc: None,
                });
            }
        }

        // Collect tools
        for tool in &agent.tools {
            self.collect_tool(tool);
        }

        // Collect event handlers
        for handler in &agent.handlers {
            self.add_definition(SymbolDef {
                name: handler.event.name.clone(),
                kind: SymbolKind::EventHandler,
                span: handler.event.span,
                type_info: None,
                doc: None,
            });
            self.collect_block(&handler.body);
        }
    }

    fn collect_skill(&mut self, skill: &Skill) {
        self.add_definition(SymbolDef {
            name: skill.name.name.clone(),
            kind: SymbolKind::Skill,
            span: skill.name.span,
            type_info: None,
            doc: skill.description.as_ref().map(|s| s.value.clone()),
        });

        for item in &skill.body {
            self.collect_item(item);
        }
    }

    fn collect_typedef(&mut self, typedef: &TypeDef) {
        let type_info = match &typedef.kind {
            TypeDefKind::Struct(fields) => {
                let field_strs: Vec<_> = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name.name, self.type_expr_to_string(&f.ty)))
                    .collect();
                format!("{{ {} }}", field_strs.join(", "))
            }
            TypeDefKind::Alias(ty) => self.type_expr_to_string(ty),
        };

        self.add_definition(SymbolDef {
            name: typedef.name.name.clone(),
            kind: SymbolKind::Type,
            span: typedef.name.span,
            type_info: Some(type_info),
            doc: None,
        });

        // Collect struct fields
        if let TypeDefKind::Struct(fields) = &typedef.kind {
            for field in fields {
                self.add_definition(SymbolDef {
                    name: field.name.name.clone(),
                    kind: SymbolKind::Field,
                    span: field.name.span,
                    type_info: Some(self.type_expr_to_string(&field.ty)),
                    doc: None,
                });
            }
        }
    }

    fn collect_function(&mut self, func: &Function) {
        let params: Vec<_> = func
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name.name, self.type_expr_to_string(&p.ty)))
            .collect();

        let return_type = func
            .return_type
            .as_ref()
            .map(|t| self.type_expr_to_string(t))
            .unwrap_or_else(|| "void".to_string());

        let type_info = format!("({}) -> {}", params.join(", "), return_type);

        self.add_definition(SymbolDef {
            name: func.name.name.clone(),
            kind: SymbolKind::Function,
            span: func.name.span,
            type_info: Some(type_info),
            doc: None,
        });

        // Collect parameters
        for param in &func.params {
            self.collect_param(param);
        }

        // Collect body
        self.collect_block(&func.body);
    }

    fn collect_tool(&mut self, tool: &Tool) {
        let params: Vec<_> = tool
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name.name, self.type_expr_to_string(&p.ty)))
            .collect();

        let return_type = tool
            .return_type
            .as_ref()
            .map(|t| self.type_expr_to_string(t))
            .unwrap_or_else(|| "void".to_string());

        let type_info = format!("({}) -> {}", params.join(", "), return_type);

        self.add_definition(SymbolDef {
            name: tool.name.name.clone(),
            kind: SymbolKind::Tool,
            span: tool.name.span,
            type_info: Some(type_info),
            doc: tool.description.as_ref().map(|s| s.value.clone()),
        });

        // Collect parameters
        for param in &tool.params {
            self.collect_param(param);
        }

        // Collect body if exists
        if let Some(body) = &tool.body {
            self.collect_block(body);
        }
    }

    fn collect_param(&mut self, param: &Param) {
        self.add_definition(SymbolDef {
            name: param.name.name.clone(),
            kind: SymbolKind::Parameter,
            span: param.name.span,
            type_info: Some(self.type_expr_to_string(&param.ty)),
            doc: None,
        });
    }

    fn collect_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.collect_stmt(stmt);
        }
    }

    fn collect_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(let_stmt) => {
                self.collect_binding_pattern(&let_stmt.pattern, let_stmt.ty.as_ref());
                self.collect_expr(&let_stmt.value);
            }
            Stmt::Var(var_stmt) => {
                self.collect_binding_pattern(&var_stmt.pattern, var_stmt.ty.as_ref());
                self.collect_expr(&var_stmt.value);
            }
            Stmt::If(if_stmt) => {
                self.collect_expr(&if_stmt.condition);
                self.collect_block(&if_stmt.then_block);
                if let Some(else_clause) = &if_stmt.else_block {
                    self.collect_else_clause(else_clause);
                }
            }
            Stmt::For(for_stmt) => {
                self.add_definition(SymbolDef {
                    name: for_stmt.binding.name.clone(),
                    kind: SymbolKind::Variable,
                    span: for_stmt.binding.span,
                    type_info: None,
                    doc: None,
                });
                self.collect_expr(&for_stmt.iterable);
                self.collect_block(&for_stmt.body);
            }
            Stmt::While(while_stmt) => {
                self.collect_expr(&while_stmt.condition);
                self.collect_block(&while_stmt.body);
            }
            Stmt::Return(ret) => {
                if let Some(expr) = &ret.value {
                    self.collect_expr(expr);
                }
            }
            Stmt::Expr(expr_stmt) => self.collect_expr(&expr_stmt.expr),
            Stmt::Emit(emit) => {
                self.collect_expr(&emit.value);
            }
            Stmt::Block(block) => {
                self.collect_block(block);
            }
            Stmt::Try(try_stmt) => {
                self.collect_block(&try_stmt.try_block);
                if let Some(catch_clause) = &try_stmt.catch {
                    if let Some(param) = &catch_clause.param {
                        self.add_definition(SymbolDef {
                            name: param.name.clone(),
                            kind: SymbolKind::Variable,
                            span: param.span,
                            type_info: catch_clause
                                .param_type
                                .as_ref()
                                .map(|t| self.type_expr_to_string(t)),
                            doc: None,
                        });
                    }
                    self.collect_block(&catch_clause.body);
                }
                if let Some(finally_block) = &try_stmt.finally {
                    self.collect_block(finally_block);
                }
            }
            Stmt::Throw(throw) => {
                self.collect_expr(&throw.value);
            }
        }
    }

    fn collect_else_clause(&mut self, clause: &ElseClause) {
        match clause {
            ElseClause::ElseIf(if_stmt) => {
                self.collect_expr(&if_stmt.condition);
                self.collect_block(&if_stmt.then_block);
                if let Some(else_clause) = &if_stmt.else_block {
                    self.collect_else_clause(else_clause);
                }
            }
            ElseClause::Else(block) => {
                self.collect_block(block);
            }
        }
    }

    fn collect_binding_pattern(&mut self, pattern: &BindingPattern, ty: Option<&TypeExpr>) {
        match pattern {
            BindingPattern::Identifier(ident) => {
                self.add_definition(SymbolDef {
                    name: ident.name.clone(),
                    kind: SymbolKind::Variable,
                    span: ident.span,
                    type_info: ty.map(|t| self.type_expr_to_string(t)),
                    doc: None,
                });
            }
            BindingPattern::Object(obj) => {
                for field in &obj.fields {
                    // If there's a nested binding pattern, collect that
                    if let Some(binding) = &field.binding {
                        self.collect_binding_pattern(binding, None);
                    } else {
                        // Otherwise the key itself becomes the variable
                        self.add_definition(SymbolDef {
                            name: field.key.name.clone(),
                            kind: SymbolKind::Variable,
                            span: field.key.span,
                            type_info: None,
                            doc: None,
                        });
                    }
                    // Collect default expression if present
                    if let Some(default) = &field.default {
                        self.collect_expr(default);
                    }
                }
                if let Some(rest) = &obj.rest {
                    self.add_definition(SymbolDef {
                        name: rest.name.clone(),
                        kind: SymbolKind::Variable,
                        span: rest.span,
                        type_info: None,
                        doc: None,
                    });
                }
            }
            BindingPattern::Array(arr) => {
                for elem in &arr.elements {
                    match elem {
                        sag_parser::ArrayPatternElement::Pattern(pat) => {
                            self.collect_binding_pattern(pat, None);
                        }
                        sag_parser::ArrayPatternElement::Rest(ident) => {
                            self.add_definition(SymbolDef {
                                name: ident.name.clone(),
                                kind: SymbolKind::Variable,
                                span: ident.span,
                                type_info: None,
                                doc: None,
                            });
                        }
                        sag_parser::ArrayPatternElement::Hole => {}
                    }
                }
            }
        }
    }

    fn collect_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Identifier(ident) => {
                // This is a reference to a symbol
                let def_span = self.find_definition_span(&ident.name);
                self.references.push(SymbolRef {
                    name: ident.name.clone(),
                    span: ident.span,
                    definition_span: def_span,
                });
            }
            Expr::Call(call) => {
                self.collect_expr(&call.callee);
                for arg in &call.args {
                    self.collect_expr(arg);
                }
            }
            Expr::Member(member) => {
                self.collect_expr(&member.object);
                // member.property is a reference to a field
            }
            Expr::Index(index) => {
                self.collect_expr(&index.object);
                self.collect_expr(&index.index);
            }
            Expr::Binary(binary) => {
                self.collect_expr(&binary.left);
                self.collect_expr(&binary.right);
            }
            Expr::Unary(unary) => {
                self.collect_expr(&unary.operand);
            }
            Expr::Array(arr) => {
                for elem in &arr.elements {
                    self.collect_expr(elem);
                }
            }
            Expr::Record(rec) => {
                for (_, value) in &rec.fields {
                    self.collect_expr(value);
                }
            }
            Expr::Await(await_expr) => {
                self.collect_expr(&await_expr.expr);
            }
            Expr::Match(match_expr) => {
                self.collect_expr(&match_expr.subject);
                for arm in &match_expr.arms {
                    if let Some(guard) = &arm.guard {
                        self.collect_expr(guard);
                    }
                    self.collect_expr(&arm.body);
                }
            }
            Expr::OptionalMember(opt) => {
                self.collect_expr(&opt.object);
            }
            Expr::OptionalIndex(opt) => {
                self.collect_expr(&opt.object);
                self.collect_expr(&opt.index);
            }
            Expr::NullCoalesce(nc) => {
                self.collect_expr(&nc.left);
                self.collect_expr(&nc.right);
            }
            Expr::Range(range) => {
                if let Some(start) = &range.start {
                    self.collect_expr(start);
                }
                if let Some(end) = &range.end {
                    self.collect_expr(end);
                }
            }
            Expr::Arrow(arrow) => {
                for param in &arrow.params {
                    self.collect_param(param);
                }
                match &arrow.body {
                    ArrowBody::Expr(expr) => self.collect_expr(expr),
                    ArrowBody::Block(block) => self.collect_block(block),
                }
            }
            Expr::Assign(assign) => {
                self.collect_expr(&assign.target);
                self.collect_expr(&assign.value);
            }
            Expr::Template(template) => {
                for part in &template.parts {
                    if let sag_parser::TemplatePart::Expr(expr) = part {
                        self.collect_expr(expr);
                    }
                }
            }
            // Literals don't contain references
            Expr::Literal(_) => {}
        }
    }

    fn find_definition_span(&self, name: &str) -> Option<Span> {
        self.definitions
            .get(name)
            .and_then(|defs| defs.first())
            .map(|def| def.span)
    }

    fn add_definition(&mut self, def: SymbolDef) {
        let key = (def.span.start, def.span.end);
        self.def_by_span.insert(key, def.clone());
        self.definitions
            .entry(def.name.clone())
            .or_default()
            .push(def);
    }

    fn type_expr_to_string(&self, ty: &TypeExpr) -> String {
        match ty {
            TypeExpr::Named(named) => {
                if named.args.is_empty() {
                    named.name.name.clone()
                } else {
                    let args_str: Vec<_> = named
                        .args
                        .iter()
                        .map(|a| self.type_expr_to_string(a))
                        .collect();
                    format!("{}<{}>", named.name.name, args_str.join(", "))
                }
            }
            TypeExpr::Array(arr) => format!("{}[]", self.type_expr_to_string(&arr.element)),
            TypeExpr::Optional(inner) => format!("{}?", self.type_expr_to_string(inner)),
            TypeExpr::Union(types) => {
                let strs: Vec<_> = types.iter().map(|t| self.type_expr_to_string(t)).collect();
                strs.join(" | ")
            }
            TypeExpr::Tuple(tuple) => {
                let strs: Vec<_> = tuple
                    .elements
                    .iter()
                    .map(|t| self.type_expr_to_string(t))
                    .collect();
                format!("({})", strs.join(", "))
            }
            TypeExpr::Record(rec) => {
                format!(
                    "record<{}, {}>",
                    self.type_expr_to_string(&rec.key),
                    self.type_expr_to_string(&rec.value)
                )
            }
            TypeExpr::Function(func) => {
                let params_str: Vec<_> = func
                    .params
                    .iter()
                    .map(|p| self.type_expr_to_string(p))
                    .collect();
                format!(
                    "({}) -> {}",
                    params_str.join(", "),
                    self.type_expr_to_string(&func.return_type)
                )
            }
        }
    }
}

/// Get all document symbols for the outline.
pub fn get_document_symbols(program: &Program, source: &str) -> Vec<tower_lsp::lsp_types::DocumentSymbol> {
    use tower_lsp::lsp_types::{DocumentSymbol, Range, SymbolKind as LspSymbolKind};

    fn span_to_range(source: &str, span: Span) -> Range {
        let start = offset_to_position(source, span.start);
        let end = offset_to_position(source, span.end);
        Range { start, end }
    }

    fn offset_to_position(source: &str, offset: usize) -> tower_lsp::lsp_types::Position {
        let offset = offset.min(source.len());
        let before = &source[..offset];
        let line = before.chars().filter(|&c| c == '\n').count() as u32;
        let line_start = before.rfind('\n').map(|i| i + 1).unwrap_or(0);
        let character = (offset - line_start) as u32;
        tower_lsp::lsp_types::Position { line, character }
    }

    let mut symbols = Vec::new();

    for item in &program.items {
        match item {
            Item::Agent(agent) => {
                let mut children = Vec::new();

                // Add tools as children
                for tool in &agent.tools {
                    #[allow(deprecated)]
                    children.push(DocumentSymbol {
                        name: tool.name.name.clone(),
                        detail: tool.description.as_ref().map(|s| s.value.clone()),
                        kind: LspSymbolKind::METHOD,
                        tags: None,
                        deprecated: None,
                        range: span_to_range(source, tool.span),
                        selection_range: span_to_range(source, tool.name.span),
                        children: None,
                    });
                }

                // Add event handlers as children
                for handler in &agent.handlers {
                    #[allow(deprecated)]
                    children.push(DocumentSymbol {
                        name: format!("on {}", handler.event.name),
                        detail: None,
                        kind: LspSymbolKind::EVENT,
                        tags: None,
                        deprecated: None,
                        range: span_to_range(source, handler.span),
                        selection_range: span_to_range(source, handler.event.span),
                        children: None,
                    });
                }

                #[allow(deprecated)]
                symbols.push(DocumentSymbol {
                    name: agent.name.name.clone(),
                    detail: agent.description.as_ref().map(|s| s.value.clone()),
                    kind: LspSymbolKind::CLASS,
                    tags: None,
                    deprecated: None,
                    range: span_to_range(source, agent.span),
                    selection_range: span_to_range(source, agent.name.span),
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                });
            }
            Item::TypeDef(typedef) => {
                let mut children = Vec::new();

                if let TypeDefKind::Struct(fields) = &typedef.kind {
                    for field in fields {
                        #[allow(deprecated)]
                        children.push(DocumentSymbol {
                            name: field.name.name.clone(),
                            detail: None,
                            kind: LspSymbolKind::FIELD,
                            tags: None,
                            deprecated: None,
                            range: span_to_range(source, field.span),
                            selection_range: span_to_range(source, field.name.span),
                            children: None,
                        });
                    }
                }

                #[allow(deprecated)]
                symbols.push(DocumentSymbol {
                    name: typedef.name.name.clone(),
                    detail: None,
                    kind: LspSymbolKind::STRUCT,
                    tags: None,
                    deprecated: None,
                    range: span_to_range(source, typedef.span),
                    selection_range: span_to_range(source, typedef.name.span),
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                });
            }
            Item::Function(func) => {
                #[allow(deprecated)]
                symbols.push(DocumentSymbol {
                    name: func.name.name.clone(),
                    detail: None,
                    kind: LspSymbolKind::FUNCTION,
                    tags: None,
                    deprecated: None,
                    range: span_to_range(source, func.span),
                    selection_range: span_to_range(source, func.name.span),
                    children: None,
                });
            }
            Item::Skill(skill) => {
                #[allow(deprecated)]
                symbols.push(DocumentSymbol {
                    name: skill.name.name.clone(),
                    detail: skill.description.as_ref().map(|s| s.value.clone()),
                    kind: LspSymbolKind::MODULE,
                    tags: None,
                    deprecated: None,
                    range: span_to_range(source, skill.span),
                    selection_range: span_to_range(source, skill.name.span),
                    children: None,
                });
            }
        }
    }

    symbols
}
