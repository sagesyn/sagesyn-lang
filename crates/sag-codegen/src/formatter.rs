//! Formatter for the Sage Agent Programming Language.
//!
//! This module provides pretty-printing/formatting for `.sag` files.

use sag_parser::*;

/// Formatter configuration.
#[derive(Debug, Clone)]
pub struct FormatConfig {
    /// Number of spaces per indentation level.
    pub indent_size: usize,
    /// Maximum line width before wrapping.
    pub max_width: usize,
    /// Whether to use trailing commas in multi-line constructs.
    pub trailing_commas: bool,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            indent_size: 2,
            max_width: 100,
            trailing_commas: true,
        }
    }
}

/// Sage code formatter.
pub struct Formatter {
    config: FormatConfig,
    indent: usize,
    output: String,
}

impl Formatter {
    /// Create a new formatter with default configuration.
    pub fn new() -> Self {
        Self::with_config(FormatConfig::default())
    }

    /// Create a new formatter with custom configuration.
    pub fn with_config(config: FormatConfig) -> Self {
        Self {
            config,
            indent: 0,
            output: String::new(),
        }
    }

    /// Format a program.
    pub fn format(program: &Program) -> String {
        let mut formatter = Formatter::new();
        formatter.format_program(program);
        formatter.output
    }

    /// Format a program with custom configuration.
    pub fn format_with_config(program: &Program, config: FormatConfig) -> String {
        let mut formatter = Formatter::with_config(config);
        formatter.format_program(program);
        formatter.output
    }

    fn indent(&self) -> String {
        " ".repeat(self.indent * self.config.indent_size)
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn writeln(&mut self, s: &str) {
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn format_program(&mut self, program: &Program) {
        let mut first = true;
        for item in &program.items {
            if !first {
                self.newline();
            }
            first = false;
            self.format_item(item);
        }
    }

    fn format_item(&mut self, item: &Item) {
        match item {
            Item::Agent(agent) => self.format_agent(agent),
            Item::TypeDef(typedef) => self.format_typedef(typedef),
            Item::Function(func) => self.format_function(func),
            Item::Skill(skill) => self.format_skill(skill),
        }
    }

    fn format_agent(&mut self, agent: &Agent) {
        self.write(&self.indent());
        self.write("agent ");
        self.write(&agent.name.name);
        self.writeln(" {");
        self.indent += 1;

        // Description
        if let Some(desc) = &agent.description {
            self.write(&self.indent());
            self.write("description: ");
            self.format_string(&desc.value);
            self.newline();
        }

        // Version
        if let Some(ver) = &agent.version {
            self.write(&self.indent());
            self.write("version: ");
            self.format_string(&ver.value);
            self.newline();
        }

        // Model
        if let Some(model) = &agent.model {
            if let Some(name) = &model.name {
                self.write(&self.indent());
                self.write("model: ");
                self.format_string(&name.value);
                self.newline();
            }
        }

        // State
        if let Some(state) = &agent.state {
            self.newline();
            self.write(&self.indent());
            self.writeln("state {");
            self.indent += 1;
            for field in &state.fields {
                self.format_field(field);
            }
            self.indent -= 1;
            self.write(&self.indent());
            self.writeln("}");
        }

        // Tools
        for tool in &agent.tools {
            self.newline();
            self.format_tool(tool);
        }

        // Event handlers
        for handler in &agent.handlers {
            self.newline();
            self.format_handler(handler);
        }

        self.indent -= 1;
        self.write(&self.indent());
        self.writeln("}");
    }

    fn format_tool(&mut self, tool: &Tool) {
        self.write(&self.indent());
        self.write("tool ");
        self.write(&tool.name.name);
        self.write("(");
        self.format_params(&tool.params);
        self.write(")");

        if let Some(ret) = &tool.return_type {
            self.write(" -> ");
            self.format_type_expr(ret);
        }

        self.writeln(" {");
        self.indent += 1;

        // Description
        if let Some(desc) = &tool.description {
            self.write(&self.indent());
            self.write("description: ");
            self.format_string(&desc.value);
            self.newline();
        }

        // MCP bindings
        if let Some(server) = &tool.mcp_server {
            self.write(&self.indent());
            self.write("mcp_server: ");
            self.writeln(&server.name);
        }
        if let Some(mcp_tool) = &tool.mcp_tool {
            self.write(&self.indent());
            self.write("mcp_tool: ");
            self.writeln(&mcp_tool.name);
        }

        // Body
        if let Some(body) = &tool.body {
            for stmt in &body.stmts {
                self.format_stmt(stmt);
            }
        }

        self.indent -= 1;
        self.write(&self.indent());
        self.writeln("}");
    }

    fn format_handler(&mut self, handler: &EventHandler) {
        self.write(&self.indent());
        self.write("on ");
        self.write(&handler.event.name);
        self.writeln(" {");
        self.indent += 1;

        for stmt in &handler.body.stmts {
            self.format_stmt(stmt);
        }

        self.indent -= 1;
        self.write(&self.indent());
        self.writeln("}");
    }

    fn format_typedef(&mut self, typedef: &TypeDef) {
        self.write(&self.indent());
        self.write("type ");
        self.write(&typedef.name.name);

        if !typedef.generics.is_empty() {
            self.write("<");
            for (i, g) in typedef.generics.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&g.name);
            }
            self.write(">");
        }

        match &typedef.kind {
            TypeDefKind::Struct(fields) => {
                self.writeln(" {");
                self.indent += 1;
                for field in fields {
                    self.format_field(field);
                }
                self.indent -= 1;
                self.write(&self.indent());
                self.writeln("}");
            }
            TypeDefKind::Alias(ty) => {
                self.write(" = ");
                self.format_type_expr(ty);
                self.newline();
            }
        }
    }

    fn format_field(&mut self, field: &Field) {
        self.write(&self.indent());
        self.write(&field.name.name);
        if field.optional {
            self.write("?");
        }
        self.write(": ");
        self.format_type_expr(&field.ty);
        self.newline();
    }

    fn format_function(&mut self, func: &Function) {
        self.write(&self.indent());
        if func.is_async {
            self.write("async ");
        }
        self.write("fn ");
        self.write(&func.name.name);
        self.write("(");
        self.format_params(&func.params);
        self.write(")");

        if let Some(ret) = &func.return_type {
            self.write(" -> ");
            self.format_type_expr(ret);
        }

        self.writeln(" {");
        self.indent += 1;

        for stmt in &func.body.stmts {
            self.format_stmt(stmt);
        }

        self.indent -= 1;
        self.write(&self.indent());
        self.writeln("}");
    }

    fn format_skill(&mut self, skill: &Skill) {
        self.write(&self.indent());
        self.write("skill ");
        self.write(&skill.name.name);
        self.writeln(" {");
        self.indent += 1;

        for item in &skill.body {
            self.format_item(item);
        }

        self.indent -= 1;
        self.write(&self.indent());
        self.writeln("}");
    }

    fn format_params(&mut self, params: &[Param]) {
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&param.name.name);
            self.write(": ");
            self.format_type_expr(&param.ty);
            if let Some(default) = &param.default {
                self.write(" = ");
                self.format_expr(default);
            }
        }
    }

    fn format_type_expr(&mut self, ty: &TypeExpr) {
        match ty {
            TypeExpr::Named(named) => {
                self.write(&named.name.name);
                if !named.args.is_empty() {
                    self.write("<");
                    for (i, arg) in named.args.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.format_type_expr(arg);
                    }
                    self.write(">");
                }
            }
            TypeExpr::Array(arr) => {
                self.write("array<");
                self.format_type_expr(&arr.element);
                self.write(">");
            }
            TypeExpr::Record(rec) => {
                self.write("record<");
                self.format_type_expr(&rec.key);
                self.write(", ");
                self.format_type_expr(&rec.value);
                self.write(">");
            }
            TypeExpr::Tuple(tup) => {
                self.write("tuple<");
                for (i, elem) in tup.elements.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_type_expr(elem);
                }
                self.write(">");
            }
            TypeExpr::Optional(inner) => {
                self.write("optional<");
                self.format_type_expr(inner);
                self.write(">");
            }
            TypeExpr::Union(types) => {
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        self.write(" | ");
                    }
                    self.format_type_expr(t);
                }
            }
            TypeExpr::Function(func) => {
                self.write("(");
                for (i, param) in func.params.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_type_expr(param);
                }
                self.write(") -> ");
                self.format_type_expr(&func.return_type);
            }
        }
    }

    fn format_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(l) => {
                self.write(&self.indent());
                self.write("let ");
                self.write(&l.name.name);
                if let Some(ty) = &l.ty {
                    self.write(": ");
                    self.format_type_expr(ty);
                }
                self.write(" = ");
                self.format_expr(&l.value);
                self.newline();
            }
            Stmt::Var(v) => {
                self.write(&self.indent());
                self.write("var ");
                self.write(&v.name.name);
                if let Some(ty) = &v.ty {
                    self.write(": ");
                    self.format_type_expr(ty);
                }
                self.write(" = ");
                self.format_expr(&v.value);
                self.newline();
            }
            Stmt::If(i) => {
                self.write(&self.indent());
                self.write("if ");
                self.format_expr(&i.condition);
                self.writeln(" {");
                self.indent += 1;
                for s in &i.then_block.stmts {
                    self.format_stmt(s);
                }
                self.indent -= 1;
                self.write(&self.indent());
                self.write("}");

                if let Some(else_clause) = &i.else_block {
                    self.format_else_clause(else_clause);
                }
                self.newline();
            }
            Stmt::For(f) => {
                self.write(&self.indent());
                self.write("for ");
                self.write(&f.binding.name);
                self.write(" in ");
                self.format_expr(&f.iterable);
                self.writeln(" {");
                self.indent += 1;
                for s in &f.body.stmts {
                    self.format_stmt(s);
                }
                self.indent -= 1;
                self.write(&self.indent());
                self.writeln("}");
            }
            Stmt::While(w) => {
                self.write(&self.indent());
                self.write("while ");
                self.format_expr(&w.condition);
                self.writeln(" {");
                self.indent += 1;
                for s in &w.body.stmts {
                    self.format_stmt(s);
                }
                self.indent -= 1;
                self.write(&self.indent());
                self.writeln("}");
            }
            Stmt::Return(r) => {
                self.write(&self.indent());
                self.write("return");
                if let Some(val) = &r.value {
                    self.write(" ");
                    self.format_expr(val);
                }
                self.newline();
            }
            Stmt::Emit(e) => {
                self.write(&self.indent());
                self.write("emit ");
                self.write(&e.event.name);
                self.write("(");
                self.format_expr(&e.value);
                self.write(")");
                self.newline();
            }
            Stmt::Expr(e) => {
                self.write(&self.indent());
                self.format_expr(&e.expr);
                self.newline();
            }
            Stmt::Block(b) => {
                self.write(&self.indent());
                self.writeln("{");
                self.indent += 1;
                for s in &b.stmts {
                    self.format_stmt(s);
                }
                self.indent -= 1;
                self.write(&self.indent());
                self.writeln("}");
            }
        }
    }

    fn format_else_clause(&mut self, clause: &ElseClause) {
        match clause {
            ElseClause::ElseIf(elif) => {
                self.write(" else if ");
                self.format_expr(&elif.condition);
                self.writeln(" {");
                self.indent += 1;
                for s in &elif.then_block.stmts {
                    self.format_stmt(s);
                }
                self.indent -= 1;
                self.write(&self.indent());
                self.write("}");
                if let Some(else_clause) = &elif.else_block {
                    self.format_else_clause(else_clause);
                }
            }
            ElseClause::Else(block) => {
                self.writeln(" else {");
                self.indent += 1;
                for s in &block.stmts {
                    self.format_stmt(s);
                }
                self.indent -= 1;
                self.write(&self.indent());
                self.write("}");
            }
        }
    }

    fn format_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(lit) => self.format_literal(lit),
            Expr::Identifier(ident) => self.write(&ident.name),
            Expr::Binary(bin) => {
                self.format_expr(&bin.left);
                self.write(" ");
                self.write(bin.op.as_str());
                self.write(" ");
                self.format_expr(&bin.right);
            }
            Expr::Unary(un) => {
                self.write(un.op.as_str());
                self.format_expr(&un.operand);
            }
            Expr::Call(call) => {
                self.format_expr(&call.callee);
                self.write("(");
                for (i, arg) in call.args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(arg);
                }
                self.write(")");
            }
            Expr::Member(mem) => {
                self.format_expr(&mem.object);
                self.write(".");
                self.write(&mem.property.name);
            }
            Expr::Index(idx) => {
                self.format_expr(&idx.object);
                self.write("[");
                self.format_expr(&idx.index);
                self.write("]");
            }
            Expr::Array(arr) => {
                self.write("[");
                for (i, elem) in arr.elements.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(elem);
                }
                self.write("]");
            }
            Expr::Record(rec) => {
                self.write("{ ");
                for (i, (key, val)) in rec.fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&key.name);
                    self.write(": ");
                    self.format_expr(val);
                }
                self.write(" }");
            }
            Expr::Await(aw) => {
                self.write("await ");
                self.format_expr(&aw.expr);
            }
            Expr::Arrow(arrow) => {
                self.write("(");
                self.format_params(&arrow.params);
                self.write(") => ");
                match &arrow.body {
                    ArrowBody::Expr(e) => self.format_expr(e),
                    ArrowBody::Block(b) => {
                        self.writeln("{");
                        self.indent += 1;
                        for s in &b.stmts {
                            self.format_stmt(s);
                        }
                        self.indent -= 1;
                        self.write(&self.indent());
                        self.write("}");
                    }
                }
            }
            Expr::Match(m) => {
                self.write("match ");
                self.format_expr(&m.subject);
                self.writeln(" {");
                self.indent += 1;
                for arm in &m.arms {
                    self.write(&self.indent());
                    self.format_pattern(&arm.pattern);
                    self.write(" => ");
                    self.format_expr(&arm.body);
                    self.newline();
                }
                self.indent -= 1;
                self.write(&self.indent());
                self.write("}");
            }
            Expr::Template(tmpl) => {
                self.write("`");
                for part in &tmpl.parts {
                    match part {
                        TemplatePart::String(s) => self.write(s),
                        TemplatePart::Expr(e) => {
                            self.write("${");
                            self.format_expr(e);
                            self.write("}");
                        }
                    }
                }
                self.write("`");
            }
            Expr::Assign(assign) => {
                self.format_expr(&assign.target);
                self.write(" = ");
                self.format_expr(&assign.value);
            }
        }
    }

    fn format_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::String(s) => self.format_string(&s.value),
            Literal::Number(n) => {
                // Format number nicely
                if n.value.fract() == 0.0 {
                    self.write(&format!("{}", n.value as i64));
                } else {
                    self.write(&n.value.to_string());
                }
            }
            Literal::Boolean(b) => {
                self.write(if b.value { "true" } else { "false" });
            }
            Literal::Null(_) => {
                self.write("null");
            }
        }
    }

    fn format_string(&mut self, s: &str) {
        self.write("\"");
        // Escape special characters
        for c in s.chars() {
            match c {
                '"' => self.write("\\\""),
                '\\' => self.write("\\\\"),
                '\n' => self.write("\\n"),
                '\r' => self.write("\\r"),
                '\t' => self.write("\\t"),
                _ => self.output.push(c),
            }
        }
        self.write("\"");
    }

    fn format_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Identifier(ident) => self.write(&ident.name),
            Pattern::Literal(lit) => self.format_literal(lit),
            Pattern::Wildcard(_) => self.write("_"),
        }
    }
}

impl Default for Formatter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sag_parser::Parser;

    #[test]
    fn test_format_simple_function() {
        let source = r#"fn add(a: number, b: number) -> number { return a + b }"#;
        let program = Parser::parse(source).unwrap();
        let formatted = Formatter::format(&program);

        assert!(formatted.contains("fn add(a: number, b: number) -> number {"));
        assert!(formatted.contains("return a + b"));
    }

    #[test]
    fn test_format_agent() {
        let source = r#"agent MyAgent { description: "Test" version: "1.0.0" }"#;
        let program = Parser::parse(source).unwrap();
        let formatted = Formatter::format(&program);

        assert!(formatted.contains("agent MyAgent {"));
        assert!(formatted.contains("description: \"Test\""));
        assert!(formatted.contains("version: \"1.0.0\""));
    }

    #[test]
    fn test_format_type_def() {
        let source = r#"type User { name: string age: number }"#;
        let program = Parser::parse(source).unwrap();
        let formatted = Formatter::format(&program);

        assert!(formatted.contains("type User {"));
        assert!(formatted.contains("name: string"));
        assert!(formatted.contains("age: number"));
    }

    #[test]
    fn test_format_preserves_template_literals() {
        let source = r#"fn greet(name: string) -> string { return `Hello, ${name}!` }"#;
        let program = Parser::parse(source).unwrap();
        let formatted = Formatter::format(&program);

        assert!(formatted.contains("`Hello, ${name}!`"));
    }

    #[test]
    fn test_format_if_else() {
        let source = r#"fn check(x: number) -> string { if x > 0 { return "positive" } else { return "non-positive" } }"#;
        let program = Parser::parse(source).unwrap();
        let formatted = Formatter::format(&program);

        assert!(formatted.contains("if x > 0 {"));
        assert!(formatted.contains("} else {"));
    }
}
