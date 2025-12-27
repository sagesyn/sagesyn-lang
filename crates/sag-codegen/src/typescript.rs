//! TypeScript code generator.

use super::{CodeGenerator, CodegenError};
use sag_parser::*;

/// TypeScript code generator.
pub struct TypeScriptGenerator {
    indent: usize,
}

impl TypeScriptGenerator {
    /// Create a new TypeScript generator.
    pub fn new() -> Self {
        Self { indent: 0 }
    }

    fn indent(&self) -> String {
        "  ".repeat(self.indent)
    }

    fn generate_type(&self, ty: &TypeExpr) -> String {
        match ty {
            TypeExpr::Named(named) => {
                let name = match named.name.name.as_str() {
                    "string" => "string".to_string(),
                    "number" => "number".to_string(),
                    "boolean" => "boolean".to_string(),
                    "timestamp" => "Date".to_string(),
                    other => other.to_string(),
                };
                if named.args.is_empty() {
                    name
                } else {
                    let args: Vec<_> = named.args.iter().map(|a| self.generate_type(a)).collect();
                    format!("{}<{}>", name, args.join(", "))
                }
            }
            TypeExpr::Array(arr) => {
                format!("{}[]", self.generate_type(&arr.element))
            }
            TypeExpr::Record(rec) => {
                format!(
                    "Record<{}, {}>",
                    self.generate_type(&rec.key),
                    self.generate_type(&rec.value)
                )
            }
            TypeExpr::Tuple(tup) => {
                let elements: Vec<_> = tup.elements.iter().map(|t| self.generate_type(t)).collect();
                format!("[{}]", elements.join(", "))
            }
            TypeExpr::Optional(inner) => {
                format!("{} | null", self.generate_type(inner))
            }
            TypeExpr::Union(types) => {
                let parts: Vec<_> = types.iter().map(|t| self.generate_type(t)).collect();
                parts.join(" | ")
            }
            TypeExpr::Function(func) => {
                let params: Vec<_> = func
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, t)| format!("arg{}: {}", i, self.generate_type(t)))
                    .collect();
                format!(
                    "({}) => {}",
                    params.join(", "),
                    self.generate_type(&func.return_type)
                )
            }
        }
    }

    fn generate_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(lit) => self.generate_literal(lit),
            Expr::Identifier(ident) => ident.name.clone(),
            Expr::Binary(bin) => {
                let left = self.generate_expr(&bin.left);
                let right = self.generate_expr(&bin.right);
                let op = match bin.op {
                    BinaryOp::Add => "+",
                    BinaryOp::Sub => "-",
                    BinaryOp::Mul => "*",
                    BinaryOp::Div => "/",
                    BinaryOp::Mod => "%",
                    BinaryOp::Pow => "**",
                    BinaryOp::Eq => "===",
                    BinaryOp::NotEq => "!==",
                    BinaryOp::Lt => "<",
                    BinaryOp::LtEq => "<=",
                    BinaryOp::Gt => ">",
                    BinaryOp::GtEq => ">=",
                    BinaryOp::And => "&&",
                    BinaryOp::Or => "||",
                };
                format!("({} {} {})", left, op, right)
            }
            Expr::Unary(un) => {
                let operand = self.generate_expr(&un.operand);
                let op = match un.op {
                    UnaryOp::Not => "!",
                    UnaryOp::Neg => "-",
                };
                format!("{}{}", op, operand)
            }
            Expr::Call(call) => {
                let callee = self.generate_expr(&call.callee);
                let args: Vec<_> = call.args.iter().map(|a| self.generate_expr(a)).collect();
                format!("{}({})", callee, args.join(", "))
            }
            Expr::Member(mem) => {
                format!("{}.{}", self.generate_expr(&mem.object), mem.property.name)
            }
            Expr::Index(idx) => {
                format!(
                    "{}[{}]",
                    self.generate_expr(&idx.object),
                    self.generate_expr(&idx.index)
                )
            }
            Expr::Array(arr) => {
                let elements: Vec<_> = arr.elements.iter().map(|e| self.generate_expr(e)).collect();
                format!("[{}]", elements.join(", "))
            }
            Expr::Record(rec) => {
                let fields: Vec<_> = rec
                    .fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.name, self.generate_expr(v)))
                    .collect();
                format!("{{ {} }}", fields.join(", "))
            }
            Expr::Await(aw) => {
                format!("await {}", self.generate_expr(&aw.expr))
            }
            Expr::Arrow(arrow) => {
                let params: Vec<_> = arrow
                    .params
                    .iter()
                    .map(|p| format!("{}: {}", p.name.name, self.generate_type(&p.ty)))
                    .collect();
                let body = match &arrow.body {
                    ArrowBody::Expr(e) => self.generate_expr(e),
                    ArrowBody::Block(_) => "{ /* block */ }".to_string(),
                };
                format!("({}) => {}", params.join(", "), body)
            }
            Expr::Match(_) => "/* match expression */".to_string(),
            Expr::Template(tmpl) => {
                let parts: Vec<_> = tmpl
                    .parts
                    .iter()
                    .map(|p| match p {
                        TemplatePart::String(s) => s.clone(),
                        TemplatePart::Expr(e) => format!("${{{}}}", self.generate_expr(e)),
                    })
                    .collect();
                format!("`{}`", parts.join(""))
            }
            Expr::Assign(assign) => {
                format!(
                    "{} = {}",
                    self.generate_expr(&assign.target),
                    self.generate_expr(&assign.value)
                )
            }
        }
    }

    fn generate_literal(&self, lit: &Literal) -> String {
        match lit {
            Literal::String(s) => format!("\"{}\"", s.value.replace('\"', "\\\"")),
            Literal::Number(n) => n.value.to_string(),
            Literal::Boolean(b) => b.value.to_string(),
            Literal::Null(_) => "null".to_string(),
        }
    }

    fn generate_stmt(&mut self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::Let(l) => {
                let ty =
                    l.ty.as_ref()
                        .map(|t| format!(": {}", self.generate_type(t)))
                        .unwrap_or_default();
                format!(
                    "{}const {}{} = {};",
                    self.indent(),
                    l.name.name,
                    ty,
                    self.generate_expr(&l.value)
                )
            }
            Stmt::Var(v) => {
                let ty =
                    v.ty.as_ref()
                        .map(|t| format!(": {}", self.generate_type(t)))
                        .unwrap_or_default();
                format!(
                    "{}let {}{} = {};",
                    self.indent(),
                    v.name.name,
                    ty,
                    self.generate_expr(&v.value)
                )
            }
            Stmt::Expr(e) => {
                format!("{}{};", self.indent(), self.generate_expr(&e.expr))
            }
            Stmt::If(i) => {
                let mut result = format!(
                    "{}if ({}) ",
                    self.indent(),
                    self.generate_expr(&i.condition)
                );
                result.push_str(&self.generate_block(&i.then_block));
                if let Some(else_clause) = &i.else_block {
                    match else_clause.as_ref() {
                        ElseClause::ElseIf(elif) => {
                            result.push_str(" else ");
                            result.push_str(&self.generate_stmt(&Stmt::If(elif.clone())));
                        }
                        ElseClause::Else(block) => {
                            result.push_str(" else ");
                            result.push_str(&self.generate_block(block));
                        }
                    }
                }
                result
            }
            Stmt::For(f) => {
                format!(
                    "{}for (const {} of {}) {}",
                    self.indent(),
                    f.binding.name,
                    self.generate_expr(&f.iterable),
                    self.generate_block(&f.body)
                )
            }
            Stmt::While(w) => {
                format!(
                    "{}while ({}) {}",
                    self.indent(),
                    self.generate_expr(&w.condition),
                    self.generate_block(&w.body)
                )
            }
            Stmt::Return(r) => match &r.value {
                Some(v) => format!("{}return {};", self.indent(), self.generate_expr(v)),
                None => format!("{}return;", self.indent()),
            },
            Stmt::Emit(e) => {
                format!(
                    "{}this.emit(\"{}\", {});",
                    self.indent(),
                    e.event.name,
                    self.generate_expr(&e.value)
                )
            }
            Stmt::Block(b) => self.generate_block(b),
        }
    }

    fn generate_block(&mut self, block: &Block) -> String {
        let mut result = "{\n".to_string();
        self.indent += 1;
        for stmt in &block.stmts {
            result.push_str(&self.generate_stmt(stmt));
            result.push('\n');
        }
        self.indent -= 1;
        result.push_str(&self.indent());
        result.push('}');
        result
    }

    fn generate_field(&self, field: &Field) -> String {
        let optional = if field.optional { "?" } else { "" };
        format!(
            "{}{}: {};",
            field.name.name,
            optional,
            self.generate_type(&field.ty)
        )
    }

    fn generate_typedef(&self, typedef: &TypeDef) -> String {
        let generics = if typedef.generics.is_empty() {
            String::new()
        } else {
            let params: Vec<_> = typedef.generics.iter().map(|g| g.name.clone()).collect();
            format!("<{}>", params.join(", "))
        };

        match &typedef.kind {
            TypeDefKind::Struct(fields) => {
                let mut result = format!("export interface {}{} {{\n", typedef.name.name, generics);
                for field in fields {
                    result.push_str("  ");
                    result.push_str(&self.generate_field(field));
                    result.push('\n');
                }
                result.push_str("}\n");
                result
            }
            TypeDefKind::Alias(ty) => {
                format!(
                    "export type {}{} = {};\n",
                    typedef.name.name,
                    generics,
                    self.generate_type(ty)
                )
            }
        }
    }

    fn generate_function(&mut self, func: &Function) -> String {
        let async_kw = if func.is_async { "async " } else { "" };
        let params: Vec<_> = func
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name.name, self.generate_type(&p.ty)))
            .collect();
        let return_type = func
            .return_type
            .as_ref()
            .map(|t| format!(": {}", self.generate_type(t)))
            .unwrap_or_default();

        format!(
            "export {}function {}({}){}{}",
            async_kw,
            func.name.name,
            params.join(", "),
            return_type,
            self.generate_block(&func.body)
        )
    }

    fn generate_agent(&mut self, agent: &Agent) -> String {
        let mut result = String::new();

        // Generate interface for state if present
        if let Some(state) = &agent.state {
            result.push_str(&format!("interface {}State {{\n", agent.name.name));
            for field in &state.fields {
                result.push_str("  ");
                result.push_str(&self.generate_field(field));
                result.push('\n');
            }
            result.push_str("}\n\n");
        }

        // Generate agent class
        result.push_str(&format!("export class {} {{\n", agent.name.name));
        self.indent += 1;

        // Description
        if let Some(desc) = &agent.description {
            result.push_str(&format!(
                "{}static readonly description = \"{}\";\n",
                self.indent(),
                desc.value
            ));
        }

        // Version
        if let Some(ver) = &agent.version {
            result.push_str(&format!(
                "{}static readonly version = \"{}\";\n",
                self.indent(),
                ver.value
            ));
        }

        // State
        if agent.state.is_some() {
            result.push_str(&format!(
                "\n{}private state: {}State;\n",
                self.indent(),
                agent.name.name
            ));
        }

        // Constructor
        result.push_str(&format!("\n{}constructor() {{\n", self.indent()));
        self.indent += 1;
        if let Some(state) = &agent.state {
            result.push_str(&format!("{}this.state = {{\n", self.indent()));
            self.indent += 1;
            for field in &state.fields {
                let default = if field.optional {
                    "null"
                } else {
                    "undefined as any"
                };
                result.push_str(&format!(
                    "{}{}: {},\n",
                    self.indent(),
                    field.name.name,
                    default
                ));
            }
            self.indent -= 1;
            result.push_str(&format!("{}}};\n", self.indent()));
        }
        self.indent -= 1;
        result.push_str(&format!("{}}}\n", self.indent()));

        // Tools
        for tool in &agent.tools {
            result.push('\n');
            result.push_str(&self.generate_tool(tool));
        }

        // Event handlers
        for handler in &agent.handlers {
            result.push('\n');
            result.push_str(&self.generate_handler(handler));
        }

        self.indent -= 1;
        result.push_str("}\n");
        result
    }

    fn generate_tool(&mut self, tool: &Tool) -> String {
        let params: Vec<_> = tool
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name.name, self.generate_type(&p.ty)))
            .collect();
        let return_type = tool
            .return_type
            .as_ref()
            .map(|t| format!(": {}", self.generate_type(t)))
            .unwrap_or_default();

        let mut result = format!(
            "{}async {}({}){}",
            self.indent(),
            tool.name.name,
            params.join(", "),
            return_type
        );

        if let Some(body) = &tool.body {
            result.push(' ');
            result.push_str(&self.generate_block(body));
        } else if tool.mcp_server.is_some() {
            // MCP-bound tool
            result.push_str(" {\n");
            self.indent += 1;
            if let (Some(server), Some(mcp_tool)) = (&tool.mcp_server, &tool.mcp_tool) {
                result.push_str(&format!(
                    "{}return await this.mcp.call(\"{}\", \"{}\", {{ {} }});\n",
                    self.indent(),
                    server.name,
                    mcp_tool.name,
                    params
                        .iter()
                        .map(|p| p.split(':').next().unwrap().trim())
                        .collect::<Vec<_>>()
                        .join(", ")
                ));
            }
            self.indent -= 1;
            result.push_str(&format!("{}}}", self.indent()));
        } else {
            result.push_str(" { throw new Error('Not implemented'); }");
        }
        result.push('\n');
        result
    }

    fn generate_handler(&mut self, handler: &EventHandler) -> String {
        let mut result = format!(
            "{}on{}(message: any) ",
            self.indent(),
            capitalize(&handler.event.name)
        );
        result.push_str(&self.generate_block(&handler.body));
        result.push('\n');
        result
    }
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

impl Default for TypeScriptGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenerator for TypeScriptGenerator {
    fn generate(&self, program: &Program) -> Result<String, CodegenError> {
        let mut gen = TypeScriptGenerator::new();
        let mut output = String::new();

        // Header comment
        output.push_str("// Generated by Sage Agent Compiler\n");
        output.push_str("// Do not edit manually\n\n");

        for item in &program.items {
            match item {
                Item::Agent(agent) => {
                    output.push_str(&gen.generate_agent(agent));
                    output.push('\n');
                }
                Item::TypeDef(typedef) => {
                    output.push_str(&gen.generate_typedef(typedef));
                    output.push('\n');
                }
                Item::Function(func) => {
                    output.push_str(&gen.generate_function(func));
                    output.push('\n');
                }
                Item::Skill(_) => {
                    // Skills are expanded at compile time, not emitted directly
                }
            }
        }

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sag_parser::Parser;

    #[test]
    fn test_generate_empty_agent() {
        let source = "agent MyAgent { }";
        let program = Parser::parse(source).unwrap();
        let generator = TypeScriptGenerator::new();
        let output = generator.generate(&program).unwrap();

        assert!(output.contains("export class MyAgent"));
    }

    #[test]
    fn test_generate_type_def() {
        let source = r#"
            type User {
                name: string
                age: number
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let generator = TypeScriptGenerator::new();
        let output = generator.generate(&program).unwrap();

        assert!(output.contains("export interface User"));
        assert!(output.contains("name: string"));
        assert!(output.contains("age: number"));
    }

    #[test]
    fn test_generate_function() {
        let source = r#"
            fn greet(name: string) -> string {
                return `Hello, ${name}!`
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let generator = TypeScriptGenerator::new();
        let output = generator.generate(&program).unwrap();

        assert!(output.contains("export function greet(name: string): string"));
    }
}
