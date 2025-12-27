//! Python code generator for Google ADK.
//!
//! Generates Python code compatible with Google's Agent Development Kit (ADK).

#![allow(clippy::only_used_in_recursion)]

use crate::{CodeGenerator, CodegenError};
use sag_parser::*;

/// Python code generator targeting Google ADK.
pub struct PythonGenerator {
    indent: usize,
}

impl PythonGenerator {
    pub fn new() -> Self {
        Self { indent: 0 }
    }

    fn indent_str(&self) -> String {
        "    ".repeat(self.indent)
    }

    fn generate_type(&self, ty: &TypeExpr) -> String {
        match ty {
            TypeExpr::Named(n) => {
                let base = match n.name.name.as_str() {
                    "string" => "str".to_string(),
                    "number" => "float".to_string(),
                    "boolean" => "bool".to_string(),
                    "timestamp" => "datetime".to_string(),
                    "void" => "None".to_string(),
                    name => name.to_string(),
                };
                if n.args.is_empty() {
                    base
                } else {
                    let args: Vec<_> = n.args.iter().map(|a| self.generate_type(a)).collect();
                    format!("{}[{}]", base, args.join(", "))
                }
            }
            TypeExpr::Array(a) => format!("list[{}]", self.generate_type(&a.element)),
            TypeExpr::Record(r) => format!(
                "dict[{}, {}]",
                self.generate_type(&r.key),
                self.generate_type(&r.value)
            ),
            TypeExpr::Tuple(t) => {
                let elements: Vec<_> = t.elements.iter().map(|e| self.generate_type(e)).collect();
                format!("tuple[{}]", elements.join(", "))
            }
            TypeExpr::Optional(inner) => format!("{} | None", self.generate_type(inner)),
            TypeExpr::Union(types) => {
                let parts: Vec<_> = types.iter().map(|t| self.generate_type(t)).collect();
                parts.join(" | ")
            }
            TypeExpr::Function(f) => {
                let params: Vec<_> = f.params.iter().map(|p| self.generate_type(p)).collect();
                format!(
                    "Callable[[{}], {}]",
                    params.join(", "),
                    self.generate_type(&f.return_type)
                )
            }
        }
    }

    /// Get a default value for a type (for dataclass fields).
    fn get_type_default(&self, ty: &TypeExpr) -> String {
        match ty {
            TypeExpr::Named(n) => match n.name.name.as_str() {
                "string" => "\"\"".to_string(),
                "number" => "0.0".to_string(),
                "boolean" => "False".to_string(),
                "timestamp" => "field(default_factory=datetime.now)".to_string(),
                _ => "None".to_string(),
            },
            TypeExpr::Array(_) => "field(default_factory=list)".to_string(),
            TypeExpr::Record(_) => "field(default_factory=dict)".to_string(),
            TypeExpr::Tuple(_) => "()".to_string(),
            TypeExpr::Optional(_) => "None".to_string(),
            TypeExpr::Union(_) => "None".to_string(),
            TypeExpr::Function(_) => "None".to_string(),
        }
    }

    fn generate_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::String(s) => format!("\"{}\"", s.value.replace('"', "\\\"")),
                Literal::Number(n) => n.value.to_string(),
                Literal::Boolean(b) => if b.value { "True" } else { "False" }.to_string(),
                Literal::Null(_) => "None".to_string(),
            },
            Expr::Identifier(id) => id.name.clone(),
            Expr::Binary(b) => {
                let op = match b.op {
                    BinaryOp::Add => "+",
                    BinaryOp::Sub => "-",
                    BinaryOp::Mul => "*",
                    BinaryOp::Div => "/",
                    BinaryOp::Mod => "%",
                    BinaryOp::Pow => "**",
                    BinaryOp::Eq => "==",
                    BinaryOp::NotEq => "!=",
                    BinaryOp::Lt => "<",
                    BinaryOp::LtEq => "<=",
                    BinaryOp::Gt => ">",
                    BinaryOp::GtEq => ">=",
                    BinaryOp::And => "and",
                    BinaryOp::Or => "or",
                };
                format!(
                    "({} {} {})",
                    self.generate_expr(&b.left),
                    op,
                    self.generate_expr(&b.right)
                )
            }
            Expr::Unary(u) => {
                let op = match u.op {
                    UnaryOp::Not => "not ",
                    UnaryOp::Neg => "-",
                };
                format!("{}{}", op, self.generate_expr(&u.operand))
            }
            Expr::Call(c) => {
                let args: Vec<_> = c.args.iter().map(|a| self.generate_expr(a)).collect();
                format!("{}({})", self.generate_expr(&c.callee), args.join(", "))
            }
            Expr::Member(m) => format!("{}.{}", self.generate_expr(&m.object), m.property.name),
            Expr::Index(i) => format!(
                "{}[{}]",
                self.generate_expr(&i.object),
                self.generate_expr(&i.index)
            ),
            Expr::Array(a) => {
                let elements: Vec<_> = a.elements.iter().map(|e| self.generate_expr(e)).collect();
                format!("[{}]", elements.join(", "))
            }
            Expr::Record(r) => {
                let fields: Vec<_> = r
                    .fields
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}", k.name, self.generate_expr(v)))
                    .collect();
                format!("{{{}}}", fields.join(", "))
            }
            Expr::Await(a) => format!("await {}", self.generate_expr(&a.expr)),
            Expr::Template(t) => {
                let parts: Vec<_> = t
                    .parts
                    .iter()
                    .map(|p| match p {
                        TemplatePart::String(s) => {
                            // Convert ${var} to {var} for Python f-strings
                            // This is a workaround until the parser properly handles template expressions
                            let mut result = s.clone();
                            while let Some(start) = result.find("${") {
                                if let Some(end) = result[start..].find('}') {
                                    let var_name = &result[start + 2..start + end];
                                    result = format!(
                                        "{}{{{}}}{}",
                                        &result[..start],
                                        var_name,
                                        &result[start + end + 1..]
                                    );
                                } else {
                                    break;
                                }
                            }
                            result
                        }
                        TemplatePart::Expr(e) => format!("{{{}}}", self.generate_expr(e)),
                    })
                    .collect();
                format!("f\"{}\"", parts.join(""))
            }
            Expr::Assign(a) => format!(
                "{} = {}",
                self.generate_expr(&a.target),
                self.generate_expr(&a.value)
            ),
            Expr::Arrow(_) => "lambda: ...".to_string(), // Simplified
            Expr::Match(_) => "# match expression".to_string(), // TODO
        }
    }

    fn generate_stmt(&mut self, stmt: &Stmt, output: &mut String) {
        let indent = self.indent_str();
        match stmt {
            Stmt::Let(l) => {
                let ty_annotation =
                    l.ty.as_ref()
                        .map(|t| format!(": {}", self.generate_type(t)))
                        .unwrap_or_default();
                output.push_str(&format!(
                    "{}{}{} = {}\n",
                    indent,
                    l.name.name,
                    ty_annotation,
                    self.generate_expr(&l.value)
                ));
            }
            Stmt::Var(v) => {
                let ty_annotation =
                    v.ty.as_ref()
                        .map(|t| format!(": {}", self.generate_type(t)))
                        .unwrap_or_default();
                output.push_str(&format!(
                    "{}{}{} = {}\n",
                    indent,
                    v.name.name,
                    ty_annotation,
                    self.generate_expr(&v.value)
                ));
            }
            Stmt::Expr(e) => {
                output.push_str(&format!("{}{}\n", indent, self.generate_expr(&e.expr)));
            }
            Stmt::If(i) => {
                output.push_str(&format!(
                    "{}if {}:\n",
                    indent,
                    self.generate_expr(&i.condition)
                ));
                self.indent += 1;
                self.generate_block(&i.then_block, output);
                self.indent -= 1;
                if let Some(else_clause) = &i.else_block {
                    match else_clause.as_ref() {
                        ElseClause::ElseIf(elif) => {
                            output.push_str(&format!("{indent}el"));
                            self.generate_stmt(&Stmt::If(elif.clone()), output);
                        }
                        ElseClause::Else(block) => {
                            output.push_str(&format!("{indent}else:\n"));
                            self.indent += 1;
                            self.generate_block(block, output);
                            self.indent -= 1;
                        }
                    }
                }
            }
            Stmt::For(f) => {
                output.push_str(&format!(
                    "{}for {} in {}:\n",
                    indent,
                    f.binding.name,
                    self.generate_expr(&f.iterable)
                ));
                self.indent += 1;
                self.generate_block(&f.body, output);
                self.indent -= 1;
            }
            Stmt::While(w) => {
                output.push_str(&format!(
                    "{}while {}:\n",
                    indent,
                    self.generate_expr(&w.condition)
                ));
                self.indent += 1;
                self.generate_block(&w.body, output);
                self.indent -= 1;
            }
            Stmt::Return(r) => {
                if let Some(val) = &r.value {
                    output.push_str(&format!("{}return {}\n", indent, self.generate_expr(val)));
                } else {
                    output.push_str(&format!("{indent}return\n"));
                }
            }
            Stmt::Emit(e) => {
                output.push_str(&format!(
                    "{}yield {{\"{}\": {}}}\n",
                    indent,
                    e.event.name,
                    self.generate_expr(&e.value)
                ));
            }
            Stmt::Block(b) => {
                self.generate_block(b, output);
            }
        }
    }

    fn generate_block(&mut self, block: &Block, output: &mut String) {
        if block.stmts.is_empty() {
            output.push_str(&format!("{}pass\n", self.indent_str()));
        } else {
            for stmt in &block.stmts {
                self.generate_stmt(stmt, output);
            }
        }
    }

    fn generate_tool(&mut self, tool: &Tool, output: &mut String) {
        let params: Vec<_> = tool
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name.name, self.generate_type(&p.ty)))
            .collect();

        let return_type = tool
            .return_type
            .as_ref()
            .map(|t| format!(" -> {}", self.generate_type(t)))
            .unwrap_or_default();

        output.push_str(&format!(
            "def {}({}){}:\n",
            tool.name.name,
            params.join(", "),
            return_type
        ));

        self.indent += 1;

        // Add docstring inside function
        if let Some(desc) = &tool.description {
            output.push_str(&format!(
                "{}\"\"\"{}.\"\"\"\n",
                self.indent_str(),
                desc.value
            ));
        }

        if let Some(body) = &tool.body {
            self.generate_block(body, output);
        } else if tool.mcp_server.is_some() {
            // MCP tool - generate a pass for now
            output.push_str(&format!(
                "{}# MCP tool: server={}, tool={}\n",
                self.indent_str(),
                tool.mcp_server
                    .as_ref()
                    .map(|s| s.name.as_str())
                    .unwrap_or(""),
                tool.mcp_tool
                    .as_ref()
                    .map(|s| s.name.as_str())
                    .unwrap_or("")
            ));
            output.push_str(&format!("{}pass\n", self.indent_str()));
        } else {
            output.push_str(&format!("{}pass\n", self.indent_str()));
        }

        self.indent -= 1;
        output.push('\n');
    }

    fn generate_agent(&mut self, agent: &Agent, output: &mut String) {
        let name = &agent.name.name;
        let description = agent
            .description
            .as_ref()
            .map(|d| d.value.clone())
            .unwrap_or_else(|| format!("{name} agent"));

        // Get model info
        let model_name = agent
            .model
            .as_ref()
            .and_then(|m| m.name.as_ref())
            .map(|n| n.value.clone())
            .unwrap_or_else(|| "gemini-2.0-flash".to_string());

        // Generate state class if agent has state
        let mut state_class = String::new();
        let mut state_init = String::new();
        if let Some(state_block) = &agent.state {
            let state_name = format!("{name}State");
            state_class.push_str(&format!("@dataclass\nclass {state_name}:\n"));
            state_class.push_str("    \"\"\"Agent state.\"\"\"\n");
            for field in &state_block.fields {
                let ty = self.generate_type(&field.ty);
                let default = if field.optional {
                    format!(": {ty} | None = None")
                } else {
                    // Use appropriate defaults based on type
                    let default_val = self.get_type_default(&field.ty);
                    format!(": {ty} = {default_val}")
                };
                state_class.push_str(&format!("    {}{}\n", field.name.name, default));
            }
            state_class.push('\n');
            state_init = format!(
                "\n# Initialize state\n{}_state = {}()\n",
                name.to_lowercase(),
                state_name
            );
        }

        // Generate tools as functions
        let mut tool_functions = String::new();
        let mut tool_names = Vec::new();

        for tool in &agent.tools {
            tool_names.push(tool.name.name.clone());
            self.generate_tool(tool, &mut tool_functions);
        }

        // Build imports
        let mut imports =
            String::from("from google.adk.agents import Agent\nfrom typing import Callable, Any");
        if !state_class.is_empty() {
            imports.push_str("\nfrom dataclasses import dataclass, field");
        }

        // Generate the ADK agent
        output.push_str(&format!(
            r#"# Generated by Sage Agent Compiler
# Target: Google ADK (Agent Development Kit)

{imports}

{state_class}# Tool definitions
{tool_functions}
# Agent definition
{name} = Agent(
    name="{name_lower}",
    model="{model}",
    description="{description}",
    instruction="""You are {name}. {description}

Available tools: {tools}""",
    tools=[{tool_list}],
){state_init}
# Export as root_agent for ADK compatibility
root_agent = {name}
"#,
            imports = imports,
            state_class = state_class,
            tool_functions = tool_functions,
            name = name,
            name_lower = name.to_lowercase(),
            model = model_name,
            description = description,
            tools = tool_names.join(", "),
            tool_list = tool_names.join(", "),
            state_init = state_init,
        ));
    }

    fn generate_type_def(&mut self, typedef: &TypeDef, output: &mut String) {
        match &typedef.kind {
            TypeDefKind::Struct(fields) => {
                output.push_str(&format!("class {}:\n", typedef.name.name));
                self.indent += 1;
                for field in fields {
                    let ty = self.generate_type(&field.ty);
                    let opt = if field.optional { " | None = None" } else { "" };
                    output.push_str(&format!(
                        "{}{}: {}{}\n",
                        self.indent_str(),
                        field.name.name,
                        ty,
                        opt
                    ));
                }
                if fields.is_empty() {
                    output.push_str(&format!("{}pass\n", self.indent_str()));
                }
                self.indent -= 1;
            }
            TypeDefKind::Alias(ty) => {
                output.push_str(&format!(
                    "{} = {}\n",
                    typedef.name.name,
                    self.generate_type(ty)
                ));
            }
        }
        output.push('\n');
    }

    fn generate_function(&mut self, func: &Function, output: &mut String) {
        let indent = self.indent_str();
        let async_prefix = if func.is_async { "async " } else { "" };

        let params: Vec<_> = func
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name.name, self.generate_type(&p.ty)))
            .collect();

        let return_type = func
            .return_type
            .as_ref()
            .map(|t| format!(" -> {}", self.generate_type(t)))
            .unwrap_or_default();

        output.push_str(&format!(
            "{}{}def {}({}){}:\n",
            indent,
            async_prefix,
            func.name.name,
            params.join(", "),
            return_type
        ));

        self.indent += 1;
        self.generate_block(&func.body, output);
        self.indent -= 1;
        output.push('\n');
    }
}

impl Default for PythonGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenerator for PythonGenerator {
    fn generate(&self, program: &Program) -> Result<String, CodegenError> {
        let mut gen = PythonGenerator::new();
        let mut output = String::new();

        // Separate agents from other items
        let mut agents = Vec::new();
        let mut other_items = Vec::new();

        for item in &program.items {
            match item {
                Item::Agent(a) => agents.push(a),
                _ => other_items.push(item),
            }
        }

        // Generate agents first (they include the header)
        for agent in agents {
            gen.generate_agent(agent, &mut output);
        }

        // If no agents, add a simple header
        if output.is_empty() {
            output.push_str("# Generated by Sage Agent Compiler\n");
            output.push_str("# Target: Python\n\n");
            output.push_str("from typing import Callable, Any\n\n");
        }

        // Generate other items
        for item in other_items {
            match item {
                Item::TypeDef(t) => gen.generate_type_def(t, &mut output),
                Item::Function(f) => gen.generate_function(f, &mut output),
                Item::Skill(_) => {} // TODO: Handle skills
                Item::Agent(_) => unreachable!(),
            }
        }

        Ok(output)
    }
}
