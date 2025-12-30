//! Go code generator.

#![allow(clippy::only_used_in_recursion)]

use super::{CodeGenerator, CodegenError};
use sag_parser::*;

/// Go code generator.
pub struct GoGenerator {
    indent: usize,
    package_name: String,
}

impl GoGenerator {
    /// Create a new Go generator.
    pub fn new() -> Self {
        Self {
            indent: 0,
            package_name: "main".to_string(),
        }
    }

    /// Create a new Go generator with a custom package name.
    pub fn with_package(package_name: impl Into<String>) -> Self {
        Self {
            indent: 0,
            package_name: package_name.into(),
        }
    }

    fn indent(&self) -> String {
        "\t".repeat(self.indent)
    }

    fn generate_type(&self, ty: &TypeExpr) -> String {
        match ty {
            TypeExpr::Named(named) => {
                let name = match named.name.name.as_str() {
                    "string" => "string".to_string(),
                    "number" => "float64".to_string(),
                    "boolean" => "bool".to_string(),
                    "timestamp" => "time.Time".to_string(),
                    other => other.to_string(),
                };
                if named.args.is_empty() {
                    name
                } else {
                    // Go doesn't have generics for user types in this context
                    name
                }
            }
            TypeExpr::Array(arr) => {
                format!("[]{}", self.generate_type(&arr.element))
            }
            TypeExpr::Record(rec) => {
                format!(
                    "map[{}]{}",
                    self.generate_type(&rec.key),
                    self.generate_type(&rec.value)
                )
            }
            TypeExpr::Tuple(tup) => {
                // Go doesn't have tuples, use struct
                let fields: Vec<_> = tup
                    .elements
                    .iter()
                    .enumerate()
                    .map(|(i, t)| format!("F{} {}", i, self.generate_type(t)))
                    .collect();
                format!("struct {{ {} }}", fields.join("; "))
            }
            TypeExpr::Optional(inner) => {
                format!("*{}", self.generate_type(inner))
            }
            TypeExpr::Union(_) => {
                // Go doesn't have union types, use interface{}
                "interface{}".to_string()
            }
            TypeExpr::Function(func) => {
                let params: Vec<_> = func.params.iter().map(|t| self.generate_type(t)).collect();
                format!(
                    "func({}) {}",
                    params.join(", "),
                    self.generate_type(&func.return_type)
                )
            }
        }
    }

    fn generate_expr(&mut self, expr: &Expr) -> String {
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
                    BinaryOp::Pow => {
                        return format!("math.Pow({left}, {right})");
                    }
                    BinaryOp::Eq => "==",
                    BinaryOp::NotEq => "!=",
                    BinaryOp::Lt => "<",
                    BinaryOp::LtEq => "<=",
                    BinaryOp::Gt => ">",
                    BinaryOp::GtEq => ">=",
                    BinaryOp::And => "&&",
                    BinaryOp::Or => "||",
                };
                format!("({left} {op} {right})")
            }
            Expr::Unary(un) => {
                let operand = self.generate_expr(&un.operand);
                let op = match un.op {
                    UnaryOp::Not => "!",
                    UnaryOp::Neg => "-",
                };
                format!("{op}{operand}")
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
                format!("[]interface{{}}{{{}}}", elements.join(", "))
            }
            Expr::Record(rec) => {
                let fields: Vec<_> = rec
                    .fields
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}", k.name, self.generate_expr(v)))
                    .collect();
                format!("map[string]interface{{}}{{{}}}", fields.join(", "))
            }
            Expr::Await(aw) => {
                // Go uses goroutines/channels, not await
                format!("<-{}", self.generate_expr(&aw.expr))
            }
            Expr::Arrow(arrow) => {
                let params: Vec<_> = arrow
                    .params
                    .iter()
                    .map(|p| format!("{} {}", p.name.name, self.generate_type(&p.ty)))
                    .collect();
                let body = match &arrow.body {
                    ArrowBody::Expr(e) => format!("return {}", self.generate_expr(e)),
                    ArrowBody::Block(block) => {
                        // Generate block body for anonymous function
                        let mut stmts = Vec::new();
                        for stmt in &block.stmts {
                            stmts.push(self.generate_stmt_inline(stmt));
                        }
                        stmts.join("\n\t\t")
                    }
                };
                format!("func({}) {{ {} }}", params.join(", "), body)
            }
            Expr::Match(m) => self.generate_match_expr(m),
            Expr::Template(tmpl) => {
                let mut parts = Vec::new();
                let mut format_args = Vec::new();

                for p in &tmpl.parts {
                    match p {
                        TemplatePart::String(s) => parts.push(s.replace('%', "%%")),
                        TemplatePart::Expr(e) => {
                            parts.push("%v".to_string());
                            format_args.push(self.generate_expr(e));
                        }
                    }
                }

                if format_args.is_empty() {
                    format!("\"{}\"", parts.join(""))
                } else {
                    format!(
                        "fmt.Sprintf(\"{}\", {})",
                        parts.join(""),
                        format_args.join(", ")
                    )
                }
            }
            Expr::Assign(assign) => {
                format!(
                    "{} = {}",
                    self.generate_expr(&assign.target),
                    self.generate_expr(&assign.value)
                )
            }
            Expr::OptionalMember(m) => {
                // Go doesn't have optional chaining - needs nil check
                let obj = self.generate_expr(&m.object);
                format!(
                    "func() interface{{}} {{ if {obj} != nil {{ return {obj}.{} }} else {{ return nil }} }}()",
                    m.property.name
                )
            }
            Expr::OptionalIndex(i) => {
                let obj = self.generate_expr(&i.object);
                let idx = self.generate_expr(&i.index);
                format!(
                    "func() interface{{}} {{ if {obj} != nil {{ return {obj}[{idx}] }} else {{ return nil }} }}()"
                )
            }
            Expr::NullCoalesce(nc) => {
                let left = self.generate_expr(&nc.left);
                let right = self.generate_expr(&nc.right);
                format!(
                    "func() interface{{}} {{ if {left} != nil {{ return {left} }} else {{ return {right} }} }}()"
                )
            }
            Expr::Range(range) => {
                // Go doesn't have range literals - generate a slice
                let start = range
                    .start
                    .as_ref()
                    .map(|e| self.generate_expr(e))
                    .unwrap_or_else(|| "0".to_string());
                let end = range
                    .end
                    .as_ref()
                    .map(|e| self.generate_expr(e))
                    .unwrap_or_else(|| "0".to_string());
                if range.inclusive {
                    format!("func() []int {{ r := make([]int, 0); for i := {start}; i <= {end}; i++ {{ r = append(r, i) }}; return r }}()")
                } else {
                    format!("func() []int {{ r := make([]int, 0); for i := {start}; i < {end}; i++ {{ r = append(r, i) }}; return r }}()")
                }
            }
        }
    }

    fn generate_binding_pattern(&self, pattern: &BindingPattern) -> String {
        match pattern {
            BindingPattern::Identifier(ident) => ident.name.clone(),
            BindingPattern::Object(_) => {
                // Go doesn't support object destructuring - just return a placeholder
                "_".to_string()
            }
            BindingPattern::Array(arr) => {
                // Go supports multiple assignment but not full destructuring
                let elements: Vec<_> = arr
                    .elements
                    .iter()
                    .map(|e| match e {
                        ArrayPatternElement::Pattern(p) => self.generate_binding_pattern(p),
                        ArrayPatternElement::Rest(_) => "_".to_string(),
                        ArrayPatternElement::Hole => "_".to_string(),
                    })
                    .collect();
                elements.join(", ")
            }
        }
    }

    fn generate_literal(&self, lit: &Literal) -> String {
        match lit {
            Literal::String(s) => format!("\"{}\"", s.value.replace('\"', "\\\"")),
            Literal::Number(n) => n.value.to_string(),
            Literal::Boolean(b) => b.value.to_string(),
            Literal::Null(_) => "nil".to_string(),
        }
    }

    fn generate_stmt(&mut self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::Let(l) => {
                let pattern = self.generate_binding_pattern(&l.pattern);
                format!(
                    "{}{} := {}",
                    self.indent(),
                    pattern,
                    self.generate_expr(&l.value)
                )
            }
            Stmt::Var(v) => {
                let pattern = self.generate_binding_pattern(&v.pattern);
                let ty =
                    v.ty.as_ref()
                        .map(|t| format!(" {}", self.generate_type(t)))
                        .unwrap_or_default();
                format!(
                    "{}var {}{}= {}",
                    self.indent(),
                    pattern,
                    ty,
                    self.generate_expr(&v.value)
                )
            }
            Stmt::Expr(e) => {
                format!("{}{}", self.indent(), self.generate_expr(&e.expr))
            }
            Stmt::If(i) => {
                let mut result =
                    format!("{}if {} ", self.indent(), self.generate_expr(&i.condition));
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
                    "{}for _, {} := range {} {}",
                    self.indent(),
                    f.binding.name,
                    self.generate_expr(&f.iterable),
                    self.generate_block(&f.body)
                )
            }
            Stmt::While(w) => {
                format!(
                    "{}for {} {}",
                    self.indent(),
                    self.generate_expr(&w.condition),
                    self.generate_block(&w.body)
                )
            }
            Stmt::Return(r) => match &r.value {
                Some(v) => format!("{}return {}", self.indent(), self.generate_expr(v)),
                None => format!("{}return", self.indent()),
            },
            Stmt::Emit(e) => {
                format!(
                    "{}a.Emit(\"{}\", {})",
                    self.indent(),
                    e.event.name,
                    self.generate_expr(&e.value)
                )
            }
            Stmt::Block(b) => self.generate_block(b),
            Stmt::Try(t) => {
                // Go uses defer/recover pattern for error handling
                let mut result = format!("{}func() {{\n", self.indent());
                self.indent += 1;
                if t.catch.is_some() {
                    result.push_str(&format!(
                        "{}defer func() {{\n{}if r := recover(); r != nil {{\n",
                        self.indent(),
                        "\t".repeat(self.indent + 1)
                    ));
                    if let Some(catch) = &t.catch {
                        if let Some(param) = &catch.param {
                            result.push_str(&format!(
                                "{}{} := r\n",
                                "\t".repeat(self.indent + 2),
                                param.name
                            ));
                        }
                        self.indent += 2;
                        for stmt in &catch.body.stmts {
                            result.push_str(&self.generate_stmt(stmt));
                            result.push('\n');
                        }
                        self.indent -= 2;
                    }
                    result.push_str(&format!(
                        "{}}}\n{}}}()\n",
                        "\t".repeat(self.indent + 1),
                        self.indent()
                    ));
                }
                for stmt in &t.try_block.stmts {
                    result.push_str(&self.generate_stmt(stmt));
                    result.push('\n');
                }
                self.indent -= 1;
                result.push_str(&format!("{}}}()", self.indent()));
                result
            }
            Stmt::Throw(t) => {
                format!("{}panic({})", self.indent(), self.generate_expr(&t.value))
            }
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

    fn generate_stmt_inline(&mut self, stmt: &Stmt) -> String {
        // Generate statement without leading indent (for inline use)
        let old_indent = self.indent;
        self.indent = 0;
        let result = self.generate_stmt(stmt);
        self.indent = old_indent;
        result
    }

    fn generate_pattern_cases(&mut self, pattern: &Pattern) -> Vec<String> {
        match pattern {
            Pattern::Literal(lit) => vec![self.generate_literal(lit)],
            Pattern::Or(patterns) => patterns
                .iter()
                .flat_map(|p| self.generate_pattern_cases(p))
                .collect(),
            _ => vec![],
        }
    }

    fn generate_match_expr(&mut self, m: &MatchExpr) -> String {
        let subject = self.generate_expr(&m.subject);
        let mut result = format!("func() interface{{}} {{\n\t\t__match_subject__ := {subject}\n");

        let mut first = true;
        for arm in &m.arms {
            let body = self.generate_expr(&arm.body);

            // Generate guard condition if present
            let guard_cond = arm.guard.as_ref().map(|g| self.generate_expr(g));

            match &arm.pattern {
                Pattern::Wildcard(_) => {
                    if let Some(guard) = guard_cond {
                        result
                            .push_str(&format!("\t\tif {guard} {{\n\t\t\treturn {body}\n\t\t}}\n"));
                    } else {
                        result.push_str(&format!("\t\treturn {body}\n"));
                    }
                }
                Pattern::Literal(_) | Pattern::Or(_) => {
                    let cases = self.generate_pattern_cases(&arm.pattern);
                    let case_conditions: Vec<_> = cases
                        .iter()
                        .map(|c| format!("__match_subject__ == {c}"))
                        .collect();
                    let pattern_cond = case_conditions.join(" || ");

                    let full_cond = if let Some(guard) = guard_cond {
                        format!("({pattern_cond}) && ({guard})")
                    } else {
                        pattern_cond
                    };

                    if first {
                        result.push_str(&format!(
                            "\t\tif {full_cond} {{\n\t\t\treturn {body}\n\t\t}}"
                        ));
                        first = false;
                    } else {
                        result.push_str(&format!(
                            " else if {full_cond} {{\n\t\t\treturn {body}\n\t\t}}"
                        ));
                    }
                }
                Pattern::Identifier(ident) => {
                    if let Some(guard) = guard_cond {
                        result.push_str(&format!(
                            " else if func() bool {{ {} := __match_subject__; return {} }}() {{\n\t\t\t{} := __match_subject__\n\t\t\treturn {}\n\t\t}}",
                            ident.name, guard, ident.name, body
                        ));
                    } else {
                        result.push_str(&format!(
                            " else {{\n\t\t\t{} := __match_subject__\n\t\t\treturn {}\n\t\t}}",
                            ident.name, body
                        ));
                    }
                }
                Pattern::Object(_) | Pattern::Array(_) => {
                    // Complex patterns - just add as else clause
                    result.push_str(&format!(" else {{\n\t\t\treturn {body}\n\t\t}}"));
                }
            }
        }

        result.push_str("\n\t\treturn nil\n\t}()");
        result
    }

    fn generate_field(&self, field: &Field) -> String {
        let name = capitalize(&field.name.name);
        let ty = self.generate_type(&field.ty);
        let json_tag = field.name.name.clone();
        let omitempty = if field.optional { ",omitempty" } else { "" };
        format!("{name} {ty} `json:\"{json_tag}{omitempty}\"`")
    }

    fn generate_typedef(&self, typedef: &TypeDef) -> String {
        match &typedef.kind {
            TypeDefKind::Struct(fields) => {
                let mut result = format!("type {} struct {{\n", typedef.name.name);
                for field in fields {
                    result.push('\t');
                    result.push_str(&self.generate_field(field));
                    result.push('\n');
                }
                result.push_str("}\n");
                result
            }
            TypeDefKind::Alias(ty) => {
                format!("type {} = {}\n", typedef.name.name, self.generate_type(ty))
            }
        }
    }

    fn generate_function(&mut self, func: &Function) -> String {
        let params: Vec<_> = func
            .params
            .iter()
            .map(|p| format!("{} {}", p.name.name, self.generate_type(&p.ty)))
            .collect();
        let return_type = func
            .return_type
            .as_ref()
            .map(|t| format!(" {}", self.generate_type(t)))
            .unwrap_or_default();

        format!(
            "func {}({}){}{}",
            capitalize(&func.name.name),
            params.join(", "),
            return_type,
            self.generate_block(&func.body)
        )
    }

    fn generate_agent(&mut self, agent: &Agent) -> String {
        let mut result = String::new();

        // Generate struct for the agent
        result.push_str(&format!("type {} struct {{\n", agent.name.name));
        self.indent += 1;

        // State fields
        if let Some(state) = &agent.state {
            for field in &state.fields {
                result.push_str(&self.indent());
                result.push_str(&self.generate_field(field));
                result.push('\n');
            }
        }

        self.indent -= 1;
        result.push_str("}\n\n");

        // Constructor
        result.push_str(&format!(
            "func New{}() *{} {{\n",
            agent.name.name, agent.name.name
        ));
        result.push_str(&format!("\treturn &{}{{}}\n", agent.name.name));
        result.push_str("}\n\n");

        // Description method
        if let Some(desc) = &agent.description {
            result.push_str(&format!(
                "func (a *{}) Description() string {{\n",
                agent.name.name
            ));
            result.push_str(&format!("\treturn \"{}\"\n", desc.value));
            result.push_str("}\n\n");
        }

        // Version method
        if let Some(ver) = &agent.version {
            result.push_str(&format!(
                "func (a *{}) Version() string {{\n",
                agent.name.name
            ));
            result.push_str(&format!("\treturn \"{}\"\n", ver.value));
            result.push_str("}\n\n");
        }

        // Tools
        for tool in &agent.tools {
            result.push_str(&self.generate_tool(tool, &agent.name.name));
            result.push('\n');
        }

        // Event handlers
        for handler in &agent.handlers {
            result.push_str(&self.generate_handler(handler, &agent.name.name));
            result.push('\n');
        }

        result
    }

    fn generate_tool(&mut self, tool: &Tool, agent_name: &str) -> String {
        let params: Vec<_> = tool
            .params
            .iter()
            .map(|p| format!("{} {}", p.name.name, self.generate_type(&p.ty)))
            .collect();
        let return_type = tool
            .return_type
            .as_ref()
            .map(|t| format!(" {}", self.generate_type(t)))
            .unwrap_or_default();

        let mut result = format!(
            "func (a *{}) {}({}){}",
            agent_name,
            capitalize(&tool.name.name),
            params.join(", "),
            return_type
        );

        if let Some(body) = &tool.body {
            result.push(' ');
            result.push_str(&self.generate_block(body));
        } else {
            result.push_str(" {\n\tpanic(\"not implemented\")\n}");
        }
        result.push('\n');
        result
    }

    fn generate_handler(&mut self, handler: &EventHandler, agent_name: &str) -> String {
        let method_name = format!("On{}", capitalize(&handler.event.name));
        let mut result = format!("func (a *{agent_name}) {method_name}(message interface{{}}) ");
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

impl Default for GoGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenerator for GoGenerator {
    fn generate(&self, program: &Program) -> Result<String, CodegenError> {
        let mut gen = GoGenerator::with_package(&self.package_name);
        let mut output = String::new();

        // Header comment
        output.push_str("// Generated by Sage Agent Compiler\n");
        output.push_str("// Do not edit manually\n\n");

        // Package declaration
        output.push_str(&format!("package {}\n\n", gen.package_name));

        // Collect imports
        let mut needs_fmt = false;
        let mut needs_time = false;

        for item in &program.items {
            if let Item::Agent(agent) = item {
                for tool in &agent.tools {
                    if tool.body.is_some() {
                        needs_fmt = true;
                    }
                }
            }
            // Check for timestamp types
            fn check_type_for_time(ty: &TypeExpr) -> bool {
                match ty {
                    TypeExpr::Named(n) => n.name.name == "timestamp",
                    TypeExpr::Array(a) => check_type_for_time(&a.element),
                    TypeExpr::Optional(o) => check_type_for_time(o),
                    _ => false,
                }
            }
            if let Item::TypeDef(td) = item {
                if let TypeDefKind::Struct(fields) = &td.kind {
                    for f in fields {
                        if check_type_for_time(&f.ty) {
                            needs_time = true;
                        }
                    }
                }
            }
        }

        // Generate imports
        if needs_fmt || needs_time {
            output.push_str("import (\n");
            if needs_fmt {
                output.push_str("\t\"fmt\"\n");
            }
            if needs_time {
                output.push_str("\t\"time\"\n");
            }
            output.push_str(")\n\n");
        }

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
                    // Skills are expanded at compile time
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
        let generator = GoGenerator::new();
        let output = generator.generate(&program).unwrap();

        assert!(output.contains("type MyAgent struct"));
        assert!(output.contains("func NewMyAgent()"));
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
        let generator = GoGenerator::new();
        let output = generator.generate(&program).unwrap();

        assert!(output.contains("type User struct"));
        assert!(output.contains("Name string"));
        assert!(output.contains("Age float64"));
    }

    #[test]
    fn test_generate_function() {
        let source = r#"
            fn add(a: number, b: number) -> number {
                return a + b
            }
        "#;
        let program = Parser::parse(source).unwrap();
        let generator = GoGenerator::new();
        let output = generator.generate(&program).unwrap();

        assert!(output.contains("func Add(a float64, b float64) float64"));
    }
}
