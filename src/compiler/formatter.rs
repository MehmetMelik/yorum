use crate::compiler::ast::*;
use crate::compiler::lexer::Comment;
use crate::compiler::span::Span;

pub struct Formatter {
    source: Vec<char>,
    comments: Vec<Comment>,
    comment_cursor: usize,
    indent: usize,
    out: String,
    at_line_start: bool,
}

impl Formatter {
    pub fn new(source: Vec<char>, comments: Vec<Comment>) -> Self {
        Self {
            source,
            comments,
            comment_cursor: 0,
            indent: 0,
            out: String::new(),
            at_line_start: true,
        }
    }

    // ── Output primitives ──────────────────────────────────────

    fn write(&mut self, s: &str) {
        if self.at_line_start && !s.is_empty() {
            self.write_indent();
            self.at_line_start = false;
        }
        self.out.push_str(s);
    }

    fn writeln(&mut self, s: &str) {
        self.write(s);
        self.newline();
    }

    fn newline(&mut self) {
        self.out.push('\n');
        self.at_line_start = true;
    }

    fn blank_line(&mut self) {
        // Ensure exactly one blank line (two newlines).
        // Trim any trailing whitespace-only lines first.
        let trimmed = self.out.trim_end_matches(' ');
        let trimmed_len = trimmed.len();
        self.out.truncate(trimmed_len);

        if self.out.ends_with("\n\n") {
            // Already have a blank line
            return;
        }
        if !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        self.out.push('\n');
        self.at_line_start = true;
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.out.push_str("    ");
        }
    }

    fn indent_up(&mut self) {
        self.indent += 1;
    }

    fn indent_down(&mut self) {
        self.indent = self.indent.saturating_sub(1);
    }

    // ── Comment handling ───────────────────────────────────────

    fn emit_leading_comments(&mut self, node_start: usize) {
        let mut last_comment_end = None;
        while self.comment_cursor < self.comments.len() {
            let comment_end = self.comments[self.comment_cursor].span.end;
            let comment_start = self.comments[self.comment_cursor].span.start;
            if comment_end <= node_start {
                // Check if there's a blank line between the previous comment and this one
                if let Some(prev_end) = last_comment_end {
                    if self.has_blank_line_between(prev_end, comment_start) {
                        self.blank_line();
                    }
                }
                last_comment_end = Some(comment_end);
                let text = self.comments[self.comment_cursor].text.clone();
                self.emit_comment(&text);
                self.comment_cursor += 1;
            } else {
                break;
            }
        }
        // Check if there's a blank line between the last comment and the node
        if let Some(prev_end) = last_comment_end {
            if self.has_blank_line_between(prev_end, node_start) {
                self.blank_line();
            }
        }
    }

    fn has_comment_between(&self, start: usize, end: usize) -> bool {
        for i in self.comment_cursor..self.comments.len() {
            let c = &self.comments[i];
            if c.span.start >= start && c.span.end <= end {
                return true;
            }
            if c.span.start >= end {
                break;
            }
        }
        false
    }

    fn has_blank_line_between(&self, start: usize, end: usize) -> bool {
        // Check if the source between these two positions contains two or more newlines
        // (i.e., a blank line)
        let mut newline_count = 0;
        for i in start..end.min(self.source.len()) {
            if self.source[i] == '\n' {
                newline_count += 1;
                if newline_count >= 2 {
                    return true;
                }
            }
        }
        false
    }

    fn is_same_line(&self, a: usize, b: usize) -> bool {
        let (lo, hi) = if a < b { (a, b) } else { (b, a) };
        let hi = hi.min(self.source.len());
        !self.source[lo..hi].contains(&'\n')
    }

    /// Returns true when the gap between `a` and `b` in the source is safe for
    /// trailing-comment attachment. Allows whitespace and trailing punctuation
    /// (`,`, `;`) but rejects closing delimiters (`}`, `)`, `]`) and newlines,
    /// which indicate the comment belongs to a parent construct.
    fn is_trailing_gap(&self, a: usize, b: usize) -> bool {
        let (lo, hi) = if a < b { (a, b) } else { (b, a) };
        let hi = hi.min(self.source.len());
        self.source[lo..hi]
            .iter()
            .all(|c| matches!(c, ' ' | '\t' | ',' | ';'))
    }

    fn emit_comment(&mut self, text: &str) {
        self.writeln(text);
    }

    fn emit_trailing_comments(&mut self) {
        while self.comment_cursor < self.comments.len() {
            let text = self.comments[self.comment_cursor].text.clone();
            self.emit_comment(&text);
            self.comment_cursor += 1;
        }
    }

    /// After a construct is emitted (ending with \n), check if next comment(s)
    /// are on the same line in source. If so, rewind \n, append comment inline.
    /// Returns span.end of the last trailing comment consumed (for prev_end tracking).
    fn emit_trailing_comment_for(&mut self, construct_end: usize) -> Option<usize> {
        if !self.out.ends_with('\n') {
            return None;
        }
        let mut last_comment_end = None;
        loop {
            if self.comment_cursor >= self.comments.len() {
                break;
            }
            let c_start = self.comments[self.comment_cursor].span.start;
            let c_end = self.comments[self.comment_cursor].span.end;
            let c_is_block = self.comments[self.comment_cursor].is_block;
            if c_start < construct_end {
                break;
            }
            if !self.is_trailing_gap(construct_end, c_start) {
                break;
            }
            // Multi-line block comments should NOT be trailing
            if c_is_block && !self.is_same_line(c_start, c_end) {
                break;
            }
            let text = self.comments[self.comment_cursor].text.clone();
            // Rewind trailing newline
            self.trim_trailing_newline();
            self.write("  ");
            self.writeln(&text);
            last_comment_end = Some(c_end);
            self.comment_cursor += 1;
        }
        last_comment_end
    }

    // ── Top-level ──────────────────────────────────────────────

    pub fn format_program(mut self, program: &Program) -> String {
        // Module declaration
        if let Some(ref name) = program.module_name {
            self.emit_leading_comments(program.span.start);
            self.writeln(&format!("module {};", name));
        }

        // Use declarations
        for use_decl in &program.uses {
            self.emit_leading_comments(use_decl.span.start);
            self.writeln(&format!("use {};", use_decl.path.join("::")));
        }

        // Add blank line after module/use if there are declarations
        if (program.module_name.is_some() || !program.uses.is_empty())
            && !program.declarations.is_empty()
        {
            self.blank_line();
        }

        for (i, decl) in program.declarations.iter().enumerate() {
            if i > 0 {
                self.blank_line();
            }
            self.emit_declaration(decl);
            self.emit_trailing_comment_for(decl_span_end(decl));
        }

        self.emit_trailing_comments();

        // Ensure single trailing newline
        let trimmed = self.out.trim_end();
        let trimmed_len = trimmed.len();
        self.out.truncate(trimmed_len);
        self.out.push('\n');

        self.out
    }

    fn emit_declaration(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Function(f) => {
                self.emit_leading_comments(f.span.start);
                self.emit_fn_decl(f);
            }
            Declaration::Struct(s) => {
                self.emit_leading_comments(s.span.start);
                self.emit_struct_decl(s);
            }
            Declaration::Enum(e) => {
                self.emit_leading_comments(e.span.start);
                self.emit_enum_decl(e);
            }
            Declaration::Const(c) => {
                self.emit_leading_comments(c.span.start);
                self.emit_const_decl(c);
            }
            Declaration::Impl(im) => {
                self.emit_leading_comments(im.span.start);
                self.emit_impl_decl(im);
            }
            Declaration::Trait(t) => {
                self.emit_leading_comments(t.span.start);
                self.emit_trait_decl(t);
            }
        }
    }

    // ── Declarations ───────────────────────────────────────────

    fn emit_fn_decl(&mut self, f: &FnDecl) {
        self.emit_fn_signature(f);

        let has_contracts = !f.contracts.is_empty();
        if has_contracts {
            self.newline();
            self.indent_up();
            for contract in &f.contracts {
                self.emit_contract(contract);
            }
            self.indent_down();
            self.emit_block_own_line(&f.body);
        } else {
            self.write(" ");
            self.emit_block_inline(&f.body);
        }
    }

    fn emit_fn_signature(&mut self, f: &FnDecl) {
        if f.is_pub {
            self.write("pub ");
        }
        if f.is_pure {
            self.write("pure ");
        }
        self.write("fn ");
        self.write(&f.name);

        // Type params
        if !f.type_params.is_empty() {
            self.write("<");
            for (i, tp) in f.type_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&tp.name);
                if !tp.bounds.is_empty() {
                    self.write(": ");
                    self.write(&tp.bounds.join(" + "));
                }
            }
            self.write(">");
        }

        self.write("(");
        for (i, param) in f.params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&param.name);
            self.write(": ");
            self.write(&format_type(&param.ty));
        }
        self.write(")");

        // Return type (always required in Yorum)
        self.write(" -> ");
        self.write(&format_type(&f.return_type));
    }

    fn emit_contract(&mut self, contract: &Contract) {
        match contract {
            Contract::Requires(expr) => {
                self.write("requires ");
                self.emit_expr(expr);
                self.newline();
            }
            Contract::Ensures(expr) => {
                self.write("ensures ");
                self.emit_expr(expr);
                self.newline();
            }
            Contract::Effects(effects) => {
                if effects.is_empty() {
                    self.writeln("effects");
                } else {
                    self.write("effects ");
                    self.writeln(&effects.join(", "));
                }
            }
        }
    }

    fn emit_struct_decl(&mut self, s: &StructDecl) {
        if s.is_pub {
            self.write("pub ");
        }
        self.write("struct ");
        self.write(&s.name);
        if !s.type_params.is_empty() {
            self.write("<");
            for (i, tp) in s.type_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&tp.name);
            }
            self.write(">");
        }

        // Single-line for short structs (like `struct Point { x: int, y: int }`)
        if s.fields.len() <= 3 && s.fields.iter().all(|f| format_type(&f.ty).len() <= 10) {
            self.write(" { ");
            for (i, field) in s.fields.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&field.name);
                self.write(": ");
                self.write(&format_type(&field.ty));
            }
            self.write(" }");
            self.newline();
        } else {
            self.writeln(" {");
            self.indent_up();
            for (i, field) in s.fields.iter().enumerate() {
                self.write(&field.name);
                self.write(": ");
                self.write(&format_type(&field.ty));
                if i < s.fields.len() - 1 {
                    self.write(",");
                }
                self.newline();
                self.emit_trailing_comment_for(field.span.end);
            }
            self.indent_down();
            self.writeln("}");
        }
    }

    fn emit_enum_decl(&mut self, e: &EnumDecl) {
        if e.is_pub {
            self.write("pub ");
        }
        self.write("enum ");
        self.write(&e.name);
        if !e.type_params.is_empty() {
            self.write("<");
            for (i, tp) in e.type_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&tp.name);
            }
            self.write(">");
        }
        self.writeln(" {");
        self.indent_up();
        for (i, variant) in e.variants.iter().enumerate() {
            self.write(&variant.name);
            if !variant.fields.is_empty() {
                self.write("(");
                for (j, ty) in variant.fields.iter().enumerate() {
                    if j > 0 {
                        self.write(", ");
                    }
                    self.write(&format_type(ty));
                }
                self.write(")");
            }
            if i < e.variants.len() - 1 {
                self.write(",");
            }
            self.newline();
            self.emit_trailing_comment_for(variant.span.end);
        }
        self.indent_down();
        self.writeln("}");
    }

    fn emit_const_decl(&mut self, c: &ConstDecl) {
        if c.is_pub {
            self.write("pub ");
        }
        self.write("const ");
        self.write(&c.name);
        self.write(": ");
        self.write(&format_type(&c.ty));
        self.write(" = ");
        self.emit_expr(&c.value);
        self.writeln(";");
    }

    fn emit_impl_decl(&mut self, im: &ImplDecl) {
        self.write("impl ");
        if !im.type_params.is_empty() {
            self.write("<");
            for (i, tp) in im.type_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(&tp.name);
            }
            self.write("> ");
        }
        if let Some(ref trait_name) = im.trait_name {
            self.write(trait_name);
            self.write(" for ");
        }
        self.write(&im.target_type);
        self.writeln(" {");
        self.indent_up();
        for (i, method) in im.methods.iter().enumerate() {
            if i > 0 {
                self.blank_line();
            }
            self.emit_leading_comments(method.span.start);
            self.emit_fn_decl(method);
            self.emit_trailing_comment_for(method.span.end);
        }
        self.indent_down();
        self.writeln("}");
    }

    fn emit_trait_decl(&mut self, t: &TraitDecl) {
        if t.is_pub {
            self.write("pub ");
        }
        self.write("trait ");
        self.write(&t.name);
        self.writeln(" {");
        self.indent_up();
        for (i, method) in t.methods.iter().enumerate() {
            if i > 0 {
                self.blank_line();
            }
            self.write("fn ");
            self.write(&method.name);
            self.write("(");
            for (j, param) in method.params.iter().enumerate() {
                if j > 0 {
                    self.write(", ");
                }
                self.write(&param.name);
                self.write(": ");
                self.write(&format_type(&param.ty));
            }
            self.write(")");
            // Return type (always required in Yorum)
            self.write(" -> ");
            self.write(&format_type(&method.return_type));
            if let Some(ref body) = method.default_body {
                self.write(" ");
                self.emit_block_inline(body);
            } else {
                self.writeln(";");
            }
            self.emit_trailing_comment_for(method.span.end);
        }
        self.indent_down();
        self.writeln("}");
    }

    // ── Blocks ─────────────────────────────────────────────────

    fn emit_block_inline(&mut self, block: &Block) {
        self.writeln("{");
        self.indent_up();
        self.emit_block_body(block);
        self.indent_down();
        self.writeln("}");
    }

    fn emit_block_own_line(&mut self, block: &Block) {
        self.writeln("{");
        self.indent_up();
        self.emit_block_body(block);
        self.indent_down();
        self.writeln("}");
    }

    fn emit_block_body(&mut self, block: &Block) {
        let mut prev_end: Option<usize> = None;
        for stmt in &block.stmts {
            let start = self.stmt_span_start(stmt);
            // Preserve blank lines between statements, but only if there are
            // no comments between them (emit_leading_comments handles that case)
            if let Some(pe) = prev_end {
                if self.has_blank_line_between(pe, start) && !self.has_comment_between(pe, start) {
                    self.blank_line();
                }
            }
            let stmt_end = self.stmt_span_end(stmt);
            self.emit_stmt(stmt);
            let effective_end = self.emit_trailing_comment_for(stmt_end).unwrap_or(stmt_end);
            prev_end = Some(effective_end);
        }
        // Flush any comments between the last statement and the closing brace
        self.emit_leading_comments(block.span.end);
    }

    // ── Statements ─────────────────────────────────────────────

    fn emit_stmt(&mut self, stmt: &Stmt) {
        let span_start = self.stmt_span_start(stmt);
        self.emit_leading_comments(span_start);

        match stmt {
            Stmt::Let(ls) => self.emit_let_stmt(ls),
            Stmt::Assign(a) => self.emit_assign_stmt(a),
            Stmt::Return(r) => self.emit_return_stmt(r),
            Stmt::If(i) => self.emit_if_stmt(i),
            Stmt::While(w) => self.emit_while_stmt(w),
            Stmt::For(f) => self.emit_for_stmt(f),
            Stmt::Match(m) => self.emit_match_stmt(m),
            Stmt::Expr(e) => self.emit_expr_stmt(e),
            Stmt::Break(_) => self.writeln("break;"),
            Stmt::Continue(_) => self.writeln("continue;"),
        }
    }

    fn stmt_span_start(&self, stmt: &Stmt) -> usize {
        match stmt {
            Stmt::Let(s) => s.span.start,
            Stmt::Assign(s) => s.span.start,
            Stmt::Return(s) => s.span.start,
            Stmt::If(s) => s.span.start,
            Stmt::While(s) => s.span.start,
            Stmt::For(s) => s.span.start,
            Stmt::Match(s) => s.span.start,
            Stmt::Expr(s) => s.span.start,
            Stmt::Break(s) => s.span.start,
            Stmt::Continue(s) => s.span.start,
        }
    }

    fn stmt_span_end(&self, stmt: &Stmt) -> usize {
        match stmt {
            Stmt::Let(s) => s.span.end,
            Stmt::Assign(s) => s.span.end,
            Stmt::Return(s) => s.span.end,
            Stmt::If(s) => s.span.end,
            Stmt::While(s) => s.span.end,
            Stmt::For(s) => s.span.end,
            Stmt::Match(s) => s.span.end,
            Stmt::Expr(s) => s.span.end,
            Stmt::Break(s) => s.span.end,
            Stmt::Continue(s) => s.span.end,
        }
    }

    fn emit_let_stmt(&mut self, ls: &LetStmt) {
        self.write("let ");
        if ls.is_mut {
            self.write("mut ");
        }
        if let Some(ref names) = ls.destructure {
            self.write("(");
            for (i, name) in names.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(name);
            }
            self.write(")");
        } else {
            self.write(&ls.name);
        }
        self.write(": ");
        self.write(&format_type(&ls.ty));
        self.write(" = ");
        self.emit_expr(&ls.value);
        self.writeln(";");
    }

    fn emit_assign_stmt(&mut self, a: &AssignStmt) {
        // Try to detect compound assignment pattern: target = target op expr
        if let Some(op_str) = self.try_compound_op(a) {
            let op_str = op_str.to_string();
            self.emit_expr(&a.target);
            self.write(" ");
            self.write(&op_str);
            self.write(" ");
            // Extract the RHS of the binary operation
            if let ExprKind::Binary(_, _, ref rhs) = a.value.kind {
                self.emit_expr(rhs);
            }
            self.writeln(";");
        } else {
            self.emit_expr(&a.target);
            self.write(" = ");
            self.emit_expr(&a.value);
            self.writeln(";");
        }
    }

    fn emit_return_stmt(&mut self, r: &ReturnStmt) {
        // Check if returning unit (empty return)
        if matches!(r.value.kind, ExprKind::Literal(Literal::Unit)) {
            self.writeln("return;");
        } else {
            self.write("return ");
            self.emit_expr(&r.value);
            self.writeln(";");
        }
    }

    fn emit_if_stmt(&mut self, i: &IfStmt) {
        self.write("if ");
        self.emit_expr(&i.condition);
        self.write(" ");
        self.emit_block_inline(&i.then_block);

        if let Some(ref else_branch) = i.else_branch {
            // Remove trailing newline to put `else` on same line as `}`
            self.trim_trailing_newline();
            match else_branch.as_ref() {
                ElseBranch::ElseIf(elif) => {
                    self.write(" else ");
                    self.emit_if_stmt(elif);
                }
                ElseBranch::Else(block) => {
                    self.write(" else ");
                    self.emit_block_inline(block);
                }
            }
        }
    }

    fn emit_while_stmt(&mut self, w: &WhileStmt) {
        self.write("while ");
        self.emit_expr(&w.condition);
        self.write(" ");
        self.emit_block_inline(&w.body);
    }

    fn emit_for_stmt(&mut self, f: &ForStmt) {
        self.write("for ");
        self.write(&f.var_name);
        self.write(" in ");
        // Range expressions: emit as `start..end`
        if let ExprKind::Range(ref start, ref end) = f.iterable.kind {
            self.emit_expr(start);
            self.write("..");
            self.emit_expr(end);
        } else {
            self.emit_expr(&f.iterable);
        }
        self.write(" ");
        self.emit_block_inline(&f.body);
    }

    fn emit_match_stmt(&mut self, m: &MatchStmt) {
        self.write("match ");
        self.emit_expr(&m.subject);
        self.writeln(" {");
        self.indent_up();
        for arm in &m.arms {
            self.emit_match_arm(arm);
        }
        self.indent_down();
        self.writeln("}");
    }

    fn emit_match_arm(&mut self, arm: &MatchArm) {
        self.emit_pattern(&arm.pattern);
        self.write(" => ");
        self.emit_block_inline(&arm.body);
        // Remove trailing newline and add comma
        self.trim_trailing_newline();
        self.writeln(",");
    }

    fn emit_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard(_) => self.write("_"),
            Pattern::Binding(name, _) => self.write(name),
            Pattern::Literal(lit, _) => {
                let s = format_literal(lit);
                self.write(&s);
            }
            Pattern::Variant(name, sub_patterns, _) => {
                self.write(name);
                if !sub_patterns.is_empty() {
                    self.write("(");
                    for (i, p) in sub_patterns.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.emit_pattern(p);
                    }
                    self.write(")");
                }
            }
        }
    }

    fn emit_expr_stmt(&mut self, e: &ExprStmt) {
        self.emit_expr(&e.expr);
        self.writeln(";");
    }

    // ── Expressions ────────────────────────────────────────────

    fn emit_expr(&mut self, expr: &Expr) {
        // Check for string interpolation (desugared in parser)
        if self.is_interp_string_expr(expr) {
            let s = self.extract_source(&expr.span);
            self.write(&s);
            return;
        }

        match &expr.kind {
            ExprKind::Literal(lit) => {
                let s = format_literal(lit);
                self.write(&s);
            }
            ExprKind::Ident(name) => self.write(name),
            ExprKind::Binary(lhs, op, rhs) => {
                self.emit_expr_maybe_paren(lhs, *op, true);
                if *op == BinOp::And {
                    self.write(" and ");
                } else if *op == BinOp::Or {
                    self.write(" or ");
                } else {
                    self.write(&format!(" {} ", op));
                }
                self.emit_expr_maybe_paren(rhs, *op, false);
            }
            ExprKind::Unary(op, operand) => {
                match op {
                    UnaryOp::Neg => self.write("-"),
                    UnaryOp::Not => self.write("not "),
                }
                // Parenthesize binary sub-expressions for clarity
                if matches!(operand.kind, ExprKind::Binary(..)) {
                    self.write("(");
                    self.emit_expr(operand);
                    self.write(")");
                } else {
                    self.emit_expr(operand);
                }
            }
            ExprKind::Call(callee, args) => {
                self.emit_expr(callee);
                self.write("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.emit_expr(arg);
                }
                self.write(")");
            }
            ExprKind::FieldAccess(obj, field) => {
                self.emit_expr(obj);
                self.write(".");
                self.write(field);
            }
            ExprKind::MethodCall(obj, method, args) => {
                self.emit_expr(obj);
                self.write(".");
                self.write(method);
                self.write("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.emit_expr(arg);
                }
                self.write(")");
            }
            ExprKind::Index(obj, idx) => {
                self.emit_expr(obj);
                self.write("[");
                self.emit_expr(idx);
                self.write("]");
            }
            ExprKind::StructInit(name, fields) => {
                self.write(name);
                self.write(" { ");
                for (i, fi) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&fi.name);
                    self.write(": ");
                    self.emit_expr(&fi.value);
                }
                self.write(" }");
            }
            ExprKind::Closure(c) => self.emit_closure(c),
            ExprKind::ArrayLit(elems) => {
                self.write("[");
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.emit_expr(elem);
                }
                self.write("]");
            }
            ExprKind::TupleLit(elems) => {
                self.write("(");
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.emit_expr(elem);
                }
                // Trailing comma required for 1-element tuples to distinguish from grouping
                if elems.len() == 1 {
                    self.write(",");
                }
                self.write(")");
            }
            ExprKind::Spawn(block) => {
                self.write("spawn ");
                self.emit_block_inline(block);
                // Remove trailing newline since spawn is an expression
                self.trim_trailing_newline();
            }
            ExprKind::Range(start, end) => {
                self.emit_expr(start);
                self.write("..");
                self.emit_expr(end);
            }
            ExprKind::Try(inner) => {
                self.emit_expr(inner);
                self.write("?");
            }
        }
    }

    fn emit_expr_maybe_paren(&mut self, expr: &Expr, parent_op: BinOp, is_lhs: bool) {
        if let ExprKind::Binary(_, child_op, _) = &expr.kind {
            let (parent_lbp, _parent_rbp) = infix_binding_power(parent_op);
            let (child_lbp, _child_rbp) = infix_binding_power(*child_op);
            // Need parens if child has lower precedence
            let needs_paren = if is_lhs {
                child_lbp < parent_lbp
            } else {
                child_lbp <= parent_lbp
            };
            if needs_paren {
                self.write("(");
                self.emit_expr(expr);
                self.write(")");
                return;
            }
        }
        self.emit_expr(expr);
    }

    fn emit_closure(&mut self, c: &ClosureExpr) {
        self.write("|");
        for (i, param) in c.params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&param.name);
            self.write(": ");
            self.write(&format_type(&param.ty));
        }
        self.write("| -> ");
        self.write(&format_type(&c.return_type));
        self.write(" ");
        self.emit_block_inline(&c.body);
        // Remove trailing newline since closure is an expression
        self.trim_trailing_newline();
    }

    // ── Helpers ────────────────────────────────────────────────

    fn trim_trailing_newline(&mut self) {
        // Remove trailing newline and any whitespace before it (indent of closing `}`)
        while self.out.ends_with('\n') || self.out.ends_with(' ') {
            self.out.pop();
        }
        self.at_line_start = false;
    }

    fn is_interp_string_expr(&self, expr: &Expr) -> bool {
        // String interpolation is desugared to str_concat/to_str chains.
        // Detect by checking if the source span corresponds to a quoted string
        // that contains `{` (but not `{{`).
        if expr.span.start >= expr.span.end || expr.span.end > self.source.len() {
            return false;
        }
        // Check if this is a Call to str_concat or to_str with the right span pattern
        if let ExprKind::Call(callee, _) = &expr.kind {
            if let ExprKind::Ident(name) = &callee.kind {
                if name == "str_concat" || name == "to_str" {
                    // Check source starts with `"`
                    let src_start = self.source.get(expr.span.start).copied();
                    let src_end_prev = if expr.span.end > 0 {
                        self.source.get(expr.span.end - 1).copied()
                    } else {
                        None
                    };
                    if src_start == Some('"') && src_end_prev == Some('"') {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn extract_source(&self, span: &Span) -> String {
        if span.start < span.end && span.end <= self.source.len() {
            self.source[span.start..span.end].iter().collect()
        } else {
            String::new()
        }
    }

    fn try_compound_op(&self, a: &AssignStmt) -> Option<&str> {
        // Compound assignment is desugared: `x += 5` becomes `x = x + 5`
        // Detect pattern: value is Binary(target, op, rhs) where target matches a.target
        if let ExprKind::Binary(ref lhs, ref op, _) = a.value.kind {
            if exprs_structurally_equal(&a.target, lhs) {
                return match op {
                    BinOp::Add => Some("+="),
                    BinOp::Sub => Some("-="),
                    BinOp::Mul => Some("*="),
                    BinOp::Div => Some("/="),
                    BinOp::Mod => Some("%="),
                    _ => None,
                };
            }
        }
        None
    }
}

// ═══════════════════════════════════════════════════════════════
//  Free functions
// ═══════════════════════════════════════════════════════════════

fn decl_span_end(decl: &Declaration) -> usize {
    match decl {
        Declaration::Function(f) => f.span.end,
        Declaration::Struct(s) => s.span.end,
        Declaration::Enum(e) => e.span.end,
        Declaration::Const(c) => c.span.end,
        Declaration::Impl(im) => im.span.end,
        Declaration::Trait(t) => t.span.end,
    }
}

fn format_type(ty: &Type) -> String {
    // Handle singleton tuples specially — Display renders (T) but Yorum requires (T,)
    if let Type::Tuple(types) = ty {
        if types.len() == 1 {
            return format!("({},)", format_type(&types[0]));
        }
    }
    format!("{}", ty)
}

fn format_literal(lit: &Literal) -> String {
    match lit {
        Literal::Int(n) => format!("{}", n),
        Literal::Float(f) => {
            let s = format!("{}", f);
            // Ensure float always has a decimal point
            if !s.contains('.') {
                format!("{}.0", s)
            } else {
                s
            }
        }
        Literal::Bool(b) => format!("{}", b),
        Literal::Char(c) => format!("'{}'", escape_char(*c)),
        Literal::String(s) => format!("\"{}\"", escape_string(s)),
        Literal::Unit => "()".to_string(),
    }
}

fn escape_string(s: &str) -> String {
    let mut out = String::new();
    for c in s.chars() {
        match c {
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\0' => out.push_str("\\0"),
            '{' => out.push_str("{{"),
            '}' => out.push_str("}}"),
            _ => out.push(c),
        }
    }
    out
}

fn escape_char(c: char) -> String {
    match c {
        '\n' => "\\n".to_string(),
        '\t' => "\\t".to_string(),
        '\r' => "\\r".to_string(),
        '\\' => "\\\\".to_string(),
        '\'' => "\\'".to_string(),
        '\0' => "\\0".to_string(),
        _ => c.to_string(),
    }
}

fn exprs_structurally_equal(a: &Expr, b: &Expr) -> bool {
    match (&a.kind, &b.kind) {
        (ExprKind::Ident(n1), ExprKind::Ident(n2)) => n1 == n2,
        (ExprKind::FieldAccess(o1, f1), ExprKind::FieldAccess(o2, f2)) => {
            f1 == f2 && exprs_structurally_equal(o1, o2)
        }
        (ExprKind::Index(o1, i1), ExprKind::Index(o2, i2)) => {
            exprs_structurally_equal(o1, o2) && exprs_structurally_equal(i1, i2)
        }
        (ExprKind::Literal(l1), ExprKind::Literal(l2)) => match (l1, l2) {
            (Literal::Int(a), Literal::Int(b)) => a == b,
            (Literal::Bool(a), Literal::Bool(b)) => a == b,
            (Literal::String(a), Literal::String(b)) => a == b,
            _ => false,
        },
        _ => false,
    }
}

fn infix_binding_power(op: BinOp) -> (u8, u8) {
    match op {
        BinOp::Or => (3, 4),
        BinOp::And => (5, 6),
        BinOp::BitOr => (7, 8),
        BinOp::BitXor => (9, 10),
        BinOp::BitAnd => (11, 12),
        BinOp::Eq | BinOp::NotEq => (13, 14),
        BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => (15, 16),
        BinOp::Shl | BinOp::Shr => (17, 18),
        BinOp::Add | BinOp::Sub => (19, 20),
        BinOp::Mul | BinOp::Div | BinOp::Mod => (21, 22),
    }
}
