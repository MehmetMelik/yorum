use std::collections::HashMap;
use std::io::{self, Write};

use crate::lsp::transport;
use crate::lsp::types::*;

pub struct LspServer {
    documents: HashMap<String, String>,
    initialized: bool,
    shutdown_requested: bool,
}

impl Default for LspServer {
    fn default() -> Self {
        Self::new()
    }
}

impl LspServer {
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            initialized: false,
            shutdown_requested: false,
        }
    }

    /// Run the LSP server on stdin/stdout.
    pub fn run(&mut self) -> io::Result<()> {
        let stdin = io::stdin();
        let stdout = io::stdout();
        let mut reader = stdin.lock();
        let mut writer = stdout.lock();

        while let Some(msg) = transport::read_message(&mut reader)? {
            let parsed: JsonRpcMessage = match serde_json::from_str(&msg) {
                Ok(m) => m,
                Err(_) => {
                    self.send_error(
                        &mut writer,
                        serde_json::Value::Null,
                        PARSE_ERROR,
                        "Parse error",
                    )?;
                    continue;
                }
            };

            let method = match parsed.method.as_deref() {
                Some(m) => m.to_string(),
                None => continue,
            };

            match method.as_str() {
                "initialize" => {
                    self.handle_initialize(&mut writer, &parsed)?;
                }
                "initialized" => {
                    // No-op notification
                }
                "shutdown" => {
                    self.shutdown_requested = true;
                    if let Some(id) = parsed.id {
                        self.send_response(&mut writer, id, serde_json::Value::Null)?;
                    }
                }
                "exit" => {
                    let code = if self.shutdown_requested { 0 } else { 1 };
                    std::process::exit(code);
                }
                "textDocument/didOpen" => {
                    self.handle_did_open(&mut writer, &parsed)?;
                }
                "textDocument/didChange" => {
                    self.handle_did_change(&mut writer, &parsed)?;
                }
                "textDocument/didClose" => {
                    self.handle_did_close(&parsed);
                }
                "textDocument/hover" => {
                    self.handle_hover(&mut writer, &parsed)?;
                }
                "textDocument/definition" => {
                    self.handle_definition(&mut writer, &parsed)?;
                }
                "textDocument/completion" => {
                    self.handle_completion(&mut writer, &parsed)?;
                }
                "textDocument/codeAction" => {
                    self.handle_code_action(&mut writer, &parsed)?;
                }
                _ => {
                    // Unknown request → MethodNotFound (only for requests, not notifications)
                    if let Some(id) = parsed.id {
                        self.send_error(
                            &mut writer,
                            id,
                            METHOD_NOT_FOUND,
                            &format!("Method not found: {}", method),
                        )?;
                    }
                }
            }
        }

        Ok(())
    }

    fn handle_initialize(
        &mut self,
        writer: &mut impl Write,
        msg: &JsonRpcMessage,
    ) -> io::Result<()> {
        self.initialized = true;
        let result = InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: 1, // Full sync
                hover_provider: Some(true),
                definition_provider: Some(true),
                position_encoding: Some("utf-16".to_string()),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: vec![".".to_string()],
                }),
                code_action_provider: Some(true),
            },
            server_info: ServerInfo {
                name: "yorum-lsp".to_string(),
                version: env!("CARGO_PKG_VERSION").to_string(),
            },
        };
        if let Some(id) = &msg.id {
            self.send_response(
                writer,
                id.clone(),
                serde_json::to_value(result).unwrap_or(serde_json::Value::Null),
            )?;
        }
        Ok(())
    }

    fn handle_did_open(&mut self, writer: &mut impl Write, msg: &JsonRpcMessage) -> io::Result<()> {
        if let Some(params) = &msg.params {
            if let Ok(p) = serde_json::from_value::<DidOpenTextDocumentParams>(params.clone()) {
                let uri = p.text_document.uri.clone();
                self.documents.insert(uri.clone(), p.text_document.text);
                self.publish_diagnostics(writer, &uri)?;
            }
        }
        Ok(())
    }

    fn handle_did_change(
        &mut self,
        writer: &mut impl Write,
        msg: &JsonRpcMessage,
    ) -> io::Result<()> {
        if let Some(params) = &msg.params {
            if let Ok(p) = serde_json::from_value::<DidChangeTextDocumentParams>(params.clone()) {
                let uri = p.text_document.uri.clone();
                if let Some(change) = p.content_changes.into_iter().last() {
                    self.documents.insert(uri.clone(), change.text);
                    self.publish_diagnostics(writer, &uri)?;
                }
            }
        }
        Ok(())
    }

    fn handle_did_close(&mut self, msg: &JsonRpcMessage) {
        if let Some(params) = &msg.params {
            if let Ok(p) = serde_json::from_value::<DidCloseTextDocumentParams>(params.clone()) {
                self.documents.remove(&p.text_document.uri);
            }
        }
    }

    fn handle_hover(&mut self, writer: &mut impl Write, msg: &JsonRpcMessage) -> io::Result<()> {
        let id = match &msg.id {
            Some(id) => id.clone(),
            None => return Ok(()),
        };

        let params = match &msg.params {
            Some(p) => p,
            None => {
                self.send_response(writer, id, serde_json::Value::Null)?;
                return Ok(());
            }
        };

        let p: TextDocumentPositionParams = match serde_json::from_value(params.clone()) {
            Ok(p) => p,
            Err(_) => {
                self.send_response(writer, id, serde_json::Value::Null)?;
                return Ok(());
            }
        };

        let source = match self.documents.get(&p.text_document.uri) {
            Some(s) => s.clone(),
            None => {
                self.send_response(writer, id, serde_json::Value::Null)?;
                return Ok(());
            }
        };

        let offset = position_to_byte_offset(&source, &p.position);
        let (_, symbols) = crate::check_with_symbols(&source);

        let hover = symbols.and_then(|sym| {
            // First check references (identifiers)
            for r in &sym.references {
                if r.span.start <= offset && offset < r.span.end {
                    return Some(Hover {
                        contents: MarkupContent {
                            kind: "markdown".to_string(),
                            value: format!("```yorum\n{}: {}\n```", r.def_name, r.resolved_type),
                        },
                        range: Some(span_to_range(&r.span, &source)),
                    });
                }
            }
            // Then check definitions
            for d in &sym.definitions {
                if d.span.start <= offset && offset < d.span.end {
                    return Some(Hover {
                        contents: MarkupContent {
                            kind: "markdown".to_string(),
                            value: format!("```yorum\n{}\n```", d.type_desc),
                        },
                        range: Some(span_to_range(&d.span, &source)),
                    });
                }
            }
            None
        });

        let result = match hover {
            Some(h) => serde_json::to_value(h).unwrap_or(serde_json::Value::Null),
            None => serde_json::Value::Null,
        };
        self.send_response(writer, id, result)
    }

    fn handle_definition(
        &mut self,
        writer: &mut impl Write,
        msg: &JsonRpcMessage,
    ) -> io::Result<()> {
        let id = match &msg.id {
            Some(id) => id.clone(),
            None => return Ok(()),
        };

        let params = match &msg.params {
            Some(p) => p,
            None => {
                self.send_response(writer, id, serde_json::Value::Null)?;
                return Ok(());
            }
        };

        let p: TextDocumentPositionParams = match serde_json::from_value(params.clone()) {
            Ok(p) => p,
            Err(_) => {
                self.send_response(writer, id, serde_json::Value::Null)?;
                return Ok(());
            }
        };

        let source = match self.documents.get(&p.text_document.uri) {
            Some(s) => s.clone(),
            None => {
                self.send_response(writer, id, serde_json::Value::Null)?;
                return Ok(());
            }
        };

        let offset = position_to_byte_offset(&source, &p.position);
        let (_, symbols) = crate::check_with_symbols(&source);

        let location = symbols.and_then(|sym| {
            for r in &sym.references {
                if r.span.start <= offset && offset < r.span.end {
                    if let Some(def_span) = &r.def_span {
                        // Skip synthetic spans (builtins)
                        if def_span.line == 0 {
                            return None;
                        }
                        return Some(Location {
                            uri: p.text_document.uri.clone(),
                            range: span_to_range(def_span, &source),
                        });
                    }
                }
            }
            None
        });

        let result = match location {
            Some(loc) => serde_json::to_value(loc).unwrap_or(serde_json::Value::Null),
            None => serde_json::Value::Null,
        };
        self.send_response(writer, id, result)
    }

    fn handle_completion(
        &mut self,
        writer: &mut impl Write,
        msg: &JsonRpcMessage,
    ) -> io::Result<()> {
        let id = match &msg.id {
            Some(id) => id.clone(),
            None => return Ok(()),
        };

        let params = match &msg.params {
            Some(p) => p,
            None => {
                self.send_response(writer, id, serde_json::Value::Null)?;
                return Ok(());
            }
        };

        let p: CompletionParams = match serde_json::from_value(params.clone()) {
            Ok(p) => p,
            Err(_) => {
                self.send_response(writer, id, serde_json::Value::Null)?;
                return Ok(());
            }
        };

        let source = match self.documents.get(&p.text_document.uri) {
            Some(s) => s.clone(),
            None => {
                self.send_response(writer, id, serde_json::Value::Null)?;
                return Ok(());
            }
        };

        let line_text = get_line_text(&source, p.position.line, p.position.character);
        let items = if line_text.ends_with('.') {
            self.dot_completions(&source, &line_text)
        } else {
            let prefix = extract_prefix(&line_text);
            self.prefix_completions(&source, &prefix)
        };

        let list = CompletionList {
            is_incomplete: false,
            items,
        };
        let result = serde_json::to_value(list).unwrap_or(serde_json::Value::Null);
        self.send_response(writer, id, result)
    }

    fn dot_completions(&self, source: &str, line_text: &str) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        let ident = extract_last_ident(line_text);
        if ident.is_empty() {
            return items;
        }

        let (_, symbols) = crate::check_with_symbols(source);
        if let Some(sym) = symbols {
            // Resolve the type of the identifier
            let resolved_type = resolve_ident_type(&ident, &sym);

            if let Some(ref type_str) = resolved_type {
                // Struct fields
                if let Some(struct_name) = extract_struct_name(type_str) {
                    for sd in &sym.definitions {
                        if sd.name == struct_name
                            && matches!(sd.kind, crate::compiler::typechecker::SymbolKind::Struct)
                        {
                            for field in extract_fields_from_struct_desc(&sd.type_desc) {
                                items.push(CompletionItem {
                                    label: field.0.clone(),
                                    kind: COMPLETION_KIND_FIELD,
                                    detail: Some(field.1.clone()),
                                    insert_text: None,
                                });
                            }
                            // User-defined struct methods (mangled as StructName_method)
                            let prefix = format!("{}_", struct_name);
                            for fd in &sym.definitions {
                                if matches!(
                                    fd.kind,
                                    crate::compiler::typechecker::SymbolKind::Function
                                ) {
                                    if let Some(method_name) = fd.name.strip_prefix(&prefix) {
                                        items.push(CompletionItem {
                                            label: method_name.to_string(),
                                            kind: COMPLETION_KIND_FUNCTION,
                                            detail: Some(fd.type_desc.clone()),
                                            insert_text: Some(format!("{}()", method_name)),
                                        });
                                    }
                                }
                            }
                        }
                    }
                }

                // Type-based method completions
                for (name, sig) in methods_for_type(type_str) {
                    items.push(CompletionItem {
                        label: name.to_string(),
                        kind: COMPLETION_KIND_FUNCTION,
                        detail: Some(sig.to_string()),
                        insert_text: Some(format!("{}()", name)),
                    });
                }
            }
        }

        items
    }

    fn prefix_completions(&self, source: &str, prefix: &str) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        if prefix.is_empty() {
            return items;
        }

        let mut seen = std::collections::HashSet::new();

        // Symbols from source
        let (_, symbols) = crate::check_with_symbols(source);
        if let Some(sym) = symbols {
            for d in &sym.definitions {
                if d.name.starts_with(prefix) && seen.insert(d.name.clone()) {
                    let kind = match d.kind {
                        crate::compiler::typechecker::SymbolKind::Function => {
                            COMPLETION_KIND_FUNCTION
                        }
                        crate::compiler::typechecker::SymbolKind::Struct => COMPLETION_KIND_STRUCT,
                        crate::compiler::typechecker::SymbolKind::Enum => {
                            COMPLETION_KIND_ENUM_MEMBER
                        }
                        _ => COMPLETION_KIND_VARIABLE,
                    };
                    items.push(CompletionItem {
                        label: d.name.clone(),
                        kind,
                        detail: Some(d.type_desc.clone()),
                        insert_text: None,
                    });
                }
            }
        }

        // Builtins
        for (name, sig) in crate::builtin_function_list() {
            if name.starts_with(prefix) && seen.insert(name.clone()) {
                items.push(CompletionItem {
                    label: name,
                    kind: COMPLETION_KIND_FUNCTION,
                    detail: Some(sig),
                    insert_text: None,
                });
            }
        }

        // Keywords
        let keywords = [
            "fn", "let", "mut", "if", "else", "while", "for", "in", "return", "struct", "enum",
            "match", "true", "false", "and", "or", "not", "pure", "pub", "module", "use", "spawn",
            "break", "continue", "impl", "trait", "effects", "requires", "ensures",
        ];
        for kw in &keywords {
            if kw.starts_with(prefix) && seen.insert(kw.to_string()) {
                items.push(CompletionItem {
                    label: kw.to_string(),
                    kind: COMPLETION_KIND_KEYWORD,
                    detail: Some("keyword".to_string()),
                    insert_text: None,
                });
            }
        }

        items
    }

    fn handle_code_action(
        &mut self,
        writer: &mut impl Write,
        msg: &JsonRpcMessage,
    ) -> io::Result<()> {
        let id = match &msg.id {
            Some(id) => id.clone(),
            None => return Ok(()),
        };

        let params = match &msg.params {
            Some(p) => p,
            None => {
                self.send_response(writer, id, serde_json::json!([]))?;
                return Ok(());
            }
        };

        let p: CodeActionParams = match serde_json::from_value(params.clone()) {
            Ok(p) => p,
            Err(_) => {
                self.send_response(writer, id, serde_json::json!([]))?;
                return Ok(());
            }
        };

        let source = match self.documents.get(&p.text_document.uri) {
            Some(s) => s.clone(),
            None => {
                self.send_response(writer, id, serde_json::json!([]))?;
                return Ok(());
            }
        };

        let mut actions: Vec<CodeAction> = Vec::new();

        for diag in &p.context.diagnostics {
            let msg_text = &diag.message;

            // "Did you mean X?" for undefined variable/function
            if msg_text.contains("undefined variable")
                || msg_text.contains("undefined function")
                || msg_text.contains("unknown function")
            {
                if let Some(unknown_name) = extract_quoted_name(msg_text) {
                    // Collect all known names
                    let mut candidates: Vec<String> = Vec::new();
                    let (_, symbols) = crate::check_with_symbols(&source);
                    if let Some(sym) = symbols {
                        for d in &sym.definitions {
                            candidates.push(d.name.clone());
                        }
                    }
                    for (name, _) in crate::builtin_function_list() {
                        candidates.push(name);
                    }

                    if let Some(closest) = find_closest_name(&unknown_name, &candidates) {
                        let mut changes = HashMap::new();
                        changes.insert(
                            p.text_document.uri.clone(),
                            vec![TextEdit {
                                range: Range {
                                    start: diag.range.start.clone(),
                                    end: diag.range.end.clone(),
                                },
                                new_text: closest.clone(),
                            }],
                        );
                        actions.push(CodeAction {
                            title: format!("Did you mean '{}'?", closest),
                            kind: "quickfix".to_string(),
                            edit: Some(WorkspaceEdit { changes }),
                            is_preferred: Some(true),
                        });
                    }
                }
            }

            // Effect violation hint
            if msg_text.contains("effect") && msg_text.contains("not allowed") {
                if let Some(effect) = extract_effect_from_message(msg_text) {
                    actions.push(CodeAction {
                        title: format!("Add 'effects {}' clause to function", effect),
                        kind: "quickfix".to_string(),
                        edit: None,
                        is_preferred: None,
                    });
                }
            }

            // Non-exhaustive match hint
            if (msg_text.contains("non-exhaustive") || msg_text.contains("missing"))
                && (msg_text.contains("match") || msg_text.contains("variant"))
            {
                actions.push(CodeAction {
                    title: "Add missing match arms".to_string(),
                    kind: "quickfix".to_string(),
                    edit: None,
                    is_preferred: None,
                });
            }
        }

        let result = serde_json::to_value(&actions).unwrap_or(serde_json::json!([]));
        self.send_response(writer, id, result)
    }

    fn publish_diagnostics(&self, writer: &mut impl Write, uri: &str) -> io::Result<()> {
        let source = match self.documents.get(uri) {
            Some(s) => s,
            None => return Ok(()),
        };

        let diags = crate::check_diagnostics(source);
        let lsp_diags: Vec<LspDiagnostic> = diags
            .iter()
            .map(|d| LspDiagnostic {
                range: span_to_range(&d.span, source),
                severity: match d.severity {
                    crate::DiagnosticSeverity::Error => SEVERITY_ERROR,
                    crate::DiagnosticSeverity::Warning => SEVERITY_WARNING,
                },
                source: "yorum".to_string(),
                message: d.message.clone(),
            })
            .collect();

        let params = PublishDiagnosticsParams {
            uri: uri.to_string(),
            diagnostics: lsp_diags,
        };

        let value = match serde_json::to_value(params) {
            Ok(v) => v,
            Err(_) => return Ok(()),
        };
        self.send_notification(writer, "textDocument/publishDiagnostics", value)
    }

    fn send_response(
        &self,
        writer: &mut impl Write,
        id: serde_json::Value,
        result: serde_json::Value,
    ) -> io::Result<()> {
        let resp = if result.is_null() {
            // null result (not an error)
            JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id,
                result: Some(serde_json::Value::Null),
                error: None,
            }
        } else {
            JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id,
                result: Some(result),
                error: None,
            }
        };
        let body = match serde_json::to_string(&resp) {
            Ok(b) => b,
            Err(_) => return Ok(()),
        };
        transport::write_message(writer, &body)
    }

    fn send_error(
        &self,
        writer: &mut impl Write,
        id: serde_json::Value,
        code: i64,
        message: &str,
    ) -> io::Result<()> {
        let resp = JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: None,
            error: Some(JsonRpcError {
                code,
                message: message.to_string(),
            }),
        };
        let body = match serde_json::to_string(&resp) {
            Ok(b) => b,
            Err(_) => return Ok(()),
        };
        transport::write_message(writer, &body)
    }

    fn send_notification(
        &self,
        writer: &mut impl Write,
        method: &str,
        params: serde_json::Value,
    ) -> io::Result<()> {
        let notif = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: method.to_string(),
            params,
        };
        let body = match serde_json::to_string(&notif) {
            Ok(b) => b,
            Err(_) => return Ok(()),
        };
        transport::write_message(writer, &body)
    }
}

/// Get the text of a specific line up to the cursor position.
fn get_line_text(source: &str, line: u32, character: u32) -> String {
    let lines: Vec<&str> = source.lines().collect();
    if (line as usize) < lines.len() {
        let l = lines[line as usize];
        let end = (character as usize).min(l.len());
        l[..end].to_string()
    } else {
        String::new()
    }
}

/// Extract the word prefix at the end of a line for completion.
pub fn extract_prefix(line: &str) -> String {
    // If line ends with a non-identifier char, there's no prefix
    if line
        .chars()
        .last()
        .map(|c| !c.is_alphanumeric() && c != '_')
        .unwrap_or(true)
    {
        return String::new();
    }
    if let Some(pos) = line.rfind(|c: char| !c.is_alphanumeric() && c != '_') {
        line[pos + 1..].to_string()
    } else {
        line.to_string()
    }
}

/// Extract the last identifier before a dot.
pub fn extract_last_ident(line: &str) -> String {
    let trimmed = line.trim_end_matches('.');
    if let Some(pos) = trimmed.rfind(|c: char| !c.is_alphanumeric() && c != '_') {
        trimmed[pos + 1..].to_string()
    } else {
        trimmed.to_string()
    }
}

/// Extract a struct name from a type description like "Point" or "let x: Point".
fn extract_struct_name(type_desc: &str) -> Option<String> {
    let name = type_desc.trim();
    // Skip basic types
    if matches!(
        name,
        "int" | "float" | "bool" | "char" | "string" | "unit" | ""
    ) {
        return None;
    }
    // If it starts with uppercase, likely a struct/enum name
    if name
        .chars()
        .next()
        .map(|c| c.is_uppercase())
        .unwrap_or(false)
    {
        Some(name.split('<').next().unwrap_or(name).to_string())
    } else {
        None
    }
}

/// Parse struct fields from a type_desc like "struct Point { x: int, y: int }".
fn extract_fields_from_struct_desc(type_desc: &str) -> Vec<(String, String)> {
    let mut fields = Vec::new();
    if let Some(start) = type_desc.find('{') {
        if let Some(end) = type_desc.rfind('}') {
            let body = &type_desc[start + 1..end];
            for field in body.split(',') {
                let field = field.trim();
                if let Some(colon) = field.find(':') {
                    let name = field[..colon].trim().to_string();
                    let ty = field[colon + 1..].trim().to_string();
                    if !name.is_empty() {
                        fields.push((name, ty));
                    }
                }
            }
        }
    }
    fields
}

/// Resolve the type of an identifier from the symbol table.
/// Checks definitions first (extracts type after ": "), then references.
fn resolve_ident_type(
    ident: &str,
    sym: &crate::compiler::typechecker::SymbolTable,
) -> Option<String> {
    // Check definitions: type_desc is like "let x: Type" or "x: Type" or "for x: Type"
    for d in &sym.definitions {
        if d.name == ident
            && matches!(
                d.kind,
                crate::compiler::typechecker::SymbolKind::Variable
                    | crate::compiler::typechecker::SymbolKind::Parameter
            )
        {
            if let Some(colon_pos) = d.type_desc.rfind(": ") {
                return Some(d.type_desc[colon_pos + 2..].to_string());
            }
        }
    }
    // Check references
    for r in &sym.references {
        if r.def_name == ident {
            return Some(r.resolved_type.clone());
        }
    }
    None
}

/// Return method completions appropriate for a given type string.
fn methods_for_type(type_str: &str) -> Vec<(&'static str, &'static str)> {
    if type_str.starts_with("Option<") {
        vec![
            ("unwrap", "() -> T"),
            ("is_some", "() -> bool"),
            ("is_none", "() -> bool"),
        ]
    } else if type_str.starts_with("Result<") {
        vec![
            ("unwrap", "() -> T"),
            ("unwrap_err", "() -> E"),
            ("is_ok", "() -> bool"),
            ("is_err", "() -> bool"),
        ]
    } else if type_str.starts_with("Task<") {
        vec![("join", "() -> T")]
    } else if type_str.starts_with('[') {
        vec![
            ("len", "() -> int"),
            ("push", "(elem) -> unit"),
            ("pop", "() -> T"),
            ("iter", "() -> Iterator"),
            ("sort_int", "() -> [int]"),
            ("contains", "(elem) -> bool"),
            ("clear", "() -> unit"),
        ]
    } else if type_str.starts_with("Map<") {
        vec![
            ("map_get", "(key) -> V"),
            ("map_set", "(key, val) -> unit"),
            ("map_has", "(key) -> bool"),
            ("map_remove", "(key) -> unit"),
            ("map_size", "() -> int"),
            ("map_keys", "() -> [K]"),
            ("map_values", "() -> [V]"),
            ("iter", "() -> Iterator<(K, V)>"),
        ]
    } else if type_str.starts_with("Set<") {
        vec![
            ("set_add", "(elem) -> unit"),
            ("set_has", "(elem) -> bool"),
            ("set_remove", "(elem) -> unit"),
            ("set_size", "() -> int"),
            ("set_values", "() -> [T]"),
            ("iter", "() -> Iterator<T>"),
        ]
    } else if type_str == "string" {
        vec![("len", "() -> int"), ("chars", "() -> Iterator<char>")]
    } else {
        vec![]
    }
}

/// Extract a quoted name from an error message like "undefined variable 'foo'".
pub fn extract_quoted_name(msg: &str) -> Option<String> {
    // Try single quotes first
    if let Some(start) = msg.find('\'') {
        if let Some(end) = msg[start + 1..].find('\'') {
            return Some(msg[start + 1..start + 1 + end].to_string());
        }
    }
    // Try backticks
    if let Some(start) = msg.find('`') {
        if let Some(end) = msg[start + 1..].find('`') {
            return Some(msg[start + 1..start + 1 + end].to_string());
        }
    }
    None
}

/// Compute Levenshtein distance between two strings.
pub fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_bytes = a.as_bytes();
    let b_bytes = b.as_bytes();
    let b_len = b_bytes.len();
    let mut prev: Vec<usize> = (0..=b_len).collect();
    let mut curr = vec![0; b_len + 1];
    for (i, &a_ch) in a_bytes.iter().enumerate() {
        curr[0] = i + 1;
        for (j, &b_ch) in b_bytes.iter().enumerate() {
            let cost = if a_ch == b_ch { 0 } else { 1 };
            curr[j + 1] = (prev[j + 1] + 1).min(curr[j] + 1).min(prev[j] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[b_len]
}

/// Find the closest name from candidates using Levenshtein distance.
pub fn find_closest_name(target: &str, candidates: &[String]) -> Option<String> {
    let mut best: Option<(String, usize)> = None;
    for c in candidates {
        let dist = levenshtein_distance(target, c);
        if dist <= 2 && best.as_ref().map(|(_, d)| dist < *d).unwrap_or(true) {
            best = Some((c.clone(), dist));
        }
    }
    best.map(|(name, _)| name)
}

/// Extract effect name from an error message like "effect 'io' not allowed".
fn extract_effect_from_message(msg: &str) -> Option<String> {
    // Try to find quoted effect name
    if let Some(name) = extract_quoted_name(msg) {
        let valid = ["io", "fs", "net", "time", "env", "concurrency"];
        if valid.contains(&name.as_str()) {
            return Some(name);
        }
    }
    None
}

/// Convert an LSP Position (0-based line/character) to a byte offset in the source.
fn position_to_byte_offset(source: &str, pos: &Position) -> usize {
    let mut line = 0u32;
    let mut offset = 0usize;
    for (i, ch) in source.char_indices() {
        if line == pos.line {
            let col = i - offset;
            if col as u32 == pos.character {
                return i;
            }
        }
        if ch == '\n' {
            line += 1;
            offset = i + 1;
        }
    }
    // If we're past the end, return source length
    source.len()
}

/// Convert a compiler Span to an LSP Range.
///
/// The Span has start/end byte offsets and start line/col (1-based).
/// We compute the end line/col by walking bytes from start to end.
fn span_to_range(span: &crate::compiler::span::Span, source: &str) -> Range {
    let start_line = if span.line > 0 { span.line - 1 } else { 0 };
    let start_col = if span.col > 0 { span.col - 1 } else { 0 };

    // Compute end line/col by walking from span.start to span.end
    let mut end_line = start_line;
    let mut end_col = start_col;

    let start = span.start.min(source.len());
    let end = span.end.min(source.len());

    for &b in &source.as_bytes()[start..end] {
        if b == b'\n' {
            end_line += 1;
            end_col = 0;
        } else {
            end_col += 1;
        }
    }

    Range {
        start: Position {
            line: start_line,
            character: start_col,
        },
        end: Position {
            line: end_line,
            character: end_col,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_to_byte_offset() {
        let source = "fn main() {\n    return 0;\n}\n";
        assert_eq!(
            position_to_byte_offset(
                source,
                &Position {
                    line: 0,
                    character: 0
                }
            ),
            0
        );
        assert_eq!(
            position_to_byte_offset(
                source,
                &Position {
                    line: 0,
                    character: 3
                }
            ),
            3
        );
        assert_eq!(
            position_to_byte_offset(
                source,
                &Position {
                    line: 1,
                    character: 0
                }
            ),
            12
        );
        assert_eq!(
            position_to_byte_offset(
                source,
                &Position {
                    line: 1,
                    character: 4
                }
            ),
            16
        );
    }

    #[test]
    fn test_span_to_range() {
        let source = "fn main() {\n    return 0;\n}\n";
        let span = crate::compiler::span::Span::new(3, 7, 1, 4); // "main"
        let range = span_to_range(&span, source);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 3);
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, 7);
    }

    #[test]
    fn test_span_to_range_multiline() {
        let source = "hello\nworld\n";
        let span = crate::compiler::span::Span::new(0, 11, 1, 1);
        let range = span_to_range(&span, source);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 1);
        assert_eq!(range.end.character, 5);
    }

    #[test]
    fn test_initialize_handshake() {
        let mut server = LspServer::new();
        let mut output = Vec::new();

        // Simulate initialize request
        let init_msg = JsonRpcMessage {
            jsonrpc: "2.0".to_string(),
            id: Some(serde_json::json!(1)),
            method: Some("initialize".to_string()),
            params: Some(serde_json::json!({"rootUri": null})),
        };

        server.handle_initialize(&mut output, &init_msg).unwrap();
        assert!(server.initialized);

        let output_str = String::from_utf8(output).unwrap();
        // Strip Content-Length header
        let body_start = output_str.find('{').unwrap();
        let body = &output_str[body_start..];
        let resp: serde_json::Value = serde_json::from_str(body).unwrap();

        assert_eq!(resp["jsonrpc"], "2.0");
        assert_eq!(resp["id"], 1);
        assert_eq!(resp["result"]["capabilities"]["textDocumentSync"], 1);
        assert_eq!(resp["result"]["capabilities"]["hoverProvider"], true);
        assert_eq!(resp["result"]["capabilities"]["definitionProvider"], true);
        assert_eq!(resp["result"]["serverInfo"]["name"], "yorum-lsp");
    }
}
