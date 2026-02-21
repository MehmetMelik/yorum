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

        let (_, symbols) = crate::check_with_symbols(source);
        let sym = match symbols {
            Some(s) => s,
            None => return items,
        };

        // Try chain-aware completion first
        if let Some(chain_text) = extract_chain_text(line_text) {
            if let Some(chain_expr) = parse_chain_expr(&chain_text) {
                if let Some(chain_type) = walk_chain_type(&chain_expr, &sym) {
                    if chain_type.is_iterator {
                        for (name, sig) in iterator_methods(&chain_type.elem_type) {
                            items.push(CompletionItem {
                                label: name.to_string(),
                                kind: COMPLETION_KIND_FUNCTION,
                                detail: Some(sig),
                                insert_text: Some(format!("{}()", name)),
                            });
                        }
                        return items;
                    } else {
                        // Non-iterator chain result — use methods_for_type
                        let type_str = &chain_type.elem_type;
                        if let Some(struct_name) = extract_struct_name(type_str) {
                            for sd in &sym.definitions {
                                if sd.name == struct_name
                                    && matches!(
                                        sd.kind,
                                        crate::compiler::typechecker::SymbolKind::Struct
                                    )
                                {
                                    for field in extract_fields_from_struct_desc(&sd.type_desc) {
                                        items.push(CompletionItem {
                                            label: field.0.clone(),
                                            kind: COMPLETION_KIND_FIELD,
                                            detail: Some(field.1.clone()),
                                            insert_text: None,
                                        });
                                    }
                                }
                            }
                        }
                        for (name, sig) in methods_for_type(type_str) {
                            items.push(CompletionItem {
                                label: name.to_string(),
                                kind: COMPLETION_KIND_FUNCTION,
                                detail: Some(sig.to_string()),
                                insert_text: Some(format!("{}()", name)),
                            });
                        }
                        return items;
                    }
                }
            }
        }

        // Fall back to existing simple-ident logic
        let ident = extract_last_ident(line_text);
        if ident.is_empty() {
            return items;
        }

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
                            if matches!(fd.kind, crate::compiler::typechecker::SymbolKind::Function)
                            {
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

/// Extract a method-chain expression ending at the trailing dot.
/// Returns None if this is a simple `ident.` (no parens before the dot).
fn extract_chain_text(line: &str) -> Option<String> {
    let trimmed = line.trim_end_matches('.');
    // Quick check: if no '(' in the trimmed text, it's not a chain
    if !trimmed.contains('(') {
        return None;
    }
    // Scan backwards from end, tracking bracket depth.
    // Skip characters inside string and char literals to avoid
    // corrupting depth counts on literal delimiters like '(' in strings.
    let chars: Vec<char> = trimmed.chars().collect();
    let mut depth_paren = 0i32;
    let mut depth_brace = 0i32;
    let mut depth_bracket = 0i32;
    let mut start = 0;
    let mut i = chars.len();
    while i > 0 {
        i -= 1;
        // Skip char literals: scan backwards past 'x' or '\n' sequences
        if chars[i] == '\'' && i >= 2 {
            // Could be 'x' (len 3) or '\n' (len 4)
            if chars[i - 2] == '\'' {
                i -= 2;
                continue;
            } else if i >= 3 && chars[i - 3] == '\'' && chars[i - 2] == '\\' {
                i -= 3;
                continue;
            }
        }
        // Skip string literals: scan backwards to the opening quote
        if chars[i] == '"' {
            if i > 0 {
                i -= 1;
                while i > 0 {
                    if chars[i] == '"' && (i == 0 || chars[i - 1] != '\\') {
                        break;
                    }
                    i -= 1;
                }
            }
            continue;
        }
        match chars[i] {
            ')' => depth_paren += 1,
            '(' => {
                depth_paren -= 1;
                if depth_paren < 0 {
                    start = i + 1;
                    break;
                }
            }
            '}' => depth_brace += 1,
            '{' => {
                depth_brace -= 1;
                if depth_brace < 0 {
                    start = i + 1;
                    break;
                }
            }
            ']' => depth_bracket += 1,
            '[' => {
                depth_bracket -= 1;
                if depth_bracket < 0 {
                    start = i + 1;
                    break;
                }
            }
            _ => {
                if depth_paren == 0 && depth_brace == 0 && depth_bracket == 0 {
                    // At top level — check for chain-start boundaries
                    if !chars[i].is_alphanumeric()
                        && chars[i] != '_'
                        && chars[i] != '.'
                        && chars[i] != ')'
                        && chars[i] != '}'
                        && chars[i] != ']'
                    {
                        start = i + 1;
                        break;
                    }
                }
            }
        }
    }
    let chain = trimmed[start..].trim();
    if chain.is_empty() {
        None
    } else {
        Some(chain.to_string())
    }
}

/// Parse a chain expression using the real parser.
fn parse_chain_expr(chain_text: &str) -> Option<crate::compiler::ast::Expr> {
    let synthetic = format!("fn __lsp() -> unit {{ {}; }}", chain_text);
    let mut lexer = crate::compiler::lexer::Lexer::new(&synthetic);
    let tokens = lexer.tokenize().ok()?;
    let mut parser = crate::compiler::parser::Parser::new(tokens);
    let program = parser.parse_program().ok()?;
    // Extract the expression from: fn __lsp() { <expr>; }
    let decl = program.declarations.first()?;
    if let crate::compiler::ast::Declaration::Function(f) = decl {
        if let Some(crate::compiler::ast::Stmt::Expr(ref expr_stmt)) = f.body.stmts.first() {
            return Some(expr_stmt.expr.clone());
        }
    }
    None
}

struct ChainType {
    /// The element type as a display string (e.g., "int", "string", "(int, string)")
    elem_type: String,
    /// Whether we're in iterator context (after .iter()/.chars())
    is_iterator: bool,
}

/// Walk a method-chain AST, propagating element types through iterator pipeline steps.
fn walk_chain_type(
    expr: &crate::compiler::ast::Expr,
    sym: &crate::compiler::typechecker::SymbolTable,
) -> Option<ChainType> {
    use crate::compiler::ast::ExprKind;

    match &expr.kind {
        ExprKind::Ident(name) => {
            let type_str = resolve_ident_type(name, sym)?;
            Some(ChainType {
                elem_type: type_str,
                is_iterator: false,
            })
        }
        ExprKind::FieldAccess(receiver, field) => {
            // e.g., self.items — resolve the field type from the struct
            let recv = walk_chain_type(receiver, sym)?;
            if let Some(struct_name) = extract_struct_name(&recv.elem_type) {
                for sd in &sym.definitions {
                    if sd.name == struct_name
                        && matches!(sd.kind, crate::compiler::typechecker::SymbolKind::Struct)
                    {
                        for (fname, ftype) in extract_fields_from_struct_desc(&sd.type_desc) {
                            if fname == *field {
                                return Some(ChainType {
                                    elem_type: ftype,
                                    is_iterator: false,
                                });
                            }
                        }
                    }
                }
            }
            None
        }
        ExprKind::MethodCall(receiver, method, args) => {
            let recv = walk_chain_type(receiver, sym)?;

            match method.as_str() {
                "iter" => {
                    let inner = extract_iter_elem_type(&recv.elem_type)?;
                    Some(ChainType {
                        elem_type: inner,
                        is_iterator: true,
                    })
                }
                "chars" => {
                    if recv.elem_type == "string" {
                        Some(ChainType {
                            elem_type: "char".to_string(),
                            is_iterator: true,
                        })
                    } else {
                        None
                    }
                }
                "map" | "flat_map" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    // Extract return type from closure argument
                    let ret_type = args.first().and_then(|arg| {
                        if let ExprKind::Closure(c) = &arg.kind {
                            Some(format!("{}", c.return_type))
                        } else {
                            None
                        }
                    });
                    if let Some(rt) = ret_type {
                        let elem = if method == "flat_map" {
                            // flat_map closure returns [T], extract T
                            extract_array_inner(&rt).unwrap_or(rt)
                        } else {
                            rt
                        };
                        Some(ChainType {
                            elem_type: elem,
                            is_iterator: true,
                        })
                    } else {
                        // Can't determine type, keep iterator context with unknown element
                        Some(ChainType {
                            elem_type: "?".to_string(),
                            is_iterator: true,
                        })
                    }
                }
                "filter" | "take" | "skip" | "chain" | "take_while" | "rev" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    Some(ChainType {
                        elem_type: recv.elem_type,
                        is_iterator: true,
                    })
                }
                "enumerate" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    Some(ChainType {
                        elem_type: format!("(int, {})", recv.elem_type),
                        is_iterator: true,
                    })
                }
                "zip" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    let other_elem = args.first().and_then(|arg| {
                        match &arg.kind {
                            ExprKind::Ident(name) => {
                                let ty = resolve_ident_type(name, sym)?;
                                extract_iter_elem_type(&ty)
                            }
                            ExprKind::Range(..) | ExprKind::RangeInclusive(..) => {
                                Some("int".to_string())
                            }
                            ExprKind::MethodCall(..) => {
                                // Recursively resolve chained zip args
                                let ct = walk_chain_type(arg, sym)?;
                                if ct.is_iterator {
                                    Some(ct.elem_type)
                                } else {
                                    extract_iter_elem_type(&ct.elem_type)
                                }
                            }
                            _ => None,
                        }
                    });
                    let other = other_elem.unwrap_or_else(|| "?".to_string());
                    Some(ChainType {
                        elem_type: format!("({}, {})", recv.elem_type, other),
                        is_iterator: true,
                    })
                }
                "flatten" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    let inner =
                        extract_array_inner(&recv.elem_type).unwrap_or(recv.elem_type.clone());
                    Some(ChainType {
                        elem_type: inner,
                        is_iterator: true,
                    })
                }
                // Terminators — produce non-iterator results
                "collect" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    Some(ChainType {
                        elem_type: format!("[{}]", recv.elem_type),
                        is_iterator: false,
                    })
                }
                "sum" | "count" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    Some(ChainType {
                        elem_type: "int".to_string(),
                        is_iterator: false,
                    })
                }
                "any" | "all" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    Some(ChainType {
                        elem_type: "bool".to_string(),
                        is_iterator: false,
                    })
                }
                "find" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    Some(ChainType {
                        elem_type: format!("Option<{}>", recv.elem_type),
                        is_iterator: false,
                    })
                }
                "position" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    Some(ChainType {
                        elem_type: "Option<int>".to_string(),
                        is_iterator: false,
                    })
                }
                "fold" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    // Result type depends on init value and closure, use element type
                    Some(ChainType {
                        elem_type: recv.elem_type,
                        is_iterator: false,
                    })
                }
                "reduce" => {
                    if !recv.is_iterator {
                        return None;
                    }
                    Some(ChainType {
                        elem_type: format!("Option<{}>", recv.elem_type),
                        is_iterator: false,
                    })
                }
                _ => None,
            }
        }
        ExprKind::Range(..) | ExprKind::RangeInclusive(..) | ExprKind::RangeFrom(..) => {
            // Range expressions produce int sequences — not yet iterated
            Some(ChainType {
                elem_type: "[int]".to_string(),
                is_iterator: false,
            })
        }
        _ => None,
    }
}

/// Extract the element type for iteration from a container type string.
fn extract_iter_elem_type(type_str: &str) -> Option<String> {
    if type_str.starts_with('[') && type_str.ends_with(']') {
        // [int] -> int
        Some(type_str[1..type_str.len() - 1].to_string())
    } else if type_str.starts_with("Map<") {
        // Map<K, V> -> (K, V)
        let inner = &type_str[4..type_str.len() - 1];
        // Split on first comma at depth 0
        if let Some((k, v)) = split_generic_args(inner) {
            Some(format!("({}, {})", k.trim(), v.trim()))
        } else {
            None
        }
    } else if type_str.starts_with("Set<") {
        // Set<T> -> T
        Some(type_str[4..type_str.len() - 1].to_string())
    } else {
        None
    }
}

/// Split "K, V" respecting generic nesting depth, at the first top-level comma.
fn split_generic_args(s: &str) -> Option<(&str, &str)> {
    let mut depth = 0;
    for (i, ch) in s.char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => depth -= 1,
            ',' if depth == 0 => {
                return Some((&s[..i], &s[i + 1..]));
            }
            _ => {}
        }
    }
    None
}

/// Extract inner type from an array type string: "[int]" -> "int".
fn extract_array_inner(type_str: &str) -> Option<String> {
    if type_str.starts_with('[') && type_str.ends_with(']') {
        Some(type_str[1..type_str.len() - 1].to_string())
    } else {
        None
    }
}

/// Return method completions for an iterator with the given element type.
fn iterator_methods(elem_type: &str) -> Vec<(&'static str, String)> {
    vec![
        // Combinators
        ("map", format!("(|{}| -> ?) -> Iterator<?>", elem_type)),
        (
            "filter",
            format!("(|{}| -> bool) -> Iterator<{}>", elem_type, elem_type),
        ),
        ("enumerate", format!("() -> Iterator<(int, {})>", elem_type)),
        ("zip", format!("(iterable) -> Iterator<({}, ?)>", elem_type)),
        ("take", format!("(n: int) -> Iterator<{}>", elem_type)),
        ("skip", format!("(n: int) -> Iterator<{}>", elem_type)),
        ("chain", format!("(iterable) -> Iterator<{}>", elem_type)),
        (
            "take_while",
            format!("(|{}| -> bool) -> Iterator<{}>", elem_type, elem_type),
        ),
        ("rev", format!("() -> Iterator<{}>", elem_type)),
        (
            "flat_map",
            format!("(|{}| -> [?]) -> Iterator<?>", elem_type),
        ),
        ("flatten", format!("() -> Iterator<{}>", elem_type)),
        // Terminators
        ("collect", format!("() -> [{}]", elem_type)),
        (
            "fold",
            format!("(init, |acc, {}| -> acc) -> acc", elem_type),
        ),
        (
            "reduce",
            format!("(|{0}, {0}| -> {0}) -> Option<{0}>", elem_type),
        ),
        ("any", format!("(|{}| -> bool) -> bool", elem_type)),
        ("all", format!("(|{}| -> bool) -> bool", elem_type)),
        (
            "find",
            format!("(|{}| -> bool) -> Option<{}>", elem_type, elem_type),
        ),
        ("sum", "() -> int".to_string()),
        ("count", "() -> int".to_string()),
        (
            "position",
            format!("(|{}| -> bool) -> Option<int>", elem_type),
        ),
    ]
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

    // ═══════════════════════════════════════════════════════════════
    //  Chain-aware dot-completion tests
    // ═══════════════════════════════════════════════════════════════

    #[test]
    fn test_extract_chain_text_simple_ident() {
        // Simple ident — no chain
        assert_eq!(extract_chain_text("    arr."), None);
        assert_eq!(extract_chain_text("    x."), None);
    }

    #[test]
    fn test_extract_chain_text_iter() {
        let result = extract_chain_text("    arr.iter().");
        assert_eq!(result, Some("arr.iter()".to_string()));
    }

    #[test]
    fn test_extract_chain_text_map_with_closure() {
        let result = extract_chain_text("    arr.iter().map(|x: int| -> int { x + 1 }).");
        assert_eq!(
            result,
            Some("arr.iter().map(|x: int| -> int { x + 1 })".to_string())
        );
    }

    #[test]
    fn test_extract_chain_text_with_let() {
        // let binding = arr.iter().
        let result = extract_chain_text("    let y = arr.iter().");
        assert_eq!(result, Some("arr.iter()".to_string()));
    }

    #[test]
    fn test_parse_chain_expr_simple() {
        let expr = parse_chain_expr("arr.iter()");
        assert!(expr.is_some());
        let expr = expr.unwrap();
        if let crate::compiler::ast::ExprKind::MethodCall(_, method, _) = &expr.kind {
            assert_eq!(method, "iter");
        } else {
            panic!("Expected MethodCall, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_parse_chain_expr_map() {
        let expr = parse_chain_expr("arr.iter().map(|x: int| -> string { return x; })");
        assert!(expr.is_some());
        let expr = expr.unwrap();
        if let crate::compiler::ast::ExprKind::MethodCall(_, method, _) = &expr.kind {
            assert_eq!(method, "map");
        } else {
            panic!("Expected MethodCall for map");
        }
    }

    #[test]
    fn test_extract_iter_elem_type() {
        assert_eq!(extract_iter_elem_type("[int]"), Some("int".to_string()));
        assert_eq!(
            extract_iter_elem_type("[string]"),
            Some("string".to_string())
        );
        assert_eq!(
            extract_iter_elem_type("Map<string, int>"),
            Some("(string, int)".to_string())
        );
        assert_eq!(extract_iter_elem_type("Set<int>"), Some("int".to_string()));
        assert_eq!(extract_iter_elem_type("int"), None);
    }

    #[test]
    fn test_chain_completion_iter_on_array() {
        // Source must be parseable — define arr, use a dummy statement
        let source =
            "fn main() -> unit {\n    let arr: [int] = [1, 2, 3];\n    let x: int = 0;\n}\n";
        let server = LspServer::new();
        let line_text = "    arr.iter().";
        let items = server.dot_completions(source, line_text);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"map"), "Should contain map: {:?}", labels);
        assert!(
            labels.contains(&"filter"),
            "Should contain filter: {:?}",
            labels
        );
        assert!(
            labels.contains(&"collect"),
            "Should contain collect: {:?}",
            labels
        );
        assert!(
            labels.contains(&"enumerate"),
            "Should contain enumerate: {:?}",
            labels
        );
        assert!(
            labels.contains(&"fold"),
            "Should contain fold: {:?}",
            labels
        );
    }

    #[test]
    fn test_chain_completion_map_changes_type() {
        let source =
            "fn main() -> unit {\n    let arr: [int] = [1, 2, 3];\n    let x: int = 0;\n}\n";
        let server = LspServer::new();
        let line_text = "    arr.iter().map(|x: int| -> string { return int_to_string(x); }).";
        let items = server.dot_completions(source, line_text);
        // Should show iterator methods with string element type
        let collect_item = items.iter().find(|i| i.label == "collect");
        assert!(collect_item.is_some(), "Should contain collect");
        assert_eq!(
            collect_item.unwrap().detail.as_deref(),
            Some("() -> [string]")
        );
    }

    #[test]
    fn test_chain_completion_enumerate() {
        let source =
            "fn main() -> unit {\n    let arr: [int] = [1, 2, 3];\n    let x: int = 0;\n}\n";
        let server = LspServer::new();
        let line_text = "    arr.iter().enumerate().";
        let items = server.dot_completions(source, line_text);
        let collect_item = items.iter().find(|i| i.label == "collect");
        assert!(collect_item.is_some(), "Should contain collect");
        assert_eq!(
            collect_item.unwrap().detail.as_deref(),
            Some("() -> [(int, int)]")
        );
    }

    #[test]
    fn test_chain_completion_filter_preserves_type() {
        let source =
            "fn main() -> unit {\n    let arr: [int] = [1, 2, 3];\n    let x: int = 0;\n}\n";
        let server = LspServer::new();
        let line_text = "    arr.iter().filter(|x: int| -> bool { return x > 0; }).";
        let items = server.dot_completions(source, line_text);
        let collect_item = items.iter().find(|i| i.label == "collect");
        assert!(collect_item.is_some(), "Should contain collect");
        assert_eq!(collect_item.unwrap().detail.as_deref(), Some("() -> [int]"));
    }

    #[test]
    fn test_chain_completion_string_chars() {
        let source =
            "fn main() -> unit {\n    let s: string = \"hello\";\n    let x: int = 0;\n}\n";
        let server = LspServer::new();
        let line_text = "    s.chars().";
        let items = server.dot_completions(source, line_text);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"map"), "Should contain map: {:?}", labels);
        let collect_item = items.iter().find(|i| i.label == "collect");
        assert!(collect_item.is_some());
        assert_eq!(
            collect_item.unwrap().detail.as_deref(),
            Some("() -> [char]")
        );
    }

    #[test]
    fn test_chain_completion_collect_returns_array_methods() {
        let source =
            "fn main() -> unit {\n    let arr: [int] = [1, 2, 3];\n    let x: int = 0;\n}\n";
        let server = LspServer::new();
        let line_text = "    arr.iter().filter(|x: int| -> bool { return x > 0; }).collect().";
        let items = server.dot_completions(source, line_text);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        // collect() returns [int], so should show array methods
        assert!(labels.contains(&"len"), "Should contain len: {:?}", labels);
        assert!(
            labels.contains(&"push"),
            "Should contain push: {:?}",
            labels
        );
        assert!(
            labels.contains(&"iter"),
            "Should contain iter: {:?}",
            labels
        );
    }

    #[test]
    fn test_extract_chain_text_skips_string_literals() {
        // Literal '(' inside a string should not corrupt depth tracking
        let result =
            extract_chain_text("    arr.iter().map(|x: int| -> string { return \"(\"; }).");
        assert_eq!(
            result,
            Some("arr.iter().map(|x: int| -> string { return \"(\"; })".to_string())
        );
    }

    #[test]
    fn test_extract_chain_text_skips_char_literals() {
        let result = extract_chain_text("    arr.iter().map(|x: int| -> char { return '('; }).");
        assert_eq!(
            result,
            Some("arr.iter().map(|x: int| -> char { return '('; })".to_string())
        );
    }

    #[test]
    fn test_chain_completion_range_pipeline() {
        let source = "fn main() -> unit {\n    let x: int = 0;\n}\n";
        let server = LspServer::new();
        let line_text = "    (0..10).iter().";
        let items = server.dot_completions(source, line_text);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"map"), "Should contain map: {:?}", labels);
        assert!(
            labels.contains(&"collect"),
            "Should contain collect: {:?}",
            labels
        );
        let collect_item = items.iter().find(|i| i.label == "collect");
        assert_eq!(collect_item.unwrap().detail.as_deref(), Some("() -> [int]"));
    }

    #[test]
    fn test_chain_completion_range_map() {
        let source = "fn main() -> unit {\n    let x: int = 0;\n}\n";
        let server = LspServer::new();
        let line_text = "    (0..10).iter().map(|x: int| -> string { return int_to_string(x); }).";
        let items = server.dot_completions(source, line_text);
        let collect_item = items.iter().find(|i| i.label == "collect");
        assert!(collect_item.is_some(), "Should contain collect");
        assert_eq!(
            collect_item.unwrap().detail.as_deref(),
            Some("() -> [string]")
        );
    }

    #[test]
    fn test_chain_completion_reduce_returns_option() {
        let source =
            "fn main() -> unit {\n    let arr: [int] = [1, 2, 3];\n    let x: int = 0;\n}\n";
        let server = LspServer::new();
        let line_text = "    arr.iter().reduce(|a: int, b: int| -> int { return a + b; }).";
        let items = server.dot_completions(source, line_text);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        // reduce returns Option<int>, so should show Option methods
        assert!(
            labels.contains(&"unwrap"),
            "Should contain unwrap: {:?}",
            labels
        );
        assert!(
            labels.contains(&"is_some"),
            "Should contain is_some: {:?}",
            labels
        );
        assert!(
            labels.contains(&"is_none"),
            "Should contain is_none: {:?}",
            labels
        );
    }
}
