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
            },
            server_info: ServerInfo {
                name: "yorum-lsp".to_string(),
                version: "0.7.0".to_string(),
            },
        };
        if let Some(id) = &msg.id {
            self.send_response(writer, id.clone(), serde_json::to_value(result).unwrap())?;
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
            if let Ok(p) = serde_json::from_value::<DidOpenTextDocumentParams>(params.clone()) {
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
            Some(h) => serde_json::to_value(h).unwrap(),
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
            Some(loc) => serde_json::to_value(loc).unwrap(),
            None => serde_json::Value::Null,
        };
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

        self.send_notification(
            writer,
            "textDocument/publishDiagnostics",
            serde_json::to_value(params).unwrap(),
        )
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
        let body = serde_json::to_string(&resp).unwrap();
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
        let body = serde_json::to_string(&resp).unwrap();
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
        let body = serde_json::to_string(&notif).unwrap();
        transport::write_message(writer, &body)
    }
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
