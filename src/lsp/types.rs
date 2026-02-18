use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ═══════════════════════════════════════════════════════════════
//  JSON-RPC 2.0
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Deserialize)]
pub struct JsonRpcMessage {
    pub jsonrpc: String,
    pub id: Option<serde_json::Value>,
    pub method: Option<String>,
    pub params: Option<serde_json::Value>,
}

#[derive(Debug, Serialize)]
pub struct JsonRpcResponse {
    pub jsonrpc: String,
    pub id: serde_json::Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

#[derive(Debug, Serialize)]
pub struct JsonRpcNotification {
    pub jsonrpc: String,
    pub method: String,
    pub params: serde_json::Value,
}

#[derive(Debug, Serialize)]
pub struct JsonRpcError {
    pub code: i64,
    pub message: String,
}

// JSON-RPC error codes
pub const PARSE_ERROR: i64 = -32700;
pub const METHOD_NOT_FOUND: i64 = -32601;
pub const INTERNAL_ERROR: i64 = -32603;

// ═══════════════════════════════════════════════════════════════
//  LSP Protocol Types
// ═══════════════════════════════════════════════════════════════

// --- Lifecycle ---

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeParams {
    #[allow(dead_code)]
    pub root_uri: Option<String>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeResult {
    pub capabilities: ServerCapabilities,
    pub server_info: ServerInfo,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ServerCapabilities {
    pub text_document_sync: i32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hover_provider: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub definition_provider: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub position_encoding: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub completion_provider: Option<CompletionOptions>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub code_action_provider: Option<bool>,
}

#[derive(Debug, Serialize)]
pub struct ServerInfo {
    pub name: String,
    pub version: String,
}

// --- Text Document ---

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DidOpenTextDocumentParams {
    pub text_document: TextDocumentItem,
}

#[derive(Debug, Deserialize)]
pub struct TextDocumentItem {
    pub uri: String,
    pub text: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DidCloseTextDocumentParams {
    pub text_document: TextDocumentIdentifier,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DidChangeTextDocumentParams {
    pub text_document: VersionedTextDocumentIdentifier,
    pub content_changes: Vec<TextDocumentContentChangeEvent>,
}

#[derive(Debug, Deserialize)]
pub struct VersionedTextDocumentIdentifier {
    pub uri: String,
}

#[derive(Debug, Deserialize)]
pub struct TextDocumentContentChangeEvent {
    pub text: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextDocumentPositionParams {
    pub text_document: TextDocumentIdentifier,
    pub position: Position,
}

#[derive(Debug, Deserialize)]
pub struct TextDocumentIdentifier {
    pub uri: String,
}

// --- Position / Range / Location ---

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

#[derive(Debug, Clone, Serialize)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Serialize)]
pub struct Location {
    pub uri: String,
    pub range: Range,
}

// --- Diagnostics ---

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PublishDiagnosticsParams {
    pub uri: String,
    pub diagnostics: Vec<LspDiagnostic>,
}

#[derive(Debug, Serialize)]
pub struct LspDiagnostic {
    pub range: Range,
    pub severity: i32,
    pub source: String,
    pub message: String,
}

// LSP diagnostic severity constants
pub const SEVERITY_ERROR: i32 = 1;
pub const SEVERITY_WARNING: i32 = 2;

// --- Hover ---

#[derive(Debug, Serialize)]
pub struct Hover {
    pub contents: MarkupContent,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub range: Option<Range>,
}

#[derive(Debug, Serialize)]
pub struct MarkupContent {
    pub kind: String,
    pub value: String,
}

// --- Completion ---

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionParams {
    pub text_document: TextDocumentIdentifier,
    pub position: Position,
    #[serde(default)]
    pub context: Option<CompletionContext>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionContext {
    pub trigger_kind: Option<i32>,
    pub trigger_character: Option<String>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionList {
    pub is_incomplete: bool,
    pub items: Vec<CompletionItem>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionItem {
    pub label: String,
    pub kind: i32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detail: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub insert_text: Option<String>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionOptions {
    pub trigger_characters: Vec<String>,
}

// Completion item kinds
pub const COMPLETION_KIND_FUNCTION: i32 = 3;
pub const COMPLETION_KIND_FIELD: i32 = 5;
pub const COMPLETION_KIND_VARIABLE: i32 = 6;
pub const COMPLETION_KIND_ENUM_MEMBER: i32 = 13;
pub const COMPLETION_KIND_KEYWORD: i32 = 14;
pub const COMPLETION_KIND_STRUCT: i32 = 22;

// --- Code Actions ---

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CodeActionParams {
    pub text_document: TextDocumentIdentifier,
    pub range: CodeActionRange,
    pub context: CodeActionContext,
}

#[derive(Debug, Deserialize)]
pub struct CodeActionRange {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Deserialize)]
pub struct CodeActionDiagnostic {
    pub range: CodeActionDiagRange,
    pub message: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct CodeActionDiagRange {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Deserialize)]
pub struct CodeActionContext {
    pub diagnostics: Vec<CodeActionDiagnostic>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CodeAction {
    pub title: String,
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub edit: Option<WorkspaceEdit>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_preferred: Option<bool>,
}

#[derive(Debug, Serialize)]
pub struct WorkspaceEdit {
    pub changes: HashMap<String, Vec<TextEdit>>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TextEdit {
    pub range: Range,
    pub new_text: String,
}
