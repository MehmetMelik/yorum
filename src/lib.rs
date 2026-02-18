pub mod compiler;
pub mod lsp;
pub mod manifest;
pub mod repl;

use compiler::codegen::Codegen;
use compiler::dce::eliminate_dead_code;
use compiler::lexer::Lexer;
use compiler::monomorphize::monomorphize;
use compiler::ownership::OwnershipChecker;
use compiler::parser::Parser;
use compiler::span::Span;
use compiler::typechecker::TypeChecker;

// ═══════════════════════════════════════════════════════════════
//  Diagnostics API (for LSP and tooling)
// ═══════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct CompilerDiagnostic {
    pub message: String,
    pub span: Span,
    pub severity: DiagnosticSeverity,
}

/// Run lex → parse → typecheck → ownership check and return structured diagnostics.
///
/// Unlike `typecheck()`, this returns `Vec<CompilerDiagnostic>` with spans
/// instead of a formatted error string. Always succeeds — errors are in the vec.
pub fn check_diagnostics(source: &str) -> Vec<CompilerDiagnostic> {
    let (diags, _) = check_with_symbols(source);
    diags
}

/// Run the checking pipeline and also collect a symbol table for hover/go-to-def.
///
/// Returns diagnostics plus an optional symbol table (None if parsing failed
/// before the typechecker could run).
pub fn check_with_symbols(
    source: &str,
) -> (
    Vec<CompilerDiagnostic>,
    Option<compiler::typechecker::SymbolTable>,
) {
    let mut diagnostics = Vec::new();

    // Phase 1: Lex
    let mut lexer = Lexer::new(source);
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => {
            diagnostics.push(CompilerDiagnostic {
                message: format!("{}", e),
                span: Span::synthetic(),
                severity: DiagnosticSeverity::Error,
            });
            return (diagnostics, None);
        }
    };

    // Phase 2: Parse
    let mut parser = Parser::new(tokens);
    let program = match parser.parse_program() {
        Ok(p) => p,
        Err(e) => {
            diagnostics.push(CompilerDiagnostic {
                message: format!("{}", e),
                span: Span::synthetic(),
                severity: DiagnosticSeverity::Error,
            });
            return (diagnostics, None);
        }
    };

    // Phase 3: Type check (with symbol collection)
    let mut typechecker = TypeChecker::new_with_symbols();
    let type_errors = typechecker.check_program(&program);
    let symbols = typechecker.take_symbol_table();

    if let Err(errs) = type_errors {
        for e in errs {
            diagnostics.push(CompilerDiagnostic {
                message: e.message.clone(),
                span: e.span,
                severity: DiagnosticSeverity::Error,
            });
        }
    }

    // Phase 4: Ownership check (still provide symbols even if ownership fails)
    let mut ownership = OwnershipChecker::new();
    if let Err(errs) = ownership.check_program(&program) {
        for e in errs {
            diagnostics.push(CompilerDiagnostic {
                message: e.message.clone(),
                span: e.span,
                severity: DiagnosticSeverity::Error,
            });
        }
    }

    (diagnostics, Some(symbols))
}

// ═══════════════════════════════════════════════════════════════
//  Existing public API
// ═══════════════════════════════════════════════════════════════

/// Compile Yorum source code to LLVM IR.
pub fn compile_to_ir(source: &str) -> Result<String, String> {
    // Phase 1: Lex
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| format!("{}", e))?;

    // Phase 2: Parse
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program().map_err(|e| format!("{}", e))?;

    // Phase 3: Type check
    let mut typechecker = TypeChecker::new();
    typechecker.check_program(&program).map_err(|errs| {
        errs.iter()
            .map(|e| format!("{}", e))
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    // Phase 4: Ownership check
    let mut ownership = OwnershipChecker::new();
    ownership.check_program(&program).map_err(|errs| {
        errs.iter()
            .map(|e| format!("{}", e))
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    // Phase 5: Monomorphize generics
    let program = monomorphize(program);

    // Phase 6: Dead code elimination
    let program = eliminate_dead_code(program);

    // Phase 7: Code generation
    let mut codegen = Codegen::new();
    let ir = codegen.generate(&program).map_err(|e| format!("{}", e))?;

    Ok(ir)
}

/// Parse Yorum source code and return the AST as a JSON string.
pub fn source_to_ast_json(source: &str) -> Result<String, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| format!("{}", e))?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program().map_err(|e| format!("{}", e))?;
    serde_json::to_string_pretty(&program).map_err(|e| format!("{}", e))
}

/// Type-check Yorum source code without generating IR.
pub fn typecheck(source: &str) -> Result<(), String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| format!("{}", e))?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program().map_err(|e| format!("{}", e))?;

    let mut typechecker = TypeChecker::new();
    typechecker.check_program(&program).map_err(|errs| {
        errs.iter()
            .map(|e| format!("{}", e))
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let mut ownership = OwnershipChecker::new();
    ownership.check_program(&program).map_err(|errs| {
        errs.iter()
            .map(|e| format!("{}", e))
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    Ok(())
}

/// Compile Yorum source code to LLVM IR with optional debug info.
pub fn compile_to_ir_with_options(
    source: &str,
    filename: &str,
    debug: bool,
) -> Result<String, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| format!("{}", e))?;

    let mut parser = Parser::new(tokens);
    let program = parser.parse_program().map_err(|e| format!("{}", e))?;

    let mut typechecker = TypeChecker::new();
    typechecker.check_program(&program).map_err(|errs| {
        errs.iter()
            .map(|e| format!("{}", e))
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let mut ownership = OwnershipChecker::new();
    ownership.check_program(&program).map_err(|errs| {
        errs.iter()
            .map(|e| format!("{}", e))
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let program = monomorphize(program);
    let program = eliminate_dead_code(program);

    let mut codegen = Codegen::new();
    if debug {
        codegen.enable_debug(filename);
    }
    let ir = codegen.generate(&program).map_err(|e| format!("{}", e))?;

    Ok(ir)
}

/// Format Yorum source code.
///
/// Lexes with comment collection, parses, then walks the AST to produce
/// consistently formatted output. Does not type-check — works on any
/// syntactically valid source.
pub fn format_source(source: &str) -> Result<String, String> {
    use compiler::formatter::Formatter;
    use compiler::lexer::Lexer;
    use compiler::parser::Parser;

    let lexer = Lexer::new_with_comments(source);
    let (tokens, comments, source_chars) = lexer
        .tokenize_with_comments()
        .map_err(|e| format!("{}", e))?;

    let mut parser = Parser::new(tokens);
    let program = parser.parse_program().map_err(|e| format!("{}", e))?;

    let formatter = Formatter::new(source_chars, comments);
    Ok(formatter.format_program(&program))
}

/// Return the list of builtin function names and their signatures.
pub fn builtin_function_list() -> Vec<(String, String)> {
    compiler::typechecker::TypeChecker::builtin_names()
}

/// Compile a multi-file Yorum project from a root directory containing yorum.toml.
pub fn compile_project(root_dir: &std::path::Path) -> Result<String, String> {
    compiler::project::compile_project(root_dir)
}
