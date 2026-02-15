pub mod compiler;

use compiler::codegen::Codegen;
use compiler::lexer::Lexer;
use compiler::ownership::OwnershipChecker;
use compiler::parser::Parser;
use compiler::typechecker::TypeChecker;

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

    // Phase 5: Code generation
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
