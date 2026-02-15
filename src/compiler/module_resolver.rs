use crate::compiler::ast::Program;
use crate::compiler::lexer::Lexer;
use crate::compiler::parser::Parser;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct ParsedModule {
    pub module_path: String,
    pub program: Program,
    pub file_path: PathBuf,
}

pub struct ModuleResolver {
    src_dir: PathBuf,
}

impl ModuleResolver {
    pub fn new(src_dir: PathBuf) -> Self {
        Self { src_dir }
    }

    /// Discover all .yrm files under src_dir, parse them, and validate module names.
    pub fn resolve_all(&self) -> Result<HashMap<String, ParsedModule>, String> {
        let mut modules = HashMap::new();
        self.discover_files(&self.src_dir, &mut modules)?;
        Ok(modules)
    }

    fn discover_files(
        &self,
        dir: &Path,
        modules: &mut HashMap<String, ParsedModule>,
    ) -> Result<(), String> {
        let entries = std::fs::read_dir(dir)
            .map_err(|e| format!("cannot read directory '{}': {}", dir.display(), e))?;

        for entry in entries {
            let entry = entry.map_err(|e| format!("directory entry error: {}", e))?;
            let path = entry.path();
            if path.is_dir() {
                self.discover_files(&path, modules)?;
            } else if path.extension().and_then(|e| e.to_str()) == Some("yrm") {
                let module_path = self.file_to_module_path(&path)?;
                let source = std::fs::read_to_string(&path)
                    .map_err(|e| format!("cannot read '{}': {}", path.display(), e))?;

                let mut lexer = Lexer::new(&source);
                let tokens = lexer.tokenize().map_err(|e| format!("{}", e))?;
                let mut parser = Parser::new(tokens);
                let program = parser.parse_program().map_err(|e| format!("{}", e))?;

                // Validate module name matches filesystem path
                if let Some(ref declared_name) = program.module_name {
                    if *declared_name != module_path {
                        return Err(format!(
                            "module name mismatch in '{}': declared '{}' but expected '{}'",
                            path.display(),
                            declared_name,
                            module_path
                        ));
                    }
                }

                modules.insert(
                    module_path.clone(),
                    ParsedModule {
                        module_path,
                        program,
                        file_path: path,
                    },
                );
            }
        }
        Ok(())
    }

    /// Convert a filesystem path like src/math/vector.yrm to module path "math.vector"
    fn file_to_module_path(&self, path: &Path) -> Result<String, String> {
        let relative = path.strip_prefix(&self.src_dir).map_err(|_| {
            format!(
                "file '{}' is not under src directory '{}'",
                path.display(),
                self.src_dir.display()
            )
        })?;

        let stem = relative.with_extension("");
        let parts: Vec<&str> = stem
            .components()
            .map(|c| {
                c.as_os_str()
                    .to_str()
                    .ok_or_else(|| format!("invalid UTF-8 in path: {}", path.display()))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(parts.join("."))
    }
}
