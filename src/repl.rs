use std::io::{self, BufRead, Write};
use std::process::Command;

pub struct Repl {
    accumulated_fns: Vec<String>,
    accumulated_types: Vec<String>,
    clang: String,
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}

impl Repl {
    pub fn new() -> Self {
        Self {
            accumulated_fns: Vec::new(),
            accumulated_types: Vec::new(),
            clang: detect_repl_clang(),
        }
    }

    pub fn run(&mut self) {
        let version = env!("CARGO_PKG_VERSION");
        println!("Yorum {} REPL", version);
        println!("Type :help for help, :exit to quit.\n");

        let stdin = io::stdin();
        let mut reader = stdin.lock();
        let mut buf = String::new();
        let mut continuation = String::new();

        loop {
            if continuation.is_empty() {
                print!("> ");
            } else {
                print!("... ");
            }
            io::stdout().flush().ok();

            buf.clear();
            if reader.read_line(&mut buf).unwrap_or(0) == 0 {
                println!();
                break;
            }
            let line = buf.trim_end().to_string();

            if continuation.is_empty() {
                // Handle special commands
                match line.as_str() {
                    ":exit" | ":quit" | ":q" => break,
                    ":clear" => {
                        self.accumulated_fns.clear();
                        self.accumulated_types.clear();
                        println!("Cleared all definitions.");
                        continue;
                    }
                    ":help" => {
                        println!("  :exit    Exit the REPL");
                        println!("  :clear   Clear accumulated definitions");
                        println!("  :type <expr>  Show expression type");
                        println!("  :help    Show this help");
                        continue;
                    }
                    "" => continue,
                    _ => {}
                }

                if let Some(expr) = line.strip_prefix(":type ") {
                    self.show_type(expr);
                    continue;
                }
            }

            continuation.push_str(&line);
            continuation.push('\n');

            if !is_complete(&continuation) {
                continue;
            }

            let input = continuation.trim().to_string();
            continuation.clear();

            self.eval(&input);
        }
    }

    fn eval(&mut self, input: &str) {
        // Detect definitions
        let trimmed = input.trim();
        if trimmed.starts_with("fn ")
            || trimmed.starts_with("pure fn ")
            || trimmed.starts_with("pub fn ")
        {
            self.accumulated_fns.push(input.to_string());
            println!("Defined.");
            return;
        }
        if trimmed.starts_with("struct ") || trimmed.starts_with("enum ") {
            self.accumulated_types.push(input.to_string());
            println!("Defined.");
            return;
        }

        // Try as statement first
        let program = self.build_program(input, WrapMode::Statement);
        if let Ok(ir) = crate::compile_to_ir(&program) {
            self.run_ir(&ir);
            return;
        }

        // Try as expression with various print wrappers
        for print_fn in &[
            "print_int",
            "print_float",
            "print_bool",
            "print_str",
            "print_char",
        ] {
            let wrapped = format!("{}({});", print_fn, input);
            let program = self.build_program(&wrapped, WrapMode::Statement);
            if let Ok(ir) = crate::compile_to_ir(&program) {
                self.run_ir(&ir);
                return;
            }
        }

        // If nothing worked, show the original error
        let program = self.build_program(input, WrapMode::Statement);
        if let Err(e) = crate::compile_to_ir(&program) {
            eprintln!("error: {}", e);
        }
    }

    fn show_type(&self, expr: &str) {
        // Try type check: wrap in a let binding to provoke a type mismatch
        // The error message reveals the actual type
        let program = format!(
            "{}\n{}\nfn main() -> int {{\n  let __repl_type_probe: unit = {};\n  return 0;\n}}\n",
            self.accumulated_types.join("\n"),
            self.accumulated_fns.join("\n"),
            expr
        );

        match crate::typecheck(&program) {
            Ok(()) => {
                // No error means the expr is of type unit
                println!("unit");
            }
            Err(e) => {
                // Look for "expected unit, found <type>"
                if let Some(pos) = e.find("expected unit, found ") {
                    let found = &e[pos + "expected unit, found ".len()..];
                    let ty = found.split_whitespace().next().unwrap_or(found);
                    println!("{}", ty);
                } else {
                    eprintln!("error: {}", e);
                }
            }
        }
    }

    fn build_program(&self, input: &str, mode: WrapMode) -> String {
        let mut prog = String::new();
        for t in &self.accumulated_types {
            prog.push_str(t);
            prog.push('\n');
        }
        for f in &self.accumulated_fns {
            prog.push_str(f);
            prog.push('\n');
        }
        match mode {
            WrapMode::Statement => {
                prog.push_str("fn main() -> int {\n  ");
                prog.push_str(input);
                prog.push_str("\n  return 0;\n}\n");
            }
        }
        prog
    }

    fn run_ir(&self, ir: &str) {
        let pid = std::process::id();
        let tmp = std::env::temp_dir();
        let ir_path = tmp.join(format!("yorum_repl_{}.ll", pid));
        let bin_path = tmp.join(format!("yorum_repl_{}", pid));

        if std::fs::write(&ir_path, ir).is_err() {
            eprintln!("error: cannot write temp file");
            return;
        }

        let mut clang_args = vec![
            "-x",
            "ir",
            ir_path.to_str().unwrap(),
            "-o",
            bin_path.to_str().unwrap(),
            "-Wno-override-module",
        ];
        if ir.contains("call i32 @pthread_create") {
            clang_args.push("-lpthread");
        }

        let status = Command::new(&self.clang).args(&clang_args).status();
        if status.map(|s| !s.success()).unwrap_or(true) {
            let _ = std::fs::remove_file(&ir_path);
            let _ = std::fs::remove_file(&bin_path);
            return;
        }

        let output = Command::new(bin_path.to_str().unwrap()).output();
        if let Ok(output) = output {
            let stdout = String::from_utf8_lossy(&output.stdout);
            if !stdout.is_empty() {
                print!("{}", stdout);
            }
            let stderr = String::from_utf8_lossy(&output.stderr);
            if !stderr.is_empty() {
                eprint!("{}", stderr);
            }
        }

        let _ = std::fs::remove_file(&ir_path);
        let _ = std::fs::remove_file(&bin_path);
    }
}

enum WrapMode {
    Statement,
}

fn is_complete(input: &str) -> bool {
    let mut depth: i32 = 0;
    for ch in input.chars() {
        match ch {
            '{' => depth += 1,
            '}' => depth -= 1,
            _ => {}
        }
    }
    depth <= 0
}

fn detect_repl_clang() -> String {
    if Command::new("clang")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        return "clang".to_string();
    }
    let brew_clang = "/opt/homebrew/opt/llvm/bin/clang";
    if Command::new(brew_clang)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        return brew_clang.to_string();
    }
    eprintln!("warning: clang not found, REPL execution disabled");
    eprintln!("hint: install clang or run 'brew install llvm'");
    "clang".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_complete_simple() {
        assert!(is_complete("1 + 2;\n"));
        assert!(is_complete("let x: int = 5;\n"));
    }

    #[test]
    fn test_is_complete_open_brace() {
        assert!(!is_complete("fn add(a: int) -> int {\n"));
        assert!(!is_complete("if true {\n"));
    }

    #[test]
    fn test_is_complete_closed() {
        assert!(is_complete("fn add(a: int) -> int { a }\n"));
    }

    #[test]
    fn test_build_program() {
        let repl = Repl {
            accumulated_fns: vec!["fn double(x: int) -> int { return x * 2; }".to_string()],
            accumulated_types: vec![],
            clang: "clang".to_string(),
        };
        let prog = repl.build_program("print_int(double(21));", WrapMode::Statement);
        assert!(prog.contains("fn double(x: int) -> int"));
        assert!(prog.contains("fn main() -> int"));
        assert!(prog.contains("print_int(double(21));"));
    }
}
