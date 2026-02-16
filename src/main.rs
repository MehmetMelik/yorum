use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    match args[1].as_str() {
        "compile" => cmd_compile(&args[2..]),
        "check" => cmd_check(&args[2..]),
        "ast" => cmd_ast(&args[2..]),
        "build" => cmd_build(&args[2..]),
        "init" => cmd_init(&args[2..]),
        "lsp" => cmd_lsp(),
        "help" | "--help" | "-h" => print_usage(),
        "version" | "--version" | "-v" => {
            println!("yorum 0.8.0");
        }
        other => {
            // If it ends with .yrm, treat it as an implicit compile
            if other.ends_with(".yrm") {
                cmd_compile(&args[1..]);
            } else {
                eprintln!("error: unknown command '{}'\n", other);
                print_usage();
                process::exit(1);
            }
        }
    }
}

fn print_usage() {
    println!(
        "Yorum — LLM-first, deterministic, LLVM-compiled language\n\
         \n\
         Usage:\n\
         \x20 yorum compile <file.yrm> [-o output]   Compile to LLVM IR\n\
         \x20 yorum check   <file.yrm>                Type-check only\n\
         \x20 yorum ast     <file.yrm>                Print AST as JSON\n\
         \x20 yorum build   [-o output]                Build project (requires yorum.toml)\n\
         \x20 yorum init    [name]                     Initialize a new project\n\
         \x20 yorum lsp                                Start LSP server (stdin/stdout)\n\
         \x20 yorum help                               Show this message\n\
         \x20 yorum version                            Show version\n\
         \n\
         To produce a native binary from LLVM IR:\n\
         \x20 yorum compile program.yrm -o program.ll\n\
         \x20 clang -x ir program.ll -o program -Wno-override-module\n\
         \n\
         Multi-file projects:\n\
         \x20 yorum init myproject && cd myproject\n\
         \x20 yorum build -o out.ll"
    );
}

fn cmd_lsp() {
    let mut server = yorum::lsp::server::LspServer::new();
    if let Err(e) = server.run() {
        eprintln!("LSP server error: {}", e);
        process::exit(1);
    }
}

fn cmd_compile(args: &[String]) {
    let (file, output) = parse_file_and_output(args);

    let source = read_source(&file);
    match yorum::compile_to_ir(&source) {
        Ok(ir) => {
            if let Some(out_path) = output {
                fs::write(&out_path, &ir).unwrap_or_else(|e| {
                    eprintln!("error: cannot write '{}': {}", out_path, e);
                    process::exit(1);
                });
                eprintln!("wrote {}", out_path);
            } else {
                print!("{}", ir);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}

fn cmd_check(args: &[String]) {
    let (file, _) = parse_file_and_output(args);
    let source = read_source(&file);

    match yorum::typecheck(&source) {
        Ok(()) => {
            eprintln!("ok: {} passed all checks", file);
        }
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}

fn cmd_ast(args: &[String]) {
    let (file, output) = parse_file_and_output(args);
    let source = read_source(&file);

    match yorum::source_to_ast_json(&source) {
        Ok(json) => {
            if let Some(out_path) = output {
                fs::write(&out_path, &json).unwrap_or_else(|e| {
                    eprintln!("error: cannot write '{}': {}", out_path, e);
                    process::exit(1);
                });
            } else {
                println!("{}", json);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}

fn cmd_build(args: &[String]) {
    let mut output = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("error: -o requires an argument");
                    process::exit(1);
                }
                output = Some(args[i].clone());
            }
            _ => {
                eprintln!("warning: unknown option '{}'", args[i]);
            }
        }
        i += 1;
    }

    // Find yorum.toml in current directory or parent directories
    let root_dir = find_project_root().unwrap_or_else(|| {
        eprintln!("error: no yorum.toml found in current or parent directories");
        eprintln!("hint: run 'yorum init' to create a new project");
        process::exit(1);
    });

    match yorum::compile_project(&root_dir) {
        Ok(ir) => {
            if let Some(out_path) = output {
                fs::write(&out_path, &ir).unwrap_or_else(|e| {
                    eprintln!("error: cannot write '{}': {}", out_path, e);
                    process::exit(1);
                });
                eprintln!("wrote {}", out_path);
            } else {
                print!("{}", ir);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}

fn cmd_init(args: &[String]) {
    let name = if args.is_empty() {
        // Use current directory name
        env::current_dir()
            .ok()
            .and_then(|p| p.file_name().map(|n| n.to_string_lossy().to_string()))
            .unwrap_or_else(|| "myproject".to_string())
    } else {
        args[0].clone()
    };

    let project_dir = if args.is_empty() {
        std::path::PathBuf::from(".")
    } else {
        std::path::PathBuf::from(&name)
    };

    // Create directory structure
    let src_dir = project_dir.join("src");
    if !args.is_empty() {
        fs::create_dir_all(&src_dir).unwrap_or_else(|e| {
            eprintln!("error: cannot create directory: {}", e);
            process::exit(1);
        });
    } else {
        fs::create_dir_all(&src_dir).unwrap_or_else(|e| {
            eprintln!("error: cannot create src directory: {}", e);
            process::exit(1);
        });
    }

    // Write yorum.toml
    let manifest = format!(
        "[package]\n\
         name = \"{}\"\n\
         version = \"0.1.0\"\n",
        name
    );
    let manifest_path = project_dir.join("yorum.toml");
    if manifest_path.exists() {
        eprintln!("error: yorum.toml already exists");
        process::exit(1);
    }
    fs::write(&manifest_path, manifest).unwrap_or_else(|e| {
        eprintln!("error: cannot write yorum.toml: {}", e);
        process::exit(1);
    });

    // Write src/main.yrm
    let main_source = "module main;\n\n\
         fn main() -> int {\n\
         \x20   print_int(42);\n\
         \x20   return 0;\n\
         }\n"
    .to_string();
    let main_path = src_dir.join("main.yrm");
    fs::write(&main_path, main_source).unwrap_or_else(|e| {
        eprintln!("error: cannot write main.yrm: {}", e);
        process::exit(1);
    });

    if args.is_empty() {
        eprintln!("initialized project '{}' in current directory", name);
    } else {
        eprintln!("created project '{}'", name);
    }
}

fn find_project_root() -> Option<std::path::PathBuf> {
    let mut dir = env::current_dir().ok()?;
    loop {
        if dir.join("yorum.toml").exists() {
            return Some(dir);
        }
        if !dir.pop() {
            return None;
        }
    }
}

fn parse_file_and_output(args: &[String]) -> (String, Option<String>) {
    if args.is_empty() {
        eprintln!("error: no input file specified");
        process::exit(1);
    }

    let file = args[0].clone();
    let mut output = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("error: -o requires an argument");
                    process::exit(1);
                }
                output = Some(args[i].clone());
            }
            _ => {
                eprintln!("warning: unknown option '{}'", args[i]);
            }
        }
        i += 1;
    }

    (file, output)
}

fn read_source(path: &str) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("error: cannot read '{}': {}", path, e);
        process::exit(1);
    })
}
