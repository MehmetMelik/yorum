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
        "help" | "--help" | "-h" => print_usage(),
        "version" | "--version" | "-v" => {
            println!("yorum 0.1.0");
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
         \x20 yorum help                              Show this message\n\
         \x20 yorum version                           Show version\n\
         \n\
         To produce a native binary from LLVM IR:\n\
         \x20 yorum compile program.yrm -o program.ll\n\
         \x20 llc -filetype=obj program.ll -o program.o\n\
         \x20 clang program.o -o program\n\
         \n\
         Or in one pipeline:\n\
         \x20 yorum compile program.yrm | llc -filetype=obj | clang -x assembler - -o program"
    );
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
