use lynxql::{parse_program, TypeChecker};
use std::fs;

fn main() {
    println!("Testing parser with example.lynx file...");

    let content = match fs::read_to_string("example.lynx") {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading example.lynx: {}", e);
            return;
        }
    };

    println!("File content:");
    println!("{}", content);
    println!("\n{}", "=".repeat(50));

    match parse_program(&content) {
        Ok((remaining, program)) => {
            println!("Parse successful!");
            if !remaining.trim().is_empty() {
                println!("Remaining unparsed input: {:?}", remaining);
            }

            println!("\nParsed {} statements:", program.statements.len());
            for (i, stmt) in program.statements.iter().enumerate() {
                println!("Statement {}: {:?}", i + 1, stmt);
            }

            println!("\nRunning type checker...");
            let mut type_checker = TypeChecker::new();
            match type_checker.check_program(&program) {
                Ok(()) => println!("Type check: PASS - All types are valid!"),
                Err(e) => println!("Type check: FAIL - {}", e),
            }
        }
        Err(e) => {
            println!("Parse failed: {:?}", e);
        }
    }
}
