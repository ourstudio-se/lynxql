use lynxql::{parse_program, Statement, TypeChecker};
use std::fs;

fn main() {
    println!("Testing complete example.lynx parsing...");

    let content = match fs::read_to_string("example.lynx") {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading example.lynx: {}", e);
            return;
        }
    };

    println!("Parsing full example.lynx file...");

    match parse_program(&content) {
        Ok((remaining, program)) => {
            println!("✓ Parse successful!");
            println!("Parsed {} statements", program.statements.len());

            if !remaining.trim().is_empty() {
                println!("⚠ Remaining unparsed content (first 200 chars):");
                let preview = if remaining.len() > 200 {
                    &remaining[..200]
                } else {
                    remaining
                };
                println!("{}", preview);
            }

            // Analyze parsed statements
            let mut type_decls = 0;
            let mut assignments = 0;
            let mut aliases = 0;
            let mut solve_statements = 0;

            for (i, stmt) in program.statements.iter().enumerate() {
                match stmt {
                    Statement::TypeDecl(type_decl) => {
                        type_decls += 1;
                        println!(
                            "Statement {}: Type '{}' with {} fields",
                            i + 1,
                            type_decl.name,
                            type_decl.fields.len()
                        );
                    }
                    Statement::Assignment(assignment) => {
                        assignments += 1;
                        println!("Statement {}: Assignment to '{}'", i + 1, assignment.name);
                    }
                    Statement::Alias(alias) => {
                        aliases += 1;
                        println!(
                            "Statement {}: Alias '{}' = {:?}",
                            i + 1,
                            alias.name,
                            alias.target
                        );
                    }
                    Statement::Solve(solve) => {
                        solve_statements += 1;
                        println!(
                            "Statement {}: Solve '{}' using {:?}",
                            i + 1,
                            solve.name,
                            solve.method
                        );
                    }
                }
            }

            println!("\nStatement Summary:");
            println!("  Type declarations: {}", type_decls);
            println!("  Assignments: {}", assignments);
            println!("  Aliases: {}", aliases);
            println!("  Solve statements: {}", solve_statements);
            println!("  Total: {}", program.statements.len());

            // Test type checking
            println!("\nRunning type checker...");
            let mut type_checker = TypeChecker::new();
            match type_checker.check_program(&program) {
                Ok(()) => println!("✓ Type check: PASS"),
                Err(e) => println!("✗ Type check: FAIL - {}", e),
            }

            // Test specific constructs from the example
            test_specific_constructs(&program);
        }
        Err(e) => {
            println!("✗ Parse failed: {:?}", e);
        }
    }
}

fn test_specific_constructs(program: &lynxql::Program) {
    println!("\nTesting specific constructs from example.lynx:");

    // Look for specific expected constructs
    let mut found_color_type = false;
    let mut found_wheel_type = false;
    let mut found_price_type = false;
    let mut found_bicycle_type = false;
    let mut found_solve_statement = false;

    for stmt in &program.statements {
        match stmt {
            Statement::TypeDecl(type_decl) => match type_decl.name.as_str() {
                "Color" => {
                    found_color_type = true;
                    println!(
                        "  ✓ Found Color type with {} fields",
                        type_decl.fields.len()
                    );
                }
                "Wheel" => {
                    found_wheel_type = true;
                    println!(
                        "  ✓ Found Wheel type with {} fields",
                        type_decl.fields.len()
                    );
                }
                "Price" => {
                    found_price_type = true;
                    println!(
                        "  ✓ Found Price type with {} fields",
                        type_decl.fields.len()
                    );
                }
                "Bicycle" => {
                    found_bicycle_type = true;
                    println!(
                        "  ✓ Found Bicycle type with {} fields",
                        type_decl.fields.len()
                    );
                }
                _ => {}
            },
            Statement::Solve(_) => {
                found_solve_statement = true;
                println!("  ✓ Found solve statement");
            }
            _ => {}
        }
    }

    // Report missing constructs
    if !found_color_type {
        println!("  ✗ Color type not found");
    }
    if !found_wheel_type {
        println!("  ✗ Wheel type not found");
    }
    if !found_price_type {
        println!("  ✗ Price type not found");
    }
    if !found_bicycle_type {
        println!("  ✗ Bicycle type not found");
    }
    if !found_solve_statement {
        println!("  ✗ Solve statement not found");
    }

    println!("\nExpected constructs summary:");
    println!(
        "  Found {}/5 expected major constructs",
        [
            found_color_type,
            found_wheel_type,
            found_price_type,
            found_bicycle_type,
            found_solve_statement
        ]
        .iter()
        .filter(|&&x| x)
        .count()
    );
}
