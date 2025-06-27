use lynxql::parse_program;

fn main() {
    let test_cases = vec![
        r#"test = match({
            _: "other"
        })"#,
        r#"weight = (h: Hammer) -> match({
            _: 5
        })"#,
    ];

    for (i, code) in test_cases.iter().enumerate() {
        println!("Test case {}: {}", i + 1, code.replace("\n", " "));
        match parse_program(code) {
            Ok(program) => {
                println!("✅ Parse successful!");
                println!("   Found {} statements", program.statements.len());
                if let Some(stmt) = program.statements.first() {
                    if let lynxql::Statement::Assignment(assignment) = stmt {
                        match &assignment.value {
                            lynxql::Expr::Match(match_expr) => {
                                println!("   Match expression with {} cases", match_expr.cases.len());
                            }
                            lynxql::Expr::Lambda(lambda) => {
                                match &*lambda.body {
                                    lynxql::Expr::Match(match_expr) => {
                                        println!("   Lambda with match expression containing {} cases", match_expr.cases.len());
                                    }
                                    _ => println!("   Lambda with other body type"),
                                }
                            }
                            _ => println!("   Other expression type"),
                        }
                    }
                }
            }
            Err(e) => {
                println!("❌ Parse error: {}", e);
            }
        }
        println!();
    }
}