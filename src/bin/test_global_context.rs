use lynxql::parse_program;

fn main() {
    let test_cases = vec![
        "test = *",
        "result = find(*, (h: Hammer) -> h.size >= 8)",
        "solution = solve(*, { nail1: 1.0 })",
    ];

    for (i, code) in test_cases.iter().enumerate() {
        println!("Test case {}: {}", i + 1, code);
        match parse_program(code) {
            Ok(program) => {
                println!("✅ Parse successful!");
                println!("   Found {} statements", program.statements.len());
                if let Some(stmt) = program.statements.first() {
                    println!("   First statement: {:#?}", stmt);
                }
            }
            Err(e) => {
                println!("❌ Parse error: {}", e);
            }
        }
        println!();
    }
}