use lynxql::{parse_program, TypeChecker};

fn main() {
    let test_inputs = vec![
        "x = Bool()",
        "y = Integer(0, 5)",
        "z = -2..1",
        "a = All(x, y, z)",
        "type Color : Bool { @tag : str }",
        "red = Color(tag = \"red\")",
        "alias Week = Integer<1,52>",
        "solution = minimize(objective = myBike.totalPrice, suchThat = All(myBike))",
    ];

    for (i, input) in test_inputs.iter().enumerate() {
        println!("=== Test {} ===", i + 1);
        println!("Input: {}", input);

        match parse_program(input) {
            Ok((remaining, program)) => {
                println!("Parse successful!");
                println!("Remaining input: {:?}", remaining);
                println!("AST: {:#?}", program);

                let mut type_checker = TypeChecker::new();
                match type_checker.check_program(&program) {
                    Ok(()) => println!("Type check: PASS"),
                    Err(e) => println!("Type check: FAIL - {}", e),
                }
            }
            Err(e) => {
                println!("Parse failed: {:?}", e);
            }
        }
        println!();
    }
}
