use lynxql::parse_program;

fn main() {
    let simple_test = r#"test = match({
        _: 5
    })"#;
    
    println!("Testing simple match: {}", simple_test.replace("\n", " "));
    match parse_program(simple_test) {
        Ok(program) => {
            println!("✅ Parse successful!");
            println!("AST: {:#?}", program);
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }
}