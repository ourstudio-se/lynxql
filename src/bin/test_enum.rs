use lynxql::parse_program;

fn main() {
    let test_cases = vec![
        r#"enum Material {
            Steel,
            Wood,
            Plastic
        }"#,
        r#"enum Size {
            Small = 1,
            Medium = 2,
            Large = 3
        }"#,
        r#"test = Material.Steel"#,
        r#"type Hammer: bool {
            material: Material,
            size: int
        }"#,
        r#"Hammer hammer1 {
            material: Material.Steel,
            size: 10
        }"#,
    ];

    for (i, code) in test_cases.iter().enumerate() {
        println!("Test case {}: {}", i + 1, code.replace("\n", " "));
        match parse_program(code) {
            Ok(program) => {
                println!("✅ Parse successful!");
                println!("   Found {} statements", program.statements.len());
                for stmt in &program.statements {
                    match stmt {
                        lynxql::Statement::EnumDecl(enum_decl) => {
                            println!("   Enum '{}' with {} variants", enum_decl.name, enum_decl.variants.len());
                            for variant in &enum_decl.variants {
                                if let Some(value) = variant.value {
                                    println!("     - {} = {}", variant.name, value);
                                } else {
                                    println!("     - {}", variant.name);
                                }
                            }
                        }
                        lynxql::Statement::Assignment(assignment) => {
                            match &assignment.value {
                                lynxql::Expr::EnumAccess(enum_access) => {
                                    println!("   Assignment using enum: {}.{}", enum_access.enum_name, enum_access.variant_name);
                                }
                                _ => println!("   Assignment: {}", assignment.name),
                            }
                        }
                        lynxql::Statement::TypeDecl(type_decl) => {
                            println!("   Type declaration: {}", type_decl.name);
                        }
                        lynxql::Statement::InstanceDecl(inst) => {
                            println!("   Instance declaration: {}", inst.instance_name);
                        }
                        _ => {}
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