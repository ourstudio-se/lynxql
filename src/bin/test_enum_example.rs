use lynxql::parse_program;
use std::fs;

fn main() {
    let lynx_code = match fs::read_to_string("example_with_enums.lynx") {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read example_with_enums.lynx: {}", e);
            return;
        }
    };

    println!("Testing comprehensive enum usage from example_with_enums.lynx...\n");

    match parse_program(&lynx_code) {
        Ok(program) => {
            println!("✅ Parse successful!");
            println!("📊 Found {} statements:", program.statements.len());
            
            let mut enum_count = 0;
            let mut type_count = 0;
            let mut instance_count = 0;
            let mut assignment_count = 0;
            let mut solve_count = 0;
            
            for (i, stmt) in program.statements.iter().enumerate() {
                println!("  {}. {}", i + 1, match stmt {
                    lynxql::Statement::EnumDecl(e) => {
                        enum_count += 1;
                        format!("Enum declaration: {} ({} variants)", e.name, e.variants.len())
                    },
                    lynxql::Statement::TypeDecl(t) => {
                        type_count += 1;
                        format!("Type declaration: {}", t.name)
                    },
                    lynxql::Statement::InstanceDecl(inst) => {
                        instance_count += 1;
                        format!("Instance declaration: {} : {}", inst.instance_name, inst.type_name)
                    },
                    lynxql::Statement::Assignment(a) => {
                        assignment_count += 1;
                        format!("Assignment: {}", a.name)
                    },
                    lynxql::Statement::SolveCall(s) => {
                        solve_count += 1;
                        format!("Solve call: {}", s.var_name)
                    },
                });
            }
            
            println!("\n📈 Summary:");
            println!("  - {} enum declarations", enum_count);
            println!("  - {} type declarations", type_count);
            println!("  - {} instance declarations", instance_count);
            println!("  - {} assignments", assignment_count);
            println!("  - {} solve calls", solve_count);
            
            println!("\n🔍 Enums found:");
            for stmt in &program.statements {
                if let lynxql::Statement::EnumDecl(enum_decl) = stmt {
                    println!("  📦 {}: {:?}", enum_decl.name, 
                             enum_decl.variants.iter().map(|v| &v.name).collect::<Vec<_>>());
                }
            }
            
            println!("\n✅ Enum system working correctly in complex scenario!");
        }
        Err(e) => {
            eprintln!("❌ Parse error: {}", e);
        }
    }
}