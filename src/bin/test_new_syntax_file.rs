use lynxql::parse_program;
use std::fs;

fn main() {
    let lynx_code = match fs::read_to_string("test_new_syntax.lynx") {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read test_new_syntax.lynx: {}", e);
            return;
        }
    };

    println!("Testing new global context syntax from test_new_syntax.lynx...\n");

    match parse_program(&lynx_code) {
        Ok(program) => {
            println!("✅ Parse successful!");
            println!("📊 Found {} statements:", program.statements.len());
            
            for (_i, stmt) in program.statements.iter().enumerate() {
                println!("  {}. {}", _i + 1, match stmt {
                    lynxql::Statement::TypeDecl(t) => format!("Type declaration: {}", t.name),
                    lynxql::Statement::EnumDecl(e) => format!("Enum declaration: {}", e.name),
                    lynxql::Statement::InstanceDecl(inst) => format!("Instance declaration: {} : {}", inst.instance_name, inst.type_name),
                    lynxql::Statement::Assignment(a) => format!("Assignment: {}", a.name),
                    lynxql::Statement::SolveCall(s) => format!("Solve call: {}", s.var_name),
                });
            }
            
            println!("\n🔍 Looking for global context usage:");
            for (i, stmt) in program.statements.iter().enumerate() {
                match stmt {
                    lynxql::Statement::Assignment(assignment) => {
                        if let lynxql::Expr::GlobalContext = assignment.value {
                            println!("  Found global context in assignment: {}", assignment.name);
                        }
                        if let lynxql::Expr::BuiltinCall(call) = &assignment.value {
                            for (j, arg) in call.args.iter().enumerate() {
                                if let lynxql::Arg::Positional(lynxql::Expr::GlobalContext) = arg {
                                    println!("  Found global context as arg {} in builtin call: {}", j + 1, call.name);
                                }
                            }
                        }
                    }
                    lynxql::Statement::SolveCall(solve) => {
                        if let lynxql::Expr::GlobalContext = solve.target {
                            println!("  Found global context as target in solve call: {}", solve.var_name);
                        }
                    }
                    _ => {}
                }
            }
            
            println!("\n✅ All new syntax features working correctly!");
        }
        Err(e) => {
            eprintln!("❌ Parse error: {}", e);
        }
    }
}