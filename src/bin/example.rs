use lynxql::parse_program;
use std::fs;

fn main() {
    let lynx_code = match fs::read_to_string("example.lynx") {
        Ok(content) => content,
        Err(_) => {
            // Fallback to a simple example if file doesn't exist
            r#"
// Simple type alias
type Size: int

// Primitive type with properties
type Hammer: bool {
    material: string,
    size: Size,
    cost: float
}

// Instance declaration using new syntax
Hammer hammer1 {
    material: "steel",
    size: 10,
    cost: 25.0
}

// Assignment with field access
test_cost = hammer1.cost

// Solve call
result = solve(hammer1, { hammer1: 1.0 })
            "#.to_string()
        }
    };

    println!("Parsing Lynx code...\n");
    
    match parse_program(&lynx_code) {
        Ok(program) => {
            println!("✅ Parse successful!");
            println!("📊 Found {} statements:", program.statements.len());
            
            for (i, stmt) in program.statements.iter().enumerate() {
                println!("  {}. {}", i + 1, match stmt {
                    lynxql::Statement::TypeDecl(t) => format!("Type declaration: {}", t.name),
                    lynxql::Statement::EnumDecl(e) => format!("Enum declaration: {}", e.name),
                    lynxql::Statement::InstanceDecl(inst) => format!("Instance declaration: {} : {}", inst.instance_name, inst.type_name),
                    lynxql::Statement::Assignment(a) => format!("Assignment: {}", a.name),
                    lynxql::Statement::SolveCall(_) => "Solve call".to_string(),
                });
            }
            
            println!("\n🔍 AST structure:");
            println!("{:#?}", program);
        }
        Err(e) => {
            eprintln!("❌ Parse error: {}", e);
        }
    }
}