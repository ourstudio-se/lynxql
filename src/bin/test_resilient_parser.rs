use lynxql::{parse_program, typecheck_program_with_details};
use std::fs;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let filename = if args.len() > 1 {
        &args[1]
    } else {
        "test_lsp.lynx"
    };
    
    println!("🔍 Testing resilient parser on: {}", filename);
    
    // Read the file
    let content = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("❌ Error reading file {}: {}", filename, e);
            return;
        }
    };
    
    // Parse with standard parser
    let parse_result = match parse_program(&content) {
        Ok(program) => program,
        Err(e) => {
            eprintln!("❌ Parse error: {}", e);
            return;
        }
    };
    
    println!("✅ Parsing complete!");
    println!("📊 Parse Results:");
    println!("  - Statements parsed: {}", parse_result.statements.len());
    
    // Show what statements were successfully parsed
    println!("\n📝 Successfully Parsed Statements:");
    for (i, statement) in parse_result.statements.iter().enumerate() {
        let stmt_type = match statement {
            lynxql::Statement::TypeDecl(type_decl) => format!("Type: {}", type_decl.name),
            lynxql::Statement::EnumDecl(enum_decl) => format!("Enum: {}", enum_decl.name),
            lynxql::Statement::InstanceDecl(instance_decl) => format!("Instance: {} of {}", instance_decl.instance_name, instance_decl.type_name),
            lynxql::Statement::Assignment(assignment) => format!("Assignment: {}", assignment.name),
            lynxql::Statement::SolveCall(solve_call) => format!("Solve: {}", solve_call.var_name),
        };
        println!("  {}. {}", i + 1, stmt_type);
    }
    
    // Try type checking on the parsed result
    println!("\n🔍 Type Checking Results:");
    match typecheck_program_with_details(&parse_result) {
        Ok(env) => {
            println!("✅ Type checking successful!");
            println!("📊 Type Environment:");
            println!("  - Types: {}", env.types.len());
            println!("  - Enums: {}", env.enums.len());
            println!("  - Variables: {}", env.variables.len());
            println!("  - Instances: {}", env.instances.len());
        }
        Err(errors) => {
            println!("❌ Type checking found {} error(s):", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("  {}. {}", i + 1, error);
            }
        }
    }
}