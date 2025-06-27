use lynxql::{parse_program, typecheck_program, typecheck_program_with_details};
use std::fs;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let filename = if args.len() > 1 {
        &args[1]
    } else {
        "example.lynx"
    };
    
    println!("🔍 Type-checking Lynx file: {}", filename);
    
    // Read the file
    let content = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("❌ Error reading file {}: {}", filename, e);
            return;
        }
    };
    
    // Parse the program
    let program = match parse_program(&content) {
        Ok(program) => {
            println!("✅ Parsing successful! Found {} statements", program.statements.len());
            program
        }
        Err(e) => {
            eprintln!("❌ Parse error: {}", e);
            return;
        }
    };
    
    // Type check the program
    match typecheck_program(&program) {
        Ok(()) => {
            println!("✅ Type checking successful! Program is type-safe.");
            
            // Get detailed type information
            if let Ok(env) = typecheck_program_with_details(&program) {
                println!("\n📊 Type Environment Summary:");
                println!("  - Types declared: {}", env.types.len());
                println!("  - Enums declared: {}", env.enums.len());
                println!("  - Variables: {}", env.variables.len());
                println!("  - Instances: {}", env.instances.len());
                
                if !env.types.is_empty() {
                    println!("\n🏗️  Declared Types:");
                    for (name, type_decl) in &env.types {
                        println!("  - {}: {} (with {} fields)", 
                                name, 
                                type_decl.base_type.to_string(),
                                type_decl.fields.len());
                    }
                }
                
                if !env.enums.is_empty() {
                    println!("\n🔢 Declared Enums:");
                    for (name, enum_decl) in &env.enums {
                        println!("  - {} (with {} variants)", name, enum_decl.variants.len());
                    }
                }
                
                if !env.instances.is_empty() {
                    println!("\n🎯 Instances:");
                    for (name, instance_type) in &env.instances {
                        println!("  - {}: {}", name, instance_type.to_string());
                    }
                }
            }
        }
        Err(errors) => {
            eprintln!("❌ Type checking failed with {} error(s):", errors.len());
            for (i, error) in errors.iter().enumerate() {
                eprintln!("  {}. {}", i + 1, error);
            }
        }
    }
}