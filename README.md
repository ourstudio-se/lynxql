# 🦊 LynxQL - Declarative Modeling Language

LynxQL is a **declarative modeling language** for defining and solving **combinatorial optimization problems** using **Integer Linear Programming (ILP)**. It features a complete Rust parser implementation with real-time type checking and VSCode extension support.

## ✨ Key Features

### Language Features
* **Declarative syntax** — no loops, no side effects
* **Strong typing** with support for primitive and composite types
* **Enum support** with type-safe variant checking
* **Logic composition** using built-ins like `All`, `Any`, `AtLeast`, `Not`, etc.
* **Lambda expressions** with type inference
* **Built-in functions**: `find`, `match`, `sum`, `solve`, and more
* **ILP-compatible**: all model logic compiles to linear formulations

### Implementation Features  
* **Complete Rust parser** using `nom` parsing combinators
* **Comprehensive type checker** with detailed error reporting
* **Language Server Protocol (LSP)** support for real-time diagnostics
* **VSCode extension** with syntax highlighting and intelligent error positioning
* **Resilient parsing** that continues after errors to show multiple issues

---

## 📦 Quick Example

```lynx
enum Material {
    Steel,
    Wood,
    Plastic
}

type Hammer: bool {
    material: Material,
    size: int,
    cost: float
}

type Toolbox: All {
    hammers: AtLeast<1>[Hammer] = (_) -> find((h: Hammer) -> h.size >= 8),
    nails: Any[Nail]?,
    weight: int = (t: Toolbox) -> match({
        Any(find((h: Hammer) -> h.material == Material.Steel)): 10,
        Any(find((h: Hammer) -> h.material == Material.Wood)): 7,
        _: 5
    })
}

type Carpenter: All {
    name: string,
    age: int,
    workable: bool = (c: Carpenter) -> c.age >= 18,
    toolbox: Toolbox,
    salary: float = (c: Carpenter) -> 20000.0 - sum(c.toolbox.hammers.cost)
}

Hammer hammer1 {
    material: Material.Steel,
    size: 12,
    cost: 25.50
}

Carpenter john {
    name: "John Doe",
    age: 30,
    toolbox: Toolbox {
        hammers: AtLeast<1> { hammer1 }
    }
}

solution = solve(john, { hammer1: 1.0 }, { Not { hammer1 } })
```

---

## 🚀 Getting Started

### Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/lynx-lang/lynxql
   cd lynxql
   ```

2. **Build the parser**:
   ```bash
   cargo build --release
   ```

3. **Install VSCode extension** (optional):
   ```bash
   cd vscode-lynx-extension
   code --install-extension lynx-language-4.0.6.vsix
   ```

### Usage

#### Parse and Type-check Files
```bash
# Parse a Lynx file
cargo run --bin debug_parser your_file.lynx

# Type-check with detailed output
cargo run --bin typecheck_example your_file.lynx
```

#### Use as Rust Library
```rust
use lynxql::{parse_program, typecheck_program_with_details};

let lynx_code = r#"
type Hammer: bool {
    material: string,
    size: int,
    cost: float
}

Hammer hammer1 {
    material: "steel",
    size: 12,
    cost: 25.50
}
"#;

match parse_program(lynx_code) {
    Ok(program) => {
        match typecheck_program_with_details(&program) {
            Ok(env) => {
                println!("✅ Program is type-safe!");
                println!("Types: {}, Variables: {}", env.types.len(), env.variables.len());
            }
            Err(errors) => {
                for error in errors {
                    eprintln!("❌ Type error: {}", error);
                }
            }
        }
    }
    Err(e) => eprintln!("❌ Parse error: {}", e),
}
```

#### Language Server (LSP)
```bash
# Start LSP server for IDE integration
cargo run --bin lynx-lsp
```

---

## 🧠 Language Concepts

### 🧱 Type System

#### Primitive Types
* `bool` - Boolean values
* `int` - Integer values  
* `float` - Floating point values
* `string` - String values

#### Named Types
```lynx
type Size: int
type Product: All {
    options: Exactly<1>[Option],
    price: float = (p: Product) -> sum(p.options.price)
}
```

#### Enum Types
```lynx
enum Material {
    Steel,
    Wood,
    Plastic
}

type Tool: bool {
    material: Material  // Type-safe enum usage
}
```

### 🔗 Logic Types & Relationships

Logic types express cardinality and relationship constraints:

* `All[T]` – all connected instances must be satisfied
* `Any[T]` – at least one must be satisfied  
* `Exactly<N>[T]` – exactly N instances must be selected
* `AtLeast<N>[T]` – at least N instances must be selected
* `AtMost<N>[T]` – at most N instances must be selected
* `Not[T]` – negation of the condition
* Optional relationships use `?` after the type (e.g. `Any[Nail]?`)

### 🪮 Computed Properties

Properties can be computed using pure, side-effect-free lambda expressions:

```lynx
type Toolbox: All {
    hammers: Any[Hammer],
    total_cost: float = (t: Toolbox) -> sum(t.hammers.cost),
    weight: int = (t: Toolbox) -> match({
        Any(find((h: Hammer) -> h.material == Material.Steel)): 10,
        _: 5
    })
}
```

### 🔍 Find & Filter

Use `find((x: Type) -> condition)` to dynamically match instances:

```lynx
heavy_hammers = find((h: Hammer) -> h.size >= 10)
steel_tools = find((t: Tool) -> t.material == Material.Steel)
```

### 🧲 Match Expressions

Use `match` for piecewise logic with multiple conditions:

```lynx
shipping_cost: float = (order: Order) -> match({
    order.weight <= 5: 10.0,
    order.weight <= 20: 25.0,
    order.priority == "express": 50.0,
    _: 35.0
})
```

### 🚀 Optimization

The `solve` function initiates optimization with:
* **Target variable** to optimize
* **Weighted objectives** (what to maximize/minimize)  
* **Constraints** (what must be satisfied)

```lynx
solution = solve(
    john,                           // Target variable
    { hammer1: 1.0, nail1: 0.5 },  // Objectives with weights
    { Not { expensive_tool } }      // Constraints
)
```

---

## 🔧 Built-in Functions

| Function         | Description                                         | Example |
| ---------------- | --------------------------------------------------- | ------- |
| `solve(var, obj, constraints)` | Optimize a variable with objectives and constraints | `solve(john, {hammer1: 1.0}, {Not{tool2}})` |
| `find((x: T) -> condition)` | Select instances based on filter | `find((h: Hammer) -> h.size > 10)` |
| `sum(collection)` | Add numeric values over a collection | `sum(toolbox.hammers.cost)` |
| `match({conditions})` | Piecewise logic mapping | `match({x > 10: "big", _: "small"})` |
| `propagate(logic)` | Forward-evaluate logical implications | `propagate(All{hammer1, hammer2})` |
| `first(collection)` | Get the first element from a collection | `first(available_tools)` |

---

## 🏗️ Parser & Type Checker

### Architecture

The LynxQL implementation uses:
* **`nom` parsing combinators** for robust, composable parsing
* **Complete AST representation** of all language constructs
* **Comprehensive type checker** with detailed error reporting
* **Resilient parsing** that recovers from errors to show multiple issues

### AST Structure
* `Program` - Root node containing multiple statements
* `Statement` - Top-level constructs (TypeDecl, InstanceDecl, Assignment, SolveCall)
* `TypeDecl` - Type declarations with logic types and field specifications  
* `Expr` - Expressions including literals, lambdas, logic expressions, constructors
* `LogicType` - Logic types (All, Any, Not, Boolean, Integer, IntegerRange, etc.)

### Type Checking Features
* **Complete type system** validation for all Lynx constructs
* **Enum validation** ensuring valid variants and type compatibility
* **Lambda type inference** with parameter and return type checking
* **Field validation** for assignments, required fields, and optional fields
* **Logic type support** for all cardinality and relationship operators
* **Comprehensive error reporting** with detailed, actionable messages

### Error Types

The type checker provides detailed errors through `TypeCheckError`:

* `UndefinedType` - Reference to undefined type
* `UndefinedEnumVariant` - Invalid enum variant usage
* `TypeMismatch` - Type incompatibility 
* `FieldNotFound` - Unknown field in type
* `MissingRequiredField` - Required field not provided
* `InvalidLambda` - Lambda expression errors
* `InvalidLogicExpression` - Logic type constraint errors

---

## 💻 VSCode Extension

The VSCode extension provides:

* **Complete syntax highlighting** for all Lynx constructs
* **Real-time type checking** with LSP integration
* **Intelligent error positioning** showing errors at exact source locations
* **Auto-completion** for types, fields, and built-in functions
* **Custom color theme** optimized for Lynx code
* **Multi-comment support** (`//` and `/* */`)

### Installation
1. Download `lynx-language-4.0.6.vsix` from releases
2. Install via VSCode: Extensions → Install from VSIX
3. Configure LSP server path in settings

---

## 📜 Design Constraints

LynxQL compiles to **Integer Linear Programs**, which enforces:

* ❌ No loops or recursion
* ❌ No nonlinear math (e.g. `x * y`, `x / y`) 
* ❌ No side effects or mutations
* ✅ All functions must return **linear-compatible** results
* ✅ All expressions are **deterministic and pure**
* ✅ All logic must be expressible as linear constraints

---

## 📃 File Format

* **Extension**: `.lynx`
* **Comments**: 
  * Single-line: `// comment`
  * Multi-line: `/* block comment */`
* **Encoding**: UTF-8

---

## 🔧 Development Commands

### Building and Testing
```bash
cargo build                    # Build the project
cargo test                     # Run all tests  
cargo check                    # Check code without building
cargo clippy                   # Run linting
cargo fmt                      # Format code
```

### Parser Testing
```bash
cargo run --bin debug_parser   # Test parser on specific inputs
cargo run --bin test_example   # Test example.lynx constructs
```

### Documentation
```bash
cargo doc --open              # Generate and open documentation
```

---

## 🛣️ Roadmap

* [x] Complete Rust parser with `nom`
* [x] Comprehensive type checker  
* [x] Enum type support with validation
* [x] Lambda expressions with type inference
* [x] Language Server Protocol (LSP) implementation
* [x] VSCode extension with real-time diagnostics
* [x] Intelligent error positioning
* [x] Resilient parsing for multiple error reporting
* [ ] Static linearity checker for ILP compatibility
* [ ] Integration with ILP solvers
* [ ] Advanced type inference and shape analysis
* [ ] Performance optimizations
* [ ] Additional IDE integrations

---

## 📚 Learn More

* **Examples**: See `example.lynx` and `example_with_enums.lynx`
* **Grammar**: Reference `grammar.lark` for complete syntax specification
* **VSCode Extension**: Check `vscode-lynx-extension/` directory
* **API Documentation**: Run `cargo doc --open`

---

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality  
4. Ensure all tests pass with `cargo test`
5. Run `cargo clippy` and `cargo fmt`
6. Submit a pull request

---

## 📄 License

MIT License - see LICENSE file for details.