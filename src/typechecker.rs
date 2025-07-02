use crate::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum TypeCheckError {
    #[error("Type error: {0}")]
    TypeError(String),
    #[error("Undefined type: {0}")]
    UndefinedType(String),
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("Undefined enum: {0}")]
    UndefinedEnum(String),
    #[error("Undefined enum variant: {0}::{1}")]
    UndefinedEnumVariant(String, String),
    #[error("Field not found: {0} in type {1}")]
    FieldNotFound(String, String),
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: String, found: String },
    #[error("Invalid field assignment: {field} in {type_name}")]
    InvalidFieldAssignment { field: String, type_name: String },
    #[error("Missing required field: {field} in {type_name}")]
    MissingRequiredField { field: String, type_name: String },
    #[error("Invalid logic expression: {0}")]
    InvalidLogicExpression(String),
    #[error("Invalid builtin call: {0}")]
    InvalidBuiltinCall(String),
    #[error("Invalid lambda expression: {0}")]
    InvalidLambda(String),
    #[error("Invalid solve call: {0}")]
    InvalidSolveCall(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Logic { logic_type: LogicType, inner: Box<Type> },
    Named(String),
    Function { param: Box<Type>, return_type: Box<Type> },
    Unknown,
}

impl Type {
    pub fn to_string(&self) -> String {
        match self {
            Type::Primitive(p) => match p {
                PrimitiveType::Bound(from, to) => format!("{}..{}", from, to),
                PrimitiveType::Int => "int".to_string(),
                PrimitiveType::Float => "float".to_string(),
                PrimitiveType::String => "string".to_string(),
                PrimitiveType::Bool => "bool".to_string(),
                PrimitiveType::Named(name) => name.clone(),
            },
            Type::Logic { logic_type, inner } => {
                let logic_str = match logic_type {
                    LogicType::All => "All",
                    LogicType::Any => "Any",
                    LogicType::Not => "Not",
                    LogicType::Exactly(n) => return format!("Exactly<{}>[{}]", n, inner.to_string()),
                    LogicType::AtLeast(n) => return format!("AtLeast<{}>[{}]", n, inner.to_string()),
                    LogicType::AtMost(n) => return format!("AtMost<{}>[{}]", n, inner.to_string()),
                };
                format!("{}[{}]", logic_str, inner.to_string())
            },
            Type::Named(name) => name.clone(),
            Type::Function { param, return_type } => {
                format!("({}) -> {}", param.to_string(), return_type.to_string())
            },
            Type::Unknown => "?".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedFieldDecl {
    pub name: String,
    pub field_type: Type,
    pub is_optional: bool,
    pub default: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct TypedTypeDecl {
    pub name: String,
    pub base_type: Type,
    pub fields: Vec<TypedFieldDecl>,
}

#[derive(Debug, Clone)]
pub struct TypedEnumDecl {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug)]
pub struct TypeEnvironment {
    pub types: HashMap<String, TypedTypeDecl>,
    pub enums: HashMap<String, TypedEnumDecl>,
    pub variables: HashMap<String, Type>,
    pub instances: HashMap<String, Type>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        let mut env = TypeEnvironment {
            types: HashMap::new(),
            enums: HashMap::new(),
            variables: HashMap::new(),
            instances: HashMap::new(),
        };
        
        // Add built-in primitive types
        env.add_primitive_types();
        env
    }
    
    fn add_primitive_types(&mut self) {
        // Primitive types are handled directly in type resolution
        // No need to add them to the types map
    }
    
    pub fn add_type(&mut self, type_decl: TypedTypeDecl) {
        self.types.insert(type_decl.name.clone(), type_decl);
    }
    
    pub fn add_enum(&mut self, enum_decl: TypedEnumDecl) {
        self.enums.insert(enum_decl.name.clone(), enum_decl);
    }
    
    pub fn add_variable(&mut self, name: String, var_type: Type) {
        self.variables.insert(name, var_type);
    }
    
    pub fn add_instance(&mut self, name: String, instance_type: Type) {
        self.instances.insert(name, instance_type);
    }
    
    pub fn get_type(&self, name: &str) -> Option<&TypedTypeDecl> {
        self.types.get(name)
    }
    
    pub fn get_enum(&self, name: &str) -> Option<&TypedEnumDecl> {
        self.enums.get(name)
    }
    
    pub fn get_variable(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }
    
    pub fn get_instance(&self, name: &str) -> Option<&Type> {
        self.instances.get(name)
    }
    
    pub fn resolve_type_ref(&self, type_ref: &TypeRef) -> Result<Type, TypeCheckError> {
        match type_ref {
            TypeRef::Primitive(p) => match p {
                PrimitiveType::Named(name) => {
                    // Check if this is actually a declared type or enum
                    if self.types.contains_key(name) || self.enums.contains_key(name) {
                        Ok(Type::Named(name.clone()))
                    } else {
                        Err(TypeCheckError::UndefinedType(name.clone()))
                    }
                }
                _ => Ok(Type::Primitive(p.clone())),
            },
            TypeRef::Logic { logic_type, inner } => {
                let inner_type = self.resolve_type_ref(inner)?;
                Ok(Type::Logic {
                    logic_type: logic_type.clone(),
                    inner: Box::new(inner_type),
                })
            },
            TypeRef::Named(name) => {
                if self.types.contains_key(name) || self.enums.contains_key(name) {
                    Ok(Type::Named(name.clone()))
                } else {
                    Err(TypeCheckError::UndefinedType(name.clone()))
                }
            }
        }
    }
}

pub struct TypeChecker {
    env: TypeEnvironment,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: TypeEnvironment::new(),
        }
    }
    
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<TypeCheckError>> {
        let mut errors = Vec::new();
        
        // First pass: collect all type and enum declarations
        for statement in &program.statements {
            match statement {
                Statement::TypeDecl(type_decl) => {
                    match self.check_type_decl(type_decl) {
                        Ok(typed_decl) => {
                            self.env.add_type(typed_decl);
                        }
                        Err(err) => errors.push(err),
                    }
                }
                Statement::EnumDecl(enum_decl) => {
                    match self.check_enum_decl(enum_decl) {
                        Ok(typed_enum) => {
                            self.env.add_enum(typed_enum);
                        }
                        Err(err) => errors.push(err),
                    }
                }
                _ => {} // Handle other statements in later passes
            }
        }
        
        // Second pass: check default values in type declarations now that all types are registered
        for statement in &program.statements {
            if let Statement::TypeDecl(type_decl) = statement {
                if let Err(err) = self.check_type_defaults(type_decl) {
                    errors.push(err);
                }
            }
        }
        
        // Third pass: check instances, assignments, and solve calls
        for statement in &program.statements {
            match statement {
                // InstanceDecl is now handled through Assignment
                Statement::Assignment(assignment) => {
                    if let Err(err) = self.check_assignment(assignment) {
                        errors.push(err);
                    }
                }
                Statement::SolveCall(solve_call) => {
                    if let Err(err) = self.check_solve_call(solve_call) {
                        errors.push(err);
                    }
                }
                _ => {} // Already handled in previous passes
            }
        }
        
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
    
    fn check_type_decl(&mut self, type_decl: &TypeDecl) -> Result<TypedTypeDecl, TypeCheckError> {
        let base_type = self.convert_base_type(&type_decl.base_type)?;
        let mut typed_fields = Vec::new();
        
        for field in &type_decl.fields {
            let field_type = self.env.resolve_type_ref(&field.type_ref)?;
            
            // Don't check default values yet - we'll do that in a second pass
            // after all types are registered in the environment
            typed_fields.push(TypedFieldDecl {
                name: field.name.clone(),
                field_type,
                is_optional: field.is_optional,
                default: field.default.clone(),
            });
        }
        
        Ok(TypedTypeDecl {
            name: type_decl.name.clone(),
            base_type,
            fields: typed_fields,
        })
    }
    
    fn check_type_defaults(&mut self, type_decl: &TypeDecl) -> Result<(), TypeCheckError> {
        let typed_decl = self.env.get_type(&type_decl.name)
            .ok_or_else(|| TypeCheckError::UndefinedType(type_decl.name.clone()))?
            .clone();
        
        for (field, typed_field) in type_decl.fields.iter().zip(typed_decl.fields.iter()) {
            if let Some(default_expr) = &field.default {
                let expr_type = self.infer_expr_type(default_expr)?;
                
                // If the default is a lambda function, check that its return type matches the field type
                if let Type::Function { return_type, .. } = &expr_type {
                    if !self.types_compatible(&typed_field.field_type, return_type) {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: typed_field.field_type.to_string(),
                            found: return_type.to_string(),
                        });
                    }
                } else {
                    // For non-function defaults, the expression type should match the field type
                    if !self.types_compatible(&typed_field.field_type, &expr_type) {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: typed_field.field_type.to_string(),
                            found: expr_type.to_string(),
                        });
                    }
                }
            }
        }
        
        Ok(())
    }
    
    fn check_enum_decl(&mut self, enum_decl: &EnumDecl) -> Result<TypedEnumDecl, TypeCheckError> {
        // Validate that enum variants don't conflict
        let mut seen_names = std::collections::HashSet::new();
        let mut seen_values = std::collections::HashSet::new();
        
        for variant in &enum_decl.variants {
            if seen_names.contains(&variant.name) {
                return Err(TypeCheckError::TypeError(
                    format!("Duplicate enum variant: {}", variant.name)
                ));
            }
            seen_names.insert(&variant.name);
            
            if let Some(value) = variant.value {
                if seen_values.contains(&value) {
                    return Err(TypeCheckError::TypeError(
                        format!("Duplicate enum value: {}", value)
                    ));
                }
                seen_values.insert(value);
            }
        }
        
        Ok(TypedEnumDecl {
            name: enum_decl.name.clone(),
            variants: enum_decl.variants.clone(),
        })
    }
    
    
    fn check_assignment(&mut self, assignment: &Assignment) -> Result<(), TypeCheckError> {
        // If the assignment has a type_name, it could be either a primitive typed assignment,
        // an enum typed assignment, or an instance declaration for a user-defined type
        if !assignment.type_name.is_empty() {
            // Check if this is a primitive type (int, float, string, bool)
            let is_primitive = matches!(assignment.type_name.as_str(), "int" | "float" | "string" | "bool");
            
            // Check if this is an enum type
            let is_enum = self.env.enums.contains_key(&assignment.type_name);
            
            if is_primitive || is_enum {
                // This is a typed variable assignment: type var_name = expr
                let expected_type = self.resolve_type_name(&assignment.type_name)?;
                let expr_type = self.infer_expr_type(&assignment.value)?;
                
                if !self.types_compatible(&expected_type, &expr_type) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: expected_type.to_string(),
                        found: expr_type.to_string(),
                    });
                }
                
                self.env.add_variable(assignment.name.clone(), expected_type);
                Ok(())
            } else {
                // This is an instance declaration: TypeName instance_name { fields... }
                self.check_instance_assignment(assignment)
            }
        } else {
            // Regular variable assignment: var = expr (shouldn't happen with new syntax)
            let expr_type = self.infer_expr_type(&assignment.value)?;
            self.env.add_variable(assignment.name.clone(), expr_type);
            Ok(())
        }
    }
    
    fn check_instance_assignment(&mut self, assignment: &Assignment) -> Result<(), TypeCheckError> {
        // Get the type declaration
        let type_decl = self.env.get_type(&assignment.type_name)
            .ok_or_else(|| TypeCheckError::UndefinedType(assignment.type_name.clone()))?
            .clone();
            
        // Check the field assignments
        match &assignment.value {
            Expr::AnonymousObjectLiteral(fields) => {
                // Check that all required fields are provided
                let mut provided_fields = std::collections::HashSet::new();
                for field_assign in fields {
                    provided_fields.insert(&field_assign.name);
                    
                    // Find the field in the type declaration
                    let field_decl = type_decl.fields.iter()
                        .find(|f| f.name == field_assign.name)
                        .ok_or_else(|| TypeCheckError::FieldNotFound(
                            field_assign.name.clone(),
                            assignment.type_name.clone()
                        ))?;
                    
                    // Check that the assigned value matches the field type
                    let expr_type = self.infer_expr_type(&field_assign.value)?;
                    if !self.types_compatible(&field_decl.field_type, &expr_type) {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: field_decl.field_type.to_string(),
                            found: expr_type.to_string(),
                        });
                    }
                }
                
                // Check for missing required fields
                for field in &type_decl.fields {
                    if !field.is_optional && field.default.is_none() && !provided_fields.contains(&field.name) {
                        return Err(TypeCheckError::MissingRequiredField {
                            field: field.name.clone(),
                            type_name: assignment.type_name.clone(),
                        });
                    }
                }
                
                // Add instance to environment
                self.env.add_instance(
                    assignment.name.clone(),
                    Type::Named(assignment.type_name.clone())
                );
                
                Ok(())
            }
            _ => {
                // For other expression types, just check compatibility with the type
                let expr_type = self.infer_expr_type(&assignment.value)?;
                let expected_type = Type::Named(assignment.type_name.clone());
                if !self.types_compatible(&expected_type, &expr_type) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: expected_type.to_string(),
                        found: expr_type.to_string(),
                    });
                }
                
                self.env.add_instance(assignment.name.clone(), expected_type);
                Ok(())
            }
        }
    }
    
    fn check_solve_call(&mut self, solve_call: &SolveCall) -> Result<(), TypeCheckError> {
        // Check target expression
        let target_type = self.infer_expr_type(&solve_call.target)?;
        
        // Check objectives (they should reference valid variables/instances)
        for objective in &solve_call.objectives {
            // Validate that the objective name refers to a valid variable or instance
            if self.env.get_variable(&objective.name).is_none() && 
               self.env.get_instance(&objective.name).is_none() {
                return Err(TypeCheckError::UndefinedVariable(objective.name.clone()));
            }
        }
        
        // Check constraints
        for constraint in &solve_call.constraints {
            let constraint_type = self.infer_expr_type(constraint)?;
            // Constraints should be boolean expressions or logic expressions
            match constraint_type {
                Type::Primitive(PrimitiveType::Bool) => {},
                Type::Logic { .. } => {},
                _ => return Err(TypeCheckError::InvalidSolveCall(
                    "Constraints must be boolean or logic expressions".to_string()
                )),
            }
        }
        
        // Add the solve result variable
        self.env.add_variable(solve_call.var_name.clone(), target_type);
        
        Ok(())
    }
    
    fn infer_expr_type(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        match expr {
            Expr::Literal(literal) => Ok(self.infer_literal_type(literal)),
            Expr::Identifier(name) => {
                if let Some(var_type) = self.env.get_variable(name) {
                    Ok(var_type.clone())
                } else if let Some(instance_type) = self.env.get_instance(name) {
                    Ok(instance_type.clone())
                } else {
                    Err(TypeCheckError::UndefinedVariable(name.clone()))
                }
            }
            Expr::GlobalContext => Ok(Type::Unknown), // Global context type is contextual
            Expr::EnumAccess(enum_access) => {
                let enum_decl = self.env.get_enum(&enum_access.enum_name)
                    .ok_or_else(|| TypeCheckError::UndefinedEnum(enum_access.enum_name.clone()))?;
                
                if !enum_decl.variants.iter().any(|v| v.name == enum_access.variant_name) {
                    return Err(TypeCheckError::UndefinedEnumVariant(
                        enum_access.enum_name.clone(),
                        enum_access.variant_name.clone()
                    ));
                }
                
                // Return the enum type itself
                Ok(Type::Named(enum_access.enum_name.clone()))
            }
            Expr::Lambda(lambda) => self.infer_lambda_type(lambda),
            Expr::Match(match_expr) => self.infer_match_type(match_expr),
            Expr::BuiltinCall(builtin_call) => self.infer_builtin_call_type(builtin_call),
            Expr::Logic(logic_expr) => self.infer_logic_expr_type(logic_expr),
            Expr::ObjectLiteral(obj_literal) => self.infer_object_literal_type(obj_literal),
            Expr::ListLiteral(exprs) => {
                if exprs.is_empty() {
                    Ok(Type::Unknown)
                } else {
                    let first_type = self.infer_expr_type(&exprs[0])?;
                    // For simplicity, assume all elements have the same type
                    Ok(Type::Logic {
                        logic_type: LogicType::Any,
                        inner: Box::new(first_type),
                    })
                }
            }
            Expr::SetLiteral(exprs) => {
                if exprs.is_empty() {
                    Ok(Type::Unknown)
                } else {
                    let first_type = self.infer_expr_type(&exprs[0])?;
                    Ok(Type::Logic {
                        logic_type: LogicType::Any,
                        inner: Box::new(first_type),
                    })
                }
            }
            Expr::FieldAccess(field_access) => self.infer_field_access_type(field_access),
            Expr::ArithExpr(arith_expr) => self.infer_arith_expr_type(arith_expr),
            Expr::BinaryOp(binary_op) => self.infer_binary_op_type(binary_op),
            Expr::AnonymousObjectLiteral(fields) => self.infer_anonymous_object_literal_type(fields),
        }
    }
    
    fn infer_literal_type(&self, literal: &Literal) -> Type {
        match literal {
            Literal::String(_) => Type::Primitive(PrimitiveType::String),
            Literal::Int(_) => Type::Primitive(PrimitiveType::Int),
            Literal::Float(_) => Type::Primitive(PrimitiveType::Float),
            Literal::Bool(_) => Type::Primitive(PrimitiveType::Bool),
        }
    }
    
    fn infer_lambda_type(&mut self, lambda: &LambdaExpr) -> Result<Type, TypeCheckError> {
        let param_type = if let Some(param_type_name) = &lambda.param_type {
            self.resolve_type_name(param_type_name)?
        } else {
            Type::Unknown
        };
        
        // Create a new scope for the lambda parameter
        let saved_vars = self.env.variables.clone();
        if let Some(param_name) = &lambda.param {
            self.env.add_variable(param_name.clone(), param_type.clone());
        }
        
        let return_type = self.infer_expr_type(&lambda.body)?;
        
        // Restore the original variable scope
        self.env.variables = saved_vars;
        
        Ok(Type::Function {
            param: Box::new(param_type),
            return_type: Box::new(return_type),
        })
    }
    
    fn infer_match_type(&mut self, match_expr: &MatchExpr) -> Result<Type, TypeCheckError> {
        if match_expr.cases.is_empty() {
            return Err(TypeCheckError::TypeError("Empty match expression".to_string()));
        }
        
        let first_case_type = match &match_expr.cases[0] {
            MatchCase::Pattern { value, .. } => self.infer_expr_type(value)?,
            MatchCase::Wildcard { value } => self.infer_expr_type(value)?,
        };
        
        // Check that all match arms return the same type
        for (i, case) in match_expr.cases.iter().enumerate().skip(1) {
            let case_type = match case {
                MatchCase::Pattern { value, .. } => self.infer_expr_type(value)?,
                MatchCase::Wildcard { value } => self.infer_expr_type(value)?,
            };
            
            if !self.types_compatible(&first_case_type, &case_type) {
                return Err(TypeCheckError::TypeError(format!(
                    "Match arm {} returns type {:?}, but first arm returns {:?}",
                    i, case_type, first_case_type
                )));
            }
        }
        
        Ok(first_case_type)
    }
    
    fn infer_builtin_call_type(&mut self, builtin_call: &BuiltinCall) -> Result<Type, TypeCheckError> {
        match builtin_call.name.as_str() {
            "solve" => Ok(Type::Unknown), // Context-dependent
            "find" => {
                // find returns a collection of the filtered type
                if builtin_call.args.len() >= 2 {
                    // The second argument should be a lambda that returns bool
                    if let Arg::Positional(Expr::Lambda(lambda)) = &builtin_call.args[1] {
                        if let Some(param_type_name) = &lambda.param_type {
                            let param_type = self.resolve_type_name(param_type_name)?;
                            return Ok(Type::Logic {
                                logic_type: LogicType::Any,
                                inner: Box::new(param_type),
                            });
                        }
                    }
                }
                Ok(Type::Unknown)
            }
            "sum" => Ok(Type::Primitive(PrimitiveType::Float)),
            "first" => {
                // first returns the element type of the collection
                if !builtin_call.args.is_empty() {
                    let arg_type = match &builtin_call.args[0] {
                        Arg::Positional(expr) => self.infer_expr_type(expr)?,
                        Arg::Named { value, .. } => self.infer_expr_type(value)?,
                    };
                    match arg_type {
                        Type::Logic { inner, .. } => Ok(*inner),
                        _ => Ok(arg_type),
                    }
                } else {
                    Ok(Type::Unknown)
                }
            }
            "match" => Ok(Type::Unknown), // Context-dependent
            "propagate" => Ok(Type::Primitive(PrimitiveType::Bool)),
            _ => Err(TypeCheckError::InvalidBuiltinCall(builtin_call.name.clone())),
        }
    }
    
    fn infer_logic_expr_type(&mut self, logic_expr: &LogicExpr) -> Result<Type, TypeCheckError> {
        let logic_type = match &logic_expr.op {
            LogicOp::All => LogicType::All,
            LogicOp::Any => LogicType::Any,
            LogicOp::Not => LogicType::Not,
            LogicOp::Exactly(n) => LogicType::Exactly(*n),
            LogicOp::AtLeast(n) => LogicType::AtLeast(*n),
            LogicOp::AtMost(n) => LogicType::AtMost(*n),
        };
        
        if logic_expr.args.is_empty() {
            return Ok(Type::Logic {
                logic_type,
                inner: Box::new(Type::Unknown),
            });
        }
        
        let first_arg_type = self.infer_expr_type(&logic_expr.args[0])?;
        Ok(Type::Logic {
            logic_type,
            inner: Box::new(first_arg_type),
        })
    }
    
    fn infer_object_literal_type(&mut self, obj_literal: &ObjectLiteral) -> Result<Type, TypeCheckError> {
        let type_decl = self.env.get_type(&obj_literal.type_name)
            .ok_or_else(|| TypeCheckError::UndefinedType(obj_literal.type_name.clone()))?
            .clone();
        
        // Validate field assignments
        for field_assign in &obj_literal.fields {
            let field_decl = type_decl.fields.iter()
                .find(|f| f.name == field_assign.name)
                .ok_or_else(|| TypeCheckError::FieldNotFound(
                    field_assign.name.clone(),
                    obj_literal.type_name.clone()
                ))?;
            
            let expr_type = self.infer_expr_type(&field_assign.value)?;
            if !self.types_compatible(&field_decl.field_type, &expr_type) {
                return Err(TypeCheckError::TypeMismatch {
                    expected: field_decl.field_type.to_string(),
                    found: expr_type.to_string(),
                });
            }
        }
        
        Ok(Type::Named(obj_literal.type_name.clone()))
    }
    
    fn infer_anonymous_object_literal_type(&mut self, fields: &[FieldAssign]) -> Result<Type, TypeCheckError> {
        // For anonymous object literals, we can't determine a specific type
        // without more context, so we return Unknown type
        // In a more sophisticated implementation, we might infer a structural type
        
        // Still validate that all field expressions are well-typed
        for field_assign in fields {
            let _expr_type = self.infer_expr_type(&field_assign.value)?;
        }
        
        Ok(Type::Unknown)
    }
    
    fn infer_field_access_type(&mut self, field_access: &FieldAccess) -> Result<Type, TypeCheckError> {
        let base_type = if let Some(var_type) = self.env.get_variable(&field_access.base) {
            var_type.clone()
        } else if let Some(instance_type) = self.env.get_instance(&field_access.base) {
            instance_type.clone()
        } else {
            return Err(TypeCheckError::UndefinedVariable(field_access.base.clone()));
        };
        
        let mut current_type = base_type;
        
        for field_name in &field_access.fields {
            match &current_type {
                Type::Named(type_name) => {
                    let type_decl = self.env.get_type(type_name)
                        .ok_or_else(|| TypeCheckError::UndefinedType(type_name.clone()))?;
                    
                    let field = type_decl.fields.iter()
                        .find(|f| f.name == *field_name)
                        .ok_or_else(|| TypeCheckError::FieldNotFound(
                            field_name.clone(),
                            type_name.clone()
                        ))?;
                    
                    current_type = field.field_type.clone();
                }
                _ => return Err(TypeCheckError::TypeError(
                    format!("Cannot access field {} on type {}", field_name, current_type.to_string())
                )),
            }
        }
        
        Ok(current_type)
    }
    
    fn infer_arith_expr_type(&mut self, arith_expr: &ArithExpr) -> Result<Type, TypeCheckError> {
        match arith_expr {
            ArithExpr::Add(left, right) |
            ArithExpr::Subtract(left, right) |
            ArithExpr::Multiply(left, right) => {
                let left_type = self.infer_expr_type(left)?;
                let right_type = self.infer_expr_type(right)?;
                
                match (&left_type, &right_type) {
                    (Type::Primitive(PrimitiveType::Int), Type::Primitive(PrimitiveType::Int)) => 
                        Ok(Type::Primitive(PrimitiveType::Int)),
                    (Type::Primitive(PrimitiveType::Float), _) | 
                    (_, Type::Primitive(PrimitiveType::Float)) => 
                        Ok(Type::Primitive(PrimitiveType::Float)),
                    _ => Err(TypeCheckError::TypeError(
                        format!("Invalid arithmetic operation between {} and {}", 
                               left_type.to_string(), right_type.to_string())
                    )),
                }
            }
            ArithExpr::Factor(expr) => self.infer_expr_type(expr),
        }
    }
    
    fn infer_binary_op_type(&mut self, binary_op: &BinaryOpExpr) -> Result<Type, TypeCheckError> {
        let left_type = self.infer_expr_type(&binary_op.left)?;
        let right_type = self.infer_expr_type(&binary_op.right)?;
        
        match binary_op.operator.as_str() {
            "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                // Comparison operators return boolean
                if self.types_compatible(&left_type, &right_type) {
                    Ok(Type::Primitive(PrimitiveType::Bool))
                } else {
                    Err(TypeCheckError::TypeMismatch {
                        expected: left_type.to_string(),
                        found: right_type.to_string(),
                    })
                }
            }
            "+" | "-" | "*" | "/" => {
                // Arithmetic operators
                match (&left_type, &right_type) {
                    (Type::Primitive(PrimitiveType::Int), Type::Primitive(PrimitiveType::Int)) => 
                        Ok(Type::Primitive(PrimitiveType::Int)),
                    (Type::Primitive(PrimitiveType::Float), _) | 
                    (_, Type::Primitive(PrimitiveType::Float)) => 
                        Ok(Type::Primitive(PrimitiveType::Float)),
                    _ => Err(TypeCheckError::TypeError(
                        format!("Invalid arithmetic operation between {} and {}", 
                               left_type.to_string(), right_type.to_string())
                    )),
                }
            }
            _ => Err(TypeCheckError::TypeError(
                format!("Unknown binary operator: {}", binary_op.operator)
            )),
        }
    }
    
    fn convert_base_type(&mut self, base_type: &BaseType) -> Result<Type, TypeCheckError> {
        match base_type {
            BaseType::Primitive(p) => Ok(Type::Primitive(p.clone())),
            BaseType::Logic(l) => Ok(Type::Logic {
                logic_type: l.clone(),
                inner: Box::new(Type::Unknown),
            }),
        }
    }
    
    fn resolve_type_name(&self, name: &str) -> Result<Type, TypeCheckError> {
        if self.env.types.contains_key(name) || self.env.enums.contains_key(name) {
            Ok(Type::Named(name.to_string()))
        } else {
            match name {
                "int" => Ok(Type::Primitive(PrimitiveType::Int)),
                "float" => Ok(Type::Primitive(PrimitiveType::Float)),
                "string" => Ok(Type::Primitive(PrimitiveType::String)),
                "bool" => Ok(Type::Primitive(PrimitiveType::Bool)),
                _ => Err(TypeCheckError::UndefinedType(name.to_string())),
            }
        }
    }
    
    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        let result = match (expected, actual) {
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::Primitive(p1), Type::Primitive(p2)) => {
                // Allow some flexibility with numeric types
                match (p1, p2) {
                    (PrimitiveType::Float, PrimitiveType::Int) => true,
                    _ => p1 == p2,
                }
            }
            // Handle compatibility between Primitive(Named) and Named
            (Type::Primitive(PrimitiveType::Named(n1)), Type::Named(n2)) => n1 == n2,
            (Type::Named(n1), Type::Primitive(PrimitiveType::Named(n2))) => n1 == n2,
            (Type::Named(n1), Type::Named(n2)) => n1 == n2,
            (Type::Logic { logic_type: l1, inner: i1 }, Type::Logic { logic_type: l2, inner: i2 }) => {
                l1 == l2 && self.types_compatible(i1, i2)
            }
            (Type::Function { param: p1, return_type: r1 }, Type::Function { param: p2, return_type: r2 }) => {
                self.types_compatible(p1, p2) && self.types_compatible(r1, r2)
            }
            _ => false,
        };
        
        result
    }
}

// Public API functions
pub fn typecheck_program(program: &Program) -> Result<(), Vec<TypeCheckError>> {
    let mut typechecker = TypeChecker::new();
    typechecker.check_program(program)
}

pub fn typecheck_program_with_details(program: &Program) -> Result<TypeEnvironment, Vec<TypeCheckError>> {
    let mut typechecker = TypeChecker::new();
    match typechecker.check_program(program) {
        Ok(()) => Ok(typechecker.env),
        Err(errors) => Err(errors),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_program;

    #[test]
    fn test_simple_type_declaration() {
        let input = "type Size: int";
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_with_fields() {
        let input = r#"type Hammer: bool {
            material: string,
            size: int,
            cost: float
        }"#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_instance_declaration() {
        let input = r#"
        type Hammer: bool {
            material: string,
            size: int,
            cost: float
        }
        
        Hammer hammer1 {
            material: "steel",
            size: 10,
            cost: 25.0
        }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_missing_required_field() {
        let input = r#"
        type Hammer: bool {
            material: string,
            size: int,
            cost: float
        }
        
        Hammer hammer1 {
            material: "steel",
            size: 10
            // missing cost field
        }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_err());
        
        if let Err(errors) = result {
            assert!(errors.iter().any(|e| matches!(e, TypeCheckError::MissingRequiredField { .. })));
        }
    }

    #[test]
    fn test_type_mismatch_in_field() {
        let input = r#"
        type Hammer: bool {
            material: string,
            size: int,
            cost: float
        }
        
        Hammer hammer1 {
            material: "steel",
            size: "large", // should be int, not string
            cost: 25.0
        }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_err());
    }

    #[test]
    fn test_undefined_type_reference() {
        let input = r#"
        type Hammer: bool {
            material: string,
            size: UndefinedType, // This type doesn't exist
            cost: float
        }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_err());
        
        if let Err(errors) = result {
            assert!(errors.iter().any(|e| matches!(e, TypeCheckError::UndefinedType(_))));
        }
    }

    #[test]
    fn test_enum_declaration() {
        let input = r#"
        enum Material {
            Steel,
            Wood,
            Plastic
        }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enum_with_values() {
        let input = r#"
        enum Size {
            Small = 1,
            Medium = 2,
            Large = 3
        }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enum_usage_in_type() {
        let input = r#"
        enum Material {
            Steel,
            Wood,
            Plastic
        }
        
        type Hammer: bool {
            material: Material,
            size: int
        }
        
        Hammer hammer1 {
            material: Material.Steel,
            size: 10
        }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        if let Err(errors) = &result {
            for error in errors {
                println!("Type error: {}", error);
            }
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_undefined_enum_variant() {
        let input = r#"
        enum Material {
            Steel,
            Wood
        }
        
        Material test = Material.Plastic  // Plastic is not a valid variant
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_err());
        
        if let Err(errors) = result {
            assert!(errors.iter().any(|e| matches!(e, TypeCheckError::UndefinedEnumVariant(_, _))));
        }
    }

    #[test]
    fn test_field_access() {
        let input = r#"
        type Hammer: bool {
            material: string,
            size: int
        }
        
        Hammer hammer1 {
            material: "steel",
            size: 10
        }
        
        test = hammer1.size
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_invalid_field_access() {
        let input = r#"
        type Hammer: bool {
            material: string,
            size: int
        }
        
        Hammer hammer1 {
            material: "steel",
            size: 10
        }
        
        int test = hammer1.weight  // weight field doesn't exist
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_err());
        
        if let Err(errors) = result {
            assert!(errors.iter().any(|e| matches!(e, TypeCheckError::FieldNotFound(_, _))));
        }
    }

    #[test]
    fn test_lambda_expression() {
        let input = r#"
        type Hammer: bool {
            material: string,
            size: int,
            weight: int = (h: Hammer) -> 10
        }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        if let Err(errors) = &result {
            for error in errors {
                println!("Type error: {}", error);
            }
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_complex_lambda_with_comparison() {
        let input = r#"
        type Carpenter: bool {
            age: int,
            workable: bool = (c: Carpenter) -> c.age >= 18
        }
        "#;
        let program = parse_program(input).unwrap();  
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_builtin_find_function() {
        let input = r#"
        type Hammer: bool {
            size: int
        }
        
        result = find(*, (h: Hammer) -> h.size >= 8)
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_logic_expressions() {
        let input = r#"
        type Hammer: bool {
            size: int
        }
        
        Hammer h1 { size: 10 }
        Hammer h2 { size: 12 }
        
        all_hammers = All { h1, h2 }
        any_hammer = Any { h1, h2 }
        at_least_one = AtLeast<1> { h1, h2 }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_solve_call() {
        let input = r#"
        type Hammer: bool {
            cost: float
        }
        
        Hammer h1 { cost: 10.0 }
        Hammer h2 { cost: 15.0 }
        
        solution = solve(*, { h1: 1.0, h2: 2.0 }, { Not { h2 } })
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_arithmetic_expressions() {
        let input = r#"
        result1 = 10 + 5
        result2 = 3.14 * 2.0
        result3 = 10 - 3
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_comparison_expressions() {
        let input = r#"
        test1 = 10 > 5
        test2 = 3.14 <= 4.0
        test3 = "hello" == "world"
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_optional_fields() {
        let input = r#"
        type Toolbox: bool {
            hammers: int,
            nails: int?  // optional field
        }
        
        Toolbox toolbox1 {
            hammers: 5
            // nails field is optional, so this should be valid
        }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_match_expression() {
        let input = r#"
        result = match {
            _: 42
        }
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_complex_program() {
        let input = r#"
        enum Material {
            Steel,
            Wood
        }
        
        type Hammer: bool {
            material: Material,
            size: int,
            weight: int = (h: Hammer) -> match {
                h.material == Material.Steel: 10,
                _: 5
            }
        }
        
        type Toolbox: All {  
            hammers: AtLeast<1>[Hammer]
        }
        
        Hammer steel_hammer {
            material: Material.Steel,
            size: 12
        }
        
        Hammer wood_hammer {
            material: Material.Wood,
            size: 8
        }
        
        Toolbox my_toolbox {
            hammers: AtLeast<1> { steel_hammer, wood_hammer }
        }
        
        heavy_hammers = find(*, (h: Hammer) -> h.weight >= 8)
        "#;
        let program = parse_program(input).unwrap();
        let result = typecheck_program(&program);
        assert!(result.is_ok());
    }
}