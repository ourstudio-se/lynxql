//! # LynxQL - Lynx Language Parser and Typechecker
//! 
//! A parser and typechecker for the Lynx declarative modeling language - a statically typed language
//! for expressing combinatorial optimization problems using Integer Linear Programming (ILP).
//! 
//! ## Usage
//! 
//! ```rust
//! use lynxql::{parse_program, typecheck_program};
//! 
//! let lynx_code = r#"
//! type Hammer: bool {
//!   material: string,
//!   size: int,
//!   cost: float
//! }
//! 
//! Hammer hammer1 {
//!   material: "steel",
//!   size: 10,
//!   cost: 25.0
//! }
//! "#;
//! 
//! match parse_program(lynx_code) {
//!     Ok(program) => {
//!         println!("Parsed {} statements", program.statements.len());
//!         
//!         // Type check the program
//!         match typecheck_program(&program) {
//!             Ok(()) => println!("Program is type-safe!"),
//!             Err(errors) => {
//!                 for error in errors {
//!                     eprintln!("Type error: {}", error);
//!                 }
//!             }
//!         }
//!     }
//!     Err(e) => {
//!         eprintln!("Parse error: {}", e);
//!     }
//! }
//! ```

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1, take_while, take_until},
    character::complete::{char, digit1, alpha1, alphanumeric1, line_ending, not_line_ending},
    combinator::{opt, map, recognize, value},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, preceded, tuple, pair},
    IResult,
};
use thiserror::Error;

// Modules
pub mod typechecker;
pub mod resilient_parser;

// Re-export typechecker functions
pub use typechecker::{typecheck_program, typecheck_program_with_details, TypeCheckError, TypeEnvironment};

// Re-export resilient parser
pub use resilient_parser::{parse_program_resilient, ParseResult, ResilientParseError};

// All types are already declared as `pub` below, so they are accessible as lynxql::TypeName

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Parse error: {0}")]
    Nom(String),
    #[error("Invalid syntax: {0}")]
    InvalidSyntax(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    TypeDecl(TypeDecl),
    EnumDecl(EnumDecl),
    InstanceDecl(InstanceDecl),
    Assignment(Assignment),
    SolveCall(SolveCall),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl {
    pub name: String,
    pub base_type: BaseType,
    pub fields: Vec<FieldDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub value: Option<i64>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseType {
    Primitive(PrimitiveType),
    Logic(LogicType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Bound(i32, i32),
    Int,
    Float,
    String,
    Bool,
    Named(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicType {
    All,
    Any,
    Not,
    Exactly(i32),
    AtLeast(i32),
    AtMost(i32),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDecl {
    pub name: String,
    pub type_ref: TypeRef,
    pub is_optional: bool,
    pub default: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeRef {
    Primitive(PrimitiveType),
    Logic { logic_type: LogicType, inner: Box<TypeRef> },
    Named(String),
}

// New instance declaration syntax: TypeName instance_name { ... }
#[derive(Debug, Clone, PartialEq)]
pub struct InstanceDecl {
    pub type_name: String,
    pub instance_name: String,
    pub fields: Vec<FieldAssign>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAssign {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    GlobalContext,
    EnumAccess(EnumAccess),
    Lambda(LambdaExpr),
    Match(MatchExpr),
    BuiltinCall(BuiltinCall),
    Logic(LogicExpr),
    ObjectLiteral(ObjectLiteral),
    ListLiteral(Vec<Expr>),
    SetLiteral(Vec<Expr>),
    FieldAccess(FieldAccess),
    ArithExpr(ArithExpr),
    BinaryOp(BinaryOpExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumAccess {
    pub enum_name: String,
    pub variant_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectLiteral {
    pub type_name: String,
    pub fields: Vec<FieldAssign>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub base: String,
    pub fields: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArithExpr {
    Add(Box<Expr>, Box<Expr>),
    Subtract(Box<Expr>, Box<Expr>),
    Multiply(Box<Expr>, Box<Expr>),
    Factor(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOpExpr {
    pub left: Box<Expr>,
    pub operator: String,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaExpr {
    pub param: Option<String>,
    pub param_type: Option<String>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
    pub cases: Vec<MatchCase>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchCase {
    Logic { pattern: LogicExpr, value: Expr },
    Wildcard { value: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuiltinCall {
    pub name: String,
    pub args: Vec<Arg>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arg {
    Positional(Expr),
    Named { name: String, value: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicExpr {
    pub op: LogicOp,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicOp {
    All,
    Any,
    Not,
    Exactly(i32),
    AtLeast(i32),
    AtMost(i32),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SolveCall {
    pub var_name: String,
    pub target: Expr,
    pub objectives: Vec<ObjectiveItem>,
    pub constraints: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectiveItem {
    pub name: String,
    pub weight: f64,
}

// Parser implementation
pub fn parse_program(input: &str) -> Result<Program, ParseError> {
    match program(input) {
        Ok((_, program)) => Ok(program),
        Err(err) => Err(ParseError::Nom(err.to_string())),
    }
}

fn ws<'a, F, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(multispace_and_comments, inner, multispace_and_comments)
}

fn multispace_and_comments(input: &str) -> IResult<&str, ()> {
    let (input, _) = many0(alt((
        value((), single_line_comment),
        value((), multi_line_comment),
        map(take_while1(|c: char| c.is_whitespace()), |_| ()),
    )))(input)?;
    Ok((input, ()))
}

fn single_line_comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("//")(input)?;
    let (input, comment) = not_line_ending(input)?;
    let (input, _) = opt(line_ending)(input)?;
    Ok((input, comment))
}

fn multi_line_comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("/*")(input)?;
    let (input, comment) = take_until("*/")(input)?;
    let (input, _) = tag("*/")(input)?;
    Ok((input, comment))
}

fn program(input: &str) -> IResult<&str, Program> {
    let (input, statements) = many0(ws(statement))(input)?;
    let (input, _) = multispace_and_comments(input)?; // Consume any trailing whitespace/comments
    
    // Check if there's unparsed content
    if !input.trim().is_empty() {
        eprintln!("Warning: Unparsed content remaining: {:?}", &input[..input.len().min(100)]);
    }
    
    Ok((input, Program { statements }))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    alt((
        map(type_decl, Statement::TypeDecl),
        map(enum_decl, Statement::EnumDecl),
        map(instance_decl, Statement::InstanceDecl),
        map(solve_call, Statement::SolveCall),
        map(assignment, Statement::Assignment),
    ))(input)
}

fn type_decl(input: &str) -> IResult<&str, TypeDecl> {
    let (input, _) = ws(tag("type"))(input)?;
    let (input, name) = ws(type_name)(input)?;
    let (input, _) = ws(char(':'))(input)?;
    let (input, base_type) = ws(base_type)(input)?;
    let (input, fields) = opt(delimited(
        ws(char('{')),
        field_decl_list,
        ws(char('}'))
    ))(input)?;
    
    Ok((input, TypeDecl {
        name: name.to_string(),
        base_type,
        fields: fields.unwrap_or_default(),
    }))
}

fn base_type(input: &str) -> IResult<&str, BaseType> {
    alt((
        map(logic_type, BaseType::Logic),
        map(primitive_type, BaseType::Primitive),
    ))(input)
}

fn primitive_type(input: &str) -> IResult<&str, PrimitiveType> {
    alt((
        map(
            tuple((int_literal, ws(tag("..")), int_literal)),
            |(from, _, to)| PrimitiveType::Bound(from as i32, to as i32),
        ),
        value(PrimitiveType::Int, tag("int")),
        value(PrimitiveType::Float, tag("float")),
        value(PrimitiveType::String, tag("string")),
        value(PrimitiveType::Bool, tag("bool")),
        map(type_name, |name| PrimitiveType::Named(name.to_string())),
    ))(input)
}

fn logic_type(input: &str) -> IResult<&str, LogicType> {
    alt((
        value(LogicType::All, tag("All")),
        value(LogicType::Any, tag("Any")),
        value(LogicType::Not, tag("Not")),
        map(
            tuple((tag("Exactly"), ws(char('<')), ws(int_literal), ws(char('>')))),
            |(_, _, n, _)| LogicType::Exactly(n as i32)
        ),
        map(
            tuple((tag("AtLeast"), ws(char('<')), ws(int_literal), ws(char('>')))),
            |(_, _, n, _)| LogicType::AtLeast(n as i32)
        ),
        map(
            tuple((tag("AtMost"), ws(char('<')), ws(int_literal), ws(char('>')))),
            |(_, _, n, _)| LogicType::AtMost(n as i32)
        ),
    ))(input)
}

fn field_decl_list(input: &str) -> IResult<&str, Vec<FieldDecl>> {
    // Allow trailing commas and missing commas between fields
    let (input, first) = opt(field_decl)(input)?;
    if first.is_none() {
        return Ok((input, Vec::new()));
    }
    
    let mut fields = vec![first.unwrap()];
    let mut remaining = input;
    
    loop {
        // Try to consume optional comma and whitespace
        let (input_after_comma, _) = opt(ws(char(',')))(remaining)?;
        
        // Try to parse another field
        if let Ok((new_input, field)) = ws(field_decl)(input_after_comma) {
            fields.push(field);
            remaining = new_input;
        } else {
            // No more fields, return what we have
            remaining = input_after_comma;
            break;
        }
    }
    
    Ok((remaining, fields))
}

fn field_decl(input: &str) -> IResult<&str, FieldDecl> {
    let (input, name) = ws(identifier)(input)?;
    let (input, _) = ws(char(':'))(input)?;
    let (input, type_ref) = ws(type_ref)(input)?;
    let (input, is_optional) = opt(ws(char('?')))(input)?;
    let (input, default) = opt(preceded(ws(char('=')), ws(expr)))(input)?;
    
    Ok((input, FieldDecl {
        name: name.to_string(),
        type_ref,
        is_optional: is_optional.is_some(),
        default,
    }))
}

fn type_ref(input: &str) -> IResult<&str, TypeRef> {
    alt((
        map(
            tuple((logic_type, ws(char('[')), ws(type_ref), ws(char(']')))),
            |(logic_type, _, inner, _)| TypeRef::Logic {
                logic_type,
                inner: Box::new(inner),
            }
        ),
        map(primitive_type, TypeRef::Primitive),
        map(type_name, |name| TypeRef::Named(name.to_string())),
    ))(input)
}

// New instance declaration syntax: TypeName instance_name { ... }
fn instance_decl(input: &str) -> IResult<&str, InstanceDecl> {
    let (input, type_name) = ws(type_name)(input)?;
    let (input, instance_name) = ws(identifier)(input)?;
    let (input, _) = ws(char('{'))(input)?;
    let (input, fields) = field_assign_list(input)?;
    let (input, _) = ws(char('}'))(input)?;
    
    Ok((input, InstanceDecl {
        type_name: type_name.to_string(),
        instance_name: instance_name.to_string(),
        fields,
    }))
}

fn field_assign_list(input: &str) -> IResult<&str, Vec<FieldAssign>> {
    separated_list0(ws(char(',')), field_assign)(input)
}

fn field_assign(input: &str) -> IResult<&str, FieldAssign> {
    let (input, name) = ws(identifier)(input)?;
    let (input, _) = ws(char(':'))(input)?;
    let (input, value) = ws(expr)(input)?;
    
    Ok((input, FieldAssign {
        name: name.to_string(),
        value,
    }))
}

fn assignment(input: &str) -> IResult<&str, Assignment> {
    let (input, name) = ws(identifier)(input)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, value) = ws(expr)(input)?;
    
    Ok((input, Assignment {
        name: name.to_string(),
        value,
    }))
}

fn expr(input: &str) -> IResult<&str, Expr> {
    comparison_expr(input)
}

// Precedence levels: comparison < arithmetic < primary
fn comparison_expr(input: &str) -> IResult<&str, Expr> {
    let (input, left) = arithmetic_expr(input)?;
    
    // Try to parse comparison operators
    if let Ok((input, op)) = ws(comparison_op)(input) {
        let (input, right) = arithmetic_expr(input)?;
        Ok((input, Expr::BinaryOp(BinaryOpExpr {
            left: Box::new(left),
            operator: op.to_string(),
            right: Box::new(right),
        })))
    } else {
        Ok((input, left))
    }
}

fn comparison_op(input: &str) -> IResult<&str, &str> {
    alt((
        tag(">="),
        tag("<="),
        tag("=="),
        tag("!="),
        tag(">"),
        tag("<"),
    ))(input)
}

fn arithmetic_expr(input: &str) -> IResult<&str, Expr> {
    let (input, left) = primary_expr(input)?;
    
    // Try to parse arithmetic operators (left-associative)
    let mut current = left;
    let mut remaining = input;
    
    loop {
        if let Ok((new_input, op)) = ws(arithmetic_op)(remaining) {
            if let Ok((new_input, right)) = primary_expr(new_input) {
                current = Expr::BinaryOp(BinaryOpExpr {
                    left: Box::new(current),
                    operator: op.to_string(),
                    right: Box::new(right),
                });
                remaining = new_input;
            } else {
                break;
            }
        } else {
            break;
        }
    }
    
    Ok((remaining, current))
}

fn arithmetic_op(input: &str) -> IResult<&str, &str> {
    alt((
        tag("+"),
        tag("-"),
        tag("*"),
        tag("/"),
    ))(input)
}

fn primary_expr(input: &str) -> IResult<&str, Expr> {
    alt((
        map(literal, Expr::Literal),
        map(global_context, |_| Expr::GlobalContext),
        map(enum_access, Expr::EnumAccess),
        map(lambda_expr, Expr::Lambda),
        map(match_expr, Expr::Match),
        map(builtin_call, Expr::BuiltinCall),
        map(logic_expr, Expr::Logic),
        map(object_literal, Expr::ObjectLiteral),
        map(list_literal, Expr::ListLiteral),
        map(set_literal, Expr::SetLiteral),
        map(field_access, Expr::FieldAccess),
        map(identifier, |s| Expr::Identifier(s.to_string())),
        // Parenthesized expressions
        delimited(ws(char('(')), expr, ws(char(')'))),
    ))(input)
}

fn literal(input: &str) -> IResult<&str, Literal> {
    alt((
        map(string_literal, Literal::String),
        map(float_literal, Literal::Float),
        map(int_literal, Literal::Int),
        map(bool_literal, Literal::Bool),
    ))(input)
}

fn string_literal(input: &str) -> IResult<&str, String> {
    let (input, _) = char('"')(input)?;
    let (input, s) = take_while(|c| c != '"')(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, s.to_string()))
}

fn int_literal(input: &str) -> IResult<&str, i64> {
    map(digit1, |s: &str| s.parse().unwrap())(input)
}

fn float_literal(input: &str) -> IResult<&str, f64> {
    map(
        recognize(tuple((digit1, char('.'), digit1))),
        |s: &str| s.parse().unwrap(),
    )(input)
}

fn bool_literal(input: &str) -> IResult<&str, bool> {
    alt((
        value(true, tag("true")),
        value(false, tag("false")),
    ))(input)
}

fn lambda_expr(input: &str) -> IResult<&str, LambdaExpr> {
    alt((
        // Simple lambda: (_) -> expr
        map(
            tuple((ws(char('(')), opt(ws(char('_'))), ws(char(')')), ws(tag("->")), ws(expr))),
            |(_, _, _, _, body)| LambdaExpr {
                param: None,
                param_type: None,
                body: Box::new(body),
            }
        ),
        // Typed lambda: (param: Type) -> expr
        map(
            tuple((
                ws(char('(')),
                ws(identifier),
                ws(char(':')),
                ws(type_name),
                ws(char(')')),
                ws(tag("->")),
                ws(expr)
            )),
            |(_, param, _, param_type, _, _, body)| LambdaExpr {
                param: Some(param.to_string()),
                param_type: Some(param_type.to_string()),
                body: Box::new(body),
            }
        ),
    ))(input)
}

fn match_expr(input: &str) -> IResult<&str, MatchExpr> {
    let (input, _) = ws(tag("match"))(input)?;
    let (input, _) = ws(char('('))(input)?;
    let (input, _) = ws(char('{'))(input)?;
    let (input, cases) = separated_list0(ws(char(',')), match_case)(input)?;
    let (input, _) = ws(char('}'))(input)?;
    let (input, _) = ws(char(')'))(input)?;
    
    Ok((input, MatchExpr {
        cases,
    }))
}

fn match_case(input: &str) -> IResult<&str, MatchCase> {
    alt((
        map(
            tuple((logic_expr, ws(char(':')), ws(expr))),
            |(pattern, _, value)| MatchCase::Logic { pattern, value },
        ),
        map(
            tuple((ws(char('_')), ws(char(':')), ws(expr))),
            |(_, _, value)| MatchCase::Wildcard { value },
        ),
    ))(input)
}

fn builtin_call(input: &str) -> IResult<&str, BuiltinCall> {
    let (input, name) = builtin_name(input)?;
    let (input, _) = ws(char('('))(input)?;
    let (input, args) = opt(arg_list)(input)?;
    let (input, _) = ws(char(')'))(input)?;
    
    Ok((input, BuiltinCall {
        name: name.to_string(),
        args: args.unwrap_or_default(),
    }))
}

fn builtin_name(input: &str) -> IResult<&str, &str> {
    alt((
        tag("solve"),
        tag("find"),
        tag("sum"),
        tag("first"),
        tag("match"),
        tag("propagate"),
    ))(input)
}

fn arg_list(input: &str) -> IResult<&str, Vec<Arg>> {
    separated_list1(ws(char(',')), arg)(input)
}

fn arg(input: &str) -> IResult<&str, Arg> {
    alt((
        map(named_arg, |(name, value)| Arg::Named { name, value }),
        map(expr, Arg::Positional),
    ))(input)
}

fn named_arg(input: &str) -> IResult<&str, (String, Expr)> {
    let (input, name) = ws(identifier)(input)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, value) = ws(expr)(input)?;
    Ok((input, (name.to_string(), value)))
}

fn logic_expr(input: &str) -> IResult<&str, LogicExpr> {
    let (input, op) = logic_op(input)?;
    let (input, args) = alt((
        delimited(ws(char('{')), expr_list, ws(char('}'))),
        delimited(ws(char('(')), expr_list, ws(char(')'))),
        value(Vec::new(), tag("")), // No args
    ))(input)?;
    
    Ok((input, LogicExpr { op, args }))
}

fn logic_op(input: &str) -> IResult<&str, LogicOp> {
    alt((
        value(LogicOp::All, tag("All")),
        value(LogicOp::Any, tag("Any")),
        value(LogicOp::Not, tag("Not")),
        map(
            tuple((tag("Exactly"), ws(char('<')), ws(int_literal), ws(char('>')))),
            |(_, _, n, _)| LogicOp::Exactly(n as i32)
        ),
        map(
            tuple((tag("AtLeast"), ws(char('<')), ws(int_literal), ws(char('>')))),
            |(_, _, n, _)| LogicOp::AtLeast(n as i32)
        ),
        map(
            tuple((tag("AtMost"), ws(char('<')), ws(int_literal), ws(char('>')))),
            |(_, _, n, _)| LogicOp::AtMost(n as i32)
        ),
    ))(input)
}

fn expr_list(input: &str) -> IResult<&str, Vec<Expr>> {
    separated_list0(ws(char(',')), ws(expr))(input)
}

fn object_literal(input: &str) -> IResult<&str, ObjectLiteral> {
    let (input, type_name) = ws(type_name)(input)?;
    let (input, _) = ws(char('{'))(input)?;
    let (input, fields) = field_assign_list(input)?;
    let (input, _) = ws(char('}'))(input)?;
    
    Ok((input, ObjectLiteral {
        type_name: type_name.to_string(),
        fields,
    }))
}

fn list_literal(input: &str) -> IResult<&str, Vec<Expr>> {
    let (input, _) = ws(char('['))(input)?;
    let (input, exprs) = separated_list0(ws(char(',')), ws(expr))(input)?;
    let (input, _) = ws(char(']'))(input)?;
    Ok((input, exprs))
}

fn set_literal(input: &str) -> IResult<&str, Vec<Expr>> {
    let (input, _) = ws(char('{'))(input)?;
    let (input, exprs) = separated_list0(ws(char(',')), ws(expr))(input)?;
    let (input, _) = ws(char('}'))(input)?;
    Ok((input, exprs))
}

fn field_access(input: &str) -> IResult<&str, FieldAccess> {
    let (input, base) = identifier(input)?;
    let (input, fields) = many0(preceded(char('.'), identifier))(input)?;
    
    // Only return field access if there are actually fields accessed
    if fields.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    
    Ok((input, FieldAccess {
        base: base.to_string(),
        fields: fields.into_iter().map(|s| s.to_string()).collect(),
    }))
}

fn solve_call(input: &str) -> IResult<&str, SolveCall> {
    let (input, var_name) = ws(identifier)(input)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, _) = ws(tag("solve"))(input)?;
    let (input, _) = ws(char('('))(input)?;
    let (input, target) = ws(expr)(input)?;
    let (input, _) = ws(char(','))(input)?;
    let (input, _) = ws(char('{'))(input)?;
    let (input, objectives) = separated_list0(ws(char(',')), objective_item)(input)?;
    let (input, _) = ws(char('}'))(input)?;
    let (input, constraints) = opt(preceded(
        ws(char(',')),
        delimited(ws(char('{')), separated_list0(ws(char(',')), ws(expr)), ws(char('}')))
    ))(input)?;
    let (input, _) = ws(char(')'))(input)?;
    
    Ok((input, SolveCall {
        var_name: var_name.to_string(),
        target,
        objectives,
        constraints: constraints.unwrap_or_default(),
    }))
}

fn objective_item(input: &str) -> IResult<&str, ObjectiveItem> {
    let (input, name) = ws(identifier)(input)?;
    let (input, _) = ws(char(':'))(input)?;
    let (input, weight) = ws(float_literal)(input)?;
    
    Ok((input, ObjectiveItem {
        name: name.to_string(),
        weight,
    }))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn type_name(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        take_while1(|c: char| c.is_ascii_uppercase()),
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
}

fn global_context(input: &str) -> IResult<&str, ()> {
    let (input, _) = char('*')(input)?;
    Ok((input, ()))
}

fn enum_decl(input: &str) -> IResult<&str, EnumDecl> {
    let (input, _) = ws(tag("enum"))(input)?;
    let (input, name) = ws(type_name)(input)?;
    let (input, _) = ws(char('{'))(input)?;
    let (input, variants) = enum_variant_list(input)?;
    let (input, _) = ws(char('}'))(input)?;
    
    Ok((input, EnumDecl {
        name: name.to_string(),
        variants,
    }))
}

fn enum_variant_list(input: &str) -> IResult<&str, Vec<EnumVariant>> {
    let (input, first) = opt(enum_variant)(input)?;
    if first.is_none() {
        return Ok((input, Vec::new()));
    }
    
    let mut variants = vec![first.unwrap()];
    let mut remaining = input;
    
    loop {
        // Try to consume optional comma and whitespace
        let (input_after_comma, _) = opt(ws(char(',')))(remaining)?;
        
        // Try to parse another variant
        if let Ok((new_input, variant)) = ws(enum_variant)(input_after_comma) {
            variants.push(variant);
            remaining = new_input;
        } else {
            // No more variants, return what we have
            remaining = input_after_comma;
            break;
        }
    }

    Ok((remaining, variants))
}

fn enum_variant(input: &str) -> IResult<&str, EnumVariant> {
    let (input, name) = ws(type_name)(input)?;
    let (input, value) = opt(preceded(ws(char('=')), ws(int_literal)))(input)?;

    Ok((
        input,
        EnumVariant {
            name: name.to_string(),
            value,
        },
    ))
}

fn enum_access(input: &str) -> IResult<&str, EnumAccess> {
    let (input, enum_name) = type_name(input)?;
    let (input, _) = char('.')(input)?;
    let (input, variant_name) = type_name(input)?;

    Ok((
        input,
        EnumAccess {
            enum_name: enum_name.to_string(),
            variant_name: variant_name.to_string(),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bound_type_decl() {
        let input = "type Size: 0..1";
        let result = parse_program(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::TypeDecl(type_decl) => {
                assert_eq!(type_decl.name, "Size");
                assert_eq!(
                    type_decl.base_type,
                    BaseType::Primitive(PrimitiveType::Bound(0, 1))
                );
                assert_eq!(type_decl.fields.len(), 0);
            }
            _ => panic!("Expected type declaration"),
        }
    }

    #[test]
    fn test_simple_type_decl() {
        let input = "type Size: int";
        let result = parse_program(input).unwrap();

        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::TypeDecl(type_decl) => {
                assert_eq!(type_decl.name, "Size");
                assert_eq!(type_decl.fields.len(), 0);
            }
            _ => panic!("Expected type declaration"),
        }
    }

    #[test]
    fn test_type_with_fields() {
        let input = r#"type Hammer: bool {
            material: string,
            size: Size,
            cost: float
        }"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::TypeDecl(type_decl) => {
                assert_eq!(type_decl.name, "Hammer");
                assert_eq!(type_decl.fields.len(), 3);
                assert_eq!(type_decl.fields[0].name, "material");
                assert_eq!(type_decl.fields[1].name, "size");
                assert_eq!(type_decl.fields[2].name, "cost");
            }
            _ => panic!("Expected type declaration"),
        }
    }

    #[test]
    fn test_new_instance_syntax() {
        let input = r#"Hammer hammer1 {
            material: "steel",
            size: 10
        }"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::InstanceDecl(instance) => {
                assert_eq!(instance.type_name, "Hammer");
                assert_eq!(instance.instance_name, "hammer1");
                assert_eq!(instance.fields.len(), 2);
            }
            _ => panic!("Expected instance declaration"),
        }
    }

    #[test]
    fn test_single_line_comment() {
        let input = r#"// This is a comment
        type Size: int"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
    }

    #[test]
    fn test_multi_line_comment() {
        let input = r#"/* This is a
        multi-line comment */
        type Size: int"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
    }

    #[test]
    fn test_optional_field() {
        let input = r#"type Toolbox: All {
            nails: Any[Nail]?
        }"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::TypeDecl(type_decl) => {
                assert_eq!(type_decl.fields.len(), 1);
                assert!(type_decl.fields[0].is_optional);
            }
            _ => panic!("Expected type declaration"),
        }
    }

    #[test]
    fn test_field_access() {
        let input = "test = c.toolbox.hammers.cost";
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Assignment(assignment) => {
                match &assignment.value {
                    Expr::FieldAccess(field_access) => {
                        assert_eq!(field_access.base, "c");
                        assert_eq!(field_access.fields, vec!["toolbox", "hammers", "cost"]);
                    }
                    _ => panic!("Expected field access"),
                }
            }
            _ => panic!("Expected assignment"),
        }
    }

    #[test]
    fn test_solve_call() {
        let input = r#"a_john_instance = solve(john, { nail3: 3.0, nail: 2.0, nail1: 1.0 }, { Not { hammer1 } })"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::SolveCall(solve) => {
                assert_eq!(solve.var_name, "a_john_instance");
                assert_eq!(solve.objectives.len(), 3);
                assert_eq!(solve.constraints.len(), 1);
            }
            _ => panic!("Expected solve call"),
        }
    }

    #[test]
    fn test_complex_lambda_with_comparison() {
        let input = r#"type Test: All {
            workable: bool = (c: Test) -> c.age >= 18
        }"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::TypeDecl(type_decl) => {
                assert_eq!(type_decl.fields.len(), 1);
                assert!(type_decl.fields[0].default.is_some());
                // Check that the default is a lambda with a binary operation
                match &type_decl.fields[0].default {
                    Some(Expr::Lambda(lambda)) => {
                        assert_eq!(lambda.param, Some("c".to_string()));
                        assert_eq!(lambda.param_type, Some("Test".to_string()));
                        match &*lambda.body {
                            Expr::BinaryOp(binop) => {
                                assert_eq!(binop.operator, ">=");
                            }
                            _ => panic!("Expected binary operation in lambda body"),
                        }
                    }
                    _ => panic!("Expected lambda expression as default"),
                }
            }
            _ => panic!("Expected type declaration"),
        }
    }

    #[test]
    fn test_builtin_call_with_lambda() {
        let input = r#"test = find((h: Hammer) -> h.size >= 8)"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Assignment(assignment) => {
                match &assignment.value {
                    Expr::BuiltinCall(call) => {
                        assert_eq!(call.name, "find");
                        assert_eq!(call.args.len(), 1);
                        match &call.args[0] {
                            Arg::Positional(Expr::Lambda(_)) => {
                                // Success - we have a lambda as argument
                            }
                            _ => panic!("Expected lambda as argument to find"),
                        }
                    }
                    _ => panic!("Expected builtin call"),
                }
            }
            _ => panic!("Expected assignment"),
        }
    }

    #[test]
    fn test_global_context_symbol() {
        let input = "test = *";
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Assignment(assignment) => {
                match &assignment.value {
                    Expr::GlobalContext => {
                        // Success - global context parsed correctly
                    }
                    _ => panic!("Expected global context expression"),
                }
            }
            _ => panic!("Expected assignment"),
        }
    }

    #[test] 
    fn test_find_with_global_context() {
        let input = r#"result = find(*, (h: Hammer) -> h.size >= 8)"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Assignment(assignment) => {
                match &assignment.value {
                    Expr::BuiltinCall(call) => {
                        assert_eq!(call.name, "find");
                        assert_eq!(call.args.len(), 2);
                        
                        // First argument should be global context
                        match &call.args[0] {
                            Arg::Positional(Expr::GlobalContext) => {},
                            _ => panic!("Expected global context as first argument"),
                        }
                        
                        // Second argument should be lambda
                        match &call.args[1] {
                            Arg::Positional(Expr::Lambda(_)) => {},
                            _ => panic!("Expected lambda as second argument"),
                        }
                    }
                    _ => panic!("Expected builtin call"),
                }
            }
            _ => panic!("Expected assignment"),
        }
    }

    #[test]
    fn test_solve_with_global_context() {
        let input = r#"solution = solve(*, { nail1: 1.0 })"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::SolveCall(solve) => {
                assert_eq!(solve.var_name, "solution");
                
                // Target should be global context
                match &solve.target {
                    Expr::GlobalContext => {},
                    _ => panic!("Expected global context as target"),
                }
                
                // Should have one objective
                assert_eq!(solve.objectives.len(), 1);
                assert_eq!(solve.objectives[0].name, "nail1");
                assert_eq!(solve.objectives[0].weight, 1.0);
            }
            _ => panic!("Expected solve call"),
        }
    }

    #[test]
    fn test_new_match_syntax() {
        let input = r#"test = match({
            _: 5
        })"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Assignment(assignment) => {
                match &assignment.value {
                    Expr::Match(match_expr) => {
                        assert_eq!(match_expr.cases.len(), 1);
                        match &match_expr.cases[0] {
                            MatchCase::Wildcard { value } => {
                                match value {
                                    Expr::Literal(Literal::Int(5)) => {},
                                    _ => panic!("Expected literal int 5"),
                                }
                            }
                            _ => panic!("Expected wildcard case"),
                        }
                    }
                    _ => panic!("Expected match expression"),
                }
            }
            _ => panic!("Expected assignment"),
        }
    }

    #[test]
    fn test_match_in_lambda() {
        let input = r#"weight = (h: Hammer) -> match({
            _: 5
        })"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Assignment(assignment) => {
                match &assignment.value {
                    Expr::Lambda(lambda) => {
                        assert_eq!(lambda.param, Some("h".to_string()));
                        assert_eq!(lambda.param_type, Some("Hammer".to_string()));
                        match &*lambda.body {
                            Expr::Match(match_expr) => {
                                assert_eq!(match_expr.cases.len(), 1);
                            }
                            _ => panic!("Expected match expression in lambda body"),
                        }
                    }
                    _ => panic!("Expected lambda expression"),
                }
            }
            _ => panic!("Expected assignment"),
        }
    }

    #[test]
    fn test_enum_declaration() {
        let input = r#"enum Material {
            Steel,
            Wood,
            Plastic
        }"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::EnumDecl(enum_decl) => {
                assert_eq!(enum_decl.name, "Material");
                assert_eq!(enum_decl.variants.len(), 3);
                assert_eq!(enum_decl.variants[0].name, "Steel");
                assert_eq!(enum_decl.variants[0].value, None);
                assert_eq!(enum_decl.variants[1].name, "Wood");
                assert_eq!(enum_decl.variants[2].name, "Plastic");
            }
            _ => panic!("Expected enum declaration"),
        }
    }

    #[test]
    fn test_enum_with_values() {
        let input = r#"enum Size {
            Small = 1,
            Medium = 2,
            Large = 3
        }"#;
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::EnumDecl(enum_decl) => {
                assert_eq!(enum_decl.name, "Size");
                assert_eq!(enum_decl.variants.len(), 3);
                assert_eq!(enum_decl.variants[0].name, "Small");
                assert_eq!(enum_decl.variants[0].value, Some(1));
                assert_eq!(enum_decl.variants[1].name, "Medium");
                assert_eq!(enum_decl.variants[1].value, Some(2));
                assert_eq!(enum_decl.variants[2].name, "Large");
                assert_eq!(enum_decl.variants[2].value, Some(3));
            }
            _ => panic!("Expected enum declaration"),
        }
    }

    #[test]
    fn test_enum_access() {
        let input = "test = Material.Steel";
        let result = parse_program(input).unwrap();
        
        assert_eq!(result.statements.len(), 1);
        match &result.statements[0] {
            Statement::Assignment(assignment) => {
                match &assignment.value {
                    Expr::EnumAccess(enum_access) => {
                        assert_eq!(enum_access.enum_name, "Material");
                        assert_eq!(enum_access.variant_name, "Steel");
                    }
                    _ => panic!("Expected enum access expression"),
                }
            }
            _ => panic!("Expected assignment"),
        }
    }
}