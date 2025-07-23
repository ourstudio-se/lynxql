use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{
        alpha1, alphanumeric1, char, digit1, line_ending, multispace0, multispace1, space0,
    },
    combinator::{map, opt, recognize, value},
    error::ParseError,
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, Parser,
};
use std::collections::HashMap;
use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    TypeDecl(TypeDecl),
    Alias(Alias),
    Assignment(Assignment),
    Solve(Solve),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl {
    pub name: String,
    pub base_type: BaseType,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Alias {
    pub name: String,
    pub target: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub name: String,
    pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Solve {
    pub name: String,
    pub method: SolveMethod,
    pub objective: Expression,
    pub constraints: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SolveMethod {
    Minimize,
    Maximize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseType {
    Bool,
    Integer(Option<i64>, Option<i64>),
    Float,
    String,
    All,
    Any,
    Not,
    Exactly(i64),
    AtLeast(i64),
    AtMost(i64),
    Free,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub field_type: Type,
    pub is_attribute: bool,
    pub default_value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Named(String),
    Parameterized(String, Vec<Type>),
    Collection(Box<Type>),
    Bounded(Box<Type>, i64, i64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Bool,
    Int,
    Float,
    String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    FieldAccess(Box<Expression>, String),
    Call(String, Vec<Expression>),
    Lambda(Lambda),
    LogicOp(LogicOp),
    Constructor(String, Vec<Type>, Vec<FieldAssignment>),
    Range(i64, i64),
    Filter(Box<Expression>, Box<Expression>),
    Spread(Box<Expression>),
    GlobalContext,
    Index(Box<Expression>, Box<Expression>),
    BinaryOp(BinaryOp),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub param: Option<String>,
    pub param_type: Option<String>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicOp {
    pub op: LogicOperator,
    pub operands: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicOperator {
    All,
    Any,
    Not,
    Exactly(i64),
    AtLeast(i64),
    AtMost(i64),
    Free,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAssignment {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOp {
    pub op: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Error)]
pub enum LynxError {
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("Type error: {0}")]
    TypeError(String),
    #[error("Undefined symbol: {0}")]
    UndefinedSymbol(String),
    #[error("Invalid type: {0}")]
    InvalidType(String),
}

pub type LynxResult<T> = Result<T, LynxError>;

pub fn parse_program(input: &str) -> IResult<&str, Program> {
    let (input, _) = skip_whitespace_and_comments(input)?;
    map(
        many0(preceded(skip_whitespace_and_comments, parse_statement)),
        |statements| Program { statements },
    )(input)
}

fn skip_whitespace_and_comments(input: &str) -> IResult<&str, ()> {
    let (input, _) = many0(alt((multispace1, single_line_comment, multi_line_comment)))(input)?;
    Ok((input, ()))
}

fn single_line_comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("//")(input)?;
    let (input, comment) = take_until("\n")(input)?;
    let (input, _) = char('\n')(input)?;
    Ok((input, comment))
}

fn multi_line_comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("/*")(input)?;
    let (input, comment) = take_until("*/")(input)?;
    let (input, _) = tag("*/")(input)?;
    Ok((input, comment))
}

fn parse_statement(input: &str) -> IResult<&str, Statement> {
    alt((
        map(parse_type_decl, Statement::TypeDecl),
        map(parse_alias, Statement::Alias),
        map(parse_solve, Statement::Solve),
        map(parse_assignment, Statement::Assignment),
    ))(input)
}

fn parse_type_decl(input: &str) -> IResult<&str, TypeDecl> {
    let (input, _) = tag("type")(input)?;
    let (input, _) = skip_whitespace_and_comments(input)?;
    let (input, name) = parse_type_name(input)?;
    let (input, _) = skip_whitespace_and_comments(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = skip_whitespace_and_comments(input)?;
    let (input, base_type) = parse_base_type(input)?;
    let (input, _) = skip_whitespace_and_comments(input)?;
    let (input, fields) = opt(delimited(
        char('{'),
        preceded(
            skip_whitespace_and_comments,
            separated_list0(
                tuple((skip_whitespace_and_comments, char(','), skip_whitespace_and_comments)),
                parse_field,
            ),
        ),
        preceded(skip_whitespace_and_comments, char('}')),
    ))(input)?;

    Ok((
        input,
        TypeDecl {
            name,
            base_type,
            fields: fields.unwrap_or_default(),
        },
    ))
}

fn parse_alias(input: &str) -> IResult<&str, Alias> {
    let (input, _) = tag("alias")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = parse_type_name(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, target) = parse_type(input)?;

    Ok((input, Alias { name, target }))
}

fn parse_assignment(input: &str) -> IResult<&str, Assignment> {
    let (input, name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expr) = parse_expression(input)?;

    Ok((input, Assignment { name, expr }))
}

fn parse_solve(input: &str) -> IResult<&str, Solve> {
    let (input, name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, method) = alt((
        value(SolveMethod::Minimize, tag("minimize")),
        value(SolveMethod::Maximize, tag("maximize")),
    ))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('(')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("objective")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, objective) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(',')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("suchThat")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, constraints) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(')')(input)?;

    Ok((
        input,
        Solve {
            name,
            method,
            objective,
            constraints,
        },
    ))
}

fn parse_base_type(input: &str) -> IResult<&str, BaseType> {
    alt((
        value(BaseType::Bool, tag("Bool")),
        map(
            tuple((
                tag("Integer"),
                opt(delimited(
                    char('<'),
                    tuple((
                        preceded(multispace0, parse_integer),
                        preceded(tuple((multispace0, char(','), multispace0)), parse_integer),
                    )),
                    preceded(multispace0, char('>')),
                )),
            )),
            |(_, bounds)| {
                if let Some((min, max)) = bounds {
                    BaseType::Integer(Some(min), Some(max))
                } else {
                    BaseType::Integer(None, None)
                }
            },
        ),
        value(BaseType::Float, tag("Float")),
        value(BaseType::String, tag("String")),
        value(BaseType::All, tag("All")),
        value(BaseType::Any, tag("Any")),
        value(BaseType::Not, tag("Not")),
        value(BaseType::Free, tag("Free")),
        map(
            tuple((
                tag("Exactly"),
                delimited(char('<'), preceded(multispace0, parse_integer), char('>')),
            )),
            |(_, n)| BaseType::Exactly(n),
        ),
        map(
            tuple((
                tag("AtLeast"),
                delimited(char('<'), preceded(multispace0, parse_integer), char('>')),
            )),
            |(_, n)| BaseType::AtLeast(n),
        ),
        map(
            tuple((
                tag("AtMost"),
                delimited(char('<'), preceded(multispace0, parse_integer), char('>')),
            )),
            |(_, n)| BaseType::AtMost(n),
        ),
    ))(input)
}

fn parse_field(input: &str) -> IResult<&str, Field> {
    let (input, is_attribute) = map(opt(char('@')), |attr| attr.is_some())(input)?;
    let (input, name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, field_type) = parse_type(input)?;
    let (input, _) = multispace0(input)?;
    let (input, default_value) =
        opt(preceded(tuple((char('='), multispace0)), parse_expression))(input)?;

    Ok((
        input,
        Field {
            name,
            field_type,
            is_attribute,
            default_value,
        },
    ))
}

fn parse_type(input: &str) -> IResult<&str, Type> {
    alt((
        map(parse_primitive_type, Type::Primitive),
        map(
            tuple((
                parse_type_name,
                opt(delimited(
                    char('<'),
                    separated_list1(
                        preceded(multispace0, char(',')), 
                        alt((
                            parse_type,
                            // Allow integer literals as type parameters
                            map(parse_integer, |i| Type::Named(i.to_string())),
                        ))
                    ),
                    char('>'),
                )),
                opt(delimited(char('['), parse_type, char(']'))),
            )),
            |(name, params, collection)| {
                let base_type = if let Some(params) = params {
                    Type::Parameterized(name, params)
                } else {
                    Type::Named(name)
                };

                if let Some(inner_type) = collection {
                    Type::Collection(Box::new(base_type))
                } else {
                    base_type
                }
            },
        ),
    ))(input)
}

fn parse_primitive_type(input: &str) -> IResult<&str, PrimitiveType> {
    alt((
        value(PrimitiveType::Bool, tag("bool")),
        value(PrimitiveType::Int, tag("int")),
        value(PrimitiveType::Float, tag("float")),
        value(PrimitiveType::String, tag("str")),
    ))(input)
}

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        parse_logic_op,
        parse_filter,
        parse_constructor,
        parse_call,
        parse_lambda,
        parse_binary_op,
        parse_index,
        parse_range,
        parse_field_access,
        parse_spread,
        map(parse_literal, Expression::Literal),
        map(parse_identifier, Expression::Identifier),
        value(Expression::GlobalContext, char('*')),
    ))(input)
}

fn parse_range(input: &str) -> IResult<&str, Expression> {
    let (input, start) = parse_integer(input)?;
    let (input, _) = tag("..")(input)?;
    let (input, end) = parse_integer(input)?;
    Ok((input, Expression::Range(start, end)))
}

fn parse_logic_op(input: &str) -> IResult<&str, Expression> {
    let (input, op) = parse_logic_operator(input)?;
    let (input, _) = multispace0(input)?;
    let (input, operands) = delimited(
        char('('),
        preceded(
            multispace0,
            separated_list0(
                tuple((multispace0, char(','), multispace0)),
                parse_expression,
            ),
        ),
        preceded(multispace0, char(')')),
    )(input)?;

    Ok((input, Expression::LogicOp(LogicOp { op, operands })))
}

fn parse_logic_operator(input: &str) -> IResult<&str, LogicOperator> {
    alt((
        value(LogicOperator::All, tag("All")),
        value(LogicOperator::Any, tag("Any")),
        value(LogicOperator::Not, tag("Not")),
        value(LogicOperator::Free, tag("Free")),
        map(
            tuple((
                tag("Exactly"),
                delimited(char('<'), preceded(multispace0, parse_integer), char('>')),
            )),
            |(_, n)| LogicOperator::Exactly(n),
        ),
        map(
            tuple((
                tag("AtLeast"),
                delimited(char('<'), preceded(multispace0, parse_integer), char('>')),
            )),
            |(_, n)| LogicOperator::AtLeast(n),
        ),
        map(
            tuple((
                tag("AtMost"),
                delimited(char('<'), preceded(multispace0, parse_integer), char('>')),
            )),
            |(_, n)| LogicOperator::AtMost(n),
        ),
    ))(input)
}

fn parse_constructor(input: &str) -> IResult<&str, Expression> {
    let (input, name) = parse_type_name(input)?;
    let (input, _) = multispace0(input)?;
    let (input, type_params) = opt(delimited(
        char('<'),
        separated_list1(
            tuple((multispace0, char(','), multispace0)), 
            alt((
                parse_type,
                // Allow integer literals as type parameters
                map(parse_integer, |i| Type::Named(i.to_string())),
            ))
        ),
        char('>'),
    ))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, fields) = delimited(
        char('('),
        preceded(
            multispace0,
            separated_list1(  // Require at least one field
                tuple((multispace0, char(','), multispace0)),
                alt((
                    parse_field_assignment,
                    // Allow spread expressions as special case
                    map(parse_spread, |expr| FieldAssignment {
                        name: String::new(),
                        value: expr,
                    }),
                )),
            ),
        ),
        preceded(multispace0, char(')')),
    )(input)?;

    Ok((input, Expression::Constructor(name, type_params.unwrap_or_default(), fields)))
}

fn parse_field_assignment(input: &str) -> IResult<&str, FieldAssignment> {
    let (input, name) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, value) = parse_expression(input)?;

    Ok((input, FieldAssignment { name, value }))
}

fn parse_call(input: &str) -> IResult<&str, Expression> {
    let (input, name) = alt((parse_identifier, parse_type_name))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, args) = delimited(
        char('('),
        preceded(
            multispace0,
            separated_list0(
                tuple((multispace0, char(','), multispace0)),
                parse_expression,
            ),
        ),
        preceded(multispace0, char(')')),
    )(input)?;

    Ok((input, Expression::Call(name, args)))
}

fn parse_lambda(input: &str) -> IResult<&str, Expression> {
    // Try parenthesized form first: (param) -> body
    let parenthesized = map(
        tuple((
            char('('),
            multispace0,
            opt(alt((
                map(
                    tuple((parse_type_name, preceded(multispace1, parse_identifier))),
                    |(type_name, param)| (Some(param), Some(type_name)),
                ),
                map(parse_identifier, |param| (Some(param), None)),
            ))),
            multispace0,
            char(')'),
            multispace0,
            tag("->"),
            multispace0,
            parse_expression,
        )),
        |(_, _, param_info, _, _, _, _, _, body)| {
            let (param, param_type) = param_info.unwrap_or((None, None));
            Expression::Lambda(Lambda {
                param,
                param_type,
                body: Box::new(body),
            })
        },
    );

    // Try unparenthesized form: param -> body  
    let unparenthesized = map(
        tuple((
            parse_identifier,
            multispace0,
            tag("->"),
            multispace0,
            parse_expression,
        )),
        |(param, _, _, _, body)| {
            Expression::Lambda(Lambda {
                param: Some(param),
                param_type: None,
                body: Box::new(body),
            })
        },
    );

    alt((parenthesized, unparenthesized))(input)
}

fn parse_field_access(input: &str) -> IResult<&str, Expression> {
    let (input, base) = parse_identifier(input)?;
    let (input, fields) = many1(preceded(char('.'), parse_identifier))(input)?;

    let mut expr = Expression::Identifier(base);
    for field in fields {
        expr = Expression::FieldAccess(Box::new(expr), field);
    }

    Ok((input, expr))
}

fn parse_filter(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("filter")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('(')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, context) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(',')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, predicate) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(')')(input)?;

    Ok((
        input,
        Expression::Filter(Box::new(context), Box::new(predicate)),
    ))
}

fn parse_spread(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("...")(input)?;
    let (input, expr) = parse_expression(input)?;
    Ok((input, Expression::Spread(Box::new(expr))))
}

fn parse_index(input: &str) -> IResult<&str, Expression> {
    let (input, base) = alt((
        map(parse_identifier, Expression::Identifier),
        delimited(char('('), parse_expression, char(')')),
    ))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, index) = delimited(
        char('['),
        preceded(multispace0, parse_expression),
        preceded(multispace0, char(']')),
    )(input)?;
    Ok((input, Expression::Index(Box::new(base), Box::new(index))))
}

fn parse_binary_op(input: &str) -> IResult<&str, Expression> {
    let (input, left) = alt((
        parse_index,
        parse_field_access,
        map(parse_literal, Expression::Literal),
        map(parse_identifier, Expression::Identifier),
        delimited(char('('), parse_expression, char(')')),
    ))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, op) = parse_binary_operator(input)?;
    let (input, _) = multispace0(input)?;
    let (input, right) = parse_expression(input)?;
    Ok((input, Expression::BinaryOp(BinaryOp {
        op,
        left: Box::new(left),
        right: Box::new(right),
    })))
}

fn parse_binary_operator(input: &str) -> IResult<&str, BinaryOperator> {
    alt((
        value(BinaryOperator::Equal, tag("==")),
        value(BinaryOperator::NotEqual, tag("!=")),
        value(BinaryOperator::LessThanOrEqual, tag("<=")),
        value(BinaryOperator::GreaterThanOrEqual, tag(">=")),
        value(BinaryOperator::LessThan, tag("<")),
        value(BinaryOperator::GreaterThan, tag(">")),
    ))(input)
}

fn parse_literal(input: &str) -> IResult<&str, Literal> {
    alt((
        value(Literal::Bool(true), tag("true")),
        value(Literal::Bool(false), tag("false")),
        map(parse_float, Literal::Float),
        map(parse_integer, Literal::Int),
        map(parse_string, Literal::String),
    ))(input)
}

fn parse_string(input: &str) -> IResult<&str, String> {
    let (input, _) = char('"')(input)?;
    let (input, content) = take_until("\"")(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, content.to_string()))
}

fn parse_float(input: &str) -> IResult<&str, f64> {
    let (input, float_str) = recognize(tuple((digit1, char('.'), digit1)))(input)?;
    Ok((input, float_str.parse().unwrap()))
}

fn parse_integer(input: &str) -> IResult<&str, i64> {
    let (input, sign) = opt(char('-'))(input)?;
    let (input, digits) = digit1(input)?;
    let mut num: i64 = digits.parse().unwrap();
    if sign.is_some() {
        num = -num;
    }
    Ok((input, num))
}

fn parse_identifier(input: &str) -> IResult<&str, String> {
    let (input, first) = alt((alpha1, tag("_")))(input)?;
    let (input, rest) = many0(alt((alphanumeric1, tag("_"))))(input)?;
    let mut result = first.to_string();
    for part in rest {
        result.push_str(part);
    }
    Ok((input, result))
}

fn parse_type_name(input: &str) -> IResult<&str, String> {
    let (input, first_char) = take_while1(|c: char| c.is_ascii_uppercase())(input)?;
    let (input, rest) = many0(alt((alphanumeric1, tag("_"))))(input)?;
    let mut result = first_char.to_string();
    for part in rest {
        result.push_str(part);
    }
    Ok((input, result))
}

#[derive(Debug, Clone)]
pub struct TypeChecker {
    types: HashMap<String, TypeDecl>,
    aliases: HashMap<String, Type>,
    variables: HashMap<String, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            aliases: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn check_program(&mut self, program: &Program) -> LynxResult<()> {
        for statement in &program.statements {
            self.check_statement(statement)?;
        }
        Ok(())
    }

    fn check_statement(&mut self, statement: &Statement) -> LynxResult<()> {
        match statement {
            Statement::TypeDecl(type_decl) => {
                self.check_type_decl(type_decl)?;
                self.types.insert(type_decl.name.clone(), type_decl.clone());
            }
            Statement::Alias(alias) => {
                self.check_type(&alias.target)?;
                self.aliases
                    .insert(alias.name.clone(), alias.target.clone());
            }
            Statement::Assignment(assignment) => {
                let expr_type = self.infer_type(&assignment.expr)?;
                self.variables.insert(assignment.name.clone(), expr_type);
            }
            Statement::Solve(solve) => {
                self.check_expression(&solve.objective)?;
                self.check_expression(&solve.constraints)?;
            }
        }
        Ok(())
    }

    fn check_type_decl(&self, type_decl: &TypeDecl) -> LynxResult<()> {
        for field in &type_decl.fields {
            self.check_type(&field.field_type)?;
            if let Some(default_value) = &field.default_value {
                self.check_expression(default_value)?;
            }
        }
        Ok(())
    }

    fn check_type(&self, type_ref: &Type) -> LynxResult<()> {
        match type_ref {
            Type::Primitive(_) => Ok(()),
            Type::Named(name) => {
                if !self.types.contains_key(name) && !self.aliases.contains_key(name) {
                    Err(LynxError::UndefinedSymbol(name.clone()))
                } else {
                    Ok(())
                }
            }
            Type::Parameterized(name, params) => {
                self.check_type(&Type::Named(name.clone()))?;
                for param in params {
                    self.check_type(param)?;
                }
                Ok(())
            }
            Type::Collection(inner) => self.check_type(inner),
            Type::Bounded(inner, _, _) => self.check_type(inner),
        }
    }

    fn check_expression(&self, expr: &Expression) -> LynxResult<()> {
        match expr {
            Expression::Literal(_) => Ok(()),
            Expression::Identifier(name) => {
                if !self.variables.contains_key(name) {
                    Err(LynxError::UndefinedSymbol(name.clone()))
                } else {
                    Ok(())
                }
            }
            Expression::FieldAccess(base, _) => self.check_expression(base),
            Expression::Call(_, args) => {
                for arg in args {
                    self.check_expression(arg)?;
                }
                Ok(())
            }
            Expression::Lambda(lambda) => self.check_expression(&lambda.body),
            Expression::LogicOp(logic_op) => {
                for operand in &logic_op.operands {
                    self.check_expression(operand)?;
                }
                Ok(())
            }
            Expression::Constructor(name, _, fields) => {
                if !self.types.contains_key(name) {
                    return Err(LynxError::UndefinedSymbol(name.clone()));
                }
                for field in fields {
                    self.check_expression(&field.value)?;
                }
                Ok(())
            }
            Expression::Range(_, _) => Ok(()),
            Expression::Filter(context, predicate) => {
                self.check_expression(context)?;
                self.check_expression(predicate)
            }
            Expression::Spread(expr) => self.check_expression(expr),
            Expression::GlobalContext => Ok(()),
            Expression::Index(base, index) => {
                self.check_expression(base)?;
                self.check_expression(index)
            }
            Expression::BinaryOp(binary_op) => {
                self.check_expression(&binary_op.left)?;
                self.check_expression(&binary_op.right)
            }
        }
    }

    fn infer_type(&self, expr: &Expression) -> LynxResult<Type> {
        match expr {
            Expression::Literal(literal) => Ok(self.literal_type(literal)),
            Expression::Identifier(name) => self
                .variables
                .get(name)
                .cloned()
                .ok_or_else(|| LynxError::UndefinedSymbol(name.clone())),
            Expression::Range(_, _) => Ok(Type::Primitive(PrimitiveType::Int)),
            Expression::Constructor(name, _, _) => {
                if self.types.contains_key(name) {
                    Ok(Type::Named(name.clone()))
                } else {
                    Err(LynxError::UndefinedSymbol(name.clone()))
                }
            }
            Expression::Call(name, _) => match name.as_str() {
                "Bool" => Ok(Type::Primitive(PrimitiveType::Bool)),
                "Integer" => Ok(Type::Primitive(PrimitiveType::Int)),
                _ => Err(LynxError::UndefinedSymbol(name.clone())),
            },
            Expression::LogicOp(_) => Ok(Type::Primitive(PrimitiveType::Bool)),
            _ => Err(LynxError::TypeError("Cannot infer type".to_string())),
        }
    }

    fn literal_type(&self, literal: &Literal) -> Type {
        match literal {
            Literal::Bool(_) => Type::Primitive(PrimitiveType::Bool),
            Literal::Int(_) => Type::Primitive(PrimitiveType::Int),
            Literal::Float(_) => Type::Primitive(PrimitiveType::Float),
            Literal::String(_) => Type::Primitive(PrimitiveType::String),
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_assignment() {
        let input = "x = Bool()";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        assert_eq!(assignment.name, "x");
        match assignment.expr {
            Expression::Call(name, args) => {
                assert_eq!(name, "Bool");
                assert_eq!(args.len(), 0);
            }
            _ => panic!("Expected Call expression"),
        }
    }

    #[test]
    fn test_parse_type_declaration() {
        let input = "type Color : Bool { @tag : str }";
        let result = parse_type_decl(input);
        assert!(result.is_ok());
        let (_, type_decl) = result.unwrap();
        assert_eq!(type_decl.name, "Color");
        assert_eq!(type_decl.fields.len(), 1);
        assert_eq!(type_decl.fields[0].name, "tag");
        assert!(type_decl.fields[0].is_attribute);
    }

    #[test]
    fn test_parse_range() {
        let input = "z = -2..1";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        match assignment.expr {
            Expression::Range(start, end) => {
                assert_eq!(start, -2);
                assert_eq!(end, 1);
            }
            _ => panic!("Expected Range expression"),
        }
    }

    #[test]
    fn test_parse_logic_op() {
        let input = "a = All(x, y, z)";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        match assignment.expr {
            Expression::LogicOp(logic_op) => {
                assert!(matches!(logic_op.op, LogicOperator::All));
                assert_eq!(logic_op.operands.len(), 3);
            }
            _ => panic!("Expected LogicOp expression"),
        }
    }

    #[test]
    fn test_type_checker() {
        let mut checker = TypeChecker::new();

        let type_decl = TypeDecl {
            name: "Color".to_string(),
            base_type: BaseType::Bool,
            fields: vec![Field {
                name: "tag".to_string(),
                field_type: Type::Primitive(PrimitiveType::String),
                is_attribute: true,
                default_value: None,
            }],
        };

        assert!(checker.check_type_decl(&type_decl).is_ok());
        checker.types.insert("Color".to_string(), type_decl);

        let assignment = Assignment {
            name: "red".to_string(),
            expr: Expression::Constructor(
                "Color".to_string(),
                vec![],
                vec![FieldAssignment {
                    name: "tag".to_string(),
                    value: Expression::Literal(Literal::String("red".to_string())),
                }],
            ),
        };

        assert!(checker.check_expression(&assignment.expr).is_ok());
    }

    #[test]
    fn test_parse_comments() {
        let input = "// Line comment\n/* Block comment */\nx = Bool()";
        let result = parse_program(input);
        assert!(result.is_ok());
        let (_, program) = result.unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Assignment(assignment) => {
                assert_eq!(assignment.name, "x");
            }
            _ => panic!("Expected Assignment statement"),
        }
    }

    #[test]
    fn test_parse_integer_with_bounds() {
        let input = "y = Integer(0, 5)";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        match assignment.expr {
            Expression::Call(name, args) => {
                assert_eq!(name, "Integer");
                assert_eq!(args.len(), 2);
            }
            _ => panic!("Expected Call expression"),
        }
    }

    #[test]
    fn test_parse_type_with_multiple_fields() {
        let input = "type Wheel : Bool { @tag : str, @weight : float }";
        let result = parse_type_decl(input);
        assert!(result.is_ok());
        let (_, type_decl) = result.unwrap();
        assert_eq!(type_decl.name, "Wheel");
        assert_eq!(type_decl.fields.len(), 2);
        assert_eq!(type_decl.fields[0].name, "tag");
        assert_eq!(type_decl.fields[1].name, "weight");
        assert!(type_decl.fields[0].is_attribute);
        assert!(type_decl.fields[1].is_attribute);
    }

    #[test]
    fn test_parse_constructor_with_field_assignments() {
        let input = "red = Color(tag = \"red\")";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        match assignment.expr {
            Expression::Constructor(name, _, fields) => {
                assert_eq!(name, "Color");
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].name, "tag");
                match &fields[0].value {
                    Expression::Literal(Literal::String(s)) => {
                        assert_eq!(s, "red");
                    }
                    _ => panic!("Expected String literal"),
                }
            }
            _ => panic!("Expected Constructor expression"),
        }
    }

    #[test]
    fn test_parse_composite_type_with_relationships() {
        let input = r#"type Price : All {
            colors : Any[Color],
            wheel  : Wheel,
            @price : float
        }"#;
        let result = parse_type_decl(input);
        assert!(result.is_ok());
        let (_, type_decl) = result.unwrap();
        assert_eq!(type_decl.name, "Price");
        assert!(matches!(type_decl.base_type, BaseType::All));
        assert_eq!(type_decl.fields.len(), 3);

        // Check relationship fields
        assert_eq!(type_decl.fields[0].name, "colors");
        assert!(!type_decl.fields[0].is_attribute);
        assert_eq!(type_decl.fields[1].name, "wheel");
        assert!(!type_decl.fields[1].is_attribute);

        // Check attribute field
        assert_eq!(type_decl.fields[2].name, "price");
        assert!(type_decl.fields[2].is_attribute);
    }

    #[test]
    fn test_parse_type_alias() {
        let input = "alias ColorRule = Exactly<1>[Color]";
        let result = parse_alias(input);
        assert!(result.is_ok());
        let (_, alias) = result.unwrap();
        assert_eq!(alias.name, "ColorRule");
        match alias.target {
            Type::Collection(inner) => match *inner {
                Type::Parameterized(name, params) => {
                    assert_eq!(name, "Exactly");
                    assert_eq!(params.len(), 1);
                }
                _ => panic!("Expected Parameterized type"),
            },
            _ => panic!("Expected Collection type"),
        }
    }

    #[test]
    fn test_parse_lambda_expression() {
        let input = "@totalPrice : float = (Bicycle b) -> sum(b.prices.price)";
        let result = parse_field(input);
        assert!(result.is_ok());
        let (_, field) = result.unwrap();
        assert_eq!(field.name, "totalPrice");
        assert!(field.is_attribute);
        assert!(field.default_value.is_some());

        match field.default_value.unwrap() {
            Expression::Lambda(lambda) => {
                assert_eq!(lambda.param, Some("b".to_string()));
                assert_eq!(lambda.param_type, Some("Bicycle".to_string()));
            }
            _ => panic!("Expected Lambda expression"),
        }
    }

    #[test]
    fn test_parse_field_access() {
        let input = "total = myBike.totalPrice";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        match assignment.expr {
            Expression::FieldAccess(base, field) => {
                match *base {
                    Expression::Identifier(name) => {
                        assert_eq!(name, "myBike");
                    }
                    _ => panic!("Expected Identifier"),
                }
                assert_eq!(field, "totalPrice");
            }
            _ => panic!("Expected FieldAccess expression"),
        }
    }

    #[test]
    fn test_parse_filter_expression() {
        let input = "aRed = filter(*, c -> c.tag)";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        match assignment.expr {
            Expression::Filter(context, predicate) => {
                match *context {
                    Expression::GlobalContext => {}
                    _ => panic!("Expected GlobalContext"),
                }
                match *predicate {
                    Expression::Lambda(_) => {}
                    _ => panic!("Expected Lambda predicate"),
                }
            }
            _ => panic!("Expected Filter expression"),
        }
    }

    #[test]
    fn test_parse_spread_operator() {
        let input = "rule = ColorRule(...aRed)";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        match assignment.expr {
            Expression::Constructor(name, _, fields) => {
                assert_eq!(name, "ColorRule");
                assert_eq!(fields.len(), 1);
                match &fields[0].value {
                    Expression::Spread(inner) => match **inner {
                        Expression::Identifier(ref name) => {
                            assert_eq!(name, "aRed");
                        }
                        _ => panic!("Expected Identifier in spread"),
                    },
                    _ => panic!("Expected Spread expression"),
                }
            }
            _ => panic!("Expected Constructor expression"),
        }
    }

    #[test]
    fn test_parse_solve_statement() {
        let input = r#"solution = minimize(
            objective = myBike.totalPrice,
            suchThat = All(myBike)
        )"#;
        let result = parse_solve(input);
        assert!(result.is_ok());
        let (_, solve) = result.unwrap();
        assert_eq!(solve.name, "solution");
        assert!(matches!(solve.method, SolveMethod::Minimize));

        match solve.objective {
            Expression::FieldAccess(base, field) => {
                match *base {
                    Expression::Identifier(name) => {
                        assert_eq!(name, "myBike");
                    }
                    _ => panic!("Expected Identifier"),
                }
                assert_eq!(field, "totalPrice");
            }
            _ => panic!("Expected FieldAccess expression"),
        }
    }

    #[test]
    fn test_parse_parameterized_type() {
        let input = "type Week : Integer<0, 52> { @dates: str }";
        let result = parse_type_decl(input);
        assert!(result.is_ok());
        let (_, type_decl) = result.unwrap();
        assert_eq!(type_decl.name, "Week");
        match type_decl.base_type {
            BaseType::Integer(min, max) => {
                assert_eq!(min, Some(0));
                assert_eq!(max, Some(52));
            }
            _ => panic!("Expected Integer base type"),
        }
    }

    #[test]
    fn test_parse_exactly_operator() {
        let input = "constraint = Exactly<12>(productionWeek)";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        match assignment.expr {
            Expression::LogicOp(logic_op) => {
                match logic_op.op {
                    LogicOperator::Exactly(n) => {
                        assert_eq!(n, 12);
                    }
                    _ => panic!("Expected Exactly operator"),
                }
                assert_eq!(logic_op.operands.len(), 1);
            }
            _ => panic!("Expected LogicOp expression"),
        }
    }

    #[test]
    fn test_complete_example_parsing() {
        let example_code = r#"
        // Boolean variable declaration 
        x = Bool()
        
        // Type declaration
        type Color : Bool { @tag : str }
        
        // Constructor
        red = Color(tag = "red")
        
        // Type alias
        alias ColorRule = Exactly<1>[Color]
        
        // Range
        z = -2..1
        "#;

        let result = parse_program(example_code);
        assert!(result.is_ok());
        let (_, program) = result.unwrap();
        assert_eq!(program.statements.len(), 5);

        // Check each statement type
        assert!(matches!(program.statements[0], Statement::Assignment(_)));
        assert!(matches!(program.statements[1], Statement::TypeDecl(_)));
        assert!(matches!(program.statements[2], Statement::Assignment(_)));
        assert!(matches!(program.statements[3], Statement::Alias(_)));
        assert!(matches!(program.statements[4], Statement::Assignment(_)));
    }

    #[test]
    fn test_parse_index_expression() {
        let input = "x = solA[0]";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        assert_eq!(assignment.name, "x");
        match assignment.expr {
            Expression::Index(base, index) => {
                match *base {
                    Expression::Identifier(name) => {
                        assert_eq!(name, "solA");
                    }
                    _ => panic!("Expected Identifier in base"),
                }
                match *index {
                    Expression::Literal(Literal::Int(i)) => {
                        assert_eq!(i, 0);
                    }
                    _ => panic!("Expected integer literal in index"),
                }
            }
            _ => panic!("Expected Index expression"),
        }
    }

    #[test]
    fn test_parse_binary_expression() {
        let input = "x = a == b";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        assert_eq!(assignment.name, "x");
        match assignment.expr {
            Expression::BinaryOp(binary_op) => {
                assert!(matches!(binary_op.op, BinaryOperator::Equal));
                match *binary_op.left {
                    Expression::Identifier(name) => {
                        assert_eq!(name, "a");
                    }
                    _ => panic!("Expected Identifier in left operand"),
                }
                match *binary_op.right {
                    Expression::Identifier(name) => {
                        assert_eq!(name, "b");
                    }
                    _ => panic!("Expected Identifier in right operand"),
                }
            }
            _ => panic!("Expected BinaryOp expression"),
        }
    }

    #[test]  
    fn test_parse_complex_index_comparison() {
        let input = "x = solA[0] == 1..1";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        assert_eq!(assignment.name, "x");
        match assignment.expr {
            Expression::BinaryOp(binary_op) => {
                assert!(matches!(binary_op.op, BinaryOperator::Equal));
                match *binary_op.left {
                    Expression::Index(_, _) => {}
                    _ => panic!("Expected Index expression in left operand"),
                }
                match *binary_op.right {
                    Expression::Range(start, end) => {
                        assert_eq!(start, 1);
                        assert_eq!(end, 1);
                    }
                    _ => panic!("Expected Range expression in right operand"),
                }
            }
            _ => panic!("Expected BinaryOp expression"),
        }
    }

    #[test]
    fn test_parse_constructor_with_type_parameters() {
        let input = "springWeeks = Week<14, 26>(dates=\"april-june\")";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        assert_eq!(assignment.name, "springWeeks");
        match assignment.expr {
            Expression::Constructor(name, type_params, fields) => {
                assert_eq!(name, "Week");
                assert_eq!(type_params.len(), 2);
                // Check type parameters are integers converted to named types
                match &type_params[0] {
                    Type::Named(n) => assert_eq!(n, "14"),
                    _ => panic!("Expected Named type for first parameter"),
                }
                match &type_params[1] {
                    Type::Named(n) => assert_eq!(n, "26"),
                    _ => panic!("Expected Named type for second parameter"),
                }
                // Check field assignment
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].name, "dates");
                match &fields[0].value {
                    Expression::Literal(Literal::String(s)) => {
                        assert_eq!(s, "april-june");
                    }
                    _ => panic!("Expected String literal in field value"),
                }
            }
            _ => panic!("Expected Constructor expression"),
        }
    }

    #[test]
    fn test_parse_multi_level_field_access() {
        let input = "x = solution.colors.firstTrue";
        let result = parse_assignment(input);
        assert!(result.is_ok());
        let (_, assignment) = result.unwrap();
        assert_eq!(assignment.name, "x");
        // Should parse as nested field access: FieldAccess(FieldAccess(solution, colors), firstTrue)
        match assignment.expr {
            Expression::FieldAccess(base, field) => {
                assert_eq!(field, "firstTrue");
                match *base {
                    Expression::FieldAccess(inner_base, inner_field) => {
                        assert_eq!(inner_field, "colors");
                        match *inner_base {
                            Expression::Identifier(name) => assert_eq!(name, "solution"),
                            _ => panic!("Expected Identifier at the base of multi-level field access"),
                        }
                    }
                    _ => panic!("Expected nested FieldAccess expression"),
                }
            }
            _ => panic!("Expected FieldAccess expression"),
        }
    }

    #[test]
    fn test_parse_assert_function_call() {
        // Test parsing assert as an expression directly since it's a function call
        let input = "assert(solA[0] == 1..1)";
        let result = parse_expression(input);
        assert!(result.is_ok());
        let (_, expr) = result.unwrap();
        match expr {
            Expression::Call(name, args) => {
                assert_eq!(name, "assert");
                assert_eq!(args.len(), 1);
                // Check the argument is a binary operation
                match &args[0] {
                    Expression::BinaryOp(binary_op) => {
                        assert!(matches!(binary_op.op, BinaryOperator::Equal));
                        // Left side should be an index operation
                        match &*binary_op.left {
                            Expression::Index(_, _) => {}
                            _ => panic!("Expected Index expression in left operand of assert argument"),
                        }
                        // Right side should be a range
                        match &*binary_op.right {
                            Expression::Range(start, end) => {
                                assert_eq!(*start, 1);
                                assert_eq!(*end, 1);
                            }
                            _ => panic!("Expected Range expression in right operand of assert argument"),
                        }
                    }
                    _ => panic!("Expected BinaryOp as assert argument"),
                }
            }
            _ => panic!("Expected Call expression for assert"),
        }
    }

    #[test]
    fn test_parse_has_predicate_function() {
        let input = "Has(frontW.tag, \"front\")";
        let result = parse_expression(input);
        assert!(result.is_ok());
        let (_, expr) = result.unwrap();
        match expr {
            Expression::Call(name, args) => {
                assert_eq!(name, "Has");
                assert_eq!(args.len(), 2);
                // First argument should be field access
                match &args[0] {
                    Expression::FieldAccess(base, field) => {
                        match &**base {
                            Expression::Identifier(identifier) => assert_eq!(identifier, "frontW"),
                            _ => panic!("Expected Identifier as base of field access"),
                        }
                        assert_eq!(field, "tag");
                    }
                    _ => panic!("Expected FieldAccess as first argument to Has"),
                }
                // Second argument should be string literal
                match &args[1] {
                    Expression::Literal(Literal::String(s)) => {
                        assert_eq!(s, "front");
                    }
                    _ => panic!("Expected String literal as second argument to Has"),
                }
            }
            _ => panic!("Expected Call expression for Has"),
        }
    }

    #[test]
    fn test_parse_method_call_on_expression() {
        // Test parsing method calls - currently the parser doesn't support method calls
        // on field access expressions, so this parses as a field access instead
        let input = "solution.colors.firstTrue()";
        let result = parse_expression(input);
        assert!(result.is_ok());
        let (remaining, expr) = result.unwrap();
        // The parser currently parses this as field access to "firstTrue" and leaves "()" unparsed
        match expr {
            Expression::FieldAccess(base, field) => {
                assert_eq!(field, "firstTrue");
                match *base {
                    Expression::FieldAccess(inner_base, inner_field) => {
                        assert_eq!(inner_field, "colors");
                        match *inner_base {
                            Expression::Identifier(name) => assert_eq!(name, "solution"),
                            _ => panic!("Expected Identifier at base"),
                        }
                    }
                    _ => panic!("Expected nested FieldAccess"),
                }
                // The parentheses should remain unparsed
                assert_eq!(remaining.trim(), "()");
            }
            _ => panic!("Expected FieldAccess expression"),
        }
    }

    #[test]
    fn test_type_checker_with_complete_example() {
        let mut checker = TypeChecker::new();

        // Create a simple program
        let program = Program {
            statements: vec![
                Statement::TypeDecl(TypeDecl {
                    name: "Color".to_string(),
                    base_type: BaseType::Bool,
                    fields: vec![Field {
                        name: "tag".to_string(),
                        field_type: Type::Primitive(PrimitiveType::String),
                        is_attribute: true,
                        default_value: None,
                    }],
                }),
                Statement::Assignment(Assignment {
                    name: "red".to_string(),
                    expr: Expression::Constructor(
                        "Color".to_string(),
                        vec![],
                        vec![FieldAssignment {
                            name: "tag".to_string(),
                            value: Expression::Literal(Literal::String("red".to_string())),
                        }],
                    ),
                }),
                Statement::Assignment(Assignment {
                    name: "z".to_string(),
                    expr: Expression::Range(-2, 1),
                }),
            ],
        };

        // Type checking should pass for this well-formed program
        assert!(checker.check_program(&program).is_ok());
    }
}
