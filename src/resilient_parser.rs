use crate::*;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1, take_while, take_until},
    character::complete::{char, digit1, alpha1, alphanumeric1, line_ending, not_line_ending},
    combinator::{opt, map, recognize, value, cut},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, preceded, tuple, pair},
    IResult, Err as NomErr,
    error::{ErrorKind, ParseError as NomParseError}
};

// Enhanced error recovery parser for LSP use
// This parser tries to recover from errors and continue parsing

#[derive(Debug, Clone)]
pub struct ResilientParseError {
    pub message: String,
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone)]
pub struct ParseResult {
    pub program: Program,
    pub errors: Vec<ResilientParseError>,
}

impl ParseResult {
    pub fn new() -> Self {
        Self {
            program: Program { statements: Vec::new() },
            errors: Vec::new(),
        }
    }
}

fn calculate_position(original_input: &str, current_input: &str) -> (u32, u32) {
    let consumed_bytes = original_input.len() - current_input.len();
    let consumed_text = &original_input[..consumed_bytes];
    
    let line = consumed_text.matches('\n').count() as u32;
    let column = if let Some(last_newline) = consumed_text.rfind('\n') {
        (consumed_text.len() - last_newline - 1) as u32
    } else {
        consumed_text.len() as u32
    };
    
    (line, column)
}

pub fn parse_program_resilient(input: &str) -> ParseResult {
    let mut result = ParseResult::new();
    let original_input = input;
    let mut remaining = input;
    
    // Skip initial whitespace and comments
    if let Ok((new_input, _)) = multispace_and_comments(remaining) {
        remaining = new_input;
    }
    
    // Parse statements with error recovery
    while !remaining.trim().is_empty() {
        match parse_statement_resilient(remaining) {
            Ok((new_input, statement)) => {
                result.program.statements.push(statement);
                remaining = new_input;
                
                // Skip whitespace and comments after successful parse
                if let Ok((new_input, _)) = multispace_and_comments(remaining) {
                    remaining = new_input;
                }
            }
            Err(error) => {
                let (line, column) = calculate_position(original_input, remaining);
                result.errors.push(ResilientParseError {
                    message: format!("Parse error: {}", error),
                    line,
                    column,
                });
                
                // Try to recover by skipping to next statement
                remaining = skip_to_next_statement(remaining);
                
                // If we can't make progress, break to avoid infinite loop
                if remaining.trim().is_empty() {
                    break;
                }
            }
        }
    }
    
    result
}

fn parse_statement_resilient(input: &str) -> IResult<&str, Statement> {
    // Try each statement type with error recovery
    alt((
        map(type_decl_resilient, Statement::TypeDecl),
        map(enum_decl_resilient, Statement::EnumDecl),
        map(instance_decl_resilient, Statement::InstanceDecl),
        map(solve_call_resilient, Statement::SolveCall),
        map(assignment_resilient, Statement::Assignment),
    ))(input)
}

fn type_decl_resilient(input: &str) -> IResult<&str, TypeDecl> {
    let (input, _) = ws(tag("type"))(input)?;
    let (input, name) = ws(type_name)(input)?;
    let (input, _) = ws(char(':'))(input)?;
    let (input, base_type) = ws(base_type)(input)?;
    
    // Try to parse fields, but don't fail if field parsing fails
    let (input, fields) = match opt(delimited(
        ws(char('{')),
        field_decl_list_resilient,
        ws(char('}'))
    ))(input) {
        Ok((input, fields)) => (input, fields.unwrap_or_default()),
        Err(_) => {
            // If field parsing fails, try to find the closing brace and continue
            let (input, _) = skip_to_closing_brace(input)?;
            (input, Vec::new())
        }
    };
    
    Ok((input, TypeDecl {
        name: name.to_string(),
        base_type,
        fields,
    }))
}

fn enum_decl_resilient(input: &str) -> IResult<&str, EnumDecl> {
    let (input, _) = ws(tag("enum"))(input)?;
    let (input, name) = ws(type_name)(input)?;
    let (input, _) = ws(char('{'))(input)?;
    let (input, variants) = enum_variant_list_resilient(input)?;
    let (input, _) = ws(char('}'))(input)?;
    
    Ok((input, EnumDecl {
        name: name.to_string(),
        variants,
    }))
}

fn instance_decl_resilient(input: &str) -> IResult<&str, InstanceDecl> {
    let (input, type_name) = ws(type_name)(input)?;
    let (input, instance_name) = ws(identifier)(input)?;
    let (input, _) = ws(char('{'))(input)?;
    let (input, fields) = field_assign_list_resilient(input)?;
    let (input, _) = ws(char('}'))(input)?;
    
    Ok((input, InstanceDecl {
        type_name: type_name.to_string(),
        instance_name: instance_name.to_string(),
        fields,
    }))
}

fn solve_call_resilient(input: &str) -> IResult<&str, SolveCall> {
    // Use the original solve_call parser for now
    solve_call(input)
}

fn assignment_resilient(input: &str) -> IResult<&str, Assignment> {
    let (input, name) = ws(identifier)(input)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, value) = ws(expr_resilient)(input)?;
    
    Ok((input, Assignment {
        name: name.to_string(),
        value,
    }))
}

fn field_decl_list_resilient(input: &str) -> IResult<&str, Vec<FieldDecl>> {
    let mut fields = Vec::new();
    let mut remaining = input;
    
    // Skip initial whitespace
    if let Ok((new_input, _)) = multispace_and_comments(remaining) {
        remaining = new_input;
    }
    
    // Parse fields with error recovery
    while !remaining.is_empty() && !remaining.starts_with('}') {
        match ws(field_decl)(remaining) {
            Ok((new_input, field)) => {
                fields.push(field);
                remaining = new_input;
                
                // Try to consume optional comma
                if let Ok((new_input, _)) = opt(ws(char(',')))(remaining) {
                    remaining = new_input;
                }
            }
            Err(_) => {
                // Skip to next comma or closing brace
                remaining = skip_to_next_field_or_end(remaining);
            }
        }
    }
    
    Ok((remaining, fields))
}

fn field_assign_list_resilient(input: &str) -> IResult<&str, Vec<FieldAssign>> {
    let mut fields = Vec::new();
    let mut remaining = input;
    
    // Skip initial whitespace
    if let Ok((new_input, _)) = multispace_and_comments(remaining) {
        remaining = new_input;
    }
    
    // Parse field assignments with error recovery
    while !remaining.is_empty() && !remaining.starts_with('}') {
        match ws(field_assign_resilient)(remaining) {
            Ok((new_input, field)) => {
                fields.push(field);
                remaining = new_input;
                
                // Try to consume optional comma
                if let Ok((new_input, _)) = opt(ws(char(',')))(remaining) {
                    remaining = new_input;
                }
            }
            Err(_) => {
                // Skip to next comma or closing brace
                remaining = skip_to_next_field_or_end(remaining);
            }
        }
    }
    
    Ok((remaining, fields))
}

fn field_assign_resilient(input: &str) -> IResult<&str, FieldAssign> {
    let (input, name) = ws(identifier)(input)?;
    let (input, _) = ws(char(':'))(input)?;
    let (input, value) = ws(expr_resilient)(input)?;
    
    Ok((input, FieldAssign {
        name: name.to_string(),
        value,
    }))
}

fn enum_variant_list_resilient(input: &str) -> IResult<&str, Vec<EnumVariant>> {
    let mut variants = Vec::new();
    let mut remaining = input;
    
    // Skip initial whitespace
    if let Ok((new_input, _)) = multispace_and_comments(remaining) {
        remaining = new_input;
    }
    
    // Parse variants with error recovery
    while !remaining.is_empty() && !remaining.starts_with('}') {
        match ws(enum_variant)(remaining) {
            Ok((new_input, variant)) => {
                variants.push(variant);
                remaining = new_input;
                
                // Try to consume optional comma
                if let Ok((new_input, _)) = opt(ws(char(',')))(remaining) {
                    remaining = new_input;
                }
            }
            Err(_) => {
                // Skip to next comma or closing brace
                remaining = skip_to_next_field_or_end(remaining);
            }
        }
    }
    
    Ok((remaining, variants))
}

fn expr_resilient(input: &str) -> IResult<&str, Expr> {
    // Try the regular expression parser first
    match expr(input) {
        Ok(result) => Ok(result),
        Err(_) => {
            // If regular parsing fails, try to create a fallback expression
            // For now, just try to parse an identifier or literal
            alt((
                map(identifier, |s| Expr::Identifier(s.to_string())),
                map(literal, Expr::Literal),
                // Fallback: create a placeholder expression
                map(take_while1(|c: char| !c.is_whitespace() && c != ',' && c != '}'), 
                    |s: &str| Expr::Identifier(format!("__error__{}", s))),
            ))(input)
        }
    }
}

// Utility functions for error recovery

fn skip_to_next_statement(input: &str) -> &str {
    // Look for keywords that start statements: "type", "enum", identifier followed by {, etc.
    let mut chars = input.chars();
    let mut pos = 0;
    
    while let Some(ch) = chars.next() {
        if ch == '\n' {
            // Try to find a statement start after newline
            let remaining = &input[pos + ch.len_utf8()..];
            if let Ok((_, _)) = multispace_and_comments(remaining) {
                if remaining.trim_start().starts_with("type ") ||
                   remaining.trim_start().starts_with("enum ") ||
                   is_instance_decl_start(remaining.trim_start()) {
                    return remaining.trim_start();
                }
            }
        }
        pos += ch.len_utf8();
    }
    
    "" // Return empty if no next statement found
}

fn skip_to_closing_brace(input: &str) -> IResult<&str, ()> {
    let mut brace_count = 1;
    let mut pos = 0;
    
    for ch in input.chars() {
        match ch {
            '{' => brace_count += 1,
            '}' => {
                brace_count -= 1;
                if brace_count == 0 {
                    return Ok((&input[pos + 1..], ()));
                }
            }
            _ => {}
        }
        pos += ch.len_utf8();
    }
    
    Err(NomErr::Error(nom::error::Error::new(input, ErrorKind::Char)))
}

fn skip_to_next_field_or_end(input: &str) -> &str {
    // Skip to next comma or closing brace
    let mut pos = 0;
    for ch in input.chars() {
        match ch {
            ',' | '}' => return &input[pos..],
            _ => pos += ch.len_utf8(),
        }
    }
    ""
}

fn is_instance_decl_start(input: &str) -> bool {
    // Check if this looks like "TypeName instance_name {"
    if let Ok((remaining, _)) = type_name(input) {
        if let Ok((remaining, _)) = ws(identifier)(remaining) {
            if let Ok((_, _)) = ws(char('{'))(remaining) {
                return true;
            }
        }
    }
    false
}

// Re-export the original parsers we're reusing
use crate::{
    multispace_and_comments, type_name, identifier, base_type, field_decl, solve_call,
    expr, literal, enum_variant, ws
};