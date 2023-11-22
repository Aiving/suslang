pub mod access_flags;
pub mod arguments;
pub mod array;
pub mod assignment;
pub mod class;
pub mod expression;
pub mod function;
pub mod function_call;
pub mod ident;
pub mod if_else;
pub mod index;
pub mod infix;
pub mod interface;
pub mod literal;
pub mod object;
pub mod statement;
pub mod ty;
pub mod variable;

use nom::branch::alt;
use nom::combinator::map;
use nom::combinator::opt;
use nom::multi::many0;
use nom::multi::separated_list0;
use nom::sequence::delimited;
use nom::sequence::tuple;
use nom::Parser as NomParser;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::ast::Program;
use crate::parser::ast::Statement;
use crate::parser::tags::brace_close_tag;
use crate::parser::tags::brace_open_tag;
use crate::parser::tags::null_tag;
use crate::parser::tags::paren_close_tag;
use crate::parser::tags::paren_open_tag;
use crate::parser::tags::return_tag;
use crate::parser::tags::semi_tag;

use self::array::parse_array;
use self::assignment::parse_assignment;
use self::class::parse_generic_types;
use self::expression::parse_expression;
use self::function::parse_function;
use self::ident::parse_ident;
use self::ident::parse_ident_expression;
use self::if_else::parse_if;
use self::literal::parse_literal_expression;
use self::object::parse_object;
use self::statement::parse_statement;

use super::ast::ClassCreationExpression;
use super::tags::comma_tag;
use super::tags::new_tag;
use super::tags::this_tag;

pub fn parse_atom(input: Tokens) -> TokenResult<Positioned<Expression>> {
    alt((
        parse_assignment,
        parse_new,
        parse_null,
        parse_this,
        parse_literal_expression,
        parse_ident_expression,
        parse_paren,
        parse_array,
        parse_object,
        parse_if,
        parse_function,
    ))
    .parse(input)
}

pub fn parse_call_arguments(input: Tokens) -> TokenResult<Positioned<Vec<Positioned<Expression>>>> {
    let (input, start) = paren_open_tag(input)?;
    let (input, arguments) = separated_list0(comma_tag, parse_expression)(input)?;
    let (input, end) = paren_close_tag(input)?;

    Ok((input, start.span.between(end.span).wrap(arguments)))
}

pub fn parse_new(input: Tokens) -> TokenResult<Positioned<Expression>> {
    map(
        tuple((
            new_tag,
            parse_ident,
            opt(parse_generic_types),
            parse_call_arguments,
        )),
        |(start, class, generics, arguments)| {
            start
                .span
                .between(arguments.span)
                .wrap(Expression::ClassCreation(ClassCreationExpression {
                    class,
                    generics: generics.unwrap_or(vec![]),
                    arguments,
                }))
        },
    )
    .parse(input)
}

pub fn parse_this(input: Tokens) -> TokenResult<Positioned<Expression>> {
    map(this_tag, |s| s.span.wrap(Expression::This)).parse(input)
}

pub fn parse_null(input: Tokens) -> TokenResult<Positioned<Expression>> {
    map(null_tag, |s| s.span.wrap(Expression::Null)).parse(input)
}

pub fn parse_paren(input: Tokens) -> TokenResult<Positioned<Expression>> {
    delimited(paren_open_tag, parse_expression, paren_close_tag).parse(input)
}

pub fn parse_code_block(i: Tokens) -> TokenResult<Positioned<Program>> {
    let (input, start) = brace_open_tag(i)?;
    let (input, value) = many0(parse_statement)(input)?;
    let (input, end) = brace_close_tag(input)?;

    Ok((input, start.span.between(end.span).wrap(value)))
}

pub fn parse_return(input: Tokens) -> TokenResult<Positioned<Statement>> {
    map(
        tuple((return_tag, parse_expression, semi_tag)),
        |(start, experssion, end)| {
            start
                .span
                .between(end.span)
                .wrap(Statement::Return(experssion.value))
        },
    )
    .parse(input)
}
