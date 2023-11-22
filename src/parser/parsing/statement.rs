use nom::branch::alt;
use nom::combinator::map;
use nom::combinator::opt;
use nom::sequence::tuple;
use nom::Parser as _;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::Statement;
use crate::parser::tags::semi_tag;

use super::class::parse_class;
use super::expression::parse_expression;
use super::interface::parse_interface;
use super::parse_return;
use super::variable::parse_variable;

pub fn parse_expression_statement(input: Tokens) -> TokenResult<Positioned<Statement>> {
    map(
        tuple((parse_expression, opt(semi_tag))),
        |(expression, end)| {
            expression
                .span
                .between(
                    end.as_ref()
                        .map(|end| end.span)
                        .unwrap_or(expression.span),
                )
                .wrap(Statement::Expression(expression.value))
        },
    )
    .parse(input)
}

pub fn parse_statement(input: Tokens) -> TokenResult<Positioned<Statement>> {
    alt((
        parse_variable,
        parse_return,
        parse_class,
        parse_interface,
        parse_expression_statement,
    ))
    .parse(input)
}
