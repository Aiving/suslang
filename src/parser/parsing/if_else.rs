use nom::branch::alt;
use nom::combinator::map;
use nom::combinator::opt;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::Parser as _;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::ast::IfElseConditionExpression;
use crate::parser::ast::Program;
use crate::parser::tags::else_tag;
use crate::parser::tags::if_tag;

use super::expression::parse_expression;
use super::parse_code_block;

pub fn parse_if(input: Tokens) -> TokenResult<Positioned<Expression>> {
    map(
        tuple((
            if_tag,
            alt((
                parse_expression,
                map(parse_code_block, |s| {
                    s.span.wrap(Expression::Block(s.value))
                }),
            )),
            parse_code_block,
            parse_else,
        )),
        |(start, condition, then, otherwise)| {
            start
                .span
                .between(
                    otherwise
                        .as_ref()
                        .map(|otherwise| otherwise.span)
                        .unwrap_or(then.span),
                )
                .wrap(Expression::IfCondition(IfElseConditionExpression {
                    condition: Box::new(condition),
                    then,
                    otherwise,
                }))
        },
    )
    .parse(input)
}

pub fn parse_else(input: Tokens) -> TokenResult<Option<Positioned<Program>>> {
    opt(preceded(else_tag, parse_code_block)).parse(input)
}
