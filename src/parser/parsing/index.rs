use nom::combinator::map;
use nom::combinator::opt;
use nom::sequence::tuple;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::ast::IndexExpression;
use crate::parser::tags::bracket_close_tag;
use crate::parser::tags::bracket_open_tag;
use crate::parser::tags::dot_tag;

use super::expression::parse_expression;
use super::ident::parse_ident_expression;

pub fn parse_index_expression(
    input: Tokens,
    arr: Positioned<Expression>,
) -> TokenResult<Positioned<Expression>> {
    map(
        tuple((
            opt(tuple((
                bracket_open_tag,
                parse_expression,
                bracket_close_tag,
            ))),
            opt(tuple((dot_tag, parse_ident_expression))),
        )),
        |(by_bracket, by_dot)| {
            if let Some((start, index, end)) = by_bracket {
                start
                    .span
                    .between(end.span)
                    .wrap(Expression::Index(IndexExpression {
                        target: Box::new(arr.clone()),
                        index: Box::new(index),
                    }))
            } else if let Some((start, index)) = by_dot {
                start
                    .span
                    .between(index.span)
                    .wrap(Expression::Index(IndexExpression {
                        target: Box::new(arr.clone()),
                        index: Box::new(index),
                    }))
            } else {
                unreachable!()
            }
        },
    )(input)
}
