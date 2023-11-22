use nom::branch::alt;
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::tuple;
use nom::Parser as _;

use crate::globals::TokenResult;
use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;
use crate::parser::ast::AssignmentExpression;
use crate::parser::ast::Expression;
use crate::parser::ast::Ident;
use crate::parser::tags::dot_tag;
use crate::parser::tags::eq_tag;
use crate::parser::tags::semi_tag;

use super::expression::parse_expression;
use super::ident::parse_ident;
use super::parse_this;

pub fn parse_assignment(input: Tokens) -> TokenResult<Positioned<Expression>> {
    map(
        tuple((
            separated_list1(
                dot_tag,
                alt((
                    parse_ident,
                    map(parse_this, |this| this.span.wrap(Ident::new("this"))),
                )),
            ),
            eq_tag,
            parse_expression,
            semi_tag,
        )),
        |(variable, _, value, end)| {
            variable
                .first()
                .unwrap()
                .span
                .between(end.span)
                .wrap(Expression::Assignment(AssignmentExpression {
                    variable: variable
                        .first()
                        .unwrap()
                        .span
                        .between(variable.last().unwrap().span)
                        .wrap(variable),
                    value: Box::new(value),
                }))
        },
    )
    .parse(input)
}
