use nom::combinator::map;
use nom::multi::separated_list0;
use nom::sequence::tuple;
use nom::Parser as _;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::ast::Literal;
use crate::parser::tags::brace_close_tag;
use crate::parser::tags::brace_open_tag;
use crate::parser::tags::colon_tag;
use crate::parser::tags::comma_tag;
use crate::parser::tags::pound_tag;

use super::expression::parse_expression;
use super::literal::parse_literal;

pub fn parse_object(i: Tokens) -> TokenResult<Positioned<Expression>> {
    let (input, start) = pound_tag(i)?;
    let (input, _) = brace_open_tag(input)?;
    let (input, properties) = separated_list0(comma_tag, parse_object_property)(input)?;
    let (input, end) = brace_close_tag(input)?;

    Ok((
        input,
        start
            .span
            .between(end.span)
            .wrap(Expression::Object(properties)),
    ))
}

pub fn parse_object_property(
    input: Tokens,
) -> TokenResult<Positioned<(Positioned<Literal>, Positioned<Expression>)>> {
    map(
        tuple((parse_literal, colon_tag, parse_expression)),
        |(name, _, value)| name.span.between(value.span).wrap((name, value)),
    )
    .parse(input)
}
