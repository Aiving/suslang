use nom::multi::separated_list0;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::tags::bracket_close_tag;
use crate::parser::tags::bracket_open_tag;
use crate::parser::tags::comma_tag;

use super::expression::parse_expression;

pub fn parse_array(i: Tokens) -> TokenResult<Positioned<Expression>> {
    let (input, start) = bracket_open_tag(i)?;
    let (input, elements) = separated_list0(comma_tag, parse_expression)(input)?;
    let (input, end) = bracket_close_tag(input)?;

    Ok((
        input,
        start
            .span
            .between(end.span)
            .wrap(Expression::Array(elements)),
    ))
}
