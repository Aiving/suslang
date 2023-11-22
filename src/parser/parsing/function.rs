use nom::combinator::map;
use nom::sequence::tuple;
use nom::Parser;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::ast::FunctionExpression;
use crate::parser::tags::function_tag;

use super::access_flags::parse_access_flags;
use super::arguments::parse_arguments;
use super::ident::parse_ident;
use super::parse_code_block;
use super::ty::parse_type;

pub fn parse_function(input: Tokens) -> TokenResult<Positioned<Expression>> {
    map(
        tuple((
            parse_access_flags,
            function_tag,
            parse_ident,
            parse_arguments,
            parse_type,
            parse_code_block,
        )),
        |(access_flags, start, name, arguments, ty, body)| {
            access_flags
                .first()
                .map(|first| first.span.between(body.span))
                .unwrap_or(start.span.between(body.span))
                .wrap(Expression::Function(FunctionExpression {
                    access_flags,
                    name,
                    return_type: ty.value.1,
                    arguments,
                    body,
                }))
        },
    )
    .parse(input)
}
