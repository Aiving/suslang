use nom::branch::alt;
use nom::combinator::map;
use nom::combinator::opt;
use nom::multi::separated_list0;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::Parser as _;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::ast::FunctionArgument;
use crate::parser::tags::comma_tag;
use crate::parser::tags::eq_tag;
use crate::parser::tags::paren_close_tag;
use crate::parser::tags::paren_open_tag;

use super::expression::parse_expression;
use super::ident::parse_ident;
use super::parse_code_block;
use super::ty::parse_type;

pub fn parse_arguments(
    input: Tokens,
) -> TokenResult<Positioned<Vec<Positioned<FunctionArgument>>>> {
    map(
        tuple((
            paren_open_tag,
            separated_list0(
                comma_tag,
                tuple((
                    parse_ident,
                    parse_type,
                    opt(preceded(
                        eq_tag,
                        alt((
                            parse_expression,
                            map(parse_code_block, |s| {
                                s.span.wrap(Expression::Block(s.value))
                            }),
                        )),
                    )),
                )),
            ),
            paren_close_tag,
        )),
        |(start, v, end)| {
            start.span.between(end.span).wrap(
                v.into_iter()
                    .map(|(name, ty, default_value)| {
                        name.span
                            .between(default_value.as_ref().map(|v| v.span).unwrap_or(ty.span))
                            .wrap(FunctionArgument {
                                name,
                                ty: ty.value.1,
                                nullable: ty.value.0,
                                default_value,
                            })
                    })
                    .collect(),
            )
        },
    )
    .parse(input)
}
