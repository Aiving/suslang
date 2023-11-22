use nom::branch::alt;
use nom::combinator::map;
use nom::combinator::opt;
use nom::multi::separated_list0;
use nom::multi::separated_list1;
use nom::sequence::tuple;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::ast::FunctionCallExpression;
use crate::parser::ast::LinearFunctionExpression;
use crate::parser::tags::comma_tag;
use crate::parser::tags::fat_arrow_tag;
use crate::parser::tags::paren_close_tag;
use crate::parser::tags::paren_open_tag;

use super::expression::parse_expression;
use super::ident::parse_ident;
use super::parse_code_block;

pub fn parse_call_expression(
    input: Tokens,
    fn_handle: Positioned<Expression>,
) -> TokenResult<Positioned<Expression>> {
    map(
        tuple((
            paren_open_tag,
            separated_list0(
                comma_tag,
                alt((
                    map(
                        tuple((
                            paren_open_tag,
                            separated_list1(comma_tag, parse_ident),
                            paren_close_tag,
                            fat_arrow_tag,
                            parse_code_block,
                        )),
                        |(start, args, args_end, _, end)| {
                            start
                                .span
                                .between(end.span)
                                .wrap(Expression::LinearFunction(LinearFunctionExpression {
                                    arguments: start.span.between(args_end.span).wrap(args),
                                    body: end,
                                }))
                        },
                    ),
                    parse_expression,
                )),
            ),
            paren_close_tag,
            opt(parse_code_block),
        )),
        |(start, arguments, pre_end, end)| {
            end.as_ref()
                .map(|end| start.span.between(end.span))
                .unwrap_or(start.span.between(pre_end.span))
                .wrap(Expression::FunctionCall(FunctionCallExpression {
                    function: Box::new(fn_handle.clone()),
                    arguments,
                    lambda: end,
                }))
        },
    )(input)
}
