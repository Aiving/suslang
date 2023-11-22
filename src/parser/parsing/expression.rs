use nom::bytes::complete::take;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::ast::Precedence;

use crate::parser::parsing::infix::FromRef;

use super::function_call::parse_call_expression;
use super::index::parse_index_expression;
use super::infix::parse_infix_expression;
use super::infix::PrecedencedInfix;
use super::parse_atom;

pub fn parse_pratt_expr(
    input: Tokens,
    precedence: Precedence,
) -> TokenResult<Positioned<Expression>> {
    let (input, left) = parse_atom(input)?;

    go_parse_pratt_expr(input, precedence, left)
}

pub fn go_parse_pratt_expr(
    input: Tokens,
    precedence: Precedence,
    left: Positioned<Expression>,
) -> TokenResult<Positioned<Expression>> {
    let (second_input, tokens) = take(1usize)(input)?;

    if tokens.tok.is_empty() {
        Ok((second_input, left))
    } else {
        let p = PrecedencedInfix::from_ref(&tokens.tok[0]);

        match p {
            (Precedence::PCall, _) if precedence < Precedence::PCall => {
                let (input, left) = parse_call_expression(input, left)?;

                go_parse_pratt_expr(input, precedence, left)
            }
            (Precedence::PIndex, _) if precedence < Precedence::PIndex => {
                let (input, left) = parse_index_expression(input, left)?;

                go_parse_pratt_expr(input, precedence, left)
            }
            (ref peek_precedence, _) if precedence < *peek_precedence => {
                let (input, left) = parse_infix_expression(input, left)?;

                go_parse_pratt_expr(input, precedence, left)
            }
            _ => Ok((input, left)),
        }
    }
}

pub fn parse_expression(input: Tokens) -> TokenResult<Positioned<Expression>> {
    parse_pratt_expr(input, Precedence::PLowest)
}
