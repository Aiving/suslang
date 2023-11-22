use nom::bytes::complete::take;
use nom::combinator::map;
use nom::error::Error;
use nom::error::ErrorKind;
use nom::Err;
use nom::Parser as NomParser;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Token;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::ast::Ident;

pub fn parse_ident_expression(input: Tokens) -> TokenResult<Positioned<Expression>> {
    map(parse_ident, |s| s.span.wrap(Expression::Ident(s.value))).parse(input)
}

pub fn parse_ident(input: Tokens) -> TokenResult<Positioned<Ident>> {
    let (i1, t1) = take(1usize)(input)?;

    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match &t1.tok[0].value {
            Token::Ident(name) => Ok((i1, t1.tok[0].span.wrap(Ident(name.clone())))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}
