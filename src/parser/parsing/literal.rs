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
use crate::parser::ast::Literal;

pub fn parse_literal(input: Tokens) -> TokenResult<Positioned<Literal>> {
    let (tokens, token) = take(1usize)(input)?;

    if token.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match &token.tok[0].value {
            Token::StringLiteral(string) => Ok((
                tokens,
                token.tok[0].span.wrap(Literal::String(string.clone())),
            )),
            Token::ByteLiteral(byte) => Ok((tokens, token.tok[0].span.wrap(Literal::Byte(*byte)))),
            Token::ShortLiteral(short) => {
                Ok((tokens, token.tok[0].span.wrap(Literal::Short(*short))))
            }
            Token::IntLiteral(int) => Ok((tokens, token.tok[0].span.wrap(Literal::Int(*int)))),
            Token::LongLiteral(long) => Ok((tokens, token.tok[0].span.wrap(Literal::Long(*long)))),
            Token::FloatLiteral(float) => {
                Ok((tokens, token.tok[0].span.wrap(Literal::Float(*float))))
            }
            Token::BoolLiteral(boolean) => {
                Ok((tokens, token.tok[0].span.wrap(Literal::Bool(*boolean))))
            }
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

pub fn parse_literal_expression(input: Tokens) -> TokenResult<Positioned<Expression>> {
    map(parse_literal, |s| s.span.wrap(Expression::Literal(s.value))).parse(input)
}
