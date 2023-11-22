pub mod ast;
pub mod parsing;
pub mod tags;

use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::tuple;
use nom::Parser as _;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Token;
use crate::lexer::token::Tokens;

use self::ast::Program;
use self::parsing::statement::parse_statement;
use self::tags::eof_tag;

fn parse_program(input: Tokens) -> TokenResult<Positioned<Program>> {
    map(
        tuple((many0(parse_statement), eof_tag)),
        |(program, end)| input.tok[0].span.between(end.span).wrap(program),
    )
    .parse(input)
}

pub struct Parser;

impl Parser {
    pub fn parse_tokens(tokens: &[Positioned<Token>]) -> TokenResult<Positioned<Program>> {
        parse_program(Tokens::new(tokens))
    }
}
