use nom::branch::alt;
use nom::combinator::map;
use nom::multi::many0;
use nom::Parser as NomParser;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Token;
use crate::lexer::token::Tokens;

use crate::parser::tags::private_tag;
use crate::parser::tags::public_tag;
use crate::parser::tags::static_tag;

pub fn parse_access_flags(input: Tokens) -> TokenResult<Vec<Positioned<Token>>> {
    map(many0(alt((public_tag, private_tag, static_tag))), |v| {
        v.iter()
            .map(|modifier| modifier.value.tok[0].clone())
            .collect()
    })
    .parse(input)
}
