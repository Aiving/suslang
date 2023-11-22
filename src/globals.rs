use nom::IResult;
use nom_locate::LocatedSpan;

use crate::lexer::token::Tokens;

pub type BytesSpan<'a> = LocatedSpan<&'a [u8]>;

pub type TokenResult<'a, T> = IResult<Tokens<'a>, T>;
pub type ByteResult<'a, T> = IResult<BytesSpan<'a>, T>;
