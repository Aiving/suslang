use nom::branch::alt;
use nom::combinator::map;
use nom::combinator::opt;
use nom::multi::many0;
use nom::multi::separated_list0;
use nom::sequence::tuple;
use nom::Parser as NomParser;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;
use crate::lexer::token::ValueType;

use crate::parser::tags::bracket_close_tag;
use crate::parser::tags::bracket_open_tag;
use crate::parser::tags::colon_tag;
use crate::parser::tags::comma_tag;
use crate::parser::tags::fat_arrow_tag;
use crate::parser::tags::paren_close_tag;
use crate::parser::tags::paren_open_tag;
use crate::parser::tags::question_tag;

use super::ident::parse_ident;

pub fn parse_array_type(input: Tokens) -> TokenResult<(Positioned<Tokens>, Positioned<Tokens>)> {
    tuple((bracket_open_tag, bracket_close_tag)).parse(input)
}

pub fn parse_function_type(input: Tokens) -> TokenResult<Positioned<ValueType>> {
    map(
        tuple((
            paren_open_tag,
            separated_list0(comma_tag, parse_value_type),
            paren_close_tag,
            fat_arrow_tag,
            parse_value_type,
        )),
        |(start, arguments, _, _, end)| {
            start.span.between(end.span).wrap(ValueType::Function(
                arguments.into_iter().map(|arg| arg.value).collect(),
                Box::new(end.value),
            ))
        },
    )
    .parse(input)
}

pub fn parse_value_type(input: Tokens) -> TokenResult<Positioned<ValueType>> {
    let (input, ((_ty, ft), arrays)) = tuple((
        alt((
            map(parse_ident, |i| (Some(i), None)),
            map(parse_function_type, |ft| (None, Some(ft))),
        )),
        many0(parse_array_type),
    ))(input)?;

    let ty = _ty
        .map(|ty| {
            ty.span.wrap(match ty.value.0.as_str() {
                "string" => ValueType::String,
                "byte" => ValueType::Byte,
                "short" => ValueType::Short,
                "int" => ValueType::Int,
                "long" => ValueType::Long,
                "boolean" => ValueType::Boolean,
                "object" => ValueType::Object,
                &_ => ValueType::Custom(ty.value.0),
            })
        })
        .unwrap_or_else(|| ft.unwrap());

    let ty = if !arrays.is_empty() {
        arrays.iter().fold(ty, |ty, _| {
            ty.span.wrap(ValueType::Array(Box::new(ty.value)))
        })
    } else {
        ty
    };

    Ok((input, ty))
}

pub fn parse_type(i: Tokens) -> TokenResult<Positioned<(bool, Positioned<ValueType>)>> {
    let (input, (raw_nullable, start, ty)) =
        tuple((opt(question_tag), colon_tag, parse_value_type))(i)?;
    let (input, arrays) = many0(parse_array_type)(input)?;

    Ok((
        input,
        raw_nullable
            .as_ref()
            .map(|n| n.span)
            .unwrap_or(start.span)
            .between(
                arrays
                    .last()
                    .map(|last| ty.span.between(last.1.span))
                    .unwrap_or(ty.span),
            )
            .wrap((raw_nullable.is_some(), ty)),
    ))
}
