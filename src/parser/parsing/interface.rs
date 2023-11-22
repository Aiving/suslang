use nom::combinator::map;
use nom::multi::separated_list0;
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::Parser as _;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::parser::ast::InterfaceStatement;
use crate::parser::ast::InterfaceField;
use crate::parser::ast::Statement;
use crate::parser::tags::brace_close_tag;
use crate::parser::tags::brace_open_tag;
use crate::parser::tags::interface_tag;
use crate::parser::tags::semi_tag;

use super::ident::parse_ident;
use super::ty::parse_type;

pub fn parse_interface(input: Tokens) -> TokenResult<Positioned<Statement>> {
    map(
        tuple((
            interface_tag,
            parse_ident,
            brace_open_tag,
            terminated(separated_list0(semi_tag, parse_interface_field), semi_tag),
            brace_close_tag,
        )),
        |(start, name, _, fields, end)| {
            start
                .span
                .between(end.span)
                .wrap(Statement::Interface(InterfaceStatement { name, fields }))
        },
    )
    .parse(input)
}

pub fn parse_interface_field(input: Tokens) -> TokenResult<Positioned<InterfaceField>> {
    map(tuple((parse_ident, parse_type)), |(key, ty)| {
        key.span.between(ty.span).wrap(InterfaceField {
            key,
            nullable: ty.value.0,
            ty: ty.value.1,
        })
    })
    .parse(input)
}
