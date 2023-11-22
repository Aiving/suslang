use nom::branch::alt;
use nom::combinator::map;
use nom::combinator::opt;
use nom::sequence::tuple;
use nom::Parser as _;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Token;
use crate::lexer::token::Tokens;

use crate::parser::ast::Statement;
use crate::parser::ast::VariableStatement;
use crate::parser::tags::const_tag;
use crate::parser::tags::eq_tag;
use crate::parser::tags::let_tag;
use crate::parser::tags::semi_tag;

use super::expression::parse_expression;
use super::ident::parse_ident;
use super::ty::parse_type;

pub fn parse_variable(input: Tokens) -> TokenResult<Positioned<Statement>> {
    map(
        tuple((
            alt((const_tag, let_tag)),
            parse_ident,
            opt(parse_type),
            eq_tag,
            parse_expression,
            semi_tag,
        )),
        |(definer, name, ty, _, expression, end)| {
            definer
                .span
                .between(end.span)
                .wrap(Statement::Variable(VariableStatement {
                    name,
                    nullable: ty.as_ref().is_some_and(|ty| ty.value.0),
                    ty: ty.map(|ty| ty.value.1),
                    changeable: definer.value.tok[0]
                        .span
                        .wrap(definer.value.tok[0].value == Token::Let),
                    value: Box::new(expression),
                }))
        },
    )
    .parse(input)
}
