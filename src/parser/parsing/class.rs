use nom::branch::alt;
use nom::combinator::map;
use nom::combinator::opt;
use nom::multi::many0;
use nom::multi::separated_list1;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::Parser as _;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Tokens;

use crate::lexer::token::ValueType;
use crate::parser::ast::ClassField;
use crate::parser::ast::ClassFunctionExpression;
use crate::parser::ast::ClassStatement;
use crate::parser::ast::ConstructorExpression;
use crate::parser::ast::Expression;
use crate::parser::ast::Ident;
use crate::parser::ast::Statement;
use crate::parser::tags::brace_close_tag;
use crate::parser::tags::brace_open_tag;
use crate::parser::tags::class_tag;
use crate::parser::tags::comma_tag;
use crate::parser::tags::constructor_tag;
use crate::parser::tags::eq_tag;
use crate::parser::tags::fat_arrow_tag;
use crate::parser::tags::gt_tag;
use crate::parser::tags::implements_tag;
use crate::parser::tags::lt_tag;
use crate::parser::tags::semi_tag;

use super::access_flags::parse_access_flags;
use super::arguments::parse_arguments;
use super::expression::parse_expression;
use super::ident::parse_ident;
use super::parse_code_block;
use super::ty::parse_type;
use super::ty::parse_value_type;

pub fn parse_constructor(input: Tokens) -> TokenResult<Positioned<Expression>> {
    map(
        tuple((
            parse_access_flags,
            constructor_tag,
            parse_arguments,
            alt((
                parse_code_block,
                map(
                    tuple((fat_arrow_tag, parse_expression, semi_tag)),
                    |(start, expression, end)| {
                        start.span.between(end.span).wrap(vec![expression
                            .span
                            .wrap(Statement::Expression(expression.value))])
                    },
                ),
            )),
        )),
        |(access_flags, name, arguments, body)| {
            access_flags
                .first()
                .map(|first| first.span.between(body.span))
                .unwrap_or(name.span.between(body.span))
                .wrap(Expression::Constructor(ConstructorExpression {
                    access_flags,
                    name: name.span.wrap(Ident("constructor".into())),
                    arguments,
                    body,
                }))
        },
    )
    .parse(input)
}

pub fn parse_method(input: Tokens) -> TokenResult<Positioned<Expression>> {
    map(
        tuple((
            parse_access_flags,
            parse_ident,
            parse_arguments,
            parse_type,
            alt((
                parse_code_block,
                map(
                    tuple((fat_arrow_tag, parse_expression, semi_tag)),
                    |(start, expression, end)| {
                        start.span.between(end.span).wrap(vec![expression
                            .span
                            .wrap(Statement::Expression(expression.value))])
                    },
                ),
            )),
        )),
        |(access_flags, name, arguments, ty, body)| {
            access_flags
                .first()
                .map(|first| first.span.between(body.span))
                .unwrap_or(name.span.between(body.span))
                .wrap(Expression::ClassFunction(ClassFunctionExpression {
                    access_flags,
                    name,
                    return_type: ty.value.1,
                    arguments,
                    body,
                }))
        },
    )
    .parse(input)
}

pub fn parse_property(input: Tokens) -> TokenResult<Positioned<ClassField>> {
    map(
        tuple((
            parse_access_flags,
            parse_ident,
            opt(parse_type),
            opt(preceded(eq_tag, parse_expression)),
            semi_tag,
        )),
        |(access_flags, name, ty, value, end)| {
            access_flags
                .first()
                .map(|first| first.span.between(end.span))
                .unwrap_or(name.span.between(end.span))
                .wrap(ClassField {
                    access_flags,
                    name,
                    nullable: ty.as_ref().is_some_and(|ty| ty.value.0),
                    ty: ty.map(|ty| ty.value.1),
                    default_value: value,
                })
        },
    )
    .parse(input)
}

pub fn parse_generic_types(input: Tokens) -> TokenResult<Vec<Positioned<ValueType>>> {
    map(
        tuple((lt_tag, separated_list1(comma_tag, parse_value_type), gt_tag)),
        |(_, generic_types, _)| generic_types,
    )
    .parse(input)
}

pub fn parse_generics(input: Tokens) -> TokenResult<Vec<Positioned<Ident>>> {
    map(
        tuple((lt_tag, separated_list1(comma_tag, parse_ident), gt_tag)),
        |(_, generics, _)| generics,
    )
    .parse(input)
}

pub fn parse_class(input: Tokens) -> TokenResult<Positioned<Statement>> {
    let (input, start) = class_tag(input)?;
    let (input, name) = parse_ident(input)?;
    let (input, generics) = opt(parse_generics)(input)?;
    let (input, implements) = opt(preceded(
        implements_tag,
        separated_list1(comma_tag, parse_ident),
    ))(input)?;
    let (input, _) = brace_open_tag(input)?;
    let (input, fields) = many0(parse_property)(input)?;
    let (input, _methods) = many0(alt((parse_method, parse_constructor)))(input)?;
    let (input, end) = brace_close_tag(input)?;

    let (mut constructor, mut methods) = (vec![], vec![]);

    for method in _methods {
        if matches!(method.value, Expression::Constructor { .. }) {
            constructor.push(method)
        } else if matches!(method.value, Expression::ClassFunction { .. }) {
            methods.push(method)
        }
    }

    Ok((
        input,
        start
            .span
            .between(end.span)
            .wrap(Statement::Class(ClassStatement {
                name,
                generics: generics.unwrap_or(vec![]),
                properties: fields,
                implements: implements.unwrap_or_default(),
                constructor,
                methods,
            })),
    ))
}
