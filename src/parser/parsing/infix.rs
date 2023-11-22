use nom::bytes::complete::take;
use nom::error::Error;
use nom::error::ErrorKind;
use nom::Err;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Token;
use crate::lexer::token::Tokens;

use crate::parser::ast::Expression;
use crate::parser::ast::Infix;
use crate::parser::ast::InfixExpression;
use crate::parser::ast::Precedence;

use super::expression::parse_pratt_expr;

pub type PrecedencedInfix = (Precedence, Option<Positioned<Infix>>);

pub trait FromRef<T> {
    fn from_ref(from: &T) -> Self;
}

impl FromRef<Positioned<Token>> for PrecedencedInfix {
    fn from_ref(token: &Positioned<Token>) -> Self {
        match token.value {
            Token::EqEq => (Precedence::PEquals, Some(token.span.wrap(Infix::Equal))),
            Token::Ne => (Precedence::PEquals, Some(token.span.wrap(Infix::NotEqual))),
            Token::Le => (
                Precedence::PLessGreater,
                Some(token.span.wrap(Infix::LessThanEqual)),
            ),
            Token::Ge => (
                Precedence::PLessGreater,
                Some(token.span.wrap(Infix::GreaterThanEqual)),
            ),
            Token::Lt => (
                Precedence::PLessGreater,
                Some(token.span.wrap(Infix::LessThan)),
            ),
            Token::Gt => (
                Precedence::PLessGreater,
                Some(token.span.wrap(Infix::GreaterThan)),
            ),
            Token::Plus => (Precedence::PSum, Some(token.span.wrap(Infix::Plus))),
            Token::Minus => (Precedence::PSum, Some(token.span.wrap(Infix::Minus))),
            Token::Star => (Precedence::PProduct, Some(token.span.wrap(Infix::Multiply))),
            Token::Slash => (Precedence::PProduct, Some(token.span.wrap(Infix::Divide))),
            Token::ParenOpen => (Precedence::PCall, None),
            Token::BracketOpen => (Precedence::PIndex, None),
            Token::Dot => (Precedence::PIndex, None),
            _ => (Precedence::PLowest, None),
        }
    }
}

pub fn parse_infix_expression(
    input: Tokens,
    left: Positioned<Expression>,
) -> TokenResult<Positioned<Expression>> {
    let (input, tokens) = take(1usize)(input)?;

    if tokens.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        let (precedence, maybe_op) = PrecedencedInfix::from_ref(&tokens.tok[0]);

        match maybe_op {
            None => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
            Some(operation) => {
                let (input, right) = parse_pratt_expr(input, precedence)?;

                Ok((
                    input,
                    left.span
                        .between(right.span)
                        .wrap(Expression::Infix(InfixExpression {
                            operation,
                            left: Box::new(left),
                            right: Box::new(right),
                        })),
                ))
            }
        }
    }
}
