use nom::bytes::complete::take;
use nom::combinator::map;
use nom::combinator::verify;

use crate::globals::TokenResult;

use crate::lexer::token::Positioned;
use crate::lexer::token::Token;
use crate::lexer::token::Tokens;

macro_rules! token {
    ($func_name:ident, $tag:expr) => {
        pub fn $func_name(tokens: Tokens<'_>) -> TokenResult<Positioned<Tokens<'_>>> {
            verify(map(take(1usize), |s: Tokens| s.tok[0].span.wrap(s)), |t| {
                if t.value.tok[0].value != $tag {
                    println!("Can't verify next tag: {:?} (because real tag is {:?})", $tag, t.value.tok[0].value);
                } else {
                    println!("Next tag was verified: {:?}", $tag);
                }

                t.value.tok[0].value == $tag
            })(tokens)
        }
    };
}

token!(const_tag, Token::Const);
token!(let_tag, Token::Let);
token!(eq_tag, Token::Eq);
token!(semi_tag, Token::Semi);
token!(return_tag, Token::Return);
token!(brace_open_tag, Token::BraceOpen);
token!(brace_close_tag, Token::BraceClose);
token!(paren_open_tag, Token::ParenOpen);
token!(paren_close_tag, Token::ParenClose);
token!(bracket_open_tag, Token::BracketOpen);
token!(bracket_close_tag, Token::BracketClose);
token!(comma_tag, Token::Comma);
token!(colon_tag, Token::Colon);
token!(class_tag, Token::Class);
token!(fat_arrow_tag, Token::FatArrow);
token!(if_tag, Token::If);
token!(else_tag, Token::Else);
token!(question_tag, Token::Question);
token!(function_tag, Token::Function);
token!(interface_tag, Token::Interface);
token!(pound_tag, Token::Pound);
token!(private_tag, Token::Private);
token!(public_tag, Token::Public);
token!(static_tag, Token::Static);
token!(eof_tag, Token::EOF);
token!(null_tag, Token::Null);
token!(implements_tag, Token::Implements);
token!(constructor_tag, Token::Constructor);
token!(dot_tag, Token::Dot);
token!(this_tag, Token::This);
token!(new_tag, Token::New);
token!(lt_tag, Token::Lt);
token!(gt_tag, Token::Gt);

