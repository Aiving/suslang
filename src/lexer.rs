use std::str;
use std::str::FromStr;
use std::str::Utf8Error;

use crate::globals::ByteResult;
use crate::globals::BytesSpan;

use self::token::Positioned;
use self::token::Span;
use self::token::Token;

use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::bytes::complete::tag;
use nom::bytes::complete::take;

use nom::character::complete::alpha1;
use nom::character::complete::alphanumeric1;
use nom::character::complete::digit1;
use nom::character::complete::multispace0;

use nom::combinator::map;
use nom::combinator::map_res;
use nom::combinator::recognize;

use nom::multi::many0;

use nom::sequence::delimited;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::AsBytes;

use nom_locate::position;
use owo_colors::OwoColorize;

pub mod token;

macro_rules! syntax {
    ($func_name: ident, $tag_string: literal, $output_token: expr) => {
        fn $func_name(input: BytesSpan) -> ByteResult<Positioned<Token>> {
            let (input, position) = tag($tag_string)(input)?;

            Ok((input, Positioned::new($output_token, position.into())))
        }
    };
}

syntax! {equal_operator, "==", Token::EqEq}
syntax! {arrow_operator, "=>", Token::FatArrow}
syntax! {not_equal_operator, "!=", Token::Ne}
syntax! {assign_operator, "=", Token::Eq}
syntax! {plus_operator, "+", Token::Plus}
syntax! {minus_operator, "-", Token::Minus}
syntax! {multiply_operator, "*", Token::Star}
syntax! {divide_operator, "/", Token::Slash}
syntax! {not_operator, "!", Token::Not}
syntax! {greater_operator_equal, ">=", Token::Gt}
syntax! {lesser_operator_equal, "<=", Token::Le}
syntax! {greater_operator, ">", Token::Gt}
syntax! {lesser_operator, "<", Token::Lt}

pub fn lex_operator(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    alt((
        equal_operator,
        not_equal_operator,
        arrow_operator,
        assign_operator,
        plus_operator,
        minus_operator,
        multiply_operator,
        divide_operator,
        not_operator,
        greater_operator_equal,
        lesser_operator_equal,
        greater_operator,
        lesser_operator,
    ))(input)
}

syntax! {dot_punctuation, ".", Token::Dot}
syntax! {comma_punctuation, ",", Token::Comma}
syntax! {semicolon_punctuation, ";", Token::Semi}
syntax! {colon_punctuation, ":", Token::Colon}
syntax! {lparen_punctuation, "(", Token::ParenOpen}
syntax! {rparen_punctuation, ")", Token::ParenClose}
syntax! {lbrace_punctuation, "{", Token::BraceOpen}
syntax! {rbrace_punctuation, "}", Token::BraceClose}
syntax! {lbracket_punctuation, "[", Token::BracketOpen}
syntax! {rbracket_punctuation, "]", Token::BracketClose}
syntax! {pound_punctuation, "#", Token::Pound}
syntax! {question_punctuation, "?", Token::Question}

pub fn lex_punctuations(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    alt((
        dot_punctuation,
        comma_punctuation,
        semicolon_punctuation,
        colon_punctuation,
        lparen_punctuation,
        rparen_punctuation,
        lbrace_punctuation,
        rbrace_punctuation,
        lbracket_punctuation,
        rbracket_punctuation,
        pound_punctuation,
        question_punctuation,
    ))(input)
}

fn pis(input: BytesSpan) -> ByteResult<Vec<u8>> {
    use std::result::Result::*;

    let (second_input, character) = take(1usize)(input)?;

    match character.as_bytes() {
        b"\"" => Ok((input, vec![])),
        b"\\" => {
            let (i2, c2) = take(1usize)(second_input)?;

            pis(i2).map(|(slice, done)| (slice, concat_slice_vec(c2.fragment(), done)))
        }
        c => pis(second_input).map(|(slice, done)| (slice, concat_slice_vec(c, done))),
    }
}

fn concat_slice_vec(c: &[u8], done: Vec<u8>) -> Vec<u8> {
    let mut new_vec = c.to_vec();

    new_vec.extend(&done);

    new_vec
}

fn convert_vec_utf8(v: Vec<u8>) -> Result<String, Utf8Error> {
    let slice = v.as_slice();

    str::from_utf8(slice).map(|s| s.to_owned())
}

fn complete_byte_slice_str_from_utf8(c: BytesSpan) -> Result<&str, Utf8Error> {
    str::from_utf8(c.fragment())
}

fn string(input: BytesSpan) -> ByteResult<String> {
    delimited(tag("\""), map_res(pis, convert_vec_utf8), tag("\""))(input)
}

fn lex_string(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    let (input, start) = position(input)?;
    let (input, value) = string(input)?;
    let (input, end) = position(input)?;

    Ok((
        input,
        Positioned::new(
            Token::StringLiteral(value),
            Span::from(start).between(Span::from(end)),
        ),
    ))
}

fn lex_reserved_ident(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    map_res(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: BytesSpan| {
            let c = complete_byte_slice_str_from_utf8(s);
            let s = s.into();

            c.map(|syntax| match syntax {
                "const" => Positioned::new(Token::Const, s),
                "let" => Positioned::new(Token::Let, s),
                "constructor" => Positioned::new(Token::Constructor, s),
                "null" => Positioned::new(Token::Null, s),
                "class" => Positioned::new(Token::Class, s),
                "interface" => Positioned::new(Token::Interface, s),
                "implements" => Positioned::new(Token::Implements, s),
                "this" => Positioned::new(Token::This, s),
                "return" => Positioned::new(Token::Return, s),
                "function" => Positioned::new(Token::Function, s),
                "public" => Positioned::new(Token::Public, s),
                "private" => Positioned::new(Token::Private, s),
                "static" => Positioned::new(Token::Static, s),
                "if" => Positioned::new(Token::If, s),
                "else" => Positioned::new(Token::Else, s),
                "true" => Positioned::new(Token::BoolLiteral(true), s),
                "false" => Positioned::new(Token::BoolLiteral(false), s),
                "new" => Positioned::new(Token::New, s),
                _ => Positioned::new(Token::Ident(syntax.to_string()), s),
            })
        },
    )(input)
}

fn complete_str_from_str<F: FromStr>(c: &str) -> Result<F, F::Err> {
    FromStr::from_str(c)
}

fn lex_illegal(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    map(take(1usize), |s: BytesSpan| {
        Positioned::new(Token::Illegal, s.into())
    })(input)
}

fn lex_integer(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    let (input, start) = position(input)?;
    let (input, value) = alt((
        map(
            map_res(
                map_res(digit1, complete_byte_slice_str_from_utf8),
                complete_str_from_str,
            ),
            Token::ByteLiteral,
        ),
        map(
            map_res(
                map_res(digit1, complete_byte_slice_str_from_utf8),
                complete_str_from_str,
            ),
            Token::ShortLiteral,
        ),
        map(
            map_res(
                map_res(digit1, complete_byte_slice_str_from_utf8),
                complete_str_from_str,
            ),
            Token::IntLiteral,
        ),
        map(
            map_res(
                map_res(digit1, complete_byte_slice_str_from_utf8),
                complete_str_from_str,
            ),
            Token::LongLiteral,
        ),
        map(
            map_res(
                map_res(
                    preceded(tag("-"), digit1),
                    complete_byte_slice_str_from_utf8,
                ),
                complete_str_from_str,
            ),
            |number: i16| Token::ShortLiteral(-number),
        ),
        map(
            map_res(
                map_res(
                    preceded(tag("-"), digit1),
                    complete_byte_slice_str_from_utf8,
                ),
                complete_str_from_str,
            ),
            |number: i32| Token::IntLiteral(-number),
        ),
        map(
            map_res(
                map_res(
                    preceded(tag("-"), digit1),
                    complete_byte_slice_str_from_utf8,
                ),
                complete_str_from_str,
            ),
            |number: i64| Token::LongLiteral(-number),
        ),
    ))(input)?;
    let (input, end) = position(input)?;

    Ok((
        input,
        Positioned::new(value, Span::from(start).between(Span::from(end))),
    ))
}

fn lex_comment(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    let (input, ((_, _), (position, comment))) = pair(
        pair(divide_operator, divide_operator),
        map(is_not("\r\n"), |s: BytesSpan| {
            (s, convert_vec_utf8(s.fragment().to_vec()).unwrap())
        }),
    )(input)?;

    Ok((
        input,
        Positioned::new(Token::Comment(comment), position.into()),
    ))
}

fn lex_token(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    alt((
        lex_comment,
        lex_operator,
        lex_punctuations,
        lex_string,
        lex_reserved_ident,
        lex_integer,
        lex_illegal,
    ))(input)
}

fn lex_tokens(input: BytesSpan) -> ByteResult<Vec<Positioned<Token>>> {
    many0(delimited(multispace0, lex_token, multispace0))(input)
}

pub struct Lexer;

impl Lexer {
    pub fn lex_tokens(bytes: BytesSpan) -> ByteResult<Vec<Positioned<Token>>> {
        lex_tokens(bytes).map(|(slice, result)| {
            (
                slice,
                [
                    &result[..],
                    &vec![Positioned::new(Token::EOF, slice.into())][..],
                ]
                .concat()
                .into_iter()
                .filter(|token| !matches!(token.value, Token::Comment(_)))
                .collect::<Vec<_>>(),
            )
        })
    }

    pub fn highlight(bytes: BytesSpan) -> String {
        lex_tokens(bytes)
            .map(|(_, tokens)| {
                let lines = tokens
                    .group_by(|a, b| a.span.line == b.span.line)
                    .map(|line| {
                        let mut raw_string =
                            String::with_capacity(line.last().unwrap().span.column + 1);

                        for token in line {
                            if raw_string.chars().count() < token.span.column {
                                raw_string.push_str(
                                    &" ".repeat(token.span.column - raw_string.chars().count()),
                                );
                            }

                            match &token.value {
                                Token::Illegal => {}
                                Token::EOF => {}
                                Token::BraceOpen => {
                                    raw_string.push('{');
                                }
                                Token::BraceClose => {
                                    raw_string.push('}');
                                }
                                Token::BracketOpen => {
                                    raw_string.push('[');
                                }
                                Token::BracketClose => {
                                    raw_string.push(']');
                                }
                                Token::ParenOpen => {
                                    raw_string.push('(');
                                }
                                Token::ParenClose => raw_string.push(')'),
                                Token::Ident(ident) => {
                                    raw_string.push_str(ident);
                                }
                                Token::StringLiteral(literal) => {
                                    raw_string.push_str(&format!("\"{literal}\""));
                                }
                                Token::ByteLiteral(literal) => {
                                    raw_string.push_str(&literal.to_string());
                                }
                                Token::ShortLiteral(literal) => {
                                    raw_string.push_str(&literal.to_string());
                                }
                                Token::IntLiteral(literal) => {
                                    raw_string.push_str(&literal.to_string());
                                }
                                Token::LongLiteral(literal) => {
                                    raw_string.push_str(&literal.to_string());
                                }
                                Token::FloatLiteral(literal) => {
                                    raw_string.push_str(&literal.to_string());
                                }
                                Token::BoolLiteral(literal) => {
                                    raw_string.push_str(&literal.to_string());
                                }
                                Token::Const => {
                                    raw_string.push_str("const");
                                }
                                Token::Let => {
                                    raw_string.push_str("let");
                                }
                                Token::Constructor => {
                                    raw_string.push_str("constructor");
                                }
                                Token::Null => {
                                    raw_string.push_str("null");
                                }
                                Token::Class => {
                                    raw_string.push_str("class");
                                }
                                Token::Interface => {
                                    raw_string.push_str("interface");
                                }
                                Token::Implements => {
                                    raw_string.push_str("implements");
                                }
                                Token::This => {
                                    raw_string.push_str("this");
                                }
                                Token::Return => {
                                    raw_string.push_str("return");
                                }
                                Token::Function => {
                                    raw_string.push_str("function");
                                }
                                Token::Public => {
                                    raw_string.push_str("public");
                                }
                                Token::Private => {
                                    raw_string.push_str("private");
                                }
                                Token::Static => {
                                    raw_string.push_str("static");
                                }
                                Token::If => {
                                    raw_string.push_str("if");
                                }
                                Token::Else => {
                                    raw_string.push_str("else");
                                }
                                Token::Comma => {
                                    raw_string.push(',');
                                }
                                Token::Dot => {
                                    raw_string.push('.');
                                }
                                Token::Ellipsis => {
                                    raw_string.push_str("...");
                                }
                                Token::Plus => {
                                    raw_string.push('+');
                                }
                                Token::Star => {
                                    raw_string.push('*');
                                }
                                Token::Slash => {
                                    raw_string.push('/');
                                }
                                Token::OrOr => {
                                    raw_string.push_str("||");
                                }
                                Token::PlusPlus => {
                                    raw_string.push_str("++");
                                }
                                Token::Minus => {
                                    raw_string.push('-');
                                }
                                Token::MinusMinus => {
                                    raw_string.push_str("--");
                                }
                                Token::EqEq => {
                                    raw_string.push_str("==");
                                }
                                Token::Eq => {
                                    raw_string.push('=');
                                }
                                Token::Ne => {
                                    raw_string.push_str("!=");
                                }
                                Token::Le => {
                                    raw_string.push_str("<=");
                                }
                                Token::Ge => {
                                    raw_string.push_str(">=");
                                }
                                Token::Lt => {
                                    raw_string.push('<');
                                }
                                Token::Gt => {
                                    raw_string.push('>');
                                }
                                Token::Not => {
                                    raw_string.push('!');
                                }
                                Token::FatArrow => {
                                    raw_string.push_str("=>");
                                }
                                Token::DoubleQuote => {
                                    raw_string.push('"');
                                }
                                Token::Colon => {
                                    raw_string.push(':');
                                }
                                Token::Semi => {
                                    raw_string.push(';');
                                }
                                Token::Question => {
                                    raw_string.push('?');
                                }
                                Token::Pound => {
                                    raw_string.push('#');
                                }
                                Token::New => raw_string.push_str("new"),
                                Token::Comment(comment) => {
                                    raw_string.push_str(&format!("//{comment}"))
                                }
                            }
                        }

                        let mut string = raw_string.clone();

                        for token in line {
                            match &token.value {
                                Token::Illegal => {}
                                Token::EOF => {}
                                Token::BraceOpen => {
                                    string = string.replace('{', &'{'.bright_yellow().to_string());
                                }
                                Token::BraceClose => {
                                    string = string.replace('}', &'}'.bright_yellow().to_string());
                                }
                                Token::BracketOpen => {
                                    string = string
                                        .replace('[', &'['.fg_rgb::<86, 156, 214>().to_string());
                                }
                                Token::BracketClose => {
                                    string = string
                                        .replace(']', &']'.fg_rgb::<86, 156, 214>().to_string());
                                }
                                Token::ParenOpen => {
                                    string = string
                                        .replace('(', &'('.fg_rgb::<197, 134, 192>().to_string());
                                }
                                Token::ParenClose => {
                                    string = string
                                        .replace(')', &')'.fg_rgb::<197, 134, 192>().to_string());
                                }
                                Token::StringLiteral(literal) => {
                                    string = string.replace(
                                        &format!("\"{literal}\""),
                                        &format!("\"{literal}\"")
                                            .fg_rgb::<206, 145, 120>()
                                            .to_string(),
                                    );
                                }
                                Token::ByteLiteral(literal) => {
                                    string = string.replace(
                                        &literal.to_string(),
                                        &literal.fg_rgb::<181, 206, 168>().to_string(),
                                    );
                                }
                                Token::ShortLiteral(literal) => {
                                    string = string.replace(
                                        &literal.to_string(),
                                        &literal.fg_rgb::<181, 206, 168>().to_string(),
                                    );
                                }
                                Token::IntLiteral(literal) => {
                                    string = string.replace(
                                        &literal.to_string(),
                                        &literal.fg_rgb::<181, 206, 168>().to_string(),
                                    );
                                }
                                Token::LongLiteral(literal) => {
                                    string = string.replace(
                                        &literal.to_string(),
                                        &literal.fg_rgb::<181, 206, 168>().to_string(),
                                    );
                                }
                                Token::FloatLiteral(literal) => {
                                    string = string.replace(
                                        &literal.to_string(),
                                        &literal.fg_rgb::<181, 206, 168>().to_string(),
                                    );
                                }
                                Token::BoolLiteral(literal) => {
                                    string = string.replace(
                                        &literal.to_string(),
                                        &literal.fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Const => {
                                    string = string.replace(
                                        "const",
                                        &"const".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Let => {
                                    string = string.replace(
                                        "let",
                                        &"let".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Constructor => {
                                    string = string.replace(
                                        "constructor",
                                        &"constructor".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Null => {
                                    string = string.replace(
                                        "null",
                                        &"null".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Class => {
                                    string = string.replace(
                                        "class",
                                        &"class".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Interface => {
                                    string = string.replace(
                                        "interface",
                                        &"interface".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Implements => {
                                    string = string.replace(
                                        "implements",
                                        &"implements".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::This => {
                                    string = string.replace(
                                        "this",
                                        &"this".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Return => {
                                    string = string.replace(
                                        "return",
                                        &"return".fg_rgb::<197, 134, 192>().to_string(),
                                    );
                                }
                                Token::Function => {
                                    string = string.replace(
                                        "function",
                                        &"function".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Public => {
                                    string = string.replace(
                                        "public",
                                        &"public".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Private => {
                                    string = string.replace(
                                        "private",
                                        &"private".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::Static => {
                                    string = string.replace(
                                        "static",
                                        &"static".fg_rgb::<86, 156, 214>().to_string(),
                                    );
                                }
                                Token::If => {
                                    string = string
                                        .replace("if", &"if".fg_rgb::<197, 134, 192>().to_string());
                                }
                                Token::Else => {
                                    string = string.replace(
                                        "else",
                                        &"else".fg_rgb::<197, 134, 192>().to_string(),
                                    );
                                }
                                Token::Comma => {
                                    // string.push(',');
                                }
                                Token::Dot => {
                                    // string.push('.');
                                }
                                Token::Ellipsis => {
                                    // string.push_str("...");
                                }
                                Token::Plus => {
                                    // string.push('+');
                                }
                                Token::Star => {
                                    // string.push('*');
                                }
                                Token::Slash => {
                                    // string.push('/');
                                }
                                Token::OrOr => {
                                    // string.push_str("||");
                                }
                                Token::PlusPlus => {
                                    // string.push_str("++");
                                }
                                Token::Minus => {
                                    // string.push('-');
                                }
                                Token::MinusMinus => {
                                    // string.push_str("--");
                                }
                                Token::EqEq => {
                                    // string.push_str("==");
                                }
                                Token::Eq => {
                                    // string.push('=');
                                }
                                Token::Ne => {
                                    // string.push_str("!=");
                                }
                                Token::Le => {
                                    // string.push_str("<=");
                                }
                                Token::Ge => {
                                    // string.push_str(">=");
                                }
                                Token::Lt => {
                                    // string.push('<');
                                }
                                Token::Gt => {
                                    // string.push('>');
                                }
                                Token::Not => {
                                    // string.push('!');
                                }
                                Token::FatArrow => {
                                    // string.push_str("=>");
                                }
                                Token::DoubleQuote => {
                                    // string.push('"');
                                }
                                Token::Colon => {
                                    // string.push(':');
                                }
                                Token::Semi => {
                                    // string.push(';');
                                }
                                Token::Question => {
                                    // string.push('?');
                                }
                                Token::Pound => {
                                    // string.push('#');
                                }
                                Token::New => {
                                    string = string.replace(
                                        "new",
                                        &"new".fg_rgb::<197, 134, 192>().to_string(),
                                    )
                                }
                                Token::Comment(comment) => {
                                    string = string.replace(
                                        &format!("//{comment}"),
                                        &format!("//{comment}")
                                            .fg_rgb::<106, 153, 85>()
                                            .to_string(),
                                    );
                                }
                                Token::Ident(ident) => {
                                    let color = match ident.as_str() {
                                        "string" | "byte" | "short" | "int" | "long"
                                        | "boolean" | "object" | "array" => {
                                            Some(ident.fg_rgb::<78, 201, 176>())
                                        }
                                        &_ => None,
                                    };

                                    if let Some(color) = color {
                                        string = string.replace(ident, &color.to_string());
                                    } else if ident.chars().nth(0).unwrap().is_uppercase() {
                                        string = string.replace(
                                            ident,
                                            &ident.fg_rgb::<78, 201, 176>().to_string(),
                                        );
                                    } else {
                                        string = string.replace(
                                            ident,
                                            &ident.fg_rgb::<220, 220, 170>().to_string(),
                                        );
                                    }
                                }
                            }
                        }

                        (line[0].span.line - 1, (raw_string, string))
                    })
                    .collect::<Vec<_>>();

                let mut string = String::new();

                for line_index in 0..(lines.last().unwrap().0 + 1) {
                    if let Some(line) = lines.iter().find_map(|line| {
                        if line.0 == line_index {
                            Some(&line.1 .1)
                        } else {
                            None
                        }
                    }) {
                        string.push_str(line);
                    }

                    string.push('\n');
                }

                string.trim_end().to_string()
            })
            .unwrap()
    }
}
