#![feature(slice_group_by)]

use std::fs;
use std::io;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::Runtime;

pub mod globals;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod util;

#[test]
fn test_main() -> Result<(), io::Error> {
    let input = fs::read_to_string("/home/aiving/Documents/kz.aiving/rs/suslang/main.sus")?;
    let code = input.as_bytes();

    let (_, tokens) = Lexer::lex_tokens(code.into()).unwrap();
    let (_, parsed) = Parser::parse_tokens(&tokens).unwrap();

    let runtime = Runtime::default();

    println!("{}\n<----------------------------------------------------------->", input);
    println!(
        "{}",
        runtime.eval_program(parsed).format(
            "/home/aiving/Documents/kz.aiving/rs/suslang/main.sus",
            &Lexer::highlight(code.into())
        )
    );

    Ok(())
}
