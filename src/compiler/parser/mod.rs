use miette::Result;
use chumsky::{span::SimpleSpan, Parser};
mod token;

/// A simple span type for use in AST items throughout the parser.
pub type Span = SimpleSpan<usize>;

pub fn parse(input: &str) -> Result<()> {
    let lexer = token::lexer();
    let (_, errs) = lexer.parse(input).into_output_errors();
    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .for_each(|e| {
            println!("{}", e.reason().to_string());
        });
    Ok(())
}