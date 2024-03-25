use chumsky::prelude::*;

use super::{token::Token, ParserInput, Span, Spanned};

/// Represents a single executable statement within a COBOL program.
#[derive(Debug)]
pub(crate) enum Stat<'src> {
    Display(&'src str)
}

/// Parser for an individual statement within a COBOL program.
pub(super) fn stat<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Stat<'src>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let display = just(Token::Display)
        .then(any().filter(|t| matches!(t, Token::Str(_))))
        .then_ignore(just(Token::Ctrl('.')))
        .map(|(_, txt)| Stat::Display(txt.unwrap_str()));

    display.map_with(|stat, meta| (stat, meta.span()))
}