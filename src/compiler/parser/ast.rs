use chumsky::prelude::*;

use super::{divs::{ident_div, proc_div, IdentDiv, ProcDiv}, token::Token, ParserInput, Span};

/// Represents the overall AST of a COBOL program.
#[derive(Debug)]
pub struct Ast<'src> {
    // The identification division of the program.
    pub ident_div: IdentDiv<'src>,

    // The procedure division of the program.
    pub proc_div: ProcDiv<'src>
}

/// Parser for an entire COBOL program's AST.
pub(super) fn ast<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Ast<'src>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let ast = ident_div()
        .then(proc_div())
        .map(|(ident, proc)| Ast {
            ident_div: ident,
            proc_div: proc
        });

    ast
}