use bimap::BiMap;
use miette::Result;
use super::{divs::{DataDiv, IdentDiv, ProcDiv}, token::Token, StrLitId, Parser};

/// Represents the overall AST of a COBOL program.
#[derive(Debug)]
pub struct Ast<'src> {
    // The identification division of the program.
    pub ident_div: IdentDiv<'src>,

    // The data division of the program.
    pub data_div: Option<DataDiv<'src>>,

    // The procedure division of the program.
    pub proc_div: ProcDiv<'src>,

    // Map of string literals used throughout the AST.
    pub str_lits: BiMap<StrLitId, String>
}

impl<'src> Parser<'src> {
    /// Parses a complete AST from the current parser position.
    pub(super) fn ast(mut self) -> Result<Ast<'src>> {
        // Extract each division.
        let ident_div = self.ident_div()?;
        let data_div = match self.peek() {
            Token::DataDiv => Some(self.data_div()?),
            _ => None
        };
        let proc_div = self.proc_div()?;
        
        Ok(Ast {
            ident_div,
            data_div,
            proc_div,
            str_lits: self.str_lit_map
        })
    }
}