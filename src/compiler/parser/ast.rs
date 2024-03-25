use miette::Result;
use super::{divs::{IdentDiv, ProcDiv}, Parser};

/// Represents the overall AST of a COBOL program.
#[derive(Debug)]
pub struct Ast<'src> {
    // The identification division of the program.
    pub ident_div: IdentDiv<'src>,

    // The procedure division of the program.
    pub proc_div: ProcDiv<'src>
}

impl<'src> Parser<'src> {
    /// Parses a complete AST from the current parser position.
    pub(crate) fn ast(&mut self) -> Result<Ast<'src>> {
        // Extract the identification, procedure division.
        let ident_div = self.ident_div()?;
        let proc_div = self.proc_div()?;
        
        Ok(Ast {
            ident_div,
            proc_div
        })
    }
}