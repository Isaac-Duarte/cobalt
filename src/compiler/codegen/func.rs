use cranelift::frontend::FunctionBuilder;
use cranelift_object::ObjectModule;
use miette::Result;
use crate::compiler::parser::{Spanned, Stat};

/// Structure for translating function-level AST nodes to Cranelift IR.
pub(super) struct FuncTranslator<'src> {
    pub builder: FunctionBuilder<'src>,
    pub module: &'src mut ObjectModule
}

impl<'src> FuncTranslator<'src> {
    /// Generates Cranelift IR for a single statement from the given set of statements.
    pub fn translate(&mut self, stats: &Vec<Spanned<Stat<'src>>>) -> Result<()> {
        Ok(())
    }
}