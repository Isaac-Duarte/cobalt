use crate::compiler::parser::{Ast, Spanned, Stat};
use cranelift::frontend::FunctionBuilder;
use cranelift_object::ObjectModule;
use miette::Result;

use self::value::ValueCache;

use super::{data::DataManager, intrinsics::IntrinsicManager};

mod io;
mod math;
mod memory;
mod value;

/// Structure for translating function-level AST nodes to Cranelift IR.
pub(super) struct FuncTranslator<'src> {
    /// The function builder to use when translating the function.
    pub builder: FunctionBuilder<'src>,

    /// The module this function is a part of.
    pub module: &'src mut ObjectModule,

    /// The overall AST being translated.
    pub ast: &'src Ast<'src>,

    /// The intrinsics manager for this function.
    pub intrinsics: &'src mut IntrinsicManager,

    /// The data manager for this function.
    pub data: &'src mut DataManager,

    /// Cache of values loaded for this function.
    values: ValueCache,
}

impl<'src> FuncTranslator<'src> {
    /// Creates a new function translator.
    pub fn new(
        builder: FunctionBuilder<'src>,
        module: &'src mut ObjectModule,
        ast: &'src Ast<'src>,
        intrinsics: &'src mut IntrinsicManager,
        data: &'src mut DataManager,
    ) -> Self {
        Self {
            builder,
            module,
            ast,
            intrinsics,
            data,
            values: ValueCache::new(),
        }
    }

    /// Generates Cranelift IR for a single statement from the given set of statements.
    pub fn translate(&mut self, stats: &Vec<Spanned<Stat<'src>>>) -> Result<()> {
        // Reset the intrinsics manager, since we're beginning a new function.
        self.intrinsics.clear_refs();

        // Translate all statements within the function.
        for stat in stats {
            self.translate_stat(stat)?;
        }
        Ok(())
    }

    /// Generates Cranelift IR for a single statement from the given set of statements.
    fn translate_stat(&mut self, stat: &Spanned<Stat<'src>>) -> Result<()> {
        match &stat.0 {
            Stat::Display(vals) => self.translate_display(vals)?,
            Stat::Move(mov_data) => self.translate_move(mov_data)?,
            Stat::Add(op_data) => self.translate_add(op_data)?,
            Stat::Subtract(op_data) => self.translate_subtract(op_data)?,
            Stat::Multiply(op_data) => self.translate_multiply(op_data)?,
        }
        Ok(())
    }
}
