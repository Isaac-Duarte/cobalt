use crate::compiler::parser::{Spanned, Stat};
use cranelift::{codegen::ir::InstBuilder, frontend::FunctionBuilder};
use cranelift_module::Module;
use cranelift_object::ObjectModule;
use miette::Result;

use super::{
    data::DataManager,
    intrinsics::{CobaltIntrinsic, IntrinsicManager},
};

/// Structure for translating function-level AST nodes to Cranelift IR.
pub(super) struct FuncTranslator<'src> {
    /// The function builder to use when translating the function.
    pub builder: FunctionBuilder<'src>,

    /// The module this function is a part of.
    pub module: &'src mut ObjectModule,

    /// The intrinsics manager for this function.
    pub intrinsics: &'src mut IntrinsicManager,

    /// The data manager for this function.
    pub data: &'src mut DataManager,
}

impl<'src> FuncTranslator<'src> {
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
        match stat.0 {
            Stat::Display(lit_id) => self.translate_display(lit_id)?,
            Stat::_Placeholder(_) => unreachable!(),
        }
        Ok(())
    }

    /// Generates Cranelift IR for a single "DISPLAY" statement.
    fn translate_display(&mut self, lit_id: usize) -> Result<()> {
        // Declare a reference to the string for this display.
        let string_gv = self
            .module
            .declare_data_in_func(self.data.str_data_id(lit_id).unwrap(), self.builder.func);

        // Call "puts" on the string.
        let puts = self.intrinsics.get_ref(self.module, self.builder.func, CobaltIntrinsic::LibcPuts)?;
        let ptr_type = self.module.target_config().pointer_type();
        let string_ptr = self.builder.ins().global_value(ptr_type, string_gv);
        self.builder.ins().call(*puts, &[string_ptr]);

        Ok(())
    }
}
