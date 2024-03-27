use std::collections::HashMap;

use cranelift::{codegen::ir::{types, AbiParam, InstBuilder}, frontend::FunctionBuilder};
use cranelift_module::{DataId, Module};
use cranelift_object::ObjectModule;
use miette::Result;
use crate::compiler::parser::{LiteralId, Spanned, Stat};

/// Structure for translating function-level AST nodes to Cranelift IR.
pub(super) struct FuncTranslator<'src> {
    pub builder: FunctionBuilder<'src>,
    pub module: &'src mut ObjectModule,
    pub lit_map: &'src mut HashMap<LiteralId, DataId>,
}

impl<'src> FuncTranslator<'src> {
    /// Generates Cranelift IR for a single statement from the given set of statements.
    pub fn translate(&mut self, stats: &Vec<Spanned<Stat<'src>>>) -> Result<()> {
        for stat in stats {
            self.translate_stat(stat)?;
        }
        Ok(())
    }

    /// Generates Cranelift IR for a single statement from the given set of statements.
    fn translate_stat(&mut self, stat: &Spanned<Stat<'src>>) -> Result<()> {
        match stat.0 {
            Stat::Display(lit_id) => self.translate_display(lit_id)?,
            Stat::_Placeholder(_) => unreachable!()
        }
        Ok(())
    }

    /// Generates Cranelift IR for a single "DISPLAY" statement.
    fn translate_display(&mut self, lit_id: usize) -> Result<()> {
        // Declare a reference to the string for this display.
        let string_gv = self.module.declare_data_in_func(*self.lit_map.get(&lit_id).unwrap(), self.builder.func);

        // Call "putchar" on test data.
        let ptr_type = self.module.target_config().pointer_type();
        let mut putchar_sig = self.module.make_signature();
        putchar_sig.params.push(AbiParam::new(ptr_type));
        putchar_sig.returns.push(AbiParam::new(types::I32));
        let puts_id = self.module.declare_function("puts", cranelift_module::Linkage::Import, &putchar_sig).unwrap();
        let puts_ref = self.module.declare_func_in_func(puts_id, self.builder.func);

        let string_ptr = self.builder.ins().global_value(ptr_type, string_gv);
        self.builder.ins().call(puts_ref, &[string_ptr]);
        Ok(())
    }
}