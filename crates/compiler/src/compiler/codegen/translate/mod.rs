use crate::compiler::parser::{Ast, Spanned, Stat};
use cranelift::{
    codegen::ir::{Block, InstBuilder},
    frontend::FunctionBuilder,
};
use cranelift_module::Module;
use cranelift_object::ObjectModule;
use miette::Result;

use self::value::ValueCache;

use super::{
    data::DataManager,
    func::FuncManager,
    intrinsics::{CobaltIntrinsic, IntrinsicManager},
};

mod cond;
mod control;
mod io;
mod math;
mod memory;
mod value;

/// Structure for translating function-level AST nodes to Cranelift IR.
pub(super) struct FuncTranslator<'a, 'src: 'a> {
    /// The function builder to use when translating the function.
    pub builder: FunctionBuilder<'src>,

    /// The module this function is a part of.
    pub module: &'a mut ObjectModule,

    /// The overall AST being translated.
    pub ast: &'a Ast<'src>,

    /// The intrinsics manager for this function.
    pub intrinsics: &'a mut IntrinsicManager,

    /// The data manager for this function.
    pub data: &'a mut DataManager,

    /// The function manager for the program.
    pub funcs: &'a mut FuncManager,

    /// Cache of values loaded for this function.
    values: ValueCache,
}

impl<'a, 'src> FuncTranslator<'a, 'src> {
    /// Creates a new function translator.
    pub fn new(
        builder: FunctionBuilder<'src>,
        module: &'a mut ObjectModule,
        ast: &'a Ast<'src>,
        intrinsics: &'a mut IntrinsicManager,
        data: &'a mut DataManager,
        funcs: &'a mut FuncManager,
    ) -> Self {
        Self {
            builder,
            module,
            ast,
            intrinsics,
            data,
            funcs,
            values: ValueCache::new(),
        }
    }

    /// Generates Cranelift IR for a single statement from the given set of statements.
    pub fn translate(&mut self, stats: &Vec<Spanned<Stat<'src>>>) -> Result<()> {
        // Reset the intrinsics manager, function manager since we're beginning a new function.
        self.intrinsics.clear_refs();
        self.funcs.clear_refs();

        // Translate all statements within the function.
        for stat in stats {
            self.translate_stat(stat)?;
        }
        Ok(())
    }

    /// Generates Cranelift IR for a program termination.
    /// Required for paragraphs which unconditionally terminate the program (e.g. with `STOP RUN`).
    pub fn translate_terminate(&mut self) -> Result<()> {
        let libc_exit = self.intrinsics.get_ref(
            self.module,
            &mut self.builder.func,
            CobaltIntrinsic::LibcExit,
        )?;
        let ptr_type = self.module.target_config().pointer_type();
        let exit_code = self.builder.ins().iconst(ptr_type, 0x0);
        self.builder.ins().call(libc_exit, &[exit_code]);
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
            Stat::Divide(div_data) => self.translate_divide(div_data)?,
            Stat::If(if_data) => self.translate_if(if_data)?,
            Stat::Perform(perform) => self.translate_perform(perform)?,
            Stat::Accept(target) => self.translate_accept(target)?,
        }
        Ok(())
    }

    /// Switches the function translator to point to the given block.
    /// The prior block must have a terminator instruction inserted before this is called.
    fn switch_to_block(&mut self, block: Block) {
        self.values.begin_block();
        self.builder.switch_to_block(block);
    }
}
