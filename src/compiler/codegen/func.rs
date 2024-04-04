use crate::compiler::parser::{self, Ast, Literal, MoveData, Spanned, Stat};
use cranelift::{
    codegen::ir::{immediates::Offset32, types, InstBuilder, MemFlags},
    frontend::FunctionBuilder,
};
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

    /// The overall AST being translated.
    pub ast: &'src Ast<'src>,

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
        match &stat.0 {
            Stat::Display(lit_id) => self.translate_display(*lit_id)?,
            Stat::Move(mov_data) => self.translate_move(mov_data)?,
        }
        Ok(())
    }

    /// Generates Cranelift IR for a single "DISPLAY" statement.
    fn translate_display(&mut self, lit_id: usize) -> Result<()> {
        // Declare a reference to the string for this display.
        let string_gv = self
            .module
            .declare_data_in_func(self.data.str_data_id(lit_id)?, self.builder.func);

        // Call "puts" on the string.
        let puts =
            self.intrinsics
                .get_ref(self.module, self.builder.func, CobaltIntrinsic::LibcPuts)?;
        let ptr_type = self.module.target_config().pointer_type();
        let string_ptr = self.builder.ins().global_value(ptr_type, string_gv);
        self.builder.ins().call(*puts, &[string_ptr]);

        Ok(())
    }

    /// Generates Cranelift IR for a single "MOVE" statement.
    fn translate_move(&mut self, mov_data: &MoveData) -> Result<()> {
        // Fetch the destination variable's layout.
        let dest_pic = self.data.sym_pic(mov_data.dest)?;

        // Move based on the source, either a literal/variable.
        // We also verify here whether the source properly matches the destination.
        match &mov_data.source {
            parser::Value::Literal(lit) => {
                if !dest_pic.verify_lit(&self.ast.str_lits, lit) {
                    miette::bail!(
                        "Attempted to move incompatible literal '{}' into variable '{}' ({} bytes).",
                        lit.text(&self.ast.str_lits),
                        mov_data.dest,
                        dest_pic.comp_size()
                    );
                }
                self.translate_mov_lit(lit, mov_data.dest)?;
            }
            parser::Value::Variable(sym) => {
                let source_pic = self.data.sym_pic(sym)?;
                if !source_pic.fits_within_comp(dest_pic) {
                    miette::bail!("Attempted to move incompatible variable '{}' ({} bytes) into variable '{}' ({} bytes).",
                        sym, source_pic.comp_size(), mov_data.dest, dest_pic.comp_size());
                }
                self.translate_mov_var(sym, mov_data.dest)?;
            }
        }
        Ok(())
    }

    /// Moves the given literal into the provided global data slot.
    /// Assumes that the literal has already been checked for compatibility with this data slot.
    fn translate_mov_lit(&mut self, lit: &Literal, dest: &str) -> Result<()> {
        let dest_id = self.data.sym_data_id(dest)?;
        let dest_pic = self.data.sym_pic(dest)?;

        // Import the destination variable into the function, get a pointer to it.
        let dest_gv = self.module.declare_data_in_func(dest_id, self.builder.func);
        let ptr_type = self.module.target_config().pointer_type();
        let dest_ptr = self.builder.ins().global_value(ptr_type, dest_gv);

        match lit {
            Literal::Int(i) => {
                let i_val = self.builder.ins().iconst(types::I64, *i);
                self.builder
                    .ins()
                    .store(MemFlags::new(), i_val, dest_ptr, Offset32::new(0));
            }
            Literal::Float(f) => {
                let f_val = self.builder.ins().f64const(*f);
                self.builder
                    .ins()
                    .store(MemFlags::new(), f_val, dest_ptr, Offset32::new(0));
            }
            Literal::String(sid) => {
                // Import the string literal into the function, get a pointer.
                let string_gv = self
                    .module
                    .declare_data_in_func(self.data.str_data_id(*sid)?, self.builder.func);
                let string_ptr = self.builder.ins().global_value(ptr_type, string_gv);

                // Get the size of the string to copy.
                let size = self
                    .ast
                    .str_lits
                    .get(*sid)
                    .ok_or(miette::diagnostic!(
                        "Failed to fetch string data for literal ID '{}'.",
                        sid
                    ))?
                    .len()
                    + 1;
                let size_val = self.builder.ins().iconst(ptr_type, size as i64);

                // Sanity check.
                assert!(size <= dest_pic.comp_size());

                // Perform a straight memory copy of the value.
                self.builder.call_memcpy(
                    self.module.target_config(),
                    dest_ptr,
                    string_ptr,
                    size_val,
                );
            }
        }
        Ok(())
    }

    /// Moves the given global variable into the provided global data slot.
    /// Assumes that the variable has already been checked for compatibility with this data slot.
    fn translate_mov_var(&mut self, src: &str, dest: &str) -> Result<()> {
        let (src_id, src_pic) = (self.data.sym_data_id(src)?, self.data.sym_pic(src)?);
        let (dest_id, dest_pic) = (self.data.sym_data_id(dest)?, self.data.sym_pic(dest)?);

        // Import both variables as global values, get pointers to them.
        let ptr_type = self.module.target_config().pointer_type();
        let dest_gv = self.module.declare_data_in_func(dest_id, self.builder.func);
        let dest_ptr = self.builder.ins().global_value(ptr_type, dest_gv);
        let src_gv = self.module.declare_data_in_func(src_id, self.builder.func);
        let src_ptr = self.builder.ins().global_value(ptr_type, src_gv);

        // Sanity check.
        assert!(src_pic.comp_size() <= dest_pic.comp_size());

        // Based on the source type, determine the copy mechanism.
        if src_pic.is_str() {
            // Get the size to copy from the source variable.
            let size_val = self
                .builder
                .ins()
                .iconst(ptr_type, src_pic.comp_size() as i64);

            // Perform a memcpy().
            self.builder
                .call_memcpy(self.module.target_config(), dest_ptr, src_ptr, size_val);
        } else if src_pic.is_float() {
            // Load & then re-store the float.
            let temp =
                self.builder
                    .ins()
                    .load(types::F64, MemFlags::new(), src_ptr, Offset32::new(0));
            self.builder
                .ins()
                .store(MemFlags::new(), temp, dest_ptr, Offset32::new(0));
        } else {
            // Load & then re-store the integer.
            let temp =
                self.builder
                    .ins()
                    .load(types::I64, MemFlags::new(), src_ptr, Offset32::new(0));
            self.builder
                .ins()
                .store(MemFlags::new(), temp, dest_ptr, Offset32::new(0));
        }
        Ok(())
    }
}
