use cranelift::{codegen::ir::immediates::Offset32, prelude::*};
use cranelift_module::{DataId, Module};
use miette::Result;

use crate::compiler::parser::{self, Literal, MoveData};

use super::FuncTranslator;

impl<'src> FuncTranslator<'src> {
    /// Generates Cranelift IR for a single "MOVE" statement.
    pub(super) fn translate_move(&mut self, mov_data: &MoveData) -> Result<()> {
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
    pub(super) fn translate_mov_lit(&mut self, lit: &Literal, dest: &str) -> Result<()> {
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

    /// Loads the given [`parser::Value`] into the function as a Cranelift [`Value`].
    /// If the value is a string, loads a pointer to the string. Immediates are loaded with iconst.
    pub(super) fn load_value(&mut self, val: &parser::Value<'src>) -> Result<Value> {
        match val {
            parser::Value::Variable(sym) => self.load_var(sym),
            parser::Value::Literal(lit) => self.load_lit(lit),
        }
    }

    /// Loads the given variable into the function as a Cranelift [`Value`].
    /// If the variable is a string, loads a pointer to the string.
    pub(super) fn load_var(&mut self, sym: &'src str) -> Result<Value> {
        let ptr = self.load_ptr(self.data.sym_data_id(sym)?);
        let pic = self.data.sym_pic(sym)?;
        if pic.is_str() {
            Ok(ptr)
        } else if pic.is_float() {
            Ok(self
                .builder
                .ins()
                .load(types::F64, MemFlags::new(), ptr, Offset32::new(0)))
        } else {
            Ok(self
                .builder
                .ins()
                .load(types::I64, MemFlags::new(), ptr, Offset32::new(0)))
        }
    }

    /// Loads the given literal into the function as a Cranelift [`Value`].
    /// If the literal is a string, loads a pointer to the string.
    pub(super) fn load_lit(&mut self, lit: &Literal) -> Result<Value> {
        match lit {
            Literal::String(sid) => Ok(self.load_ptr(self.data.str_data_id(*sid)?)),
            Literal::Int(i) => Ok(self.builder.ins().iconst(types::I64, *i)),
            Literal::Float(f) => Ok(self.builder.ins().f64const(*f)),
        }
    }

    /// Loads a pointer to the data associated with the given [`DataId`] into the function.
    /// todo: Make this re-use static (read-only) global values instead of creating one every time.
    pub(super) fn load_ptr(&mut self, data_id: DataId) -> Value {
        let ptr_type = self.module.target_config().pointer_type();
        let gv = self.module.declare_data_in_func(data_id, self.builder.func);
        self.builder.ins().global_value(ptr_type, gv)
    }
}
