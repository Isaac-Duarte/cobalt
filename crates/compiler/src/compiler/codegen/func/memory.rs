use cranelift::{
    codegen::ir::{immediates::Offset32, GlobalValue},
    prelude::*,
};
use cranelift_module::{DataId, Module};
use miette::Result;

use crate::compiler::parser::{self, Literal, MoveData};

use super::{var::CodegenLiteral, FuncTranslator};

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

        // Import the destination variable into the function, get a pointer to it.
        let ptr_type = self.module.target_config().pointer_type();
        let dest_ptr = self.load_static_ptr(dest_id)?;
        let src_val = self.load_lit(lit)?;

        // Load the relevant PIC.
        let dest_pic = self.data.sym_pic(dest)?;

        match lit {
            Literal::Int(_) | Literal::Float(_) => {
                self.builder
                    .ins()
                    .store(MemFlags::new(), src_val, dest_ptr, Offset32::new(0));
            }
            Literal::String(sid) => {
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
                self.builder
                    .call_memcpy(self.module.target_config(), dest_ptr, src_val, size_val);
            }
        }
        Ok(())
    }

    /// Moves the given global variable into the provided global data slot.
    /// Assumes that the variable has already been checked for compatibility with this data slot.
    fn translate_mov_var(&mut self, src: &str, dest: &str) -> Result<()> {
        // Import both variables as global values, get pointers to them.
        let ptr_type = self.module.target_config().pointer_type();
        let (src_id, dest_id) = (self.data.sym_data_id(src)?, self.data.sym_data_id(dest)?);
        let (src_ptr, dest_ptr) = (
            self.load_static_ptr(src_id)?,
            self.load_static_ptr(dest_id)?,
        );

        // Load the PIC for the source/destination.
        let (src_pic, dest_pic) = (self.data.sym_pic(src)?, self.data.sym_pic(dest)?);

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
        let ptr = self.load_static_ptr(self.data.sym_data_id(sym)?)?;
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
        // Is the variable in cache?
        if let Some(var) = self.var_cache.get_litv(lit) {
            return Ok(self.builder.use_var(var));
        }

        // No, load the value & store it inside a variable in cache.
        let var = self.var_cache.create_var();
        let ptr_type = self.module.target_config().pointer_type();
        match lit {
            Literal::String(sid) => {
                let val = self.load_static_ptr(self.data.str_data_id(*sid)?)?;
                self.builder.declare_var(var, ptr_type);
                self.builder.def_var(var, val);
            },
            Literal::Int(i) => {
                let val = self.builder.ins().iconst(types::I64, *i);
                self.builder.declare_var(var, types::I64);
                self.builder.def_var(var, val);
            },
            Literal::Float(f) => {
                let val = self.builder.ins().f64const(*f);
                self.builder.declare_var(var, types::F64);
                self.builder.def_var(var, val);
            },
        }
        self.var_cache.insert_litv(lit, var)?;
        Ok(self.builder.use_var(var))
    }

    /// Loads the given codegen-only literal into the function as a Cranelift [`Value`].
    /// Utilises variable cache when available.
    pub(super) fn load_cg_lit(&mut self, cglit: &CodegenLiteral) -> Result<Value> {
        // Is a variable holding this literal in cache?
        if let Some(var) = self.var_cache.get_cg_litv(cglit) {
            return Ok(self.builder.use_var(var));
        }

        // No, we still need to load it first, then store in cache.
        let var = self.var_cache.create_var();
        match cglit {
            CodegenLiteral::Char(c) => {
                let val = self.builder.ins().iconst(types::I8, (*c as u8) as i64);
                self.builder.declare_var(var, types::I8);
                self.builder.def_var(var, val);
            },
        }
        self.var_cache.insert_cg_litv(cglit, var)?;
        Ok(self.builder.use_var(var))
    }

    /// Loads an immutable pointer to the data associated with the given [`DataId`] into the function.
    /// Utilises static variable cache when possible.
    pub(super) fn load_static_ptr(&mut self, data_id: DataId) -> Result<Value> {
        // Check whether a variable holding this pointer is in cache.
        if let Some(sptr) = self.var_cache.get_static_ptr(&data_id) {
            return Ok(self.builder.use_var(sptr));
        }

        // Not in cache, load it up.
        // We also declare a variable for this pointer to place in cache.
        let ptr_type = self.module.target_config().pointer_type();
        let var = self.var_cache.create_var();
        self.builder.declare_var(var, ptr_type);

        // Load the pointer into the variable, use it.
        let gv = self.load_gv(data_id)?;
        let sptr_val = self.builder.ins().global_value(ptr_type, gv);
        self.builder.def_var(var, sptr_val);

        self.var_cache.insert_static_ptr(&data_id, var)?;
        Ok(self.builder.use_var(var))
    }

    /// Loads a single [`GlobalValue`] into the current function, caching it if not already present.
    /// If the given data ID has already been loaded, returns the GV from cache.
    fn load_gv(&mut self, data_id: DataId) -> Result<GlobalValue> {
        if let Some(gv) = self.var_cache.get_gv(&data_id) {
            return Ok(gv);
        }
        let gv = self.module.declare_data_in_func(data_id, self.builder.func);
        self.var_cache.insert_gv(&data_id, gv.clone())?;
        Ok(gv)
    }
}
