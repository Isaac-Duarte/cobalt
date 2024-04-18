use cranelift::{
    codegen::ir::{immediates::Offset32, GlobalValue},
    prelude::*,
};
use cranelift_module::{DataId, Module};
use miette::Result;

use crate::compiler::parser::{self, IntrinsicCall, Literal, MoveData, MoveSource};

use super::{value::CodegenLiteral, FuncTranslator};

impl<'a, 'src> FuncTranslator<'a, 'src> {
    /// Generates Cranelift IR for a single "MOVE" statement.
    pub(super) fn translate_move(&mut self, mov_data: &MoveData<'src>) -> Result<()> {
        match &mov_data.source {
            MoveSource::Intrinsic(ic_call) => self.translate_move_intrinsic(ic_call, mov_data.dest),
            MoveSource::Value(val) => self.translate_move_value(val, mov_data.dest)
        }
    }

    /// Generates Cranelift IR for a single move instruction resulting from an intrinsic call.
    fn translate_move_intrinsic(&mut self, call: &IntrinsicCall<'src>, dest: &'src str) -> Result<()> {
        // Translate the intrinsic call, load a pointer to the destination.
        let (ret_val, ret_type) = self.translate_intrinsic_call(call)?;
        let dest_ptr = self.load_static_ptr(self.data.sym_data_id(dest)?)?;

        // Determine whether the output type of that intrinsic call is valid for the destination.
        let dest_pic = self.data.sym_pic(dest)?;
        let ptr_type = self.module.target_config().pointer_type();
        if dest_pic.is_float() && ret_type != types::F64
            || dest_pic.is_str() && ret_type != ptr_type
            || !dest_pic.is_float() && !dest_pic.is_str() && ret_type != types::I64 {
            miette::bail!("Invalid destination for the return type of intrinsic function '{}'.", call.name);
        }

        // Perform a store of the value.
        if dest_pic.is_float() || (!dest_pic.is_float() && !dest_pic.is_str()) {
            self.builder
                .ins()
                .store(MemFlags::new(), ret_val, dest_ptr, Offset32::new(0));
        } else {
            miette::bail!("String copy intrinsics are currently unimplemented.");
        }
        Ok(())
    }

    /// Generates Cranelift IR for a single move resulting from a [`parser::Value`].
    fn translate_move_value(&mut self, source: &parser::Value<'src>, dest: &'src str) -> Result<()> {
        // Fetch the destination variable's layout.
        let dest_pic = self.data.sym_pic(dest)?;

        // Move based on the source, either a literal/variable.
        // We also verify here whether the source properly matches the destination.
        match &source {
            parser::Value::Literal(lit) => {
                if !dest_pic.verify_lit(&self.ast.str_lits, lit) {
                    miette::bail!(
                    "Attempted to move incompatible literal '{}' into variable '{}' ({} bytes).",
                    lit.text(&self.ast.str_lits),
                    dest,
                    dest_pic.comp_size()
                );
                }
                self.translate_mov_lit(lit, dest)?;
            }
            parser::Value::Variable(sym) => {
                let source_pic = self.data.sym_pic(sym)?;
                if !source_pic.fits_within_comp(dest_pic) {
                    miette::bail!("Attempted to move incompatible variable '{}' ({} bytes) into variable '{}' ({} bytes).",
                    sym, source_pic.comp_size(), dest, dest_pic.comp_size());
                }
                self.translate_mov_var(sym, dest)?;
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
        // Is the value in cache?
        if let Some(val) = self.values.get_litv(lit) {
            return Ok(val);
        }

        // No, load the value & store it in cache.
        let litv = match lit {
            Literal::String(sid) => self.load_static_ptr(self.data.str_data_id(*sid)?)?,
            Literal::Int(i) => self.builder.ins().iconst(types::I64, *i),
            Literal::Float(f) => self.builder.ins().f64const(*f),
        };
        self.values.insert_litv(lit, litv.clone())?;
        Ok(litv)
    }

    /// Loads the given codegen-only literal into the function as a Cranelift [`Value`].
    /// Utilises cache when available.
    pub(super) fn load_cg_lit(&mut self, cglit: &CodegenLiteral) -> Result<Value> {
        // Is it in cache?
        if let Some(val) = self.values.get_cg_litv(cglit) {
            return Ok(val);
        }

        // No, we still need to load it first.
        let cglitv = match cglit {
            CodegenLiteral::Char(c) => self.builder.ins().iconst(types::I8, (*c as u8) as i64),
        };
        self.values.insert_cg_litv(cglit, cglitv.clone())?;
        Ok(cglitv)
    }

    /// Loads an immutable pointer to the data associated with the given [`DataId`] into the function.
    /// Utilises static value cache when possible.
    pub(super) fn load_static_ptr(&mut self, data_id: DataId) -> Result<Value> {
        // Check whether this pointer is in cache.
        if let Some(sptr) = self.values.get_static_ptr(&data_id) {
            return Ok(sptr);
        }

        // Not in cache, load it up.
        let ptr_type = self.module.target_config().pointer_type();
        let gv = self.load_gv(data_id)?;
        let sptr = self.builder.ins().global_value(ptr_type, gv);
        self.values.insert_static_ptr(&data_id, sptr.clone())?;
        Ok(sptr)
    }

    /// Loads a single [`GlobalValue`] into the current function, caching it if not already present.
    /// If the given data ID has already been loaded, returns the GV from cache.
    fn load_gv(&mut self, data_id: DataId) -> Result<GlobalValue> {
        if let Some(gv) = self.values.get_gv(&data_id) {
            return Ok(gv);
        }
        let gv = self.module.declare_data_in_func(data_id, self.builder.func);
        self.values.insert_gv(&data_id, gv.clone())?;
        Ok(gv)
    }
}
