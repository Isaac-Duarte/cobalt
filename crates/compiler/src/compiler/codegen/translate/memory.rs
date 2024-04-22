use cranelift::{
    codegen::ir::{immediates::Offset32, GlobalValue},
    prelude::*,
};
use cranelift_module::{DataId, Module};
use miette::Result;

use crate::compiler::parser::{self, IntrinsicCall, Literal, MoveData, MoveRef, MoveSource, Pic};

use super::{value::CodegenLiteral, FuncTranslator};

impl<'a, 'src> FuncTranslator<'a, 'src> {
    /// Generates Cranelift IR for a single "MOVE" statement.
    pub(super) fn translate_move(&mut self, mov_data: &MoveData<'src>) -> Result<()> {
        match &mov_data.source {
            MoveSource::Intrinsic(ic_call) => self.translate_move_intrinsic(ic_call, &mov_data.dest),
            MoveSource::Literal(lit) => self.translate_mov_lit(lit, &mov_data.dest),
            MoveSource::MoveRef(mov_ref) => self.translate_mov_ref(mov_ref, &mov_data.dest),
        }
    }

    /// Generates Cranelift IR for a single move instruction resulting from an intrinsic call.
    fn translate_move_intrinsic(&mut self, call: &IntrinsicCall<'src>, dest: &MoveRef<'src>) -> Result<()> {
        // Translate the intrinsic call, load a pointer to the destination.
        let (ret_val, ret_type) = self.translate_intrinsic_call(call)?;
        let dest_ptr = self.load_static_ptr(self.data.sym_data_id(&dest.sym)?)?;

        // Determine whether the output type of that intrinsic call is valid for the destination.
        let dest_pic = self.data.sym_pic(&dest.sym)?;
        let ptr_type = self.module.target_config().pointer_type();
        if dest_pic.is_float() && ret_type != types::F64
            || dest_pic.is_str() && ret_type != ptr_type
            || !dest_pic.is_float() && !dest_pic.is_str() && ret_type != types::I64 {
            miette::bail!("Invalid destination for the return type of intrinsic function '{}'.", call.name);
        }

        // Verify the destination reference is valid.
        dest.validate(dest_pic)?;

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

    /// Moves the given literal into the provided global data slot.
    pub(super) fn translate_mov_lit(&mut self, lit: &Literal, dest: &MoveRef<'src>) -> Result<()> {
        let dest_id = self.data.sym_data_id(&dest.sym)?;

        // Import the destination variable into the function, get a pointer to it.
        let ptr_type = self.module.target_config().pointer_type();
        let dest_ptr = self.load_static_ptr(dest_id)?;
        let src_val = self.load_lit(lit)?;

        // Load the relevant PIC, verify the destination is valid.
        let dest_pic = self.data.sym_pic(&dest.sym)?;
        dest.validate(dest_pic)?;

        // Verify the source literal actually fits within the destination.
        if !dest_pic.verify_lit(&self.ast.str_lits, lit) {
            miette::bail!(
            "Attempted to move incompatible literal '{}' into variable '{}' ({} bytes).",
            lit.text(&self.ast.str_lits),
            &dest.sym,
            dest_pic.comp_size()
        );
        }

        // If the destination has a span, verify that the source is a string, and the
        // destination span can fit the literal being passed.
        if let Some(span) = &dest.span {
            if let Literal::String(sid) = lit {
                let lit_str = self.ast.str_lits.get(*sid).ok_or(miette::diagnostic!(
                    "Failed to fetch string data for literal ID '{}'.",
                    sid
                ))?;
                if lit_str.len() > span.len {
                    miette::bail!("Attempted to move overly large string literal ({} bytes) into span {}({}:{}) ({} bytes).", lit_str.len(), dest.sym, span.start_idx, span.len, span.len);
                }
            } else {
                // This should have been checked by Pic::verify_lit().
                unreachable!();
            }
        }

        match lit {
            Literal::Int(_) | Literal::Float(_) => {
                self.builder
                    .ins()
                    .store(MemFlags::new(), src_val, dest_ptr, Offset32::new(0));
            }
            Literal::String(sid) => {
                // Adjust the destination pointer if a span is present.
                let mut dest_ptr = dest_ptr;
                if let Some(span) = &dest.span {
                    dest_ptr = self.builder.ins().iadd_imm(dest_ptr, span.start_idx as i64);
                }

                // Get the size of the string to copy.
                let src_size = self
                    .ast
                    .str_lits
                    .get(*sid)
                    .ok_or(miette::diagnostic!(
                        "Failed to fetch string data for literal ID '{}'.",
                        sid
                    ))?
                    .len()
                    + 1;
                let span_len = dest.span.as_ref().map_or(src_size, |s| s.len);
                let size_val = self.builder.ins().iconst(ptr_type, span_len as i64);

                // Sanity check.
                assert!(span_len <= dest_pic.comp_size());

                // Perform a straight memory copy of the value.
                self.builder
                    .call_memcpy(self.module.target_config(), dest_ptr, src_val, size_val);
            }
        }
        Ok(())
    }

    /// Moves the given global variable into the provided global data slot.
    fn translate_mov_ref(&mut self, src: &MoveRef<'src>, dest: &MoveRef<'src>) -> Result<()> {
        // Import both variables as global values, get pointers to them.
        let ptr_type = self.module.target_config().pointer_type();
        let (src_id, dest_id) = (self.data.sym_data_id(&src.sym)?, self.data.sym_data_id(&dest.sym)?);
        let (src_ptr, dest_ptr) = (
            self.load_static_ptr(src_id)?,
            self.load_static_ptr(dest_id)?,
        );

        // Load the PIC for the source/destination.
        let (src_pic, dest_pic) = (self.data.sym_pic(&src.sym)?, self.data.sym_pic(&dest.sym)?);

        // Verify that the references are valid, source fits within the destination.
        src.validate(src_pic)?;
        dest.validate(dest_pic)?;
        src.fits_within(dest, src_pic, dest_pic)?;

        // Based on the source type, determine the copy mechanism.
        if src_pic.is_str() {
            // Adjust the source, destination pointer if there is a span.
            let (mut src_ptr, mut dest_ptr) = (src_ptr, dest_ptr);
            let (mut src_len, mut dest_len) = (src_pic.comp_size(), dest_pic.comp_size());
            if let Some(span) = &src.span {
                src_ptr = self.builder.ins().iadd_imm(src_ptr, span.start_idx as i64);
                src_len = span.len;
            }
            if let Some(span) = &dest.span {
                dest_ptr = self.builder.ins().iadd_imm(dest_ptr, span.start_idx as i64);
                dest_len = span.len;
            }

            // Determine the size to copy from the source.
            // If there is a span, we utilise this.
            let size = std::cmp::min(src_len, dest_len);
            let size_val = self
                .builder
                .ins()
                .iconst(ptr_type, size as i64);

            // Sanity check.
            assert!(size <= dest_pic.comp_size());

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

/// Additional functionality for [`MoveRef`] to allow for checking size bounds.
impl<'src> MoveRef<'src> {
    /// Validates whether this move reference is valid against the given [`Pic`].
    pub(crate) fn validate(&self, pic: &Pic) -> Result<()> {
        if let Some(span) = &self.span {
            if !pic.is_str() {
                miette::bail!("Cannot reference a span within non-string variable {}.", self.sym);
            }
            // We require the -1 here to remove the null terminator byte.
            let pic_str_len = pic.comp_size() - 1;
            if span.start_idx >= pic_str_len {
                miette::bail!("Start index for span ({}:{}) within variable {} is out of bounds (>={}).", span.start_idx, span.len, self.sym, pic_str_len);
            }
            if span.start_idx + span.len >= pic_str_len {
                miette::bail!("Span ({}:{}) within variable {} ends out of bounds (>={}).", span.start_idx, span.len, self.sym, pic_str_len);
            }
        }
        Ok(())
    }

    /// Tests whether this [`MoveRef`] will fit, when copied, within the provided [`MoveRef`].
    /// Requires the provision of [`Pic`] structures for both this reference and the provided reference.
    pub(crate) fn fits_within(&self, other: &MoveRef<'src>, self_pic: &Pic, other_pic: &Pic) -> Result<()> {
        match &self.span {
            Some(span) => {
                let self_len = span.len;
                let other_len = match &other.span {
                    Some(s) => s.len,
                    // We require the -1 here to remove the null terminator byte.
                    None => other_pic.comp_size() - 1
                };

                if self_len > other_len {
                    miette::bail!("Attempted to move overly large span {}({}:{}) into smaller destination within {}.", self.sym, span.start_idx, span.len, other.sym);
                }
            },
            None => {
                if !self_pic.fits_within_comp(other_pic) {
                    miette::bail!("Attempted to move incompatible variable '{}' ({} bytes) into variable '{}' ({} bytes).",
                    self.sym, self_pic.comp_size(), other.sym, other_pic.comp_size());
                }
            }
        }
        Ok(())
    }
}