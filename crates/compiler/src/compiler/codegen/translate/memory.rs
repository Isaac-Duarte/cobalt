use cranelift::{
    codegen::ir::{immediates::Offset32, GlobalValue},
    prelude::*,
};
use cranelift_module::{DataId, Module};
use miette::Result;

use crate::compiler::{
    codegen::{data::DataManager, intrinsics::CobaltIntrinsic},
    parser::{self, IntrinsicCall, Literal, MoveData, MoveRef, MoveSource, MoveSpan, Pic},
};

use super::{value::CodegenLiteral, FuncTranslator};

impl<'a, 'src> FuncTranslator<'a, 'src> {
    /// Generates Cranelift IR for a single "MOVE" statement.
    pub(super) fn translate_move(&mut self, mov_data: &MoveData<'src>) -> Result<()> {
        match &mov_data.source {
            MoveSource::Intrinsic(ic_call) => {
                self.translate_move_intrinsic(ic_call, &mov_data.dest)
            }
            MoveSource::Literal(lit) => self.translate_mov_lit(lit, &mov_data.dest),
            MoveSource::MoveRef(mov_ref) => self.translate_mov_ref(mov_ref, &mov_data.dest),
        }
    }

    /// Generates Cranelift IR for a single move instruction resulting from an intrinsic call.
    fn translate_move_intrinsic(
        &mut self,
        call: &IntrinsicCall<'src>,
        dest: &MoveRef<'src>,
    ) -> Result<()> {
        // Translate the intrinsic call, load a pointer to the destination.
        let (ret_val, ret_type) = self.translate_intrinsic_call(call)?;
        let dest_ptr = self.load_static_ptr(self.data.sym_data_id(&dest.sym)?)?;

        // Determine whether the output type of that intrinsic call is valid for the destination.
        let dest_pic = self.data.sym_pic(&dest.sym)?;
        let ptr_type = self.module.target_config().pointer_type();
        if dest_pic.is_float() && ret_type != types::F64
            || dest_pic.is_str() && ret_type != ptr_type
            || !dest_pic.is_float() && !dest_pic.is_str() && ret_type != types::I64
        {
            miette::bail!(
                "Invalid destination for the return type of intrinsic function '{}'.",
                call.name
            );
        }

        // Verify the destination reference is valid.
        dest.validate(dest_pic, &self.data)?;

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
        let dest_pic = self.data.sym_pic(&dest.sym)?.clone();
        dest.validate(&dest_pic, &self.data)?;

        // Verify the source literal actually fits within the destination.
        if !dest_pic.verify_lit(&self.ast.str_lits, lit) {
            miette::bail!(
                "Attempted to move incompatible literal '{}' into variable '{}' ({} bytes).",
                lit.text(&self.ast.str_lits),
                &dest.sym,
                dest_pic.comp_size()
            );
        }

        match lit {
            Literal::Int(_) | Literal::Float(_) => {
                self.builder
                    .ins()
                    .store(MemFlags::new(), src_val, dest_ptr, Offset32::new(0));
            }
            Literal::String(sid) => {
                // Get the size of the string to copy.
                let src_len = self
                    .ast
                    .str_lits
                    .get(*sid)
                    .ok_or(miette::diagnostic!(
                        "Failed to fetch string data for literal ID '{}'.",
                        sid
                    ))?
                    .len();

                // Get total possible length for destination string.
                // We take one here to remove the null terminator.
                let dest_len = dest_pic.comp_size() - 1;

                // Check if there's a span, if there's no span we can just use a standard memcpy.
                if let Some(span) = &dest.span {
                    // Load the requisite arguments for the strcpy intrinsic.
                    let src_span_idx = self.builder.ins().iconst(types::I64, 0);
                    let src_span_len = self.builder.ins().iconst(types::I64, src_len as i64);
                    let (dest_span_idx, dest_span_len) = self.load_span(&dest_pic, Some(span))?;
                    let dest_len_val = self.builder.ins().iconst(types::I64, dest_len as i64);

                    // Call the intrinsic.
                    let strcpy_ref = self.intrinsics.get_ref(
                        self.module,
                        self.builder.func,
                        CobaltIntrinsic::StrCpy,
                    )?;
                    self.builder.ins().call(
                        strcpy_ref,
                        &[
                            src_val,
                            dest_ptr,
                            src_span_len,
                            dest_len_val,
                            src_span_idx,
                            src_span_len,
                            dest_span_idx,
                            dest_span_len,
                        ],
                    );
                } else {
                    // No span found, just use a simple memcpy.
                    let src_size = src_len + 1; // +1 for the null terminator
                    println!("{}", src_size);
                    let size_val = self.builder.ins().iconst(ptr_type, src_size as i64);

                    // Sanity check.
                    assert!(src_size <= dest_pic.comp_size());
                    self.builder.call_memcpy(
                        self.module.target_config(),
                        dest_ptr,
                        src_val,
                        size_val,
                    );
                }
            }
        }
        Ok(())
    }

    /// Moves the given global variable into the provided global data slot.
    fn translate_mov_ref(&mut self, src: &MoveRef<'src>, dest: &MoveRef<'src>) -> Result<()> {
        // Import both variables as global values, get pointers to them.
        let ptr_type = self.module.target_config().pointer_type();
        let (src_id, dest_id) = (
            self.data.sym_data_id(&src.sym)?,
            self.data.sym_data_id(&dest.sym)?,
        );
        let (src_ptr, dest_ptr) = (
            self.load_static_ptr(src_id)?,
            self.load_static_ptr(dest_id)?,
        );

        // Load the PIC for the source/destination.
        let (src_pic, dest_pic) = (
            self.data.sym_pic(&src.sym)?.clone(),
            self.data.sym_pic(&dest.sym)?.clone(),
        );

        // Verify that the references are valid.
        src.validate(&src_pic, &self.data)?;
        dest.validate(&dest_pic, &self.data)?;

        // If neither variable contains a span, we can statically verify that the move is valid by checking their PIC layouts.
        if src.span.is_none() && dest.span.is_none() {
            if !src_pic.fits_within_comp(&dest_pic) {
                miette::bail!("Attempted to move incompatible variable '{}' ({} bytes) into variable '{}' ({} bytes).",
                src.sym, src_pic.comp_size(), dest.sym, dest_pic.comp_size());
            }
        }

        // Based on the source type, determine the copy mechanism.
        if src_pic.is_str() {
            if src.span.is_some() || dest.span.is_some() {
                // Spans specified, we will need to use the Cobalt `strcpy` intrinsic.
                // Get the span to copy from the source, destination.
                let (src_span_idx, src_span_len) = self.load_span(&src_pic, src.span.as_ref())?;
                let (dest_span_idx, dest_span_len) =
                    self.load_span(&dest_pic, dest.span.as_ref())?;

                // Load the length of both strings for the intrinsic.
                let src_len = self
                    .builder
                    .ins()
                    .iconst(types::I64, src_pic.comp_size() as i64);
                let dest_len = self
                    .builder
                    .ins()
                    .iconst(types::I64, dest_pic.comp_size() as i64);

                // Call the intrinsic.
                let strcpy_ref = self.intrinsics.get_ref(
                    self.module,
                    self.builder.func,
                    CobaltIntrinsic::StrCpy,
                )?;
                self.builder.ins().call(
                    strcpy_ref,
                    &[
                        src_ptr,
                        dest_ptr,
                        src_len,
                        dest_len,
                        src_span_idx,
                        src_span_len,
                        dest_span_idx,
                        dest_span_len,
                    ],
                );
            } else {
                // No spans specified, a simple copy is fine.
                let size_val = self
                    .builder
                    .ins()
                    .iconst(ptr_type, src_pic.comp_size() as i64);

                // Sanity check.
                assert!(src_pic.comp_size() <= dest_pic.comp_size());

                // Perform a memcpy().
                self.builder
                    .call_memcpy(self.module.target_config(), dest_ptr, src_ptr, size_val);
            }
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

    /// Loads a single [`MoveSpan`] for the given variable into the function.
    /// If no span is provided, creates a default span targeting the entire string.
    fn load_span(&mut self, pic: &Pic, span: Option<&MoveSpan<'src>>) -> Result<(Value, Value)> {
        let ptr_type = self.module.target_config().pointer_type();
        if let Some(span) = &span {
            let idx = self.load_value(&span.start_idx)?;
            let len = if let Some(len) = &span.len {
                self.load_value(len)?
            } else {
                let total_len = self
                    .builder
                    .ins()
                    .iconst(types::I64, pic.comp_size() as i64);
                self.builder.ins().isub(total_len, idx)
            };
            Ok((idx, len))
        } else {
            // No span specified for source, use whole string.
            let idx = self.builder.ins().iconst(ptr_type, 0);
            let len = self.builder.ins().iconst(ptr_type, pic.comp_size() as i64);
            Ok((idx, len))
        }
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
    fn validate(&self, pic: &Pic, data: &DataManager) -> Result<()> {
        if let Some(span) = &self.span {
            if !pic.is_str() {
                miette::bail!(
                    "Cannot reference a span within non-string variable {}.",
                    self.sym
                );
            }

            // Verify the elements of the span are both integers.
            if span.start_idx.is_float(data)? || span.start_idx.is_str(data)? {
                miette::bail!("Value of span start index must be of type integer.");
            }
            if let Some(len) = &span.len {
                if len.is_float(data)? || len.is_str(data)? {
                    miette::bail!("Value of span length must be of type integer.");
                }
            }
        }
        Ok(())
    }
}
