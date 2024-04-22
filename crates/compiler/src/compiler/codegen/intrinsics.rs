use std::collections::HashMap;

use cranelift::codegen::ir::{types, AbiParam, FuncRef, Function, Signature};
use cranelift_module::{FuncId, Module};
use cranelift_object::ObjectModule;
use miette::Result;

/// Manages importing Cobalt intrinsics (libc functions, etc.) into generated output modules/functions.
/// Must be called at the function translate stage, and have its held [`FuncRef`] cache reset after each function
/// using [`IntrinsicManager::clear_refs()`].
pub(super) struct IntrinsicManager {
    /// Module-level function imports for Cobalt intrinsics.
    funcs: HashMap<CobaltIntrinsic, FuncId>,

    /// Function-level references for Cobalt intrinsics.
    /// Must be reset per-function with [`IntrinsicManager::clear_refs()`].
    refs: HashMap<CobaltIntrinsic, FuncRef>,
}

/// A comprehensive list of all available intrinsics in Cobalt.
/// Those prepended with "Libc" are from the libc dependency.
/// All others have sources within the [`cobalt_intrinsics`] crate.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum CobaltIntrinsic {
    LibcPutchar, // int putc(char)
    LibcExit,    // void exit(int)
    PrintStr,    // void cb_print_str(char*)
    PrintFloat,  // void cb_print_f64(f64)
    PrintInt,    // void cb_print_i64(i64)
    StrCmp,      // i8 cb_strcmp(char*, char*)
    StrCpy,      // void cb_strcpy(char*, char*, i64, i64, i64, i64, i64, i64)
    ReadStr,     // void cb_readstr(char*, usize)
    ReadInt,     // i64 cb_readint()
    ReadFloat,   // f64 cb_readfloat()
    Mod,         // i64 cb_mod(i64, i64)
    Length,      // i64 cb_length(char*)
}

impl IntrinsicManager {
    /// Creates a new intrinsics manager.
    pub fn new() -> Self {
        Self {
            funcs: HashMap::new(),
            refs: HashMap::new(),
        }
    }

    /// Resolves the given COBOL intrinsic name into a known [`CobaltIntrinsic`].
    /// On failure to resolve an intrinsic with the given name, returns an error.
    pub fn resolve_name(&self, name: &str) -> Result<CobaltIntrinsic> {
        let intrinsic = match name {
            "MOD" => CobaltIntrinsic::Mod,
            "LENGTH" => CobaltIntrinsic::Length,
            unk @ _ => {
                miette::bail!(
                    "Unknown/unimplemented intrinsic '{}' was attempted to be resolved.",
                    unk
                );
            }
        };
        Ok(intrinsic)
    }

    /// Returns a function reference for the given Cobalt intrinsic.
    /// If one does not exist, creates one. Should only be called from within function translation.
    pub fn get_ref(
        &mut self,
        module: &mut ObjectModule,
        func: &mut Function,
        i: CobaltIntrinsic,
    ) -> Result<FuncRef> {
        // If we've used this intrinsic in the current function before, return the existing ref.
        if self.refs.contains_key(&i) {
            return Ok(*self.refs.get(&i).unwrap());
        }

        // If we haven't imported the intrinsic into the module yet, do that.
        let func_id = if !self.funcs.contains_key(&i) {
            self.import_intrinsic(module, i)?
        } else {
            self.funcs.get(&i).unwrap()
        };

        // Create a new reference for this function.
        let func_ref = module.declare_func_in_func(*func_id, func);
        self.refs.insert(i, func_ref);
        Ok(*self.refs.get(&i).unwrap())
    }

    /// Clears existing function references remaining in the intrinsic manager.
    pub fn clear_refs(&mut self) {
        self.refs.clear();
    }

    /// Fetches the function signature for the given [`CobaltIntrinsic`].
    pub fn get_signature(&mut self, module: &mut ObjectModule, i: CobaltIntrinsic) -> Signature {
        let mut sig = module.make_signature();
        match i {
            CobaltIntrinsic::LibcPutchar => libcputchar_sig(&mut sig),
            CobaltIntrinsic::LibcExit => libcexit_sig(&mut sig, module),
            CobaltIntrinsic::PrintStr => printstr_sig(&mut sig, module),
            CobaltIntrinsic::PrintFloat => printfloat_sig(&mut sig),
            CobaltIntrinsic::PrintInt => printint_sig(&mut sig),
            CobaltIntrinsic::StrCmp => strcmp_sig(&mut sig, module),
            CobaltIntrinsic::StrCpy => strcpy_sig(&mut sig, module),
            CobaltIntrinsic::ReadStr => readstr_sig(&mut sig, module),
            CobaltIntrinsic::ReadInt => readint_sig(&mut sig),
            CobaltIntrinsic::ReadFloat => readfloat_sig(&mut sig),
            CobaltIntrinsic::Mod => mod_sig(&mut sig),
            CobaltIntrinsic::Length => length_sig(&mut sig, module),
        };
        sig
    }

    /// Attempts to import the given intrinsic into the current module.
    fn import_intrinsic(
        &mut self,
        module: &mut ObjectModule,
        i: CobaltIntrinsic,
    ) -> Result<&FuncId> {
        // Sanity check.
        assert!(!self.funcs.contains_key(&i));

        // Get the function signature, name.
        let sig = self.get_signature(module, i);
        let name = match i {
            CobaltIntrinsic::LibcPutchar => "putchar",
            CobaltIntrinsic::LibcExit => "exit",
            CobaltIntrinsic::PrintStr => "cb_print_str",
            CobaltIntrinsic::PrintFloat => "cb_print_f64",
            CobaltIntrinsic::PrintInt => "cb_print_i64",
            CobaltIntrinsic::StrCmp => "cb_strcmp",
            CobaltIntrinsic::StrCpy => "cb_strcpy",
            CobaltIntrinsic::ReadStr => "cb_readstr",
            CobaltIntrinsic::ReadInt => "cb_readint",
            CobaltIntrinsic::ReadFloat => "cb_readfloat",
            CobaltIntrinsic::Mod => "cb_mod",
            CobaltIntrinsic::Length => "cb_length",
        };

        // Import it.
        let func_id = module
            .declare_function(name, cranelift_module::Linkage::Import, &sig)
            .map_err(|err| miette::diagnostic!("codegen: Failed to import intrinsic: {}", err))?;

        // Save this to the hashmap, return.
        self.funcs.insert(i, func_id);
        Ok(self.funcs.get(&i).unwrap())
    }
}

/// Generates a function signature for [`CobaltIntrinsic::LibcPutchar`].
fn libcputchar_sig(sig: &mut Signature) {
    sig.params.push(AbiParam::new(types::I8));
    sig.returns.push(AbiParam::new(types::I32));
}

/// Generates a function signature for [`CobaltIntrinsic::LibcExit`].
fn libcexit_sig(sig: &mut Signature, module: &mut ObjectModule) {
    let ptr_type = module.target_config().pointer_type();
    sig.params.push(AbiParam::new(ptr_type));
}

/// Generates a function signature for [`CobaltIntrinsic::PrintStr`].
fn printstr_sig(sig: &mut Signature, module: &mut ObjectModule) {
    let ptr_type = module.target_config().pointer_type();
    sig.params.push(AbiParam::new(ptr_type));
}

/// Generates a function signature for [`CobaltIntrinsic::PrintFloat`].
fn printfloat_sig(sig: &mut Signature) {
    sig.params.push(AbiParam::new(types::F64));
}

/// Generates a function signature for [`CobaltIntrinsic::PrintInt`].
fn printint_sig(sig: &mut Signature) {
    sig.params.push(AbiParam::new(types::I64));
}

/// Generates a function signature for [`CobaltIntrinsic::StrCmp`].
fn strcmp_sig(sig: &mut Signature, module: &mut ObjectModule) {
    let ptr_type = module.target_config().pointer_type();
    sig.params.push(AbiParam::new(ptr_type));
    sig.params.push(AbiParam::new(ptr_type));
    sig.returns.push(AbiParam::new(types::I8));
}

/// Generates a function signature for [`CobaltIntrinsic::StrCpy`].
fn strcpy_sig(sig: &mut Signature, module: &mut ObjectModule) {
    let ptr_type = module.target_config().pointer_type();
    sig.params.push(AbiParam::new(ptr_type)); // src_str
    sig.params.push(AbiParam::new(ptr_type)); // dest_str

    // src_len, dest_len, src_span_idx, src_span_len, dest_span_idx, dest_span_len
    for _ in 0..6 {
        sig.params.push(AbiParam::new(types::I64));
    }
}

/// Generates a function signature for [`CobaltIntrinsic::ReadStr`].
fn readstr_sig(sig: &mut Signature, module: &mut ObjectModule) {
    let ptr_type = module.target_config().pointer_type();
    sig.params.push(AbiParam::new(ptr_type));
    sig.params.push(AbiParam::new(ptr_type));
}

/// Generates a function signature for [`CobaltIntrinsic::ReadInt`].
fn readint_sig(sig: &mut Signature) {
    sig.returns.push(AbiParam::new(types::I64));
}

/// Generates a function signature for [`CobaltIntrinsic::ReadFloat`].
fn readfloat_sig(sig: &mut Signature) {
    sig.returns.push(AbiParam::new(types::F64));
}

/// Generates a function signature for [`CobaltIntrinsic::Mod`].
fn mod_sig(sig: &mut Signature) {
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));
}

/// Generates a function signature for [`CobaltIntrinsic::Length`].
fn length_sig(sig: &mut Signature, module: &mut ObjectModule) {
    let ptr_type = module.target_config().pointer_type();
    sig.params.push(AbiParam::new(ptr_type));
    sig.returns.push(AbiParam::new(types::I64));
}
