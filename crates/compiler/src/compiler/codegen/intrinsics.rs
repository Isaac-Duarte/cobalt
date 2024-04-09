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
    PrintStr,    // void cb_print_str(char*)
    PrintFloat,  // void cb_print_f64(f64)
    PrintInt,    // void cb_print_i64(i64)
    StrCmp,      // i64 cb_strcmp(char*, char*)
}

impl IntrinsicManager {
    /// Creates a new intrinsics manager.
    pub fn new() -> Self {
        Self {
            funcs: HashMap::new(),
            refs: HashMap::new(),
        }
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

    /// Attempts to import the given intrinsic into the current module.
    fn import_intrinsic(
        &mut self,
        module: &mut ObjectModule,
        i: CobaltIntrinsic,
    ) -> Result<&FuncId> {
        // Sanity check.
        assert!(!self.funcs.contains_key(&i));

        // Get the function signature, name.
        let mut sig = module.make_signature();
        match i {
            CobaltIntrinsic::LibcPutchar => libcputchar_sig(&mut sig),
            CobaltIntrinsic::PrintStr => printstr_sig(&mut sig, module),
            CobaltIntrinsic::PrintFloat => printfloat_sig(&mut sig),
            CobaltIntrinsic::PrintInt => printint_sig(&mut sig),
            CobaltIntrinsic::StrCmp => strcmp_sig(&mut sig, module),
        };
        let name = match i {
            CobaltIntrinsic::LibcPutchar => "putchar",
            CobaltIntrinsic::PrintStr => "cb_print_str",
            CobaltIntrinsic::PrintFloat => "cb_print_f64",
            CobaltIntrinsic::PrintInt => "cb_print_i64",
            CobaltIntrinsic::StrCmp => "cb_strcmp",
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
    sig.returns.push(AbiParam::new(types::I64));
}