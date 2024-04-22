use cranelift::codegen::ir::{types, InstBuilder, Type, Value};
use cranelift_module::Module;
use miette::Result;
use crate::compiler::parser::IntrinsicCall;

use super::FuncTranslator;

impl<'a, 'src> FuncTranslator<'a, 'src> {
    /// Translates the given [`IntrinsicCall`], returning the produced value.
    /// Support for string-based intrinsics is not yet implemented.
    pub(super) fn translate_intrinsic_call(&mut self, call: &IntrinsicCall<'src>) -> Result<(Value, Type)> {
        // Fetch this intrinsic, related signature from the manager.
        let intrinsic = self.intrinsics.resolve_name(call.name)?;
        let sig = self.intrinsics.get_signature(self.module, intrinsic);
        
        // Verify that the call arguments match the signature.
        if call.args.len() != sig.params.len() {
            miette::bail!("Number of arguments to intrinsic function '{}' do not match signature ({} instead of {}).", call.name, call.args.len(), sig.params.len());
        }
        let ptr_type = self.module.target_config().pointer_type();
        for (idx, (val, param)) in call.args.iter().zip(sig.params.iter()).enumerate() {
            let (is_str, is_float) = (val.is_str(self.data)?, val.is_float(self.data)?);
            if is_str && param.value_type != ptr_type
              || is_float && param.value_type != types::F64
              || !is_str && !is_float && param.value_type != types::I64 {
                miette::bail!("Mismatched argument type for argument {} in intrinsic function '{}'.", idx, call.name);
            }
        }

        // Prepare the call.
        let arg_vals = call.args.iter().map(|arg| self.load_value(arg)).collect::<Result<Vec<_>>>()?;
        let intrinsic_ref = self.intrinsics.get_ref(self.module, self.builder.func, intrinsic)?;
        let inst = self.builder.ins().call(intrinsic_ref, arg_vals.as_slice());

        // Fetch the results of that instruction to return.
        let result = *self.builder.inst_results(inst).first().unwrap();
        Ok((result, sig.returns.first().unwrap().value_type))
    }
}