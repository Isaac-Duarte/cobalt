use miette::Result;
use cranelift::{codegen::settings::{self, Configurable}, frontend::FunctionBuilderContext};
use cranelift_module::{DataDescription, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use super::parser::Ast;

/// Base code generator state.
pub struct CodeGenerator {
    /// Function builder context, re-used for all function builders within the module.
    builder_ctx: FunctionBuilderContext,

    /// Main Cranelift context. Holds the codegen state, separate from the module.
    ctx: cranelift::codegen::Context,

    /// Data context for functions.
    data_description: DataDescription,

    /// The main module.
    module: ObjectModule
}

impl CodeGenerator {
    pub fn new() -> Result<Self> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap();
        let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();

        let obj_builder = ObjectBuilder::new(isa, "some_name",  cranelift_module::default_libcall_names()).unwrap();
        let obj_module = ObjectModule::new(obj_builder);
        
        Ok(Self {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: obj_module.make_context(),
            data_description: DataDescription::new(),
            module: obj_module
        })
    }

    pub fn generate(self, ast: Ast) {
        let out_obj = self.module.finish();
        println!("{:#?}", out_obj.emit().unwrap());
    }
}