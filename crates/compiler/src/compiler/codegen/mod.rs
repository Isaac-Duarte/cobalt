use std::path::PathBuf;

/**
 * Structures and utilities for converting parsed ASTs into Cranelift IR.
 */
use cranelift::{
    codegen::{
        ir::{AbiParam, InstBuilder},
        verify_function,
    },
    frontend::{FunctionBuilder, FunctionBuilderContext},
};
use cranelift_module::Module;
use cranelift_object::{ObjectBuilder, ObjectModule};
use miette::Result;

use crate::config::BuildConfig;

use self::{
    data::DataManager, func::FuncManager, intrinsics::IntrinsicManager, isa::Isa, translate::FuncTranslator
};

use super::parser::{Ast, Paragraph};

mod data;
mod func;
mod intrinsics;
mod isa;
mod translate;
mod utils;

/// Base code generator state.
pub struct CodeGenerator<'cfg, 'src> {
    /// The global configuration for this build.
    cfg: &'cfg BuildConfig,

    /// The AST this code generator is translating.
    /// This is wrapped in an [`Option`] for ownership purposes, when the translation
    /// is complete this field becomes [`None`].
    ast: Option<Ast<'src>>,

    /// Function builder context, re-used for all function builders within the module.
    builder_ctx: FunctionBuilderContext,

    /// Main Cranelift context. Holds the codegen state, separate from the module.
    ctx: cranelift::codegen::Context,

    /// The main module.
    module: ObjectModule,

    /// Intrinsics manager for this module.
    intrinsics: IntrinsicManager,

    /// Manages object data for this module.
    data_manager: DataManager,

    /// Manages function registration for this module.
    func_manager: FuncManager,
}

impl<'cfg, 'src> CodeGenerator<'cfg, 'src> {
    /// Creates a new code generator based on the given AST.
    pub fn new(cfg: &'cfg BuildConfig, ast: Ast<'src>) -> Result<Self> {
        let isa = Isa::new_from_platform()?.to_cranelift_isa()?;
        let obj_builder = ObjectBuilder::new(
            isa,
            ast.ident_div.program_id,
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        let obj_module = ObjectModule::new(obj_builder);

        Ok(Self {
            cfg,
            ast: Some(ast),
            builder_ctx: FunctionBuilderContext::new(),
            ctx: obj_module.make_context(),
            module: obj_module,
            intrinsics: IntrinsicManager::new(),
            data_manager: DataManager::new(),
            func_manager: FuncManager::new(),
        })
    }

    /// Generates Cranelift IR from the given AST, consuming it.
    pub fn translate(&mut self) -> Result<()> {
        // Process all AST literal/variable data into Cranelift object data.
        let ast = self.ast.take().unwrap();
        self.data_manager.upload(&mut self.module, &ast)?;

        // Define all functions.
        for (idx, para) in ast.proc_div.paragraphs.iter().enumerate() {
            let is_entrypoint = idx == 0;
            self.func_manager
                .create_fn(&mut self.module, para.name.map(|x| x.0), is_entrypoint)?;
        }

        // Translate all functions.
        for para in ast.proc_div.paragraphs.iter() {
            self.translate_fn(para, &ast)?;
        }

        // Create an entrypoint for the program.
        self.translate_entrypoint(&ast)?;

        Ok(())
    }

    /// Generates the program entrypoint, executing paragraphs in order until a terminating
    /// paragraph is encountered.
    fn translate_entrypoint(&mut self, ast: &Ast<'src>) -> Result<()> {
        // Create "main" function for later linking.
        // Returns int, has no parameters.
        let int = self.module.target_config().pointer_type();
        self.ctx.func.signature.returns.push(AbiParam::new(int));

        // Declare the function in the module.
        // Must be "export" for ld.
        let func_id = self
            .module
            .declare_function(
                "main",
                cranelift_module::Linkage::Export,
                &self.ctx.func.signature,
            )
            .expect("Failed to declare function!");

        // Create builder, begin entry block for function.
        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Call each paragraph in turn, until we encounter one which terminates.
            // A terminating paragraph may be called before this, but we can't be 100% sure of that through static analysis alone,
            // at least without some quite annoying processing. Worst case scenario, we generate some useless function calls.
            self.func_manager.clear_refs();
            let mut terminator_found = false;
            for para in ast.proc_div.paragraphs.iter() {
                let para_func_ref = match para.name {
                    Some((name, _)) => {
                        self.func_manager
                            .get_ref(&mut self.module, builder.func, name)?
                    }
                    None => self
                        .func_manager
                        .get_entrypoint_ref(&mut self.module, builder.func)?,
                };
                builder.ins().call(para_func_ref, &[]);

                if para.terminates {
                    terminator_found = true;
                    break;
                }
            }

            // If there was no terminator found, no "STOP RUN" statement is present anywhere in the code.
            if !terminator_found {
                miette::bail!(
                    "No paragraph within the program terminates execution with 'STOP RUN'."
                );
            }

            // Generate a return value (for now, just 0).
            let ret_val = builder.ins().iconst(int, 0x0);
            builder.ins().return_(&[ret_val]);

            // Finish the function.
            builder.finalize();
        }

        // Verify that the function is valid.
        println!("{}", self.ctx.func.display());
        verify_function(&self.ctx.func, self.module.isa())
            .map_err(|err| miette::diagnostic!("codegen: Function verification failed: {}", err))?;

        // Define the function from the built function held in ctx.
        self.module
            .define_function(func_id, &mut self.ctx)
            .map_err(|err| {
                miette::diagnostic!("codegen: Failed to define function body: {}", err)
            })?;

        self.module.clear_context(&mut self.ctx);
        Ok(())
    }

    /// Generates a single function from the AST, given a name & list of statements.
    fn translate_fn(&mut self, paragraph: &Paragraph<'src>, ast: &Ast<'src>) -> Result<()> {
        // Fetch the function to be defined.
        let func_id = match paragraph.name {
            Some((name, _)) => self.func_manager.get_id(name)?,
            None => self.func_manager.get_entrypoint_id()?,
        };

        // Create builder, begin entry block for function.
        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);

            // Switch to block for this function.
            // By "sealing" here, we inform Cranelift there are no predecessors to this block, as it's the
            // entry block.
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Create function translator, translate all statements.
            let mut trans = FuncTranslator::new(
                builder,
                &mut self.module,
                ast,
                &mut self.intrinsics,
                &mut self.data_manager,
                &mut self.func_manager,
            );
            trans.translate(&paragraph.stats)?;

            // If this function terminates, emit a call to `exit()`.
            if paragraph.terminates {
                trans.translate_terminate()?;
            }
            trans.builder.ins().return_(&[]);

            // Finish the function.
            trans.builder.finalize();
        }

        // Verify that the function is valid.
        println!("{}", self.ctx.func.display());
        verify_function(&self.ctx.func, self.module.isa())
            .map_err(|err| miette::diagnostic!("codegen: Function verification failed: {}", err))?;

        // Define the function from the built function held in ctx.
        self.module
            .define_function(func_id, &mut self.ctx)
            .map_err(|err| {
                miette::diagnostic!("codegen: Failed to define function body: {}", err)
            })?;

        self.module.clear_context(&mut self.ctx);
        Ok(())
    }

    /// Converts the generated Cranelift IR to object code, emitting it.
    /// Returns a path to the generated object file, if successful.
    pub fn generate(self) -> Result<PathBuf> {
        // Finish the module, we're all done.
        let out_obj = self.module.finish();

        // Determine the path to the output object file.
        let mut out_path = self.cfg.out_dir.clone();
        out_path.push(format!(
            "{}.o",
            self.cfg.input_file.file_stem().unwrap().to_str().unwrap()
        ));

        // Flush the output object to file.
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(out_path.clone())
            .map_err(|err| {
                miette::diagnostic!("codegen: Failed to create output object file: {}", err)
            })?;
        std::io::Write::write_all(&mut file, &out_obj.emit().unwrap())
            .map_err(|err| miette::diagnostic!("codegen: Object file write failed: {}", err))?;

        Ok(out_path)
    }
}
