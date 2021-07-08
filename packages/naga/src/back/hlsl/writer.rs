//TODO: temp
#![allow(dead_code)]
use super::{Error, Options};
use crate::{
    back,
    proc::{self, NameKey},
    valid, Handle, Module, ShaderStage, TypeInner,
};
use std::fmt::Write;

const LOCATION_SEMANTIC: &str = "LOC";

/// Shorthand result used internally by the backend
type BackendResult = Result<(), Error>;

/// Structure contains information required for generating
/// wrapped structure of all entry points arguments
struct EntryPointBinding {
    /// Associated shader stage
    stage: ShaderStage,
    /// Generated structure name
    name: String,
    /// Members of generated structure
    members: Vec<EpStructMember>,
}

struct EpStructMember {
    pub name: String,
    pub ty: Handle<crate::Type>,
    pub binding: Option<crate::Binding>,
}

pub struct Writer<'a, W> {
    out: W,
    names: crate::FastHashMap<NameKey, String>,
    namer: proc::Namer,
    /// HLSL backend options
    options: &'a Options,
    /// Information about entry point arguments wrapped into structure
    ep_inputs: Vec<Option<EntryPointBinding>>,
    /// Set of expressions that have associated temporary variables
    named_expressions: crate::NamedExpressions,
}

impl<'a, W: Write> Writer<'a, W> {
    pub fn new(out: W, options: &'a Options) -> Self {
        Self {
            out,
            names: crate::FastHashMap::default(),
            namer: proc::Namer::default(),
            options,
            ep_inputs: Vec::new(),
            named_expressions: crate::NamedExpressions::default(),
        }
    }

    fn reset(&mut self, module: &Module) {
        self.names.clear();
        self.namer
            .reset(module, super::keywords::RESERVED, &[], &mut self.names);
        self.named_expressions.clear();
        self.ep_inputs.clear();
    }

    pub fn write(
        &mut self,
        module: &Module,
        info: &valid::ModuleInfo,
    ) -> Result<super::ReflectionInfo, Error> {
        self.reset(module);

        // Write all constants
        // For example, input wgsl shader:
        // ```wgsl
        // let c_scale: f32 = 1.2;
        // return VertexOutput(uv, vec4<f32>(c_scale * pos, 0.0, 1.0));
        // ```
        //
        // Output shader:
        // ```hlsl
        // static const float c_scale = 1.2;
        // const VertexOutput vertexoutput1 = { vertexinput.uv3, float4((c_scale * vertexinput.pos1), 0.0, 1.0) };
        // ```
        //
        // If we remove `write_global_constant` `c_scale` will be inlined.
        for (handle, constant) in module.constants.iter() {
            if constant.name.is_some() {
                self.write_global_constant(module, &constant.inner, handle)?;
            }
        }

        // Save all entry point output types
        let ep_results = module
            .entry_points
            .iter()
            .map(|ep| (ep.stage, ep.function.result.clone()))
            .collect::<Vec<(ShaderStage, Option<crate::FunctionResult>)>>();

        // Write all structs
        for (handle, ty) in module.types.iter() {
            if let crate::TypeInner::Struct {
                top_level,
                ref members,
                ..
            } = ty.inner
            {
                let ep_result = ep_results.iter().find(|e| {
                    if let Some(ref result) = e.1 {
                        result.ty == handle
                    } else {
                        false
                    }
                });

                if let Some(result) = ep_result {
                    self.write_struct(
                        module,
                        handle,
                        top_level,
                        members,
                        Some(result.0),
                        Some(true),
                    )?;
                } else {
                    self.write_struct(module, handle, top_level, members, None, None)?;
                }
                writeln!(self.out)?;
            }
        }

        // Write all globals
        for (ty, _) in module.global_variables.iter() {
            self.write_global(module, ty)?;
        }

        if !module.global_variables.is_empty() {
            // Add extra newline for readability
            writeln!(self.out)?;
        }

        // Write all entry points wrapped structs
        for ep in module.entry_points.iter() {
            self.write_ep_input_struct(module, &ep.function, ep.stage, &ep.name)?;
        }

        // Write all regular functions
        for (handle, function) in module.functions.iter() {
            let info = &info[handle];
            let ctx = back::FunctionCtx {
                ty: back::FunctionType::Function(handle),
                info,
                expressions: &function.expressions,
                named_expressions: &function.named_expressions,
            };
            let name = self.names[&NameKey::Function(handle)].clone();

            self.write_function(module, name.as_str(), function, &ctx)?;

            writeln!(self.out)?;
        }

        let mut entry_points_info = Vec::with_capacity(module.entry_points.len());

        // Write all entry points
        for (index, ep) in module.entry_points.iter().enumerate() {
            let ctx = back::FunctionCtx {
                ty: back::FunctionType::EntryPoint(index as u16),
                info: info.get_entry_point(index),
                expressions: &ep.function.expressions,
                named_expressions: &ep.function.named_expressions,
            };

            if ep.stage == ShaderStage::Compute {
                // HLSL is calling workgroup size, num threads
                let num_threads = ep.workgroup_size;
                writeln!(
                    self.out,
                    "[numthreads({}, {}, {})]",
                    num_threads[0], num_threads[1], num_threads[2]
                )?;
            }

            let name = self.names[&NameKey::EntryPoint(index as u16)].clone();

            self.write_function(module, &name, &ep.function, &ctx)?;

            if index < module.entry_points.len() - 1 {
                writeln!(self.out)?;
            }

            entry_points_info.push(name);
        }

        Ok(super::ReflectionInfo {
            entry_points: entry_points_info,
        })
    }

    fn write_semantic(
        &mut self,
        binding: &crate::Binding,
        stage: Option<ShaderStage>,
        output: Option<bool>,
    ) -> BackendResult {
        match *binding {
            crate::Binding::BuiltIn(builtin) => {
                write!(self.out, " : {}", builtin_str(builtin))?;
            }
            crate::Binding::Location { location, .. } => {
                if stage == Some(crate::ShaderStage::Fragment) && output == Some(true) {
                    write!(self.out, " : SV_Target{}", location)?;
                } else {
                    write!(self.out, " : {}{}", LOCATION_SEMANTIC, location)?;
                }
            }
        }

        Ok(())
    }

    fn write_ep_input_struct(
        &mut self,
        module: &Module,
        func: &crate::Function,
        stage: ShaderStage,
        entry_point_name: &str,
    ) -> BackendResult {
        if !func.arguments.is_empty() {
            let struct_name_prefix = match stage {
                ShaderStage::Vertex => "VertexInput",
                ShaderStage::Fragment => "FragmentInput",
                ShaderStage::Compute => "ComputeInput",
            };
            let struct_name = format!("{}_{}", struct_name_prefix, entry_point_name);

            let mut members = Vec::with_capacity(func.arguments.len());

            write!(self.out, "struct {}", &struct_name)?;
            writeln!(self.out, " {{")?;

            for arg in func.arguments.iter() {
                let member_name = if let Some(ref name) = arg.name {
                    name
                } else {
                    "member"
                };
                let member = EpStructMember {
                    name: self.namer.call_unique(member_name),
                    ty: arg.ty,
                    binding: arg.binding.clone(),
                };

                write!(self.out, "{}", back::INDENT)?;
                self.write_type(module, member.ty)?;
                write!(self.out, " {}", &member.name)?;
                if let Some(ref binding) = member.binding {
                    self.write_semantic(binding, Some(stage), Some(false))?;
                }
                write!(self.out, ";")?;
                writeln!(self.out)?;

                members.push(member);
            }

            writeln!(self.out, "}};")?;
            writeln!(self.out)?;

            let ep_input = EntryPointBinding {
                stage,
                name: struct_name,
                members,
            };

            self.ep_inputs.push(Some(ep_input));
        } else {
            self.ep_inputs.push(None);
        }

        Ok(())
    }

    /// Helper method used to write global variables
    /// # Notes
    /// Always adds a newline
    fn write_global(
        &mut self,
        module: &Module,
        handle: Handle<crate::GlobalVariable>,
    ) -> BackendResult {
        let global = &module.global_variables[handle];
        let inner = &module.types[global.ty].inner;

        if let Some(storage_access) = storage_access(global.storage_access) {
            write!(self.out, "{} ", storage_access)?;
        }

        let (storage_class, register_ty) = match *inner {
            TypeInner::Image { .. } => ("", "t"),
            TypeInner::Sampler { .. } => ("", "s"),
            TypeInner::Struct { .. } | TypeInner::Vector { .. } => ("static ", ""),
            // TODO: other register ty https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-variable-register
            _ => return Err(Error::Unimplemented(format!("register_ty {:?}", inner))),
        };

        write!(self.out, "{}", storage_class)?;
        self.write_type(module, global.ty)?;
        if let TypeInner::Array { size, .. } = module.types[global.ty].inner {
            self.write_array_size(module, size)?;
        }
        write!(
            self.out,
            " {}",
            &self.names[&NameKey::GlobalVariable(handle)]
        )?;

        if let Some(ref binding) = global.binding {
            writeln!(self.out, " : register({}{});", register_ty, binding.binding)?;
        } else {
            write!(self.out, " = ")?;
            if let Some(init) = global.init {
                self.write_constant(module, init)?;
            } else {
                self.write_default_init(module, global.ty)?;
            }
            writeln!(self.out, ";")?;
        }

        Ok(())
    }

    /// Helper method used to write global constants
    ///
    /// # Notes
    /// Ends in a newline
    fn write_global_constant(
        &mut self,
        module: &Module,
        inner: &crate::ConstantInner,
        handle: Handle<crate::Constant>,
    ) -> BackendResult {
        write!(self.out, "static const ")?;

        match *inner {
            crate::ConstantInner::Scalar {
                width: _,
                ref value,
            } => {
                // Write type
                match *value {
                    crate::ScalarValue::Sint(_) => write!(self.out, "int")?,
                    crate::ScalarValue::Uint(_) => write!(self.out, "uint")?,
                    crate::ScalarValue::Float(_) => write!(self.out, "float")?,
                    crate::ScalarValue::Bool(_) => write!(self.out, "bool")?,
                };
                let name = &self.names[&NameKey::Constant(handle)];
                write!(self.out, " {} = ", name)?;

                // Second match required to avoid heap allocation by `format!()`
                match *value {
                    crate::ScalarValue::Sint(value) => write!(self.out, "{}", value)?,
                    crate::ScalarValue::Uint(value) => write!(self.out, "{}", value)?,
                    crate::ScalarValue::Float(value) => {
                        // Floats are written using `Debug` instead of `Display` because it always appends the
                        // decimal part even it's zero
                        write!(self.out, "{:?}", value)?
                    }
                    crate::ScalarValue::Bool(value) => write!(self.out, "{}", value)?,
                };
            }
            crate::ConstantInner::Composite { ty, ref components } => {
                self.write_type(module, ty)?;
                self.write_composite_constant(module, ty, components)?;
            }
        }
        writeln!(self.out, ";")?;
        // End with extra newline for readability
        writeln!(self.out)?;
        Ok(())
    }

    // copy-paste from glsl-out
    fn write_array_size(&mut self, module: &Module, size: crate::ArraySize) -> BackendResult {
        write!(self.out, "[")?;

        // Write the array size
        // Writes nothing if `ArraySize::Dynamic`
        // Panics if `ArraySize::Constant` has a constant that isn't an uint
        match size {
            crate::ArraySize::Constant(const_handle) => {
                match module.constants[const_handle].inner {
                    crate::ConstantInner::Scalar {
                        width: _,
                        value: crate::ScalarValue::Uint(size),
                    } => write!(self.out, "{}", size)?,
                    _ => unreachable!(),
                }
            }
            crate::ArraySize::Dynamic => (),
        }

        write!(self.out, "]")?;
        Ok(())
    }

    /// Helper method used to write structs
    ///
    /// # Notes
    /// Ends in a newline
    fn write_struct(
        &mut self,
        module: &Module,
        handle: Handle<crate::Type>,
        _block: bool,
        members: &[crate::StructMember],
        shader_stage: Option<ShaderStage>,
        out: Option<bool>,
    ) -> BackendResult {
        // Write struct name
        write!(self.out, "struct {}", self.names[&NameKey::Type(handle)])?;
        writeln!(self.out, " {{")?;

        for (index, member) in members.iter().enumerate() {
            // The indentation is only for readability
            write!(self.out, "{}", back::INDENT)?;

            match module.types[member.ty].inner {
                TypeInner::Array {
                    base,
                    size,
                    stride: _,
                } => {
                    // HLSL arrays are written as `type name[size]`
                    let ty_name = match module.types[base].inner {
                        // Write scalar type by backend so as not to depend on the front-end implementation
                        // Name returned from frontend can be generated (type1, float1, etc.)
                        TypeInner::Scalar { kind, width } => scalar_kind_str(kind, width)?,
                        _ => &self.names[&NameKey::Type(base)],
                    };

                    // Write `type` and `name`
                    write!(self.out, "{}", ty_name)?;
                    write!(
                        self.out,
                        " {}",
                        &self.names[&NameKey::StructMember(handle, index as u32)]
                    )?;
                    // Write [size]
                    self.write_array_size(module, size)?;
                }
                _ => {
                    // Write interpolation modifier before type
                    if let Some(crate::Binding::Location {
                        interpolation,
                        sampling,
                        ..
                    }) = member.binding
                    {
                        if let Some(interpolation) = interpolation {
                            write!(self.out, "{} ", interpolation_str(interpolation))?
                        }

                        if let Some(sampling) = sampling {
                            if let Some(str) = sampling_str(sampling) {
                                write!(self.out, "{} ", str)?
                            }
                        }
                    }

                    // Write the member type and name
                    self.write_type(module, member.ty)?;
                    write!(
                        self.out,
                        " {}",
                        &self.names[&NameKey::StructMember(handle, index as u32)]
                    )?;
                }
            }

            if let Some(ref binding) = member.binding {
                self.write_semantic(binding, shader_stage, out)?;
            };
            write!(self.out, ";")?;
            writeln!(self.out)?;
        }

        writeln!(self.out, "}};")?;
        Ok(())
    }

    /// Helper method used to write non image/sampler types
    ///
    /// # Notes
    /// Adds no trailing or leading whitespace
    fn write_type(&mut self, module: &Module, ty: Handle<crate::Type>) -> BackendResult {
        let inner = &module.types[ty].inner;
        match *inner {
            TypeInner::Struct { .. } => write!(self.out, "{}", self.names[&NameKey::Type(ty)])?,
            // hlsl array has the size separated from the base type
            TypeInner::Array { base, .. } => self.write_type(module, base)?,
            ref other => self.write_value_type(module, other)?,
        }

        Ok(())
    }

    /// Helper method used to write value types
    ///
    /// # Notes
    /// Adds no trailing or leading whitespace
    fn write_value_type(&mut self, module: &Module, inner: &TypeInner) -> BackendResult {
        match *inner {
            TypeInner::Scalar { kind, width } => {
                write!(self.out, "{}", scalar_kind_str(kind, width)?)?;
            }
            TypeInner::Vector { size, kind, width } => {
                write!(
                    self.out,
                    "{}{}",
                    scalar_kind_str(kind, width)?,
                    back::vector_size_str(size)
                )?;
            }
            TypeInner::Matrix {
                columns,
                rows,
                width,
            } => {
                // The IR supports only float matrix
                // https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-matrix
                write!(
                    self.out,
                    "{}{}x{}",
                    scalar_kind_str(crate::ScalarKind::Float, width)?,
                    back::vector_size_str(columns),
                    back::vector_size_str(rows),
                )?;
            }
            TypeInner::Image {
                dim,
                arrayed: _, //TODO:
                class,
            } => {
                let dim_str = image_dimension_str(dim);
                if let crate::ImageClass::Sampled { kind, multi: false } = class {
                    write!(
                        self.out,
                        "Texture{}<{}4>",
                        dim_str,
                        scalar_kind_str(kind, 4)?
                    )?
                } else {
                    return Err(Error::Unimplemented(format!(
                        "write_value_type {:?}",
                        inner
                    )));
                }
            }
            TypeInner::Sampler { comparison: false } => {
                write!(self.out, "SamplerState")?;
            }
            // HLSL arrays are written as `type name[size]`
            // Current code is written arrays only as `[size]`
            // Base `type` and `name` should be written outside
            TypeInner::Array { size, .. } => {
                self.write_array_size(module, size)?;
            }
            _ => {
                return Err(Error::Unimplemented(format!(
                    "write_value_type {:?}",
                    inner
                )))
            }
        }

        Ok(())
    }

    /// Helper method used to write structs
    /// # Notes
    /// Ends in a newline
    fn write_function(
        &mut self,
        module: &Module,
        name: &str,
        func: &crate::Function,
        func_ctx: &back::FunctionCtx<'_>,
    ) -> BackendResult {
        // Function Declaration Syntax - https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-function-syntax
        if let Some(ref result) = func.result {
            self.write_type(module, result.ty)?;
        } else {
            write!(self.out, "void")?;
        }

        // Write function name
        write!(self.out, " {}(", name)?;

        // Write function arguments for non entry point functions
        match func_ctx.ty {
            back::FunctionType::Function(handle) => {
                for (index, arg) in func.arguments.iter().enumerate() {
                    // Write argument type
                    self.write_type(module, arg.ty)?;

                    let argument_name =
                        &self.names[&NameKey::FunctionArgument(handle, index as u32)];

                    // Write argument name. Space is important.
                    write!(self.out, " {}", argument_name)?;
                    if index < func.arguments.len() - 1 {
                        // Add a separator between args
                        write!(self.out, ", ")?;
                    }
                }
            }
            back::FunctionType::EntryPoint(index) => {
                // EntryPoint arguments wrapped into structure
                // We need to ensure that entry points have arguments too.
                // For the case when we working with multiple entry points
                // for example vertex shader with arguments and fragment shader without arguments.
                if !self.ep_inputs.is_empty()
                    && !module.entry_points[index as usize]
                        .function
                        .arguments
                        .is_empty()
                {
                    if let Some(ref ep_input) = self.ep_inputs[index as usize] {
                        write!(
                            self.out,
                            "{} {}",
                            ep_input.name,
                            self.namer
                                .call_unique(ep_input.name.to_lowercase().as_str())
                        )?;
                    }
                }
            }
        }
        // Ends of arguments
        write!(self.out, ")")?;

        // Write semantic if it present
        let stage = match func_ctx.ty {
            back::FunctionType::EntryPoint(index) => {
                Some(module.entry_points[index as usize].stage)
            }
            _ => None,
        };
        if let Some(ref result) = func.result {
            if let Some(ref binding) = result.binding {
                let output = stage.is_some();
                self.write_semantic(binding, stage, Some(output))?;
            }
        }

        // Function body start
        writeln!(self.out)?;
        writeln!(self.out, "{{")?;
        // Write function local variables
        for (handle, local) in func.local_variables.iter() {
            // Write indentation (only for readability)
            write!(self.out, "{}", back::INDENT)?;

            // Write the local name
            // The leading space is important
            self.write_type(module, local.ty)?;
            write!(self.out, " {}", self.names[&func_ctx.name_key(handle)])?;

            write!(self.out, " = ")?;
            // Write the local initializer if needed
            if let Some(init) = local.init {
                // Put the equal signal only if there's a initializer
                // The leading and trailing spaces aren't needed but help with readability

                // Write the constant
                // `write_constant` adds no trailing or leading space/newline
                self.write_constant(module, init)?;
            } else {
                // Zero initialize local variables
                self.write_default_init(module, local.ty)?;
            }

            // Finish the local with `;` and add a newline (only for readability)
            writeln!(self.out, ";")?
        }

        if !func.local_variables.is_empty() {
            writeln!(self.out)?;
        }

        // Write the function body (statement list)
        for sta in func.body.iter() {
            // The indentation should always be 1 when writing the function body
            self.write_stmt(module, sta, func_ctx, 1)?;
        }

        writeln!(self.out, "}}")?;

        self.named_expressions.clear();

        Ok(())
    }

    /// Helper method used to write statements
    ///
    /// # Notes
    /// Always adds a newline
    fn write_stmt(
        &mut self,
        module: &Module,
        stmt: &crate::Statement,
        func_ctx: &back::FunctionCtx<'_>,
        indent: usize,
    ) -> BackendResult {
        use crate::Statement;
        use back::INDENT;

        match *stmt {
            Statement::Emit(ref range) => {
                for handle in range.clone() {
                    let expr_name = if let Some(name) = func_ctx.named_expressions.get(&handle) {
                        // Front end provides names for all variables at the start of writing.
                        // But we write them to step by step. We need to recache them
                        // Otherwise, we could accidentally write variable name instead of full expression.
                        // Also, we use sanitized names! It defense backend from generating variable with name from reserved keywords.
                        Some(self.namer.call_unique(name))
                    } else {
                        let min_ref_count = func_ctx.expressions[handle].bake_ref_count();
                        if min_ref_count <= func_ctx.info[handle].ref_count {
                            Some(format!("_expr{}", handle.index()))
                        } else {
                            None
                        }
                    };

                    if let Some(name) = expr_name {
                        write!(self.out, "{}", INDENT.repeat(indent))?;
                        self.write_named_expr(module, handle, name, func_ctx)?;
                    }
                }
            }
            // TODO: copy-paste from glsl-out
            Statement::Block(ref block) => {
                write!(self.out, "{}", INDENT.repeat(indent))?;
                writeln!(self.out, "{{")?;
                for sta in block.iter() {
                    // Increase the indentation to help with readability
                    self.write_stmt(module, sta, func_ctx, indent + 1)?
                }
                writeln!(self.out, "{}}}", INDENT.repeat(indent))?
            }
            // TODO: copy-paste from glsl-out
            Statement::If {
                condition,
                ref accept,
                ref reject,
            } => {
                write!(self.out, "{}", INDENT.repeat(indent))?;
                write!(self.out, "if (")?;
                self.write_expr(module, condition, func_ctx)?;
                writeln!(self.out, ") {{")?;

                for sta in accept {
                    // Increase indentation to help with readability
                    self.write_stmt(module, sta, func_ctx, indent + 1)?;
                }

                // If there are no statements in the reject block we skip writing it
                // This is only for readability
                if !reject.is_empty() {
                    writeln!(self.out, "{}}} else {{", INDENT.repeat(indent))?;

                    for sta in reject {
                        // Increase indentation to help with readability
                        self.write_stmt(module, sta, func_ctx, indent + 1)?;
                    }
                }

                writeln!(self.out, "{}}}", INDENT.repeat(indent))?
            }
            // TODO: copy-paste from glsl-out
            Statement::Kill => writeln!(self.out, "{}discard;", INDENT.repeat(indent))?,
            Statement::Return { value: None } => {
                writeln!(self.out, "{}return;", INDENT.repeat(indent))?;
            }
            Statement::Return { value: Some(expr) } => {
                let base_ty_res = &func_ctx.info[expr].ty;
                let mut resolved = base_ty_res.inner_with(&module.types);
                if let TypeInner::Pointer { base, class: _ } = *resolved {
                    resolved = &module.types[base].inner;
                }

                if let TypeInner::Struct { .. } = *resolved {
                    // We can safery unwrap here, since we now we working with struct
                    let ty = base_ty_res.handle().unwrap();
                    let struct_name = &self.names[&NameKey::Type(ty)];
                    let variable_name = self.namer.call_unique(struct_name.as_str()).to_lowercase();
                    write!(
                        self.out,
                        "{}const {} {} = ",
                        INDENT.repeat(indent),
                        struct_name,
                        variable_name
                    )?;
                    self.write_expr(module, expr, func_ctx)?;
                    writeln!(self.out, ";")?;
                    writeln!(
                        self.out,
                        "{}return {};",
                        INDENT.repeat(indent),
                        variable_name
                    )?;
                } else {
                    write!(self.out, "{}return ", INDENT.repeat(indent))?;
                    self.write_expr(module, expr, func_ctx)?;
                    writeln!(self.out, ";")?
                }
            }
            Statement::Store { pointer, value } => {
                write!(self.out, "{}", INDENT.repeat(indent))?;
                self.write_expr(module, pointer, func_ctx)?;
                write!(self.out, " = ")?;
                self.write_expr(module, value, func_ctx)?;
                writeln!(self.out, ";")?
            }
            Statement::Call {
                function,
                ref arguments,
                result,
            } => {
                write!(self.out, "{}", INDENT.repeat(indent))?;
                if let Some(expr) = result {
                    write!(self.out, "const ")?;
                    let name = format!("{}{}", back::BAKE_PREFIX, expr.index());
                    let expr_ty = &func_ctx.info[expr].ty;
                    match *expr_ty {
                        proc::TypeResolution::Handle(handle) => self.write_type(module, handle)?,
                        proc::TypeResolution::Value(ref value) => {
                            self.write_value_type(module, value)?
                        }
                    };
                    write!(self.out, " {} = ", name)?;
                    self.write_expr(module, expr, func_ctx)?;
                    self.named_expressions.insert(expr, name);
                }
                let func_name = &self.names[&NameKey::Function(function)];
                write!(self.out, "{}(", func_name)?;
                for (index, argument) in arguments.iter().enumerate() {
                    self.write_expr(module, *argument, func_ctx)?;
                    // Only write a comma if isn't the last element
                    if index != arguments.len().saturating_sub(1) {
                        // The leading space is for readability only
                        write!(self.out, ", ")?;
                    }
                }
                writeln!(self.out, ");")?
            }
            Statement::Loop {
                ref body,
                ref continuing,
            } => {
                write!(self.out, "{}", INDENT.repeat(indent))?;
                writeln!(self.out, "while(true) {{")?;

                for sta in body.iter().chain(continuing.iter()) {
                    self.write_stmt(module, sta, func_ctx, indent + 1)?;
                }

                writeln!(self.out, "{}}}", INDENT.repeat(indent))?
            }
            Statement::Break => {
                write!(self.out, "{}", INDENT.repeat(indent))?;
                writeln!(self.out, "break;")?
            }
            Statement::Continue => {
                write!(self.out, "{}", INDENT.repeat(indent))?;
                writeln!(self.out, "continue;")?
            }
            _ => return Err(Error::Unimplemented(format!("write_stmt {:?}", stmt))),
        }

        Ok(())
    }

    /// Helper method to write expressions
    ///
    /// # Notes
    /// Doesn't add any newlines or leading/trailing spaces
    fn write_expr(
        &mut self,
        module: &Module,
        expr: Handle<crate::Expression>,
        func_ctx: &back::FunctionCtx<'_>,
    ) -> BackendResult {
        use crate::Expression;

        if let Some(name) = self.named_expressions.get(&expr) {
            write!(self.out, "{}", name)?;
            return Ok(());
        }

        let expression = &func_ctx.expressions[expr];

        match *expression {
            Expression::Constant(constant) => self.write_constant(module, constant)?,
            Expression::Compose { ty, ref components } => {
                let is_struct = if let TypeInner::Struct { .. } = module.types[ty].inner {
                    true
                } else {
                    false
                };
                if is_struct {
                    write!(self.out, "{{ ")?;
                } else {
                    self.write_type(module, ty)?;
                    write!(self.out, "(")?;
                }
                for (index, component) in components.iter().enumerate() {
                    self.write_expr(module, *component, func_ctx)?;
                    // Only write a comma if isn't the last element
                    if index != components.len().saturating_sub(1) {
                        // The leading space is for readability only
                        write!(self.out, ", ")?;
                    }
                }
                if is_struct {
                    write!(self.out, " }}")?
                } else {
                    write!(self.out, ")")?
                }
            }
            Expression::Binary {
                op: crate::BinaryOperator::Multiply,
                left,
                right,
            } => {
                write!(self.out, "mul(")?;
                self.write_expr(module, left, func_ctx)?;
                write!(self.out, ", ")?;
                self.write_expr(module, right, func_ctx)?;
                write!(self.out, ")")?;
            }
            // TODO: copy-paste from wgsl-out
            Expression::Binary { op, left, right } => {
                write!(self.out, "(")?;
                self.write_expr(module, left, func_ctx)?;
                write!(self.out, " {} ", crate::back::binary_operation_str(op))?;
                self.write_expr(module, right, func_ctx)?;
                write!(self.out, ")")?;
            }
            // TODO: copy-paste from glsl-out
            Expression::AccessIndex { base, index } => {
                self.write_expr(module, base, func_ctx)?;

                let base_ty_res = &func_ctx.info[base].ty;
                let mut resolved = base_ty_res.inner_with(&module.types);
                let base_ty_handle = match *resolved {
                    TypeInner::Pointer { base, class: _ } => {
                        resolved = &module.types[base].inner;
                        Some(base)
                    }
                    _ => base_ty_res.handle(),
                };

                match *resolved {
                    TypeInner::Vector { .. } => {
                        // Write vector access as a swizzle
                        write!(self.out, ".{}", back::COMPONENTS[index as usize])?
                    }
                    TypeInner::Matrix { .. }
                    | TypeInner::Array { .. }
                    | TypeInner::ValuePointer { .. } => write!(self.out, "[{}]", index)?,
                    TypeInner::Struct { .. } => {
                        // This will never panic in case the type is a `Struct`, this is not true
                        // for other types so we can only check while inside this match arm
                        let ty = base_ty_handle.unwrap();

                        write!(
                            self.out,
                            ".{}",
                            &self.names[&NameKey::StructMember(ty, index)]
                        )?
                    }
                    ref other => return Err(Error::Custom(format!("Cannot index {:?}", other))),
                }
            }
            Expression::FunctionArgument(pos) => {
                match func_ctx.ty {
                    back::FunctionType::Function(handle) => {
                        let name = &self.names[&NameKey::FunctionArgument(handle, pos)];
                        write!(self.out, "{}", name)?;
                    }
                    back::FunctionType::EntryPoint(index) => {
                        // EntryPoint arguments wrapped into structure
                        // We can safery unwrap here, because if we write function arguments it means, that ep_input struct already exists
                        let ep_input = self.ep_inputs[index as usize].as_ref().unwrap();
                        let member_name = &ep_input.members[pos as usize].name;
                        write!(
                            self.out,
                            "{}.{}",
                            &ep_input.name.to_lowercase(),
                            member_name
                        )?
                    }
                };
            }
            Expression::ImageSample {
                image,
                sampler,        // TODO:
                coordinate,     // TODO:
                array_index: _, // TODO:
                offset: _,      // TODO:
                level: _,       // TODO:
                depth_ref: _,   // TODO:
            } => {
                self.write_expr(module, image, func_ctx)?;
                write!(self.out, ".Sample(")?;
                self.write_expr(module, sampler, func_ctx)?;
                write!(self.out, ", ")?;
                self.write_expr(module, coordinate, func_ctx)?;
                write!(self.out, ")")?;
            }
            // TODO: copy-paste from wgsl-out
            Expression::GlobalVariable(handle) => {
                let name = &self.names[&NameKey::GlobalVariable(handle)];
                write!(self.out, "{}", name)?;
            }
            Expression::LocalVariable(handle) => {
                write!(self.out, "{}", self.names[&func_ctx.name_key(handle)])?
            }
            Expression::Load { pointer } => {
                self.write_expr(module, pointer, func_ctx)?;
            }
            Expression::Access { base, index } => {
                self.write_expr(module, base, func_ctx)?;
                write!(self.out, "[")?;
                self.write_expr(module, index, func_ctx)?;
                write!(self.out, "]")?;
            }
            Expression::Unary { op, expr } => {
                // https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-operators#unary-operators
                let convert_to_bool = if let TypeInner::Scalar {
                    kind: crate::ScalarKind::Bool,
                    ..
                } = *func_ctx.info[expr].ty.inner_with(&module.types)
                {
                    false
                } else {
                    true
                };
                let op_str = match op {
                    crate::UnaryOperator::Negate => "-",
                    crate::UnaryOperator::Not => "!",
                };
                write!(self.out, "({}", op_str)?;

                if convert_to_bool {
                    write!(self.out, "bool(")?;
                }

                self.write_expr(module, expr, func_ctx)?;

                if convert_to_bool {
                    write!(self.out, ")")?;
                }

                write!(self.out, ")")?
            }
            Expression::As { expr, kind, .. } => {
                let inner = func_ctx.info[expr].ty.inner_with(&module.types);
                match *inner {
                    TypeInner::Vector { size, width, .. } => {
                        write!(
                            self.out,
                            "{}{}",
                            scalar_kind_str(kind, width)?,
                            back::vector_size_str(size),
                        )?;
                    }
                    TypeInner::Scalar { width, .. } => {
                        write!(self.out, "{}", scalar_kind_str(kind, width)?)?
                    }
                    _ => {
                        return Err(Error::Unimplemented(format!(
                            "write_expr expression::as {:?}",
                            inner
                        )));
                    }
                };
                write!(self.out, "(")?;
                self.write_expr(module, expr, func_ctx)?;
                write!(self.out, ")")?;
            }
            Expression::Math {
                fun,
                arg,
                arg1,
                arg2,
            } => {
                use crate::MathFunction as Mf;

                let fun_name = match fun {
                    // comparison
                    Mf::Abs => "abs",
                    Mf::Min => "min",
                    Mf::Max => "max",
                    Mf::Clamp => "clamp",
                    // trigonometry
                    Mf::Cos => "cos",
                    Mf::Cosh => "cosh",
                    Mf::Sin => "sin",
                    Mf::Sinh => "sinh",
                    Mf::Tan => "tan",
                    Mf::Tanh => "tanh",
                    Mf::Acos => "acos",
                    Mf::Asin => "asin",
                    Mf::Atan => "atan",
                    Mf::Atan2 => "atan2",
                    // decomposition
                    Mf::Ceil => "ceil",
                    Mf::Floor => "floor",
                    Mf::Round => "round",
                    Mf::Fract => "frac",
                    Mf::Trunc => "trunc",
                    Mf::Modf => "modf",
                    Mf::Frexp => "frexp",
                    Mf::Ldexp => "ldexp",
                    // exponent
                    Mf::Exp => "exp",
                    Mf::Exp2 => "exp2",
                    Mf::Log => "log",
                    Mf::Log2 => "log2",
                    Mf::Pow => "pow",
                    // geometry
                    Mf::Dot => "dot",
                    //Mf::Outer => ,
                    Mf::Cross => "cross",
                    Mf::Distance => "distance",
                    Mf::Length => "length",
                    Mf::Normalize => "normalize",
                    Mf::FaceForward => "faceforward",
                    Mf::Reflect => "reflect",
                    Mf::Refract => "refract",
                    // computational
                    Mf::Sign => "sign",
                    Mf::Fma => "fma",
                    Mf::Mix => "lerp",
                    Mf::Step => "step",
                    Mf::SmoothStep => "smoothstep",
                    Mf::Sqrt => "sqrt",
                    Mf::InverseSqrt => "rsqrt",
                    //Mf::Inverse =>,
                    Mf::Transpose => "transpose",
                    Mf::Determinant => "determinant",
                    // bits
                    Mf::CountOneBits => "countbits",
                    Mf::ReverseBits => "reversebits",
                    _ => return Err(Error::Unimplemented(format!("write_expr_math {:?}", fun))),
                };

                write!(self.out, "{}(", fun_name)?;
                self.write_expr(module, arg, func_ctx)?;
                if let Some(arg) = arg1 {
                    write!(self.out, ", ")?;
                    self.write_expr(module, arg, func_ctx)?;
                }
                if let Some(arg) = arg2 {
                    write!(self.out, ", ")?;
                    self.write_expr(module, arg, func_ctx)?;
                }
                write!(self.out, ")")?
            }
            Expression::Swizzle {
                size,
                vector,
                pattern,
            } => {
                self.write_expr(module, vector, func_ctx)?;
                write!(self.out, ".")?;
                for &sc in pattern[..size as usize].iter() {
                    self.out.write_char(back::COMPONENTS[sc as usize])?;
                }
            }
            // `ArrayLength` is written as `expr.length()`
            Expression::ArrayLength(expr) => {
                self.write_expr(module, expr, func_ctx)?;
                write!(self.out, ".length()")?
            }
            Expression::Derivative { axis, expr } => {
                use crate::DerivativeAxis as Da;

                write!(
                    self.out,
                    "{}(",
                    match axis {
                        Da::X => "ddx",
                        Da::Y => "ddy",
                        Da::Width => "fwidth",
                    }
                )?;
                self.write_expr(module, expr, func_ctx)?;
                write!(self.out, ")")?
            }
            Expression::Splat { size, value } => {
                // hlsl is not supported one value constructor
                // if we write, for example, int4(0), dxc returns error:
                // error: too few elements in vector initialization (expected 4 elements, have 1)
                let number_of_components = match size {
                    crate::VectorSize::Bi => "xx",
                    crate::VectorSize::Tri => "xxx",
                    crate::VectorSize::Quad => "xxxx",
                };
                let resolved = func_ctx.info[expr].ty.inner_with(&module.types);
                self.write_value_type(module, resolved)?;
                write!(self.out, "(")?;
                self.write_expr(module, value, func_ctx)?;
                write!(self.out, ".{})", number_of_components)?
            }
            Expression::Select {
                condition,
                accept,
                reject,
            } => {
                write!(self.out, "(")?;
                self.write_expr(module, condition, func_ctx)?;
                write!(self.out, " ? ")?;
                self.write_expr(module, accept, func_ctx)?;
                write!(self.out, " : ")?;
                self.write_expr(module, reject, func_ctx)?;
                write!(self.out, ")")?
            }
            // Nothing to do here, since call expression already cached
            Expression::Call(_) => {}
            _ => return Err(Error::Unimplemented(format!("write_expr {:?}", expression))),
        }

        Ok(())
    }

    /// Helper method used to write constants
    ///
    /// # Notes
    /// Doesn't add any newlines or leading/trailing spaces
    fn write_constant(
        &mut self,
        module: &Module,
        handle: Handle<crate::Constant>,
    ) -> BackendResult {
        let constant = &module.constants[handle];
        match constant.inner {
            crate::ConstantInner::Scalar {
                width: _,
                ref value,
            } => {
                if constant.name.is_some() {
                    write!(self.out, "{}", &self.names[&NameKey::Constant(handle)])?;
                } else {
                    self.write_scalar_value(*value)?;
                }
            }
            crate::ConstantInner::Composite { ty, ref components } => {
                self.write_composite_constant(module, ty, components)?;
            }
        }

        Ok(())
    }

    fn write_composite_constant(
        &mut self,
        module: &Module,
        ty: Handle<crate::Type>,
        components: &[Handle<crate::Constant>],
    ) -> BackendResult {
        let (open_b, close_b) = match module.types[ty].inner {
            TypeInner::Struct { .. } => ("{ ", " }"),
            _ => {
                // We should write type only for non struct constants
                self.write_type(module, ty)?;
                ("(", ")")
            }
        };
        write!(self.out, "{}", open_b)?;
        for (index, constant) in components.iter().enumerate() {
            self.write_constant(module, *constant)?;
            // Only write a comma if isn't the last element
            if index != components.len().saturating_sub(1) {
                // The leading space is for readability only
                write!(self.out, ", ")?;
            }
        }
        write!(self.out, "{}", close_b)?;

        Ok(())
    }

    /// Helper method used to write [`ScalarValue`](ScalarValue)
    ///
    /// # Notes
    /// Adds no trailing or leading whitespace
    fn write_scalar_value(&mut self, value: crate::ScalarValue) -> BackendResult {
        use crate::ScalarValue as Sv;

        match value {
            Sv::Sint(value) => write!(self.out, "{}", value)?,
            Sv::Uint(value) => write!(self.out, "{}u", value)?,
            // Floats are written using `Debug` instead of `Display` because it always appends the
            // decimal part even it's zero
            Sv::Float(value) => write!(self.out, "{:?}", value)?,
            Sv::Bool(value) => write!(self.out, "{}", value)?,
        }

        Ok(())
    }

    fn write_named_expr(
        &mut self,
        module: &Module,
        handle: Handle<crate::Expression>,
        name: String,
        ctx: &back::FunctionCtx,
    ) -> BackendResult {
        match ctx.info[handle].ty {
            proc::TypeResolution::Handle(ty_handle) => match module.types[ty_handle].inner {
                TypeInner::Struct { .. } => {
                    let ty_name = &self.names[&NameKey::Type(ty_handle)];
                    write!(self.out, "{}", ty_name)?;
                }
                _ => {
                    self.write_type(module, ty_handle)?;
                }
            },
            proc::TypeResolution::Value(ref inner) => {
                self.write_value_type(module, inner)?;
            }
        }

        let base_ty_res = &ctx.info[handle].ty;
        let resolved = base_ty_res.inner_with(&module.types);

        write!(self.out, " {}", name)?;
        // If rhs is a array type, we should write array size
        if let TypeInner::Array { size, .. } = *resolved {
            self.write_array_size(module, size)?;
        }
        write!(self.out, " = ")?;
        self.write_expr(module, handle, ctx)?;
        writeln!(self.out, ";")?;
        self.named_expressions.insert(handle, name);

        Ok(())
    }

    /// Helper function that write default zero initialization
    fn write_default_init(&mut self, module: &Module, ty: Handle<crate::Type>) -> BackendResult {
        write!(self.out, "(")?;
        self.write_type(module, ty)?;
        write!(self.out, ")0")?;

        Ok(())
    }
}

fn image_dimension_str(dim: crate::ImageDimension) -> &'static str {
    use crate::ImageDimension as IDim;

    match dim {
        IDim::D1 => "1D",
        IDim::D2 => "2D",
        IDim::D3 => "3D",
        IDim::Cube => "Cube",
    }
}

fn builtin_str(built_in: crate::BuiltIn) -> &'static str {
    use crate::BuiltIn as Bi;

    match built_in {
        Bi::Position => "SV_Position",
        // vertex
        Bi::ClipDistance => "SV_ClipDistance",
        Bi::CullDistance => "SV_CullDistance",
        Bi::InstanceIndex => "SV_InstanceID",
        // based on this page https://docs.microsoft.com/en-us/windows/uwp/gaming/glsl-to-hlsl-reference#comparing-opengl-es-20-with-direct3d-11
        // No meaning unless you target Direct3D 9
        Bi::PointSize => "PSIZE",
        Bi::VertexIndex => "SV_VertexID",
        // fragment
        Bi::FragDepth => "SV_Depth",
        Bi::FrontFacing => "SV_IsFrontFace",
        Bi::SampleIndex => "SV_SampleIndex",
        Bi::SampleMask => "SV_Coverage",
        // compute
        Bi::GlobalInvocationId => "SV_DispatchThreadID",
        Bi::LocalInvocationId => "SV_GroupThreadID",
        Bi::LocalInvocationIndex => "SV_GroupIndex",
        Bi::WorkGroupId => "SV_GroupID",
        _ => todo!("builtin_str {:?}", built_in),
    }
}

/// Helper function that returns scalar related strings
/// https://docs.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-scalar
fn scalar_kind_str(kind: crate::ScalarKind, width: crate::Bytes) -> Result<&'static str, Error> {
    use crate::ScalarKind as Sk;

    match kind {
        Sk::Sint => Ok("int"),
        Sk::Uint => Ok("uint"),
        Sk::Float => match width {
            2 => Ok("half"),
            4 => Ok("float"),
            8 => Ok("double"),
            _ => Err(Error::UnsupportedScalar(kind, width)),
        },
        Sk::Bool => Ok("bool"),
    }
}

fn storage_access(storage_access: crate::StorageAccess) -> Option<&'static str> {
    if storage_access == crate::StorageAccess::LOAD {
        Some("ByteAddressBuffer")
    } else if storage_access.is_all() {
        Some("RWByteAddressBuffer")
    } else {
        None
    }
}

fn number_of_components(vector_size: crate::VectorSize) -> usize {
    match vector_size {
        crate::VectorSize::Bi => 2,
        crate::VectorSize::Tri => 3,
        crate::VectorSize::Quad => 4,
    }
}

/// Helper function that returns the string corresponding to the HLSL interpolation qualifier
fn interpolation_str(interpolation: crate::Interpolation) -> &'static str {
    use crate::Interpolation as I;

    match interpolation {
        I::Perspective => "linear",
        I::Linear => "noperspective",
        I::Flat => "nointerpolation",
    }
}

/// Return the HLSL auxiliary qualifier for the given sampling value.
fn sampling_str(sampling: crate::Sampling) -> Option<&'static str> {
    use crate::Sampling as S;

    match sampling {
        S::Center => None,
        S::Centroid => Some("centroid"),
        S::Sample => Some("sample"),
    }
}
