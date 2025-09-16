#![deny(warnings)]

use {
    anyhow::{Context, Result, bail},
    async_trait::async_trait,
    futures::future::BoxFuture,
    std::{
        collections::{HashMap, hash_map::Entry},
        convert, iter,
        ops::Range,
    },
    wasm_encoder::{
        Alias, CanonicalFunctionSection, CanonicalOption, CodeSection, Component,
        ComponentAliasSection, ComponentExportKind, ComponentExportSection, ComponentTypeSection,
        ComponentValType, ConstExpr, DataCountSection, DataSection, ExportKind, ExportSection,
        Function, FunctionSection, GlobalSection, GlobalType, ImportSection, InstanceSection,
        Instruction as Ins, MemArg, MemorySection, Module, ModuleArg, ModuleSection,
        NestedComponentSection, PrimitiveValType, RawSection, TypeSection, ValType,
        reencode::{Reencode, RoundtripReencoder as Encode},
    },
    wasmparser::{
        CanonicalFunction, ComponentAlias, ComponentExternalKind, ComponentTypeRef, ExternalKind,
        Instance, Operator, Parser, Payload, TypeRef, Validator,
    },
};

const PAGE_SIZE_BYTES: i32 = 64 * 1024;

// TODO: this should ideally be 8 in order to minimize binary size, but that can result in larger numbers of data
// segments than some tools and runtimes will tolerate.  We should probably start at 8 and increase as necessary if
// the segment count is too high for a given component.
const MAX_CONSECUTIVE_ZEROS: usize = 64;

#[async_trait]
pub trait Invoker {
    async fn call_s32(&mut self, function: &str) -> Result<i32>;
    async fn call_s64(&mut self, function: &str) -> Result<i64>;
    async fn call_f32(&mut self, function: &str) -> Result<f32>;
    async fn call_f64(&mut self, function: &str) -> Result<f64>;
    async fn call_list_u8(&mut self, function: &str) -> Result<Vec<u8>>;
}

pub async fn initialize(
    component: &[u8],
    initialize: impl FnOnce(Vec<u8>) -> BoxFuture<'static, Result<Box<dyn Invoker>>>,
) -> Result<Vec<u8>> {
    initialize_staged(component, None, initialize).await
}

pub type Stage2Map<'a> = Option<(&'a [u8], &'a dyn Fn(u32) -> u32)>;

#[allow(clippy::type_complexity)]
pub async fn initialize_staged(
    component_stage1: &[u8],
    component_stage2_and_map_module_index: Stage2Map<'_>,
    initialize: impl FnOnce(Vec<u8>) -> BoxFuture<'static, Result<Box<dyn Invoker>>>,
) -> Result<Vec<u8>> {
    // First, instrument the input component, validating that it conforms to certain rules and exposing the memory
    // and all mutable globals via synthesized function exports.
    //
    // Note that we currently only support a certain style of component, but plan to eventually generalize this
    // tool to support arbitrary component graphs.
    //
    // Current rules:
    // - Flat structure (i.e. no subcomponents)
    // - Single memory
    // - No runtime table operations
    // - No reference type globals
    // - Each module instantiated at most once
    //
    // `instrumentation` keeps track of all of the state which will be gathered from the instrumented
    // component.
    let (instrumented_component, instrumentation) = instrument(component_stage1)?;

    Validator::new().validate_all(&instrumented_component)?;

    // A component runtime will instantiate the component and run its component init function.
    let mut invoker = initialize(instrumented_component).await?;

    // The Invoker interface is used to extract the values instrumentation provided into a
    // measurement.
    let measurement = instrumentation.measure(&mut invoker).await?;

    // Finally, create a new component by applying the measurement (contents of all globals and memory) to the component.
    // The resulting component will identical to the original except with all mutable globals initialized to
    // the snapshoted values, with all data sections and start functions removed, and with a single active data
    // section added containing the memory snapshot.
    apply(
        measurement,
        component_stage1,
        component_stage2_and_map_module_index,
    )
}

struct MemoryInfo {
    module_index: u32,
    export_name: String,
    ty: wasmparser::MemoryType,
}
type GlobalMap<T> = HashMap<u32, HashMap<u32, T>>;
#[derive(Debug)]
enum GlobalExport {
    Existing {
        module_index: u32,
        global_index: u32,
        export_name: String,
    },
    Synthesized {
        module_index: u32,
        global_index: u32,
    },
}
impl GlobalExport {
    fn module_export(&self) -> String {
        match self {
            Self::Existing { export_name, .. } => export_name.clone(),
            Self::Synthesized { global_index, .. } => format!("component-init:{global_index}"),
        }
    }
    fn component_export(&self) -> String {
        match self {
            Self::Existing {
                module_index,
                global_index,
                ..
            } => format!("component-init-get-module{module_index}-global{global_index}"),
            Self::Synthesized {
                module_index,
                global_index,
            } => format!("component-init-get-module{module_index}-global{global_index}"),
        }
    }
}

#[derive(Default)]
struct Instrumentation {
    memory: Option<MemoryInfo>,
    globals: GlobalMap<(GlobalExport, wasmparser::ValType)>,
}
impl Instrumentation {
    fn register_memory(
        &mut self,
        module_index: u32,
        name: impl AsRef<str>,
        ty: wasmparser::MemoryType,
    ) -> Result<()> {
        if self.memory.is_some() {
            bail!("only one memory allowed per component");
        }
        self.memory = Some(MemoryInfo {
            module_index,
            export_name: name.as_ref().to_string(),
            ty,
        });
        Ok(())
    }
    fn register_global(&mut self, module_index: u32, global_index: u32, ty: wasmparser::ValType) {
        self.globals.entry(module_index).or_default().insert(
            global_index,
            (
                GlobalExport::Synthesized {
                    module_index,
                    global_index,
                },
                ty,
            ),
        );
    }
    fn register_global_export(
        &mut self,
        module_index: u32,
        global_index: u32,
        export_name: impl AsRef<str>,
    ) {
        if let Some((name, _)) = self
            .globals
            .get_mut(&module_index)
            .and_then(|map| map.get_mut(&global_index))
        {
            let export_name = export_name.as_ref().to_string();
            *name = GlobalExport::Existing {
                module_index,
                global_index,
                export_name,
            };
        }
    }
    fn amend_module_exports(&self, module_index: u32, exports: &mut ExportSection) {
        if let Some(g_map) = self.globals.get(&module_index) {
            for (export, _ty) in g_map.values() {
                if let GlobalExport::Synthesized { global_index, .. } = export {
                    exports.export(&export.module_export(), ExportKind::Global, *global_index);
                }
            }
        }
    }
    async fn measure(&self, invoker: &mut Box<dyn Invoker>) -> Result<Measurement> {
        let mut globals = HashMap::new();

        for (module_index, globals_to_export) in &self.globals {
            let mut my_global_values = HashMap::new();
            for (global_index, (global_export, ty)) in globals_to_export {
                my_global_values.insert(
                    *global_index,
                    match ty {
                        wasmparser::ValType::I32 => ConstExpr::i32_const(
                            invoker
                                .call_s32(&global_export.component_export())
                                .await
                                .with_context(|| {
                                    format!("retrieving global value {global_export:?}")
                                })?,
                        ),
                        wasmparser::ValType::I64 => ConstExpr::i64_const(
                            invoker
                                .call_s64(&global_export.component_export())
                                .await
                                .with_context(|| {
                                    format!("retrieving global value {global_export:?}")
                                })?,
                        ),
                        wasmparser::ValType::F32 => ConstExpr::f32_const(
                            invoker
                                .call_f32(&global_export.component_export())
                                .await
                                .with_context(|| {
                                    format!("retrieving global value {global_export:?}")
                                })?
                                .into(),
                        ),
                        wasmparser::ValType::F64 => ConstExpr::f64_const(
                            invoker
                                .call_f64(&global_export.component_export())
                                .await
                                .with_context(|| {
                                    format!("retrieving global value {global_export:?}")
                                })?
                                .into(),
                        ),
                        wasmparser::ValType::V128 => bail!("V128 not yet supported"),
                        wasmparser::ValType::Ref(_) => bail!("reference types not supported"),
                    },
                );
            }
            globals.insert(*module_index, my_global_values);
        }

        let memory = if let Some(info) = &self.memory {
            let name = "component-init-get-memory";
            Some((
                info.module_index,
                invoker
                    .call_list_u8(name)
                    .await
                    .with_context(|| format!("retrieving memory with {name}"))?,
            ))
        } else {
            None
        };
        Ok(Measurement { memory, globals })
    }
}

struct Measurement {
    memory: Option<(u32, Vec<u8>)>,
    globals: GlobalMap<wasm_encoder::ConstExpr>,
}

impl Measurement {
    fn data_section(&self, module_index: u32) -> (Option<DataSection>, u32) {
        if let Some((m_ix, value)) = &self.memory
            && *m_ix == module_index
        {
            let mut data = DataSection::new();
            let mut data_segment_count = 0;
            for (start, len) in Segments::new(value) {
                data_segment_count += 1;
                data.active(
                    0,
                    &ConstExpr::i32_const(start.try_into().unwrap()),
                    value[start..][..len].iter().copied(),
                );
            }
            (Some(data), data_segment_count)
        } else {
            (None, 0)
        }
    }

    fn memory_initial(&self, module_index: u32) -> Option<u64> {
        if let Some((m_ix, value)) = &self.memory
            && *m_ix == module_index
        {
            Some(
                u64::try_from((value.len() / usize::try_from(PAGE_SIZE_BYTES).unwrap()) + 1)
                    .unwrap(),
            )
        } else {
            None
        }
    }

    fn global_init(&self, module_index: u32, global_index: u32) -> Option<wasm_encoder::ConstExpr> {
        self.globals
            .get(&module_index)
            .and_then(|m| m.get(&global_index).cloned())
    }
}

fn instrument(component_stage1: &[u8]) -> Result<(Vec<u8>, Instrumentation)> {
    let mut module_count = 0;
    let mut instance_count = 0;
    let mut core_function_count = 0;
    let mut function_count = 0;
    let mut type_count = 0;
    let mut instrumentation = Instrumentation::default();
    let mut instantiations = HashMap::new();
    let mut instrumented_component = Component::new();
    let mut parser = Parser::new(0).parse_all(component_stage1);
    #[allow(clippy::while_let_on_iterator)]
    while let Some(payload) = parser.next() {
        let payload = payload?;
        let section = payload.as_section();
        match payload {
            Payload::ComponentSection {
                unchecked_range, ..
            } => {
                let mut subcomponent = Component::new();
                while let Some(payload) = parser.next() {
                    let payload = payload?;
                    let section = payload.as_section();
                    let my_range = section.as_ref().map(|(_, range)| range.clone());
                    copy_component_section(section, component_stage1, &mut subcomponent);

                    if let Some(my_range) = my_range
                        && my_range.end >= unchecked_range.end
                    {
                        break;
                    }
                }
                instrumented_component.section(&NestedComponentSection(&subcomponent));
            }

            Payload::ModuleSection {
                unchecked_range, ..
            } => {
                let module_index = get_and_increment(&mut module_count);
                let mut global_types = Vec::new();
                let mut instrumented_module = Module::new();
                let mut global_count = 0;
                while let Some(payload) = parser.next() {
                    let payload = payload?;
                    let section = payload.as_section();
                    let my_range = section.as_ref().map(|(_, range)| range.clone());
                    match payload {
                        Payload::ImportSection(reader) => {
                            for import in reader {
                                if let TypeRef::Global(_) = import?.ty {
                                    global_count += 1;
                                }
                            }
                            copy_module_section(
                                section,
                                component_stage1,
                                &mut instrumented_module,
                            );
                        }

                        Payload::MemorySection(reader) => {
                            for memory in reader {
                                instrumentation.register_memory(module_index, "memory", memory?)?;
                            }
                            copy_module_section(
                                section,
                                component_stage1,
                                &mut instrumented_module,
                            );
                        }

                        Payload::GlobalSection(reader) => {
                            for global in reader {
                                let global = global?;
                                let ty = global.ty;
                                global_types.push(ty);
                                let global_index = get_and_increment(&mut global_count);
                                if global.ty.mutable {
                                    instrumentation.register_global(
                                        module_index,
                                        global_index,
                                        ty.content_type,
                                    )
                                }
                            }
                            copy_module_section(
                                section,
                                component_stage1,
                                &mut instrumented_module,
                            );
                        }

                        Payload::ExportSection(reader) => {
                            let mut exports = ExportSection::new();
                            for export in reader {
                                let export = export?;
                                if let ExternalKind::Global = export.kind {
                                    instrumentation.register_global_export(
                                        module_index,
                                        export.index,
                                        export.name,
                                    )
                                }
                                exports.export(
                                    export.name,
                                    Encode.export_kind(export.kind)?,
                                    export.index,
                                );
                            }

                            instrumentation.amend_module_exports(module_index, &mut exports);

                            instrumented_module.section(&exports);
                        }

                        Payload::CodeSectionEntry(body) => {
                            for operator in body.get_operators_reader()? {
                                match operator? {
                                    Operator::TableCopy { .. }
                                    | Operator::TableFill { .. }
                                    | Operator::TableGrow { .. }
                                    | Operator::TableInit { .. }
                                    | Operator::TableSet { .. } => {
                                        bail!("table operations not allowed");
                                    }

                                    _ => (),
                                }
                            }
                            copy_module_section(
                                section,
                                component_stage1,
                                &mut instrumented_module,
                            );
                        }

                        _ => {
                            copy_module_section(section, component_stage1, &mut instrumented_module)
                        }
                    }

                    if let Some(my_range) = my_range
                        && my_range.end >= unchecked_range.end
                    {
                        break;
                    }
                }
                instrumented_component.section(&ModuleSection(&instrumented_module));
            }

            Payload::InstanceSection(reader) => {
                for instance in reader {
                    let instance_index = get_and_increment(&mut instance_count);

                    if let Instance::Instantiate { module_index, .. } = instance? {
                        match instantiations.entry(module_index) {
                            Entry::Vacant(entry) => {
                                entry.insert(instance_index);
                            }
                            Entry::Occupied(_) => bail!("modules may be instantiated at most once"),
                        }
                    }
                }
                copy_component_section(section, component_stage1, &mut instrumented_component);
            }

            Payload::ComponentAliasSection(reader) => {
                for alias in reader {
                    match alias? {
                        ComponentAlias::CoreInstanceExport {
                            kind: ExternalKind::Func,
                            ..
                        } => {
                            core_function_count += 1;
                        }
                        ComponentAlias::InstanceExport {
                            kind: ComponentExternalKind::Type,
                            ..
                        } => {
                            type_count += 1;
                        }
                        ComponentAlias::InstanceExport {
                            kind: ComponentExternalKind::Func,
                            ..
                        } => {
                            function_count += 1;
                        }
                        _ => (),
                    }
                }
                copy_component_section(section, component_stage1, &mut instrumented_component);
            }

            Payload::ComponentCanonicalSection(reader) => {
                for function in reader {
                    match function? {
                        CanonicalFunction::Lower { .. }
                        | CanonicalFunction::ResourceNew { .. }
                        | CanonicalFunction::ResourceDrop { .. }
                        | CanonicalFunction::ResourceRep { .. } => {
                            core_function_count += 1;
                        }
                        CanonicalFunction::Lift { .. } => {
                            function_count += 1;
                        }
                        // Unused for now
                        _ => {}
                    }
                }
                copy_component_section(section, component_stage1, &mut instrumented_component);
            }

            Payload::ComponentImportSection(reader) => {
                for import in reader {
                    match import?.ty {
                        ComponentTypeRef::Func(_) => {
                            function_count += 1;
                        }
                        ComponentTypeRef::Type(_) => {
                            type_count += 1;
                        }
                        _ => (),
                    }
                }
                copy_component_section(section, component_stage1, &mut instrumented_component);
            }

            Payload::ComponentExportSection(reader) => {
                for export in reader {
                    match export?.kind {
                        ComponentExternalKind::Func => {
                            function_count += 1;
                        }
                        ComponentExternalKind::Type => {
                            type_count += 1;
                        }
                        _ => (),
                    }
                }
                copy_component_section(section, component_stage1, &mut instrumented_component);
            }

            Payload::ComponentTypeSection(reader) => {
                for _ in reader {
                    type_count += 1;
                }
                copy_component_section(section, component_stage1, &mut instrumented_component);
            }

            _ => copy_component_section(section, component_stage1, &mut instrumented_component),
        }
    }

    let mut types = TypeSection::new();
    let mut imports = ImportSection::new();
    let mut functions = FunctionSection::new();
    let mut exports = ExportSection::new();
    let mut code = CodeSection::new();
    let mut aliases = ComponentAliasSection::new();
    let mut lifts = CanonicalFunctionSection::new();
    let mut component_types = ComponentTypeSection::new();
    let mut component_exports = ComponentExportSection::new();

    for (module_index, module_globals) in &instrumentation.globals {
        for (global_export, ty) in module_globals.values() {
            let ty = Encode.val_type(*ty)?;
            let offset = types.len();
            types.ty().function([], [ty]);
            imports.import(
                &module_index.to_string(),
                &global_export.module_export(),
                GlobalType {
                    val_type: ty,
                    mutable: true,
                    shared: false,
                },
            );
            functions.function(offset);
            let mut function = Function::new([]);
            function.instruction(&Ins::GlobalGet(offset));
            function.instruction(&Ins::End);
            code.function(&function);
            let export_name = global_export.component_export();
            exports.export(&export_name, ExportKind::Func, offset);
            aliases.alias(Alias::CoreInstanceExport {
                instance: instance_count,
                kind: ExportKind::Func,
                name: &export_name,
            });
            component_types
                .function()
                .params(iter::empty::<(_, ComponentValType)>())
                .result(Some(ComponentValType::Primitive(match ty {
                    ValType::I32 => PrimitiveValType::S32,
                    ValType::I64 => PrimitiveValType::S64,
                    ValType::F32 => PrimitiveValType::F32,
                    ValType::F64 => PrimitiveValType::F64,
                    ValType::V128 => bail!("V128 not yet supported"),
                    ValType::Ref(_) => bail!("reference types not supported"),
                })));
            lifts.lift(
                core_function_count + offset,
                type_count + component_types.len() - 1,
                [CanonicalOption::UTF8],
            );
            component_exports.export(
                &export_name,
                ComponentExportKind::Func,
                function_count + offset,
                None,
            );
        }
    }

    if let Some(memory_info) = &instrumentation.memory {
        let offset = types.len();
        types.ty().function([], [wasm_encoder::ValType::I32]);
        imports.import(
            &memory_info.module_index.to_string(),
            &memory_info.export_name,
            Encode.entity_type(TypeRef::Memory(memory_info.ty))?,
        );
        functions.function(offset);

        let mut function = Function::new([(1, wasm_encoder::ValType::I32)]);
        function.instruction(&Ins::MemorySize(0));
        // stack[0] = current memory, in pages

        function.instruction(&Ins::I32Const(PAGE_SIZE_BYTES));
        function.instruction(&Ins::I32Mul);
        function.instruction(&Ins::LocalTee(0));
        // stack[0] = local[0] = current memory, in bytes

        function.instruction(&Ins::I32Const(1));
        function.instruction(&Ins::MemoryGrow(0));
        // stack[1] = old memory, in bytes
        // stack[0] = grown memory, in pages, or -1 if failed
        function.instruction(&Ins::I32Const(0));
        function.instruction(&Ins::I32LtS);
        function.instruction(&Ins::If(wasm_encoder::BlockType::Empty));
        // Trap if memory grow failed
        function.instruction(&Ins::Unreachable);
        function.instruction(&Ins::Else);
        function.instruction(&Ins::End);

        // stack[0] = old memory, in bytes
        function.instruction(&Ins::I32Const(0));
        // stack[1] = old memory in bytes
        // stack[0] = 0 (start of memory)
        function.instruction(&Ins::I32Store(mem_arg(0, 1)));
        // 0 stored at end of old memory
        function.instruction(&Ins::LocalGet(0));
        function.instruction(&Ins::LocalGet(0));
        // stack[1] = old memory in bytes
        // stack[0] = old memory in bytes
        function.instruction(&Ins::I32Store(mem_arg(4, 1)));
        // old memory size, stored at old memory + 4

        function.instruction(&Ins::LocalGet(0));
        // stack[0] = old memory in bytes
        function.instruction(&Ins::End);
        code.function(&function);

        let export_name = "component-init-get-memory".to_owned();
        exports.export(&export_name, ExportKind::Func, offset);
        aliases.alias(Alias::CoreInstanceExport {
            instance: instance_count,
            kind: ExportKind::Func,
            name: &export_name,
        });
        let list_type = type_count + component_types.len();
        component_types.defined_type().list(PrimitiveValType::U8);
        component_types
            .function()
            .params(iter::empty::<(_, ComponentValType)>())
            .result(Some(ComponentValType::Type(list_type)));
        lifts.lift(
            core_function_count + offset,
            type_count + component_types.len() - 1,
            [CanonicalOption::UTF8, CanonicalOption::Memory(0)],
        );
        component_exports.export(
            &export_name,
            ComponentExportKind::Func,
            function_count + offset,
            None,
        );
    }

    let mut instances = InstanceSection::new();
    instances.instantiate(
        module_count,
        instantiations
            .into_iter()
            .map(|(module_index, instance_index)| {
                (
                    module_index.to_string(),
                    ModuleArg::Instance(instance_index),
                )
            }),
    );

    let mut module = Module::new();
    module.section(&types);
    module.section(&imports);
    module.section(&functions);
    module.section(&exports);
    module.section(&code);

    instrumented_component.section(&ModuleSection(&module));
    instrumented_component.section(&instances);
    instrumented_component.section(&component_types);
    instrumented_component.section(&aliases);
    instrumented_component.section(&lifts);
    instrumented_component.section(&component_exports);

    // Next, invoke the provided `initialize` function, which will return a trait object through which we can
    // invoke the functions we added above to capture the state of the initialized instance.

    let instrumented_component = instrumented_component.finish();
    Ok((instrumented_component, instrumentation))
}

fn apply(
    measurement: Measurement,
    component_stage1: &[u8],
    component_stage2_and_map_module_index: Stage2Map<'_>,
) -> Result<Vec<u8>> {
    let (component_stage2, map_module_index) =
        component_stage2_and_map_module_index.unwrap_or((component_stage1, &convert::identity));
    let mut initialized_component = Component::new();
    let mut parser = Parser::new(0).parse_all(component_stage2);
    let mut module_count = 0;
    #[allow(clippy::while_let_on_iterator)]
    while let Some(payload) = parser.next() {
        let payload = payload?;
        let section = payload.as_section();
        match payload {
            Payload::ComponentSection {
                unchecked_range, ..
            } => {
                let mut subcomponent = Component::new();
                while let Some(payload) = parser.next() {
                    let payload = payload?;
                    let section = payload.as_section();
                    let my_range = section.as_ref().map(|(_, range)| range.clone());
                    copy_component_section(section, component_stage2, &mut subcomponent);

                    if let Some(my_range) = my_range
                        && my_range.end >= unchecked_range.end
                    {
                        break;
                    }
                }
                initialized_component.section(&NestedComponentSection(&subcomponent));
            }

            Payload::ModuleSection {
                unchecked_range, ..
            } => {
                let module_index = map_module_index(get_and_increment(&mut module_count));
                let mut initialized_module = Module::new();
                let mut global_count = 0;
                let (data_section, data_segment_count) = measurement.data_section(module_index);
                while let Some(payload) = parser.next() {
                    let payload = payload?;
                    let section = payload.as_section();
                    let my_range = section.as_ref().map(|(_, range)| range.clone());
                    match payload {
                        Payload::MemorySection(reader) => {
                            let mut memories = MemorySection::new();
                            for memory in reader {
                                let mut memory = memory?;

                                memory.initial = measurement
                                    .memory_initial(module_index)
                                    .expect("measurement for module's memory");

                                memories.memory(Encode.memory_type(memory)?);
                            }
                            initialized_module.section(&memories);
                        }

                        Payload::ImportSection(reader) => {
                            for import in reader {
                                if let TypeRef::Global(_) = import?.ty {
                                    global_count += 1;
                                }
                            }
                            copy_module_section(section, component_stage2, &mut initialized_module);
                        }

                        Payload::GlobalSection(reader) => {
                            let mut globals = GlobalSection::new();
                            for global in reader {
                                let global = global?;
                                let global_index = get_and_increment(&mut global_count);
                                globals.global(
                                    Encode.global_type(global.ty)?,
                                    &if global.ty.mutable {
                                        measurement
                                            .global_init(module_index, global_index)
                                            .expect("measurement for global")
                                    } else {
                                        Encode.const_expr(global.init_expr)?
                                    },
                                );
                            }
                            initialized_module.section(&globals);
                        }

                        Payload::DataSection(_) | Payload::StartSection { .. } => (),

                        Payload::DataCountSection { .. } => {
                            initialized_module.section(&DataCountSection {
                                count: data_segment_count,
                            });
                        }

                        _ => {
                            copy_module_section(section, component_stage2, &mut initialized_module)
                        }
                    }

                    if let Some(my_range) = my_range
                        && my_range.end >= unchecked_range.end
                    {
                        break;
                    }
                }
                if let Some(data_section) = data_section {
                    initialized_module.section(&data_section);
                }

                initialized_component.section(&ModuleSection(&initialized_module));
            }

            _ => copy_component_section(section, component_stage2, &mut initialized_component),
        }
    }

    let initialized_component = initialized_component.finish();

    let mut add = wasm_metadata::AddMetadata::default();
    add.processed_by = vec![(
        "component-init-transform".to_owned(),
        env!("CARGO_PKG_VERSION").to_owned(),
    )];

    let initialized_component = add.to_wasm(&initialized_component)?;

    Validator::new().validate_all(&initialized_component)?;

    Ok(initialized_component)
}

struct Segments<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> Segments<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, offset: 0 }
    }
}

impl<'a> Iterator for Segments<'a> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let mut zero_count = 0;
        let mut start = 0;
        let mut length = 0;
        for (index, value) in self.bytes[self.offset..].iter().enumerate() {
            if *value == 0 {
                zero_count += 1;
            } else {
                if zero_count > MAX_CONSECUTIVE_ZEROS {
                    if length > 0 {
                        start += self.offset;
                        self.offset += index;
                        return Some((start, length));
                    } else {
                        start = index;
                        length = 1;
                    }
                } else {
                    length += zero_count + 1;
                }
                zero_count = 0;
            }
        }
        if length > 0 {
            start += self.offset;
            self.offset = self.bytes.len();
            Some((start, length))
        } else {
            self.offset = self.bytes.len();
            None
        }
    }
}

fn get_and_increment(n: &mut u32) -> u32 {
    let v = *n;
    *n += 1;
    v
}

fn mem_arg(offset: u64, align: u32) -> MemArg {
    MemArg {
        offset,
        align,
        memory_index: 0,
    }
}

fn copy_component_section(
    section: Option<(u8, Range<usize>)>,
    component: &[u8],
    result: &mut Component,
) {
    if let Some((id, range)) = section {
        result.section(&RawSection {
            id,
            data: &component[range],
        });
    }
}

fn copy_module_section(section: Option<(u8, Range<usize>)>, module: &[u8], result: &mut Module) {
    if let Some((id, range)) = section {
        result.section(&RawSection {
            id,
            data: &module[range],
        });
    }
}
