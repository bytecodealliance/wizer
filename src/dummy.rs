//! Dummy implementations of things that a Wasm module can import.
//!
//! Forked from `wasmtime/crates/fuzzing/src/oracles/dummy.rs`.

use anyhow::{anyhow, Result};
use wasmtime::*;

/// Create dummy imports for instantiating the module.
pub fn dummy_imports(
    store: &mut crate::Store,
    module: &wasmtime::Module,
    linker: &mut crate::Linker,
) -> Result<()> {
    log::debug!("Creating dummy imports");

    for imp in module.imports() {
        let name = imp.name();
        if linker.get(&mut *store, imp.module(), name).is_some() {
            // Already defined, must be part of WASI.
            continue;
        }

        linker
            .define(
                imp.module(),
                name,
                dummy_extern(
                    &mut *store,
                    imp.ty(),
                    &format!("'{}' '{}'", imp.module(), name),
                )?,
            )
            .unwrap();
    }

    Ok(())
}

/// Construct a dummy `Extern` from its type signature
pub fn dummy_extern(store: &mut crate::Store, ty: ExternType, name: &str) -> Result<Extern> {
    Ok(match ty {
        ExternType::Func(func_ty) => Extern::Func(dummy_func(store, func_ty, name)),
        ExternType::Global(_) => {
            anyhow::bail!("Error: attempted to import unknown global: {}", name)
        }
        ExternType::Table(_) => anyhow::bail!("Error: attempted to import unknown table: {}", name),
        ExternType::Memory(_) => {
            anyhow::bail!("Error: attempted to import unknown memory: {}", name)
        }
    })
}

/// Construct a dummy function for the given function type
pub fn dummy_func(store: &mut crate::Store, ty: FuncType, name: &str) -> Func {
    let name = name.to_string();
    Func::new(store, ty.clone(), move |_caller, _params, _results| {
        Err(anyhow!(
            "Error: attempted to call an unknown imported function: {}\n\
             \n\
             You cannot call arbitrary imported functions during Wizer initialization.",
            name,
        ))
    })
}

/// Construct a dummy value for the given value type.
#[cfg(fuzzing)]
pub fn dummy_value(val_ty: ValType) -> Val {
    match val_ty {
        ValType::I32 => Val::I32(0),
        ValType::I64 => Val::I64(0),
        ValType::F32 => Val::F32(0),
        ValType::F64 => Val::F64(0),
        ValType::V128 => Val::V128(0),
        ValType::ExternRef => Val::ExternRef(None),
        ValType::FuncRef => Val::FuncRef(None),
    }
}

/// Construct a sequence of dummy values for the given types.
#[cfg(fuzzing)]
pub fn dummy_values(val_tys: impl IntoIterator<Item = ValType>) -> Vec<Val> {
    val_tys.into_iter().map(dummy_value).collect()
}
