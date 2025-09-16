use anyhow::{Context, Result, anyhow};
use test_programs_artifacts::{RAW, USING_MACRO};

async fn execute(component: &[u8]) -> Result<()> {
    use wasmtime::{
        Config, Engine, Store,
        component::{Component, Linker, ResourceTable},
    };
    use wasmtime_wasi::{WasiCtx, WasiCtxView};

    let mut config = Config::new();
    config.async_support(true);
    let engine = Engine::new(&config).context("create engine")?;
    let component = Component::new(&engine, component).context("load component")?;

    struct Ctx {
        table: ResourceTable,
        wasi: WasiCtx,
    }
    impl wasmtime_wasi::WasiView for Ctx {
        fn ctx(&mut self) -> WasiCtxView<'_> {
            WasiCtxView {
                ctx: &mut self.wasi,
                table: &mut self.table,
            }
        }
    }

    let mut linker = Linker::new(&engine);
    wasmtime_wasi::p2::add_to_linker_async(&mut linker).context("add wasi to linker")?;
    let mut store = Store::new(
        &engine,
        Ctx {
            table: ResourceTable::new(),
            wasi: WasiCtx::builder().inherit_stdout().inherit_stderr().build(),
        },
    );

    let instance = linker
        .instantiate_async(&mut store, &component)
        .await
        .context("instantiate")?;

    let wasi_cli_run = instance
        .get_export_index(&mut store, None, "wasi:cli/run@0.2.3")
        .ok_or_else(|| anyhow!("`wasi:cli/run` is not exported"))?;
    let export = instance
        .get_export_index(&mut store, Some(&wasi_cli_run), "run")
        .ok_or_else(|| anyhow!("`wasi:cli/run.run` is not exported"))?;
    let func = instance
        .get_func(&mut store, export)
        .ok_or_else(|| anyhow!("`wasi:cli/run.run` export is not a func"))?;
    let func = func
        .typed::<(), (Result<(), ()>,)>(&mut store)
        .context("type of run func")?;
    func.call_async(&mut store, ())
        .await
        .context("executing run")?
        .0
        .map_err(|()| anyhow!("run returned error"))?;
    func.post_return_async(&mut store)
        .await
        .context("post-return run")?;
    Ok(())
}

async fn inits_properly(component: &[u8]) -> Result<()> {
    // Without initialization, run will trap.
    let err = execute(component)
        .await
        .err()
        .context("uninitialized run should trap")?;
    assert!(
        format!("{err:?}").contains("unreachable"),
        "should die with an unreachable trap, got: {err:?}"
    );

    let initialized_component = component_init_wasmtime::initialize(component).await?;

    // After initialization, will not trap.
    execute(&initialized_component)
        .await
        .context("execute initialized component")?;

    Ok(())
}

#[tokio::test]
async fn raw() -> Result<()> {
    println!("test component: {RAW:?}");
    let component = std::fs::read(RAW)?;
    inits_properly(&component).await
}

#[tokio::test]
async fn using_macro() -> Result<()> {
    println!("test component: {USING_MACRO:?}");
    let component = std::fs::read(USING_MACRO)?;
    inits_properly(&component).await
}
