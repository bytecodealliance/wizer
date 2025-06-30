use anyhow::{Context, Result, anyhow};
use test_programs_artifacts::TEST;

async fn execute(component: &[u8]) -> Result<()> {
    use wasmtime::{
        Config, Engine, Store,
        component::{Component, Linker, ResourceTable},
    };
    use wasmtime_wasi::p2::WasiCtx;

    let mut config = Config::new();
    config.async_support(true);
    let engine = Engine::new(&config).context("create engine")?;
    let component = Component::new(&engine, component).context("load component")?;

    struct Ctx {
        table: ResourceTable,
        wasi: WasiCtx,
    }
    impl wasmtime_wasi::p2::IoView for Ctx {
        fn table(&mut self) -> &mut ResourceTable {
            &mut self.table
        }
    }
    impl wasmtime_wasi::p2::WasiView for Ctx {
        fn ctx(&mut self) -> &mut WasiCtx {
            &mut self.wasi
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
        .with_context(|| format!("type of run func"))?;
    func.call_async(&mut store, ())
        .await
        .with_context(|| format!("executing run"))?
        .0
        .map_err(|()| anyhow!("run returned error"))?;
    func.post_return_async(&mut store)
        .await
        .with_context(|| format!("post-return run"))?;
    Ok(())
}

#[tokio::test]
async fn init_rust() -> Result<()> {
    println!("test component: {TEST:?}");

    let component = std::fs::read(TEST)?;

    // Without initialization, run will trap.
    let err = execute(&component)
        .await
        .err()
        .context("uninitialized run should trap")?;
    assert!(
        format!("{err:?}").contains("unreachable"),
        "should die with an unreachable trap, got: {err:?}"
    );

    let initialized_component = component_init_wasmtime::initialize(&component).await?;

    execute(&initialized_component)
        .await
        .context("execute initialized component")?;

    Ok(())
}
