use anyhow::{anyhow, Result};
use wasmtime::error::Context as _;
use wasmtime_wasi::WasiCtxBuilder;
use wat::parse_str as wat_to_wasm;
use wizer::Wizer;

fn get_wizer() -> Wizer {
    let mut wizer = Wizer::new();
    wizer.init_func("wizer.initialize");
    wizer
}

async fn run_wasm(args: &[wasmtime::Val], expected: i32, wasm: &[u8]) -> Result<()> {
    let _ = env_logger::try_init();

    let mut config = wasmtime::Config::new();
    wasmtime::Cache::from_file(None)
        .map(|cache| config.cache(Some(cache)))
        .unwrap();
    config.wasm_multi_memory(true);
    config.wasm_multi_value(true);

    let engine = wasmtime::Engine::new(&config)?;
    let wasi_ctx = WasiCtxBuilder::new().build_p1();
    let mut store = wasmtime::Store::new(&engine, wasi_ctx);

    let wasm = get_wizer()
        .run(&mut store, wasm, async |store, module| {
            let mut linker = wasmtime::Linker::new(store.engine());
            linker.func_wrap("foo", "bar", |x: i32| x + 1)?;
            linker.instantiate(store, module)
        })
        .await
        .map_err(|e| anyhow!(e))?;

    log::debug!(
        "=== Wizened Wasm ==========================================================\n\
       {}\n\
       ===========================================================================",
        wasmprinter::print_bytes(&wasm).unwrap()
    );
    if log::log_enabled!(log::Level::Debug) {
        std::fs::write("test.wasm", &wasm).unwrap();
    }

    let module =
        wasmtime::Module::new(store.engine(), wasm).context("Wasm test case failed to compile")?;

    let mut linker = wasmtime::Linker::new(&engine);
    linker.func_wrap("foo", "bar", |_: i32| -> wasmtime::error::Result<i32> {
        wasmtime::error::bail!("shouldn't be called")
    })?;

    let instance = linker.instantiate(&mut store, &module)?;

    let run = instance
        .get_func(&mut store, "run")
        .ok_or_else(|| anyhow::anyhow!("the test Wasm module does not export a `run` function"))?;

    let mut actual = vec![wasmtime::Val::I32(0)];
    run.call(&mut store, args, &mut actual)
        .context("failed to call wasm run fn")?;
    anyhow::ensure!(actual.len() == 1, "expected one result");
    let actual = match actual[0] {
        wasmtime::Val::I32(x) => x,
        _ => anyhow::bail!("expected an i32 result"),
    };
    anyhow::ensure!(
        expected == actual,
        "expected `{}`, found `{}`",
        expected,
        actual,
    );

    Ok(())
}

async fn run_wat(args: &[wasmtime::Val], expected: i32, wat: &str) -> Result<()> {
    let _ = env_logger::try_init();
    let wasm = wat_to_wasm(wat)?;
    run_wasm(args, expected, &wasm).await
}

#[tokio::test]
async fn custom_linker() -> Result<()> {
    run_wat(
        &[],
        1,
        r#"
(module
  (type (func (param i32) (result i32)))
  (import "foo" "bar" (func (type 0)))
  (global $g (mut i32) (i32.const 0))
  (func (export "wizer.initialize")
    global.get $g
    call 0
    global.set $g
  )
  (func (export "run") (result i32)
    (global.get $g)
  )
)"#,
    )
    .await
}
