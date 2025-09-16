use anyhow::{Context, Result, anyhow};

#[tokio::test]
async fn init_memory() -> Result<()> {
    // This component exports a `component-init` which initializes a core memory,
    // and a `run` which traps unless initialization has occured.
    let wat_component = r#"
        (component
            (core module $m
                (memory (export "memory") 1)
                ;; assert mem location 0 is 0, set it to 1
                (func (export "component-init")
                    (i32.load (i32.const 0))
                    if (unreachable) else end
                    (i32.store (i32.const 0) (i32.const 1))
                )
                ;; assert mem location 0 is 1
                (func (export "run")
                    (i32.eq (i32.const 0) (i32.load (i32.const 0)))
                    if (unreachable) else end
                )
            )
            (core instance $i (instantiate $m))
            (func (export "component-init")
                (canon lift (core func $i "component-init")))
            (func (export "run")
                (canon lift (core func $i "run")))

            ;; component_init needs the memory aliased in the component at index 0
            (alias core export $i "memory" (core memory (;0;)))
        )
    "#;
    test(wat_component).await
}

#[tokio::test]
async fn init_global() -> Result<()> {
    // This component exports a `component-init` which initializes a global,
    // and a `run` which traps unless initialization has occured.
    let wat_component = r#"
        (component
            (core module $m
                (global $g (mut i32) (i32.const 0))
                ;; assert $g is 0, set it to 1
                (func (export "component-init")
                    (global.get $g)
                    if (unreachable) else end
                    (global.set $g (i32.const 1))
                )
                ;; assert $g is 1
                (func (export "run")
                    (i32.eq (i32.const 0) (global.get $g))
                    if (unreachable) else end
                )
            )
            (core instance $i (instantiate $m))
            (func (export "component-init")
                (canon lift (core func $i "component-init")))
            (func (export "run")
                (canon lift (core func $i "run")))
        )
    "#;
    test(wat_component).await
}

async fn test(wat_component: &str) -> Result<()> {
    // component-init only supports component binaries, not wat form
    let component = wat::parse_str(wat_component)?;

    // Without initialization, run will trap.
    let err = execute(&component, "run")
        .await
        .err()
        .context("uninitialized run should trap")?;
    assert!(
        format!("{err:?}").contains("unreachable"),
        "should die with an unreachable trap, got: {err:?}"
    );

    // Initialize the component. This will execute component-init on the base component.
    let initialized_component = component_init_wasmtime::initialize(&component).await?;

    // After initialization, run will not trap.
    execute(&initialized_component, "run")
        .await
        .context("initialized component should not trap in `run`")?;

    // After initialization, component-init will trap.
    let err = execute(&initialized_component, "component-init")
        .await
        .err()
        .context("calling component-init again should trap")?;
    assert!(
        format!("{err:?}").contains("unreachable"),
        "should die with an unreachable trap, got: {err:?}"
    );

    Ok(())
}

async fn execute(component: &[u8], name: &str) -> Result<()> {
    use wasmtime::{
        Config, Engine, Store,
        component::{Component, Linker},
    };

    let mut config = Config::new();
    config.async_support(true);
    let engine = Engine::new(&config).context("create engine")?;
    let component = Component::new(&engine, component).context("load component")?;
    let mut linker = Linker::new(&engine);
    linker
        .define_unknown_imports_as_traps(&component)
        .context("unknown imports as traps")?;
    let mut store = Store::new(&engine, ());

    let instance = linker
        .instantiate_async(&mut store, &component)
        .await
        .context("instantiate")?;

    let export = instance
        .get_export_index(&mut store, None, name)
        .ok_or_else(|| anyhow!("{name} is not exported"))?;
    let func = instance
        .get_func(&mut store, export)
        .ok_or_else(|| anyhow!("{name} export is not a func"))?;
    let func = func
        .typed::<(), ()>(&mut store)
        .with_context(|| format!("type of {name} func"))?;
    func.call_async(&mut store, ())
        .await
        .with_context(|| format!("executing {name}"))?;
    func.post_return_async(&mut store)
        .await
        .with_context(|| format!("post-return {name}"))?;
    Ok(())
}
