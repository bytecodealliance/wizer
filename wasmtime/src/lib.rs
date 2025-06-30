use anyhow::{Context, Result, anyhow};
use component_init::Invoker;
use wasmtime::{
    Config, Engine, Store,
    component::{Component, ComponentNamedList, Instance, Lift, Linker},
};

pub async fn initialize(component: &[u8]) -> Result<Vec<u8>> {
    component_init::initialize(component, |instrumented| {
        Box::pin(async move {
            let i = invoker(instrumented)
                .await
                .context("running instrumented component")?;
            let i: Box<dyn Invoker> = Box::new(i);
            Ok(i)
        })
    })
    .await
}

async fn invoker(component: Vec<u8>) -> Result<Impl> {
    let mut config = Config::new();
    config.async_support(true);
    let engine = Engine::new(&config).context("creating engine")?;
    let component =
        Component::new(&engine, &component).context("compiling instrumented component")?;
    let mut linker = Linker::new(&engine);
    linker
        .define_unknown_imports_as_traps(&component)
        .context("link unknown imports as traps")?;
    let mut store = Store::new(&engine, Ctx);
    let instance = linker
        .instantiate_async(&mut store, &component)
        .await
        .context("instantiate")?;
    let mut this = Impl { instance, store };
    this.call::<()>("component-init")
        .await
        .context("running the component-init export func")?;
    Ok(this)
}

pub struct Ctx;

struct Impl {
    instance: Instance,
    store: Store<Ctx>,
}

impl Impl {
    async fn call<T: ComponentNamedList + Lift + Send + Sync>(&mut self, name: &str) -> Result<T> {
        let export = self
            .instance
            .get_export_index(&mut self.store, None, name)
            .ok_or_else(|| anyhow!("{name} is not exported"))?;
        let func = self
            .instance
            .get_func(&mut self.store, export)
            .ok_or_else(|| anyhow!("{name} export is not a func"))?;
        let func = func
            .typed::<(), T>(&mut self.store)
            .with_context(|| format!("type of {name} func"))?;
        let r = func
            .call_async(&mut self.store, ())
            .await
            .with_context(|| format!("executing {name}"))?;
        func.post_return_async(&mut self.store)
            .await
            .with_context(|| format!("post-return {name}"))?;
        Ok(r)
    }
}

#[async_trait::async_trait]
impl Invoker for Impl {
    async fn call_s32(&mut self, name: &str) -> Result<i32> {
        Ok(self.call::<(i32,)>(name).await?.0)
    }
    async fn call_s64(&mut self, name: &str) -> Result<i64> {
        Ok(self.call::<(i64,)>(name).await?.0)
    }
    async fn call_f32(&mut self, name: &str) -> Result<f32> {
        Ok(self.call::<(f32,)>(name).await?.0)
    }
    async fn call_f64(&mut self, name: &str) -> Result<f64> {
        Ok(self.call::<(f64,)>(name).await?.0)
    }
    async fn call_list_u8(&mut self, name: &str) -> Result<Vec<u8>> {
        Ok(self.call::<(Vec<u8>,)>(name).await?.0)
    }
}
