use std::sync::atomic::{AtomicBool, Ordering};

static IS_INITIALIZED: AtomicBool = AtomicBool::new(false);

#[component_init::init]
fn init() {
    let before = IS_INITIALIZED.swap(true, Ordering::Relaxed);
    assert!(!before, "component should only be initialized once");
}

fn main() {
    let initialized = IS_INITIALIZED.load(Ordering::Relaxed);
    assert!(initialized, "component was not initialized")
}
