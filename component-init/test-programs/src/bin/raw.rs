use std::sync::atomic::{AtomicBool, Ordering};

wit_bindgen::generate!({
    inline: r"
    package this:wit;
    world w {
        export component-init: func();
    }"
});

static IS_INITIALIZED: AtomicBool = AtomicBool::new(false);

struct S;
impl Guest for S {
    fn component_init() {
        let before = IS_INITIALIZED.swap(true, Ordering::Relaxed);
        assert!(!before, "component should only be initialized once");
    }
}

export!(S);

fn main() {
    let initialized = IS_INITIALIZED.load(Ordering::Relaxed);
    assert!(initialized, "component was not initialized")
}
