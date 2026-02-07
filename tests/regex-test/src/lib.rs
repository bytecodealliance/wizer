use regex::Regex;
use std::sync::OnceLock;

/// A regex that matches numbers that start with "1".
static REGEX: OnceLock<Regex> = OnceLock::new();

#[export_name = "wizer.initialize"]
pub fn init() {
    REGEX.get_or_init(|| Regex::new(r"^1\d*$").expect("failed to compile regex"));
}

#[no_mangle]
pub fn run(n: i32) -> i32 {
    let s = format!("{}", n);
    let regex = REGEX
        .get()
        .expect("regex should have been initialized at wizering");
    if regex.is_match(&s) {
        42
    } else {
        0
    }
}
