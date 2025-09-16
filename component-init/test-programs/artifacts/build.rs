use heck::ToShoutySnakeCase;
use std::env::var_os;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let out_dir = PathBuf::from(var_os("OUT_DIR").expect("OUT_DIR env var exists"));

    let meta = cargo_metadata::MetadataCommand::new()
        .exec()
        .expect("cargo metadata");
    let test_programs_meta = meta
        .packages
        .iter()
        .find(|p| p.name == "test-programs")
        .expect("test-programs is in cargo metadata");
    let test_programs_root = test_programs_meta.manifest_path.parent().unwrap();
    println!(
        "cargo:rerun-if-changed={}",
        test_programs_root.as_os_str().to_str().unwrap()
    );

    let status = Command::new("cargo")
        .arg("build")
        .arg("--target=wasm32-wasip2")
        .arg("--package=test-programs")
        .env("CARGO_TARGET_DIR", &out_dir)
        .env("CARGO_PROFILE_DEV_DEBUG", "2")
        .env("RUSTFLAGS", rustflags())
        .env_remove("CARGO_ENCODED_RUSTFLAGS")
        .status()
        .expect("cargo build test programs");
    assert!(status.success());

    let mut generated_code = "// THIS FILE IS GENERATED CODE\n".to_string();

    for binary in test_programs_meta
        .targets
        .iter()
        .filter(|t| t.kind == ["bin"])
    {
        let component_path = out_dir
            .join("wasm32-wasip2")
            .join("debug")
            .join(format!("{}.wasm", binary.name));

        let const_name = binary.name.to_shouty_snake_case();
        generated_code += &format!(
            "pub const {const_name}: &str = {:?};\n",
            component_path.as_os_str().to_str().expect("path is str")
        );
    }

    std::fs::write(out_dir.join("gen.rs"), generated_code).unwrap();
}

fn rustflags() -> &'static str {
    match option_env!("RUSTFLAGS") {
        Some(s) if s.contains("-D warnings") => "-D warnings",
        _ => "",
    }
}
