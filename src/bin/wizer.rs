use std::fs;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;
use wasmtime::error::Context;
use wizer::Wizer;

#[derive(Parser)]
pub struct Options {
    /// The input Wasm module's file path.
    ///
    /// If not specified, then `stdin` is used.
    input: Option<PathBuf>,

    /// The file path to write the output Wasm module to.
    ///
    /// If not specified, then `stdout` is used.
    #[arg(short = 'o')]
    output: Option<PathBuf>,

    #[command(flatten)]
    wizer: Wizer,

    /// Allow WASI imports to be called during initialization.
    ///
    /// This can introduce diverging semantics because the initialization can
    /// observe nondeterminism that might have gone a different way at runtime
    /// than it did at initialization time.
    ///
    /// If your Wasm module uses WASI's `get_random` to add randomness to
    /// something as a security mitigation (e.g. something akin to ASLR or the
    /// way Rust's hash maps incorporate a random nonce) then note that, if the
    /// randomization is added during initialization time and you don't ever
    /// re-randomize at runtime, then that randomization will become per-module
    /// rather than per-instance.
    #[arg(long = "allow-wasi")]
    allow_wasi: bool,

    /// Use deterministic behavior for relaxed SIMD instructions.
    ///
    /// The relaxed SIMD instructions in Wasm are instructions which are
    /// permitted to have different results when run on different host
    /// CPU architectures. This flag tells wizer to instead execute relaxed
    /// SIMD instructions according to the [deterministic profile], which
    /// ensures that they're deterministic and platform-independent.
    ///
    /// [deterministic profile]: https://webassembly.github.io/spec/core/appendix/profiles.html#deterministic-profile-small-mathrm-det
    #[arg(long, require_equals = true, value_name = "true|false")]
    relaxed_simd_deterministic: Option<Option<bool>>,

    /// Provide an additional preloaded module that is available to the
    /// main module.
    ///
    /// This allows running a module that depends on imports from
    /// another module. Note that the additional module's state is *not*
    /// snapshotted, nor is its code included in the Wasm snapshot;
    /// rather, it is assumed that the resulting snapshot Wasm will also
    /// be executed with the same imports available.
    ///
    /// The main purpose of this option is to allow "stubs" for certain
    /// intrinsics to be included, when these will be provided with
    /// different implementations when running or further processing the
    /// snapshot.
    ///
    /// The format of this option is `name=file.{wasm,wat}`; e.g.,
    /// `intrinsics=stubs.wat`. Multiple instances of the option may
    /// appear.
    #[arg(long = "preload")]
    preload: Vec<String>,

    /// When using WASI during initialization, should `stdin`, `stderr`, and
    /// `stdout` be inherited?
    ///
    /// This is true by default.
    #[arg(long = "inherit-stdio", value_name = "true|false")]
    inherit_stdio: Option<Option<bool>>,

    /// When using WASI during initialization, should environment variables be
    /// inherited?
    ///
    /// This is false by default.
    #[arg(long = "inherit-env", value_name = "true|false")]
    inherit_env: Option<Option<bool>>,

    /// When using WASI during initialization, which file system directories
    /// should be made available?
    ///
    /// None are available by default.
    #[arg(long = "dir", value_name = "directory")]
    dirs: Vec<String>,

    /// When using WASI during initialization, which guest directories should be
    /// mapped to a host directory?
    ///
    /// The `--mapdir` option differs from `--dir` in that it allows giving a
    /// custom guest name to the directory that is different from its name in
    /// the host.
    ///
    /// None are mapped by default.
    #[
        arg(long = "mapdir", value_name = "GUEST_DIR::HOST_DIR", value_parser = parse_map_dirs)
    ]
    map_dirs: Vec<(String, PathBuf)>,

    /// Enable or disable Wasm multi-memory proposal.
    ///
    /// Enabled by default.
    #[arg(long, value_name = "true|false")]
    wasm_multi_memory: Option<Option<bool>>,

    /// Enable or disable the Wasm multi-value proposal.
    ///
    /// Enabled by default.
    #[arg(long, value_name = "true|false")]
    wasm_multi_value: Option<Option<bool>>,

    /// Enable or disable Wasm bulk memory operations.
    ///
    /// Note that only `memory.copy`, `memory.fill`, and `memory.init` operations
    /// are currently supported.  Modules which use other instructions, such as
    /// `table.copy` will be rejected.
    ///
    /// Enabled by default.
    #[arg(long, value_name = "true|false")]
    wasm_bulk_memory: Option<Option<bool>>,

    /// Enable or disable the Wasm SIMD128 proposal.
    ///
    /// Enabled by default.
    #[arg(long, value_name = "true|false")]
    wasm_simd: Option<Option<bool>>,

    /// Enable or disable the Wasm relaxed SIMD proposal.
    ///
    /// Disabled by default. When enabled, by default relaxed SIMD instructions
    /// will produce different results on different platforms. For deterministic
    /// results, additionally enable the `--relaxed-simd-deterministic` flag.
    #[arg(long, value_name = "true|false")]
    wasm_relaxed_simd: Option<Option<bool>>,

    /// Enable or disable the Wasm reference-types proposal.
    ///
    /// Currently does not implement snapshotting or the use of references,
    /// but enables initializing Wasm modules that use encodings introduced
    /// in the reference-types proposal.
    ///
    /// Enabled by default.
    #[arg(long, value_name = "true|false")]
    wasm_reference_types: Option<Option<bool>>,
}

fn parse_map_dirs(s: &str) -> Result<(String, PathBuf)> {
    let mut parts = s.split("::");
    let host = parts.next().unwrap();
    let guest = match parts.next() {
        Some(guest) => guest,
        None => host,
    };
    Ok((host.into(), guest.into()))
}

fn optional_flag_with_default(flag: Option<Option<bool>>, default: bool) -> bool {
    match flag {
        None => default,
        Some(None) => true,
        Some(Some(val)) => val,
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();
    let options = Options::parse();

    let stdin = io::stdin();
    let mut input: Box<dyn BufRead> = if let Some(input) = options.input.as_ref() {
        Box::new(io::BufReader::new(
            fs::File::open(input).context("failed to open input file")?,
        ))
    } else {
        Box::new(stdin.lock())
    };

    let mut output: Box<dyn Write> = if let Some(output) = options.output.as_ref() {
        Box::new(io::BufWriter::new(
            fs::File::create(output).context("failed to create output file")?,
        ))
    } else {
        Box::new(io::stdout())
    };

    let mut input_wasm = vec![];
    input
        .read_to_end(&mut input_wasm)
        .context("failed to read input Wasm module")?;

    let mut wasi = wasmtime_wasi::WasiCtxBuilder::new();

    if optional_flag_with_default(options.inherit_stdio, true) {
        wasi.inherit_stdio();
    }
    if optional_flag_with_default(options.inherit_env, false) {
        wasi.inherit_env();
    }
    for dir in &options.dirs {
        wasi.preopened_dir(
            dir,
            dir,
            wasmtime_wasi::DirPerms::all(),
            wasmtime_wasi::FilePerms::all(),
        )?;
    }
    for (guest, host) in &options.map_dirs {
        wasi.preopened_dir(
            host,
            guest,
            wasmtime_wasi::DirPerms::all(),
            wasmtime_wasi::FilePerms::all(),
        )?;
    }

    let mut config = wasmtime::Config::new();
    config.relaxed_simd_deterministic(optional_flag_with_default(
        options.relaxed_simd_deterministic,
        true,
    ));
    config.wasm_multi_memory(optional_flag_with_default(options.wasm_multi_memory, true));
    config.wasm_multi_value(optional_flag_with_default(options.wasm_multi_value, true));
    config.wasm_bulk_memory(optional_flag_with_default(options.wasm_bulk_memory, true));
    config.wasm_simd(optional_flag_with_default(options.wasm_simd, true));
    config.wasm_relaxed_simd(optional_flag_with_default(options.wasm_relaxed_simd, true));
    config.wasm_reference_types(optional_flag_with_default(
        options.wasm_reference_types,
        true,
    ));

    let engine = wasmtime::Engine::new(&config)?;
    let mut store = wasmtime::Store::new(&engine, wasi.build_p1());

    let mut linker = wasmtime::Linker::new(store.engine());
    if options.allow_wasi {
        wasmtime_wasi::p1::add_to_linker_sync(&mut linker, |x| x)?;
    }

    for preload in options.preload.iter() {
        if let Some((name, value)) = preload.split_once('=') {
            let module = wasmtime::Module::from_file(&engine, value)
                .context("failed to parse preload module")?;
            let instance = linker
                .instantiate(&mut store, &module)
                .context("failed to instantiate preload module")?;
            linker
                .instance(&mut store, name, instance)
                .context("failed to add preload's exports to linker")?;
        } else {
            anyhow::bail!(
                "Bad preload option: {} (must be of form `name=file`)",
                preload
            );
        }
    }

    let output_wasm = options
        .wizer
        .run(&mut store, &input_wasm, async |store, module| {
            linker.define_unknown_imports_as_traps(module)?;
            linker.instantiate(store, module)
        })
        .await?;

    output
        .write_all(&output_wasm)
        .context("failed to write to output")?;

    Ok(())
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Options::command().debug_assert()
}
