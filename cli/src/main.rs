use anyhow::{Context, Result};
use clap::Parser;
use std::path::PathBuf;

/// Initialize a component. Saves the internal state of the component
/// after its `component-init` export function has been called.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Input component.
    #[arg(required = true)]
    input: PathBuf,

    /// Output component. If not provided, output will overwrite input.
    #[arg(short, long)]
    output: Option<PathBuf>,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    let infile = &args.input;
    let input = std::fs::read(infile).with_context(|| format!("reading input from {infile:?}"))?;
    let output = component_init_wasmtime::initialize(&input).await?;

    let outfile = args.output.as_ref().unwrap_or(infile);
    std::fs::write(outfile, output).with_context(|| format!("writing output to {outfile:?}"))?;
    Ok(())
}
