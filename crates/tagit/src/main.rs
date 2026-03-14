//! <https://tagit.parrrate.com/>

#[cfg(feature = "workspaces")]
extern crate tagit_workspace_cargo;
#[cfg(feature = "workspaces")]
extern crate tagit_workspace_npm;
#[cfg(feature = "workspaces")]
extern crate tagit_workspace_pyproject;

use clap::{CommandFactory, Parser};

#[derive(Parser)]
#[command(version, about)]
struct Args {
    #[command(subcommand)]
    command: tagit_command::Command,
}

#[doc(hidden)]
pub fn main() -> anyhow::Result<()> {
    let Args { command } = Args::parse();
    tagit_impl::run(
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
        command,
        Args::command,
    )
}
