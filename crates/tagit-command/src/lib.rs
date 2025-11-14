//! `clap` part of `tagit`.

use clap::Subcommand;
use tagit_sub_command::SubtreeCommand;

/// Commands passed to `tagit` CLI.
#[derive(Subcommand)]
pub enum Command {
    /// Automatically push tags for workspace packages
    ///
    /// For root package: X.Y.Z
    ///
    /// For subpackages: package/X.Y.Z
    ///
    /// Requires signing
    Tag {
        #[arg(long)]
        dry_run: bool,
        #[arg(long)]
        no_retag: bool,
        #[arg(long)]
        total_order: bool,
        #[arg(long)]
        sign: Option<bool>,
    },
    /// Manage subtrees in the .tagit/sub/ directory
    ///
    /// Without a subcomand, equivalent to `tagit sub ls`
    Sub {
        #[command(subcommand)]
        command: Option<SubtreeCommand>,
    },
    Changelog {
        #[arg(long)]
        dry_run: bool,
    },
    /// Bash completions
    ///
    /// tagit completions > /usr/share/bash-completion/completions/tagit
    Completions,
    /// Generate docs
    ///
    /// Writes manpages to ./target/man1
    Doc,
}
