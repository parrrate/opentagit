//! Implementation of `tagit` CLI.

use clap_complete::{generate, shells::Bash};
use clap_mangen::generate_to;
#[cfg(feature = "changelog")]
use tagit_changelog_command::ChangelogCommand;
use tagit_command::Command;

/// Handle [`Command`].
pub fn run(
    tagit_package: &'static str,
    tagit_version: &'static str,
    command: Command,
    to_command: impl FnOnce() -> clap::Command,
) -> anyhow::Result<()> {
    match command {
        #[cfg(feature = "tag")]
        Command::Tag {
            dry_run,
            no_retag,
            total_order,
            sign,
        } => tagit_tag::tag(
            tagit_package,
            tagit_version,
            dry_run,
            no_retag,
            total_order,
            sign,
        ),
        #[cfg(feature = "sub")]
        Command::Sub { command } => tagit_sub::sub(command.unwrap_or_default()),
        #[cfg(feature = "changelog")]
        Command::Changelog { dry_run, command } => match command.unwrap_or_default() {
            ChangelogCommand::Bump => tagit_workspace_changelog::bump_changelog(dry_run),
            ChangelogCommand::Init => tagit_workspace_changelog::init_changelog(dry_run),
        },
        #[cfg(feature = "diff")]
        Command::Diff { short } => tagit_diff::diff(short),
        Command::Completions => {
            generate(
                Bash,
                &mut to_command(),
                env!("CARGO_PKG_NAME"),
                &mut std::io::stdout(),
            );
            Ok(())
        }
        Command::Doc => {
            std::fs::create_dir_all("target/man1")?;
            generate_to(to_command(), "target/man1")?;
            Ok(())
        }
        #[cfg(not(feature = "tag"))]
        Command::Tag { .. } => {
            let _ = tagit_package;
            let _ = tagit_version;
            anyhow::bail!("enable `tag` feature")
        }
        #[cfg(not(feature = "sub"))]
        Command::Sub { .. } => anyhow::bail!("enable `sub` feature"),
        #[cfg(not(feature = "changelog"))]
        Command::Changelog { .. } => anyhow::bail!("enable `changelog` feature"),
        #[cfg(not(feature = "diff"))]
        Command::Diff => anyhow::bail!("enable `diff` feature"),
    }
}
