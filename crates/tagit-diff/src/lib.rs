//! Implementation for the `tagit diff` command.

use std::process::Command;

use owo_colors::OwoColorize;
use tagit_core::out;
use tagit_workspace::{WorkspaceEntry, with_workspace_entries};

/// List workspace members whose sources differ from the tag they declare.
pub fn diff() -> anyhow::Result<()> {
    with_workspace_entries(
        false,
        false,
        |WorkspaceEntry {
             version,
             tag_prefix,
             paths,
             name,
             ..
         }| {
            let tag = &*format!("{tag_prefix}{version}");
            let no_diff = paths.is_empty()
                || Command::new("git")
                    .arg("diff-index")
                    .arg("--quiet")
                    .arg(tag)
                    .args(paths)
                    .status()?
                    .success();
            if !no_diff {
                out!("differs", "{}", name.purple());
                Command::new("git")
                    .arg("--no-pager")
                    .arg("diff")
                    .arg("-U0")
                    .arg("--cached")
                    .arg(tag)
                    .arg("--")
                    .args(paths)
                    .output()?;
            }
            Ok(())
        },
    )
}
