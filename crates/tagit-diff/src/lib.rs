//! Implementation for the `tagit diff` command.

use std::{io::Write, process::Command};

use owo_colors::OwoColorize;
use tagit_core::{Tagit, out};
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
                || !Tagit::exists(tag)?
                || Command::new("git")
                    .arg("diff-index")
                    .arg("--quiet")
                    .arg(tag)
                    .args(paths)
                    .status()?
                    .success();
            if !no_diff {
                out!("differs", "{}", name.purple());
                let stdout = Command::new("git")
                    .arg("--no-pager")
                    .arg("diff")
                    .arg("--color")
                    .arg("-U0")
                    .arg(tag)
                    .arg("--")
                    .args(paths)
                    .output()?
                    .stdout;
                std::io::stdout().write_all(&stdout)?;
            }
            Ok(())
        },
    )
}
