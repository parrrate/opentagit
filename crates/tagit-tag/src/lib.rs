//! Implementation for the `tagit tag` command.

use std::process::Command;

use anyhow::Context;
use tagit_core::{Okie, Tagit, out};
use tagit_workspace::{WorkspaceEntry, with_workspace_entries};

fn upstream_remote() -> anyhow::Result<String> {
    let output = Command::new("git")
        .arg("rev-parse")
        .arg("--abbrev-ref")
        .arg("@{upstream}")
        .output()?;
    output.status.okie()?;
    let stdout = String::from_utf8(output.stdout)?;
    let (remote, _) = stdout
        .trim()
        .split_once('/')
        .context("no `/` in upstream")?;
    Ok(remote.into())
}

/// Update tags based on workspace members' project manifests.
pub fn tag(dry_run: bool, no_retag: bool, total_order: bool) -> anyhow::Result<()> {
    let branch = Tagit::current_branch()?;
    out!("found branch", "{branch}");
    let remote = upstream_remote()?;
    out!("found remote", "{remote}");
    let tagit = Tagit::new(&remote, env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))?
        .with_total_order(total_order);
    with_workspace_entries(
        dry_run,
        true,
        |WorkspaceEntry {
             version,
             root,
             name,
             tag_prefix,
             skip_retag,
             ..
         }| {
            let msg = format!("{name} {version}");
            let extended_msg = || {
                let mut msg = msg.clone();
                #[cfg(feature = "changelog")]
                if let Some(changelog) =
                    tagit_changelog::version_changelog(version.clone(), root, tag_prefix)?
                {
                    let w = msg.len();
                    msg += "\n";
                    msg += &msg.clone();
                    msg += &"=".repeat(w);
                    msg += "\n\n";
                    msg += &changelog;
                    msg += "\n---\n\n";
                    out!("message", "{}", msg);
                }
                Ok(msg)
            };
            let retag = !no_retag && !skip_retag;
            tagit.tagit(version, tag_prefix, &msg, extended_msg, dry_run, retag)?;
            Ok(())
        },
    )
}
