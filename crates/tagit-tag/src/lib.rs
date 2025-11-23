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
pub fn tag(
    tagit_package: &'static str,
    tagit_version: &'static str,
    dry_run: bool,
    no_retag: bool,
    total_order: bool,
    sign: Option<bool>,
) -> anyhow::Result<()> {
    let branch = Tagit::current_branch()?;
    out!("found branch", "{branch}");
    let remote = upstream_remote()?;
    out!("found remote", "{remote}");
    let mut tagit =
        Tagit::new(&remote, tagit_package, tagit_version)?.with_total_order(total_order);
    if let Some(sign) = sign {
        tagit = tagit.with_sign(sign);
    }
    with_workspace_entries(
        dry_run,
        true,
        |WorkspaceEntry {
             version,
             #[cfg(feature = "changelog")]
             root,
             name,
             tag_prefix,
             skip_retag,
             ..
         }| {
            let msg = format!("{name} {version}");
            let extended_msg = || {
                let msg = msg.clone();
                #[cfg(feature = "changelog")]
                let mut msg = msg;
                #[cfg(feature = "changelog")]
                if let Some(changelog) =
                    tagit_changelog::version_changelog(version.clone(), root, tag_prefix)?
                {
                    msg += "\n";
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
