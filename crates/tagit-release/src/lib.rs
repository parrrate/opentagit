use std::process::Command;

use tagit_core::{Okie, default_sign, sign_args};

pub fn release(
    tagit_package: &'static str,
    tagit_version: &'static str,
    dry_run: bool,
    no_retag: bool,
    total_order: bool,
    sign: Option<bool>,
    message: Option<&str>,
) -> anyhow::Result<()> {
    let bumped = tagit_workspace_changelog::bump_changelog(dry_run)?;
    if !dry_run {
        if !bumped.is_empty() {
            Command::new("git")
                .arg("add")
                .arg("--")
                .args(bumped)
                .status()?
                .okie()?;
        }
        let mut cmd = Command::new("git");
        cmd.arg("commit");
        if let Some(message) = message {
            cmd.args(["--message", message]);
        }
        cmd.args(sign_args(sign.unwrap_or_else(default_sign)));
        cmd.status()?.okie()?;
        Command::new("git").arg("push").status()?.okie()?;
    }
    tagit_tag::tag(
        tagit_package,
        tagit_version,
        dry_run,
        no_retag,
        total_order,
        sign,
    )?;
    Ok(())
}
