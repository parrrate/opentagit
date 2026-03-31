use std::process::Command;

use anyhow::Context;
use tagit_core::{Okie, Tagit};
use url::Url;

pub fn infer_url() -> anyhow::Result<Url> {
    let remote = Tagit::upstream_remote()?;
    let output = Command::new("git")
        .arg("remote")
        .arg("get-url")
        .arg(remote)
        .output()?;
    output.status.okie_with("failed to get remote url")?;
    let mut url =
        &*String::from_utf8(output.stdout)?.replace("git@github.com:", "ssh://git@github.com/");
    let https;
    if let Some(tail) = url.strip_prefix("ssh://") {
        https = format!("https://{tail}");
        url = &https;
    }
    let mut url = Url::parse(url)?;
    url.set_password(None)
        .ok()
        .context("failed to remove password")?;
    url.set_username("")
        .ok()
        .context("failed to remove username")?;
    url.set_port(None).ok().context("failed to remove port")?;
    if let Some(path) = url.path().strip_suffix("/") {
        let path = &*path.to_owned();
        url.set_path(path);
    }
    if let Some(path) = url.path().strip_suffix(".git") {
        let path = &*path.to_owned();
        url.set_path(path);
    }
    url.set_query(None);
    Ok(url)
}
