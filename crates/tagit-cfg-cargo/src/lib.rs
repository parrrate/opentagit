//! `tagit-cfg` parsing for `cargo`.

use anyhow::Context;
use cargo_metadata::Package;
use tagit_cfg::TagitCfg;

fn get_value(manifest: &Package) -> Option<&serde_json::Value> {
    manifest.metadata.as_object()?.get("tagit")
}

/// Parse `[package.metadata.tagit]`.
pub fn parse(manifest: &Package) -> anyhow::Result<TagitCfg> {
    Ok(get_value(manifest)
        .cloned()
        .map(serde_json::from_value)
        .transpose()
        .context("failed to parse [package.metadata.tagit]")?
        .unwrap_or_default())
}
