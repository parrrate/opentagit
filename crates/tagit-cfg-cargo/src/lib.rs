//! `tagit-cfg` parsing for `cargo`.

use anyhow::Context;
use cargo::core::Manifest;
use tagit_cfg::TagitCfg;
use toml::Value;

fn get_value(manifest: &Manifest) -> Option<&Value> {
    manifest.custom_metadata()?.as_table()?.get("tagit")
}

/// Parse `[package.metadata.tagit]`.
pub fn parse(manifest: &Manifest) -> anyhow::Result<TagitCfg> {
    Ok(get_value(manifest)
        .cloned()
        .map(Value::try_into)
        .transpose()
        .context("failed to parse [package.metadata.tagit]")?
        .unwrap_or_default())
}
