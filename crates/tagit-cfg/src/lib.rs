//! Configuration for `tagit`.

use serde::Deserialize;

#[derive(Deserialize, Default, Clone)]
#[serde(deny_unknown_fields, default, rename_all = "kebab-case")]
#[non_exhaustive]
/// Configuration for a specific package, from tool metadata section of the manifest.
pub struct TagitCfg {
    /// Ignore this package entirely.
    pub skip: bool,
    /// Do not do semver-guided retagging.
    pub skip_retag: bool,
}
