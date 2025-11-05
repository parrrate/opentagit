//! Core parts of `tagit sub`, which might influence other commands.

use std::path::Path;

/// Root for `tagit`-specific files.
pub const TAGIT_DIR: &str = ".tagit";
/// Where `tagit`'s `.gitattributes` are stored.
pub const TAGIT_ATTRIBUTES_FILE: &str = ".tagit/.gitattributes";
/// Where `tagit` stores subtrees.
pub const TAGIT_SUB_DIR: &str = ".tagit/sub";

/// Remote name for a particular subpath.
pub fn remote_name(path: &Path) -> anyhow::Result<String> {
    Ok(format!(
        "_tagit/_/{}",
        path.to_str()
            .ok_or("non-utf8 path")
            .map_err(anyhow::Error::msg)?
            .replace('\\', "/"),
    ))
}
