//! Core parts of `tagit sub`, which might influence other commands.

use std::path::{Path, PathBuf};

/// Root for `tagit`-specific files.
pub const TAGIT_DIR: &str = ".tagit";
/// Where `tagit`'s `.gitattributes` are stored.
pub const TAGIT_ATTRIBUTES_FILE: &str = ".tagit/.gitattributes";
/// Where `tagit` stores subtrees.
pub const TAGIT_SUB_DIR: &str = ".tagit/sub";

/// Remote name for a particular subpath.
pub fn path_to_remote(path: &Path) -> anyhow::Result<String> {
    Ok(format!(
        "_tagit/_/{}",
        path.to_str()
            .ok_or("non-utf8 path")
            .map_err(anyhow::Error::msg)?
            .replace('\\', "/"),
    ))
}

#[derive(Debug, thiserror::Error)]
#[error("missing subtree: {0}")]
pub struct MissingPath<'a>(&'a str);

impl<'a> MissingPath<'a> {
    pub fn path(&self) -> &'a str {
        self.0
    }
}

fn full_path(path: impl AsRef<Path>) -> PathBuf {
    Path::new(TAGIT_SUB_DIR).join(path)
}

pub fn remote_to_path<'a>(remote: &'a str) -> Result<Option<&'a str>, MissingPath<'a>> {
    if let Some(path) = remote.strip_prefix("_tagit/_/") {
        if full_path(path).exists() {
            Ok(Some(path))
        } else {
            Err(MissingPath(path))
        }
    } else {
        Ok(None)
    }
}
