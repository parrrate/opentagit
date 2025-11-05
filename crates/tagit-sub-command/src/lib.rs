//! `clap` part of `tagit sub`.
//!
//! Separated for the purposes of conditional compilation. Nothing other than `tagit-sub` should be
//! depending on this directly.

use std::path::PathBuf;

use clap::Subcommand;

#[derive(Subcommand, Default)]
/// Command that goes after `tagit sub`
pub enum SubtreeCommand {
    /// Start tracking a new subtree
    ///
    /// If remote or subtree are already present, keeps them as is and pulls
    Add {
        /// Path relative to .tagit/sub/
        path: PathBuf,
        /// Url of the remote
        remote: String,
    },
    /// Diff local and remote versions of the subtree
    Diff {
        /// Path relative to .tagit/sub/
        path: PathBuf,
    },
    /// Pull changes for subtrees
    ///
    /// To pull for all subtrees use `tagit sub pull '*'`
    Pull {
        /// Path relative to .tagit/sub/
        path: PathBuf,
    },
    /// Stop tracking a subtree
    ///
    /// This just removes the remote
    Remove {
        /// Path relative to .tagit/sub/
        path: PathBuf,
    },
    /// List all the subtrees
    ///
    /// All subdirectories of .tagit/sub/ which have a corresponding remote
    #[default]
    Ls,
}
