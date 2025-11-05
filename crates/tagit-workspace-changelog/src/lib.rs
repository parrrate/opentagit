//! Implementation for the `tagit changelog` command.

use tagit_changelog::bump_one_changelog;
use tagit_workspace::{WorkspaceEntry, with_workspace_entries};

/// Update `CHANGELOG.md` on all members of the workspace.
pub fn bump_changelog(dry_run: bool) -> anyhow::Result<()> {
    with_workspace_entries(
        dry_run,
        false,
        |WorkspaceEntry {
             version,
             root,
             tag_prefix,
             ..
         }| bump_one_changelog(version.clone(), root, tag_prefix, dry_run),
    )
}
