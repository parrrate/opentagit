use std::{
    fmt::{Debug, Display},
    path::Path,
    process::{Command, Stdio},
};

use anyhow::bail;
#[doc(hidden)]
pub use inventory;
use owo_colors::OwoColorize;
use semver::Version;
use tagit_cfg::TagitCfg;
use tagit_core::out;
use tagit_sub_core::TAGIT_DIR;

pub fn with_workspace(
    workspace: &dyn TagitWorkspace,
    dry_run: bool,
    check_committed: bool,
    mut f: impl FnMut(WorkspaceEntry<'_>) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    let mut uncommitted: usize = 0;
    for package in workspace.members() {
        let is_subtree = package
            .manifest_path()
            .components()
            .any(|part| part.as_os_str().to_str() == Some(TAGIT_DIR));
        if is_subtree {
            continue;
        }
        let TagitCfg { skip, skip_retag } = package.cfg()?;
        if skip {
            out!("skipping package", "{}", package.dimmed());
            continue;
        }
        out!("found package", "{package}");
        let path = package.manifest_path();
        if check_committed {
            let manifest_tracked = Command::new("git")
                .arg("ls-files")
                .arg("--error-unmatch")
                .arg(path)
                .stdout(Stdio::null())
                .status()?
                .success();
            if !manifest_tracked {
                uncommitted += 1;
                if !dry_run {
                    bail!("untracked manifest")
                } else {
                    out!("untracked", "{}", path.display().purple())
                }
            }
            let manifest_commited = Command::new("git")
                .arg("diff-index")
                .arg("--quiet")
                .arg("HEAD")
                .arg(path)
                .status()?
                .success();
            if !manifest_commited {
                uncommitted += 1;
                if !dry_run {
                    bail!("uncommitted manifest")
                } else {
                    out!("uncommitted", "{}", path.display().purple())
                }
            }
        }
        let name = package.name();
        let is_root = workspace.root_manifest() == package.manifest_path();
        let tag_prefix = if is_root {
            "".to_string()
        } else {
            format!("{name}/")
        };
        let version = package.version();
        let root = package.root();
        let tag_prefix = &tag_prefix;
        f(WorkspaceEntry {
            version,
            root,
            name,
            tag_prefix,
            skip_retag,
        })?;
    }
    if dry_run {
        if uncommitted == 0 {
            println!("{} {}", "dry run".yellow(), "ok".green());
        } else {
            println!(
                "{} encountered {} untracked/uncommitted manifest(s)",
                "dry run".yellow(),
                uncommitted.red(),
            )
        }
    } else {
        assert_eq!(uncommitted, 0);
    }
    Ok(())
}

#[non_exhaustive]
pub struct WorkspaceEntry<'a> {
    pub version: &'a Version,
    pub root: &'a Path,
    pub name: &'a str,
    pub tag_prefix: &'a str,
    pub skip_retag: bool,
}

pub trait TagitPackage: Display {
    fn manifest_path(&self) -> &Path;
    fn cfg(&self) -> anyhow::Result<TagitCfg>;
    fn name(&self) -> &str;
    fn version(&self) -> &Version;
    fn root(&self) -> &Path;
}

pub trait TagitWorkspace {
    fn members(&self) -> Vec<&dyn TagitPackage>;
    fn root_manifest(&self) -> &Path;
}

pub trait TagitWorkspaceProvider: 'static + Send + Sync + Debug {
    fn with_workspace(
        &self,
        f: &mut dyn FnMut(&dyn TagitWorkspace) -> anyhow::Result<()>,
    ) -> anyhow::Result<()>;
}

#[derive(Debug, Clone)]
#[doc(hidden)]
pub struct WorkspaceProvider(&'static dyn TagitWorkspaceProvider);

impl WorkspaceProvider {
    #[doc(hidden)]
    pub const fn new(provider: &'static impl TagitWorkspaceProvider) -> Self {
        Self(provider)
    }
}

pub fn with_workspaces(
    mut f: impl FnMut(&dyn TagitWorkspace) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    for provider in inventory::iter::<WorkspaceProvider> {
        provider.0.with_workspace(&mut f)?;
    }
    Ok(())
}

inventory::collect!(WorkspaceProvider);

#[macro_export]
macro_rules! submit {
    ($provider:expr) => {
        $crate::inventory::submit! {
            $crate::WorkspaceProvider::new(&$provider)
        }
    };
}
