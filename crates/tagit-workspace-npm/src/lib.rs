use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use semver::Version;
use serde::Deserialize;
use tagit_cfg::TagitCfg;
use tagit_core::{Tagit, out};
use tagit_workspace::{TagitPackage, TagitWorkspace, TagitWorkspaceProvider};

#[derive(Deserialize)]
struct PackageJson {
    name: String,
    version: Version,
    tagit: TagitCfg,
    workspaces: Vec<String>,
}

struct NpmPackage {
    path: PathBuf,
    cfg: TagitCfg,
    name: String,
    version: Version,
    root: PathBuf,
    workspaces: Vec<String>,
}

impl Display for NpmPackage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}@{} ({})",
            self.name,
            self.version,
            self.root.display(),
        )
    }
}

impl NpmPackage {
    fn from_root(root: PathBuf) -> anyhow::Result<Self> {
        let path = root.join("package.json");
        let PackageJson {
            name,
            version,
            tagit,
            workspaces,
        } = serde_json::from_reader(std::fs::File::open(&path)?)?;
        Ok(Self {
            path,
            cfg: tagit,
            name,
            version,
            root,
            workspaces,
        })
    }
}

impl TagitPackage for NpmPackage {
    fn manifest_path(&self) -> &Path {
        &self.path
    }

    fn cfg(&self) -> anyhow::Result<TagitCfg> {
        Ok(self.cfg.clone())
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn version(&self) -> &Version {
        &self.version
    }

    fn root(&self) -> &Path {
        &self.root
    }
}

struct NpmWorkspace {
    packages: Vec<NpmPackage>,
    root_manifest: PathBuf,
}

impl NpmWorkspace {
    fn from_root(root: PathBuf) -> anyhow::Result<Self> {
        let root_package = NpmPackage::from_root(root)?;
        let mut packages = root_package
            .workspaces
            .iter()
            .map(|member| root_package.root.join(member))
            .map(NpmPackage::from_root)
            .collect::<anyhow::Result<Vec<_>>>()?;
        let root_manifest = root_package.path.clone();
        packages.push(root_package);
        Ok(Self {
            packages,
            root_manifest,
        })
    }
}

impl TagitWorkspace for NpmWorkspace {
    fn members(&self) -> Vec<&dyn TagitPackage> {
        self.packages.iter().map(|p| p as _).collect()
    }

    fn root_manifest(&self) -> &Path {
        &self.root_manifest
    }
}

#[derive(Debug)]
pub struct NpmProvider;

impl TagitWorkspaceProvider for NpmProvider {
    fn with_workspace(
        &self,
        f: &mut dyn FnMut(&dyn TagitWorkspace) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let root = Tagit::root()?;
        out!("found root", "{}", root.display());
        f(&NpmWorkspace::from_root(root)?)
    }
}

tagit_workspace::submit!(NpmProvider);
