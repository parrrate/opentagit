use std::{
    collections::BTreeSet,
    fmt::Display,
    path::{Path, PathBuf},
};

use anyhow::Context;
use cargo_metadata::{Metadata, MetadataCommand, Package};
use semver::Version;
use tagit_cfg::TagitCfg;
use tagit_core::{Tagit, out};
use tagit_workspace::{TagitPackage, TagitWorkspace, TagitWorkspaceProvider};

struct CargoPackage<'a>(&'a Package, &'a Path);

impl Display for CargoPackage<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{}", self.0.name, self.0.version)
    }
}

impl TagitPackage for CargoPackage<'_> {
    fn manifest_path(&self) -> &Path {
        self.0.manifest_path.as_std_path()
    }

    fn cfg(&self) -> anyhow::Result<TagitCfg> {
        tagit_cfg_cargo::parse(self.0).with_context(|| format!("invalid metadata for {self}"))
    }

    fn name(&self) -> &str {
        &self.0.name
    }

    fn version(&self) -> &Version {
        &self.0.version
    }

    fn root(&self) -> &Path {
        self.1
    }

    fn paths(&self) -> anyhow::Result<Vec<PathBuf>> {
        Ok(self
            .0
            .targets
            .iter()
            .map(|target| target.src_path.as_std_path())
            .map(Path::to_owned)
            .collect::<BTreeSet<_>>()
            .into_iter()
            .chain(std::iter::once(
                self.0.manifest_path.as_std_path().to_owned(),
            ))
            .collect())
    }
}

pub struct CargoWorkspace<'a>(Vec<CargoPackage<'a>>, &'a Path);

impl<'a> CargoWorkspace<'a> {
    pub fn new(workspace: &'a Metadata, root_manifest: &'a Path) -> anyhow::Result<Self> {
        let members = workspace
            .workspace_packages()
            .into_iter()
            .map(|package| {
                Ok(CargoPackage(
                    package,
                    package
                        .manifest_path
                        .parent()
                        .context("no parent directory for the manifest")?
                        .as_std_path(),
                ))
            })
            .collect::<anyhow::Result<_>>()?;
        Ok(Self(members, root_manifest))
    }
}

impl TagitWorkspace for CargoWorkspace<'_> {
    fn members(&self) -> Vec<&dyn TagitPackage> {
        self.0.iter().map(|p| p as _).collect()
    }

    fn root_manifest(&self) -> &Path {
        self.1
    }
}

#[derive(Debug)]
pub struct CargoProvider;

impl TagitWorkspaceProvider for CargoProvider {
    fn with_workspace(
        &self,
        f: &mut dyn FnMut(&dyn TagitWorkspace) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let root = Tagit::root()?;
        out!("found root", "{}", root.display());
        let path = root.join("Cargo.toml");
        if !path.exists() {
            out!("doesn't exist", "{}", path.display());
            return Ok(());
        }
        let workspace: Metadata = MetadataCommand::new().exec()?;
        out!("found workspace", "{}", workspace.workspace_root);
        f(&CargoWorkspace::new(&workspace, &path)?)
    }
}

tagit_workspace::submit!(CargoProvider);
