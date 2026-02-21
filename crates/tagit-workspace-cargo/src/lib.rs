use std::{
    collections::BTreeSet,
    fmt::Display,
    path::{Path, PathBuf},
};

use anyhow::Context;
use cargo::{
    GlobalContext,
    core::{Package, Workspace},
};
use semver::Version;
use tagit_cfg::TagitCfg;
use tagit_core::{Tagit, out};
use tagit_workspace::{TagitPackage, TagitWorkspace, TagitWorkspaceProvider};

struct CargoPackage<'a>(&'a Package);

impl Display for CargoPackage<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl TagitPackage for CargoPackage<'_> {
    fn manifest_path(&self) -> &Path {
        self.0.manifest_path()
    }

    fn cfg(&self) -> anyhow::Result<TagitCfg> {
        tagit_cfg_cargo::parse(self.0.manifest())
            .with_context(|| format!("invalid metadata for {self}"))
    }

    fn name(&self) -> &str {
        self.0.name().as_str()
    }

    fn version(&self) -> &Version {
        self.0.version()
    }

    fn root(&self) -> &Path {
        self.0.root()
    }

    fn paths(&self) -> anyhow::Result<Vec<PathBuf>> {
        Ok(self
            .0
            .targets()
            .iter()
            .filter_map(|target| target.src_path().path())
            .map(Path::to_owned)
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect())
    }
}

pub struct CargoWorkspace<'a, 'b>(&'a Workspace<'b>, Vec<CargoPackage<'a>>);

impl<'a, 'b> CargoWorkspace<'a, 'b> {
    pub fn new(workspace: &'a Workspace<'b>) -> Self {
        let members = workspace.members().map(CargoPackage).collect();
        Self(workspace, members)
    }
}

impl TagitWorkspace for CargoWorkspace<'_, '_> {
    fn members(&self) -> Vec<&dyn TagitPackage> {
        self.1.iter().map(|p| p as _).collect()
    }

    fn root_manifest(&self) -> &Path {
        self.0.root_manifest()
    }
}

#[derive(Debug)]
pub struct CargoProvider;

impl TagitWorkspaceProvider for CargoProvider {
    fn with_workspace(
        &self,
        f: &mut dyn FnMut(&dyn TagitWorkspace) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let ctx = &GlobalContext::default()?;
        out!("found cargo", "{}", ctx.cargo_exe()?.display());
        let root = Tagit::root()?;
        out!("found root", "{}", root.display());
        let path = root.join("Cargo.toml");
        if !path.exists() {
            out!("doesn't exist", "{}", path.display());
            return Ok(());
        }
        let workspace = Workspace::new(&path, ctx)?;
        out!("found workspace", "{}", workspace.root().display());
        f(&CargoWorkspace::new(&workspace))
    }
}

tagit_workspace::submit!(CargoProvider);
