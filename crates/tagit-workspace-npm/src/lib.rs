use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use anyhow::Context;
use semver::Version;
use serde::Deserialize;
use tagit_cfg::TagitCfg;
use tagit_core::{Tagit, out};
use tagit_workspace::{TagitPackage, TagitWorkspace, TagitWorkspaceProvider};

#[derive(Deserialize, Default)]
#[serde(untagged)]
enum Workspaces {
    #[default]
    #[serde(skip)]
    None,
    Node(Vec<String>),
    Bun {
        packages: Vec<String>,
    },
}

#[derive(Deserialize)]
struct PackageJson {
    name: String,
    version: Option<Version>,
    #[serde(default)]
    tagit: TagitCfg,
    #[serde(default)]
    workspaces: Workspaces,
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
    fn from_root(root: PathBuf) -> anyhow::Result<Result<Self, (PathBuf, Vec<String>)>> {
        let path = root.join("package.json");
        let PackageJson {
            name,
            version,
            tagit,
            workspaces,
        } = serde_json::from_reader(std::fs::File::open(&path)?)?;
        let workspaces = match workspaces {
            Workspaces::None => Vec::new(),
            Workspaces::Node(packages) => packages,
            Workspaces::Bun { packages } => packages,
        };
        Ok(if let Some(version) = version {
            Ok(Self {
                path,
                cfg: tagit,
                name,
                version,
                root,
                workspaces,
            })
        } else {
            Err((path, workspaces))
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
        let (root_manifest, root_package, members) = match NpmPackage::from_root(root.clone())? {
            Ok(mut root_package) => {
                let members = std::mem::take(&mut root_package.workspaces);
                (root_package.path.clone(), Some(root_package), members)
            }
            Err((root_manifest, members)) => (root_manifest, None, members),
        };
        let mut packages = members
            .iter()
            .map(|glob| root.join(glob))
            .map(|glob| {
                Ok(glob
                    .as_os_str()
                    .to_str()
                    .context("not utf-8 path")?
                    .to_owned())
            })
            .collect::<anyhow::Result<Vec<_>>>()?
            .into_iter()
            .map(|glob| {
                glob::glob(&glob)?
                    .map(|r| Ok(r?))
                    .collect::<anyhow::Result<Vec<_>>>()
            })
            .collect::<anyhow::Result<Vec<_>>>()?
            .into_iter()
            .flatten()
            .map(NpmPackage::from_root)
            .collect::<anyhow::Result<Vec<_>>>()?
            .into_iter()
            .filter_map(Result::ok)
            .collect::<Vec<_>>();
        packages.extend(root_package);
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
        let path = root.join("package.json");
        if !path.exists() {
            out!("doesn't exist", "{}", path.display());
            return Ok(());
        }
        f(&NpmWorkspace::from_root(root)?)
    }
}

tagit_workspace::submit!(NpmProvider);
