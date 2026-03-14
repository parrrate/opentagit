use std::{fmt::Display, path::PathBuf};

use anyhow::{Context, bail};
use semver::{BuildMetadata, Prerelease, Version};
use serde::Deserialize;
use tagit_cfg::TagitCfg;
use tagit_core::{Tagit, out};
use tagit_workspace::{TagitPackage, TagitWorkspace, TagitWorkspaceProvider};

#[derive(Deserialize)]
struct PyProjectToml {
    #[serde(flatten)]
    inner: pyproject_toml::PyProjectToml,
    #[serde(default)]
    tool: Tool,
}

#[derive(Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
struct Tool {
    #[serde(default)]
    tagit: TagitCfg,
}

struct PythonProject {
    name: String,
    version: Version,
    cfg: TagitCfg,
    path: PathBuf,
    root: PathBuf,
}

impl Display for PythonProject {
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

impl TagitPackage for PythonProject {
    fn manifest_path(&self) -> &std::path::Path {
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

    fn root(&self) -> &std::path::Path {
        &self.root
    }
}

impl TagitWorkspace for PythonProject {
    fn members(&self) -> Vec<&dyn TagitPackage> {
        vec![self]
    }

    fn root_manifest(&self) -> &std::path::Path {
        &self.path
    }
}

#[derive(Debug)]
pub struct PyProvider;

impl TagitWorkspaceProvider for PyProvider {
    fn with_workspace(
        &self,
        f: &mut dyn FnMut(&dyn TagitWorkspace) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let root = Tagit::root()?;
        let path = root.join("pyproject.toml");
        if !path.exists() {
            out!("doesn't exist", "{}", path.display());
            return Ok(());
        }
        let PyProjectToml { inner, tool } = toml::from_str(&std::fs::read_to_string(&path)?)?;
        let project = inner.project.context("no [project]")?;
        let name = project.name;
        let v = project.version.context("no version")?;
        if v.epoch() != 0 {
            bail!("non-0 epoch is not supported");
        }
        if v.post().is_some() {
            bail!("post-releases are not supported");
        }
        if v.dev().is_some() {
            bail!("dev releases are not supported");
        }
        if v.release().len() != 3 {
            bail!("non-3-segment releases are not supported");
        }
        let mut version = Version::new(v.release()[0], v.release()[1], v.release()[2]);
        if let Some(pre) = v.pre() {
            version.pre = Prerelease::new(&format!("{}.{}", pre.kind, pre.number))?;
        }
        if !v.local().is_empty() {
            version.build = BuildMetadata::new(
                &v.local()
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("+"),
            )?;
        }
        f(&PythonProject {
            name,
            version,
            cfg: tool.tagit,
            path,
            root,
        })
    }
}

tagit_workspace::submit!(PyProvider);
