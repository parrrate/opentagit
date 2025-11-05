use std::{fmt::Display, path::Path};

use semver::Version;
use tagit_cfg::TagitCfg;

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
