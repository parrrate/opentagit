use std::{
    fmt::{Debug, Display},
    path::Path,
};

#[doc(hidden)]
pub use inventory;
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
