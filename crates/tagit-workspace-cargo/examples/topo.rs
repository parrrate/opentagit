use tagit_workspace::TagitWorkspaceProvider;
use tagit_workspace_cargo::CargoProvider;

fn main() -> anyhow::Result<()> {
    CargoProvider.with_workspace(&mut |workspace| {
        workspace
            .members()
            .iter()
            .for_each(|p| println!("{}", p.name()));
        Ok(())
    })?;
    Ok(())
}
