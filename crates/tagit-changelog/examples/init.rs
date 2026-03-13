use tagit_changelog::init_changelog;

fn main() -> anyhow::Result<()> {
    init_changelog(
        "0.1.0".parse()?,
        "crates/tagit-changelog/examples",
        "tagit-infer-url/",
        false,
    )?;
    Ok(())
}
