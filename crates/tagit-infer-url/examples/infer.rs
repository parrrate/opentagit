use tagit_infer_url::infer_url;

fn main() -> anyhow::Result<()> {
    let url = infer_url()?;
    println!("{url}");
    Ok(())
}
