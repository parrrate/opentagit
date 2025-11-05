//! Implementation of `tagit sub` command.

use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
    process::Command,
};

use owo_colors::OwoColorize;
use tagit_core::{Okie, Tagit};
use tagit_sub_command::SubtreeCommand;
use tagit_sub_core::{TAGIT_ATTRIBUTES_FILE, TAGIT_SUB_DIR, path_to_remote, remote_to_path};

const GITATTRIBUTES: &str = r#"/** linguist-vendored
"#;

fn save_attributes() -> anyhow::Result<()> {
    std::fs::write(TAGIT_ATTRIBUTES_FILE, GITATTRIBUTES)?;
    Ok(())
}

fn full_path(path: impl AsRef<Path>) -> PathBuf {
    Path::new(TAGIT_SUB_DIR).join(path)
}

fn read_from(name: &str) -> anyhow::Result<String> {
    let branch = Tagit::current_branch()?;
    Ok(format!("{name}/{branch}"))
}

fn subs() -> anyhow::Result<Vec<PathBuf>> {
    let mut subs = Vec::new();
    let remotes = remotes()?;
    for remote in remotes.iter() {
        if let Some(path) = remote_to_path(remote).transpose() {
            match path {
                Ok(path) => {
                    subs.push(path.into());
                }
                Err(path) => {
                    eprintln!("missing: {}", path.path().red());
                }
            }
        }
    }
    Ok(subs)
}

fn remotes() -> anyhow::Result<BTreeSet<String>> {
    let output = Command::new("git").arg("remote").output()?;
    output.status.okie()?;
    Ok(String::from_utf8(output.stdout)?
        .lines()
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_owned())
        .collect())
}

fn add(path: PathBuf, remote: String) -> anyhow::Result<()> {
    let name = path_to_remote(&path)?;
    let read_from = read_from(&name)?;
    let path = full_path(&path);
    let remotes = remotes()?;
    if !remotes.iter().any(|remote| *remote == name) {
        Command::new("git")
            .arg("remote")
            .arg("add")
            .arg("--fetch")
            .arg("--no-tags")
            .arg(&name)
            .arg(remote)
            .status()?
            .okie()?;
    }
    Command::new("git")
        .arg("merge")
        .args(["-s", "ours"])
        .arg("--no-commit")
        .arg("-m")
        .arg(format!("add {}", path.display()))
        .arg("--allow-unrelated-histories")
        .arg(&read_from)
        .status()?
        .okie()?;
    let root = Tagit::root()?;
    let path = path
        .to_str()
        .ok_or("non-utf8 path")
        .map_err(anyhow::Error::msg)?
        .replace('\\', "/");
    if !root.join(&path).exists() {
        Command::new("git")
            .arg("read-tree")
            .arg("--prefix")
            .arg(&path)
            .args(["-u", &read_from])
            .status()?
            .okie()?;
    }
    Ok(())
}

fn diff(path: PathBuf) -> anyhow::Result<()> {
    let name = path_to_remote(&path)?;
    let read_from = read_from(&name)?;
    let path = full_path(&path);
    Command::new("git")
        .arg("diff")
        .arg(format!("HEAD:{}", path.display()))
        .arg(read_from)
        .status()?
        .okie()?;
    Ok(())
}

fn pull(path: PathBuf) -> anyhow::Result<()> {
    if path.as_os_str().to_str() == Some("*") {
        for path in subs()? {
            assert_ne!(path.as_os_str().to_str(), Some("*"));
            pull(path)?;
        }
    } else {
        let name = path_to_remote(&path)?;
        let read_from = read_from(&name)?;
        Command::new("git")
            .arg("fetch")
            .arg(&name)
            .status()?
            .okie()?;
        Command::new("git")
            .arg("merge")
            .args(["-s", "subtree"])
            .arg("--no-commit")
            .arg("-m")
            .arg(format!("update {}", path.display()))
            .arg(&read_from)
            .status()?
            .okie()?;
    }
    Ok(())
}

fn remove(path: PathBuf) -> anyhow::Result<()> {
    let name = path_to_remote(&path)?;
    Command::new("git")
        .arg("remote")
        .arg("remove")
        .arg(&name)
        .status()?
        .okie()?;
    Ok(())
}

fn ls() -> anyhow::Result<()> {
    for sub in subs()? {
        println!("{}", sub.display());
    }
    Ok(())
}

/// Handle subcommands for `tagit sub`.
pub fn sub(command: SubtreeCommand) -> anyhow::Result<()> {
    match command {
        SubtreeCommand::Add { path, remote } => {
            add(path, remote)?;
            save_attributes()
        }
        SubtreeCommand::Diff { path } => diff(path),
        SubtreeCommand::Pull { path } => {
            pull(path)?;
            save_attributes()
        }
        SubtreeCommand::Remove { path } => {
            remove(path)?;
            save_attributes()
        }
        SubtreeCommand::Ls => ls(),
    }
}
