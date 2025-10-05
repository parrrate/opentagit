use std::{
    ffi::OsString,
    process::{Command, ExitStatus, Stdio},
};

use anyhow::bail;
#[doc(hidden)]
pub use owo_colors;
use owo_colors::OwoColorize;
use semver::Version;
#[doc(hidden)]
pub use static_assertions;

#[macro_export]
macro_rules! out {
    ($name: literal, $($arg:tt)*) => {{
        $crate::static_assertions::const_assert!($name.len() <= 16);
        println!(
            "{:18}{}",
            $crate::owo_colors::OwoColorize::dimmed(&format!("{}: ", $name)),
            format_args!($($arg)*),
        );
    }};
}

pub trait Okie {
    fn okie(&self) -> anyhow::Result<()>;
}

impl Okie for ExitStatus {
    fn okie(&self) -> anyhow::Result<()> {
        if !self.success() {
            bail!("bwaaa");
        }
        Ok(())
    }
}

pub struct Tagit {
    remote: String,
    package: &'static str,
    version: &'static str,
    total_order: bool,
}

struct VersionIterator {
    version: Version,
    patch: bool,
    minor: bool,
}

impl Iterator for VersionIterator {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let v = &mut self.version;
        if !std::mem::take(&mut v.build).is_empty() {
            Some(format!("{v}"))
        } else if !v.pre.is_empty() || v.major == 0 && v.minor == 0 {
            None
        } else if self.patch {
            self.patch = false;
            Some(format!("{}.{}", v.major, v.minor))
        } else if v.major != 0 && self.minor {
            self.minor = false;
            Some(format!("{}", v.major))
        } else {
            None
        }
    }
}

fn sign() -> &'static [&'static str] {
    if std::env::var_os("CI").is_some() {
        &[]
    } else {
        &["--sign"]
    }
}

impl Tagit {
    pub fn new(remote: &str, package: &'static str, version: &'static str) -> anyhow::Result<Self> {
        Ok(Self {
            remote: remote.to_owned(),
            package,
            version,
            total_order: false,
        })
    }

    pub fn with_total_order(self, total_order: bool) -> Self {
        Self {
            total_order,
            ..self
        }
    }

    pub fn date(ref_: &str) -> anyhow::Result<String> {
        let date = Command::new("git")
            .arg("log")
            .arg("-n1")
            .arg("--pretty=%aD")
            .arg(ref_)
            .output()?
            .stdout;
        Ok(String::from_utf8_lossy(&date).into_owned())
    }

    fn push_tag(&self, tag: &str) -> anyhow::Result<()> {
        Command::new("git")
            .arg("push")
            .arg(&self.remote)
            .arg("tag")
            .arg(tag)
            .status()?
            .okie()?;
        Ok(())
    }

    fn new_tag(&self, msg: &str, tag: &str, target: Option<&str>) -> anyhow::Result<()> {
        Command::new("git")
            .env(
                "GIT_COMMITTER_DATE",
                OsString::from(Self::date(target.unwrap_or("HEAD"))?.as_str()),
            )
            .arg("tag")
            .args(target.map(|_| "--force").as_slice())
            .args(sign())
            .arg("--annotate")
            .args([
                "--message",
                &format!("{msg}\ngenerated with {} {}", self.package, self.version),
            ])
            .arg(tag)
            .args(target.as_slice())
            .status()?
            .okie()?;
        Ok(())
    }

    pub fn exists(tag: &str) -> anyhow::Result<bool> {
        let exists = Command::new("git")
            .arg("rev-parse")
            .arg("--verify")
            .arg(tag)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()?
            .success();
        Ok(exists)
    }

    pub fn tagit(
        &self,
        version: &Version,
        tag_prefix: &str,
        msg: &str,
        extended_msg: impl FnOnce() -> anyhow::Result<String>,
        dry_run: bool,
        retag: bool,
    ) -> anyhow::Result<()> {
        let tag = &format!("{tag_prefix}{version}");
        let exists = Self::exists(tag)?;
        if exists {
            out!("already exists", "{}", tag.dimmed());
        } else {
            out!("tagging version", "{}", tag.green());
            let msg = &*extended_msg()?;
            if !dry_run {
                self.new_tag(msg, tag, None)?;
                self.push_tag(tag)?;
            }
        }
        if retag {
            let current = tag;
            self.retag_current(version, tag_prefix, msg, dry_run, current)?;
        }
        Ok(())
    }

    fn retag_current(
        &self,
        version: &Version,
        tag_prefix: &str,
        msg: &str,
        dry_run: bool,
        current: &str,
    ) -> anyhow::Result<()> {
        let commit = Command::new("git")
            .arg("rev-list")
            .args(["-n", "1"])
            .arg(current)
            .output()?
            .stdout;
        let commit = String::from_utf8(commit)?;
        let commit = commit.trim();
        if !dry_run && commit.is_empty() {
            bail!("tag doesn't exist: {current}");
        }
        let current_commit = commit;
        let versions = VersionIterator {
            version: version.clone(),
            patch: true,
            minor: true,
        };
        'versions: for tag in versions {
            let tag = &format!("{tag_prefix}{tag}");
            {
                let tags = Command::new("git")
                    .arg("tag")
                    .arg("--points-at")
                    .arg(current)
                    .output()?
                    .stdout;
                let tags = String::from_utf8(tags)?;
                for other in tags.lines() {
                    if tag == other {
                        out!("already matches", "{}", tag.dimmed());
                        continue 'versions;
                    }
                }
            }
            out!("checking retag", "{tag}");
            let mut pulled = false;
            if !dry_run {
                pulled = Command::new("git")
                    .arg("fetch")
                    .arg("--quiet")
                    .arg(&self.remote)
                    .arg("tag")
                    .arg(tag)
                    .status()?
                    .success();
            }
            let commit = Command::new("git")
                .arg("rev-list")
                .args(["-n", "1"])
                .arg(tag)
                .output()?
                .stdout;
            let commit = String::from_utf8(commit)?;
            let commit = commit.trim();
            if !dry_run && pulled && commit.is_empty() {
                bail!("inconsistent state");
            }
            let mut outdated = true;
            if !commit.is_empty() {
                let tags = Command::new("git")
                    .arg("tag")
                    .arg("--points-at")
                    .arg(commit)
                    .output()?
                    .stdout;
                let tags = String::from_utf8(tags)?;
                for other in tags.lines() {
                    if let Some(other) = other.strip_prefix(tag_prefix)
                        && let Ok(other) = Version::parse(other)
                    {
                        let cmp = if self.total_order {
                            version.cmp(&other)
                        } else {
                            version.cmp_precedence(&other)
                        };
                        if cmp.is_le() {
                            out!("found existing", "{}", other.dimmed());
                            outdated = false;
                        }
                    }
                }
            }
            if outdated {
                out!("retagging", "{}", tag.green());
                if !dry_run {
                    assert!(!current_commit.is_empty());
                    if pulled {
                        assert!(!commit.is_empty());
                        out!("deleting", "{}", tag.red());
                        Command::new("git")
                            .arg("push")
                            .arg("--quiet")
                            .arg("--delete")
                            .arg(format!("--force-with-lease={tag}:{tag}"))
                            .arg(&self.remote)
                            .arg("tag")
                            .arg(tag)
                            .status()?
                            .okie()?;
                    }
                    self.new_tag(msg, tag, Some(current_commit))?;
                    self.push_tag(tag)?;
                }
            }
        }
        Ok(())
    }

    pub fn retag(
        &self,
        version: &Version,
        tag_prefix: &str,
        msg: &str,
        dry_run: bool,
    ) -> anyhow::Result<()> {
        let current = &format!("{tag_prefix}{version}");
        self.retag_current(version, tag_prefix, msg, dry_run, current)
    }
}
