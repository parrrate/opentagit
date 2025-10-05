use std::{
    collections::{BTreeMap, btree_map},
    ops::Add,
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{Context, bail};
use chrono::{DateTime, Utc};
use markdown::{
    ParseOptions,
    mdast::{Definition, Heading, LinkReference, List, Node, ReferenceKind, Root, Text},
    to_mdast,
};
use mdast_util_to_markdown::{IndentOptions, Options, to_markdown_with_options};
use semver::{BuildMetadata, Version};
use strum::EnumString;
use tagit_core::Tagit;

const DATE_SEPARATOR: &str = "\u{2002}—\u{2002}";

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum MaybeVersion {
    Version(Version),
    Unreleased,
}

impl FromStr for MaybeVersion {
    type Err = semver::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "unreleased" {
            Ok(Self::Unreleased)
        } else {
            s.parse().map(Self::Version)
        }
    }
}

struct Section {
    heading: Heading,
    prefix: Vec<Node>,
    added: Option<(Heading, List)>,
    changed: Option<(Heading, List)>,
    deprecated: Option<(Heading, List)>,
    removed: Option<(Heading, List)>,
    fixed: Option<(Heading, List)>,
    security: Option<(Heading, List)>,
    date: Option<String>,
}

fn join_nodes(a: Vec<Node>, b: Vec<Node>) -> Vec<Node> {
    [a, b].concat()
}

fn join_lists(a: List, b: List) -> List {
    assert!(!a.ordered);
    assert!(!b.ordered);
    assert!(!a.spread);
    assert!(!b.spread);
    List {
        children: join_nodes(a.children, b.children),
        position: a.position,
        ordered: false,
        start: None,
        spread: false,
    }
}

fn join_opt(a: Option<(Heading, List)>, b: Option<(Heading, List)>) -> Option<(Heading, List)> {
    match (a, b) {
        (None, b) => b,
        (a, None) => a,
        (Some((heading, a)), Some((_, b))) => Some((heading, join_lists(a, b))),
    }
}

impl Add for Section {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            heading: self.heading,
            prefix: join_nodes(self.prefix, rhs.prefix),
            added: join_opt(self.added, rhs.added),
            changed: join_opt(self.changed, rhs.changed),
            deprecated: join_opt(self.deprecated, rhs.deprecated),
            removed: join_opt(self.removed, rhs.removed),
            fixed: join_opt(self.fixed, rhs.fixed),
            security: join_opt(self.security, rhs.security),
            date: self.date,
        }
    }
}

#[derive(EnumString)]
enum ListKind {
    Added,
    Changed,
    Deprecated,
    Removed,
    Fixed,
    Security,
}

enum SecState<'a> {
    Prefix(&'a mut Vec<Node>),
    List { heading: Heading, kind: ListKind },
    AfterList,
}

impl Section {
    fn from_nodes(
        heading: Heading,
        nodes: Vec<Node>,
        date: Option<String>,
    ) -> anyhow::Result<Self> {
        let mut this = Self {
            heading,
            prefix: Vec::new(),
            added: None,
            changed: None,
            deprecated: None,
            removed: None,
            fixed: None,
            security: None,
            date,
        };
        let mut state = SecState::Prefix(&mut this.prefix);
        for node in nodes {
            match (&mut state, node) {
                (SecState::Prefix(_) | SecState::AfterList, Node::Heading(heading)) => {
                    let kind = match heading.children.as_slice() {
                        [Node::Text(Text { value, .. })] => value.parse()?,
                        _ => bail!("unsupported header"),
                    };
                    state = SecState::List { kind, heading };
                }
                (SecState::Prefix(nodes), node) => {
                    nodes.push(node);
                }
                (SecState::List { kind, heading }, Node::List(list)) => {
                    match kind {
                        ListKind::Added => this.added = Some((heading.clone(), list)),
                        ListKind::Changed => this.changed = Some((heading.clone(), list)),
                        ListKind::Deprecated => this.deprecated = Some((heading.clone(), list)),
                        ListKind::Removed => this.removed = Some((heading.clone(), list)),
                        ListKind::Fixed => this.fixed = Some((heading.clone(), list)),
                        ListKind::Security => this.security = Some((heading.clone(), list)),
                    }
                    state = SecState::AfterList;
                }
                (SecState::List { .. }, _) => bail!("expected a list"),
                (SecState::AfterList, _) => bail!("expected an h3"),
            }
        }
        match state {
            SecState::Prefix(_) => {}
            SecState::List { .. } => bail!("expected a list"),
            SecState::AfterList => {}
        }
        Ok(this)
    }

    fn into_nodes(mut self) -> Vec<Node> {
        let mut nodes = vec![Node::Heading(self.heading)];
        nodes.append(&mut self.prefix);
        for (heading, list) in [
            self.added,
            self.changed,
            self.deprecated,
            self.removed,
            self.fixed,
            self.security,
        ]
        .into_iter()
        .flatten()
        {
            nodes.push(Node::Heading(heading));
            nodes.push(Node::List(list));
        }
        nodes
    }

    fn ensure_date(
        &mut self,
        version: &MaybeVersion,
        latest: &Version,
        tag_prefix: &str,
    ) -> anyhow::Result<()> {
        let MaybeVersion::Version(version) = version else {
            return Ok(());
        };
        if self.date.is_some() {
            return Ok(());
        }
        let tag = &format!("{tag_prefix}{version}");
        let date = if latest == version && !Tagit::exists(tag)? {
            Utc::now()
        } else {
            DateTime::parse_from_rfc2822(Tagit::date(tag)?.trim())?.with_timezone(&Utc)
        }
        .format("%F")
        .to_string();
        let value = format!("{DATE_SEPARATOR}{date}");
        self.date = Some(date);
        self.heading.children.push(Node::Text(Text {
            value,
            position: None,
        }));
        Ok(())
    }
}

enum State<'a> {
    Preface(&'a mut Vec<Node>),
    Version {
        sections: &'a mut BTreeMap<MaybeVersion, Section>,
        version: MaybeVersion,
        heading: Heading,
        nodes: Vec<Node>,
        date: Option<String>,
    },
    Definitions(&'a mut Vec<Definition>),
}

struct Document {
    preface: Vec<Node>,
    sections: BTreeMap<MaybeVersion, Section>,
    definitions: Vec<Definition>,
    version: Version,
}

impl Document {
    fn into_nodes(mut self, children: &mut Vec<Node>, tag_prefix: &str) -> anyhow::Result<()> {
        children.append(&mut self.preface);
        for (version, mut section) in self.sections.into_iter().rev() {
            section.ensure_date(&version, &self.version, tag_prefix)?;
            children.append(&mut section.into_nodes());
        }
        children.extend(self.definitions.into_iter().map(Node::Definition));
        Ok(())
    }

    fn from_children(
        children: &mut Vec<Node>,
        version: Version,
        tag_prefix: &str,
    ) -> anyhow::Result<Self> {
        let mut preface = Vec::new();
        let mut definitions = Vec::new();
        let mut sections = BTreeMap::new();
        let mut state = State::Preface(&mut preface);
        for node in children.drain(..) {
            match (&mut state, node) {
                (_, Node::Root(_)) => unreachable!(),
                (
                    State::Preface(_),
                    Node::Heading(
                        ref heading @ Heading {
                            ref children,
                            depth: 2,
                            ..
                        },
                    ),
                ) if {
                    matches!(children.as_slice(), [Node::LinkReference(LinkReference { identifier, .. })] if identifier == "unreleased")
                } =>
                {
                    state = State::Version {
                        sections: &mut sections,
                        version: MaybeVersion::Unreleased,
                        heading: heading.clone(),
                        nodes: Vec::new(),
                        date: None,
                    };
                }
                (State::Preface(nodes), node) => {
                    nodes.push(node);
                }
                (
                    State::Version {
                        sections,
                        version,
                        heading,
                        nodes,
                        date,
                    },
                    Node::Heading(next_heading @ Heading { depth: 2, .. }),
                ) => {
                    sections.insert(
                        version.clone(),
                        Section::from_nodes(heading.clone(), std::mem::take(nodes), date.take())?,
                    );
                    *version = match next_heading.children.as_slice() {
                        [
                            Node::LinkReference(LinkReference { identifier, .. }),
                            rest @ ..,
                        ] => {
                            match rest {
                                [Node::Text(Text { value, .. })] => {
                                    *date = Some(
                                        value
                                            .strip_prefix(DATE_SEPARATOR)
                                            .context("unsupported header")?
                                            .to_string(),
                                    );
                                }
                                [] => {}
                                _ => bail!("unsupported header"),
                            }
                            identifier.parse()?
                        }
                        _ => bail!("unsupported header"),
                    };
                    *heading = next_heading;
                }
                (
                    State::Version {
                        sections,
                        version,
                        heading,
                        nodes,
                        date,
                    },
                    Node::Definition(definition),
                ) => {
                    sections.insert(
                        version.clone(),
                        Section::from_nodes(heading.clone(), std::mem::take(nodes), date.take())?,
                    );
                    definitions.push(definition);
                    state = State::Definitions(&mut definitions);
                }
                (State::Version { nodes, .. }, node) => {
                    nodes.push(node);
                }
                (State::Definitions(definitions), Node::Definition(definition)) => {
                    definitions.push(definition);
                }
                (State::Definitions { .. }, _) => {
                    bail!("tailing stuff other than definitions")
                }
            }
        }
        let (base_url, mut tag) = definitions
            .last()
            .context("no base version")?
            .url
            .rsplit_once(&format!("/{tag_prefix}"))
            .context("invalid url: no /{version}")?;
        let base_url = base_url
            .strip_suffix("/releases/tag")
            .context("invalid url: no /releases/tag before /{version}")?;
        for def in definitions.iter().rev().skip(1) {
            let prefix = format!("{base_url}/compare/{tag_prefix}{tag}...");
            tag = def.url.strip_prefix(&prefix).context("invalid url")?;
            let head = tag == "HEAD" || def.identifier == "unreleased";
            if !head {
                tag = tag.strip_prefix(tag_prefix).context("missing tag prefix")?;
            }
            let matches = if head {
                tag == "HEAD" && def.identifier == "unreleased"
            } else {
                tag == def.identifier
            };
            if !matches {
                bail!("tag doesn't match the version");
            }
        }
        if tag != "HEAD" {
            bail!("no ...HEAD URL");
        }
        let prior_releases = sections
            .keys()
            .any(|v| matches!(v, MaybeVersion::Version(version) if version.pre.is_empty()));
        let rearrange = !prior_releases || version.pre.is_empty();
        if rearrange {
            let latest: Version = definitions
                .get(1)
                .context("no versions")?
                .identifier
                .parse()?;
            if version != latest {
                let base = definitions[0]
                    .url
                    .strip_suffix(&format!("{tag_prefix}{latest}...HEAD"))
                    .context("invalid prefix")?;
                let url = format!("{base}{tag_prefix}{latest}...{tag_prefix}{version}");
                definitions[0].url = format!("{base}{tag_prefix}{version}...HEAD");
                definitions.insert(
                    1,
                    Definition {
                        position: None,
                        url,
                        title: None,
                        identifier: version.to_string(),
                        label: None,
                    },
                );
            }
            if let Some(mut section) = sections.remove(&MaybeVersion::Unreleased) {
                let children = match section
                    .heading
                    .children
                    .get_mut(0)
                    .context("invalid heading")?
                {
                    Node::LinkReference(LinkReference {
                        children,
                        identifier,
                        label,
                        ..
                    }) => {
                        *identifier = version.to_string();
                        *label = Some(version.to_string());
                        children
                    }
                    _ => bail!("invalid heading"),
                };
                match children.get_mut(0).context("invalid heading")? {
                    Node::Text(Text { value, .. }) => {
                        *value = version.to_string();
                    }
                    _ => bail!("invalid heading"),
                }
                match sections.entry(MaybeVersion::Version(version.clone())) {
                    btree_map::Entry::Vacant(entry) => {
                        entry.insert(section);
                    }
                    btree_map::Entry::Occupied(entry) => {
                        let (k, v) = entry.remove_entry();
                        sections.insert(k, v + section);
                    }
                }
            }
            sections.insert(
                MaybeVersion::Unreleased,
                Section {
                    heading: Heading {
                        children: vec![Node::LinkReference(LinkReference {
                            children: vec![Node::Text(Text {
                                value: "Unreleased".into(),
                                position: None,
                            })],
                            position: None,
                            reference_kind: ReferenceKind::Shortcut,
                            identifier: "unreleased".into(),
                            label: Some("Unreleased".into()),
                        })],
                        position: None,
                        depth: 2,
                    },
                    prefix: Vec::new(),
                    added: None,
                    changed: None,
                    deprecated: None,
                    removed: None,
                    fixed: None,
                    security: None,
                    date: None,
                },
            );
        }
        Ok(Self {
            preface,
            sections,
            definitions,
            version,
        })
    }
}

fn with_package<T>(
    mut version: Version,
    package_root: impl AsRef<Path>,
    tag_prefix: &str,
    f: impl FnOnce(&mut Vec<Node>, Document, PathBuf) -> anyhow::Result<T>,
    on_missing: impl FnOnce() -> anyhow::Result<T>,
) -> anyhow::Result<T> {
    version.build = BuildMetadata::EMPTY;
    let path = package_root.as_ref().join("CHANGELOG.md");
    if !path.exists() {
        return on_missing();
    }
    let changelog = std::fs::read_to_string(&path).context("failed to read CHANGELOG.md")?;
    let mut ast = to_mdast(&changelog, &ParseOptions::gfm()).map_err(|e| anyhow::anyhow!("{e}"))?;
    let children = ast.children_mut().context("no children nodes")?;
    let document = Document::from_children(children, version, tag_prefix)?;
    f(children, document, path)
}

pub fn version_changelog(
    mut version: Version,
    package_root: impl AsRef<Path>,
    tag_prefix: &str,
) -> anyhow::Result<Option<String>> {
    version.build = BuildMetadata::EMPTY;
    with_package(
        version.clone(),
        package_root,
        tag_prefix,
        |_, Document { mut sections, .. }, _| {
            let Some(section) = sections.remove(&MaybeVersion::Version(version)) else {
                return Ok(None);
            };
            let mut children = section.into_nodes();
            children.remove(0);
            for node in &mut children {
                if let Node::Heading(Heading { depth, .. }) = node {
                    *depth -= 1;
                }
            }
            to_string(
                &Node::Root(Root {
                    children,
                    position: None,
                }),
                Options {
                    setext: true,
                    ..options()
                },
            )
            .map(Some)
        },
        || Ok(None),
    )
}

fn options() -> Options {
    Options {
        bullet: '-',
        bullet_ordered: '.',
        bullet_other: '*',
        close_atx: false,
        emphasis: '*',
        list_item_indent: IndentOptions::One,
        setext: false,
        strong: '*',
        tight_definitions: true,
        ..Default::default()
    }
}

fn to_string(node: &Node, options: Options) -> anyhow::Result<String> {
    to_markdown_with_options(node, &options).map_err(|e| anyhow::anyhow!("{e}"))
}

pub fn bump_one_changelog(
    version: Version,
    package_root: impl AsRef<Path>,
    tag_prefix: &str,
    dry_run: bool,
) -> anyhow::Result<()> {
    with_package(
        version,
        package_root,
        tag_prefix,
        |children, document, path| {
            document.into_nodes(children, tag_prefix)?;
            let changelog = to_string(
                &Node::Root(Root {
                    children: std::mem::take(children),
                    position: None,
                }),
                options(),
            )?;
            if !dry_run {
                std::fs::write(path, changelog).context("failed to write CHANGELOG.md")?;
            }
            Ok(())
        },
        || Ok(()),
    )?;
    Ok(())
}
