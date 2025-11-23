# `tagit changelog`

Rearranges the `CHANGELOG.md` file in all workspace members:

```bash
tagit changelog
```

> [!TIP]
> It's recommended to stage your manual changes before auto-rearrangement, to ensure
> `tagit changelog` is non-destructive.

To check that the changelog is valid without applying any changes, use `--dry-run`:

```bash
tagit tag --dry-run
```

Just like with `tagit tag`, versions are parsed from package manifests.

## Changelog format

We use <https://keepachangelog.com/en/1.1.0/> as the basis.

```markdown
# Changelog

## [Unreleased]

### Added

- (feature)

- (feature)

## [1.0.1]

### Added

- (feature)

### Changed

- (behaviour change)

### Deprecated

- (something planned to be removed)

### Removed

- (something removed)

### Fixed

- (fix)

### Security

- (fix)

## [1.0.0]

(base version)

[unreleased]: (base)/compare/(tag_prefix)1.0.1...HEAD
[1.0.1]: (base)/compare/(tag_prefix)1.0.0...(tag_prefix)1.0.1
[1.0.0]: (base)/releases/tag/(tag_prefix)1.0.0
```

> [!IMPORTANT]
> Deviations from the format usually lead to errors. We don't try to correct them.
