# Rust (`cargo`) support

*provided by `tagit-workspace-cargo`*

## Workspaces

Supported.

## Versioning

Uses versions from `Cargo.toml` as is.

## Config

Optional section in `Cargo.toml`:

```toml
[package.metadata.tagit]
skip = false # or `true`; optional
skip_retag = false # or `true`; optional
```

## Diff

Presently, only includes `src` and `Cargo.toml`.
