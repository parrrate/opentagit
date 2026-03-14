# Example Workflow

> [!IMPORTANT]
> `tagit` currently uses only `1.2.3` tags for versions, not `v1.2.3`. This is highly unlikely to
> change, except for maybe the major-only `v1` in addition to `1`, because of compatibility
> (some tools misunderstand `1` as short hash)

## Init

```bash
tagit changelog init
```

## Make changes

edit stuff

```bash
vim src/somefile.rs
```

mention that in the changelog within the `[Unreleased]` section

```bash
vim CHANGELOG.md
```

bump the version; if you're on `1.2.3`, bump to `1.2.4-a.0`; if you're on `1.2.4-a.0`, bump to
`1.2.4-a.1`; and so on...

```bash
vim Cargo.toml
```

update `Cargo.lock`

```bash
cargo clippy
```

commit (or skip directly to the next section)

```bash
git add src/somefile.rs CHANGELOG.md Cargo.toml Cargo.lock
# or just git add .
git commit -m "..." # write what changed instead of `...`
```

## Prepare release

if necessary, bump to a non-prerelease version; if you're on `1.2.4-a.1`, then bump to `1.2.4`

```bash
vim Cargo.toml
cargo clippy
```

re-arrange the changelog

```bash
tagit changelog
```

commit

```bash
git add CHANGELOG.md Cargo.toml Cargo.lock
# or just git add .
git commit -m "..." # instead of `...`, write either what changed or release version
git push
```

update tags

```bash
tagit tag
```

assuming you've set up CI with <https://codeberg.org/parrrate/forgejo-release>, the changelog will
show up in the release
