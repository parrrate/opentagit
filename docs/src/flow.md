# Example Workflow

## Init

```bash
tagit changelog init
```

## Make changes

edit stuff

```bash
vim src/somefile.rs
```

mention that in the changelog

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
git commit -m "..."
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
git commit -m "..."
git push
```

update tags

```bash
tagit tag
```
