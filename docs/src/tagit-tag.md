# `tagit tag`

Updates repository tags in accordance with workspace's package manifests.

Normal way to run it is:

```bash
tagit tag
```

> [!CAUTION]
> By default, this will tag *all* packages that it can find. See `tagit.skip` config.

If you cannot enable commit signing on your system, you'll have to actively opt out:

```bash
tagit tag --sign=false
```

> [!NOTE]
> `tagit` forces signing by default to avoid accidentally missing a signature. This is because of
> how parrrate uses `tagit` internally.

To check what `tagit tag` is going to do without applying any changes, use `--dry-run`:

```bash
tagit tag --dry-run
```

## Retagging

By default, all stable releases (ones without the pre-release segment), when a new tag is created
for them, also update related Rust-style SemVer tags:

- If `--no-retag` is used, no extra tags are created or updated.
- If tag without `+metadata` doesn't exist, it gets created.
- If `--total-order` is used, tag without `+metadata` gets updated.
- `A.B.C` with `A` greater than or equal to 1 updates `A.B` and `A`
- `0.B.C` with `B` greater than or equal to 1 updates `0.B`

## Changelog

When a new tag is created, its message is pulled from the related section of the
[`CHANGELOG.md`](./tagit-changelog.md).

> [!WARNING]
> If anything goes wrong at this step, it's possible that it'll get silently ignored.
