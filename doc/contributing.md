# Contributing

See [CONTRIBUTING.md](../CONTRIBUTING.md) in the repository root for:

- Pull request workflow
- Commit message format (`[Prefix][Component] Title`)
- Code review process
- Testing requirements

## Package Development

New packages go in `systole/systole/packages/` under the
`(systole packages ...)` namespace. New services go in
`systole/systole/services/` under `(systole services ...)`.

When adding patches, create a subdirectory under
`systole/systole/packages/patches/` and register it in
`%patch-path` in `systole/systole/packages.scm`.

## Architecture

The channel layout is documented in [CLAUDE.md](../CLAUDE.md),
which serves as both an AI assistant guide and an architecture
reference.
