# Contributing to Yorum

Thank you for your interest in contributing to Yorum. All contributions are made through pull requests.

## Getting Started

1. Fork the repository
2. Clone your fork
3. Create a branch from `main` (see naming conventions below)
4. Make your changes
5. Verify all checks pass
6. Push and open a pull request

## Branch Naming Conventions

All branches must be created from `main` and follow these naming patterns:

| Prefix | Purpose | Example |
|---|---|---|
| `feature/` | New functionality or enhancements | `feature/networking` |
| `bug/` | Bug fixes | `bug/parser-crash-on-empty-input` |
| `docs/` | Documentation changes only | `docs/update-readme` |
| `refactor/` | Code restructuring without behavior changes | `refactor/codegen-cleanup` |
| `test/` | Adding or improving tests | `test/closure-edge-cases` |
| `chore/` | Build, CI, tooling, or dependency updates | `chore/update-serde` |

Use short, descriptive, kebab-case names. Never put version numbers in branch names — versions are only for tags and releases.

## Before Opening a Pull Request

All three CI checks must pass locally before you open a PR:

```bash
cargo fmt --check                              # formatting
cargo clippy --all-targets -- -D warnings      # linting
cargo test                                     # all tests
```

If you added new functionality, add corresponding tests in `tests/integration_tests.rs` (and unit tests in the relevant module if appropriate).

## Pull Request Guidelines

Every PR must include:

- **What** — A clear description of the changes made. List the files modified and what was added, removed, or changed.
- **Why** — The motivation behind the change. Explain the problem being solved or the feature being added and why this approach was chosen.

### PR format

```
## Summary
Brief description of what this PR does and why.

## Changes
- List of specific changes

## Test plan
- How the changes were tested
- Any new tests added
```

### Additional guidelines

- Keep PRs focused. One logical change per PR. If you are fixing a bug and also want to refactor nearby code, open separate PRs.
- Do not include unrelated formatting or whitespace changes.
- If the PR addresses an open issue, reference it (e.g., "Fixes #12").
- Ensure your commits have clear, descriptive messages.

## Development Reference

See [CLAUDE.md](CLAUDE.md) for build commands, architecture overview, and codebase conventions.

## Code of Conduct

Be respectful and constructive in all interactions. Focus on the technical merits of contributions.
