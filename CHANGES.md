## 0.1.7 (unreleased)

### Added

### Changed

- Use `expect_test_helpers_core.expect_test_helpers_base`.

### Deprecated

### Fixed

### Removed

## 0.1.6 (2024-07-26)

### Changed

- Upgrade `ppxlib` to `0.33` - activate unused items warnings.
- Upgrade `ocaml` to `5.2`.
- Upgrade `dune` to `3.16`.
- Upgrade base & co to `0.17`.

### Added

- Added dependabot config for automatically upgrading action files.
- Added tests to increase coverage

### Removed

- Removed `Torch` dependency. Reasoning:
    - I can't get `torch` to compile reliably at this time. In particular, I have build issues with `torch.v0.17.0`.
    - I find the `Torch` API not easy enough to use for these simple cases, and I am getting runtime exceptions when running code that type checks OK and looks sort of reasonable (although perhaps naively).
    - Keeping only the micrograd part makes the project more self-contained.

## 0.1.5 (2024-03-13)

### Changed

- Uses `expect-test-helpers` (reduce core dependencies)
- Run `ppx_js_style` as a linter & make it a `dev` dependency.
- Upgrade GitHub workflows `actions/checkout` to v4.
- In CI, specify build target `@all`, and add `@lint`.
- List ppxs instead of `ppx_jane`.

## 0.1.4 (2024-02-14)

### Changed

- Upgrade dune to `3.14`.
- Build the doc with sherlodoc available to enable the doc search bar.

## 0.1.3 (2024-02-09)

### Added

- Setup `bisect_ppx` for test coverage.

### Changed

- Internal changes related to the release process.
- Upgrade dune and internal dependencies.

## 0.1.2 (2024-01-18)

### Changed

- Internal changes related to build and release process.
- Generate opam file from `dune-project`.

## 0.1.1 (2023-11-01)

### Changed

- Change changelog format to be closer to dune-release's.
- Add dependency to `Appendable_list`, remove local re-implementation.

## 0.1.0 (2023-08-31)

### Added

- Value engine
- Gradient descent on a small binary classifier example
