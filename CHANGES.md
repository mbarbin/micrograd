## 0.1.6 (unreleased)

### Added

- Added tests to increase coverage

### Removed

- Replaced [Value.With_tensor] by a [Value_map] returned by [Value.tensor].

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
