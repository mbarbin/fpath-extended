## 0.0.6 (unreleased)

### Added

- Added new modules `Absolute_path`, `Relative_path` to distinguish absolute
  from relative path at the type level.

### Changed

- Design the package now so that it is meant to be open.
- Override the `Fpath` in the scope, so you can use e.g. `Fpath.sexp_of_t` directly.

### Deprecated

### Fixed

### Removed

- Remove `Arg_type` and dependency into `core.command`. To be revisited.

## 0.0.5 (2024-02-14)

### Added

- Add new test && increase coverage.

### Changed

- Upgrade dune to `3.14`.
- Build the doc with sherlodoc available to enable the doc search bar.
- Couple `hash` with `hash_fold_t` for consistency.

## 0.0.4 (2024-02-09)

### Added

- Setup `bisect_ppx` for test coverage.

### Changed

- Internal changes related to the release process.
- Upgrade dune and internal dependencies.

## 0.0.3 (2024-01-18)

### Changed

- Internal changes related to build and release process.

## 0.0.2 (2023-11-12)

### Added

- Add `arg_type` to parse paths from params of core commands.

## 0.0.1 (2023-11-07)

Initial release.

### Added

- Add `hash` and `sexp_of`. This makes `Fpath_extended` compatible with Base
  Containers such as Map, Set, Hashtbl, etc.
