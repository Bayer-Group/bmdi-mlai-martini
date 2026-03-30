# martini Release Checklist

## Pre-release (on `dev` branch)

- [ ] **Decide release version number** — use `major` / `minor` / `patch` based on the scope of changes in `NEWS.md`
- [ ] **Update `DESCRIPTION` version** — `usethis::use_version()` to set the release version (drops `.9000`)
- [ ] **Finalize `NEWS.md`** — replace `# martini (development version)` with `# martini x.y.z`
- [ ] **Regenerate package data** — source `data-raw/data-martini.R` end-to-end:
  - Verify the `waldo::compare()` output for `martini_ml_*` objects
  - If changes are expected, run the guarded `usethis::use_data()` block to export updated `.rda` files
- [ ] **Rebuild documentation** — `devtools::document()` and verify no warnings
- [ ] **Run `R CMD check`** — `devtools::check()`, resolve all errors/warnings/notes
- [ ] **Run full test suite** — `devtools::test()`, all tests pass
- [ ] **Build and check pkgdown site** — `pkgdown::build_site()` to ensure vignettes and docs render (preview with `servr::httd("docs")`)
- [ ] **Review spelling** — `spelling::spell_check_package()`
- [ ] **Commit all changes** on `dev`

## Merge to `main`

- [ ] **Open PR** from `dev` → `main`
- [ ] **Code review** — get LGTM from a maintainer
- [ ] **Merge PR** into `main`
- [ ] **Tag release** — `git tag vx.y.z` on `main` and push tag
- [ ] **Create GitHub Release** from the tag, copy `NEWS.md` entry as release notes

## Post-release (back on `dev`)

- [ ] **Bump version to dev** — `usethis::use_dev_version()` to append `.9000` and 
add new dev header to `NEWS.md` (add `# martini (development version)` at the top)
- [ ] **Commit and push** to `dev`
