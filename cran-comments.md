## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local macOS Golden Gate 27.0 (aarch64), R 4.6.1:
  `R CMD check --as-cran --run-donttest` with remote incoming checks, all
  Suggests installed and API credentials removed. Examples, tests (8,773
  passing assertions), vignettes, and both PDF and HTML manuals passed.
* GitHub Actions: ubuntu-latest (R release and oldrel-1), windows-latest
  (R release and R-devel), macos-latest (R release).
* win-builder: R-devel.

The tests and vignettes do not require API keys or network access; tests that
need a live language-model provider are skipped on CRAN.

## First submission

This is the first CRAN submission of dsprrr.

## Bundled files

The Shiny demo includes rebuilt JavaScript and CSS assets. Complete
third-party notices are in `inst/THIRD-PARTY-NOTICES.md`; a generated manifest
records the locked dependency versions and source/output hashes. Locally
copied UI components and source excerpts in recorded traces are attributed
separately, including the limits of their historical revision metadata.

## Method references

The DESCRIPTION cites the 'DSPy' framework that motivates the package's
declarative language-model programming and optimization interfaces.

## Downstream dependencies

There are currently no downstream CRAN dependencies.
