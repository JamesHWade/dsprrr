## R CMD check results

The complete local check of dsprrr 0.1.0 on 2026-11-16 reported:

0 errors | 0 warnings | 1 note

* This is a new submission.

The check used `devtools::check(remote = TRUE, manual = TRUE,
force_suggests = TRUE, run_dont_test = TRUE)` with API credentials removed.
Examples, tests, vignettes, and both PDF and HTML manuals passed.

The checked source includes the native helper that preserves exact filesystem
IDs and the documented compiler prerequisite. The full test suite passed
8,869 assertions; compilation, compiled-code checks and manual rebuilding
also passed.

## Test environments

* local macOS Golden Gate 27.0, R 4.6.1 (aarch64)
* Hosted release and Windows-devel qualification results are recorded in
  https://github.com/JamesHWade/dsprrr/issues/143.

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
