## Resubmission

The previous 0.1.0 incoming check on Debian had errors when a writable,
non-sticky ancestor of the temporary directory prevented private persistent
storage. The examples and tests now respect that restriction without weakening
package storage checks. The second 0.1.0 pretest passed examples and tests on
Debian and Windows, but Windows reported an overall check time of 17 minutes.

This revised 0.1.0 tarball keeps the version because no previous submission
was published. The CRAN check runs core contracts and representative integration
tests, including native file identity and the cache, trial-log, and
checkpoint boundaries. The full integration suite still runs in developer
CI. The Omni help topic now links to the same upstream announcement at a
revision-pinned source on GitHub, addressing the TLS failure on the former
URL reported by the Windows pretest.

This is the first release of dsprrr on CRAN. The spelling flags for
"Khattab et al.", "LLMs", "composable", and "optimizable" are intentional.

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
