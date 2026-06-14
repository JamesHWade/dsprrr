# dsprrr (development version)

First development changelog. dsprrr is experimental; the API may change.

## Bug fixes

* `BootstrapFewShot` now harvests demonstrations when the metric targets a
  specific output field (e.g. `metric_exact_match(field = "answer")`).
  Previously the single-module path passed a bare value to the metric, which
  errored internally and silently bootstrapped zero demos (#dsprrr-s3b).

* Failed items in a batch are now counted and reported. Previously
  `print()` on a batch result always said "All items completed successfully"
  because it looked for the error in the wrong place (#dsprrr-8l0).

* Unknown or misspelled types in a signature string (e.g. `"q -> a: interger"`)
  now raise an error suggesting the closest valid type, instead of silently
  becoming a string field (#dsprrr-47p).

* `optimize_grid()` now gives a clear error when every trial fails (for
  example, when the API is unreachable) instead of a cryptic
  "attempt to select less than one element" (#dsprrr-hew).

* `.cache` is now honored by all module types and pipelines, not only
  `PredictModule`. Previously it was silently dropped (with a spurious
  "unknown input" warning) for every other module (#dsprrr-jup).

* Evaluation now treats a failed prediction as a score of 0 rather than
  dropping it from the mean, so optimizers no longer prefer configurations
  that fail on most of the data (#dsprrr-tn1).

## Internal

* Tests now isolate the on-disk cache to a temporary directory, so running
  the test suite no longer writes into the package source tree (#dsprrr-63v).

## Packaging

* Lowered the minimum `ellmer` requirement to the released `>= 0.4.1` and
  removed the `Remotes:` entry, so the package installs from CRAN-released
  dependencies (#dsprrr-w0e).

* Removed unused `Suggests`: `future` and `lifecycle`.
