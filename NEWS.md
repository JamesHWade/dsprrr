# dsprrr (development version)

First development changelog. dsprrr is experimental; the API may change.

## Bug fixes

* Failed items in a batch are now counted and reported. Previously
  `print()` on a batch result always said "All items completed successfully"
  because it looked for the error in the wrong place (#dsprrr-8l0).

* Unknown or misspelled types in a signature string (e.g. `"q -> a: interger"`)
  now raise an error suggesting the closest valid type, instead of silently
  becoming a string field (#dsprrr-47p).

* `optimize_grid()` now gives a clear error when every trial fails (for
  example, when the API is unreachable) instead of a cryptic
  "attempt to select less than one element" (#dsprrr-hew).

* `.cache` is now a validated, first-class argument to `run()` and `forward()`
  for every module type, instead of triggering a spurious "unknown input"
  warning and being dropped for non-`PredictModule` modules (#dsprrr-jup).
  `PredictModule` (and the wrapper/few-shot modules that delegate to it) honor
  it for the structured-output cache; modules that drive the LLM directly
  (e.g. `RAGModule`, `ReActModule`, `RLMModule`) accept `.cache` but do not yet
  route their own calls through the cache (#dsprrr-aa2).

* `BootstrapFewShot` now harvests demonstrations when the metric targets a
  specific output field (e.g. `metric_exact_match(field = "answer")`).
  Previously the single-module path passed a bare value to the metric, which
  errored internally and silently bootstrapped zero demos (#dsprrr-s3b).

* Evaluation now treats a failed prediction as a score of 0 rather than
  dropping it from the mean, so optimizers no longer prefer configurations
  that fail on most of the data (#dsprrr-tn1).

* `BestOfN`, `Refine`, and `Assert` now thread a per-attempt `rollout_id` into
  the cache key, so retries make distinct attempts instead of replaying one
  cached response when caching is enabled. Nested wrappers (e.g.
  `refine(best_of_n(mod))`) compose their ids cleanly instead of crashing with
  a duplicate-argument error (#dsprrr-pcd, #dsprrr-wx6).

* `pin_module_config()` now errors clearly for pipelines and other unsupported
  module types instead of silently serialising them as an empty `PredictModule`
  and dropping every step on restore (#dsprrr-07u).

## Internal

* Tests now isolate the on-disk cache to a temporary directory, so running
  the test suite no longer writes into the package source tree (#dsprrr-63v).

## Packaging

* Lowered the minimum `ellmer` requirement to the released `>= 0.4.1` and
  removed the `Remotes:` entry, so the package installs from CRAN-released
  dependencies (#dsprrr-w0e).

* Removed unused `Suggests`: `future` and `lifecycle`.
