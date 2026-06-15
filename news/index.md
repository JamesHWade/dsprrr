# Changelog

## dsprrr (development version)

First development changelog. dsprrr is experimental; the API may change.

### Bug fixes

- Failed items in a batch are now counted and reported. Previously
  [`print()`](https://rdrr.io/r/base/print.html) on a batch result
  always said “All items completed successfully” because it looked for
  the error in the wrong place (#dsprrr-8l0).

- Unknown or misspelled types in a signature string
  (e.g. `"q -> a: interger"`) now raise an error suggesting the closest
  valid type, instead of silently becoming a string field (#dsprrr-47p).

- [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
  now gives a clear error when every trial fails (for example, when the
  API is unreachable) instead of a cryptic “attempt to select less than
  one element” (#dsprrr-hew).

- `.cache` is now a validated, first-class argument to
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) and
  `forward()` for every module type, instead of triggering a spurious
  “unknown input” warning and being dropped for non-`PredictModule`
  modules (#dsprrr-jup). `PredictModule` (and the wrapper/few-shot
  modules that delegate to it) honor it for the structured-output cache;
  modules that drive the LLM directly (e.g. `RAGModule`, `ReActModule`,
  `RLMModule`) accept `.cache` but do not yet route their own calls
  through the cache (#dsprrr-aa2).

### Internal

- Tests now isolate the on-disk cache to a temporary directory, so
  running the test suite no longer writes into the package source tree
  (#dsprrr-63v).

### Packaging

- Lowered the minimum `ellmer` requirement to the released `>= 0.4.1`
  and removed the `Remotes:` entry, so the package installs from
  CRAN-released dependencies (#dsprrr-w0e).

- Removed unused `Suggests`: `future` and `lifecycle`.
