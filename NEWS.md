# dsprrr 0.0.0.9000

First development changelog. dsprrr is experimental; the API may change.

## Bug fixes

* `configure_cache()` now keeps persistent response envelopes in the
  platform-specific per-user cache directory by default. Unix cache directories
  and files are bound to their effective owner, canonical identity, and private
  POSIX modes before every serialized read or write. Unsafe or unverifiable
  caches fall back to memory when enabled, or leave no cache tier active;
  extended ACL and Windows inherited-ACL boundaries are reported honestly.
  Project-local and shared caches require an explicit path, and disabling
  privacy enforcement requires `disk_private = FALSE` (#dsprrr-etge).

* Empty Predict batches and zero-row datasets now return correctly shaped
  empty results without resolving a provider or changing cache, trace, or
  history state. Batch rows record one canonical ordered trace across
  sequential, native ellmer, mirai, cache-hit, and failure paths; mixed
  zero/non-zero inputs fail during typed preflight. Scalar ellmer content and
  other opaque runtime objects recycle without losing identity, and native
  ellmer batches reconstruct nested objects and arrays with the same row types
  as scalar calls. Direct `PredictModule$run()` batches now use the isolated,
  observable scheduler; unsupported custom and specialized modules reject
  vectorized execution before work instead of silently sharing mutable state
  or bypassing specialized logic. `Module$predict()` retains named output
  records for compatibility even though simple `run()` batches simplify a
  single-field row. Native ellmer batches retain row failures for non-object
  outputs through an internal typed wrapper, including valid optional `NULL`
  values, and schemas whose optional nested presence is ambiguous use isolated
  scalar rows instead of guessing between absent and present-empty values
  (#dsprrr-bbdm).

* `concurrency_control()` now gives batch execution one enforceable contract
  for backend selection, exact in-flight limits, per-task and total timeouts,
  error budgets, and cancellation. Ellmer receives the requested `max_active`;
  mirai runs in a verified dsprrr-owned pool without replacing user topology;
  and every row reports requested and effective execution metadata. Explicit
  backends fail before provider work when their contract cannot be honored
  (#dsprrr-ywhf).

* Cached requests now use versioned, account-partitioned identities covering
  conversation state, provider settings, exact schemas, and multimodal content.
  Cache hits replay provider-recorded semantic turns, including ellmer's native
  structured JSON content, without fabricated usage. Assistant metadata adapts
  to the installed ellmer contract, preserving finish reasons when available
  while remaining cacheable with CRAN ellmer 0.4.1. Opaque/custom Chats and
  registered tools bypass caching. Sequential batch rows preserve isolated,
  completed Chat histories instead of sharing or replacing state. Persistent
  cache envelopes can contain request content, outputs, and turn deltas and must
  be treated as sensitive storage.

* Batch and evaluation failures now preserve the original LLM/provider error.
  Structured `run_dataset()` results expose a row-level `.error` column, and
  `evaluate()` reports run failures separately from metric failures instead of
  passing failed predictions into the metric. Native ellmer parallel batches
  continue after individual request failures (#dsprrr-hqp, #dsprrr-lr7).

* Unknown provider costs remain `NA` through traces, evaluation, optimizer
  trials, and summaries instead of being reported as `$0`. `optimize_grid()`
  now records per-trial cost explicitly (#dsprrr-e3u).

* `ReActModule` now enforces `max_iterations`, preserves native ellmer turn
  history and tool-call IDs, treats parallel calls in one assistant turn as one
  iteration, and records structured finalization metadata (#dsprrr-7nu).

* The built-in R code runner now advertises its real trust boundary: callr
  provides process isolation, not a security sandbox. Code-executing modules
  accept external container/OS sandbox backends through a documented
  `execute()` + `policy()` protocol. Subprocess workers shed parent-process
  source metadata before transport, avoiding instrumented-namespace startup
  costs without weakening execution checks (#dsprrr-ady).

* Failed items in a batch are now counted and reported. Previously
  `print()` on a batch result always said "All items completed successfully"
  because it looked for the error in the wrong place (#dsprrr-8l0).

* Unknown or misspelled types in a signature string (e.g. `"q -> a: interger"`)
  now raise an error suggesting the closest valid type, instead of silently
  becoming a string field (#dsprrr-47p).

* `optimize_grid()` now gives a clear error when every trial fails (for
  example, when the API is unreachable) instead of a cryptic
  "attempt to select less than one element" (#dsprrr-hew).

* `optimizer_control()` now applies `max_errors` as an exact consecutive-error
  boundary while retaining total-error and completed-evaluation overshoot
  metadata consistently across optimizers. Bootstrap random search preserves
  validation outcomes row by row without adding a second candidate summary,
  and returns its typed baseline/partial result when a strict resource budget
  blocks the first validation.

* Optimizer controls now cap metric and provider calls, input/output/total
  tokens, known cost, and monotonic elapsed time without treating unknown usage
  as free. Private atomic checkpoints preserve fingerprints, RNG state,
  counters, lineage, best partial programs, and append-only trial logs;
  Bootstrap and MIPRO resume without repeating completed paid rows. GEPA,
  SIMBA, and COPRO share the same ledger and typed stop reasons while their
  fine-grained resume engines remain explicitly tracked follow-ups
  (#dsprrr-krq4).

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

* New `program_artifact()`, `save_program()`, and `load_program()` APIs provide
  a versioned whole-program contract for nested module graphs, shared identity,
  multimodal demos, and curated optimization state. Runtime objects use stable
  registry IDs by default or require dual `trusted = TRUE` opt-in; pins and
  standalone code export reuse the same validated manifest, and legacy v2 pins
  migrate without retaining raw trial/runtime history. Local persistence rejects
  same-file aliases before publication and documents its stable-local-filesystem
  and trusted-directory atomicity boundary (#dsprrr-g6gq, #dsprrr-07u).

## Internal

* Tests now isolate the on-disk cache to a temporary directory, so running
  the test suite no longer writes into the package source tree (#dsprrr-63v).

## Packaging

* Source archives exclude local dsprrr/vignette caches, editor state, and review
  configuration. R >= 4.1 is now explicit because generated examples use the
  native pipe (#dsprrr-wn9).

* Runtime ellmer compatibility checks now match `DESCRIPTION` (`>= 0.4.1`).
  Dictionary signatures also avoid ellmer's deprecated factory argument while
  remaining compatible with the released minimum.

* Lowered the minimum `ellmer` requirement to the released `>= 0.4.1` and
  removed the `Remotes:` entry, so the package installs from CRAN-released
  dependencies (#dsprrr-w0e).

* Removed unused `Suggests`: `future` and `lifecycle`.

## Quality

* The full test suite is warning-free, expected warning/error sequences are
  asserted explicitly, and vitals/cache state is isolated in temporary
  directories. Static analysis (`jarl`) is clean, and R CMD check CI now covers
  R-devel and oldrel-1 in addition to release R on Linux, macOS, and Windows
  (#dsprrr-h8k).
