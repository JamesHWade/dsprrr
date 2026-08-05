# dsprrr 0.0.0.9000

First development changelog. dsprrr is experimental; the API may change.

## New features

* DSPy 3.3 alignment adds immutable `with_instructions()` and
  `append_instructions()` transforms, plus `metric_with_trace()` for objectives
  that score both outputs and row-owned execution traces. Structured
  evaluations now return final-epoch `traces` and, for repeated evaluations,
  `epoch_traces`; optimizer examples carry the corresponding `program_trace`.

* `AutoResearch()` runs a persistent, provider-neutral research agent over
  validated multi-module instruction and template snapshots. The agent can
  request OS-sandboxed R experiments, branch from prior candidates, and choose
  when to finish while dsprrr owns evaluation, budgets, lineage, checkpoints,
  and best-partial selection.

* `mcp_repl_runner()` connects code-executing modules and agentic optimizers to
  Posit's persistent `mcp-repl` R runtime with OS-enforced workspace-write
  sandboxing and network access disabled by default. Injected `repl` functions
  are now marked unverified and fail closed when an agentic harness requires an
  OS sandbox; only connections launched and configured by dsprrr advertise the
  managed sandbox policy. Arbitrary mcp-repl `extra_args` are rejected so
  callers cannot override the enforced filesystem or network policy. For RLM
  control traffic, encoded frames are capped below mcp-repl's inline-output
  threshold and file-preview or pager compaction fails closed rather than
  treating a partial frame as a submission.

* `MetaHarness()` uses fresh proposer sessions to generate bounded candidate
  batches from a persisted scored frontier. Its trusted R outer loop validates,
  deduplicates, evaluates, checkpoints, and selects joint edits across module
  graphs.

* `Omni()` explores multiple teleprompters from the same seed, selects their
  best program with one shared validation metric, and runs a fresh continuation
  optimizer without allowing a regressing stage to replace a better candidate.

## Bug fixes

* DSPy 3.3 execution contracts are enforced in the R runtime: `rlm_module()`
  accepts the `max_iters` alias, rejects duplicate, reserved, missing, and
  ellipsis-style tool names, rejects unexpected invocation inputs, and no
  longer stringifies arbitrary sub-LM responses. RLM submit/query control frames
  now survive text-only runners through versioned, per-invocation authenticated
  envelopes; malformed and duplicate frames fail closed. `code_act()` now
  limits tool calls executed inside ellmer's internal tool loop and protects
  its built-in runner-tool namespace. Authenticated decoding ignores valid
  stale frames while requiring exactly one frame for the current invocation,
  and `SUBMIT()` rejects duplicate output names. The generic `module()` factory
  now routes RLM's `max_iters` alias instead of silently using its
  `max_iterations` default, and rejects supplying both spellings. CodeAct list
  aliases are validated against ellmer's provider-neutral tool-name grammar
  before registration. The RLM alias is appended after the pre-existing
  positional arguments so older positional calls retain their meaning.

* DSPy's 3.3 fresh-interpreter factory is not yet mirrored.
  `ProgramOfThoughtModule`, `RLMModule`, and `CodeActModule` retain their
  configured runner; persistent backends keep state across calls and must be
  reset between logically isolated jobs. One persistent runner must not be
  shared by concurrent invocations. ProgramOfThought now validates its runner
  and iteration bound at both public and direct-constructor boundaries.

* Metric correctness and composition are stricter: token F1 uses multiset
  overlap, numeric field equality no longer fails solely because one value is
  integer and the other double, `metric_custom()` accepts score-plus-feedback
  results, requested-but-missing fields fail instead of earning accidental
  perfect credit, and `metric_threshold()` preserves feedback and trace
  dispatch. Trace metrics keep row identity through optimizer budgets, receive
  Bootstrap execution metadata, and use the primary GEPA metric's output field.
  Trace dispatch supports both an explicitly named `program_trace` formal
  around `...` and a positional third trace argument before `...`.

* Signatures reject invalid, duplicate, and input/output-colliding field names.
  Multi-output types correctly preserve colons inside quoted enum values.
  Optimizer instruction updates now replace signatures copy-on-write instead
  of mutating a shared signature object.

* Agentic harness seeds are constrained to R's integer range and compile calls
  restore the caller's RNG state. MCP REPL reset now treats protocol-level
  errors as failures instead of silently succeeding.

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
  (#dsprrr-ywhf). Owned mirai shutdown now initiates a non-blocking reset and
  bounds verification against the batch deadline.

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
  fine-grained resume engines remain explicitly tracked follow-ups. Sticky
  error-budget stops survive checkpoint round-trips, and MIPRO retains its
  requested worker count when no explicit optimizer control overrides it
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
  standalone code export reuse the same validated, current-version manifest.
  Unpublished legacy shapes are rejected rather than interpreted through a
  second constructor path, and restoration uses the stored signature rather
  than accepting an out-of-band override. Credential-like demo fields fail
  instead of silently changing program semantics, and remote content rejects
  recognizable signed-path credentials. Local persistence rejects same-file
  aliases before publication and documents its stable-local-filesystem and
  trusted-directory atomicity boundary (#dsprrr-g6gq, #dsprrr-07u).

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
