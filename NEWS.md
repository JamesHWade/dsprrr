# dsprrr 0.0.0.9000

First development changelog. dsprrr is experimental; the API may change.

## Breaking changes

* `module()` now constructs only standard prediction modules. Tool use and
  other advanced execution semantics use explicit constructors such as
  `react()`, `chain_of_thought()`, `program_of_thought()`, `code_act()`,
  `rlm_module()`, and `flex()`; advanced arguments passed to `module()` fail
  with a typed, actionable error instead of silently changing module type.
  `compile(program, teleprompter, trainset)` is the only compilation entry point;
  `compile_module()` and the shallow R6 `$predict()` and `$optimize()` aliases
  have been removed. `stats::predict()` remains for data-frame interoperability,
  `module_fn()` remains the custom-module extension seam, and the functional
  `optimize_grid()` interface remains primary.

* The public API now centers on `signature()`, `module()`, `run()`,
  `run_dataset()`, `evaluate()`, and `compile()`. Redundant DSP-style wrappers,
  typed-input convenience constructors, pipeline wrapper helpers, the separate
  ensemble teleprompter, and one-line ellmer registration helpers have been
  removed. Implementation classes such as `Signature`, `Assertion`,
  `OptimizerControl`, and `Trial` are internal; their public constructor and
  collection functions remain the supported interface. This reduces the
  namespace from 188 to 161 exports and removes inert S3 registrations,
  reducing them from 55 to 36, without removing module, optimizer,
  integration, persistence, or inspection capabilities.

* Runtime contracts are current-only. Program artifacts accept format version
  5; persisted trial records require their complete versioned schema; batch
  execution accepts `.concurrency` only; code runners implement `start()`,
  `execute()`, and `shutdown()`; RLM uses `max_iterations`, `llm_query()`, and
  `llm_query_batched()`; and Predict templates interpolate documented
  `{field}` placeholders. Old versions and incomplete records fail closed
  instead of being upgraded or defaulted.

* `input()` is the single input-field constructor. Its `type` is either an
  ellmer type or one of the exact labels `string`, `number`, `integer`,
  `boolean`, `array`, and `object`; S7 classes, `class =`, synonyms, and unknown
  type fallback are no longer accepted.

* Existing private disk caches and trial logs on Unix must already use mode
  `0700` for their directory and `0600` for their files, with no special mode
  bits. dsprrr no longer repairs broader permissions and then reuses the stored
  data; it fails closed before enumeration, deserialization, locking, or
  mutation. To migrate an existing directory, run `chmod 700` on it and
  `chmod 600` on its files; the reported reason names the exact path and
  command. A directory that inherited a setgid bit from a shared parent is
  rejected even though its permission triplet looks correct, and the reason
  says so. A rejected disk cache is reported by `cache_stats()` as degraded
  rather than silently dropping to memory-only.

* Trial logs written before record schema versioning cannot be read. The
  rejection names the missing `schema_version` rather than reporting a generic
  parse failure, and `read_trials_jsonl()` now aborts instead of returning an
  empty list when every record in a non-empty file is rejected. Re-run the
  optimization to write a current log.

* Undeclared dot-prefixed arguments to `run()`, `run_dataset()`, and
  `evaluate()` are an error. Runtime arguments are formal parameters, so a
  dot-prefixed name reaching `...` is a typo or an argument this version no
  longer accepts; it is no longer absorbed as a signature field with a warning.
  Calls passing the removed `.parallel` or `.parallel_method` are named
  explicitly and pointed at `.concurrency`.

* dsprrr does not expose or depend on an external Agent SDK compatibility
  layer. The unused `signature_to_json_schema()` integration hook has been
  removed. `as_ellmer_tool()` and `module_fn()` remain public because they are
  native ellmer and custom-module extension points. Agentic harnesses and RLM
  recursive queries retain separate proposer models, but `.agent_llm` and
  `sub_lm` must now be ellmer `Chat` objects rather than factories or duck-typed
  adapters. COPRO and SIMBA likewise accept only ellmer `Chat` prompt models.
  MIPROv2's unused `prompt_model` and `init_temperature` properties have been
  removed, and its effective `task_model` is now validated as an ellmer `Chat`.
  The unused provider-capability guess table `provider_defaults()` has also
  been removed; provider behavior belongs to the configured ellmer Chat. RAG
  and parsnip now follow that same resolver instead of reconstructing providers
  from `model` and `provider` strings.

## New features

* `flex()` lets GEPA optimize how a module executes—not only its instructions—
  with two source modes. The safe default is a bounded versioned JSON graph with
  allowlisted Predict and Chain-of-Thought steps, typed references, zero-step
  deterministic plans, and transactional validation. Opt-in executable mode
  evaluates a complete R `forward()` program only in a fresh runner from an
  explicit `interpreter_factory`; a versioned JSON bridge exposes the
  DSPy Flex primitive family and named host tools, enforces runtime predictor
  and direct host-tool limits, validates typed outputs, and requires an
  advertised sandbox by default. Guest bindings use a separate lexical
  environment from bridge state, large values use structured runner results
  when available, and tools retain their host closure environments while
  generated source never evaluates in the host R session.

* `GEPA()` now searches complete, validated component candidates for ordinary
  programs and programs containing Flex leaves, spanning ordinary instructions
  and complete `module_src` values. Its source proposer receives the task objective,
  signatures, field descriptions and schemas, source runtime, tools, current
  source, and row-aligned metric feedback. Provider failures propagate;
  malformed structural proposals are recorded but cannot be selected.
  Candidate-program parent pools unite validation-example winners with the
  multi-metric objective Pareto front. Component selection supports
  round-robin, budget-atomic all-component, and custom policies; lineage-aware
  three-way merge caps count attempted merges. When a validation set is
  supplied, training rows remain exclusive to discovery/reflection while
  validation rows drive selection, per-example winners, and optional retained
  outputs.

* `program_of_thought()`, `code_act()`, and `rlm_module()` now accept an
  `interpreter_factory`: a zero-argument function that creates one fresh,
  invocation-owned code runner, which dsprrr shuts down exactly once when the
  invocation ends. A directly supplied `runner` remains caller-owned and is
  reused; whether state persists or can be reset is backend-specific. Supply
  exactly one of `runner` and `interpreter_factory`. RLM now requires the
  selected runner to advertise `persistent = TRUE`; existing
  `rlm_module(..., runner = r_code_runner())` calls must opt into
  `r_code_runner(persistent = TRUE)` or use a persistent factory.

* `r_code_runner(persistent = TRUE)` now keeps one callr process and execution
  environment alive across `execute()` calls. A factory-backed RLM can stage a
  large or rich R context once, preserve derived values between iterations, and
  shut the process down with its invocation-owned lifecycle. The backend
  remains trusted-input-only and retains the host user's permissions.

* `rlm_module()` now exposes graph-visible `generate_action` and `extract`
  predictors. GEPA and the agentic harnesses can tune both; nested MIPROv2 can
  tune their instructions with `max_bootstrapped_demos = 0L`, while unsupported
  child-demo bootstrapping and BootstrapFewShot or LabeledFewShot on programs
  containing RLM fail explicitly. BootstrapFewShotWithRandomSearch rejects the
  same ineligible graphs instead of returning an unchanged baseline marked as
  compiled. `sub_lm = NULL` inherits the outer LM; recursive single and batch queries return
  host-produced values through one nonce-bound, schema-checked ordered replay
  ledger; incompatible typed `SUBMIT()` payloads become repairable
  observations; and the default 10,000-character module excerpt preserves both
  head and tail after any stricter runner limit. Structured results report
  submission versus fallback source, bounded trajectory, requested recursive
  calls, known provider-call attempts, complete usage when every contributing call
  reports it, and runner policy. The one-call `rlm()` helper now creates a fresh
  managed `mcp-repl` sandbox factory by default, while still accepting an
  explicit runner or interpreter factory.

* The code-runner protocol now has explicit `start()`/`shutdown()` lifecycle
  hooks, typed repairable execution versus terminal interpreter failures, and
  terminal-session invalidation. Code modules do not retry or reuse a runner
  after process/protocol failure and preserve the primary failure when teardown
  also fails. RLM host tools now cross a nonce-bound, schema-checked replay bridge, so the
  original live closure executes once on the host without being deparsed or
  serialized into guest code. Factory-backed Program of Thought, CodeAct, and
  RLM modules support isolated async and mirai `run_dataset()` workflows;
  caller-owned runners remain sequential-only. Direct `run()` calls stage each
  RLM input as one REPL variable regardless of its R length; explicit batches
  use `run_dataset()`, with list-columns for rich per-row objects.

* Program artifacts use format version 5, including graph-visible RLM action
  and extraction predictors with their tuned instructions, demos, and optimizer
  state. Restoration requires the complete closed v5 schema and verifies its
  integrity. Artifact construction and restoration never invoke a stored
  factory.

* `program_artifact_id()` exposes the validated SHA-256 identity already stored
  in each program artifact. Restored current-format programs retain their
  validated source ID across compatible producer environments until the program
  changes. `run()`, `run_dataset()`, `evaluate()`, `compile()`, and
  `as_ellmer_tool()` accept strict JSON-compatible correlation context and carry
  it through scalar and batch traces, evaluation results, optimizer trials,
  Flex, and RLM. Execution and evaluation metadata also name the exact program
  artifact identity. Correlation context rejects credential-like field names
  and runtime objects, and never enters prompts, provider requests, cache keys,
  or artifact identity.

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
  treating a partial frame as a submission. Managed startup and teardown now
  terminate and precisely prune the owned process even when MCP initialization
  or graceful transport close fails.

* `MetaHarness()` uses fresh proposer sessions to generate bounded candidate
  batches from a persisted scored frontier. Its trusted R outer loop validates,
  deduplicates, evaluates, checkpoints, and selects joint edits across module
  graphs.

* `Omni()` explores multiple teleprompters from the same seed, selects their
  best program with one shared validation metric, and runs a fresh continuation
  optimizer without allowing a regressing stage to replace a better candidate.

## Bug fixes

* DSPy 3.3 execution contracts are enforced in the R runtime: `rlm_module()`
  rejects duplicate, reserved, missing, and ellipsis-style tool names, rejects
  unexpected invocation inputs, and no longer stringifies arbitrary sub-LM
  responses. RLM submit/query control
  frames now survive text-only runners through versioned, per-invocation
  nonce-bound envelopes; malformed and duplicate frames fail closed, and
  one-query batches retain their array shape. `code_act()` now limits tool
  calls executed inside ellmer's internal tool loop and protects its built-in
  runner-tool namespace. Invocation-bound decoding ignores valid stale frames
  while requiring exactly one frame for the current invocation, and `SUBMIT()`
  rejects duplicate output names. The generic `module()` factory uses the same
  `max_iterations` spelling and 20-iteration RLM default. CodeAct tool names are
  validated against ellmer's provider-neutral grammar before registration.

* Code-executing modules validate runner results consistently and preserve the
  primary execution error if teardown also fails. ProgramOfThought validates
  its runner and iteration bound at both public and direct-constructor
  boundaries. Factory-created runners must expose a zero-argument `shutdown()`
  before module work begins. RLM ignores submit/query control values from
  failed runner results instead of allowing failure payloads to terminate or
  recurse.

* Direct provider async and streaming entry points now fail closed for modules
  and composites with specialized `forward()` semantics; only ordinary
  Predict modules use those paths. `run_stream()` retains its one-shot
  `forward()` fallback, while matching token-stream requests are preflighted
  across pipeline steps and rejected before provider work if they would bypass
  specialized execution or runner lifecycle contracts.

* Composite and retry modules report canonical `provider_calls`, token fields,
  and `cost` metadata. Nested usage is summed when every child reports it;
  missing child usage and swallowed child failures remain unknown so finite
  optimizer budgets fail closed instead of accepting partial totals.

* Flex no longer silently accepts BootstrapFewShot demonstrations that its
  runtime cannot consume. Predictor-call limits may be `NULL`, declarative
  input-only plans avoid provider resolution, and each actual inner predictor
  call produces exactly one ordered trace event. Native concurrent dataset
  execution supports declarative zero/one-step Flex, while multi-step and
  executable requests fail before provider work until a row-isolated async
  engine is available.

* Larger executable Flex requests now cross `mcp_repl_runner()` reliably.
  dsprrr realizes `mcptools` wrapper arguments before invocation and compacts
  only host-generated requests that exceed the wire bound, preserving fitting
  high-entropy requests. Flex control frames are decoded from raw inline output
  before display truncation. A plain file preview is accepted only when it
  contains one bounded current-step Flex frame; malformed inline frames,
  ambiguous previews, and all RLM previews still fail closed.

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

* `TrialLog` now requires pre-existing private Unix log directories to be mode
  exactly `0700` and pre-existing log files to be mode exactly `0600`, with no
  special bits. Every existing ancestor must be owned by root or the effective
  user, including sticky parents. Initialization and save-directory overrides
  preflight every known target, including `metadata.json`, before locking,
  reading, or mutating. Unsafe paths fail closed without silent repair; newly
  created storage remains owner-only.

* Agentic harness seeds are constrained to R's integer range and compile calls
  restore the caller's RNG state. MCP REPL reset now treats protocol-level
  errors as failures instead of silently succeeding.

* `configure_cache()` now keeps persistent response envelopes in the
  platform-specific per-user cache directory by default. Unix cache directories
  and files are bound to their effective owner, canonical identity, and exact
  private POSIX modes without special bits before every serialized read or
  write. Every existing ancestor, including a sticky parent, must be owned by
  root or the effective user. Unsafe or unverifiable caches fall back to memory
  when enabled, or leave no cache tier active; extended ACL and Windows
  inherited-ACL boundaries are reported honestly. Project-local and shared
  caches require an explicit path, and disabling privacy enforcement requires
  `disk_private = FALSE` (#dsprrr-etge).

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
  or bypassing specialized logic. `run()` and `run_dataset()` retain named
  declared output records consistently across
  scalar, batch, and Flex execution, and all batch routes isolate mutable Chat
  state per row. Native ellmer batches retain row failures
  for non-object outputs through an internal typed wrapper, including valid
  optional `NULL` values, and schemas whose optional nested presence is
  ambiguous use isolated scalar rows instead of guessing between absent and
  present-empty values.
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
  it for the structured-output cache. RLM forwards it to the graph-visible
  action and fallback predictors; recursive `llm_query()` calls and runner
  execution remain uncached. Modules that drive the LLM directly (e.g.
  `RAGModule` and `ReActModule`) accept `.cache` but do not yet route their own
  calls through the cache (#dsprrr-aa2).

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
