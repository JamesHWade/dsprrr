# Agentic Optimization with AutoResearch and Meta-Harness

Most dsprrr teleprompters own both candidate generation and selection.
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
and
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)
split those responsibilities differently: an LLM proposes experiments,
while a trusted R evaluator applies validated edits, runs the program,
enforces budgets, and preserves the best result.

Both harnesses can jointly edit the instructions and templates of every
optimizable leaf in a module graph. This makes them useful when the
search problem is not “find one better prompt,” but “repair the contract
among several pipeline stages.”

[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
and
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)
do **not** change module topology or run optimizer-authored module
source. They edit allowlisted instructions and templates while the
trusted R evaluator keeps the workflow fixed.

Use the separate experimental
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) module
when GEPA should optimize the implementation strategy itself: predictor
structure, deterministic R, or selected tools. See [Flex: Optimize the
Whole
Program](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md)
for that workflow and its sandbox boundary.

## Choose the Search Owner

| Harness | Who owns the loop? | Agent memory | Candidate shape |
|----|----|----|----|
| [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md) | One research agent | Persistent across experiments | One candidate per evaluate action |
| [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md) | Trusted R outer loop | Fresh proposer each iteration | A bounded candidate batch |

[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
is the better fit when a researcher should pursue a line of inquiry, use
earlier results as working memory, branch, and decide when to stop.
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)
is the better fit when you want independent proposals, explicit frontier
selection, and less dependence on one conversation’s hidden state.

In both cases, dsprrr:

1.  evaluates the unmodified program as a baseline;
2.  gives every candidate a content fingerprint and parent id;
3.  rejects unknown module paths and unsupported fields;
4.  deduplicates equivalent program snapshots;
5.  evaluates candidates through
    [`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md);
    and
6.  returns the best scored program, including when a budget stops the
    run.

## Keep Code Execution Sandboxed

The harnesses require an operating-system-sandboxed runner by default.
With no injected `repl` function,
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
launches Posit’s open-source
[`mcp-repl`](https://github.com/posit-dev/mcp-repl), which provides a
persistent R session behind one MCP `repl(input, timeout_ms)` tool.

Install the executable and the R MCP client:

``` sh
pipx install posit-mcp-repl
# or: uv tool install posit-mcp-repl
```

``` r

install.packages("mcptools")
```

Then create the runner:

``` r

runner <- mcp_repl_runner(
  sandbox = "workspace-write",
  timeout = 30
)
```

The package-managed mcp-repl policy uses OS primitives, disables network
access, and limits writes to the workspace and runtime temp paths. The
REPL remains alive across calls, so an agent can load a package or
construct an analysis once and iterate on it. `runner$reset()` starts a
clean session.

An externally supplied `repl = function(input, timeout_ms) ...` is
different: dsprrr did not launch its server and cannot attest its
isolation policy. Such a runner reports `sandboxed = FALSE` with
unverified enforcement, and a sandbox-required harness rejects it. This
fail-closed rule prevents a custom or test connection from silently
inheriting the trust assigned to a dsprrr-managed mcp-repl process.

For RLM submit and recursive-query messages, dsprrr uses a versioned,
per-invocation nonce-bound, schema-checked text frame capped at 3,000
encoded bytes. That cap keeps ordinary control messages below mcp-repl’s
inline-output threshold. If user code produces enough additional output
to trigger a file preview or interactive pager, dsprrr fails the
iteration (and attempts to reset the pager) instead of accepting a
possibly partial frame. The upstream MCP response currently exposes
compaction only as a plain-text marker, so this detection is
conservative; dsprrr deliberately does not read the disclosed sandbox
file from the host process. Use smaller submissions, suppress large
incidental prints, or use a runner transport with a structured
out-of-band result for larger payloads.

[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
is intentionally rejected while sandboxing is enabled. Its callr
subprocess is useful for trusted code, but it retains the host user’s
permissions and is not a security boundary.

Set `sandbox = FALSE` on a harness only to remove the sandbox action
entirely. Any supplied runner is then ignored; the agent can still
propose program edits but cannot execute code.

## AutoResearch: One Persistent Research Session

Use separate chats for the program under test and the research agent.
That separates task traces, model choice, and cost attribution.

``` r

library(dsprrr)
library(ellmer)

task_chat <- chat_openai(model = "gpt-4.1-mini")
research_chat <- chat_anthropic(model = "claude-sonnet-4-6")

metric <- metric_with_feedback(
  function(prediction, expected) {
    correct <- identical(as.character(prediction), expected$answer)
    list(
      score = as.numeric(correct),
      feedback = if (correct) {
        "Correct."
      } else {
        paste("Expected", expected$answer, "but received", prediction)
      }
    )
  },
  field = "answer"
)

research <- AutoResearch(
  metric = metric,
  max_iterations = 16L,
  patience = 5L,
  target_score = 0.95,
  max_context_examples = 20L,
  max_feedback_examples = 8L,
  seed = 42L
)

compiled <- compile(
  qa_program,
  research,
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = research_chat,
  runner = runner,
  objective = paste(
    "Answer from supplied evidence.",
    "Do not invent facts when the context is insufficient."
  ),
  .cache = FALSE
)
```

The harnesses also accept
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md)
metrics. A trace-aware metric can derive its score and feedback from the
row’s `status`, ordered `events`, and module `metadata`; candidate
selection remains in the trusted R evaluator.

On each turn the agent must choose one typed action:

- `sandbox`: run bounded R analysis against a context containing the
  editable program manifest, visible training rows, and current
  frontier;
- `propose`: branch from a known candidate and submit one or more
  complete instruction or template replacements; or
- `finish`: stop when another evaluation is unlikely to improve the
  result.

The agent owns the research policy, but it cannot write directly into
the module or evaluator. dsprrr re-validates and applies every
candidate.

## Meta-Harness: Fresh Proposers, Trusted Selection

Meta-Harness creates a fresh proposer chat for each outer iteration. The
proposer sees persisted evidence rather than relying on earlier
conversation state. Pass an ellmer `Chat`; dsprrr clones and resets it
automatically before each iteration:

``` r

harness <- MetaHarness(
  metric = metric,
  max_iterations = 8L,
  max_candidates_per_iteration = 4L,
  frontier_size = 8L,
  patience = 3L,
  seed = 42L
)

compiled <- compile(
  qa_program,
  harness,
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = research_chat,
  runner = runner,
  objective = "Improve factual accuracy without increasing abstention errors."
)
```

Meta-Harness rejects non-Chat adapters and non-cloneable Chat objects
because reusing them could carry hidden conversation state across
iterations.

Within an iteration, the proposer may request a few sandbox steps before
it submits its batch. The outer loop then:

1.  materializes each proposal from its declared parent;
2.  rejects invalid or duplicate snapshots;
3.  evaluates every unique candidate that fits the remaining budget;
4.  updates the scored frontier; and
5.  checkpoints the new lineage before starting a fresh proposer.

This is a useful boundary for coding-agent experiments: the agent can
inspect and hypothesize, but the scorer, budget, and accepted state
remain host-owned.

## Joint Pipeline Edits

Candidate paths come from
[`named_parameters()`](https://jameshwade.github.io/dsprrr/reference/module-graph.md)
and remain stable within one program graph:

``` r

names(named_parameters(qa_pipeline, boundaries = "respect"))
#> [1] "$/steps/1" "$/steps/2" "$/steps/3"
```

A single candidate may replace fields at several paths. For example, a
retriever stage can promise a citation-bearing intermediate format while
the answer stage is changed to consume exactly that format. The
candidate ledger stores the complete resulting snapshot, not only the
patch, so fingerprints, resume behavior, and comparisons are
deterministic.

Protected graph boundaries are respected. If you intentionally need to
expose an inner component, change the module graph boundary before
compiling rather than asking the agent to bypass it.

## Budget Every Kind of Work

The shared optimizer ledger accounts for baseline and candidate
evaluations as trials, program calls and tokens during evaluation,
proposer calls and tokens, metric calls, cost, elapsed time, and errors:

``` r

control <- optimizer_control(
  max_trials = 25L,
  max_metric_calls = 200L,
  max_provider_calls = 250L,
  max_total_tokens = 500000L,
  max_cost = 10,
  max_elapsed_seconds = 1800,
  checkpoint_path = "meta-harness-checkpoint.rds"
)

compiled <- compile(
  qa_program,
  harness,
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = research_chat,
  runner = runner,
  control = control
)
```

A finite provider, token, or cost cap fails closed when a custom chat
cannot report verified usage. A stopped run returns its best partial
program and a typed stop reason:

``` r

optimization_result(compiled)$budget
optimization_result(compiled)$stop_reason
optimization_result(compiled)$status
```

## Resume and Audit a Run

Both harnesses support optimizer checkpoints. Resume with the same
program, data, metric, and harness configuration; resource limits may be
raised:

``` r

resumed <- compile(
  qa_program,
  harness,
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = research_chat,
  runner = runner,
  control = optimizer_control(
    max_trials = 40L,
    max_cost = 20,
    checkpoint_path = "meta-harness-checkpoint.rds",
    resume = TRUE
  )
)
```

The candidate ledger is attached to the returned module:

``` r

candidates <- optimization_result(resumed)$trials
candidates[, c(
  "id",
  "parent_id",
  "name",
  "iteration",
  "score",
  "selected",
  "status"
)]

# Complete per-candidate program state and evaluation feedback:
candidates$snapshot[[1]]
candidates$feedback[[1]]

optimization_result(resumed)$extensions$meta_harness$frontier_ids
optimization_result(resumed)$extensions$meta_harness$events
optimization_result(resumed)$stop_reason
```

With `log_dir` in the teleprompter or
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
evaluations are also written through `TrialLog`.

Checkpoint resume restores candidate lineage, evaluation evidence,
budgets, and random-number state. It starts a new chat and mcp-repl
connection rather than trying to serialize hidden provider or
interpreter session state.

## Compose Harnesses with Omni

These are ordinary `Teleprompter` objects, so sequential
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md) runs
can use them as explorers or as the continuation stage. Pass their agent
and runner dependencies through the per-step argument lists:

``` r

omni <- Omni(
  metric = metric,
  explorers = list(
    demos = BootstrapFewShotWithRandomSearch(metric = metric),
    meta = MetaHarness(metric = metric, max_iterations = 4L)
  ),
  continuation = AutoResearch(metric = metric, max_iterations = 6L)
)

compiled <- compile(
  qa_program,
  omni,
  trainset,
  valset = valset,
  .llm = task_chat,
  explorer_compile_args = list(
    meta = list(.agent_llm = research_chat, runner = runner)
  ),
  continuation_compile_args = list(
    .agent_llm = research_chat,
    runner = runner
  )
)
```

Do not use a live chat or runner with Omni’s mirai `parallel = TRUE`
mode. Those stateful runtime objects are intentionally not serialized to
workers.

## Inspiration and Differences

The design credits three open projects:

- Andrej Karpathy’s
  [`autoresearch`](https://github.com/karpathy/autoresearch), for the
  explicit baseline, experiment, score, keep-or-revert loop;
- the [GEPA optimize-anything](https://github.com/gepa-ai/gepa)
  AutoResearch and Meta-Harness engines, for a common trusted evaluation
  boundary and optimizer composition;
- the [Meta-Harness paper](https://arxiv.org/abs/2603.28052), for the
  fresh-proposer outer-loop design; and
- Posit’s [`mcp-repl`](https://github.com/posit-dev/mcp-repl), for
  persistent R execution with OS-level sandboxing.

dsprrr does not launch a particular coding-agent CLI, expose an HTTP
evaluator, or ask an agent to edit arbitrary files. It uses
provider-neutral ellmer chats, typed actions, canonical module
snapshots, the package’s optimizer ledger, and safe program checkpoints.
Benchmark these harnesses on your own task and budget; results reported
by the source projects do not automatically transfer to a different
model, metric, or program.
