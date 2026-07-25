# Meta-Harness Teleprompter

Runs an outer-loop optimization harness that starts a fresh proposer
session on every iteration. Each proposer sees the current frontier,
candidate lineage, evaluation traces, and training context, then
proposes a batch of joint program edits. The R outer loop validates and
evaluates every unique candidate and alone decides what enters the
frontier.

## Usage

``` r
MetaHarness(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  max_iterations = 20L,
  patience = 6L,
  target_score = NULL,
  max_context_examples = 20L,
  max_feedback_examples = 8L,
  max_agent_steps = 4L,
  sandbox = TRUE,
  seed = NULL,
  log_dir = NULL,
  verbose = TRUE,
  max_candidates_per_iteration = 4L,
  frontier_size = 8L
)

# S3 method for class 'MetaHarness'
print(x, ...)
```

## Arguments

- metric:

  Metric used to evaluate candidates.

- metric_threshold:

  Optional success threshold inherited from
  [Teleprompter](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md).

- max_errors:

  Consecutive optimizer error budget.

- max_iterations:

  Maximum evaluated experiments after the baseline.

- patience:

  Stop after this many evaluated experiments without improvement.

- target_score:

  Optional score at which optimization stops.

- max_context_examples:

  Maximum training examples exposed to the research agent.

- max_feedback_examples:

  Maximum failed examples returned after each evaluation.

- max_agent_steps:

  Maximum consecutive sandbox or invalid actions before the harness
  requires evaluation progress.

- sandbox:

  Whether an OS-sandboxed runner is required. Defaults to TRUE.

- seed:

  Optional random seed.

- log_dir:

  Optional directory for a durable
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md).

- verbose:

  Whether to report progress.

- max_candidates_per_iteration:

  Maximum candidates evaluated from one proposer batch.

- frontier_size:

  Maximum scored candidates summarized to each proposer.

- x:

  A MetaHarness object.

- ...:

  Additional arguments.

## Value

A `MetaHarness` teleprompter.

## Details

`MetaHarness()` is inspired by the Meta-Harness engine in the [GEPA
optimize-anything project](https://github.com/gepa-ai/gepa) and the
associated [Meta-Harness paper](https://arxiv.org/abs/2603.28052). It
preserves the important separation between an untrusted coding proposer
and a trusted evaluator while adapting the candidate representation to
dsprrr module graphs.

A proposer may request one or more R analyses before submitting its
batch. Those analyses execute only through `runner`; with the default
`sandbox = TRUE`, the runner must advertise an OS sandbox such as
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md).
Proposer sessions are fresh by design, so each iteration must reason
from the persisted frontier rather than hidden chat history. An ellmer
`Chat` is cloned and reset automatically. A custom proposer must either
provide `chat_structured()` and `clone()`, or be supplied as a
zero-argument `.agent_llm` factory that returns a fresh compatible
proposer on every call. Non-cloneable proposer objects are rejected.

## Compilation arguments

In addition to the standard
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
arguments, this teleprompter accepts `.agent_llm` for the proposer,
`runner` for sandboxed analysis, `control` for optimizer budgets and
checkpointing, and `objective` for multi-objective selection. Named
arguments in `...`, such as `.cache`, are forwarded to candidate
evaluation.

## Examples

``` r
if (FALSE) { # \dontrun{
harness <- MetaHarness(
  metric = metric_exact_match(field = "answer"),
  max_iterations = 8L,
  max_candidates_per_iteration = 4L
)
compiled <- compile(
  harness,
  program,
  trainset,
  valset = valset,
  .agent_llm = proposer_chat,
  runner = mcp_repl_runner()
)
} # }
```
