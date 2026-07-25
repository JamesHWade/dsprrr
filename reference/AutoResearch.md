# AutoResearch Teleprompter

Runs a persistent research agent that owns an explicit
hypothesize-sandbox-evaluate-keep-or-revert loop. The agent can branch
from any prior candidate, inspect structured per-example feedback,
request sandboxed R experiments, and decide when to finish. dsprrr
retains control of budgets, evaluation, checkpointing, and final
best-candidate selection.

## Usage

``` r
AutoResearch(
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
  verbose = TRUE
)

# S3 method for class 'AutoResearch'
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

- x:

  An AutoResearch object.

- ...:

  Additional arguments.

## Value

An `AutoResearch` teleprompter.

## Details

`AutoResearch()` is inspired by Andrej Karpathy's
[`autoresearch`](https://github.com/karpathy/autoresearch) and the
AutoResearch engine in the [GEPA optimize-anything
project](https://github.com/gepa-ai/gepa). This is an R-native
implementation for dsprrr `Module` graphs, not a port of either
command-line harness.

Candidates are complete, validated snapshots of every optimizable leaf
module. A single experiment can therefore change instructions and
templates across multiple pipeline components jointly. Candidate
evaluation always runs in the host process through dsprrr's optimizer
ledger. Only exploratory R code is sent to `runner`; with the default
`sandbox = TRUE`, `runner` must advertise an operating-system sandbox
such as
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md).

## Compilation arguments

In addition to the standard
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
arguments, this teleprompter accepts `.agent_llm` for the research
agent, `runner` for sandboxed analysis, `control` for optimizer budgets
and checkpointing, and `objective` for multi-objective selection. Named
arguments in `...`, such as `.cache`, are forwarded to candidate
evaluation.

## Examples

``` r
if (FALSE) { # \dontrun{
research <- AutoResearch(
  metric = metric_exact_match(field = "answer"),
  max_iterations = 12L
)
compiled <- compile(
  research,
  program,
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = research_chat,
  runner = mcp_repl_runner(),
  control = optimizer_control(
    max_trials = 13L,
    max_cost = 5,
    checkpoint_path = "autoresearch.rds"
  )
)
} # }
```
