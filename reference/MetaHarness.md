# Meta-Harness: fresh proposer sessions over a candidate frontier

`MetaHarness()` runs an outer optimization loop that starts a fresh
proposer session on every iteration. Each proposer sees the current best
candidates (the frontier), their lineage, evaluation traces and training
context, then proposes a batch of program edits. The R loop validates
and evaluates every unique candidate and alone decides what enters the
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
```

## Arguments

- metric:

  A metric function (required) used to evaluate candidates.

- metric_threshold:

  Accepted for consistency with the other optimizers (see
  [`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md));
  not used.

- max_errors:

  Integer; stop after this many consecutive failed evaluations when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  gets no `control` (default `5L`).

- max_iterations:

  Integer maximum number of evaluated experiments after the baseline
  (default `20L`).

- patience:

  Integer; stop after this many evaluated experiments without
  improvement (default `6L`).

- target_score:

  Optional score at which the search stops.

- max_context_examples:

  Integer maximum number of training rows shown to the agent (default
  `20L`).

- max_feedback_examples:

  Integer maximum number of failed rows returned after each evaluation
  (default `8L`).

- max_agent_steps:

  Integer maximum number of consecutive sandbox or invalid actions
  before the agent must submit a candidate (default `4L`).

- sandbox:

  If `TRUE` (the default),
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  requires a `runner` that advertises an OS sandbox. If `FALSE`, the
  agent cannot run code and `runner` is ignored.

- seed:

  Optional whole-number random seed.

- log_dir:

  Directory for a durable
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md),
  or `NULL` (the default).

- verbose:

  Whether to report progress (default `TRUE`).

- max_candidates_per_iteration:

  Integer maximum number of candidates evaluated from one proposer batch
  (default `4L`).

- frontier_size:

  Integer maximum number of scored candidates summarized for each
  proposer (default `8L`).

## Value

A `MetaHarness` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

`MetaHarness()` is inspired by the Meta-Harness engine in the [GEPA
optimize-anything project](https://github.com/gepa-ai/gepa) and the
[Meta-Harness paper](https://arxiv.org/abs/2603.28052). It keeps the
separation between an untrusted proposer and a trusted evaluator,
adapted to dsprrr programs.

A proposer may request R analyses before submitting its batch. Those run
only through `runner`, which must advertise an operating-system sandbox,
such as
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
unless `sandbox = FALSE`. The proposer must be an ellmer `Chat`; it is
cloned and reset before each iteration, so each iteration reasons from
the saved frontier rather than from chat history.

## Compilation arguments

Besides the standard
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
arguments, this optimizer accepts `.agent_llm` (the proposer Chat;
defaults to `.llm`), `runner` (for sandboxed analysis), `control` (an
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md)
object for budgets and checkpoints) and `objective` (a text description
of what to optimize for). Other named arguments, such as `.cache`, are
passed to candidate evaluation.

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
harness <- MetaHarness(
  metric = metric_exact_match(field = "answer"),
  max_iterations = 8L,
  max_candidates_per_iteration = 4L
)
harness
#> 
#> ── Meta-Harness Teleprompter 
#> Max iterations: 8
#> Candidates per iteration: 4
#> Frontier size: 8
#> OS sandbox required: TRUE

if (FALSE) { # \dontrun{
compiled <- compile(
  program,
  harness,
  trainset,
  valset = valset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .agent_llm = ellmer::chat_anthropic(model = "claude-sonnet-4-5"),
  runner = mcp_repl_runner()
)
} # }
```
