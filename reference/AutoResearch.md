# AutoResearch: let an agent run optimization experiments

`AutoResearch()` hands the search to a research agent. In a loop, the
agent forms a hypothesis, may test ideas in sandboxed R code, proposes
an edit to the program, and sees how the edit scores; it keeps or
reverts edits and decides when to stop. dsprrr keeps control of budgets,
evaluation, checkpoints and the choice of the final program.

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

## Value

An `AutoResearch` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

`AutoResearch()` is inspired by Andrej Karpathy's
[`autoresearch`](https://github.com/karpathy/autoresearch) and the
AutoResearch engine in the [GEPA optimize-anything
project](https://github.com/gepa-ai/gepa). It is an R implementation for
dsprrr programs, not a port of either command-line tool.

A candidate is a validated snapshot of every optimizable module in the
program, so one experiment can change instructions and templates across
several pipeline steps at once. The agent can branch from any earlier
candidate and sees per-example feedback. Candidates are always evaluated
in the host R process. Only the agent's exploratory R code goes to
`runner`, which must advertise an operating-system sandbox, such as
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
unless `sandbox = FALSE`.

## Compilation arguments

Besides the standard
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
arguments, this optimizer accepts `.agent_llm` (the agent's Chat;
defaults to `.llm`), `runner` (for sandboxed analysis), `control` (an
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md)
object for budgets and checkpoints) and `objective` (a text description
of what to optimize for). Other named arguments, such as `.cache`, are
passed to candidate evaluation.

## See also

Other teleprompters:
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
research <- AutoResearch(
  metric = metric_exact_match(field = "answer"),
  max_iterations = 12L
)
research
#> 
#> ── AutoResearch Teleprompter 
#> Max experiments: 12
#> Patience: 6
#> OS sandbox required: TRUE

if (FALSE) { # \dontrun{
compiled <- compile(
  program,
  research,
  trainset,
  valset = valset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .agent_llm = ellmer::chat_anthropic(model = "claude-sonnet-4-5"),
  runner = mcp_repl_runner(),
  control = optimizer_control(
    max_trials = 13L,
    max_cost = 5,
    checkpoint_path = file.path(tempdir(), "autoresearch.rds")
  )
)
} # }
```
