# Budgets and settings for an optimizer run

`optimizer_control()` collects the limits and options for one optimizer
run: error, trial, call, token, cost and time budgets, concurrency,
trial logging and checkpoints. Pass it to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
as `control`. It replaces the control that the optimizer would otherwise
build from its own `max_errors`, `num_threads` and `log_dir` settings.

[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
and
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)
use `control`; the other optimizers ignore it.

## Usage

``` r
optimizer_control(
  seed = NULL,
  max_trials = NULL,
  max_errors = 5L,
  max_metric_calls = NULL,
  max_provider_calls = NULL,
  max_input_tokens = NULL,
  max_output_tokens = NULL,
  max_total_tokens = NULL,
  max_cost = NULL,
  max_elapsed_seconds = NULL,
  num_threads = 1L,
  progress = NA,
  log_dir = NULL,
  checkpoint_path = NULL,
  resume = FALSE,
  checkpoint_registry = list(),
  verbose = FALSE
)
```

## Arguments

- seed:

  Random seed, or `NULL` (the default). Currently only
  [`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md)
  reads it, in place of its own `seed`; the other optimizers use their
  own `seed` argument.

- max_trials:

  Maximum number of trials (candidate evaluations), or `NULL` (the
  default) for no limit.

- max_errors:

  Non-negative integer (default `5L`). The run stops once this many
  evaluations have failed in a row; each success resets the count, while
  the total number of errors is still reported. With `0L`, the first
  failure stops the run. Outcomes of an evaluation that had already
  started are all counted.

- max_metric_calls:

  Maximum number of metric calls, or `NULL` for no limit.

- max_provider_calls:

  Maximum number of verified provider calls, or `NULL` for no limit.

- max_input_tokens, max_output_tokens, max_total_tokens:

  Maximum verified input, output, or input plus output tokens, or `NULL`
  for no limit.

- max_cost:

  Maximum known provider cost in US dollars, or `NULL` for no limit.

- max_elapsed_seconds:

  Maximum active run time in seconds, or `NULL` for no limit. Time
  between a checkpoint and its resume is not counted.

- num_threads:

  Integer number of rows evaluated at the same time (default `1L`).

- progress:

  Whether to show progress bars. `NA` (the default) means
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- log_dir:

  Directory for a
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)
  of the run, or `NULL` (the default).
  [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
  ignores it and uses its own `log_dir`.

- checkpoint_path:

  Optional file for optimizer checkpoints.

- resume:

  Whether to resume from `checkpoint_path` (default `FALSE`).

- checkpoint_registry:

  Named runtime registry used to save and restore the programs stored in
  checkpoints; see
  [`program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).

- verbose:

  Currently unused.

## Value

An `OptimizerControl` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
as `control`.

## Details

When a budget stops a run, the optimizer returns the best program found
so far and marks its
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md)
as `"partial"`.

Finite metric, provider, token, cost and elapsed-time limits make the
optimizer evaluate one row at a time, so a run overshoots a limit by at
most one evaluation row that had already started (or one direct provider
request made by the optimizer itself). When a cap is finite and a
provider does not report its usage, tokens or cost, the run stops rather
than guess.

[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
and
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md)
can resume from a checkpoint.
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md) and
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md)
respect the budgets and return the best partial program, but reject
`resume = TRUE`.

## See also

Other optimizer building blocks:
[`TrialLog`](https://jameshwade.github.io/dsprrr/reference/TrialLog.md),
[`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md),
[`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md),
[`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md),
[`load_trial_log()`](https://jameshwade.github.io/dsprrr/reference/load_trial_log.md),
[`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md),
[`sample_dataset()`](https://jameshwade.github.io/dsprrr/reference/sample_dataset.md),
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md),
[`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)

## Examples

``` r
optimizer_control()
#> <dsprrr::OptimizerControl>
#>  @ seed               : NULL
#>  @ max_trials         : NULL
#>  @ max_errors         : int 5
#>  @ max_metric_calls   : NULL
#>  @ max_provider_calls : NULL
#>  @ max_input_tokens   : NULL
#>  @ max_output_tokens  : NULL
#>  @ max_total_tokens   : NULL
#>  @ max_cost           : NULL
#>  @ max_elapsed_seconds: NULL
#>  @ num_threads        : int 1
#>  @ progress           : logi FALSE
#>  @ log_dir            : NULL
#>  @ checkpoint_path    : NULL
#>  @ resume             : logi FALSE
#>  @ checkpoint_registry: list()
#>  @ verbose            : logi FALSE

# Stop after 50 trials or US$2 of known cost, whichever comes first
ctrl <- optimizer_control(max_trials = 50L, max_cost = 2)

if (FALSE) { # \dontrun{
compiled <- compile(
  program,
  MIPROv2(metric = metric_exact_match(field = "answer")),
  trainset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  control = ctrl
)
} # }
```
