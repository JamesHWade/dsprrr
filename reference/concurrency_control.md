# Control Batch Concurrency

Create a validated execution policy for batch calls made by
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) and
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).
The policy controls the backend, the exact maximum number of active
requests, error budgets, timeouts, and cancellation behavior.

`backend = "auto"` is the only mode that may select a different backend.
An explicitly requested backend either runs with the requested contract
or fails before provider work begins.

## Usage

``` r
concurrency_control(
  backend = c("auto", "sequential", "ellmer", "mirai"),
  max_active = 1L,
  task_timeout = Inf,
  total_timeout = Inf,
  max_errors = Inf,
  cancel = TRUE
)
```

## Arguments

- backend:

  Execution backend. `"auto"` uses sequential execution when
  `max_active = 1`, otherwise preferring ellmer when the configured Chat
  and requested limits are compatible, then mirai, then sequential
  execution. Explicit `"ellmer"`, `"mirai"`, and `"sequential"` requests
  never fall back.

- max_active:

  Positive integer giving the exact maximum number of active requests.
  It maps to ellmer's `max_active` argument and the size of a
  dsprrr-owned mirai pool.

- task_timeout:

  Per-task timeout in seconds, or `Inf` for no timeout. Finite task
  timeouts currently require the mirai backend.

- total_timeout:

  Total batch execution timeout in seconds, or `Inf` for no timeout.
  Finite total timeouts currently require the mirai backend. Shutdown is
  initiated at the deadline and verification is bounded; the native
  backend reset can add brief cleanup latency before
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
  returns.

- max_errors:

  Non-negative integer error budget, or `Inf`. Zero permits work to
  begin and stops new work after the first failure. With ellmer, an
  already-started wave of at most `max_active` rows completes before the
  budget is observed.

- cancel:

  Logical. When `TRUE`, active mirai tasks are stopped when an error or
  timeout limit is reached. When `FALSE`, no new work is scheduled, but
  already-started tasks are drained before return. A total timeout
  always stops active work so no task continues after
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
  returns.

## Value

A `dsprrr_concurrency_control` object for `.concurrency` in
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
or
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md).

## Details

Native ellmer and mirai batch execution currently bypass dsprrr's
response cache; structured metadata reports this as `cache = "bypass"`.
Choose `backend = "sequential"` when response-cache reuse is required.

## Examples

``` r
control <- concurrency_control(
  backend = "mirai",
  max_active = 2L,
  task_timeout = 30,
  total_timeout = 120,
  max_errors = 1L
)
```
