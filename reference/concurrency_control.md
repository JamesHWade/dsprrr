# Control how batches run in parallel

`concurrency_control()` creates a policy for the `.concurrency` argument
of [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
and
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md):
which backend runs the rows, how many requests may be active at once,
and what happens on errors and timeouts. Without a policy, rows run one
after another.

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

  Where rows run. `"sequential"` runs them one at a time. `"ellmer"`
  sends them with ellmer's parallel requests. `"mirai"` runs them in a
  pool of background R processes. `"auto"` (the default) is sequential
  when `max_active = 1`; otherwise it uses ellmer when the chat and the
  limits allow, then mirai, then sequential. Only `"auto"` ever picks a
  different backend; an explicit backend that cannot honour the policy
  is an error before any request is made.

- max_active:

  The maximum number of requests active at once: ellmer's `max_active`
  or the size of the mirai pool.

- task_timeout:

  Seconds allowed per row, or `Inf`. A finite value needs the mirai
  backend.

- total_timeout:

  Seconds allowed for the whole batch, or `Inf`. A finite value needs
  the mirai backend. At the deadline, active work is stopped, which can
  add a short cleanup delay.

- max_errors:

  How many failed rows to tolerate before no new rows are started, or
  `Inf`. With `0`, work stops after the first failure. With ellmer, a
  wave of up to `max_active` rows that has already started finishes
  first.

- cancel:

  If `TRUE` (the default), active mirai tasks are stopped when the error
  budget or a timeout is reached. If `FALSE`, no new rows start, but
  rows already running finish. A total timeout always stops active work.

## Value

A policy object of class `dsprrr_concurrency_control`.

## Details

The ellmer and mirai backends do not use dsprrr's response cache (the
metadata reports `cache = "bypass"`); use the sequential backend when
cached responses matter. Parallel backends are available for prediction
modules from
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
and
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
and for some code-running modules; for other modules, a parallel backend
is an error.

## See also

Other execution:
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`predict.Module()`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md),
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md),
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)

## Examples

``` r
control <- concurrency_control(
  backend = "mirai",
  max_active = 2L,
  task_timeout = 30,
  total_timeout = 120,
  max_errors = 1L
)
control
#> <dsprrr_concurrency_control>
#>   Backend: mirai
#>   Maximum active: 2
#>   Task timeout: 30 seconds
#>   Total timeout: 120 seconds
#>   Maximum errors: 1
#>   Cancel active work: TRUE

if (FALSE) { # \dontrun{
classify <- module(signature("text -> sentiment"))
reviews <- data.frame(text = c("Great!", "Broken on arrival", "Fine"))
run_dataset(
  classify,
  reviews,
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .concurrency = concurrency_control(max_active = 3L)
)
} # }
```
