# Check a module and its data before a run

`validate_workflow()` runs quick checks before an expensive batch run or
pipeline step: that `module` is a dsprrr module, how many inputs its
signature has, that `data` has a column for every input, and that
`board` is a pins board. It prints one line per check and makes no model
calls.

## Usage

``` r
validate_workflow(module, data = NULL, board = NULL)
```

## Arguments

- module:

  The module to check.

- data:

  Optional data frame to check against the signature's inputs.

- board:

  Optional pins board. Only its class is checked; the board is not
  accessed.

## Value

Invisibly, a list with `valid` (`FALSE` when the module, data or board
check fails) and `checks` (one list per check with `passed` and
`message`).

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
[`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md),
[`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md),
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md),
[`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md),
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md),
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md),
[`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md),
[`register_dsprrr_engine()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_engine.md),
[`summarize_traces_df()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces_df.md),
[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
[`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
classifier <- module(signature("text -> sentiment"))
reviews <- data.frame(text = c("Great!", "Awful."))
validate_workflow(classifier, data = reviews)
#> 
#> ── Workflow Validation ──
#> 
#> ✔ module: Module type: PredictModule
#> ✔ signature: 1 input(s) defined
#> ✔ data: 2 rows, 1 required columns present
#> ✔ Workflow validation passed

# A missing input column fails the check
result <- validate_workflow(classifier, data = data.frame(review = "Great!"))
#> 
#> ── Workflow Validation ──
#> 
#> ✔ module: Module type: PredictModule
#> ✔ signature: 1 input(s) defined
#> ✖ data: Missing columns: text
#> ✖ Workflow validation failed
result$valid
#> [1] FALSE
```
