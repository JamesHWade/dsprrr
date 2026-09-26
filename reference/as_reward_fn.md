# Turn a metric into a reward function

`as_reward_fn()` adapts a metric, called as
`metric(prediction, expected)`, into a reward function for
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md)
and
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
called as `reward(prediction, inputs)`. The expected value is read from
the inputs under `expected_field`.

## Usage

``` r
as_reward_fn(metric, expected_field = "expected", prediction_field = NULL)
```

## Arguments

- metric:

  A metric, such as
  [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md).
  The metric receives `prediction_field` of the prediction and the bare
  expected value, so do not give it a `field` of its own.

- expected_field:

  Name of the input that holds the expected value.

- prediction_field:

  Name of the prediction field to compare. With `NULL`, the whole
  prediction is passed to the metric.

## Value

A function `function(prediction, inputs)` returning a numeric score.

## Details

Because the expected value travels with the inputs of
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md), the
wrapped module receives it too, and
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) warns
that it is not declared in the signature. A module without a custom
`template` writes every input into its prompt, so it would show the
expected answer to the model. Give the wrapped module a `template` that
leaves that field out, as in the example. If the input is missing, the
reward is 0 with a warning.

## See also

Other composition:
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
[`module-graph`](https://jameshwade.github.io/dsprrr/reference/module-graph.md),
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md),
[`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md),
[`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md),
[`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md),
[`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
reward <- as_reward_fn(
  metric_exact_match(ignore_case = TRUE),
  expected_field = "expected",
  prediction_field = "answer"
)
reward(list(answer = "paris"), list(question = "Capital?", expected = "Paris"))
#> [1] 1
reward(list(answer = "Lyon"), list(question = "Capital?", expected = "Paris"))
#> [1] 0

if (FALSE) { # \dontrun{
qa <- module(
  signature("question -> answer"),
  template = "Question: {question}\nReply with a single word."
)
checked <- best_of_n(qa, N = 3L, reward_fn = reward)
run(
  checked,
  question = "What is the capital of France?",
  expected = "Paris",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
