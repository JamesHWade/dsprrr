# Validate a module's outputs and retry on failure

`with_assertions()` wraps a module so that every output is checked
against assertions. When a hard assertion fails, the module runs again
with feedback that lists the failed checks, up to `max_retries` more
times. Soft assertions (suggestions) only give a warning.

## Usage

``` r
with_assertions(
  module,
  assertions,
  max_retries = 3L,
  on_failure = c("error", "warn"),
  feedback_template = NULL,
  ...
)
```

## Arguments

- module:

  The module to wrap.

- assertions:

  A list of assertions made with
  [`assert_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md),
  [`suggest_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)
  or the `assert_*()` helpers, or an
  [`assertion_set()`](https://jameshwade.github.io/dsprrr/reference/assertions.md).

- max_retries:

  Number of retries after the first attempt.

- on_failure:

  What happens when hard assertions still fail after the last retry:
  `"error"` (the default) raises an error; `"warn"` gives a warning and
  returns the attempt with the fewest failed hard assertions.

- feedback_template:

  A glue template for the retry feedback, where `{failures}` is the list
  of failed assertion messages, one per line. The default says the
  previous response did not satisfy the requirements, lists them, and
  asks for a new response that satisfies all of them.

- ...:

  Passed to the wrapper: `chat` (an ellmer Chat, which defaults to the
  wrapped module's chat) or `config`.

## Value

A module (an R6 object of class `AssertModule`) with the same signature
as `module`.

## Details

On a retry, the feedback is passed to the wrapped module as an extra
input named `assertion_feedback`. A prediction module without a custom
template adds it to the prompt as an `assertion_feedback:` line. A
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
function must accept `assertion_feedback` (or `...`) to be retried. Each
attempt uses its own partition of the response cache, so retries get
fresh responses, and each retry is another model call.

The returned module's `get_attempts()` method lists the attempts of the
last run with their numbers of failed hard and soft assertions (or those
of all runs, with `all = TRUE`).

## See also

Other assertions:
[`assert_contains()`](https://jameshwade.github.io/dsprrr/reference/assert_contains.md),
[`assert_custom()`](https://jameshwade.github.io/dsprrr/reference/assert_custom.md),
[`assert_length()`](https://jameshwade.github.io/dsprrr/reference/assert_length.md),
[`assert_matches()`](https://jameshwade.github.io/dsprrr/reference/assert_matches.md),
[`assert_not_contains()`](https://jameshwade.github.io/dsprrr/reference/assert_not_contains.md),
[`assert_not_empty()`](https://jameshwade.github.io/dsprrr/reference/assert_not_empty.md),
[`assert_not_matches()`](https://jameshwade.github.io/dsprrr/reference/assert_not_matches.md),
[`assert_one_of()`](https://jameshwade.github.io/dsprrr/reference/assert_one_of.md),
[`assert_range()`](https://jameshwade.github.io/dsprrr/reference/assert_range.md),
[`assertions`](https://jameshwade.github.io/dsprrr/reference/assertions.md)

Other composition:
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
[`module-graph`](https://jameshwade.github.io/dsprrr/reference/module-graph.md),
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md),
[`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md),
[`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md),
[`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md),
[`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md)

## Examples

``` r
qa <- module(signature("question -> answer"))
checked <- with_assertions(
  qa,
  assertions = list(
    assert_length("answer", max = 100),
    assert_matches("answer", "^[A-Z]", "Start with a capital letter"),
    suggest_output(~ !grepl("!", .x$answer), "Avoid exclamation marks")
  ),
  max_retries = 2L
)
checked
#> 
#> ── AssertModule ──
#> 
#> Wrapped module: <PredictModule>
#> Hard assertions: 2
#> Soft suggestions: 1
#> Max retries: 2
#> On failure: error

if (FALSE) { # \dontrun{
run(
  checked,
  question = "What is the capital of France?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
checked$get_attempts()
} # }
```
