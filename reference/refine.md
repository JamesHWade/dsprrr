# Retry a module with feedback until it scores well

`refine()` works like
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
but after an attempt scores below `threshold` it passes feedback about
that attempt into the next one. The feedback is the `feedback_template`
filled in with the attempt's score, its prediction and the inputs; it
says only what the template says.

## Usage

``` r
refine(
  module,
  N = 3L,
  reward_fn = NULL,
  threshold = 1,
  fail_count = NULL,
  feedback_template = NULL,
  feedback_field = "feedback",
  ...
)
```

## Arguments

- module:

  The module to wrap.

- N:

  Maximum number of attempts.

- reward_fn:

  A function called as `reward_fn(prediction, inputs)`, returning a
  score, usually between 0 and 1. The default gives 1 to every
  prediction, so pass a real reward function.

- threshold:

  Score at which to stop early. Default 1.

- fail_count:

  Number of consecutive failed attempts after which to give up with an
  error. Defaults to `N`.

- feedback_template:

  A glue template for the feedback. It can use `{score}` (rounded to 3
  digits), `{prediction}` (the output fields written as `field: value`,
  separated by `; `) and any input field. The default is "Previous
  attempt scored {score}. The answer was: {prediction}. Please try again
  with improvements."

- feedback_field:

  Name of the input that carries the feedback.

- ...:

  Passed to the wrapper: `chat` (an ellmer Chat, which defaults to the
  wrapped module's chat) or `config`.

## Value

A module (an R6 object of class `RefineModule`) with the same signature
as `module`.

## Details

If the wrapped module's signature declares `feedback_field` as an input,
the first attempt receives "No feedback yet." and later attempts receive
the filled-in template; callers never pass it to
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md). If the
signature does not declare it, the feedback is still passed to the
wrapped module: a prediction module without a custom template then adds
it to the prompt as an extra `feedback:` line on retries.

The returned module has `get_attempts()` and `get_feedback_history()`
methods for the last run (or all runs, with `all = TRUE`).

## See also

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
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
one_word <- function(prediction, inputs) {
  length(strsplit(prediction$answer, " ")[[1]]) == 1
}

# An offline stand-in for a model that shortens its answer once it gets
# feedback
drafter <- module_fn(
  "question, feedback -> answer",
  function(question, feedback) {
    if (feedback == "No feedback yet.") "The capital of France is Paris." else "Paris"
  }
)
refined <- refine(
  drafter,
  N = 3L,
  reward_fn = one_word,
  feedback_template = "Your answer '{prediction}' scored {score}. Reply with one word."
)
run(refined, question = "What is the capital of France?")
#> $answer
#> [1] "Paris"
#> 
refined$get_feedback_history()
#> [1] "Your answer 'answer: The capital of France is Paris.' scored 0. Reply with one word."

if (FALSE) { # \dontrun{
qa <- module(signature("question, feedback -> answer"))
refined <- refine(
  qa,
  N = 3L,
  reward_fn = one_word,
  feedback_template = "Your answer '{prediction}' was too long. Give a single word."
)
run(
  refined,
  question = "What is the capital of France?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
