# Create a Refine Wrapper Module

Factory function to create a Refine wrapper around any module. Extends
BestOfN with iterative refinement using feedback.

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

  A Module object to wrap

- N:

  Maximum number of attempts (default 3)

- reward_fn:

  Reward function with signature `function(prediction, inputs)`,
  returning a score between 0 and 1

- threshold:

  Score threshold for early stopping (default 1.0)

- fail_count:

  Maximum consecutive failures before erroring (default N)

- feedback_template:

  Template for generating feedback. Uses glue syntax with available
  variables: `\{score\}`, `\{prediction\}`, and input field names.

- feedback_field:

  Name of the input field to inject feedback into

- ...:

  Additional arguments passed to the module constructor

## Value

A RefineModule object

## Examples

``` r
# Create a QA module - include feedback in signature for refinement
# RefineModule will automatically inject feedback on subsequent attempts
qa <- module(signature("question, feedback -> answer"))

# Wrap with refinement
one_word_reward <- function(pred, inputs) {
  words <- strsplit(as.character(pred$answer), "\\s+")[[1]]
  if (length(words) == 1) 1.0 else 0.0
}

refined <- refine(
  qa,
  N = 3,
  reward_fn = one_word_reward,
  feedback_template = "Score: {score}. Your answer '{prediction}' was too long. Give a single word."
)

# When running, only provide 'question' - feedback is auto-injected:
# result <- run(refined, question = "What is the capital of France?", .llm = llm)
```
