# GridSearchTeleprompter

A teleprompter that performs grid search over different instruction and
template variants to find the best performing configuration.

## Arguments

- metric:

  A metric function for evaluating predictions, such as
  [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md).
  Required when compiling.

- metric_threshold:

  Minimum score required to be considered successful. If NULL, uses the
  metric's default threshold.

- max_errors:

  Maximum number of errors allowed during optimization. Default is 5.

- variants:

  A data frame containing variant configurations to test. Must have an
  'id' column. Other columns define parameter values. Default is a
  tibble with one row containing NA values for instructions and
  template.

- k:

  Number of examples to include in few-shot prompts. Default is 2.

- eval_sample_size:

  Number of examples to use for evaluation during grid search. Default
  is 50.

- verbose:

  Whether to print progress messages. Default is TRUE.

## Examples

``` r
# Search over two instruction variants
variants <- data.frame(
  id = c("terse", "detailed"),
  instructions = c("Be concise.", "Explain your reasoning step by step.")
)
tp <- GridSearchTeleprompter(
  variants = variants,
  metric = metric_exact_match(field = "sentiment")
)

if (FALSE) { # \dontrun{
# Compile picks the variant that scores best on the training set
classifier <- module(signature("text -> sentiment"))
trainset <- data.frame(
  text = c("I love it!", "Terrible experience"),
  sentiment = c("positive", "negative")
)
optimized <- compile(classifier, tp, trainset)
} # }
```
