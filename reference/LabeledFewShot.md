# LabeledFewShot Teleprompter

A simple teleprompter that adds labeled examples from the training set
as demonstrations to the module. This is the simplest form of few-shot
learning.

## Usage

``` r
LabeledFewShot(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  k = 4L,
  sample = TRUE,
  seed = 123L
)
```

## Arguments

- metric:

  A metric function for evaluating predictions. If NULL, uses
  exact_match() by default.

- metric_threshold:

  Minimum score required to be considered successful. If NULL, uses the
  metric's default threshold.

- max_errors:

  Maximum number of errors allowed during optimization. Default is 5.

- k:

  Number of examples to include in few-shot prompts. Default is 4.

- sample:

  Whether to randomly sample examples. Default is TRUE.

- seed:

  Random seed for reproducibility. Default is 123.

## Examples

``` r
# A teleprompter that adds 2 labeled training examples as demonstrations
tp <- LabeledFewShot(k = 2L, seed = 42L)
tp@k
#> [1] 2

if (FALSE) { # \dontrun{
# Compile a module with few-shot demos drawn from the training set
classifier <- module(signature("text -> sentiment"), type = "predict")
trainset <- dsp_trainset(
  text = c("I love it!", "Terrible experience", "It's okay"),
  sentiment = c("positive", "negative", "neutral")
)
optimized <- compile(tp, classifier, trainset)
} # }
```
