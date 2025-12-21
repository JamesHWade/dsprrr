# Compile a DSPrrr Program

Main user-facing function to compile/optimize a DSPrrr module using a
teleprompter optimization strategy.

## Usage

``` r
compile_module(
  program,
  teleprompter,
  trainset,
  valset = NULL,
  .llm = NULL,
  ...
)
```

## Arguments

- program:

  A DSPrrr module to optimize (e.g., from [`module()`](module.md))

- teleprompter:

  A Teleprompter object defining the optimization strategy

- trainset:

  Training data as a data frame

- valset:

  Optional validation set for evaluation

- .llm:

  Optional ellmer chat object to reuse during compilation

- ...:

  Additional arguments passed to the teleprompter

## Value

An optimized module with updated demonstrations and/or instructions

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a simple module
classifier <- signature("text -> sentiment") |>
  module(type = "predict")

# Prepare training data
trainset <- data.frame(
  text = c("I love it!", "Terrible experience"),
  sentiment = c("positive", "negative")
)

# Compile with LabeledFewShot
tp <- LabeledFewShot(k = 2)
optimized <- compile_module(classifier, tp, trainset)

# Compile with GridSearch
variants <- data.frame(
  id = c("terse", "detailed"),
  instructions_suffix = c(
    "Be concise.",
    "Provide detailed reasoning."
  )
)
tp <- GridSearchTeleprompter(
  variants = variants,
  metric = metric_exact_match(field = "sentiment")
)
optimized <- compile_module(classifier, tp, trainset)
} # }
```
