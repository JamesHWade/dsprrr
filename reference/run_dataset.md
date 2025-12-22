# Execute Module on Dataset

Execute a module on a dataset (tibble/data.frame) with optimized batch
processing.

## Usage

``` r
run_dataset(module, ...)

# S3 method for class 'Module'
run_dataset(
  module,
  dataset,
  .llm = NULL,
  .verbose = FALSE,
  .parallel = FALSE,
  .progress = TRUE,
  .return_format = "simple",
  ...
)
```

## Arguments

- module:

  A DSPrrr module (e.g., created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md))

- ...:

  Additional arguments including:

  dataset

  :   A tibble or data frame with columns matching the module's inputs

  .llm

  :   An ellmer chat object for LLM interaction (optional)

  .verbose

  :   Logical indicating whether to print debug information

  .parallel

  :   Logical indicating whether to process in parallel (default TRUE)

  .progress

  :   Logical indicating whether to show progress bar (default TRUE)

  .return_format

  :   Character, either "simple" or "structured" (default "simple")

- dataset:

  A data frame or tibble containing input columns

- .llm:

  Optional ellmer Chat object for LLM calls

- .verbose:

  Logical whether to print verbose output

- .parallel:

  Logical whether to enable parallel processing

- .progress:

  Logical whether to show progress bar

- .return_format:

  Character either "simple" or "structured"

## Value

A tibble with the input columns plus a result column containing outputs

## Examples

``` r
if (FALSE) { # \dontrun{
# Process a dataset
data <- tibble::tibble(
  text = c("I love this!", "This is bad", "Okay product")
)

llm <- ellmer::chat_openai()
results <- signature("text -> sentiment") |>
  module(type = "predict") |>
  run_dataset(data, .llm = llm)
} # }
```
