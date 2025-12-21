# Execute Module on Dataset

Execute a module on a dataset (tibble/data.frame) with optimized batch
processing.

## Usage

``` r
run_dataset(module, ...)
```

## Arguments

- module:

  A DSPrrr module (e.g., created with [`module()`](module.md))

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
