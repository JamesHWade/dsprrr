# Execute Module on Data

Execute a module on a data frame/tibble with optimized batch processing.

## Usage

``` r
run_dataset(module, ...)

# S3 method for class 'Module'
run_dataset(
  module,
  data,
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

  Additional arguments passed to
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

- data:

  A tibble or data frame with columns matching the module's inputs.

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
# Process data
df <- tibble::tibble(
  text = c("I love this!", "This is bad", "Okay product")
)

llm <- ellmer::chat_openai()
results <- signature("text -> sentiment") |>
  module(type = "predict") |>
  run_dataset(df, .llm = llm)
} # }
```
