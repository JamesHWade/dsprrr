# Execute Module on Data

Execute a module on a data frame/tibble with optimized batch processing.
Zero-row data frames return a zero-row tibble with the same result
columns as a non-empty call, without resolving a Chat or changing
runtime state.

## Usage

``` r
run_dataset(module, ...)

# S3 method for class 'Module'
run_dataset(
  module,
  data,
  .llm = NULL,
  .verbose = FALSE,
  .concurrency = NULL,
  .progress = TRUE,
  .return_format = "simple",
  ...,
  .trace_context = list()
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

- .concurrency:

  Optional batch policy created by
  [`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md).
  Omission uses sequential execution.

- .progress:

  Logical whether to show progress bar

- .return_format:

  Character either "simple" or "structured"

- .trace_context:

  A named, JSON-compatible list copied into row metadata and traces.
  When omitted inside another dsprrr operation, the active context is
  inherited.

## Value

A tibble with the input columns plus a `result` list-column containing
one named declared-output record per row. With
`.return_format = "structured"`, the tibble also contains `.error`,
`.metadata`, and `.chat`; `.error` is `NA` for successful rows and
contains the LLM execution error message for failed rows.

## Examples

``` r
if (FALSE) { # \dontrun{
# Process data
df <- tibble::tibble(
  text = c("I love this!", "This is bad", "Okay product")
)

llm <- ellmer::chat_openai()
results <- signature("text -> sentiment") |>
  module() |>
  run_dataset(df, .llm = llm)
} # }
```
