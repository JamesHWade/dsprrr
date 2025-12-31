# Execute an LLM Module

Execute a module with the provided inputs to generate LLM output. This
is the primary function for running modules created with
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

Supports both single inputs and batch processing. Batch execution can be
parallelised, but is conservative by default to avoid reusing LLM
clients across workers.

## Usage

``` r
run(module, ...)
```

## Arguments

- module:

  A DSPrrr module (e.g., created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md))

- ...:

  Named arguments corresponding to the module's signature inputs. Can be
  single values or vectors for batch processing. Additional parameters:

  .llm

  :   An ellmer chat object for LLM interaction (optional)

  .verbose

  :   Logical indicating whether to print debug information

  .parallel

  :   Logical indicating whether to process batch inputs in parallel
      (default FALSE). When `TRUE`, a fresh LLM client is created per
      worker unless a custom `.llm` is supplied (in which case the call
      falls back to sequential execution).

  .progress

  :   Logical indicating whether to show progress bar for batch
      processing (default TRUE)

  .return_format

  :   Character, either "simple" (default) or "structured". "simple"
      returns just the output, "structured" returns list with output,
      chat, and metadata.

## Value

For single inputs with .return_format="simple": The parsed output
according to the module's signature. For single inputs with
.return_format="structured": A list with components:

- output: The parsed output

- chat: The ellmer chat object used

- metadata: Additional metadata (tokens used, latency, etc.) For batch
  inputs: A list of results matching the input length

## Details

**Retry Behavior:** ellmer automatically retries failed requests up to 3
times (configurable via `options(ellmer_max_tries = n)`). This handles
transient errors like rate limits and connection failures. See ellmer
documentation for more details.

## See also

- [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md) for
  one-shot LLM calls without creating a module

- [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
  for running a module on a data frame

- [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
  for running with metric evaluation

- [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  for creating modules

## Examples

``` r
if (FALSE) { # \dontrun{
# Single input
llm <- ellmer::chat_openai()
result <- signature("text -> sentiment") |>
  module(type = "predict") |>
  run(text = "I love this!", .llm = llm)

# Batch processing
results <- signature("text -> sentiment") |>
  module(type = "predict") |>
  run(text = c("I love this!", "This is bad"), .llm = llm)

# Structured return
result <- signature("text -> sentiment") |>
  module(type = "predict") |>
  run(text = "Great!", .llm = llm, .return_format = "structured")
# Access: result$output, result$chat, result$metadata

# Configure ellmer retry behavior (if needed)
options(ellmer_max_tries = 5)
} # }
```
