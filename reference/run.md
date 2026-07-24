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
      (default FALSE).

  .parallel_method

  :   Character, either "ellmer" (default) or "mirai". "ellmer" uses
      ellmer's
      [`parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html)
      for native async HTTP parallelism (more efficient, single
      process). "mirai" uses mirai for multi-process parallelism
      (requires `.llm = NULL` so each worker can create an independent
      client).

  .concurrency

  :   A validated policy created by
      [`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md).
      When supplied, do not also pass `.parallel` or `.parallel_method`.

  .progress

  :   Logical indicating whether to show progress bar for batch
      processing (default TRUE)

  .return_format

  :   Character, either "simple" (default) or "structured". "simple"
      returns just the output, "structured" returns list with output,
      chat, and metadata.

  .cache

  :   Logical or NULL. Per-call cache control. If NULL (default), uses
      global config. If TRUE, attempts to use cache (no effect if
      caching globally disabled). If FALSE, bypasses cache for this call
      only.

## Value

For single inputs with .return_format="simple": The parsed output
according to the module's signature. For single inputs with
.return_format="structured": A list with components:

- output: The parsed output

- chat: The ellmer chat object used

- metadata: Additional metadata (tokens used, latency, etc.)

For batch inputs: A list of results matching the input length. Empty
batches return a zero-length list (with class `dsprrr_batch_result` for
structured output).

## Details

**Retry Behavior:** ellmer automatically retries failed requests up to 3
times (configurable via `options(ellmer_max_tries = n)`). This handles
transient errors like rate limits and connection failures. See ellmer
documentation for more details.

Zero-length inputs form an empty batch only when every input is zero
length. Empty batches return immediately without resolving a Chat or
touching cache, trace, or prompt-history state. Mixing zero-length and
non-empty inputs is an error.

Scalar and batch Predict calls record one trace per attempted row.
Structured metadata reports usage, error, cache, backend, and
batch-index fields. Native ellmer and mirai workers return row records
that are committed to module and global trace state by the parent in
input order. Specialized Predict subclasses, such as ReAct, preserve
their scalar `forward()` method and currently reject vectorized inputs
rather than bypassing specialized logic.

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
