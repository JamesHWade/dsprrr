# Run a Recursive Language Model in one call

Convenience wrapper that creates a runner, module, and executes an RLM
in a single call. Equivalent to:

    runner <- r_code_runner(timeout = .timeout)
    mod <- rlm_module(
      signature,
      runner = runner,
      max_iterations = .max_iterations,
      max_llm_calls = .max_llm_calls,
      sub_lm = .sub_lm,
      verbose = .verbose,
      tools = .tools
    )
    run(mod, ..., .llm = .llm)

For repeated use or optimization, prefer creating a module with
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
and calling
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
separately.

## Usage

``` r
rlm(
  signature,
  ...,
  .llm = NULL,
  .timeout = 30,
  .max_iterations = 20L,
  .max_llm_calls = 50L,
  .sub_lm = NULL,
  .tools = list(),
  .verbose = FALSE
)
```

## Arguments

- signature:

  A Signature object or string notation defining inputs/outputs (e.g.,
  `"question -> answer"`)

- ...:

  Named arguments matching the signature's inputs. These are passed to
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

- .llm:

  An ellmer Chat object. If `NULL`, uses the default Chat from
  [`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md).

- .timeout:

  Numeric. Maximum execution time in seconds per code evaluation.
  Default 30.

- .max_iterations:

  Integer. Maximum REPL iterations before fallback. Default 20.

- .max_llm_calls:

  Integer. Maximum recursive LLM calls allowed. Default 50.

- .sub_lm:

  Optional ellmer Chat for recursive `llm_query()` calls. `NULL`
  disables recursive queries.

- .tools:

  Named list of user-defined R functions available in the REPL.

- .verbose:

  Logical. Print execution progress. Default `FALSE`.

## Value

The module output according to the signature.

## See also

- [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
  for creating reusable RLM modules

- [`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
  for configuring the code execution backend

- [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
  executing modules

- [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md) for
  simple one-shot LLM calls (no code execution)

## Examples

``` r
if (FALSE) { # \dontrun{
# One-liner RLM call
result <- rlm(
  "document, question -> answer",
  document = readLines("big_file.txt") |> paste(collapse = "\n"),
  question = "What are the main themes?",
  .llm = ellmer::chat_openai()
)

# With recursive sub-queries
result <- rlm(
  "codebase, question -> answer",
  codebase = source_code,
  question = "How does auth work?",
  .llm = ellmer::chat_openai(),
  .sub_lm = ellmer::chat_openai(model = "gpt-4o-mini")
)
} # }
```
