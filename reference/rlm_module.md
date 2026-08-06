# Create a Recursive Language Model (RLM) Module

Factory function to create an RLMModule that enables LLMs to
programmatically explore large contexts through a REPL interface.

## Usage

``` r
rlm_module(
  signature,
  runner,
  max_iterations = 20L,
  max_llm_calls = 50L,
  max_output_chars = 100000L,
  sub_lm = NULL,
  verbose = FALSE,
  tools = list(),
  max_iters = NULL,
  ...
)
```

## Arguments

- signature:

  A Signature object or string notation defining inputs/outputs

- runner:

  A code runner implementing `execute()` and `policy()`. Required. The
  module retains this object; reset persistent runners between logically
  isolated jobs and do not use one runner concurrently.

- max_iterations:

  Maximum REPL iterations before fallback (default 20)

- max_llm_calls:

  Maximum recursive LLM calls allowed (default 50)

- max_output_chars:

  Maximum characters per execution output (default 100000)

- sub_lm:

  Optional ellmer Chat for recursive queries. NULL = disabled.

- verbose:

  Logical. Print execution progress (default FALSE)

- tools:

  Named list of user-defined R functions to inject into REPL. Each tool
  becomes available as a function in the code execution environment.
  Non-function values in the list will cause an error.

- max_iters:

  DSPy 3.3-compatible alias for `max_iterations`. Supply only one of
  these arguments.

- ...:

  Additional arguments passed to the module

## Value

An RLMModule object

## Examples

``` r
if (FALSE) { # \dontrun{
runner <- r_code_runner(timeout = 30)
rlm <- rlm_module("question -> answer", runner = runner)
result <- run(rlm, question = "What is the 10th Fibonacci number?", .llm = llm)
} # }
```
