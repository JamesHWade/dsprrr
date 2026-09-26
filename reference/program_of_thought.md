# Create a Program of Thought module that answers by running R code

`program_of_thought()` creates a module that answers by writing R code,
running it with `runner`, and reading the answer from the result. It
suits tasks that need exact computation, such as arithmetic, statistics
or data manipulation, where a model's direct answer is unreliable. Run
it with [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

## Usage

``` r
program_of_thought(
  signature,
  runner = NULL,
  interpreter_factory = NULL,
  max_iters = 3L,
  extract_answer = TRUE,
  config = list(),
  chat = NULL,
  ...
)
```

## Arguments

- signature:

  A
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
  object or a signature string such as `"question -> answer"`.

- runner:

  A code runner you own, such as
  [`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md).
  It is reused across calls and never shut down by dsprrr.

- interpreter_factory:

  A function with no arguments that returns a fresh runner for each
  call. Supply exactly one of `runner` and `interpreter_factory`.

- max_iters:

  Integer maximum number of attempts to write working code (default
  `3L`).

- extract_answer:

  If `TRUE` (the default), a model call turns the execution result into
  the output fields. If `FALSE`, the formatted result is returned
  directly.

- config:

  Optional module configuration, such as runtime settings.

- chat:

  Optional ellmer Chat stored on the module.

- ...:

  Must be empty.

## Value

A Program of Thought module.

## Details

Each call works as follows:

1.  The model writes R code for the inputs.

2.  The runner executes it.

3.  If it fails, the error goes back to the model, which repairs the
    code. Steps 2 and 3 repeat up to `max_iters` times; if no attempt
    succeeds, the call fails.

4.  With `extract_answer = TRUE`, a second model call turns the
    execution result into the output fields; otherwise the result itself
    is returned.

Code runs only through the runtime you supply, as either `runner` or
`interpreter_factory`; see
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
for how each is owned and shut down.
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
runs code in a separate process with your permissions and is not a
sandbox. For untrusted input, use a sandboxed runner such as
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md);
`runner$policy()` shows what a runner enforces. A stateful `runner` must
not be used by two calls at the same time.

[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
supports Program of Thought with an `interpreter_factory`, in a separate
mirai process, but rejects a caller-owned `runner`. Token streaming with
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)
or the module's `$stream()` method is unavailable, because it would
bypass code execution.
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
runs the module as one non-streaming call and rejects requests for token
streaming.

## See also

Other program constructors:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md),
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

Other code execution:
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
pot <- program_of_thought(
  "question -> answer",
  runner = r_code_runner(timeout = 30)
)
pot
#> 
#> ── ProgramOfThoughtModule 
#> • Signature: question -> answer
#> • Max iterations: 3
#> • Extract answer: TRUE
#> • Runner: callr (caller-owned)

if (FALSE) { # \dontrun{
run(
  pot,
  question = "What is the sum of the primes below 100?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
