# Run a recursive language model (RLM) in one call

`rlm()` runs a one-off RLM investigation: it builds an
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md),
runs it on the inputs in `...`, and returns the result. By default each
call gets a fresh managed
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
whose sandbox disables network access but allows writes in its
workspace; this needs the suggested mcptools package and Posit's
`mcp-repl` executable. Pass `.runner` or `.interpreter_factory` to use
another backend. For repeated use, optimization or control over the
runner's lifetime, create an
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
instead.

## Usage

``` r
rlm(
  signature,
  ...,
  .llm = NULL,
  .timeout = 30,
  .max_iterations = 20L,
  .max_llm_calls = 50L,
  .max_output_chars = 10000L,
  .sub_lm = NULL,
  .tools = list(),
  .verbose = FALSE,
  .runner = NULL,
  .interpreter_factory = NULL
)
```

## Arguments

- signature:

  A
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
  object or a signature string such as `"question -> answer"`.

- ...:

  Named inputs, plus
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
  options such as `.return_format`. Each input is staged as one
  variable, including vectors, lists, matrices and data frames. For
  several investigations, create an
  [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
  and use
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
  with list-columns.

- .llm:

  An ellmer Chat. `NULL` uses the default chat from
  [`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md).

- .timeout:

  Maximum execution time per code step, in seconds, for the default
  managed runner (default 30). Runners you supply use their own timeout.

- .max_iterations:

  Integer maximum number of code steps before the fallback extraction
  (default `20L`).

- .max_llm_calls:

  Integer maximum number of sub-queries (default `50L`).

- .max_output_chars:

  Maximum number of characters of each execution result shown to the
  model (default `10000L`).

- .sub_lm:

  Optional ellmer Chat for `llm_query()` sub-queries. `NULL` uses
  `.llm`; set `.max_llm_calls = 0L` to disable sub-queries.

- .tools:

  Named list of R functions or ellmer tools that the generated code can
  call. They run in the host R process, outside the sandbox.

- .verbose:

  Whether to print progress (default `FALSE`).

- .runner:

  Optional persistent runner you own. Supply at most one of this and
  `.interpreter_factory`.

- .interpreter_factory:

  Optional function with no arguments that returns a fresh persistent
  runner for the call. When both this and `.runner` are `NULL`, a
  managed
  [`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
  is used.

## Value

With `.return_format = "simple"` (the default), a named list with the
signature's outputs. With `.return_format = "structured"`, a
`dsprrr_result` with `output`, `chat` and `metadata`.

## See also

Other program constructors:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md),
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

Other code execution:
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- rlm(
  "document, question -> answer",
  document = "Owner: team-a\nObligation: rotate keys quarterly",
  question = "What are the main themes?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .max_iterations = 4L,
  .max_llm_calls = 0L
)

# Rich local R objects need a trusted runner that can stage them
sessions <- data.frame(
  release = c("2.3.9", "2.4.0"),
  converted = c(TRUE, FALSE)
)
local_runner <- r_code_runner(persistent = TRUE)
result <- rlm(
  "sessions, question -> answer",
  sessions = sessions,
  question = "Where did conversion fall?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .runner = local_runner
)
local_runner$shutdown()
} # }
```
