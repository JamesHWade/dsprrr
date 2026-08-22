# Run a Recursive Language Model in one call

Run a one-off RLM investigation. By default this creates a fresh managed
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
for the invocation. Its default OS sandbox disables network access but
permits writes inside the allowed workspace. Pass `.runner` or
`.interpreter_factory` to select another execution backend. For repeated
use, optimization, or explicit lifecycle control, create an
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
instead. The managed default requires the suggested `mcptools` package
and Posit's external `mcp-repl` executable; see
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
for setup and transport limits.

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

  A Signature object or string notation defining inputs/outputs (e.g.,
  `"question -> answer"`)

- ...:

  Named signature inputs and
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
  controls such as `.return_format`. Every supplied input is one scalar
  REPL variable, including vectors, lists, matrices, and data frames. To
  run multiple investigations, create an
  [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
  and call
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md);
  store rich per-row values in list-columns.

- .llm:

  An ellmer Chat object. If `NULL`, uses the default Chat from
  [`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md).

- .timeout:

  Numeric. Maximum execution time in seconds per code evaluation for the
  implicit managed MCP runner. Explicit runners and factories own their
  timeout settings. Default 30.

- .max_iterations:

  Integer. Maximum REPL iterations before fallback. Default 20.

- .max_llm_calls:

  Integer. Maximum recursive LLM calls allowed. Default 50.

- .max_output_chars:

  Maximum model-visible characters per execution output. Default 10000.

- .sub_lm:

  Optional ellmer Chat for recursive `llm_query()` calls. `NULL`
  inherits `.llm`; use `.max_llm_calls = 0` to disable recursion.

- .tools:

  Named list of user-defined R functions or ellmer ToolDef objects
  available in the REPL. They execute in the dsprrr host process,
  outside the guest runner sandbox.

- .verbose:

  Logical. Print execution progress. Default `FALSE`.

- .runner:

  Optional caller-owned runner. Supply at most one of this and
  `.interpreter_factory`. Its policy must advertise `persistent = TRUE`.

- .interpreter_factory:

  Optional zero-argument factory for a fresh, invocation-owned runner.
  When both execution arguments are `NULL`, a managed
  [`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
  factory is used. Custom factories must return a runner whose policy
  advertises `persistent = TRUE`.

## Value

With `.return_format = "simple"` (the default), the output record
according to the signature. With `.return_format = "structured"`, a
`dsprrr_result` containing `output`, `chat`, and `metadata`.

## See also

- [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
  for creating reusable RLM modules

- [`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
  for configuring the code execution backend

- [`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
  for managed sandboxed execution

- [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
  executing modules

## Examples

``` r
if (FALSE) { # \dontrun{
result <- rlm(
  "document, question -> answer",
  document = "Owner: team-a\nObligation: rotate keys quarterly",
  question = "What are the main themes?",
  .llm = ellmer::chat_openai(),
  .max_iterations = 4L,
  .max_llm_calls = 0L
)

# Large or rich local R objects require explicit trusted execution.
sessions <- data.frame(
  release = c("2.3.9", "2.4.0"),
  converted = c(TRUE, FALSE)
)
local_runner <- r_code_runner(persistent = TRUE)
result <- rlm("sessions, question -> answer", sessions = sessions,
  question = "Where did conversion fall?",
  .llm = ellmer::chat_openai(),
  .runner = local_runner)
local_runner$shutdown()
} # }
```
