# Create a Recursive Language Model (RLM) Module

Create an RLM whose implementation can adaptively explore R objects at
inference time. Use RLM when the inspection path is not known in
advance; use ordinary R when that path becomes stable, or Flex when
labeled examples should discover a reusable implementation.

## Usage

``` r
rlm_module(
  signature,
  runner = NULL,
  interpreter_factory = NULL,
  max_iterations = 20L,
  max_llm_calls = 50L,
  max_output_chars = 10000L,
  sub_lm = NULL,
  verbose = FALSE,
  tools = list(),
  config = list(),
  chat = NULL,
  generate_action = NULL,
  extract = NULL,
  ...
)
```

## Arguments

- signature:

  A Signature object or string notation defining inputs/outputs with
  explicit ellmer string, number, integer, boolean, enum, array, or
  object output types. Opaque `TypeJsonSchema` outputs are unsupported.

- runner:

  Optional caller-owned code runner implementing `execute()` and
  `policy()`. Its policy must declare `persistent = TRUE`. It is
  retained, never automatically shut down, and must not be shared
  concurrently. For the trusted callr backend, use
  `r_code_runner(persistent = TRUE)`.

- interpreter_factory:

  Optional zero-argument function returning a fresh runner with
  `execute()`, `policy()`, optional
  [`start()`](https://rdrr.io/r/stats/start.html), and idempotent
  terminal `shutdown()`. Its policy must advertise `persistent = TRUE`
  for RLM. Supply exactly one of `runner` and `interpreter_factory`.

- max_iterations:

  Maximum REPL iterations before fallback (default 20)

- max_llm_calls:

  Maximum recursive LLM calls allowed (default 50)

- max_output_chars:

  Maximum model-visible characters per execution output. Longer output
  is shown as a head-and-tail excerpt. Default 10000.

- sub_lm:

  Optional ellmer Chat for recursive queries. `NULL` inherits the
  invocation's outer Chat. Set `max_llm_calls = 0` to disable recursion.

- verbose:

  Logical. Print execution progress (default FALSE)

- tools:

  Named list of user-defined host functions or ellmer ToolDef objects.
  Guest code emits an invocation-bound request, dsprrr validates it and
  invokes the original function in the host, and the guest is replayed
  with the response. Closures are never deparsed or serialized into
  generated code. These tools execute in the host process, outside the
  guest runner sandbox, with the host's permissions. ToolDef schemas
  guide generation; the callable must still enforce semantic constraints
  beyond the bridge's lossless JSON-compatible value checks. A protocol
  safety ceiling permits at most 1,000 host-tool calls in one generated
  R step.

- config:

  Optional prediction configuration.

- chat:

  Optional ellmer Chat object.

- generate_action:

  Optional advanced action predictor.

- extract:

  Optional advanced extraction predictor.

- ...:

  Must be empty.

## Value

An RLMModule object

## Examples

``` r
if (FALSE) { # \dontrun{
analyst <- rlm_module(
  "document, question -> answer",
  interpreter_factory = function() mcp_repl_runner(timeout = 30)
)
result <- run(
  analyst,
  document = "Owner: team-a\nObligation: rotate keys quarterly",
  question = "Which obligations have no owner?",
  .llm = ellmer::chat_openai()
)
} # }
```
