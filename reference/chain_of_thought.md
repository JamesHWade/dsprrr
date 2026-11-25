# Create a chain-of-thought module

`chain_of_thought()` creates a prediction module with one of three
reasoning modes. The default, `"rationale"`, adds a `reasoning` output
field before the requested fields. `"plan"` adds a short, explicit
`plan` field before the requested fields. `"native"` keeps the signature
unchanged and exposes provider-supplied reasoning metadata when the
provider records it in ellmer turns.

## Usage

``` r
chain_of_thought(
  x,
  prefix = "Let's think step by step in order to",
  chat = NULL,
  template = "",
  demos = list(),
  config = list(),
  mode = c("rationale", "native", "plan"),
  ...
)
```

## Arguments

- x:

  A signature from
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  or a signature string.

- prefix:

  Start of the reasoning field's description; see
  [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md).
  Used only when `mode = "rationale"`.

- chat, template, demos, config:

  As in
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- mode:

  The reasoning behavior: `"rationale"` (the default), `"native"` or
  `"plan"`. `"native"` does not select a model or enable provider
  reasoning by itself. Configure provider parameters such as
  `reasoning_effort` through `chat` or `config`, using ellmer's
  supported parameters.

- ...:

  Must be empty.

## Value

A prediction module, as from
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).
The `"rationale"` and `"plan"` modes include their field in the output.
With `"native"`, the output keeps the signature's fields; on a
structured result,
[`get_metadata()`](https://jameshwade.github.io/dsprrr/reference/accessors.md)
exposes `native_reasoning` as a list of ellmer `ContentThinking` objects
from the current call, or `NULL` when the provider did not expose any.
It never synthesizes a reasoning string.

Native metadata is supported in structured results from synchronous
scalar [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
calls and direct
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
calls. Native mode errors before provider work for batch or streaming
execution, whose current result contracts do not preserve these turns.

## See also

Other program constructors:
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md),
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
solver <- chain_of_thought("question -> answer: float")
solver
#> 
#> ── PredictModule ──
#> 
#> ── Signature 
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • question: "string" - Input: question
#> 
#> ── Output 
#> Type: "object(reasoning: string, answer: number)"
#> 
#> ── Instructions 
#> Given the fields `question`, produce the fields `answer`. Think through your
#> reasoning step by step before providing the answer.

native_solver <- chain_of_thought(
  "question -> answer: float",
  mode = "native",
  config = list(reasoning_effort = "low")
)

if (FALSE) { # \dontrun{
result <- run(
  solver,
  question = "What is 15 * 24?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
result$reasoning
result$answer

native_result <- run(
  native_solver,
  question = "What is 15 * 24?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .return_format = "structured"
)
get_metadata(native_result)$native_reasoning
} # }
```
