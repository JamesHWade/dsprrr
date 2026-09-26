# Create a chain-of-thought module

`chain_of_thought()` is `module(with_reasoning(x))`: a prediction module
whose output starts with a `reasoning` field, so the model reasons step
by step before it gives the other outputs.

## Usage

``` r
chain_of_thought(
  x,
  prefix = "Let's think step by step in order to",
  chat = NULL,
  template = "",
  demos = list(),
  config = list(),
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

- chat, template, demos, config:

  As in
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- ...:

  Must be empty.

## Value

A prediction module, as from
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) returns
the reasoning along with the other outputs, for example
`list(reasoning = "...", answer = "...")`.

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

if (FALSE) { # \dontrun{
result <- run(
  solver,
  question = "What is 15 * 24?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
result$reasoning
result$answer
} # }
```
