# Compare several reasoning chains and synthesize an answer

`multi_chain_comparison()` builds a module that runs an inner module `M`
times, then makes one more call that reads all the attempts and writes a
final answer with its own reasoning (DSPy's MultiChainComparison).

## Usage

``` r
multi_chain_comparison(
  signature,
  inner_module = NULL,
  M = 3L,
  temperature = 0.7,
  comparison_template = NULL,
  config = list(),
  chat = NULL,
  ...
)
```

## Arguments

- signature:

  A signature from
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  or a signature string.

- inner_module:

  The module that produces each attempt. The default is
  `chain_of_thought(signature)`.

- M:

  Number of attempts.

- temperature:

  Temperature applied to the attempts, to make them differ; `NULL` sends
  none. Reasoning models may reject a temperature: gpt-6-luna accepts it
  only with `reasoning_effort = "none"`.

- comparison_template:

  A glue template for the comparison prompt. It can use `{M}`,
  `{attempts_text}` (each attempt's output fields under an "=== Attempt
  i ===" heading) and input fields such as `{question}`. The default
  shows the attempts but not the original inputs.

- config, chat:

  As in
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- ...:

  Must be empty.

## Value

A module (an R6 object of class `MultiChainComparisonModule`).

## Details

Each [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
makes `M + 1` model calls. A failed attempt gives a warning and is left
out; the module fails only if every attempt fails. The final output has
a `reasoning` field followed by the signature's output fields. The
returned module's `get_attempts()` method lists the attempts of the last
run.

With the response cache on, identical attempts are served from the
cache, so pass `.cache = FALSE` to
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) to get
`M` independent attempts.

## See also

Other program constructors:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md),
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
mcc <- multi_chain_comparison("question -> answer", M = 3L)
mcc
#> 
#> ── MultiChainComparisonModule ──
#> 
#> M: 3 reasoning chains
#> Temperature: 0.7
#> Inner module: <PredictModule>

# Let the comparison step see the question too
mcc_with_question <- multi_chain_comparison(
  "question -> answer",
  M = 3L,
  comparison_template = paste(
    "Question: {question}",
    "Here are {M} attempts:",
    "{attempts_text}",
    "Write the best final answer.",
    sep = "\n\n"
  )
)

if (FALSE) { # \dontrun{
run(
  mcc_with_question,
  question = "A bat and a ball cost $1.10. The bat costs $1 more. What does the ball cost?",
  .llm = ellmer::chat_openai(
    model = "gpt-6-luna",
    params = ellmer::params(reasoning_effort = "none")
  ),
  .cache = FALSE
)
} # }
```
