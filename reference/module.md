# Create a prediction module

Create a standard structured-prediction module. This is the primary
constructor in the dsprrr journey:
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
-\> `module()` -\>
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) -\>
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
-\>
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

Agentic, reasoning, and code-executing programs use explicit
constructors such as
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md),
and [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md).
Keeping those choices in the function name prevents configuration from
silently changing the kind of program being built.

## Usage

``` r
module(
  signature,
  chat = NULL,
  template = "",
  demos = list(),
  config = list(),
  ...
)
```

## Arguments

- signature:

  A Signature object defining the module interface.

- chat:

  Optional ellmer Chat object. When supplied,
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) uses
  it unless an explicit `.llm` is provided.

- template:

  Optional glue template for prompt generation.

- demos:

  Optional list of demonstration examples.

- config:

  Optional prediction configuration. Model parameters such as
  temperature belong here, for example
  `config = list(temperature = 0.2)`.

- ...:

  Must be empty. Use a dedicated constructor for advanced module
  behavior.

## Value

A PredictModule executed with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

## Examples

``` r
classifier <- signature("text -> sentiment") |>
  module(template = "Analyze: {text}")

configured <- signature("question -> answer") |>
  module(config = list(temperature = 0.2))

if (FALSE) { # \dontrun{
llm <- ellmer::chat_openai()
result <- classifier |>
  run(text = "Great package!", .llm = llm)
} # }
```
