# Create a prediction module

`module()` turns a signature into a prediction module: one structured
model call per input, whose output follows the signature. It is the
usual starting point:
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
-\> `module()` -\>
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) -\>
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
-\>
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

Other kinds of program have their own constructors:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
adds a reasoning step,
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md)
calls tools,
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md)
compares several reasoning chains, and
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
and [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
run code.

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

  A signature from
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md).
  A string is not accepted here; wrap it in
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md).

- chat:

  An ellmer Chat stored on the module.
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) uses
  it unless you pass `.llm`; see
  [`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md)
  for the full order.

- template:

  A glue template for the input part of the prompt, with input fields in
  single braces, as in `"Review: {text}"`. The default (`""`) lists each
  input as `name: value`.

- demos:

  Worked examples placed before the input in every prompt: a list of
  `list(inputs = list(...), output = list(...))`.
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  sets demos for you.

- config:

  Model settings applied to a copy of the chat on every call:
  `temperature`, `top_p`, `reasoning_effort`, `max_tokens`,
  `max_output_tokens`, `frequency_penalty`, `presence_penalty` and
  `service_tier`, for example `config = list(reasoning_effort = "low")`.
  Reasoning models such as gpt-6-luna accept `temperature` and `top_p`
  only with `reasoning_effort = "none"`. Chat settings such as `model`
  or `provider` are an error here; set them on the chat instead.

- ...:

  Must be empty. Arguments of other constructors, such as `tools` or
  `type`, give an error that names the constructor to use.

## Value

A prediction module (an R6 object of class `PredictModule`) to use with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
and
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## See also

Other program constructors:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md),
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
classifier <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')"),
  template = "Classify the sentiment of this review:\n{text}",
  demos = list(
    list(
      inputs = list(text = "Arrived broken."),
      output = list(sentiment = "negative")
    )
  ),
  config = list(reasoning_effort = "low")
)
classifier
#> 
#> ── PredictModule ──
#> 
#> ── Signature 
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • text: "string" - Input: text
#> 
#> ── Output 
#> Type: "object(sentiment: enum(positive, negative, neutral))"
#> 
#> ── Instructions 
#> Given the fields `text`, produce the fields `sentiment`.
#> 
#> ── Template 
#> Classify the sentiment of this review:
#> {text}
#> 
#> ── Demos 
#> 1 demonstration(s) loaded

if (FALSE) { # \dontrun{
run(
  classifier,
  text = "Great package!",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
