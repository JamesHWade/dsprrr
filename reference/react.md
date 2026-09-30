# Create a tool-using ReAct module

`react()` builds an agent that alternates between reasoning and calling
tools until it can answer, then returns a structured answer that follows
the signature (the ReAct pattern).

## Usage

``` r
react(
  signature,
  tools = list(),
  max_iterations = 10L,
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
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  or a signature string.

- tools:

  A list of ellmer tool definitions, made with
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html),
  [`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md),
  [`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md)
  or
  [`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md).
  Their names must be unique. The module's
  `$add_tool(tool, replace = FALSE)` adds one later; with
  `replace = TRUE` it replaces the tool of the same name.

- max_iterations:

  Maximum number of tool-calling rounds. Several tool calls in one model
  turn count as one round. Exceeding the limit is an error.

- chat, template, demos, config:

  As in
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- ...:

  Must be empty.

## Value

A module (an R6 object of class `ReactModule`) to use with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) and
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).

## Details

ellmer runs the tool-calling loop: the model's tool requests are
executed and their results sent back, keeping ellmer's turn history and
tool-call IDs. After the loop, one more request asks for the final
answer in the signature's output format. ReAct calls do not use the
response cache.

[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) accepts
one input at a time for this module; use
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
for several. With `.return_format = "structured"`, the metadata records
`iterations`, `tool_calls` and `tools_used`.

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
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
lookup_population <- ellmer::tool(
  function(city) {
    switch(city, Paris = "2.1 million", Lyon = "0.5 million", "unknown")
  },
  name = "lookup_population",
  description = "Look up the population of a French city.",
  arguments = list(city = ellmer::type_string("City name"))
)

agent <- react(
  "question -> answer",
  tools = list(lookup_population),
  max_iterations = 5L
)
agent
#> 
#> ── ReactModule ──
#> 
#> ── Signature 
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • question: "string" - Input: question
#> 
#> ── Output 
#> Type: "object(answer: string)"
#> 
#> ── Instructions 
#> Given the fields `question`, produce the fields `answer`.
#> 
#> ── Tools 
#> • lookup_population
#> Max iterations: 5

if (FALSE) { # \dontrun{
llm <- ellmer::chat_openai(model = "gpt-6-luna")
run(agent, question = "How many more people live in Paris than in Lyon?", .llm = llm)

questions <- data.frame(question = c("Population of Paris?", "Population of Lyon?"))
run_dataset(agent, questions, .llm = llm)
} # }
```
