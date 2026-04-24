# ReAct Module

A ReAct-style module that can call tools during execution. Use this to
build tool-using agents that alternate between reasoning, tool calls,
and observations before producing a final structured answer.

## Details

Create a ReAct module with `module(type = "react", tools = list(...))`.
If you pass `tools` with `type = "predict"`, the module automatically
upgrades to `react`. ellmer executes the tool-calling loop internally;
dsprrr records the observed tool-call rounds and warns if they exceed
`max_iterations`, then produces output based on the module signature.

## See also

- [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  for creating modules

- [`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md)
  and
  [`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md)
  for search tools

- [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md)
  for tool + code execution agents

## Examples

``` r
if (FALSE) { # \dontrun{
library(dsprrr)
library(ellmer)

search_tool <- ellmer::tool(
  function(query) "Search results...",
  description = "Search for information",
  arguments = list(query = ellmer::type_string())
)

agent <- module(
  signature("question -> answer"),
  type = "react",
  tools = list(search_tool),
  max_iterations = 8L
)

result <- run(agent, question = "What is ReAct?", .llm = llm)

# Batch execution
questions <- tibble::tibble(question = c("Q1", "Q2"))
results <- run_dataset(agent, questions, .llm = llm)
} # }
```
