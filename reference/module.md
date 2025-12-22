# Create an LLM Module

The primary function for creating executable LLM modules. Supports
"predict" for standard structured prediction and "react" for ReAct-style
tool-using modules.

## Usage

``` r
module(
  signature,
  type = "predict",
  tools = NULL,
  max_iterations = 10L,
  template = "",
  demos = list(),
  config = list(),
  chat = NULL,
  ...
)
```

## Arguments

- signature:

  A Signature object defining the module's interface

- type:

  Character string specifying the module type:

  - `"predict"` (default): Standard prediction module

  - `"react"`: ReAct-style module with tool support

- tools:

  Optional list of ellmer ToolDef objects for react modules. If provided
  with `type = "predict"`, automatically upgrades to react.

- max_iterations:

  Maximum ReAct iterations (default: 10, only for react)

- template:

  Optional glue template for prompt generation

- demos:

  Optional list of demonstration examples

- config:

  Optional configuration list

- chat:

  Optional ellmer Chat object for LLM operations. If provided, the
  module will use this Chat for all predictions unless overridden with
  `.llm`.

- ...:

  Additional arguments for future module types

## Value

A module object (R6) that can be executed with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)

## Examples

``` r
# Create a simple prediction module
classifier <- signature("text -> sentiment") |>
  module(type = "predict", template = "Analyze: {text}")

# With demonstrations
qa <- signature("context, question -> answer") |>
  module(
    type = "predict",
    demos = list(
      list(
        inputs = list(context = "...", question = "..."),
        output = "..."
      )
    )
  )

if (FALSE) { # \dontrun{
# Execute the module (requires an llm object)
llm <- ellmer::chat_openai()
result <- classifier |> run(text = "Great package!", .llm = llm)

# Or create module with Chat attached
classifier <- signature("text -> sentiment") |>
  module(type = "predict", chat = chat_openai())
result <- classifier |> run(text = "Great package!")  # No .llm needed

# Create a ReAct module with tools
search_tool <- ellmer::tool(
  search_fn,
  description = "Search for information",
  arguments = list(query = ellmer::type_string())
)
agent <- signature("question -> answer") |>
  module(type = "react", tools = list(search_tool), chat = chat_openai())
} # }
```
