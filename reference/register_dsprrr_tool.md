# Register a DSPrrr Module as a Tool in a Chat

Convenience function that creates an ellmer tool from a module and
registers it with a Chat object in one step.

## Usage

``` r
register_dsprrr_tool(
  chat,
  module,
  name = NULL,
  description = NULL,
  .llm = NULL,
  annotations = list(),
  output = c("auto", "json", "text", "raw"),
  copy = c("none", "deep"),
  error = c("reject", "abort", "return"),
  trace_context = list()
)
```

## Arguments

- chat:

  An ellmer Chat object.

- module:

  A DSPrrr module.

- name:

  Optional tool name.

- description:

  Optional tool description.

- .llm:

  Optional ellmer Chat object for the module to use when called. If not
  provided, the module's stored chat or default chat is used.

- annotations:

  Optional ellmer tool annotations list, passed through to
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html).

- output:

  Tool result serialization mode. See
  [`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md).

- copy:

  Whether tool calls should use the supplied module directly or a fresh
  deep copy. See
  [`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md).

- error:

  Tool error handling mode. See
  [`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md).

- trace_context:

  A named, JSON-compatible list captured by the tool and propagated to
  dsprrr execution metadata and traces.

## Value

The Chat object (invisibly), with the tool registered.

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- ellmer::chat_openai()
mod <- module(signature("query -> answer"), type = "predict")

# Register in one step
register_dsprrr_tool(chat, mod, name = "knowledge_lookup")

# Use the tool
chat$chat("Use the knowledge_lookup tool to answer: What is R?")
} # }
```
