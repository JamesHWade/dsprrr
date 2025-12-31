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
  .llm = NULL
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
