# Convert a DSPrrr Module to an ellmer Tool

Creates an ellmer-compatible tool from a dsprrr module. This allows
modules to be used as tools in ellmer Chat objects, enabling agentic
workflows where the LLM can call dsprrr modules as part of its
reasoning.

## Usage

``` r
as_ellmer_tool(module, name = NULL, description = NULL, .llm = NULL)
```

## Arguments

- module:

  A DSPrrr module (created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)).

- name:

  Optional tool name. Defaults to a name derived from the signature.

- description:

  Optional tool description. Defaults to the signature's instructions or
  a generated description.

- .llm:

  Optional ellmer Chat object for the module to use when called. If not
  provided, the module's stored chat or default chat is used.

## Value

A `ToolDef` object from ellmer, suitable for use with
`ellmer::Chat$register_tool()`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a sentiment analysis module
sentiment_mod <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')"),
  type = "predict"
)

# Convert to ellmer tool
sentiment_tool <- as_ellmer_tool(sentiment_mod, name = "analyze_sentiment")

# Register with a Chat for agentic use
chat <- ellmer::chat_openai()
chat$register_tool(sentiment_tool)

# Now the LLM can use the sentiment tool
chat$chat("Analyze the sentiment of: 'I love this product!'")
} # }
```
