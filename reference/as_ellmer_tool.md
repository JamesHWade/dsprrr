# Convert a DSPrrr Module to an ellmer Tool

Creates an ellmer-compatible tool from a dsprrr module. This allows
modules to be used as tools in ellmer Chat objects, enabling agentic
workflows where the LLM can call dsprrr modules as part of its
reasoning.

## Usage

``` r
as_ellmer_tool(
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

- annotations:

  Optional ellmer tool annotations list, passed through to
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html).
  This lets downstream runtimes reason about properties such as
  read-only or destructive behavior without dsprrr depending on them.

- output:

  Tool result serialization mode:

  - `"auto"` returns the native module result with fields ordered to
    match the signature.

  - `"json"` returns compact JSON with signature-ordered top-level
    fields.

  - `"text"` returns the primary output field as text when possible, or
    JSON text otherwise.

  - `"raw"` returns the native module result unchanged, without field
    reordering.

- copy:

  Whether tool calls should use the supplied module directly (`"none"`)
  or a fresh deep copy (`"deep"`).

- error:

  Tool error handling:

  - `"reject"` (default) returns a structured recoverable error
    observation suitable for surfacing back to an LLM. The condition
    class is preserved in `$type`.

  - `"abort"` propagates the original error to the caller.

  - `"return"` signals the error as a classed `dsprrr_tool_error`
    condition carrying the structured observation in `$payload`. Callers
    can install a
    [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html) to
    inspect the failure without aborting.

- trace_context:

  A named, JSON-compatible list captured by the tool and propagated to
  dsprrr execution metadata and traces. The tool's declared result
  schema is unchanged.

## Value

A `ToolDef` object from ellmer, suitable for use with
`ellmer::Chat$register_tool()`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a sentiment analysis module
sentiment_mod <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
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
