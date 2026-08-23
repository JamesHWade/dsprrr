# Create a ragnar Search Tool for ReAct Modules

Creates an ellmer-compatible tool that searches a ragnar document store.
This tool can be used with ReAct modules or registered with ellmer Chat
objects for agentic document retrieval.

## Usage

``` r
ragnar_tool(store, k = 5L, name = "search_knowledge", description = NULL)
```

## Arguments

- store:

  A ragnar store created with `ragnar::ragnar_store_create()`.

- k:

  Number of documents to retrieve per search (default 5).

- name:

  Tool name (default "search_knowledge").

- description:

  Tool description for the LLM.

## Value

A function suitable for use with ReAct modules or
`ellmer::Chat$register_tool()`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a ragnar store from documents
library(ragnar)
store <- ragnar_store_create(
  documents = my_docs,
  embedding_fn = embed_openai()
)

# Create a search tool
search_tool <- ragnar_tool(store, k = 3)

# Use with ReAct module
react_mod <- react(
  signature("question -> answer"),
  tools = list(search_tool)
)

# Or register with ellmer Chat
chat <- ellmer::chat_openai()
chat$register_tool(search_tool)
} # }
```
