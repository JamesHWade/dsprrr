# Create a Semantic Search Tool from Documents

Convenience function that creates a ragnar store from documents and
wraps it in a search tool in one step.

## Usage

``` r
create_search_tool(
  documents,
  embedding_fn,
  k = 5L,
  name = "search_documents",
  description = NULL,
  ...
)
```

## Arguments

- documents:

  Character vector of documents, or a data frame with a 'text' or
  'content' column.

- embedding_fn:

  Embedding function from ragnar (e.g., `ragnar::embed_openai()`).

- k:

  Number of documents to retrieve per search (default 5).

- name:

  Tool name (default "search_documents").

- description:

  Optional tool description.

- ...:

  Additional arguments passed to `ragnar::ragnar_store_create()`.

## Value

A search tool function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create tool directly from documents
docs <- c(
  "R is a programming language for statistical computing.",
  "Python is a general-purpose programming language.",
  "Julia is designed for high-performance numerical computing."
)

search_tool <- create_search_tool(
  documents = docs,
  embedding_fn = ragnar::embed_openai(),
  k = 2
)

# Use the tool
search_tool("What language is best for statistics?")
} # }
```
