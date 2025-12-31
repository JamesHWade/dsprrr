# Create a RAG Module

Factory function to create a Retrieval-Augmented Generation module. RAG
modules automatically retrieve relevant context from a document store
before generating responses.

## Usage

``` r
rag_module(
  signature,
  store = NULL,
  retriever = NULL,
  k = 5L,
  context_format = "relevant_context",
  config = list(),
  chat = NULL
)
```

## Arguments

- signature:

  A signature string or Signature object defining inputs/outputs.

- store:

  Optional ragnar store for document retrieval.

- retriever:

  Optional custom retriever function. Should accept `(query, k)` and
  return a character vector of documents.

- k:

  Number of documents to retrieve (default 5).

- context_format:

  Field name for the context in the prompt (default "relevant_context").
  This field is automatically added to inputs.

- config:

  Optional configuration list.

- chat:

  Optional ellmer Chat object.

## Value

A RAGModule object.

## Examples

``` r
if (FALSE) { # \dontrun{
# With ragnar store
store <- ragnar::ragnar_store_create(documents)
mod <- rag_module(
  "question, relevant_context -> answer",
  store = store,
  k = 3
)

result <- run(mod, question = "What is the capital of France?")

# With custom retriever
my_retriever <- function(query, k) {
  # Custom retrieval logic
  c("Document 1 content", "Document 2 content")
}

mod <- rag_module(
  "query, relevant_context -> response",
  retriever = my_retriever
)
} # }
```
