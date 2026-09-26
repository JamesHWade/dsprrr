# Create a retrieval-augmented generation module

`rag_module()` builds a module that, on every call, retrieves documents
related to the input, adds them to the prompt, and asks the model to
answer from them. Retrieval comes from a ragnar store or from any R
function you supply.

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

  A signature from
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  or a signature string, such as `"question -> answer"`.

- store:

  A ragnar store (see
  [`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md)
  for how to build one), searched with
  `ragnar::ragnar_retrieve(store, query, top_k = k)`.

- retriever:

  A function called as `retriever(query, k = k)` that returns a
  character vector of documents. It takes precedence over `store` (with
  a warning if both are given).

- k:

  Number of documents to retrieve.

- context_format:

  Name under which the retrieved text appears in the prompt.

- config:

  A list of settings. `fail_on_retrieval_error = TRUE` turns retrieval
  errors into errors; by default they give a warning and the model sees
  "No relevant context found.".

- chat:

  An ellmer Chat stored on the module.

## Value

A module (an R6 object of class `RAGModule`).

## Details

The query is the first of the inputs `query`, `question`, `text`,
`input` or `prompt`, or else the first non-empty string input. The
retrieved documents are numbered (`[1] ...`) and added at the end of the
prompt, under a `# Retrieved context` heading and the label
`context_format`.

The context is added whether or not the signature declares a
`context_format` input, and callers never pass it to
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md). If the
signature does declare it, as in
`"question, relevant_context -> answer"`, the retrieved text is
currently written into the prompt twice.

With `.return_format = "structured"`, the metadata records the `query`
and the `retrieved_context`.

## See also

Other program constructors:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md),
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
docs <- c(
  "The Eiffel Tower is 330 metres tall.",
  "The Louvre is the most visited museum in the world.",
  "Paris has 20 arrondissements."
)
# A keyword retriever, so the example needs no embeddings
keyword_retriever <- function(query, k) {
  words <- tolower(strsplit(query, "\\W+")[[1]])
  hits <- vapply(
    docs,
    function(doc) sum(words %in% tolower(strsplit(doc, "\\W+")[[1]])),
    numeric(1)
  )
  unname(head(docs[order(-hits)], k))
}
keyword_retriever("How tall is the Eiffel Tower?", k = 2)
#> [1] "The Eiffel Tower is 330 metres tall."               
#> [2] "The Louvre is the most visited museum in the world."

rag <- rag_module("question -> answer", retriever = keyword_retriever, k = 2L)
rag
#> 
#> ── RAGModule ──
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
#> ── Retrieval Configuration 
#> k: 2 documents
#> Context field: relevant_context
#> Retriever: <custom function>

if (FALSE) { # \dontrun{
run(
  rag,
  question = "How tall is the Eiffel Tower?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)

# With a ragnar store, built as shown in ?ragnar_tool
rag_store <- rag_module("question -> answer", store = store, k = 3L)
} # }
```
