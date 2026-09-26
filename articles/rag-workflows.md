# Retrieval-augmented generation

A retrieval-augmented generation (RAG) module looks up passages related
to its input and adds them to the prompt before the model answers. This
page shows how to build one with your own retriever or a ragnar store,
how to let an agent search when it needs to, how to evaluate the result,
and what happens when retrieval fails.

## Use your own retriever

A retriever is a function of `query` and `k` that returns up to `k`
passages as a character vector, best first. It can wrap a database
query, a search API or plain R. This one ranks a few store policies by
shared words:

``` r

library(dsprrr)

policies <- c(
  "Standard delivery takes three to five business days.",
  "Express delivery arrives the next business day if you order before noon.",
  "Unused items can be returned within 30 days of purchase.",
  "Refunds go back to the original payment method within five business days.",
  "Contact support to change the email address on your account."
)

keyword_retriever <- function(query, k) {
  words <- tolower(unlist(strsplit(query, "\\W+")))
  words <- words[nchar(words) > 3]
  shared <- function(passage) {
    sum(vapply(words, grepl, logical(1), x = tolower(passage), fixed = TRUE))
  }
  overlap <- vapply(policies, shared, numeric(1), USE.NAMES = FALSE)
  ranked <- order(overlap, decreasing = TRUE)
  head(policies[ranked[overlap[ranked] > 0]], k)
}

keyword_retriever("How long does standard delivery take?", k = 2)
#> [1] "Standard delivery takes three to five business days."                    
#> [2] "Express delivery arrives the next business day if you order before noon."
```

Give
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md)
a signature, the retriever and `k`. The instructions matter here: they
are the only place that tells the model to rely on the retrieved text
and what to do when it falls short.

``` r

faq_sig <- signature(
  "question -> answer",
  instructions = paste(
    "Answer in one sentence, using only the retrieved context.",
    "If the context does not contain the answer, say that you don't know."
  )
)

faq <- rag_module(faq_sig, retriever = keyword_retriever, k = 2L)
```

Only the model call needs an API key:

``` r

llm <- ellmer::chat_openai(model = "gpt-6-luna")
result <- run(
  faq,
  question = "How long do I have to return an unused item?",
  .llm = llm,
  .return_format = "structured"
)
result$output$answer
result$metadata$retrieved_context
```

For each call,
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md)
takes the query from the first input named `query`, `question`, `text`,
`input` or `prompt` (otherwise the first string input), calls
`retriever(query, k = k)`, numbers the passages `[1]`, `[2]`, and
appends them to the prompt under a `relevant_context:` heading. Rename
the heading with `context_format`. The module fills in the context
itself, so the signature only needs the question. Structured results
keep the `query` and the `retrieved_context` in `metadata`.

## Use a ragnar store

[ragnar](https://ragnar.tidyverse.org/) stores documents as chunks with
embeddings in DuckDB. Create a store with an embedding function, insert
chunked documents, and build the search index:

``` r

store <- ragnar::ragnar_store_create(
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small")
)
for (i in seq_along(policies)) {
  doc <- ragnar::MarkdownDocument(policies[[i]], origin = paste0("policy-", i))
  ragnar::ragnar_store_insert(store, ragnar::markdown_chunk(doc))
}
ragnar::ragnar_store_build_index(store)
```

ragnar saves the `embed` function with the store and runs it in other
sessions, so call functions inside it with `::`. For files or web pages,
`ragnar::read_as_markdown()` returns the same kind of document. The
store above lives in memory; pass a file path as the first argument of
`ragnar_store_create()` to keep it, and reopen it later with
`ragnar::ragnar_store_connect()`.

Pass the store instead of a retriever:

``` r

faq_store <- rag_module(faq_sig, store = store, k = 2L)
run(faq_store, question = "Where does my refund go?", .llm = llm)
```

The module calls `ragnar::ragnar_retrieve(store, query, top_k = k)`.
ragnar runs an embedding search and a BM25 keyword search, each for
`top_k` chunks, and merges the results, so a store can return more than
`k` passages.

## Let an agent search

A RAG module retrieves once, using its input as the query. An agent can
search several times and rephrase its queries.
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md)
turns a store into an ellmer tool for
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md):

``` r

search_policies <- ragnar_tool(
  store,
  k = 3L,
  name = "search_policies",
  description = "Search the store's delivery, return and account policies."
)

agent <- react(
  signature(
    "question -> answer",
    instructions = "Search the policies before answering, and answer from what you find."
  ),
  tools = list(search_policies)
)
run(
  agent,
  question = "I ordered at 9am. When will express delivery arrive, and can I still return it?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
```

The agent registers its tools on the chat it runs with, so give it a
chat of its own. `create_search_tool(documents, embedding_fn)` builds an
in-memory store from a character vector and returns the tool in one
step, which is convenient for small document sets.

## Evaluate a RAG module

Evaluation data needs the module’s input and the expected answer. Word
overlap works for short factual answers:

``` r

faq_tests <- tibble::tibble(
  question = c(
    "How long does standard delivery take?",
    "How long do I have to return an unused item?",
    "How do I change my email address?",
    "Do you ship to Canada?"
  ),
  answer = c(
    "Standard delivery takes three to five business days.",
    "You can return unused items within 30 days of purchase.",
    "Contact support to change your email address.",
    "I don't know."
  ),
  key_fact = c("three to five", "30 days", "support", "know")
)
```

``` r

evaluate(faq, faq_tests, metric = metric_f1(field = "answer"), .llm = llm)
```

`metric_f1(field = "answer")` scores the words that the prediction’s
`answer` shares with the `answer` column, so a correct answer in other
words still earns partial credit. When one fact decides correctness, a
custom metric can check for it. A metric receives the prediction and the
data row:

``` r

mentions_key_fact <- function(prediction, expected) {
  grepl(expected$key_fact, prediction$answer, ignore.case = TRUE)
}

mentions_key_fact(
  list(answer = "Returns are accepted within 30 days."),
  faq_tests[2, ]
)
#> [1] TRUE
```

``` r

evaluate(faq, faq_tests, metric = mentions_key_fact, .llm = llm)
```

The last row has no matching policy, so it tests whether the module
admits that it does not know. When a score is low, look at the retrieved
context before changing the prompt: a wrong answer from the right
passages and a wrong answer from the wrong passages need different
fixes.

## When retrieval fails

A retriever that finds nothing, or that errors, does not stop the module
by default. An error becomes a warning (“Retriever failed, continuing
with empty context”), and in both cases the model receives “No relevant
context found.” in place of passages, which is why the instructions
above say what to do without an answer. When an answer without context
is worse than no answer, set `fail_on_retrieval_error = TRUE` in
`config` so that retrieval errors stop the call. An empty result still
reaches the model.

``` r

flaky_search <- function(query, k) stop("search service unavailable")

strict <- rag_module(
  faq_sig,
  retriever = flaky_search,
  config = list(fail_on_retrieval_error = TRUE)
)
try(run(strict, question = "Can I return shoes I wore once?"))
#> Error in private$retrieve_context(query) : Retriever failed
#> ✖ search service unavailable
#> ℹ Set `config$fail_on_retrieval_error = FALSE` to continue with empty context
#> Caused by error in `self$retriever()`:
#> ! search service unavailable
```

The error comes from retrieval, before any request to the model.
