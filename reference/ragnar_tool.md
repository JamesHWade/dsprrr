# Search a ragnar store from a tool-using module

`ragnar_tool()` wraps a ragnar document store as an ellmer tool that
takes a search query and returns the `k` best-matching chunks as text.
Give it to
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md) so
the agent can search your documents while it reasons, or register it on
an ellmer Chat.
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md)
is the alternative that retrieves once, before every call.

## Usage

``` r
ragnar_tool(store, k = 5L, name = "search_knowledge", description = NULL)
```

## Arguments

- store:

  A ragnar store, built with `ragnar::ragnar_store_create()`,
  `ragnar::ragnar_store_insert()` and
  `ragnar::ragnar_store_build_index()`.

- k:

  Number of chunks to return per search.

- name:

  Tool name shown to the model.

- description:

  Tool description shown to the model. The default says it searches the
  knowledge base and returns the top `k` documents.

## Value

An ellmer tool definition (see
[`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html))
for [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md)
modules or `Chat$register_tool()`. It can also be called directly with a
query string.

## Details

Results are numbered (`[Result 1]`, `[Result 2]`, ...) and separated by
`---`. A search error is returned to the model as text instead of
stopping the conversation. Requires the ragnar package.

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
[`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md),
[`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md),
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md),
[`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md),
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md),
[`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md),
[`register_dsprrr_engine()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_engine.md),
[`summarize_traces_df()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces_df.md),
[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
[`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a ragnar store from documents
library(ragnar)
store <- ragnar_store_create(
  embed = \(x) embed_openai(x, model = "text-embedding-3-small")
)
for (i in seq_along(my_docs)) {
  doc <- MarkdownDocument(my_docs[[i]], origin = paste0("doc-", i))
  ragnar_store_insert(store, markdown_chunk(doc))
}
ragnar_store_build_index(store)

# Create a search tool
search_tool <- ragnar_tool(store, k = 3L)
search_tool("How do I configure the cache?")

# Use with a ReAct module
agent <- react("question -> answer", tools = list(search_tool))
run(agent, question = "How do I turn caching off?", .llm = ellmer::chat_openai(model = "gpt-6-luna"))

# Or register it on an ellmer Chat
chat <- ellmer::chat_openai(model = "gpt-6-luna")
chat$register_tool(search_tool)
} # }
```
