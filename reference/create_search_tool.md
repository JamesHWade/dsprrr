# Build a search tool from a set of documents

`create_search_tool()` builds a ragnar store from documents (one chunked
document per element, embedded with `embedding_fn`), indexes it and
wraps it with
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md),
in one step. Embedding the documents calls the embedding provider.

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

  A character vector of documents, or a data frame with a `text` or
  `content` column.

- embedding_fn:

  An embedding function, passed to `ragnar::ragnar_store_create()` as
  `embed`, for example
  `\(x) ragnar::embed_openai(x, model = "text-embedding-3-small")`.

- k:

  Number of chunks to return per search.

- name:

  Tool name shown to the model.

- description:

  Tool description shown to the model; see
  [`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md).

- ...:

  Passed to `ragnar::ragnar_store_create()`, for example `location` to
  keep the store in a file instead of in memory.

## Value

An ellmer tool definition, as returned by
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md).

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
[`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md),
[`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md),
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md),
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md),
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md),
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
# Create tool directly from documents
docs <- c(
  "R is a programming language for statistical computing.",
  "Python is a general-purpose programming language.",
  "Julia is designed for high-performance numerical computing."
)

search_tool <- create_search_tool(
  documents = docs,
  embedding_fn = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small"),
  k = 2
)

# Use the tool
search_tool("What language is best for statistics?")
} # }
```
