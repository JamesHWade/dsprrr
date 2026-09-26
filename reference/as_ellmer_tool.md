# Turn a module into an ellmer tool

`as_ellmer_tool()` wraps a module as an ellmer tool, so a Chat, or a
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md)
agent, can call it during a conversation. The tool's arguments are the
module's input fields, with their types and descriptions.

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

  A module, such as one created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  or
  [`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md).

- name:

  Tool name. Defaults to `dsprrr_` followed by the input field names.

- description:

  Tool description for the model. Defaults to the signature's
  instructions.

- .llm:

  The chat the module uses when the tool is called. With `NULL`, the
  module's own chat or the default chat (see
  [`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md)).

- annotations:

  A list of ellmer tool annotations, passed to
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html),
  for example to mark the tool read-only.

- output:

  How the tool returns its result:

  - `"auto"` (the default): the module's output with fields in signature
    order.

  - `"json"`: compact JSON with fields in signature order.

  - `"text"`: the main output field as text if possible, otherwise JSON.

  - `"raw"`: the module's output unchanged.

- copy:

  `"none"` (the default) calls `module` itself, so its traces
  accumulate; `"deep"` calls a fresh deep copy each time.

- error:

  What happens when the module fails:

  - `"reject"` (the default): the tool returns an error description the
    model can read and react to. Its `$type` holds the condition class.

  - `"abort"`: the error propagates to the caller.

  - `"return"`: a `dsprrr_tool_error` condition carrying the error
    description in `$payload` is signalled, which a
    [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
    handler can inspect.

- trace_context:

  A named, JSON-compatible list recorded in the metadata and traces of
  every call made through the tool, as in
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

## Value

An ellmer tool definition (`ToolDef`) for `Chat$register_tool()` or the
`tools` argument of
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md). It
can also be called directly with the input fields as arguments.

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md),
[`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md),
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md),
[`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md),
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
shout <- module_fn("text -> reply", function(text) toupper(text))
shout_tool <- as_ellmer_tool(shout, name = "shout", description = "Upper-case text.")
shout_tool(text = "quiet please")
#> $reply
#> [1] "QUIET PLEASE"
#> 

if (FALSE) { # \dontrun{
sentiment <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
)
sentiment_tool <- as_ellmer_tool(sentiment, name = "analyze_sentiment")

chat <- ellmer::chat_openai(model = "gpt-6-luna")
chat$register_tool(sentiment_tool)
chat$chat("What is the sentiment of: 'I love this product!'")
} # }
```
