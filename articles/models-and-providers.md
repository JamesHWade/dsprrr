# Models, providers and streaming

dsprrr sends every request through an
[ellmer](https://ellmer.tidyverse.org) `Chat`, so it works with any
provider ellmer supports. This page shows how to pick the Chat a call
uses, set provider parameters, call reasoning models, send images,
expose a module as a tool, change timeouts, and get results
asynchronously or as a stream.

``` r

library(dsprrr)
```

## Choose the Chat for a call

Each call uses the first Chat it finds, in this order:

| Where the Chat comes from | How to set it | Applies to |
|----|----|----|
| The `.llm` argument | `run(mod, ..., .llm = chat)` | That call |
| The module’s own Chat | `module(sig, chat = chat)`, or `set_module_lm(program, chat)` for every module in a pipeline | Every call on that module |
| A scoped Chat | `with_lm(chat, { ... })`, or `local_lm(chat)` inside a function | Code in that block or function |
| The default Chat | `set_default_chat(chat)` or [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md) | The session |

If none is set, dsprrr creates a default Chat from the first API key it
finds: `OPENAI_API_KEY`, then `ANTHROPIC_API_KEY`, then
`GOOGLE_API_KEY`.
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)
shows the active default.

``` r

set_default_chat(ellmer::chat_openai(model = "gpt-6-luna"))
classify <- module(signature("text -> sentiment: enum('positive', 'negative')"))
reviews <- c("Arrived broken.", "Works great.")

run(classify, text = reviews) # default Chat

claude <- ellmer::chat_anthropic(model = "claude-sonnet-4-5")
run(classify, text = reviews, .llm = claude) # this call only
with_lm(claude, run(classify, text = reviews)) # every call in the block
```

A module’s own Chat takes precedence over
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md).
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md)
builds a Chat from a provider name, e.g.
`dsp_configure(provider = "openai", model = "gpt-6-luna")`, applies its
`temperature` argument through
[`ellmer::params()`](https://ellmer.tidyverse.org/reference/params.html),
and passes other arguments to the ellmer constructor.

A Chat keeps its conversation. Each single
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) call
adds its question and answer to the Chat it used (the `.llm` argument,
the module’s own Chat, a scoped Chat or the default), and later calls on
that Chat send the whole history. Batch rows,
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
and
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
start each row from a copy of the history at that moment and add nothing
to it; so does a module whose `config` sets request parameters. Over a
long session, input tokens grow, earlier exchanges can influence
answers, and cache keys change because they include the history. To keep
calls independent, create a new Chat for the task or clear one with
`chat$set_turns(list())`.

## Set provider parameters

Put sampling and output settings on the Chat with
[`ellmer::params()`](https://ellmer.tidyverse.org/reference/params.html).
ellmer translates them into each provider’s field names (for OpenAI,
`max_tokens` is sent as `max_output_tokens` and `reasoning_effort` as
`reasoning.effort`):

``` r

chat <- ellmer::chat_openai(
  model = "gpt-6-luna",
  params = ellmer::params(
    reasoning_effort = "none",
    temperature = 0,
    max_tokens = 500
  )
)
```

A module can also carry parameters in its `config`, as in
`module(sig, config = list(reasoning_effort = "low"))`;
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
applies the values it tries this way. Standard parameters go through
[`ellmer::params()`](https://ellmer.tidyverse.org/reference/params.html)
and are translated the same way; other names are sent as you give them.

## Reasoning models

Reasoning models work through a problem before they answer. gpt-6-luna
reasons by default; its `reasoning_effort` can be `"none"`, `"low"`,
`"medium"` (the default), `"high"`, `"xhigh"` or `"max"`. Three things
change when you use one:

- `temperature` and `top_p` apply only when reasoning is off. With
  gpt-6-luna, set `reasoning_effort = "none"` before you set or sweep
  them, and leave them unset otherwise. dsprrr sends whatever you set;
  it does not adjust parameters for reasoning models.
- Set the amount of thinking with `reasoning_effort`, on the Chat
  (`ellmer::params(reasoning_effort = )`) or in a module’s `config`,
  including a grid tried by
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).
  Either way it reaches OpenAI as `reasoning.effort`.
- The reasoning is not part of the result.
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
  returns the signature’s output fields; use
  [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
  if you want a `reasoning` field.

``` r

solver <- module(signature("problem -> answer: int"))
thinker <- ellmer::chat_openai(
  model = "gpt-6-luna",
  params = ellmer::params(reasoning_effort = "low")
)
run(
  solver,
  problem = "A farmer has 17 sheep. All but 9 run away. How many are left?",
  .llm = thinker
)
```

[`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md)
guesses from the name alone. It matches OpenAI’s o-series, `gpt-5` and
`gpt-6` names and any name containing “reasoning”:

``` r

models <- c("gpt-6-luna", "o4-mini", "claude-sonnet-4-5")
vapply(models, is_reasoning_model, logical(1))
#>        gpt-6-luna           o4-mini claude-sonnet-4-5 
#>              TRUE              TRUE             FALSE
```

Its only use in dsprrr is
[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md),
which suggests tunable parameters and swaps `temperature` and `top_p`
for `reasoning_effort` when the name looks like a reasoning model:

``` r

qa <- module(signature("question -> answer"))
knobs <- c("temperature", "top_p", "reasoning_effort")
module_parameters(qa, model = "claude-sonnet-4-5", include = knobs)
#> Collection of 2 parameters for tuning
#> 
#>   identifier        type    object
#>  temperature temperature dparam[+]
#>        top_p       top_p dparam[+]
#> 
module_parameters(qa, model = "gpt-6-luna", include = knobs)
#> Collection of 1 parameters for tuning
#>        identifier             type    object
#>  reasoning_effort reasoning_effort dparam[+]
#> 
```

## Images and other content

Pass ellmer content objects as inputs. dsprrr writes the other inputs
into the prompt text and sends each content object after it:

``` r

read_receipt <- module(signature("receipt -> merchant: str, total: float"))
chat <- ellmer::chat_openai(model = "gpt-6-luna")

run(
  read_receipt,
  receipt = ellmer::content_image_url("https://example.com/receipts/0901.jpg"),
  .llm = chat
)
```

A list of content objects is a batch, one call per element:

``` r

receipts <- list(
  ellmer::content_image_url("https://example.com/receipts/0901.jpg"),
  ellmer::content_image_url("https://example.com/receipts/0903.jpg")
)
totals <- run(read_receipt, receipt = receipts, .llm = chat)
totals[[1]]$total
```

The result has one named list per image.
[`ellmer::content_image_file()`](https://ellmer.tidyverse.org/reference/content_image_url.html)
and
[`ellmer::content_pdf_url()`](https://ellmer.tidyverse.org/reference/content_pdf_file.html)
build other kinds of content; the model must accept the kind you send.

## Use a module as a tool

[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md)
wraps a module as an ellmer tool, so a chat can call it in the middle of
a conversation. The tool runs the module on its own Chat (`.llm`), not
on the conversation that calls it:

``` r

sentiment <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
)
sentiment_tool <- as_ellmer_tool(
  sentiment,
  name = "classify_sentiment",
  description = "Classify the sentiment of a customer review.",
  output = "json",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)

assistant <- ellmer::chat_openai(model = "gpt-6-luna")
assistant$register_tool(sentiment_tool)
assistant$chat("Is this review positive? 'I love this product!'")
```

`output = "json"` hands the result back as JSON text. The default,
`"auto"`, returns an R list, which ellmer 0.5.0 converts with a
deprecation warning.

A prediction module’s tool is marked read-only and closed-world, because
it only sends its inputs to its Chat’s provider. Agent runtimes that
check tool annotations use this:
[deputy](https://jameshwade.github.io/deputy/), for example, runs the
tool in its default `"standard"` permission mode without web access, and
in `"readonly"` mode when you list it in `tool_allowlist`. Tools from
modules that can run your functions or tools, such as
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
or [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
carry no annotations unless you pass them.

## Run modules through an agent runtime

An agent runtime that follows ellmer’s Chat protocol can be the Chat a
module uses. With a deputy `Agent`, each call is one of the agent’s
runs, so its permissions, hooks and usage limits apply, and its system
prompt frames the request.
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
and the optimizers run each row on an independent copy of the agent, as
they do with any Chat:

``` r

agent <- deputy::Agent$new(
  chat = ellmer::chat_openai(model = "gpt-6-luna"),
  system_prompt = "Classify customer reviews for the support team.",
  usage_limits = deputy::UsageLimits(max_requests = 3)
)
result <- run(
  classify,
  text = "Arrived broken.",
  .llm = agent,
  .trace_context = list(request_id = "r-42"),
  .return_format = "structured"
)
result$metadata$agent_run
```

dsprrr passes the program’s ID and your `.trace_context` to the agent
under `run_context$dsprrr`, and records the agent’s run in the call’s
metadata and trace as `agent_run` (its `run_id`, `agent_id`,
`session_id` and `stop_reason`), so each trace can be matched with the
agent’s own records. Responses from an agent are never cached, because
the agent’s hooks and limits must see every call.

## Timeouts and retries

ellmer applies both. Set them with options before you make calls:

``` r

options(
  ellmer_timeout_s = 600, # default 300 seconds
  ellmer_max_tries = 5 # default 3
)
```

`run_dataset(.return_format = "structured")` records a failed row in the
`.error` column and carries on. Per-row time limits for batches come
from
[`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md);
see [Run dsprrr in
pipelines](https://jameshwade.github.io/dsprrr/articles/orchestration.md).

## Async calls

[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
returns a promise that resolves to the named list
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) would
return:

``` r

library(promises)
qa <- module(signature("question -> answer"))

run_async(
  qa,
  question = "What is the capital of Peru?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
) |>
  then(\(result) cat(result$answer, "\n"))
```

Promises resolve when R is idle: at the console, in Shiny, or in a
script after `while (!later::loop_empty()) later::run_now()`.
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
calls the provider directly, so it skips the response cache and records
no trace. It accepts modules built with
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) or
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md);
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md) and
most other specialized modules raise an error. Give each concurrent call
its own Chat, because ellmer reads each answer from the Chat’s last turn
and calls sharing a Chat can swap answers. For many inputs, a batch
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) with
`.concurrency` is simpler.

## Streaming

[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
passes output to callbacks as it arrives. Attach a
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)
to an output field:

``` r

writer <- module(signature("topic -> essay"))

run_stream(
  writer,
  topic = "Why rivers meander",
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  listeners = stream_listener("essay", \(chunk) cat(chunk))
)
```

When the signature has one string output and the coro package is
installed, dsprrr asks for plain text and hands each chunk to the
listener; the full text becomes the field’s value. Otherwise the module
runs normally and the listener fires once with the finished value.
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
returns the result invisibly, skips the cache and records no trace. An
`on_status` callback receives step and field events, one set per step
for a pipeline.

[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)
returns a coro async generator of text chunks, not a promise. Read it
inside [`coro::async()`](https://coro.r-lib.org/reference/async.html):

``` r

library(coro)
chunks <- stream_async(
  writer,
  topic = "Why rivers meander",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
print_all <- async(function() {
  for (chunk in await_each(chunks)) cat(chunk)
})
print_all()
```

To run many calls at once and track what they cost, continue with [Run
dsprrr in
pipelines](https://jameshwade.github.io/dsprrr/articles/orchestration.md).
