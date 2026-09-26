# Quick reference

Copyable snippets for everyday dsprrr work, grouped by task. Chunks that
need no model ran when this page was built, so their output is real.
Chunks that call a model show the code only. When a call fails, look up
the message in
[Troubleshooting](https://jameshwade.github.io/dsprrr/articles/troubleshooting.md).

``` r

library(dsprrr)
```

| Task | Functions | Returns |
|----|----|----|
| Describe the task | [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md) | a `Signature` |
| Build a program | [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md), [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md), [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md), … | a module |
| Run it | [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md), [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md), [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md), [`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md) | a named list, a tibble, a promise, the final output |
| Score it | [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md) with a metric | a list with `mean_score`, `scores`, `n_errors` |
| Improve it | [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md) with a teleprompter | a new module (the input is unchanged) |
|  | [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) | the same module, changed in place |
| Look inside | [`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md), [`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md), [`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md), [`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md) | a prompt record, a tibble, summary lists |
| Keep it | [`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md), [`load_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md), [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md) | a file or pin, a module |

## Configure a model

``` r

qa <- module(signature("question -> answer"))

# Pass a chat to one call
chat <- ellmer::chat_openai(model = "gpt-6-luna")
run(qa, question = "What is the capital of France?", .llm = chat)

# Or set a default for the session
dsp_configure(provider = "openai", model = "gpt-6-luna") # or "anthropic", "google"
set_default_chat(ellmer::chat_anthropic(model = "claude-sonnet-4-5")) # any ellmer chat
run(qa, question = "What is the capital of France?")

# Use a chat only inside a block or a function
with_lm(chat, run(qa, question = "What is the capital of France?"))
ask <- function(question) {
  local_lm(chat) # reverts when ask() returns
  run(qa, question = question)
}

dsprrr_sitrep() # default chat, API keys, cache status
```

Called without arguments,
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md)
uses the first key it finds among `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`
and `GOOGLE_API_KEY`. When several chats are set, dsprrr uses the first
of: the `.llm` argument, the module’s own chat
(`module(sig, chat = chat)`),
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)
or
[`local_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md),
and the default. [Models, providers and
streaming](https://jameshwade.github.io/dsprrr/articles/models-and-providers.md)
covers providers in detail.

Sampling parameters can be set for the default chat, on a chat you
build, or for one module. gpt-6-luna reasons by default and accepts
`temperature` only with `reasoning_effort = "none"`.

``` r

dsp_configure(
  provider = "openai",
  model = "gpt-6-luna",
  temperature = 0,
  params = ellmer::params(reasoning_effort = "none")
)
chat <- ellmer::chat_openai(
  model = "gpt-6-luna",
  params = ellmer::params(reasoning_effort = "none", temperature = 0)
)
qa_quick <- module(
  signature("question -> answer"),
  config = list(reasoning_effort = "low")
)
```

## Define signatures

``` r

sig <- signature(
  "review: str, stars: int -> sentiment: enum('positive', 'negative', 'neutral'), confidence: float",
  instructions = "Classify the sentiment of a product review."
)
sig
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • review: "string" - Input: review
#> • stars: "integer" - Input: stars
#> 
#> ── Output
#> Type: "object(sentiment: enum(positive, negative, neutral), confidence:
#> number)"
#> 
#> ── Instructions
#> Classify the sentiment of a product review.
```

| Write | Meaning |
|----|----|
| `a, b -> x, y` | inputs `a` and `b`, outputs `x` and `y` |
| `answer` | a string (fields without a type are strings) |
| `name: str` or `string` | string |
| `n: int` or `integer` | integer |
| `score: float` or `number` | number |
| `flag: bool` or `boolean` | `TRUE` or `FALSE` |
| `label: enum('a', 'b')` or `Literal['a', 'b']` | one of the listed values |
| `note: Optional[str]` | a field that may be left out |
| `tags: list[str]`, `array(str)` or `str[]` | a vector of values of the inner type |
| `counts: dict[str, int]` | an object with free-form keys (value types are not enforced) |

Bounds such as `number[0, 100]` parse as a plain number: the bounds are
dropped, so check ranges in R.
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
also ignores arguments it does not know, so a misspelled `instructions`
disappears without a warning.

For descriptions or nested outputs, build the signature from
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md) and
ellmer types:

``` r

review_sig <- signature(
  inputs = list(input("review", description = "Customer review text")),
  output_type = ellmer::type_object(
    sentiment = ellmer::type_enum(c("positive", "negative", "neutral")),
    aspects = ellmer::type_array(
      ellmer::type_string(),
      description = "Product aspects the review mentions"
    )
  ),
  instructions = "Classify the review and list the aspects it mentions."
)
```

## Build modules

| Constructor | Use it when |
|----|----|
| `module(sig)` | one structured prediction per call |
| `chain_of_thought(sig)` | the model should reason first; adds a `reasoning` output |
| `multi_chain_comparison(sig, M = 3L)` | several reasoning attempts are compared and merged |
| `react(sig, tools = list(...))` | the model calls R functions as tools |
| `program_of_thought(sig, interpreter_factory = r_code_runner)` | the model writes R code whose result answers the question |
| `code_act(sig, tools = list(...), interpreter_factory = r_code_runner)` | tools and R code in one agent |
| `rlm_module(sig, interpreter_factory = ...)` | the model explores large inputs in an R session |
| `flex(sig)` | experimental: an optimizer may rewrite the whole program |
| `module_fn(sig, forward)` | plain R code with the module interface |
| `best_of_n(mod, N = 5L, reward_fn = f)` | sample several answers and keep the one with the highest reward |
| `refine(mod, N = 3L, reward_fn = f)` | retry with feedback until the reward passes |
| `ensemble(list(mod_a, mod_b))` | vote across modules |
| `with_assertions(mod, list(...))` | retry when checks on the output fail |
| [`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md), `%>>%`, [`step()`](https://jameshwade.github.io/dsprrr/reference/step.md) | chain modules |

[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
takes a `Signature` object; the other constructors also accept a
signature string. [Choose a module
type](https://jameshwade.github.io/dsprrr/articles/advanced-modules.md)
compares them.

``` r

qa <- module(signature("question -> answer"))
cot <- chain_of_thought("question -> answer")
mcc <- multi_chain_comparison("question -> answer", M = 3L)

get_weather <- function(city) paste("Sunny in", city)
weather_tool <- ellmer::tool(
  get_weather,
  description = "Get today's weather for a city.",
  arguments = list(city = ellmer::type_string("City name"))
)
agent <- react("question -> answer", tools = list(weather_tool))

# r_code_runner() (needs callr) runs code in a separate R process.
# It is not a sandbox: use it only with trusted input.
pot <- program_of_thought("question -> answer", interpreter_factory = r_code_runner)
actor <- code_act(
  "question -> answer",
  tools = list(weather_tool),
  interpreter_factory = r_code_runner
)
analyst <- rlm_module(
  "document, question -> answer",
  interpreter_factory = function() r_code_runner(persistent = TRUE)
)

word_count <- module_fn(
  "text -> n_words: int",
  function(text) list(n_words = lengths(strsplit(text, "\\s+")))
)
run(word_count, text = "Signatures describe the task")
#> $n_words
#> [1] 4

under_100 <- function(prediction, inputs) as.numeric(nchar(prediction$answer) <= 100)
best <- best_of_n(qa, N = 5L, reward_fn = under_100)
refined <- refine(qa, N = 3L, reward_fn = under_100)
voted <- ensemble(list(qa, cot), reduce_fn = reduce_majority(field = "answer"))
checked <- with_assertions(
  qa,
  list(assert_output(~ nchar(.x$answer) <= 100, "Keep the answer under 100 characters."))
)

program <- flex("question -> answer")
#> Warning: `flex()` is experimental and its module source schema may change
#> ℹ The default source is declarative JSON; executable R source requires an
#>   explicit interpreter factory.
```

Pipelines pass outputs forward by name. The first step receives the
pipeline inputs; every later step receives only the previous step’s
outputs plus the fixed inputs given to
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md). In
`map`, the names are upstream fields and the values are this step’s
inputs. `select` keeps only the named output fields of a step. See
[Chain modules into
pipelines](https://jameshwade.github.io/dsprrr/articles/chaining-modules.md).

``` r

summarize <- module(signature("article -> summary"))
headline <- module(signature("summary -> headline"))
news <- summarize %>>% headline # `summary` flows into the second step

explain <- module(signature("text, audience -> explanation"))
for_kids <- pipeline(
  summarize,
  step(explain, map = c(summary = "text"), audience = "children")
)
run(for_kids, article = "The council approved a bike lane on Main Street.", .llm = chat)
```

## Run

``` r

reviews <- tibble::tibble(
  text = c("I love it", "Arrived broken", "Great value", "Not great, returned it"),
  sentiment = c("positive", "negative", "positive", "negative")
)
```

``` r

classifier <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
)

# One call returns a named list
result <- run(classifier, text = "I love it", .llm = chat)
result$sentiment

# Vector inputs run as a batch: one named list per element
run(classifier, text = c("I love it", "Arrived broken"), .llm = chat)

# A data frame, row by row: the input columns plus a `result` list-column
run_dataset(classifier, reviews, .llm = chat)
run_dataset(classifier, reviews, .llm = chat, .return_format = "structured") # adds .error, .metadata, .chat

# Per-call options
run(classifier, text = "I love it", .llm = chat, .cache = FALSE) # skip the cache
run(classifier, text = "I love it", .llm = chat, .return_format = "structured") # output, chat, metadata
run_dataset(classifier, reviews, .llm = chat, .concurrency = concurrency_control(max_active = 4L))

# Asynchronous: a promise that resolves to the named list
run_async(classifier, text = "I love it", .llm = chat) |>
  promises::then(\(result) print(result$sentiment))

# Streaming: callbacks receive text as it arrives
run_stream(
  qa,
  question = "Explain recursion in two sentences.",
  .llm = chat,
  listeners = stream_listener("answer", \(chunk) cat(chunk))
)
```

Token streaming needs the coro package and a module with a single string
output; other modules run normally and the listener fires once with the
full value.

## Evaluate

A metric is called as `metric(prediction, expected)`. Most built-in
metrics compare one field, and `field` names both the output field and
the data column:

``` r

prediction <- list(sentiment = "positive", answer = "Paris is the capital of France.")
expected <- list(sentiment = "Positive", answer = "Paris, France")

metric_exact_match(field = "sentiment")(prediction, expected)
#> [1] FALSE
metric_exact_match(field = "sentiment", ignore_case = TRUE)(prediction, expected)
#> [1] TRUE
metric_f1(field = "answer")(prediction, expected)
#> [1] 0.5
metric_threshold(metric_f1(field = "answer"), threshold = 0.8)(prediction, expected)
#> [1] FALSE
metric_contains("Paris", field = "answer")(prediction, expected) # ignores `expected`
#> [1] TRUE
```

Inside
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
`prediction` is the run result and `expected` is the data row, so a
custom metric reads fields from both:

``` r

within_one_star <- function(prediction, expected) {
  abs(prediction$stars - expected$stars) <= 1
}
within_one_star(list(stars = 4L), tibble::tibble(stars = 5L))
#> [1] TRUE
```

A
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
baseline scores without a model:

``` r

keyword_baseline <- module_fn("text -> sentiment", function(text) {
  positive <- grepl("love|great", text, ignore.case = TRUE)
  list(sentiment = if (positive) "positive" else "negative")
})
baseline <- evaluate(
  keyword_baseline,
  reviews,
  metric = metric_exact_match(field = "sentiment")
)
baseline$mean_score
#> [1] 0.75
baseline$scores
#> [1] 1 1 1 0
```

``` r

result <- evaluate(
  classifier,
  reviews,
  metric = metric_exact_match(field = "sentiment"),
  .llm = chat
)
result$mean_score
result$n_errors # rows where the call or the metric failed; they score 0
```

The full list of metrics, including
[`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
[`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md)
and
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md),
is in [Metrics and
evaluation](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md).
To score with vitals, see [Evaluate with
vitals](https://jameshwade.github.io/dsprrr/articles/vitals-recipes.md).

## Optimize

`trainset` below is a data frame with the same columns as `reviews`,
usually a few dozen rows or more.

``` r

# compile() returns a new module; `classifier` is unchanged
few_shot <- classifier |> compile(LabeledFewShot(k = 4L), trainset)
bootstrapped <- classifier |>
  compile(
    BootstrapFewShot(
      metric = metric_exact_match(field = "sentiment"),
      max_bootstrapped_demos = 4L
    ),
    trainset,
    .llm = chat
  )

# optimize_grid() changes the module in place
optimize_grid(
  classifier,
  data = trainset,
  metric = metric_exact_match(field = "sentiment"),
  parameters = list(
    reasoning_effort = c("none", "low"),
    instructions_suffix = c("", "Reply with the label only.")
  ),
  .llm = chat
)
module_trials(classifier)
```

Integer settings need an `L` suffix: `k = 4L` works, `k = 4` is an
error.
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
does not accept
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
programs.

| Teleprompter | What it changes | Example |
|----|----|----|
| `LabeledFewShot` | demos: `k` training rows drawn at random (seeded); no model calls | `LabeledFewShot(k = 4L)` |
| `BootstrapFewShot` | demos: runs the program on training rows and keeps those that pass the metric; handles pipelines jointly | `BootstrapFewShot(metric = m, max_bootstrapped_demos = 4L)` |
| `BootstrapFewShotWithRandomSearch` | demos: several bootstrapped candidates, keeps the best | `BootstrapFewShotWithRandomSearch(metric = m, num_candidate_programs = 8L)` |
| `KNNFewShot` | demos picked per input by embedding similarity | `KNNFewShot(k = 3L, vectorizer = embed)` |
| `GridSearchTeleprompter` | tries instruction or template variants (instructions are appended) | `GridSearchTeleprompter(variants = variants, metric = m)` |
| `COPRO` | instructions, by coordinate ascent over model-written candidates | `COPRO(metric = m, breadth = 5L, depth = 2L)` |
| `MIPROv2` | instructions and demos, chosen with a UCB1 bandit | `MIPROv2(metric = m, auto = "light")` |
| `SIMBA` | rules and demos learned from hard examples (simplified) | `SIMBA(metric = m, max_steps = 4L)` |
| `GEPA` | instructions or Flex source, by reflecting on failures; reads [`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md) feedback | `GEPA(metric = m, generations = 5L)` |
| `BetterTogether` | runs several teleprompters in sequence | `BetterTogether(metric = m, p = BootstrapFewShot(metric = m), g = GEPA(metric = m), default_strategy = "p -> g")` |
| `Omni` | experimental: explores several teleprompters and continues from the best | `Omni(metric = m, explorers = list(...), continuation = GEPA(metric = m))` |
| `ReAnchor` | experimental: fits the thresholds, cuts and weights of [`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md) outputs | `ReAnchor(metric = m)` |

[Choose an
optimizer](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md)
explains when to use each. The experimental
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
and
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)
are covered in [Agentic optimization
harnesses](https://jameshwade.github.io/dsprrr/articles/agentic-optimization-harnesses.md).

## Inspect

``` r

get_last_prompt() # the last prompt and response, with model, tokens and cost
inspect_history(n = 5) # a tibble of the last five calls
summarize_traces(classifier) # calls, tokens, cost and latency for one module
export_traces(classifier, include_prompts = TRUE, include_outputs = TRUE)
session_cost() # tokens and estimated cost for the whole session
```

For a debugging routine and the meaning of common errors, see
[Troubleshooting](https://jameshwade.github.io/dsprrr/articles/troubleshooting.md).

## Cache

Identical requests (same model, parameters, prompt and output type) are
answered from a memory and disk cache. The first cache hit in a session
prints “Using cached LLM responses”.

``` r

configure_cache(enable_disk = FALSE) # memory only, for this session
cache_stats()
#> 
#> ── dsprrr Cache Statistics
#> • Hit rate: 0%
#> • Hits: 0
#> • Misses: 0
clear_cache()
```

``` r

run(qa, question = "Suggest a name for a cat.", .llm = chat, .cache = FALSE) # one call
configure_cache(enable = FALSE) # the rest of the session
```

In CI, set the environment variable `DSPRRR_CACHE_ENABLED=false`. The
disk cache holds prompts and responses, so dsprrr uses it only when the
directory is private to you; see
[Troubleshooting](https://jameshwade.github.io/dsprrr/articles/troubleshooting.md)
if it is rejected.

## Save and reload

``` r

save_program(few_shot, "sentiment-classifier.rds")
classifier <- load_program("sentiment-classifier.rds")

board <- pins::board_folder("pins")
pin_module_config(board, "sentiment-classifier", few_shot)
classifier <- restore_module_config(pins::pin_read(board, "sentiment-classifier"))

use_dsprrr_template("targets", path = "my_project") # a targets pipeline to start from
```

Saved programs keep the signature, demos and configuration. Chats are
never saved, so pass `.llm` or set a default after loading; tools and
other R functions need a `registry` (see
[`?save_program`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md)).
[Tutorial 6: Save and reuse a
module](https://jameshwade.github.io/dsprrr/articles/tutorial-deploy-to-production.md)
walks through the full workflow.
