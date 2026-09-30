# Troubleshooting

Find the message you got, read why it happens, and apply the fix below
it. The messages on this page are real: the page was built in an R
session without API keys, and each error was produced with
[`try()`](https://rdrr.io/r/base/try.html) while rendering it.

## Check your setup with `dsprrr_sitrep()`

[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)
reports package versions, the default chat, which API keys are set, and
the state of the response cache. This is its output in the session that
built this page:

``` r

library(dsprrr)
```

``` r

dsprrr_sitrep()
#> 
#> ── dsprrr configuration ────────────────────────────────────────────────────────
#> 
#> ── Packages ──
#> 
#> ✔ ellmer 0.5.0 (OK)
#> ✔ dsprrr 0.0.0.9000
#> 
#> ── Default Chat ──
#> 
#> ✖ Not configured
#> Run `dsp_configure()` or set an API key
#> 
#> ── API Keys ──
#> 
#> ✖ OPENAI_API_KEY
#> ✖ ANTHROPIC_API_KEY
#> ✖ GOOGLE_API_KEY
#> 
#> ── Session State ──
#> 
#> • Prompt history: 0 / 100 entries
#> 
#> ── Options ──
#> 
#> Using defaults (no options set)
#> 
#> ── Cache ──
#> 
#> ✔ Cache tiers: memory, disk
#> • Disk path: /home/runner/.cache/R/dsprrr
#> ℹ Private disk permissions will be checked on first use
#> ℹ No cache activity yet
```

## No chat or API key

### “No default Chat available”

``` r

qa <- module(signature("question -> answer"))
try(run(qa, question = "What is the capital of France?"))
#> Error in get_default_chat(create = TRUE) : 
#>   No default Chat available
#> ℹ Set up a default Chat using one of these methods:
#>   1. Set an API key environment variable:
#>   `Sys.setenv(OPENAI_API_KEY = 'your-key')`
#>   `Sys.setenv(ANTHROPIC_API_KEY = 'your-key')`
#>   2. Set a default Chat explicitly:
#>   `options(dsprrr.default_chat = ellmer::chat_openai())`
#>   3. Pass a Chat to `run()`:
#>   `run(module(signature('q -> a')), q = 'Hello', .llm = chat)`
```

[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) needs an
ellmer chat. Pass one with `.llm`, or set a default once per session:

``` r

chat <- ellmer::chat_openai(model = "gpt-6-luna")
run(qa, question = "What is the capital of France?", .llm = chat)

set_default_chat(chat)
run(qa, question = "What is the capital of France?")
```

When `OPENAI_API_KEY`, `ANTHROPIC_API_KEY` or `GOOGLE_API_KEY` is set,
dsprrr creates a default chat for that provider on first use. With none
of them set,
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md)
cannot guess either:

``` r

try(dsp_configure())
#> Error in dsp_configure() : Could not auto-detect provider
#> ℹ Set an API key environment variable or specify `provider`
```

### “Can’t find env var `OPENAI_API_KEY`”

``` r

chat <- ellmer::chat_openai(model = "gpt-6-luna")
try(run(qa, question = "What is the capital of France?", .llm = chat))
#> Warning: Cannot safely identify this LLM request; bypassing the cache
#> ✖ Failed to resolve credential/account identity Caused by error in
#>   `openai_key()`: ! Can't find env var `OPENAI_API_KEY`.
#> ℹ The request will still be sent normally.
#> This warning is displayed once per session.
#> Error in openai_key() : Can't find env var `OPENAI_API_KEY`.
```

ellmer creates the chat without checking for a key, so the error appears
at the first request. The warning comes from dsprrr’s cache, which also
needs the key to tell accounts apart; it goes away once the key is set.

Store the key in `~/.Renviron` (`usethis::edit_r_environ()` opens it) as
a line such as `OPENAI_API_KEY=sk-...`, then restart R. Each ellmer
provider reads its own variable, for example `ANTHROPIC_API_KEY` for
[`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html).
A key that is set but invalid or revoked reaches the provider and fails
with an HTTP 401 error instead.

### “Unknown provider”

``` r

try(dsp_configure(provider = "mistral"))
#> Error in dsp_configure(provider = "mistral") : 
#>   Unknown provider: "mistral"
#> ℹ Valid providers: "openai", "anthropic", and "google"
```

[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md)
builds chats for three providers. For any other provider ellmer
supports, create the chat yourself and make it the default:

``` r

set_default_chat(ellmer::chat_mistral())
```

[Models, providers and
streaming](https://jameshwade.github.io/dsprrr/articles/models-and-providers.md)
covers provider setup in detail.

## Signature errors

### “Invalid arrow in signature” and other parsing errors

``` r

try(signature("question => answer"))
#> Error in parse_signature(x, instructions) : 
#>   Invalid arrow in signature
#> ✖ You provided: "question => answer"
#> ℹ Use `->` not `=>`
#> ℹ Corrected: `question -> answer`
try(signature("question answer"))
#> Error in parse_signature(x, instructions) : 
#>   Missing `->` separator in signature
#> ✖ You provided: "question answer"
#> ℹ Did you mean: `'question -> answer'`
try(signature("question -> reasoning -> answer"))
#> Error in parse_signature(x, instructions) : 
#>   Multiple `->` separators found in signature
#> ✖ You provided: "question -> reasoning -> answer"
#> ℹ Use exactly one `->` to separate inputs from outputs
#> ℹ Example: `'input1, input2 -> output'`
```

A signature has exactly one `->`, with inputs on the left and outputs on
the right, each separated by commas. To have the model reason before
answering, use
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
which adds the `reasoning` output for you:

``` r

signature("question -> reasoning, answer")
chain_of_thought("question -> answer")
```

### “Unknown type”

``` r

try(signature("review -> score: integr"))
#> Error in validate_type_string(type_str) : 
#>   Unknown type: "integr"
#> ℹ Did you mean `integer`?
#> ℹ Available simple types: `string`, `number`, `integer`, `boolean`
#> ℹ Available complex types: `enum('a', 'b')`, `array(string)`, `object`
```

The types are `str`, `int`, `float`, `bool`, `enum(...)` or
`Literal[...]`, `Optional[...]`, `list[...]` and `dict[...]`. The [Quick
reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.html#define-signatures)
has the full grammar.

### Instructions or bounds are ignored

``` r

sig <- signature("question -> answer", instrctions = "Answer in one word.")
sig@instructions
#> [1] "Given the fields `question`, produce the fields `answer`."
```

[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
silently drops arguments it does not recognize, so the misspelled
`instrctions` never reaches the prompt and the default instruction is
used. Bounds are dropped the same way: `score: number[0, 100]` is an
unbounded number, so check ranges in R or with
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md).

## Module errors

### “`module()` creates standard prediction modules and does not accept …”

``` r

try(module(signature("question -> answer"), type = "react"))
#> Error in reject_module_arguments(...) : 
#>   `module()` creates standard prediction modules and does not accept
#> `type`.
#> ℹ Choose the constructor directly: react(), chain_of_thought(),
#>   multi_chain_comparison(), program_of_thought(), code_act(), rlm_module(), or
#>   flex().
try(module(signature("question -> answer"), temperature = 0))
#> Error in reject_module_arguments(...) : 
#>   `module()` creates standard prediction modules and does not accept
#> `temperature`.
#> ℹ Use multi_chain_comparison() for multiple reasoning chains. For ordinary
#>   model temperature, use config = list(temperature = ...).
```

[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
builds one kind of module, a single prediction. The error has class
`dsprrr_module_argument_error`. The old `type` argument became separate
constructors:

| Instead of | Use |
|----|----|
| `module(sig, type = "react")` | `react(sig, tools = list(...))` |
| `module(sig, type = "chain_of_thought")` | `chain_of_thought(sig)` |
| `module(sig, type = "multichain")` | `multi_chain_comparison(sig)` |
| `module(sig, type = "program_of_thought")` | `program_of_thought(sig, interpreter_factory = r_code_runner)` |
| `module(sig, type = "codeact")` | `code_act(sig, tools = list(...), interpreter_factory = r_code_runner)` |
| `module(sig, type = "rlm")` | `rlm_module(sig, interpreter_factory = ...)` |
| `module(sig, type = "flex")` | `flex(sig)` |
| `module(sig, temperature = 0)` | `module(sig, config = list(temperature = 0))` |

### “First argument must be a Signature object”

``` r

try(module("question -> answer"))
#> Error in module("question -> answer") : 
#>   First argument must be a Signature object
#> ✖ Got <character>
#> ℹ Create one with: `signature('question -> answer')`
```

[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
needs a `Signature` object: `module(signature("question -> answer"))`.
The other constructors, such as
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
and [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
accept the string directly.

## Errors when calling `run()`

### “Missing required inputs”

``` r

context_qa <- module(signature("context, question -> answer"))
try(run(context_qa, question = "Who maintains ggplot2?"))
#> Error in validate_signature_inputs(module$signature, inputs, missing = if (inherits(module,  : 
#>   Missing required inputs: context
#> ℹ Signature expects: context and question
#> ℹ You provided: question
```

Pass every input the signature names. A misspelled input name ends up
here too; compare “Signature expects” with “You provided”.

### “Unknown dot-prefixed argument”

``` r

try(run(qa, question = "What is the capital of France?", .temperature = 0))
#> Error in validate_reserved_input_names(names(inputs) %||% character(),  : 
#>   Unknown dot-prefixed argument: `.temperature`
#> ✖ These are not treated as signature fields.
try(run(qa, question = "What is the capital of France?", .parallel = TRUE))
#> Error in validate_reserved_input_names(names(inputs) %||% character(),  : 
#>   Unknown dot-prefixed argument: `.parallel`
#> ✖ These are not treated as signature fields.
#> ℹ Use `.concurrency` with `concurrency_control()`.
```

Arguments that start with a dot configure the call.
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) accepts
`.llm`, `.cache`, `.concurrency`, `.return_format`, `.progress`,
`.verbose`, `.show_prompt` and `.trace_context`; anything else is
rejected rather than sent to the model as an input. Set temperature on
the chat (`ellmer::params(temperature = 0)`) or the module
(`config = list(temperature = 0)`), and run batches in parallel with
`.concurrency = concurrency_control(max_active = 4L)`.

### “Extra input not declared in the signature”

``` r

try(run(qa, question = "What is the capital of France?", tone = "formal", .llm = chat))
#> Warning: Extra input not declared in the signature: tone
#> ℹ The field remains available to custom templates but is not declared in the
#>   signature.
#> ℹ Signature fields: question
#> Error in openai_key() : Can't find env var `OPENAI_API_KEY`.
```

This is a warning, not an error: the call goes on to the model (on this
page it then stops at the missing API key). The extra value is added to
the prompt and is available to a custom `template`, but demos,
optimizers and type checks ignore it. If the model should use the value,
declare it in the signature: `signature("question, tone -> answer")`.

### “Predict templates use single-brace placeholders”

``` r

templated <- module(
  signature("question -> answer"),
  template = "Answer briefly.\n\nQuestion: {{question}}"
)
try(run(templated, question = "What is the capital of France?", .llm = chat))
#> Error in build_prompt(module, inputs) : 
#>   Predict templates use single-brace placeholders
#> ✖ Found a double-brace placeholder in `template`.
#> ℹ Use one brace pair around each declared input name.
```

Templates are glue strings. Write each placeholder with one pair of
braces:

``` r

templated <- module(
  signature("question -> answer"),
  template = "Answer briefly.\n\nQuestion: {question}"
)
```

## Evaluation and optimization errors

### “`@k must be <integer>, not <double>`”

``` r

try(LabeledFewShot(k = 3))
#> Error : <dsprrr::LabeledFewShot> object properties are invalid:
#> - @k must be <integer>, not <double>
```

Teleprompter settings are typed, and whole numbers need an `L`:
`LabeledFewShot(k = 3L)`,
`BootstrapFewShot(max_bootstrapped_demos = 4L)`,
`GEPA(generations = 5L)`.

### “Can’t tell which field this metric should compare”

``` r

keyword_baseline <- module_fn("text -> sentiment", function(text) {
  positive <- grepl("love|great", text, ignore.case = TRUE)
  list(sentiment = if (positive) "positive" else "negative")
})
reviews <- tibble::tibble(
  text = c("I love it", "Arrived broken", "Great value"),
  label = c("positive", "negative", "positive")
)
result <- evaluate(keyword_baseline, reviews, metric = metric_exact_match())
#> Warning: Metric evaluation failed for row 1
#> ✖ Can't tell which field this metric should compare.
#> Warning: Metric evaluation failed for row 2
#> ✖ Can't tell which field this metric should compare.
#> Warning: Metric evaluation failed for row 3
#> ✖ Can't tell which field this metric should compare.
result$mean_score
#> [1] 0
result$n_metric_errors
#> [1] 3
```

[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
calls the metric with the module’s output and the whole data row.
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md)
and
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md)
compare one output field with the data column of the same name. Without
`field` they look for exactly one name the output and the data share,
and fail when there is none (here the column is called `label`) or more
than one. Failed rows count as 0 in `mean_score`, so a score of 0 can
mean the metric failed on every row; check `n_errors`. Rename the column
and name the field, or write a metric that reads both:

``` r

reviews$sentiment <- reviews$label
evaluate(
  keyword_baseline,
  reviews,
  metric = metric_exact_match(field = "sentiment")
)$mean_score
#> [1] 1

label_matches <- function(prediction, expected) {
  prediction$sentiment == expected$label
}
evaluate(keyword_baseline, reviews, metric = label_matches)$mean_score
#> [1] 1
```

### “Callable modules created with `module_fn()` do not support optimization”

``` r

try(compile(keyword_baseline, LabeledFewShot(k = 2L), reviews))
#> Error in abort_if_fn_module(program) : 
#>   Callable modules created with `module_fn()` do not support optimization
#> ℹ Use `run()` or `evaluate()` with this module, or wrap an optimizable dsprrr
#>   module instead.
```

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
improves prompts, and a
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
program has none. Use
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
for baselines and evaluation, and compile a prediction module such as
`module(signature("text -> sentiment"))`.

## Provider errors

### “LLM call failed” and “Failed to process item”

How a provider error (a bad key, a rate limit, a timeout, a server
error) reaches you depends on how the module was called:

| Call | What you see |
|----|----|
| [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) with one input | the provider’s error, unchanged, as above |
| [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) with a vector, [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md), [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md) | a warning “Failed to process item N: …” for each failed row, and `NA` as its result |
| pipelines and wrappers such as [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md), [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md) and [`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md) | “LLM call failed: …”, inside the wrapper’s own error |

``` r

questions <- tibble::tibble(question = c("What is 2 + 2?", "What is 3 + 3?"))
answers <- run_dataset(qa, questions, .llm = chat, .return_format = "structured")
#> Warning: Failed to process item 1: Can't find env var `OPENAI_API_KEY`.
#> Warning: Failed to process item 2: Can't find env var `OPENAI_API_KEY`.
answers$.error
#> [1] "\033[1m\033[22mCan't find env var `OPENAI_API_KEY`."
#> [2] "\033[1m\033[22mCan't find env var `OPENAI_API_KEY`."

two_steps <- qa %>>% module(signature("answer -> summary"))
try(run(two_steps, question = "What is 2 + 2?", .llm = chat))
#> Error in self$forward(inputs, .llm = .llm, trace = TRUE, .cache = .cache) : 
#>   Pipeline step 1 failed
#> ✖ LLM call failed: Can't find env var `OPENAI_API_KEY`.
#> Caused by error in `step@module$forward()`:
#> ! LLM call failed: Can't find env var `OPENAI_API_KEY`.
#> Caused by error in `openai_key()`:
#> ! Can't find env var `OPENAI_API_KEY`.
```

With `.return_format = "structured"`,
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
keeps each row’s message in `.error`.
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
counts failed calls in `n_run_errors`, lists them in `run_errors`, and
scores those rows 0. A
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md)
agent reports “LLM call failed in ReAct loop”.

### Rate limits (HTTP 429)

ellmer retries requests that fail with a transient error, such as HTTP
429 or 503, or a dropped connection. It tries three times by default,
waiting longer after each failure. Allow more tries:

``` r

options(ellmer_max_tries = 6)
```

dsprrr sends batch requests one at a time unless you pass
`.concurrency`. If you did, lower the number of requests in flight:

``` r

run_dataset(qa, questions, .llm = chat, .concurrency = concurrency_control(max_active = 2L))
```

### Timeouts

ellmer waits up to 300 seconds for a response. Long outputs from
reasoning models can take longer; raise the limit:

``` r

options(ellmer_timeout_s = 600)
```

To stop slow rows in a batch instead, set a per-row limit. Finite
timeouts need the mirai backend, which runs the module in worker
processes, so attach the chat to the module instead of passing `.llm`:

``` r

timed_qa <- module(signature("question -> answer"), chat = chat)
run_dataset(
  timed_qa,
  questions,
  .concurrency = concurrency_control(backend = "mirai", max_active = 2L, task_timeout = 120)
)
```

## Cache problems

### The same answer every time (“Using cached LLM responses”)

dsprrr caches responses in memory and on disk. A request identical to an
earlier one (same model, parameters, prompt and output type) returns the
stored answer, and the first cache hit in a session prints “Using cached
LLM responses”. That is usually what you want, but not when you need a
fresh sample or are measuring how answers vary.

``` r

run(qa, question = "Suggest a name for a cat.", .llm = chat, .cache = FALSE) # this call
clear_cache() # forget everything cached
configure_cache(enable = FALSE) # stop caching for this session
```

In CI, set the environment variable `DSPRRR_CACHE_ENABLED=false`. To
sample several different answers on purpose, use
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
which keeps its attempts apart in the cache.

### “Disk caching is unavailable at …”

``` r

shared_dir <- file.path(tempdir(), "shared-cache")
dir.create(shared_dir)
Sys.chmod(shared_dir, mode = "0755") # readable by other accounts
configure_cache(disk_path = shared_dir)
try(run(qa, question = "What is the capital of France?", .llm = chat))
#> Warning: ! Disk caching is unavailable at /tmp/RtmpKCHl0r/shared-cache
#> ✖ an existing private cache directory must have mode exactly 0700, but it is
#>   755; restrict it yourself, then retry: chmod 700
#>   '/tmp/RtmpKCHl0r/shared-cache'
#> ℹ Falling back to memory-only caching for this session.
#> This warning is displayed once per session.
#> Error in openai_key() : Can't find env var `OPENAI_API_KEY`.
```

The disk cache stores prompts and responses, so dsprrr uses it only when
you own the directory, the directory has mode `0700` and every file in
it has mode `0600`. Otherwise it warns once, with the reason and the
`chmod` command that fixes it, and keeps only a memory cache for the
session. (The error after the warning is the missing API key.) This
happens with directories copied from elsewhere, created under a
permissive umask, or shared between accounts. On Windows, dsprrr cannot
check these permissions and relies on the ACLs of the per-user cache
directory.

Run the `chmod` command from the warning, or restrict the directory from
R:

``` r

cache_dir <- tools::R_user_dir("dsprrr", "cache") # or Sys.getenv("DSPRRR_CACHE_PATH")
Sys.chmod(cache_dir, mode = "0700")
Sys.chmod(list.files(cache_dir, full.names = TRUE), mode = "0600")
```

Alternatively, point `configure_cache(disk_path = )` at a new directory,
which dsprrr creates with private permissions, or turn the disk tier off
with `configure_cache(enable_disk = FALSE)`. For a cache that only
trusted accounts can write, `configure_cache(disk_private = FALSE)`
skips the check.
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)
shows the result.

## The answer is wrong or empty

When a call succeeds but the answer is off, look at what the model
received:

``` r

get_last_prompt() # prompt, response, model, tokens and cost of the last call
inspect_history(n = 5) # a tibble of the last five calls
export_traces(qa, include_prompts = TRUE, include_outputs = TRUE) # every call of one module
```

Check that each input appears in the prompt, that your instructions made
it in, and that the output types are as strict as you meant. If you are
still stuck, open an issue at
<https://github.com/JamesHWade/dsprrr/issues> with a minimal example and
the output of
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md).
