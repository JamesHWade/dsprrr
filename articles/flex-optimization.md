# Flex: optimize a whole program

Most dsprrr optimizers keep a program’s structure fixed and tune its
instructions or demonstrations.
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
*(experimental)* makes the program’s structure itself part of the search
space: [`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md)
can add or remove predictor steps, introduce deterministic R logic, or
call tools you expose.

| What should change? | Use | Execution boundary |
|----|----|----|
| Instructions or demos inside a fixed module | Regular [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) + teleprompter | Standard host runtime |
| A bounded sequence of model steps | **JSON Flex** (`source_format = "json"`, default) | Validated data; no code interpreter |
| Branches, R logic, or host tools | **Executable Flex** (`source_format = "r"`) | Fresh runner; OS sandbox required by default |

Start with JSON Flex when the workflow is a pure graph of `Predict` and
`ChainOfThought` steps. Reach for executable R Flex only when
deterministic checks or tools can solve part of the task directly.

> **Note on the offline runner:** To execute end-to-end in documentation
> builds without API keys, this walkthrough uses a deterministic local
> Chat fixture (defined in a hidden setup chunk) and
> [`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
> over two reviewed source strings. A live search with
> [`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
> and real ellmer Chats appears near the end of the page.

## 1. Start with declarative JSON Flex

Calling
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) on a
signature builds a one-step declarative JSON program by default. JSON
Flex checks field types ahead of time and needs no code interpreter:

``` r

library(dsprrr)

route_signature <- signature(
  "ticket -> queue: enum('database', 'security', 'payments', 'identity')",
  instructions = "Route each support ticket to its owning queue."
)

json_program <- flex(route_signature)
cat(json_program$module_src, "\n")
#> {"schema_version":1,"steps":[{"name":"predict","primitive":"predict","signature":"$outer","inputs":{"ticket":"$input.ticket"}}],"outputs":{"queue":"$step.predict.queue"}}
```

Run it like any other module:

``` r

json_fixture <- new_route_fixture()

json_result <- run(
  json_program,
  ticket = "[INC-SEC-9] unusual-login burst",
  .llm = json_fixture$chat,
  .cache = FALSE,
  .return_format = "structured"
)

json_result$output
#> $queue
#> [1] "security"
json_result$metadata$predictor_calls
#> [1] 1
```

## 2. Add a host tool with executable R Flex

Suppose a support team routes tickets to `database`, `security`,
`payments`, or `identity`. Some tickets carry an incident code whose
owner already lives in an internal catalog; the rest require an LLM
call.

Keep `train` and `validation` separate during compilation, and hold out
a third dataset for the final check:

``` r

train <- data.frame(
  ticket = c(
    "[INC-SEC-9] unusual-login burst",
    "Password reset links expire immediately"
  ),
  queue = c("security", "identity")
)

validation <- data.frame(
  ticket = c(
    "[INC-PAY-4] capture queue is growing",
    "Renewal created two invoices"
  ),
  queue = c("payments", "payments")
)
```

Expose the incident catalog as a plain host R function. Executable Flex
calls only the named tools you pass in—the optimizer never sees the
closure’s private environment or the rest of your R session:

``` r

lookup_incident <- local({
  catalog <- c(
    "INC-DB-17" = "database",
    "INC-SEC-9" = "security",
    "INC-PAY-4" = "payments"
  )

  function(ticket) {
    hit <- names(catalog)[vapply(
      names(catalog),
      grepl,
      logical(1),
      x = ticket,
      fixed = TRUE
    )]
    if (!length(hit)) {
      return(list(found = FALSE))
    }
    list(found = TRUE, queue = unname(catalog[[hit[[1L]]]]))
  }
})

lookup_incident("[INC-DB-17] replicas unavailable")
#> $found
#> [1] TRUE
#> 
#> $queue
#> [1] "database"
lookup_incident("A password reset email is late")
#> $found
#> [1] FALSE
```

An executable Flex program defines a top-level `forward()` function that
can call Flex primitives (`Predict()`, `ChainOfThought()`,
`Prediction()`, `ReAct()`, `RLM()`, `ProgramOfThought()`) alongside your
named tools. Our baseline calls a predictor on every ticket; the
candidate we want GEPA to find checks `lookup_incident()` first and
falls back to the predictor only when the catalog misses:

``` r

baseline_source <- paste(
  "router <- Predict(\"$outer\", instructions = \"Choose the owning queue.\")",
  "forward <- function(ticket) router(ticket = ticket)",
  sep = "\n"
)

selected_fixture_source <- paste(
  "fallback <- Predict(\"$outer\", instructions = \"Route tickets without a known incident code.\")",
  "forward <- function(ticket) {",
  "  if (grepl(\"INC-\", ticket, fixed = TRUE)) {",
  "    hit <- lookup_incident(ticket = ticket)",
  "    if (isTRUE(hit$found)) return(Prediction(queue = hit$queue))",
  "  }",
  "  fallback(ticket = ticket)",
  "}",
  sep = "\n"
)

baseline <- flex(
  route_signature,
  module_src = baseline_source,
  tools = list(lookup_incident = lookup_incident),
  interpreter_factory = r_code_runner,
  source_format = "r",
  require_sandbox = FALSE,
  max_predictor_calls = 1L,
  max_tool_calls = 1L
)
```

Notice `require_sandbox = FALSE` with
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
(requires the optional `callr` package). That is safe here *only*
because both source strings are reviewed fixtures; for live
model-authored R code, keep the default `require_sandbox = TRUE` with an
OS-sandboxed runner such as
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md).

## 3. Reward efficiency with a trace-aware metric

A trace-aware metric
([`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md))
can reward both accuracy and avoiding unnecessary model calls: `1.0` for
a right answer with zero predictor calls, `0.5` for a right answer using
a predictor, and `0.0` for a wrong route:

``` r

route_metric <- metric_with_trace(
  function(prediction, expected, program_trace) {
    correct <- identical(
      as.character(prediction$queue),
      as.character(expected$queue[[1L]])
    )
    predictor_calls <- program_trace$metadata$predictor_calls %||% 0L

    list(
      score = if (!correct) 0 else if (predictor_calls == 0L) 1 else 0.5,
      feedback = if (!correct) {
        sprintf("Expected '%s'; got '%s'.", expected$queue[[1L]], prediction$queue)
      } else if (predictor_calls > 0L) {
        "Correct route; prefer the incident catalog when it has an owner."
      } else {
        "Correct route without a predictor call."
      }
    )
  },
  field = "queue"
)
```

## 4. Compile with `GEPA()` and inspect the winning source

``` r

optimizer_fixture <- new_route_fixture(
  proposed_source = selected_fixture_source,
  model = "fixture-gepa-router"
)

optimized <- compile(
  baseline,
  GEPA(
    metric = route_metric,
    population_size = 2L,
    generations = 1L,
    selection = "current_best",
    track_best_outputs = TRUE,
    verbose = FALSE
  ),
  trainset = train,
  valset = validation,
  .llm = optimizer_fixture$chat,
  control = optimizer_control(
    max_metric_calls = 40L,
    max_provider_calls = 30L,
    max_elapsed_seconds = 300,
    num_threads = 1L,
    progress = FALSE
  )
)

compile_result <- optimization_result(optimized)
compile_result
#> <dsprrr_optimization_result>
#> Optimizer: GEPA
#> Status: completed
#> Best score: 0.75
#> Trials: 2
#> Stopped: completed
```

The source GEPA selected is stored directly on the compiled
module—always inspect it before deploying:

``` r

cat(optimized$module_src, "\n")
#> fallback <- Predict("$outer", instructions = "Route tickets without a known incident code.")
#> forward <- function(ticket) {
#>   if (grepl("INC-", ticket, fixed = TRUE)) {
#>     hit <- lookup_incident(ticket = ticket)
#>     if (isTRUE(hit$found)) return(Prediction(queue = hit$queue))
#>   }
#>   fallback(ticket = ticket)
#> }
```

## 5. Compare once on untouched holdout tickets

Evaluate both the baseline and compiled programs on six fresh tickets
(three with catalog incident codes, three without):

``` r

holdout <- data.frame(
  ticket = c(
    "[INC-DB-17] replicas unavailable",
    "[INC-SEC-9] login spray detected",
    "[INC-PAY-4] payment capture is delayed",
    "Reset email arrives after its token expires",
    "Duplicate charge after a plan upgrade",
    "Read traffic fails after failover"
  ),
  queue = c("database", "security", "payments", "identity", "payments", "database")
)

accuracy <- metric_exact_match(field = "queue")
before_fixture <- new_route_fixture(model = "fixture-holdout-before")
after_fixture <- new_route_fixture(model = "fixture-holdout-after")

before <- evaluate(
  baseline, holdout, metric = accuracy,
  .llm = before_fixture$chat, .cache = FALSE, .progress = FALSE
)
after <- evaluate(
  optimized, holdout, metric = accuracy,
  .llm = after_fixture$chat, .cache = FALSE, .progress = FALSE
)

count_calls <- function(res, field) {
  sum(vapply(res$metadata, function(m) as.integer(m[[field]] %||% 0L), integer(1)))
}

heldout <- data.frame(
  program = c("baseline", "selected"),
  exact_match = c(before$mean_score, after$mean_score),
  predictor_calls = c(
    count_calls(before, "predictor_calls"),
    count_calls(after, "predictor_calls")
  ),
  tool_calls = c(
    count_calls(before, "tool_calls"),
    count_calls(after, "tool_calls")
  )
)
heldout
#>    program exact_match predictor_calls tool_calls
#> 1 baseline           1               6          0
#> 2 selected           1               3          3
```

Both programs route all six holdout tickets accurately (`1.0`), while
the selected Flex program cuts predictor calls in half (6 → 3) by
resolving catalog incidents locally through `lookup_incident()`.

## 6. Run a live sandboxed search

For live model-authored R code, install `mcptools` and Posit’s
`mcp-repl` sandbox
([posit-dev/mcp-console](https://github.com/posit-dev/mcp-console)) and
keep `require_sandbox = TRUE` (the default):

``` sh
Rscript -e 'install.packages("mcptools")'
uv tool install posit-mcp-repl
```

``` r

live_baseline <- flex(
  route_signature,
  module_src = baseline_source,
  tools = list(lookup_incident = lookup_incident),
  interpreter_factory = function() mcp_repl_runner(timeout = 45),
  source_format = "r",
  max_predictor_calls = 1L,
  max_tool_calls = 1L
)

live_optimized <- compile(
  live_baseline,
  GEPA(
    metric = route_metric,
    population_size = 4L,
    generations = 2L,
    selection = "current_best",
    seed = 42L,
    verbose = FALSE
  ),
  trainset = train,
  valset = validation,
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .reflection_llm = ellmer::chat_openai(model = "gpt-6-astra"),
  control = optimizer_control(
    max_metric_calls = 80L,
    max_provider_calls = 50L,
    max_cost = 1,
    progress = FALSE
  )
)

optimization_result(live_optimized)
```

## Operational limits to keep in mind

- `max_predictor_calls` and `max_tool_calls` bound direct calls per
  invocation (nested `ReAct()` / `RLM()` predictors have their own step
  bounds).
- Guest R code replays from the top after each bridged predictor or tool
  call, so keep guest-side logic pure and bound loops explicitly.
- Host tools run in your R session with their own permissions, even when
  guest source runs inside
  [`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md).
- Executable Flex evaluates dataset rows sequentially and does not
  stream tokens.

When the workflow structure is already known, a plain
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) or
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
is simpler and cheaper; see [Choose an
optimizer](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md)
for the other optimization strategies.
