# A GEPA learning journey

GEPA improves instructions by scoring candidate programs, showing a
reflection model the failures, and asking it to rewrite the
instructions. This journey uses a small support-ticket task and an
offline Chat fixture so the complete path runs in documentation builds
without credentials.

The fixture is deliberately scripted to make each transition
predictable. Its scores show that the workflow, feedback, result fields
and budget accounting work; they are not evidence that a real model
improves. A live-model example and the checkpoint limits appear below.

## Define the task and data splits

Route tickets as `urgent` when they report a current outage or
compromised account, and `routine` otherwise. Keep the three data sets
separate: GEPA sees training rows when it gathers failures for
reflection, validation rows when it selects candidates, and holdout rows
only for the final comparison.

``` r

library(dsprrr)
```

    ## 
    ## Attaching package: 'dsprrr'

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    ## The following object is masked from 'package:methods':
    ## 
    ##     signature

``` r

train <- data.frame(
  ticket = c(
    "Checkout is unavailable for every customer during an outage.",
    "Please update the billing contact.",
    "An unknown user took over my account.",
    "When is the next invoice due?"
  ),
  severity = c("urgent", "routine", "urgent", "routine")
)

validation <- data.frame(
  ticket = c(
    "A production outage is blocking every customer.",
    "A stranger gained access to my account.",
    "Can I change my billing email?"
  ),
  severity = c("urgent", "urgent", "routine")
)

holdout <- data.frame(
  ticket = c(
    "A service outage is stopping password resets.",
    "An attacker accessed my account overnight.",
    "Please add a second billing contact.",
    "Where can I see next month's invoice?"
  ),
  severity = c("urgent", "urgent", "routine", "routine")
)

program <- module(signature(
  "ticket -> severity: enum('urgent', 'routine')",
  instructions = "Use a quick baseline rule."
))
```

The fixture implements the small part of ellmer’s Chat protocol used
here. It returns a routine label for the baseline, an outage-only
proposal on its first reflection, and a complete rule after it sees
feedback about the missed account-compromise case. It is only an offline
teaching fixture; use an ellmer Chat constructor such as
[`ellmer::chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.html)
in an application.

``` r

fixture_chat_class <- R6::R6Class(
  "Chat",
  public = list(
    turns = NULL,
    initialize = function(model, responder) {
      self$turns <- list()
      private$model <- ellmer::Model(name = model, extra_args = list())
      private$provider <- ellmer::Provider(name = "fixture", base_url = "")
      private$responder <- responder
    },
    chat = function(prompt, ...) {
      private$responder(paste(as.character(prompt), collapse = "\n"), NULL, ...)
    },
    chat_structured = function(prompt, type, ...) {
      private$responder(paste(as.character(prompt), collapse = "\n"), type, ...)
    },
    stream = function(...) character(),
    get_turns = function() self$turns,
    set_turns = function(turns) {
      self$turns <- turns
      invisible(self)
    },
    last_turn = function(role = NULL) NULL,
    get_model = function() private$model@name,
    get_provider = function() private$provider,
    get_model_object = function() private$model
  ),
  private = list(model = NULL, provider = NULL, responder = NULL),
  lock_objects = FALSE,
  parent_env = globalenv()
)

new_fixture_chat <- function(model, responder) {
  audit <- new.env(parent = emptyenv())
  audit$prompts <- character()
  wrapped_responder <- function(prompt, type, ...) {
    prompt <- paste(as.character(prompt), collapse = "\n")
    audit$prompts <- c(audit$prompts, prompt)
    responder(prompt, type, ...)
  }
  list(
    chat = fixture_chat_class$new(model, wrapped_responder),
    audit = audit
  )
}

partial_rule <- paste(
  "Mark a ticket urgent when it reports an explicit outage;",
  "otherwise mark it routine."
)
complete_rule <- paste(
  "Mark tickets urgent when they report a current service outage",
  "or compromised account; otherwise mark them routine."
)

task_fixture <- new_fixture_chat("fixture-task", function(prompt, type, ...) {
  prompt_lines <- strsplit(prompt, "\n", fixed = TRUE)[[1L]]
  input_header <- grep("^# Input:", prompt_lines)
  ticket_line <- grep("^ticket: ", prompt_lines, value = TRUE)
  if (length(input_header) != 1L || length(ticket_line) != 1L) {
    stop("Task fixture could not read one ticket from the prompt")
  }

  instructions <- paste(
    prompt_lines[seq_len(input_header - 1L)],
    collapse = "\n"
  )
  ticket <- sub("^ticket: ", "", ticket_line[[1L]])
  outage_rule <- grepl("explicit outage", instructions, fixed = TRUE) ||
    grepl("current service outage", instructions, fixed = TRUE)
  compromise_rule <- grepl("compromised account", instructions, fixed = TRUE)
  outage <- grepl(
    "outage|unavailable|blocking|stopping|down",
    ticket,
    ignore.case = TRUE
  )
  compromised <- grepl(
    "unknown user|stranger gained access|attacker|unknown login|compromised",
    ticket,
    ignore.case = TRUE
  )

  urgent <- (outage_rule && outage) || (compromise_rule && compromised)
  list(severity = if (urgent) "urgent" else "routine")
})

reflection_fixture <- new_fixture_chat(
  "fixture-reflection",
  function(prompt, type, ...) {
    account_failure <- grepl(
      "Expected urgent but got routine",
      prompt,
      fixed = TRUE
    ) && grepl(
      "An unknown user took over my account.",
      prompt,
      fixed = TRUE
    )
    instructions <- if (account_failure) {
      complete_rule
    } else {
      partial_rule
    }
    list(instructions = instructions)
  }
)

old_cache <- configure_cache(enable = FALSE)
```

## Score the baseline

The metric returns both a score and a sentence the reflection step can
use. The baseline fixture intentionally misses both urgent validation
rows.

``` r

feedback_metric <- metric_with_feedback(
  function(prediction, expected) {
    correct <- identical(prediction$severity, expected$severity)
    list(
      score = as.numeric(correct),
      feedback = if (correct) {
        "Correct."
      } else {
        paste0(
          "Expected ", expected$severity, " but got ",
          prediction$severity,
          ". Check for an outage or a compromised account."
        )
      }
    )
  },
  field = "severity"
)

baseline_validation <- evaluate(
  program,
  validation,
  metric = feedback_metric,
  .llm = task_fixture$chat,
  .cache = FALSE,
  .progress = FALSE
)

baseline_validation$mean_score
```

    ## [1] 0.3333333

``` r

stopifnot(isTRUE(all.equal(as.numeric(baseline_validation$mean_score), 1 / 3)))

data.frame(
  ticket = validation$ticket,
  expected = validation$severity,
  predicted = vapply(
    baseline_validation$predictions,
    `[[`,
    character(1),
    "severity"
  ),
  score = baseline_validation$scores
)
```

    ##                                            ticket expected predicted score
    ## 1 A production outage is blocking every customer.   urgent   routine     0
    ## 2         A stranger gained access to my account.   urgent   routine     0
    ## 3                  Can I change my billing email?  routine   routine     1

## Run a bounded search

`.llm` evaluates candidate instructions. `.reflection_llm` proposes new
instructions from training failures and their feedback. The two roles
can use different models; this fixture keeps them local and
deterministic. The
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md)
cap applies to the compile run.

``` r

task_prompt_start <- length(task_fixture$audit$prompts)
reflection_prompt_start <- length(reflection_fixture$audit$prompts)

optimized <- compile(
  program,
  GEPA(
    metric = feedback_metric,
    population_size = 2L,
    generations = 2L,
    mutation_rate = 1,
    crossover_rate = 0,
    selection = "current_best",
    seed = 42L,
    verbose = FALSE
  ),
  train,
  valset = validation,
  .llm = task_fixture$chat,
  .reflection_llm = reflection_fixture$chat,
  control = optimizer_control(
    max_metric_calls = 40L,
    progress = FALSE
  )
)

result <- optimization_result(optimized)
result
```

    ## <dsprrr_optimization_result>
    ## Optimizer: GEPA
    ## Status: completed
    ## Best score: 1
    ## Trials: 3
    ## Stopped: completed

The first proposal handles outages but still misses compromised
accounts. The next reflection sees the failed training row and its
feedback:

``` r

optimizer_task_prompts <- task_fixture$audit$prompts[
  seq.int(task_prompt_start + 1L, length(task_fixture$audit$prompts))
]
optimizer_reflection_prompts <- reflection_fixture$audit$prompts[
  seq.int(reflection_prompt_start + 1L, length(reflection_fixture$audit$prompts))
]

feedback_prompts <- optimizer_reflection_prompts[
  grepl("Feedback:", optimizer_reflection_prompts, fixed = TRUE)
]
stopifnot(
  length(feedback_prompts) > 0L,
  grepl(
    "Expected urgent but got routine",
    feedback_prompts[[1L]],
    fixed = TRUE
  ),
  grepl(train$ticket[[3L]], feedback_prompts[[1L]], fixed = TRUE)
)
feedback_lines <- strsplit(feedback_prompts[[1L]], "\n", fixed = TRUE)[[1L]]
cat(grep("Feedback:", feedback_lines, value = TRUE), sep = "\n")
```

    ## Inputs: ticket: An unknown user took over my account. | Expected: urgent | Predicted: routine | Feedback: Expected urgent but got routine. Check for an outage or a compromised account.

``` r

optimizer_prompts <- paste(
  c(optimizer_task_prompts, optimizer_reflection_prompts),
  collapse = "\n"
)
stopifnot(
  !any(vapply(
    holdout$ticket,
    grepl,
    logical(1),
    x = optimizer_prompts,
    fixed = TRUE
  ))
)
```

The prompt audit confirms that holdout tickets did not enter candidate
evaluation or reflection. They are only used after the search. The
result’s GEPA extension records each candidate’s parent and validation
score:

``` r

gepa_details <- result$extensions$gepa
candidate_ids <- names(gepa_details$candidates)

candidate_lineage <- data.frame(
  candidate_id = candidate_ids,
  parent = vapply(candidate_ids, function(candidate_id) {
    parents <- gepa_details$parents[[candidate_id]]
    if (length(parents) == 0L) "seed" else paste(parents, collapse = ", ")
  }, character(1)),
  validation_score = unname(gepa_details$val_aggregate_scores[candidate_ids]),
  selected = candidate_ids == result$lineage$best_candidate_id
)
candidate_lineage
```

    ##                                          candidate_id
    ## candidate:83ea54f4f9d988bb candidate:83ea54f4f9d988bb
    ## candidate:4212b0b5569dc025 candidate:4212b0b5569dc025
    ## candidate:54815fa4c45d5940 candidate:54815fa4c45d5940
    ##                                                parent validation_score selected
    ## candidate:83ea54f4f9d988bb                       seed        0.3333333    FALSE
    ## candidate:4212b0b5569dc025 candidate:83ea54f4f9d988bb        0.6666667    FALSE
    ## candidate:54815fa4c45d5940 candidate:4212b0b5569dc025        1.0000000     TRUE

``` r

stopifnot(
  isTRUE(any(candidate_lineage$selected)),
  isTRUE(result$best_score == 1),
  isTRUE(candidate_lineage$validation_score[
    candidate_lineage$selected
  ] == 1)
)

list(
  candidate_id = result$lineage$best_candidate_id,
  validation_score = result$best_score,
  instructions = result$best_params$component_values[[1L]]$value
)
```

    ## $candidate_id
    ## [1] "candidate:54815fa4c45d5940"
    ## 
    ## $validation_score
    ## [1] 1
    ## 
    ## $instructions
    ## [1] "Mark tickets urgent when they report a current service outage or compromised account; otherwise mark them routine."

## Compare once on holdout

The holdout comparison is a separate
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
call. It does not change GEPA’s candidate selection or its recorded
budget.

``` r

holdout_before <- evaluate(
  program,
  holdout,
  metric = feedback_metric,
  .llm = task_fixture$chat,
  .cache = FALSE,
  .progress = FALSE
)
holdout_after <- evaluate(
  optimized,
  holdout,
  metric = feedback_metric,
  .llm = task_fixture$chat,
  .cache = FALSE,
  .progress = FALSE
)

data.frame(
  ticket = holdout$ticket,
  expected = holdout$severity,
  before = vapply(holdout_before$predictions, `[[`, character(1), "severity"),
  after = vapply(holdout_after$predictions, `[[`, character(1), "severity")
)
```

    ##                                          ticket expected  before   after
    ## 1 A service outage is stopping password resets.   urgent routine  urgent
    ## 2    An attacker accessed my account overnight.   urgent routine  urgent
    ## 3          Please add a second billing contact.  routine routine routine
    ## 4         Where can I see next month's invoice?  routine routine routine

``` r

c(
  before = holdout_before$mean_score,
  after = holdout_after$mean_score
)
```

    ## before  after 
    ##    0.5    1.0

``` r

stopifnot(
  isTRUE(holdout_before$mean_score == 0.5),
  isTRUE(holdout_after$mean_score == 1)
)
```

The fixture score rises from 0.5 to 1.0 on these four holdout rows. This
is a scripted workflow demonstration, not a real-model improvement
claim. Candidate selection, feedback and results from a live search are
stochastic and may not follow this path.

## Read the budget and choose objectives

The public [optimization
result](https://jameshwade.github.io/dsprrr/reference/optimization_result.md)
gives the compile budget and its configured limits. Here, the GEPA
search used 28 metric calls from its 40-call cap. The baseline
validation and two holdout evaluations below are outside that compile
budget:

``` r

gepa_budget <- result$budget
stopifnot(
  isTRUE(gepa_budget$metric_calls == 28),
  isTRUE(gepa_budget$metric_calls <= gepa_budget$limits$max_metric_calls)
)
metric_accounting <- data.frame(
  phase = c(
    "GEPA compile",
    "Baseline validation",
    "Before and after holdout evaluations"
  ),
  metric_calls = c(
    gepa_budget$metric_calls,
    nrow(validation),
    2L * nrow(holdout)
  )
)
metric_accounting <- rbind(
  metric_accounting,
  data.frame(
    phase = "Total across this article",
    metric_calls = sum(metric_accounting$metric_calls)
  )
)
metric_accounting
```

    ##                                  phase metric_calls
    ## 1                         GEPA compile           28
    ## 2                  Baseline validation            3
    ## 3 Before and after holdout evaluations            8
    ## 4            Total across this article           39

``` r

stopifnot(isTRUE(metric_accounting$metric_calls[[4L]] == 39))

gepa_budget$limits$max_metric_calls
```

    ## [1] 40

This example has one objective, so candidates are ranked by that score.
With several named metrics and the default `selection = "pareto"`, GEPA
ranks non-dominated trade-offs across objectives; it does not reduce
them to one weighted score. `selection = "current_best"` instead ranks
candidates only by the first metric.

The fixture does not report provider token counts or cost. With real
Chats, use
`optimizer_control(max_provider_calls = ..., max_total_tokens = ..., max_cost = ...)`
to limit provider usage when the provider supplies the required
measurements. Finite limits are checked between metric rows and
optimizer provider calls, so the last row or call can overshoot a limit;
a cost limit is not a hard provider-side spending ceiling. If a provider
does not report a value, a finite cap cannot be enforced by guessing.

## Where GEPA fits

- **GEPA** searches candidate instructions from task failures and
  feedback.
- **Flex** lets GEPA change a complete program source; it is a separate
  experimental path described in [Flex: optimize a whole
  program](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md).
- **Omni** compares several optimizers from the same starting program
  and can continue from the selected result; see [Compose optimizers
  with
  Omni](https://jameshwade.github.io/dsprrr/articles/omni-meta-optimization.md).
- **AutoResearch** and **MetaHarness** are outer experiment harnesses:
  an agent proposes edits, and dsprrr’s R evaluator validates and scores
  them. See [Agentic optimization
  harnesses](https://jameshwade.github.io/dsprrr/articles/agentic-optimization-harnesses.md).

In DSPy 3.5.0, GEPA uses a native search budget chosen as exactly one of
`auto`, `max_full_evals` or `max_metric_calls`, and its feedback metric
can receive predictor and execution-trace context. dsprrr’s
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md)
exposes `population_size` and `generations`, with optional call, token,
cost and time limits through
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md);
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md)
receives the prediction and expected row. Similar names do not make
candidate paths or scores interchangeable. See the [DSPy 3.5.0 GEPA
source](https://github.com/stanfordnlp/dspy/blob/3.5.0/dspy/teleprompt/gepa/gepa.py).

## Checkpoints and an opt-in live run

GEPA checkpoint resume supports sequential `round_robin` searches over
plain `Predict` leaves and built-in predictor pipelines that return
plain data. It does not support Flex, other component selectors, custom
module types or custom Chats, Chats with tools, or Chats with existing
turns. A resume must match the original program, data, metrics,
algorithm settings and effective Chat identities; resource limits may
change. Completed proposals and evaluation rows are restored, but a
provider request interrupted after the provider completes and before the
result is committed may run again after a crash. Resume does not
guarantee exactly-once provider effects. See the [GEPA
reference](https://jameshwade.github.io/dsprrr/reference/GEPA.md) for
the full contract.

To try a live search with your own labeled data, create separate task
and reflection Chats and set a budget. This block is opt-in and is not
run during documentation builds:

``` r

task_chat <- ellmer::chat_openai(model = "gpt-6-luna")
reflection_chat <- ellmer::chat_openai(model = "gpt-6-astra")

live_result <- compile(
  program,
  GEPA(
    metric = feedback_metric,
    population_size = 6L,
    generations = 3L,
    seed = 42L
  ),
  train,
  valset = validation,
  .llm = task_chat,
  .reflection_llm = reflection_chat,
  control = optimizer_control(
    max_metric_calls = 80L,
    max_provider_calls = 50L,
    max_cost = 1
  )
)

optimization_result(live_result)
```

If `.reflection_llm` is omitted, an explicitly supplied `.llm` is also
used for reflection as a compatibility fallback. The reflection model
proposes instructions; the task model scores them. Compare models with
the same data, validation set, seed and limits, and inspect
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md)
rather than treating a fixture score or one stochastic run as a general
result.
