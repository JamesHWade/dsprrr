# A GEPA learning journey

GEPA improves a module’s instructions through *reflection*: it scores
candidate instructions, shows a reflection model the rows a candidate
got wrong along with your metric’s textual feedback, and asks it to
rewrite the instructions. Over several generations, the strongest
candidates survive and evolve.

> **Note on the offline runner:** To execute end-to-end during
> documentation builds without API keys, this walkthrough uses a
> deterministic stand-in `task_chat` and `reflection_chat` (defined in a
> hidden setup chunk). It illustrates the real GEPA loop, lineage, and
> budget accounting; on your own data, swap in live ellmer Chats as
> shown at the end of the page.

## 1. Define the task and keep three splits separate

Suppose you want to route customer support tickets as `urgent` (current
service outage or compromised account) or `routine` (everything else).
Keep three datasets strictly separate:

- **`train`**: where GEPA collects failures and feedback for reflection.
- **`validation`**: where GEPA scores candidates and picks parents.
- **`holdout`**: touched only once, at the very end, to verify whether
  the winning instructions actually generalize.

``` r

library(dsprrr)

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

## 2. Write a feedback metric and score the baseline

What makes GEPA different from plain grid search is *why* it knows how
to improve a prompt. Wrapping a metric in
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md)
lets each row return both a numeric `score` and a short `feedback`
sentence explaining what went wrong:

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

baseline_val <- evaluate(
  program,
  validation,
  metric = feedback_metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)

baseline_val$mean_score
#> [1] 0.3333333

data.frame(
  ticket = validation$ticket,
  expected = validation$severity,
  predicted = vapply(baseline_val$predictions, `[[`, character(1), "severity"),
  score = baseline_val$scores
)
#>                                            ticket expected predicted score
#> 1 A production outage is blocking every customer.   urgent   routine     0
#> 2         A stranger gained access to my account.   urgent   routine     0
#> 3                  Can I change my billing email?  routine   routine     1
```

With the vague baseline instruction (`"Use a quick baseline rule."`),
the module marks every ticket `routine` and misses both urgent cases
(`1/3`).

## 3. Run a bounded GEPA search

Pass the task Chat as `.llm`, the reflection Chat as `.reflection_llm`,
and a resource cap through
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md):

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
  .llm = task_chat,
  .reflection_llm = reflection_chat,
  control = optimizer_control(
    max_metric_calls = 40L,
    progress = FALSE
  )
)

result <- optimization_result(optimized)
result
#> <dsprrr_optimization_result>
#> Optimizer: GEPA
#> Status: completed
#> Best score: 1
#> Trials: 3
#> Stopped: completed
```

## 4. Watch the instructions evolve

Inspecting `result$extensions$gepa` shows the exact lineage of
candidates and how each generation’s wording improved on the validation
set:

``` r

gepa_details <- result$extensions$gepa
candidate_ids <- names(gepa_details$candidates)

data.frame(
  candidate = candidate_ids,
  parent = vapply(candidate_ids, function(id) {
    parents <- gepa_details$parents[[id]]
    if (length(parents) == 0L) "seed" else paste(parents, collapse = ", ")
  }, character(1)),
  val_score = round(unname(gepa_details$val_aggregate_scores[candidate_ids]), 3),
  selected = candidate_ids == result$lineage$best_candidate_id,
  instructions = vapply(candidate_ids, function(id) {
    gepa_details$candidates[[id]]$component_values[[1L]]$value
  }, character(1)),
  row.names = NULL
)
#>                    candidate                     parent val_score selected
#> 1 candidate:83ea54f4f9d988bb                       seed     0.333    FALSE
#> 2 candidate:4212b0b5569dc025 candidate:83ea54f4f9d988bb     0.667    FALSE
#> 3 candidate:54815fa4c45d5940 candidate:4212b0b5569dc025     1.000     TRUE
#>                                                                                                         instructions
#> 1                                                                                         Use a quick baseline rule.
#> 2                                Mark a ticket urgent when it reports an explicit outage; otherwise mark it routine.
#> 3 Mark tickets urgent when they report a current service outage or compromised account; otherwise mark them routine.
```

Look at the three-step progression:

1.  **Seed (`val_score = 0.333`):** `"Use a quick baseline rule."`
    misses both outages and compromised accounts.
2.  **First rewrite (`val_score = 0.667`):** catches outages, but still
    misses `"An unknown user took over my account."` on the training
    set.
3.  **Feedback-driven rewrite (`val_score = 1.000`):** GEPA feeds the
    missed training row and its feedback sentence back into the
    reflection prompt:

``` r

reflection_prompts <- reflection_fixture$audit$prompts[
  seq.int(reflection_prompt_start + 1L, length(reflection_fixture$audit$prompts))
]
feedback_prompts <- reflection_prompts[
  grepl("Feedback:", reflection_prompts, fixed = TRUE)
]
feedback_lines <- strsplit(feedback_prompts[[1L]], "\n", fixed = TRUE)[[1L]]
cat(grep("Feedback:", feedback_lines, value = TRUE), sep = "\n")
#> Inputs: ticket: An unknown user took over my account. | Expected: urgent | Predicted: routine | Feedback: Expected urgent but got routine. Check for an outage or a compromised account.
```

Seeing that specific miss, the reflection model adds
`"or compromised account"` to the instructions, and the new candidate
scores `1.0` on validation.

## 5. Check once on holdout

Now evaluate both the original and compiled modules on the untouched
`holdout` rows:

``` r

holdout_before <- evaluate(
  program,
  holdout,
  metric = feedback_metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)
holdout_after <- evaluate(
  optimized,
  holdout,
  metric = feedback_metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)

data.frame(
  ticket = holdout$ticket,
  expected = holdout$severity,
  before = vapply(holdout_before$predictions, `[[`, character(1), "severity"),
  after = vapply(holdout_after$predictions, `[[`, character(1), "severity")
)
#>                                          ticket expected  before   after
#> 1 A service outage is stopping password resets.   urgent routine  urgent
#> 2    An attacker accessed my account overnight.   urgent routine  urgent
#> 3          Please add a second billing contact.  routine routine routine
#> 4         Where can I see next month's invoice?  routine routine routine

c(before = holdout_before$mean_score, after = holdout_after$mean_score)
#> before  after 
#>    0.5    1.0
```

## 6. Read the budget and choose objectives

`optimization_result(optimized)$budget` tracks every metric call spent
inside
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).
Baseline and final holdout evaluations sit outside that budget:

``` r

gepa_budget <- result$budget
data.frame(
  phase = c(
    "GEPA compile",
    "Baseline validation",
    "Before and after holdout",
    "Total across this walkthrough"
  ),
  metric_calls = c(
    gepa_budget$metric_calls,
    nrow(validation),
    2L * nrow(holdout),
    gepa_budget$metric_calls + nrow(validation) + 2L * nrow(holdout)
  )
)
#>                           phase metric_calls
#> 1                  GEPA compile           28
#> 2           Baseline validation            3
#> 3      Before and after holdout            8
#> 4 Total across this walkthrough           39
```

With one metric, GEPA ranks candidates by that score. When you supply
several named objectives via `metrics = list(...)` and keep the default
`selection = "pareto"`, GEPA retains the non-dominated Pareto front
instead of collapsing trade-offs into a single weighted sum
(`selection = "current_best"` ranks by the first metric alone).

## 7. Run GEPA on live models

On real data, pair a fast task model with a strong reflection model and
set a cost/call budget (if you omit `.reflection_llm`, GEPA uses `.llm`
for reflection as well). Finite limits are checked between rows and
provider calls, so treat `max_cost` as a soft budget rather than a hard
billing ceiling:

``` r

live_optimized <- compile(
  program,
  GEPA(
    metric = feedback_metric,
    population_size = 6L,
    generations = 3L,
    seed = 42L
  ),
  train,
  valset = validation,
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .reflection_llm = ellmer::chat_openai(model = "gpt-6-astra"),
  control = optimizer_control(
    max_metric_calls = 80L,
    max_provider_calls = 50L,
    max_cost = 1,
    checkpoint_path = file.path(tempdir(), "gepa-checkpoint.rds")
  )
)

optimization_result(live_optimized)
```

GEPA supports checkpoint resume for sequential `round_robin` searches
over plain prediction modules and built-in pipelines; see
[GEPA](https://jameshwade.github.io/dsprrr/reference/GEPA.md) for the
full checkpoint contract, and [dsprrr for DSPy
users](https://jameshwade.github.io/dsprrr/articles/dspy-comparison.md)
for how dsprrr’s
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md)
compares with DSPy 3.5.0.

## Where GEPA fits

- **GEPA** evolves instructions from failures and textual feedback.
- **[Flex](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md)**
  lets GEPA rewrite a whole program’s structure, not only its
  instructions.
- **[Omni](https://jameshwade.github.io/dsprrr/articles/omni-meta-optimization.md)**
  compares several optimizers from one starting point and continues from
  the best.
- **[Agentic optimization
  harnesses](https://jameshwade.github.io/dsprrr/articles/agentic-optimization-harnesses.md)**
  let an outer research agent explore hypotheses and propose
  multi-module edits.
