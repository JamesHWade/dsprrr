# Compile and optimize

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
runs an optimizer, called a teleprompter, over a module and a set of
labeled examples, and returns a copy of the module with new demos or
instructions. This guide goes through one complete run: define the task,
the labeled data and the metric, compile with two teleprompters, score
the result on rows the optimizer never saw, check what changed, and save
the compiled module.

## Setup

``` r

library(dsprrr)
library(ellmer)

llm <- chat_openai(model = "gpt-6-luna")
search_llm <- chat_openai(
  model = "gpt-6-luna",
  params = params(reasoning_effort = "low")
)
```

`llm` runs and scores the module. `search_llm` thinks less, which makes
it cheaper, and is used only while searching. The output on this page
was recorded with `gpt-5-mini` (runs) and `gpt-4o-mini` (search) and is
replayed when the site is built, so `gpt-6-luna` may give different
answers and scores.

## Task, data and metric

The running example triages customer emails. The module returns three
fields: a category, a priority, and whether the email needs a reply.

``` r

email_classifier <- signature(
  inputs = list(
    input("email", description = "Customer email text")
  ),
  output_type = type_object(
    category = type_enum(
      values = c("complaint", "inquiry", "feedback", "spam")
    ),
    priority = type_enum(values = c("urgent", "normal", "low")),
    needs_response = type_boolean()
  ),
  instructions = "Classify customer emails by category and priority."
) |>
  module()
```

Optimizers learn from a training set. A separate test set, which no
optimizer sees, tells you how the result does on new emails:

``` r

email_trainset <- tibble::tibble(
  email = c(
    "Your product broke after 2 days! I want a refund immediately!",
    "Hi, I'm wondering if you ship to Canada?",
    "Just wanted to say your customer service is excellent.",
    "CLICK HERE FOR FREE PRIZES!!! Limited time offer!!!"
  ),
  category = c("complaint", "inquiry", "feedback", "spam"),
  priority = c("urgent", "normal", "low", "low"),
  needs_response = c(TRUE, TRUE, FALSE, FALSE)
)

test_emails <- tibble::tibble(
  email = c(
    "The package was damaged during shipping.",
    "Do you offer student discounts?",
    "Your team went above and beyond. Thank you!"
  ),
  category = c("complaint", "inquiry", "feedback"),
  priority = c("normal", "low", "low"),
  needs_response = c(TRUE, TRUE, FALSE)
)
```

Seven rows are enough to show the calls, not to trust the scores;
[Metrics and
evaluation](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md)
covers how much data a comparison needs. If your labeled rows are in one
table, `split_dataset(data, prop = 0.8, seed = 1)` returns a `train` and
a `val` part.

dsprrr calls a metric with the module’s output and the whole data row,
so `field` names both the output field and the column to compare it
with:

``` r

category_metric <- metric_exact_match(field = "category")

category_metric(
  list(category = "spam", priority = "low", needs_response = FALSE),
  email_trainset[4, ]
)
#> [1] TRUE
```

This metric checks the category only. The evaluation below shows what
that leaves out.

Before compiling anything, score the module as it is. That baseline is
the number a compiled module has to beat:

``` r

baseline <- email_classifier |>
  evaluate(test_emails, metric = category_metric, .llm = llm)
baseline$mean_score
```

## Compile with LabeledFewShot

`LabeledFewShot` samples `k` training rows and attaches them to the
module as demos. It makes no model calls, and it does not score its
choices: the metric’s `field` only tells it which column holds each
demo’s output.

``` r

optimized_simple <- email_classifier |>
  compile(LabeledFewShot(k = 3L, metric = category_metric), email_trainset)

str(optimized_simple$demos[[1]])
#> List of 2
#>  $ inputs:List of 1
#>   ..$ email: chr "Just wanted to say your customer service is excellent."
#>  $ output: chr "feedback"
```

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
returns a new module and leaves its input alone. The optimization record
lists the training rows that became demos:

``` r

optimized_simple$is_compiled()
#> [1] TRUE
email_classifier$is_compiled()
#> [1] FALSE
optimization_result(optimized_simple)$lineage$selected_rows
#> [1] 3 4 1
```

Each demo carries one output column, here `category`. The model sees
three examples of how an email maps to a category and none of how
priority is set.

## Search instructions with GridSearchTeleprompter

`GridSearchTeleprompter` scores a fixed set of variants and keeps the
best. Each row of `variants` is one candidate: `instructions_suffix` is
appended to the module’s instructions, an `instructions` column replaces
them, and a `template` column replaces the prompt template. It also
attaches `k` demos drawn from the training rows.

``` r

email_variants <- data.frame(
  id = c("professional", "friendly", "analytical"),
  instructions_suffix = c(
    ". Use professional business judgment.",
    ". Consider customer satisfaction and relationship.",
    ". Analyze linguistic patterns and intent markers."
  )
)

grid_search <- GridSearchTeleprompter(
  variants = email_variants,
  metric = category_metric,
  k = 2L
)

# The validation rows and demos are drawn with R's random number generator
set.seed(37)
optimized_grid <- email_classifier |>
  compile(grid_search, email_trainset, .llm = search_llm)

optimized_grid |>
  run(
    email = "I've been waiting for 2 hours on hold! This is unacceptable!",
    .llm = llm
  )
#> $category
#> [1] "complaint"
#> 
#> $priority
#> [1] "urgent"
#> 
#> $needs_response
#> [1] TRUE
```

The search ran on `search_llm`, and the result is used with `llm`. A
variant wins because it suited the model that ran the search, so confirm
the result with the model you will deploy, on held-out rows.

## Score on held-out data

[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
runs every test row and scores it:

``` r

results <- optimized_grid |>
  evaluate(test_emails, metric = category_metric, .llm = llm)
results$mean_score
#> [1] 1
results$scores
#> [1] 1 1 1

strict_metric <- metric_field_match(
  c("category", "priority", "needs_response")
)
strict_results <- optimized_grid |>
  evaluate(test_emails, metric = strict_metric, .llm = llm)
strict_results$mean_score
#> [1] 0.3333333
```

Every category is right, but only one email in three is right on all
three fields. The predictions show where the difference comes from:

``` r

tibble::tibble(
  email = test_emails$email,
  expected = test_emails$priority,
  predicted = vapply(results$predictions, `[[`, character(1), "priority")
)
#> # A tibble: 3 × 3
#>   email                                       expected predicted
#>   <chr>                                       <chr>    <chr>    
#> 1 The package was damaged during shipping.    normal   urgent   
#> 2 Do you offer student discounts?             low      normal   
#> 3 Your team went above and beyond. Thank you! low      low
```

Both misses are priorities. Neither the metric nor the demos covered
priority, so the search had no way to improve it: an optimizer improves
only what its metric measures. Compare `results$mean_score` with the
baseline from before, on the same rows, and keep the compiled module
only if it wins there.

## Inspect what changed

[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md)
reports the same fields for every optimizer: `best_score`,
`best_params`, `trials`, `lineage`, `budget`, `stop_reason`, and
optimizer-specific details under `extensions`.

``` r

res <- optimization_result(optimized_grid)
res
#> <dsprrr_optimization_result>
#> Optimizer: GridSearchTeleprompter
#> Status: completed
#> Best score: 1
#> Trials: 3
#> Stopped: completed
res$best_params$id
#> [1] "professional"
res$trials[c("trial_id", "score", "n_evaluated")]
#> # A tibble: 3 × 3
#>   trial_id score n_evaluated
#>      <int> <dbl>       <int>
#> 1        1     1           1
#> 2        2     1           1
#> 3        3     1           1
```

Each variant was scored on a single email, so all three tied and the
first one won. Without a `valset`, `GridSearchTeleprompter` holds out a
random fifth of `trainset` (rounded up, at most `eval_sample_size` rows)
to score the variants, which is one row here. With real data, pass a
validation set large enough to separate the candidates:

``` r

splits <- split_dataset(labeled_emails, prop = 0.8, seed = 1)

optimized_grid <- email_classifier |>
  compile(grid_search, splits$train, valset = splits$val, .llm = search_llm)
```

The winning instructions are in `optimized_grid$signature@instructions`,
and the demos in `optimized_grid$demos`.

## Save and reload

[`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md)
writes a compiled module to disk, and
[`load_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md)
reads it back. The file holds the signature, instructions, demos and
optimization record. Chats and credentials are not saved, so pass `.llm`
when you run the restored module. Here the `LabeledFewShot` module from
earlier makes the round trip:

``` r

path <- tempfile(fileext = ".rds")
save_program(optimized_simple, path)

restored <- load_program(path)
restored
#> 
#> ── PredictModule ──
#> 
#> ── Signature
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • email: "string" - Customer email text
#> 
#> ── Output
#> Type: "object(category: enum(complaint, inquiry, feedback, spam), priority:
#> enum(urgent, normal, low), needs_response: boolean)"
#> 
#> ── Instructions
#> Classify customer emails by category and priority.
#> 
#> ── Demos
#> 3 demonstration(s) loaded
#> 
#> ── Compilation Status
#> ✔ Compiled
#> Teleprompter: LabeledFewShot
```

[Tutorial
6](https://jameshwade.github.io/dsprrr/articles/tutorial-deploy-to-production.md)
covers saving and reusing modules in more depth.

## Other optimizers

Every teleprompter plugs into the same call. `BootstrapFewShot`, for
example, runs the module on training rows and keeps its own outputs that
pass the metric as demos. Those demos carry all three output fields,
where the labeled ones above carried only the category:

``` r

bootstrapped <- email_classifier |>
  compile(
    BootstrapFewShot(metric = category_metric, max_labeled_demos = 0L),
    email_trainset,
    .llm = llm
  )
```

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
always returns a copy. To tune settings such as reasoning effort on the
module itself, use
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md),
which updates the module in place ([Tutorial
5](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md)
walks through it).

[Choose an
optimizer](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md)
compares the eleven teleprompters and shows how to call each one.
