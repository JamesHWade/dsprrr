# Metrics and evaluation

A metric turns “this output looks right” into a number you can compare
across prompts, models and optimizer runs. This page shows how dsprrr
calls a metric, which built-in metric fits which task, what
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
reports, and how to keep a score honest.

``` r

library(dsprrr)
```

## Why reading outputs is not enough

Most prompts are tuned by trying a few inputs and reading the answers.
That misleads in predictable ways. After a tweak you re-test the example
that was failing, not the ones that already worked. Five hand-picked
inputs rarely look like production data. A new prompt can read
differently without being more accurate, and “good enough” depends on
who is reading and when.

A metric applied to the same labeled rows every time gives a score that
is reproducible, comparable between versions, and cheap to recompute
after every change. Whether the score means anything still depends on
the rows and on the metric, which the rest of this page covers.

## How dsprrr calls a metric

A metric is a function `metric(prediction, expected)` that returns
`TRUE` or `FALSE`, or a number between 0 and 1. When
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
or
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
call it, `prediction` is what
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) returned
for one row (a named list such as `list(sentiment = "positive")`) and
`expected` is that row of your data, as a one-row data frame. Built-in
metrics take `field` to say which output to compare with the column of
the same name:

``` r

metric <- metric_exact_match(field = "sentiment")
row <- tibble::tibble(text = "I love this product", sentiment = "positive")

metric(list(sentiment = "positive"), row)
#> [1] TRUE
metric(list(sentiment = "negative"), row)
#> [1] FALSE
```

## Built-in metrics

### Exact match

For classification and extraction, where the answer must be exactly
right. Called directly on two values, a metric compares them as given.
Whitespace is normalized, and case matters unless you set
`ignore_case = TRUE`:

``` r

exact <- metric_exact_match()
exact("positive", "positive")
#> [1] TRUE
exact("Positive", "positive")
#> [1] FALSE
metric_exact_match(ignore_case = TRUE)("Positive", "positive")
#> [1] TRUE
```

### Contains

[`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md)
checks that an output contains a fixed pattern, or matches a regular
expression with `fixed = FALSE`. It ignores `expected`, so it suits
checks that are the same for every row, such as a required keyword or
disclaimer:

``` r

mentions_refund <- metric_contains("refund", field = "reply", ignore_case = TRUE)
mentions_refund(list(reply = "We have issued a full Refund."))
#> [1] TRUE
mentions_refund(list(reply = "Your order has shipped."))
#> [1] FALSE
```

### F1 score

For free-text answers where partial overlap deserves partial credit:

``` r

metric_f1()("the quick brown fox", "the fast brown fox")
#> [1] 0.75
```

The strings share three of their four tokens (the, brown, fox), so
precision and recall are both 3/4 and F1 is 0.75.

### Custom metrics

Any function with the same two arguments works. Read fields from both
sides by name:

``` r

confident_match <- function(prediction, expected) {
  prediction$sentiment == expected$sentiment && prediction$confidence >= 0.8
}

row <- tibble::tibble(text = "Great product", sentiment = "positive")
confident_match(list(sentiment = "positive", confidence = 0.9), row)
#> [1] TRUE
confident_match(list(sentiment = "positive", confidence = 0.6), row)
#> [1] FALSE
```

### Trace-aware metrics

Sometimes output correctness is not enough. You may also need to score
whether the program used tools, stayed within an execution policy, or
produced a useful trajectory for a reflective optimizer.
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md)
marks a three-argument metric that receives a program-trace envelope:

``` r

metric <- metric_with_trace(
  function(prediction, expected, program_trace) {
    correct <- identical(prediction$sentiment, expected$sentiment)
    list(
      score = as.numeric(correct),
      feedback = paste(
        "Evaluation status:",
        program_trace$status,
        "- events:",
        length(program_trace$events)
      )
    )
  },
  field = "sentiment"
)
```

The envelope contains `row_id`, `epoch`, `status`, ordered module
`events`, and per-row `metadata`. The metric may return a scalar score
or `list(score = ..., feedback = ...)`, so the same function can guide
reflective optimizers. Ordinary two-argument metrics continue to work
unchanged.

Trace events can contain prompts, inputs, and model responses. Treat
them as potentially sensitive when storing or exporting evaluation
results.

## Running an evaluation

[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
runs a module on every row of a data frame and scores each prediction:

``` r

llm <- ellmer::chat_openai(model = "gpt-6-luna")
mod <- module(signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')"
))

test_data <- tibble::tibble(
  text = c("Great product!", "Terrible service", "It's okay"),
  sentiment = c("positive", "negative", "neutral")
)

result <- evaluate(
  mod,
  data = test_data,
  metric = metric_exact_match(field = "sentiment"),
  .llm = llm
)
```

The result is a list:

| Element | Contents |
|----|----|
| `mean_score` | The mean score over all rows; rows that failed count as 0 |
| `scores` | One score per row |
| `predictions` | What the module returned for each row |
| `n_evaluated`, `n_errors` | Rows scored, and rows where the run or the metric failed |
| `errors` | The error messages, if any |
| `traces` | One trace envelope per row |

## Metric-driven development

### 1. Build the evaluation set first

Before writing any prompt, collect labeled examples:

``` r

eval_data <- tibble::tibble(
  question = c(
    "What is 2+2?",
    "What is the capital of France?",
    "Who wrote Romeo and Juliet?"
  ),
  answer = c("4", "Paris", "Shakespeare")
)
```

This forces you to define success up front: what counts as a correct
answer, and which edge cases matter.

### 2. Establish a baseline

``` r

base_sig <- signature(
  "question -> answer",
  instructions = "Answer the question directly."
)
mod_v1 <- module(base_sig)
metric <- metric_exact_match(field = "answer")

baseline <- evaluate(mod_v1, eval_data, metric, .llm = llm)
baseline$mean_score
```

That number is the one to beat.

### 3. Measure every change

``` r

concise_sig <- append_instructions(base_sig, "Use at most five words.")
mod_v2 <- module(concise_sig)
v2 <- evaluate(mod_v2, eval_data, metric, .llm = llm)

mod_v3 <- mod_v2$copy()
mod_v3$demos <- list(
  list(inputs = list(question = "What is 3+3?"), output = "6")
)
v3 <- evaluate(mod_v3, eval_data, metric, .llm = llm)

c(v1 = baseline$mean_score, v2 = v2$mean_score, v3 = v3$mean_score)
```

[`append_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md)
and
[`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md)
return a new signature, so `base_sig` and `mod_v1` stay as they were.
`$copy()` gives `mod_v3` its own demos and an empty trace history;
`$clone()` would carry over the traces `mod_v2` has already recorded.

Model outputs vary from run to run. To see by how much, repeat the
evaluation with `epochs`. Each epoch makes fresh calls; running the same
evaluation again replays all epochs from the response cache, so pass
`.cache = FALSE` when you want new samples:

``` r

repeated <- evaluate(
  mod_v2,
  eval_data,
  metric,
  .llm = llm,
  epochs = 5L
)
repeated$ci_95
```

With `epochs` above one,
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
also returns `epoch_scores`, `score_std` (the standard deviation of the
per-epoch means) and `ci_95`, a 95% t-interval around the mean.
`$traces` holds the last epoch’s traces and `$epoch_traces` those of
every epoch. The interval measures how much the score moves between
repeated runs on the same rows; it says nothing about how the score
would change on different rows.

### 4. Hold out a test set

Before optimizing, split your labeled data (`all_data` here) three ways.
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md)
returns a `train` and a `val` part, so call it twice:

``` r

outer <- split_dataset(all_data, prop = 0.8, seed = 42)
testset <- outer$val

inner <- split_dataset(outer$train, prop = 0.75, seed = 42)
trainset <- inner$train
valset <- inner$val
```

That leaves 60% of the rows for training, 20% for validation and 20% for
the final test. Optimizers draw demonstrations from the training rows
and can compare candidates on the validation rows, passed as
`compile(..., valset = valset)`. Score the test rows once, at the end.

### 5. Let an optimizer search

Once the score is trustworthy, an optimizer can try variants for you:

``` r

optimize_grid(
  mod_v2,
  data = trainset,
  metric = metric,
  parameters = list(
    reasoning_effort = c("none", "low"),
    instructions_suffix = c("Answer in one word.", "Give only the name or number.")
  ),
  .llm = llm
)
optimization_result(mod_v2)$best_score

final <- evaluate(mod_v2, testset, metric, .llm = llm)
final$mean_score
```

[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
scores every combination on `trainset` and updates `mod_v2` in place
with the best one. It can only find what the metric rewards, so a metric
that misses part of the task produces a prompt that misses it too.

## Choosing a metric

| Task | Metric | Why |
|----|----|----|
| Classification | `metric_exact_match(field = "label")` | The answer must be exactly right |
| Extracting one value | `metric_exact_match(field = "amount")` | Compares one output with the column of the same name |
| Extracting several values | `metric_field_match(c("name", "date"))` | Every listed field must match |
| Free-text answers | `metric_f1(field = "answer")` | Partial credit for word overlap |
| Pass/fail on a graded score | `metric_threshold(metric_f1(field = "answer"), 0.8)` | Counts a row as correct above a cutoff |
| Yes/no questions | `metric_exact_match(field = "answer", ignore_case = TRUE)` | “Yes” and “yes” count as equal |
| Required content | `metric_contains("refund", field = "reply")` | The same fixed pattern for every row; ignores the expected value |
| Anything else | A custom function | Domain-specific rules |

### When exact match is too strict

Equivalent answers can look different:

``` r

exact <- metric_exact_match()
exact("4", "four")
#> [1] FALSE
exact("Paris", "paris")
#> [1] FALSE
exact("$100", "100 dollars")
#> [1] FALSE
```

Normalize outputs before comparing, set `ignore_case = TRUE`, constrain
the output type in the signature (an `int` output cannot come back as
“four”), write a custom metric that accepts the variants, or use F1 for
partial credit.

### When F1 is too lenient

F1 rewards shared words, even when the meaning is opposite:

``` r

metric_f1()("The answer is yes", "The answer is no")
#> [1] 0.75
```

When meaning matters more than wording, compare the key field exactly,
or have another model grade the output (see [Evaluate with
vitals](https://jameshwade.github.io/dsprrr/articles/vitals-recipes.md)).

## Common pitfalls

### Scoring on the rows you optimized

`optimization_result(mod)$best_score` is the score of the winning
candidate on the rows the search chose it with, so it is optimistic: it
measures how well the search fit those rows, not how the module will do
on new inputs. Report the score on rows the search never saw, like
`final` in step 5.

### A metric that does not fit the task

Exact match on generated prose, such as product descriptions, scores
nearly every output 0, so every prompt looks equally bad. Use F1 against
a reference text, a custom check for the properties you need, or a
model-graded rubric.

### Too few examples

With pass/fail scores, the uncertainty that comes from a small test set
is easy to compute. Here are exact binomial 95% intervals for 4 correct
answers out of 5, and for 40 out of 50:

``` r

round(binom.test(4, 5)$conf.int[1:2], 2)
#> [1] 0.28 0.99
round(binom.test(40, 50)$conf.int[1:2], 2)
#> [1] 0.66 0.90
```

Both score 0.8, but five rows are consistent with a true accuracy
anywhere from 0.28 to 0.99. Fifty rows narrow that to 0.66 to 0.90,
which is still wide enough to hide a five-point difference between two
prompts. Compare versions on the same rows, and grow the test set until
the differences you care about are larger than this range. The `ci_95`
from repeated epochs does not replace this check, because it only
reflects run-to-run variation on the same rows.

To watch a metric drive an optimizer end to end, continue with [Tutorial
5: Optimize a
module](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md).
