# Evaluate with vitals

[vitals](https://vitals.tidyverse.org) is Posit’s package for evaluating
LLM applications: a `Task` runs a solver over a dataset, grades each
answer with a scorer, and writes a log you can browse. This page shows
how to evaluate a dsprrr module as a vitals solver, choose a scorer,
read the results, and use vitals scorers as dsprrr metrics. The code
uses `gpt-6-luna`; the outputs below are recorded responses from
`gpt-4o-mini`, replayed when the site is built.

``` r

library(dsprrr)
library(vitals)
library(ellmer)
library(tibble)
```

## Evaluate a module

[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md)
turns a module and a data frame into a vitals `Task`. The data needs one
column per signature input plus a `target` column with the expected
answer:

``` r

classifier <- signature("input -> label: enum('positive', 'negative')") |>
  module()

test_data <- tibble(
  input = c(
    "I love this product!",
    "Terrible experience, waste of money"
  ),
  target = c("positive", "negative")
)

task <- as_vitals_task(
  module = classifier,
  dataset = test_data,
  scorer = detect_match(),
  name = "sentiment",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-6-luna")
)

task$eval()
#> ℹ Solving
#> ✔ Solving [1.8s]
#> 
#> ℹ Scoring
#> ✔ Scoring [92ms]
#> 
task$get_samples()
#> # A tibble: 2 × 9
#>   input            target      id result   solver_chat score scorer_metadata 
#>   <list>           <chr>    <int> <chr>    <list>      <ord> <list>          
#> 1 <tibble [1 × 1]> positive     1 positive <Chat>      C     <named list [2]>
#> 2 <tibble [1 × 1]> negative     2 negative <Chat>      C     <named list [2]>
#> # ℹ 2 more variables: scorer_explanation <chr>, scorer <chr>
```

`$eval()` runs the module on every row, scores the answers and writes a
log to `dir`; `vitals_view(dir)` opens the log viewer. In
`$get_samples()`, `result` holds the module’s output. When the signature
has one string or enum output, that is the plain value; otherwise it is
all output fields as JSON. `score` is `C` (correct) or `I` (incorrect).
Leave out `scorer` and you get
[`model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html).

## Evaluate on held-out data

To check a compiled module, compile on training rows and evaluate on
rows it has not seen. `LabeledFewShot(k = 2L)` adds two randomly sampled
training rows to the prompt as examples:

``` r

qa_data <- tibble(
  question = c(
    "What is the capital of France?",
    "Who wrote Romeo and Juliet?",
    "What is 2 + 2?",
    "What color is the sky?",
    "Who painted the Mona Lisa?",
    "What is the largest planet?"
  ),
  target = c(
    "Paris",
    "Shakespeare",
    "4",
    "Blue",
    "Leonardo da Vinci",
    "Jupiter"
  )
)

set.seed(42)
train_idx <- sample(nrow(qa_data), 4)
trainset <- qa_data[train_idx, ]
testset <- qa_data[-train_idx, ]

qa_module <- signature("question -> answer") |>
  module()

optimized <- compile(
  qa_module,
  LabeledFewShot(k = 2L),
  trainset = trainset
)

test_task <- as_vitals_task(
  module = optimized,
  dataset = testset,
  scorer = detect_match(),
  name = "qa-test",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-6-luna")
)

test_task$eval()
#> ℹ Solving
#> ✔ Solving [802ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [63ms]
#> 

test_task$get_samples()[c("target", "result", "score")]
#> # A tibble: 2 × 3
#>   target      result              score
#>   <chr>       <chr>               <ord>
#> 1 Shakespeare William Shakespeare C    
#> 2 4           4                   C
```

The model answered “William Shakespeare” and still passed.
[`detect_match()`](https://vitals.tidyverse.org/reference/scorer_detect.html)
looks for the target at the end of the answer by default
(`location = "end"`), ignoring case, so “Shakespeare wrote it” would
have failed. Use `location = "exact"`, `"begin"` or `"any"` when that
matters.

## Choose a scorer

| Scorer | Marks an answer correct when | Good for |
|----|----|----|
| [`detect_match()`](https://vitals.tidyverse.org/reference/scorer_detect.html) | the target appears at the end of the answer (see `location`) | short factual answers and labels |
| [`detect_includes()`](https://vitals.tidyverse.org/reference/scorer_detect.html) | the target appears anywhere in the answer | a word or phrase that must be present |
| [`detect_pattern()`](https://vitals.tidyverse.org/reference/scorer_detect.html) | text captured by a regular expression matches the target | pulling a number or code out of longer text |
| [`model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html) | a grader model judges that the answer meets the target | open-ended answers |

The examples above use
[`detect_match()`](https://vitals.tidyverse.org/reference/scorer_detect.html).
For explanations,
[`model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html)
asks a model whether the answer meets the target, treated as a
criterion. Without a `scorer_chat` it grades with a fresh copy of the
solver’s Chat, so the model grades its own answers.
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
adds a `reasoning` output, so `result` is JSON with both fields and the
grader sees the reasoning too:

``` r

explainer <- signature("topic -> explanation") |>
  chain_of_thought()

task <- as_vitals_task(
  module = explainer,
  dataset = tibble(
    topic = c("Why is the sky blue?", "How do plants make food?"),
    target = c(
      "Light scattering in atmosphere",
      "Photosynthesis using sunlight"
    )
  ),
  scorer = model_graded_qa(),
  name = "explanations",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-6-luna")
)

task$eval()
#> ℹ Solving
#> ✔ Solving [771ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [294ms]
#> 
task$get_samples()
#> # A tibble: 2 × 10
#>   input    target        id result solver_chat solver_metadata score scorer_chat
#>   <list>   <chr>      <int> <chr>  <list>      <list>          <ord> <list>     
#> 1 <tibble> Light sca…     1 "{\"r… <Chat>      <named list>    C     <Chat>     
#> 2 <tibble> Photosynt…     2 "{\"r… <Chat>      <named list>    C     <Chat>     
#> # ℹ 2 more variables: scorer_metadata <list>, scorer <chr>
```

[`detect_includes()`](https://vitals.tidyverse.org/reference/scorer_detect.html)
checks that the target text appears anywhere. It is literal: the prompt
below never says which language to use, the model wrote Python
(`def add_numbers`), and the first row fails because “function” is
missing:

``` r

coder <- signature("task -> code") |> module()

task <- as_vitals_task(
  module = coder,
  dataset = tibble(
    task = c(
      "Write a function to add two numbers",
      "Create a loop that prints 1 to 5"
    ),
    target = c("function", "for")
  ),
  scorer = detect_includes(),
  name = "code-gen",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-6-luna")
)

task$eval()
#> ℹ Solving
#> ✔ Solving [784ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [67ms]
#> 
task$get_samples()
#> # A tibble: 2 × 9
#>   input            target      id result       solver_chat score scorer_metadata
#>   <list>           <chr>    <int> <chr>        <list>      <ord> <list>         
#> 1 <tibble [1 × 1]> function     1 "def add_nu… <Chat>      I     <named list>   
#> 2 <tibble [1 × 1]> for          2 "for i in r… <Chat>      C     <named list>   
#> # ℹ 2 more variables: scorer_explanation <chr>, scorer <chr>
```

## Read the results

`$get_samples()` returns one row per sample. `input` is a list column
holding each row’s signature inputs, so pull fields out of it to build a
readable table:

``` r

sentiment <- signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')"
) |>
  module()

dataset <- tibble(
  text = c(
    "Best purchase ever!",
    "Complete garbage",
    "It's okay I guess",
    "Amazing quality!",
    "Never buying again"
  ),
  target = c("positive", "negative", "neutral", "positive", "negative")
)

task <- as_vitals_task(
  module = sentiment,
  dataset = dataset,
  scorer = detect_match(),
  name = "sentiment-analysis",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-6-luna")
)

task$eval()
#> ℹ Solving
#> ✔ Solving [1.9s]
#> 
#> ℹ Scoring
#> ✔ Scoring [115ms]
#> 

samples <- task$get_samples()
tibble(
  text = vapply(samples$input, \(x) x$text, character(1)),
  target = samples$target,
  result = samples$result,
  score = samples$score
)
#> # A tibble: 5 × 4
#>   text                target   result   score
#>   <chr>               <chr>    <chr>    <ord>
#> 1 Best purchase ever! positive positive C    
#> 2 Complete garbage    negative negative C    
#> 3 It's okay I guess   neutral  neutral  C    
#> 4 Amazing quality!    positive positive C    
#> 5 Never buying again  negative negative C

mean(samples$score == "C")
#> [1] 1
```

Filter on `score == "I"` to list the failures.

## Compare two modules

Run the same data through each variant and compare the share of correct
answers. Here a plain module and a chain-of-thought module answer two
word problems, graded by
[`model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html):

``` r

sig <- signature("question -> answer")
basic <- module(sig)
cot <- chain_of_thought(sig)

test_data <- tibble(
  question = c(
    "If a train travels 60 mph for 2 hours, how far does it go?",
    "What is 15% of 80?"
  ),
  target = c("120 miles", "12")
)

llm <- chat_openai(model = "gpt-6-luna")

results <- list()
for (name in c("basic", "cot")) {
  mod <- if (name == "basic") basic else cot

  task <- as_vitals_task(
    module = mod,
    dataset = test_data,
    scorer = model_graded_qa(),
    name = paste0("math-", name),
    dir = tempdir(),
    .llm = llm
  )
  task$eval()

  results[[name]] <- mean(task$get_samples()$score == "C")
}
#> ℹ Solving
#> ✔ Solving [756ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [229ms]
#> 
#> ℹ Solving
#> ✔ Solving [777ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [272ms]
#> 

tibble(variant = names(results), accuracy = unlist(results))
#> # A tibble: 2 × 2
#>   variant accuracy
#>   <chr>      <dbl>
#> 1 basic          1
#> 2 cot            1
```

Both variants get both problems right, so two questions can’t separate
them. Use enough rows that a difference would show.

## Repeat samples with epochs

A model can answer the same prompt differently from run to run. `epochs`
repeats every row so you can see how stable the scores are. Pass
`.cache = FALSE` (forwarded to
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md))
so each repeat makes a new request; with dsprrr’s response cache on, the
repeats would replay the first answer:

``` r

classifier <- signature(
  "text -> category: enum('tech', 'sports', 'politics')"
) |>
  module()

test_data <- tibble(
  text = c(
    "New iPhone released today",
    "Lakers win championship",
    "Senate passes new bill"
  ),
  target = c("tech", "sports", "politics")
)

task <- as_vitals_task(
  module = classifier,
  dataset = test_data,
  scorer = detect_match(),
  name = "news-classification",
  epochs = 3L,
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-6-luna"),
  .cache = FALSE
)

task$eval()
#> ℹ Solving
#> ✔ Solving [3s]
#> 
#> ℹ Scoring
#> ✔ Scoring [172ms]
#> 

scores <- task$get_samples()
nrow(scores)
#> [1] 9
table(scores$target, scores$score)
#>           
#>            I C
#>   politics 0 3
#>   sports   0 3
#>   tech     0 3
```

Three rows times three epochs gives nine samples, and each headline got
the same label every time.

For larger datasets, add
`.concurrency = concurrency_control(backend = "ellmer", max_active = 4L)`
to
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md)
to run rows in parallel. Concurrent rows skip dsprrr’s response cache.

## Build the Task yourself

[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md)
wraps a module as a vitals solver, for when you want to call
`Task$new()` directly. A module whose only input is named `input` reads
vitals’ `input` column as is; for other or multiple inputs, use
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md),
which packs them into that column for you.

``` r

triage <- module(
  signature("input -> priority: enum('low', 'medium', 'high')")
)
tickets <- tibble(
  input = c("Checkout fails for every customer", "Typo on the About page"),
  target = c("high", "low")
)

task <- Task$new(
  dataset = tickets,
  solver = as_vitals_solver(triage, .llm = chat_openai(model = "gpt-6-luna")),
  scorer = detect_match(location = "exact"),
  dir = tempdir()
)
task$eval()
```

## Use vitals scorers as dsprrr metrics

[`metric_detect_match()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md),
[`metric_detect_includes()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
and
[`metric_detect_pattern()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
wrap the matching vitals scorers as dsprrr metrics, so
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
and
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
can use them. They take the expected answer from the data’s `target`
column, return 1 or 0, and work with modules that have a single output
field:

``` r

ends_with_target <- metric_detect_match()
exact_target <- metric_detect_match(location = "exact")
row <- tibble(question = "Who wrote Hamlet?", target = "Shakespeare")

ends_with_target(list(answer = "William Shakespeare"), row)
#> [1] 1
exact_target(list(answer = "William Shakespeare"), row)
#> [1] 0
```

In an evaluation, pass the metric like any other:

``` r

evaluate(
  qa_module,
  testset,
  metric = metric_detect_match(),
  .llm = chat_openai(model = "gpt-6-luna")
)
```

[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)
adapts other vitals-style scorers the same way.
[`metric_model_graded_qa()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
and
[`metric_model_graded_fact()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
exist too, but they currently show the grader the prediction as R list
syntax and no question unless the data has an `input` column; for model
grading, use
[`model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html)
in a vitals Task as shown above. For how metrics drive optimization, see
[Metrics and
evaluation](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md).
