# BootstrapFewShot: keep the program's own successful outputs as demos

`BootstrapFewShot()` runs the program on training rows, scores each
output with `metric`, and keeps the outputs that pass as few-shot
demonstrations. Labeled training rows can be added as demonstrations
too. This is DSPy's basic demonstration optimizer and a building block
of
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md)
and
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md).

## Usage

``` r
BootstrapFewShot(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 16L,
  max_rounds = 1L,
  teacher_settings = NULL,
  seed = NULL,
  log_dir = NULL
)
```

## Arguments

- metric:

  A metric function (required). Use a field-aware metric such as
  `metric_exact_match(field = "answer")`: like
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
  it receives the whole training row as `expected`, and its `field`
  names the column that supplies labeled demonstrations. A metric
  without a `field` receives only the bare value of the label column,
  which is the first column named `output`, `label`, `answer`,
  `response`, `result` or `y`, or else the first non-input column.

- metric_threshold:

  Minimum score for a bootstrapped output to become a demonstration.
  `NULL` (the default) keeps any output that scores above 0.

- max_errors:

  Integer; stop after this many consecutive failed attempts when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  gets no `control` (default `5L`).

- max_bootstrapped_demos:

  Integer maximum number of bootstrapped demonstrations (default `4L`).

- max_labeled_demos:

  Integer number of leading training rows used as labeled demonstrations
  (default `16L`). See Details.

- max_rounds:

  Integer number of passes over the remaining rows (default `1L`).

- teacher_settings:

  A list of settings meant for the teacher, such as
  `list(temperature = 0.7)`. It is currently not applied: the teacher
  runs with the same chat and settings as the program.

- seed:

  Recorded with the run, but it does not currently change the result:
  the training rows are used in their original order. Shuffle the
  training set yourself to vary which rows are used.

- log_dir:

  Directory for a
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)
  of the run, or `NULL` (the default) for none. When `valset` is passed
  to
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
  the compiled program is scored on it for the log.

## Value

A `BootstrapFewShot` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

Compilation works through the training rows in their original order:

1.  The first `max_labeled_demos` rows become labeled demonstrations,
    copied from the data.

2.  A copy of the program (the teacher) runs on each remaining row,
    using the demonstrations collected so far. An output that passes the
    metric becomes a demonstration. This stops once
    `max_bootstrapped_demos` are collected.

3.  With `max_rounds` above `1L`, step 2 repeats over the same rows
    until enough demonstrations are collected.

The compiled copy gets the labeled demonstrations followed by the
bootstrapped ones. Rows used as labeled demonstrations are never
bootstrapped, so with the default `max_labeled_demos = 16L` a training
set of 16 rows or fewer yields labeled demonstrations only and makes no
model calls. Set `max_labeled_demos = 0L` to bootstrap from every row.

The teacher runs with the `.llm` passed to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
or with the chat the program would otherwise use.

### Joint pipeline compilation

When `program` is a pipeline (built with
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
or `%>>%`), BootstrapFewShot compiles the whole program jointly, like
DSPy: the teacher pipeline runs end-to-end on each training example, the
*final* output is scored with the metric, and when a run passes the
threshold every step's `(inputs, output)` pair from that trace is
harvested as a demonstration for the corresponding step module.
Intermediate steps therefore receive demos even though the training set
only labels the final output. Labeled demos (`max_labeled_demos`) are
applied to the final step only, and only when its input fields exist in
the trainset. Programs containing Flex or an RLM, at the root or nested,
are rejected. Flex constructs its inner predictors per invocation; RLM
root examples do not match its children's `state -> ...` signatures. Use
GEPA for these programs, or instruction-only MIPROv2 for an RLM graph.

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
tp <- BootstrapFewShot(
  metric = metric_exact_match(field = "answer"),
  max_bootstrapped_demos = 2L,
  max_labeled_demos = 0L
)
tp
#> 
#> ── BootstrapFewShot Teleprompter 
#> max_bootstrapped_demos: 2
#> max_labeled_demos: 0
#> max_rounds: 1

if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
trainset <- data.frame(
  question = c(
    "Capital of France?", "Capital of Japan?", "Capital of Peru?"
  ),
  answer = c("Paris", "Tokyo", "Lima")
)
compiled <- compile(
  qa,
  tp,
  trainset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
best_demos(compiled, as_tibble = TRUE)
} # }
```
