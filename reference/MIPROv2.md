# MIPROv2: search instructions and demonstrations together

`MIPROv2()` builds a few candidate demonstration sets and candidate
instructions, then searches their combinations: most trials score a
small minibatch of training rows, and every few trials a combination is
scored on the full validation set.
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
returns a copy of the program with the best combination.

## Usage

``` r
MIPROv2(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  task_model = NULL,
  teacher_settings = NULL,
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 4L,
  auto = "light",
  num_candidates = NULL,
  num_threads = 1L,
  seed = 9L,
  track_stats = TRUE,
  log_dir = NULL
)
```

## Arguments

- metric:

  A metric function (required), such as
  `metric_exact_match(field = "answer")`.

- metric_threshold:

  Passed to the
  [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
  demonstration runs: the minimum score for a bootstrapped output to
  become a demonstration. `NULL` (the default) keeps any output that
  scores above 0.

- max_errors:

  Integer; stop after this many consecutive failed evaluations when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  gets no `control` (default `5L`).

- task_model:

  Optional ellmer Chat used to score candidates during the search.
  `NULL` (the default) uses the `.llm` passed to
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).
  The demonstration runs always use `.llm`, and the compiled program
  does not keep `task_model`.

- teacher_settings:

  Passed to the
  [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
  demonstration runs, where it is currently not applied.

- max_bootstrapped_demos, max_labeled_demos:

  Integer settings for the demonstration candidates (defaults `4L` and
  `4L`), with the same meaning as in
  [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md).

- auto:

  Search preset: `"light"` (the default), `"medium"`, `"heavy"`, or
  `NULL` to size the search with `num_candidates`. See Presets.

- num_candidates:

  Used only when `auto = NULL`: sets both the number of trials and the
  number of instruction candidates.

- num_threads:

  Integer number of rows scored at the same time (default `1L`). Ignored
  when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  gets a `control`.

- seed:

  Integer seed (default `9L`) for the candidate order, minibatches, tip
  choice and bootstrap seeds. It must be an integer such as `42L`; a
  double such as `42` currently makes compilation fail.

- track_stats:

  Whether to keep the trial history in `optimization_result()$trials`
  (default `TRUE`).

- log_dir:

  Directory for a
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)
  with one trial per search step, or `NULL` (the default).

## Value

A `MIPROv2` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

### Candidates

- Demonstration sets: one
  [`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md)
  set of `max_labeled_demos` rows, plus
  [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
  runs with different seeds. Because BootstrapFewShot does not currently
  shuffle rows, the bootstrapped sets are often identical.

- Instructions: the program's own instructions, plus variants that
  append a short dataset summary (the input and output column names) and
  one tip from a fixed list: "Be concise and accurate.", "Reason
  step-by-step before answering.", "Only use information in the
  inputs.", "Avoid assumptions; stick to the data." and "Return only the
  final output field." Unlike DSPy's MIPROv2, no model writes
  instructions, so there are at most six distinct instruction
  candidates.

### Search

Every pairing of a demonstration set and an instruction is a candidate.
DSPy searches these with Bayesian optimization; dsprrr uses a UCB1
bandit. Each candidate is first tried once, in random order. After that,
each trial picks the candidate with the highest mean score plus an
exploration bonus of `sqrt(2 * log(t) / n)`, where `t` is the trial
number and `n` is how often the candidate was tried. Most trials score a
random minibatch of `trainset`; at a fixed interval (see Presets) a
trial scores the whole `valset` instead, or `trainset` when no `valset`
is given. The winner is the candidate with the best full evaluation, or,
with a warning, the best minibatch score when no full evaluation
finished.

### Presets

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `auto` | Trials | Minibatch rows | Full evaluation every | Demo sets | Instructions |
| `"light"` (default) | 20 | 5 | 5 trials | 3 | 5 |
| `"medium"` | 50 | 10 | 10 trials | 5 | 6 |
| `"heavy"` | 100 | 20 | 20 trials | 7 | 6 |
| `NULL` | `num_candidates` (20 if `NULL`) | 10 | 5 trials | 4 | `num_candidates`, at most 6 |

Demo sets and instructions are upper bounds: a bootstrap run that
collects no demonstrations adds no set. Minibatches never exceed
`nrow(trainset)`. The training set needs more rows than
`max_labeled_demos`: otherwise the run currently stops before the first
trial and returns the program unchanged, with status `"partial"` in
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md).

### Nested predictors

For programs with nested predictors, such as an RLM, MIPROv2 tunes each
inner predictor's instructions and keeps their demonstrations. Set
`max_bootstrapped_demos = 0L` for these programs.

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
tp <- MIPROv2(
  metric = metric_exact_match(field = "answer"),
  auto = "light",
  max_labeled_demos = 2L,
  max_bootstrapped_demos = 2L
)
tp
#> 
#> ── MIPROv2 Teleprompter 
#> auto: light
#> max_bootstrapped_demos: 2
#> max_labeled_demos: 2
#> seed: 9
#> num_threads: 1
#> metric: <function>

if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
# Use a few dozen rows in practice
trainset <- data.frame(
  question = c(
    "Capital of France?", "Capital of Peru?", "Capital of Chad?",
    "Capital of Cuba?", "Capital of Laos?", "Capital of Oman?"
  ),
  answer = c("Paris", "Lima", "N'Djamena", "Havana", "Vientiane", "Muscat")
)
compiled <- compile(
  qa,
  tp,
  trainset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
optimization_result(compiled)
} # }
```
