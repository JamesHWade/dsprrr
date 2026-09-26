# Investigate a regression with RLM

You will build a 40,000-row session table and a 200-row change log, ask
an RLM why checkout conversion fell after a release, and check its
answer against a calculation that does not involve the model. RLM suits
this kind of question: the answer is buried in a large object, and you
do not know in advance which slice of it to look at. The model inspects
the data with R code and only sees the results it prints.

The data are built without random numbers, so the right answer is fixed:

``` text
release:      2.4.0
cohort:       platform=mobile / plan=pro
before_rate:  0.92
after_rate:   0.61
drop_pp:      31
change_id:    CHG-1842
evidence:     Mobile Pro token refresh: retry budget changed from 3 to 0 and timeout from 8 s to 800 ms.
```

The model has to discover which categorical columns define the affected
cohort, calculate the drop, and find the change record that best
explains it. Of the session table (about 3.6 MB as row-oriented JSON),
only a short [`str()`](https://rdrr.io/r/utils/str.html) preview goes
into the prompt. [Check the answer
independently](#check-the-answer-independently) computes the same answer
without a model.

## Where RLM fits

The task needs exact aggregation over a table too large to paste into a
prompt, a grouping the question does not name, and a judgment about one
short piece of text.
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) or
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
would have to receive the data in the prompt.
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md)
fits when you already know which calculation to run. Once an
investigation like this becomes routine, a plain R function is the
better tool. RLM covers the exploratory middle: the model decides what
to inspect, R does the arithmetic, and the model revises its plan from
the results.

## Build the incident data

Every cohort has exactly 5,000 sessions per release. Only mobile Pro
users on release 2.4.0 convert less often:

``` r

n <- 5000L

sessions <- expand.grid(
  release = c("2.3.9", "2.4.0"),
  platform = c("desktop", "mobile"),
  plan = c("free", "pro"),
  within_group = seq_len(n),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

base_rate <- c(
  "desktop.free" = 0.88,
  "mobile.free" = 0.84,
  "desktop.pro" = 0.94,
  "mobile.pro" = 0.92
)

cohort_key <- paste(sessions$platform, sessions$plan, sep = ".")
conversion_rate <- unname(base_rate[cohort_key])
conversion_rate[
  sessions$release == "2.4.0" & cohort_key == "mobile.pro"
] <- 0.61

sessions$converted <-
  sessions$within_group <= n * conversion_rate
```

Most change records are routine. One describes a checkout-authentication
change for the affected release and cohort:

``` r

changes <- data.frame(
  change_id = sprintf("CHG-%04d", 1701:1900),
  release = rep(c("2.3.9", "2.4.0"), each = 100),
  component = "miscellaneous",
  note = "Routine maintenance with no expected checkout impact."
)

target <- which(changes$change_id == "CHG-1842")
changes$component[target] <- "checkout-auth"
changes$note[target] <- paste(
  "Mobile Pro token refresh:",
  "retry budget changed from 3 to 0 and timeout from 8 s to 800 ms."
)
```

The question does not reveal the affected dimensions:

``` r

question <- paste(
  "Checkout conversion fell after release 2.4.0.",
  "Find the finest low-cardinality categorical cohort with the largest",
  "before/after drop; do not stop at a marginal roll-up that dilutes the",
  "change. Identify its dimensions and values, quantify the rates, and cite",
  "the change-log record that is the strongest candidate explanation."
)
```

## Define the investigation

The output signature makes the answer checkable. `SUBMIT()` must provide
every field with a compatible type.

``` r

library(dsprrr)

incident_signature <- signature(
  paste(
    "sessions, changes, question ->",
    "release: string, cohort: string,",
    "before_rate: number, after_rate: number,",
    "drop_pp: number, change_id: string, evidence: string"
  ),
  instructions = paste(
    "Inspect the schema and low-cardinality categorical fields; exclude the",
    "release, outcome, and row-index fields from candidate cohort dimensions.",
    "Find the finest low-cardinality cohort with the largest before/after",
    "drop; do not stop at a marginal roll-up that dilutes the change.",
    "Quantify it and cite the strongest matching change record.",
    "Format cohort in source-column order as '<dimension>=<value> / ...'.",
    "Copy the selected change note verbatim into evidence. Treat that record as",
    "evidence, not proof of causation."
  )
)

investigator <- rlm_module(
  incident_signature,
  interpreter_factory = function() {
    r_code_runner(timeout = 30, persistent = TRUE)
  },
  max_iterations = 8,
  max_llm_calls = 0L,
  max_output_chars = 10000
)
```

This example uses `r_code_runner(persistent = TRUE)` because the input
is a large R data frame and both the data and the generated code are
treated as trusted. The callr process is a separate R session, not a
security sandbox: it has your file, network and environment access. Do
not use this configuration for input you do not control. The factory
creates one runner per call, keeps its state between steps, and shuts it
down when the call ends.

`max_llm_calls = 0L` turns off recursive queries, because R aggregation
and one change record are enough here. When generated code must read
text that R cannot classify, such as vague change notes, `llm_query()`
asks a model about that slice; [How RLM
works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.html#recursive-queries)
explains how those calls run.

## Run the investigation

Request the structured result so the answer and its trajectory stay
together:

``` r

result <- run(
  investigator,
  sessions = sessions,
  changes = changes,
  question = question,
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .return_format = "structured"
)

result$output
```

This page does not show a recorded model run. The generated code and the
number of steps vary from run to run; the fixed answer at the top is
what a successful run must recover.

## What a successful trajectory looks like

The exact code will differ, but the useful work falls into four steps.

### 1. Look at the shape of the data

The first step inspects the schema instead of printing the rows:

``` r

list(
  session_dim = dim(.context$sessions),
  session_fields = names(.context$sessions),
  releases = table(.context$sessions$release),
  change_fields = names(.context$changes)
)
```

### 2. Calculate cohort-level changes

R does the aggregation exactly:

``` r

rates <- aggregate(
  converted ~ release + platform + plan,
  data = .context$sessions,
  FUN = mean
)

before <- subset(rates, release == "2.3.9")
after <- subset(rates, release == "2.4.0")
deltas <- merge(
  before,
  after,
  by = c("platform", "plan"),
  suffixes = c("_before", "_after")
)
deltas$drop_pp <-
  100 * (deltas$converted_before - deltas$converted_after)
winner <- deltas[which.max(deltas$drop_pp), ]
deltas[order(-deltas$drop_pp), ]
```

Only this four-row table, not the 40,000 sessions, goes into the next
prompt.

### 3. Find the matching change records

The model cannot search for words it has not seen yet, so it starts from
what step 2 found: 2.4.0 records that mention the affected platform and
plan.

``` r

mentions <- function(value) {
  grepl(paste0("\\b", value, "\\b"), .context$changes$note, ignore.case = TRUE)
}
candidate <- .context$changes[
  .context$changes$release == "2.4.0" &
    mentions(winner$platform) &
    mentions(winner$plan),
]
candidate
```

### 4. Submit typed evidence

The last step returns the calculated values and the matching record:

``` r

SUBMIT(
  release = after$release[[1L]],
  cohort = paste0(
    "platform=", winner$platform,
    " / plan=", winner$plan
  ),
  before_rate = winner$converted_before,
  after_rate = winner$converted_after,
  drop_pp = winner$drop_pp,
  change_id = candidate$change_id[[1L]],
  evidence = candidate$note[[1L]]
)
```

If a value is missing or has the wrong type, the RLM receives the
validation error and can correct the submission on a later step. Run in
order in a real runner, these four blocks produce exactly the answer at
the top of the page.

## Check the answer independently

The structured result carries the trajectory of this call:

``` r

trajectory <- result$metadata$repl_history

vapply(trajectory, function(step) step$code, character(1))
vapply(trajectory, function(step) step$success, logical(1))
```

Do not trust the model’s prose. Compute the expected answer from the
data without it:

``` r

rates <- aggregate(
  converted ~ release + platform + plan,
  data = sessions,
  FUN = mean
)
before <- subset(rates, release == "2.3.9")
after <- subset(rates, release == "2.4.0")
comparison <- merge(
  before,
  after,
  by = c("platform", "plan"),
  suffixes = c("_before", "_after")
)
comparison$drop_pp <-
  100 * (comparison$converted_before - comparison$converted_after)
oracle <- comparison[which.max(comparison$drop_pp), ]
oracle
#>   platform plan release_before converted_before release_after converted_after
#> 4   mobile  pro          2.3.9             0.92         2.4.0            0.61
#>   drop_pp
#> 4      31
```

The change lookup starts from the computed cohort, not from words in the
expected note:

``` r

mentions <- function(value) {
  grepl(paste0("\\b", value, "\\b"), changes$note, ignore.case = TRUE)
}
expected_change <- changes[
  changes$release == "2.4.0" &
    mentions(oracle$platform) &
    mentions(oracle$plan),
]
expected_change[, c("change_id", "component")]
#>     change_id     component
#> 142  CHG-1842 checkout-auth
```

This lookup relies on a property of the fixture: exactly one 2.4.0
record names the affected platform and plan. In a real change log,
linking a cohort to a change is a judgment call, which is why the
signature asks for the note as evidence rather than proof.

Then compare the model’s answer with both:

``` r

stopifnot(
  identical(result$output$release, "2.4.0"),
  identical(
    result$output$cohort,
    paste0("platform=", oracle$platform, " / plan=", oracle$plan)
  ),
  isTRUE(all.equal(result$output$before_rate, oracle$converted_before)),
  isTRUE(all.equal(result$output$after_rate, oracle$converted_after)),
  isTRUE(all.equal(result$output$drop_pp, oracle$drop_pp)),
  identical(result$output$change_id, expected_change$change_id),
  identical(result$output$evidence, expected_change$note)
)
```

## Choose the runner

The one-call
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md) helper
runs in a fresh
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
sandbox by default. That sandbox suits compact inputs you do not
control, but every input is sent with every step, so this 40,000-row
table is far too large for it. See [How RLM
works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.html#limits)
for the sandbox’s size limits and safety model. For a compact document,
the one-call form is enough:

``` r

answer <- rlm(
  "document, question -> answer",
  document = "Owner: team-a\nCommitment: publish the audit by Friday",
  question = "Which commitments have no owner?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .max_iterations = 4L,
  .max_llm_calls = 0L
)
```

## Turn discovery into a program

RLM is a poor default for a known report. If several investigations
aggregate the same columns and join the same records, write that path in
R or as a dsprrr pipeline. If the best implementation is itself the
thing to search for, evaluate candidates over labeled cases with
[Flex](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md).
Use RLM to discover the path, then replace it with code once you know
it.
