# Agentic optimization harnesses

[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
and
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)
let an agent propose changes to a program while dsprrr owns validation,
evaluation, budgets, and candidate selection. This article executes both
journeys through the real
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
pipeline with deterministic, local Chat fixtures. The scores prove that
the workflow and result contract run; they are not evidence that a real
model improves a task.

The task is a two-step question-answering pipeline. The first step
extracts evidence from a context; the second turns that evidence into an
answer. The candidate that changes only the evidence step improves the
validation score, but leaves a multiword answer truncated. A coordinated
edit to both steps completes the answer contract.

## Define the program and independent data splits

The search sees training rows as proposal context and validation rows
for candidate selection. The separate holdout rows stay outside
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
and are evaluated only after each search has returned.

``` r

library(dsprrr)

trainset <- data.frame(
  context = c(
    "The North Hall opened in 2018.",
    "Refunds are processed in 5 business days.",
    "The Kyoto office employs 12 analysts.",
    "Support is available every Tuesday."
  ),
  question = c(
    "Which year did North Hall open?",
    "How long do refunds take?",
    "How many analysts work in Kyoto?",
    "Which weekday is support available?"
  ),
  answer = c("2018", "5 business days", "12", "Tuesday")
)

valset <- data.frame(
  context = c(
    "The Cedar lab opened in 2021.",
    "Standard returns are completed in 7 business days.",
    "The Porto office employs 18 staff.",
    "Releases ship every Thursday."
  ),
  question = c(
    "Which year did Cedar lab open?",
    "How long do standard returns take?",
    "How many staff work in Porto?",
    "Which weekday do releases ship?"
  ),
  answer = c("2021", "7 business days", "18", "Thursday")
)

holdout <- data.frame(
  context = c(
    "The Field team launched in 2016.",
    "Replacement parts arrive in 3 business days.",
    "The Tallinn group has 9 engineers.",
    "Backups are checked every Friday."
  ),
  question = c(
    "Which year did the Field team start?",
    "How long do replacement parts take?",
    "How many engineers are in Tallinn?",
    "Which weekday are backups checked?"
  ),
  answer = c("2016", "3 business days", "9", "Friday")
)

find_evidence <- module(
  signature(
    "context, question -> evidence: string",
    instructions = "Quote the supplied context as evidence."
  ),
  template = "Question: {question}\nContext: {context}\nEvidence:"
)
answer_from_evidence <- module(
  signature(
    "evidence -> answer: string",
    instructions = "Return the first word of the evidence as the answer."
  ),
  template = "Evidence: {evidence}\nAnswer:"
)
qa_program <- pipeline(find_evidence, answer_from_evidence)

metric <- metric_with_feedback(
  function(prediction, expected) {
    correct <- identical(prediction$answer, expected$answer)
    list(
      score = as.numeric(correct),
      feedback = if (correct) {
        "Correct."
      } else {
        paste0(
          "Expected '",
          expected$answer,
          "' but got '",
          prediction$answer,
          "'. Preserve the full value."
        )
      }
    )
  },
  field = "answer"
)
```

Before either search starts, inspect the same paths and fields the
harness allows.
[`named_parameters()`](https://jameshwade.github.io/dsprrr/reference/module-graph.md)
reports the stable module paths; instructions and prompt templates are
the editable values on each leaf module.

``` r

editable_manifest <- function(program) {
  modules <- named_parameters(
    program,
    include_root = TRUE,
    boundaries = "respect"
  )
  data.frame(
    path = names(modules),
    editable = vapply(
      modules,
      function(component) {
        fields <- character()
        instructions <- tryCatch(
          component$signature@instructions,
          error = function(e) NULL
        )
        template <- tryCatch(component$template, error = function(e) NULL)
        if (is.character(instructions) && length(instructions) == 1L) {
          fields <- c(fields, "instructions")
        }
        if (is.character(template) && length(template) == 1L) {
          fields <- c(fields, "template")
        }
        paste(fields, collapse = ", ")
      },
      character(1)
    ),
    row.names = NULL
  )
}

editable_manifest(qa_program)
#>        path               editable
#> 1 $/steps/1 instructions, template
#> 2 $/steps/2 instructions, template
```

For this pipeline the manifest contains `$/steps/1` and `$/steps/2`,
each with `instructions` and `template`. A proposal that names another
path or field is rejected before evaluation. An edit replaces a field’s
complete value; it does not run module source code.

## Create deterministic offline Chat fixtures

These small fixtures implement the ellmer Chat methods used by
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).
The task fixture reads the rendered prompts and returns scripted
answers. The proposer fixture returns prewritten actions and records
each session id and prompt. The support file ships with the package and
its implementation is available in the collapsed appendix. No
credentials, provider requests, network access, or R interpreter service
are used by either executed search.

Run this setup once before either journey to load the supplied fixtures
into your R session:

``` r

fixture_file <- system.file(
  "examples",
  "agentic-optimization-harnesses.R",
  package = "dsprrr",
  mustWork = TRUE
)
source(fixture_file, local = TRUE)
```

The fixture’s rules make the transition explicit: the baseline quotes
the context, the first candidate extracts the requested value, and only
the coordinated candidate preserves the full multiword value through
both stages. That deterministic behavior is a teaching device, not a
model of general language understanding.

## AutoResearch: one persistent research session

AutoResearch evaluates the baseline, then lets one proposer submit one
candidate at a time. The first edit changes only the evidence step. It
raises the score, but the answer step still truncates `7 business days`.
The second proposal edits both steps and reaches the target score.

``` r

auto_agent_audit <- new_offline_audit()
auto_script <- new.env(parent = emptyenv())
auto_script$next_action <- 0L
auto_actions <- list(
  action_propose(
    "Extract evidence first; this tests whether the answer step can use it.",
    list(candidate(
      "evidence-only",
      "The downstream step may still truncate a multiword value.",
      list(edit_evidence_only)
    ))
  ),
  action_propose(
    "The answer step must preserve the complete extracted value.",
    list(candidate(
      "coordinated-pipeline",
      "Update the evidence and answer contracts together.",
      list(edit_evidence_only, edit_answer)
    ))
  )
)
auto_agent_responder <- function(prompt, type, ...) {
  auto_script$next_action <- auto_script$next_action + 1L
  auto_actions[[auto_script$next_action]]
}
auto_agent <- new_offline_chat(
  "offline-autoresearch-proposer",
  auto_agent_responder,
  auto_agent_audit
)

auto_call_start <- length(task_audit$calls)
auto_compiled <- compile(
  qa_program,
  AutoResearch(
    metric = metric,
    max_iterations = 2L,
    patience = 2L,
    target_score = 1,
    seed = 42L,
    sandbox = FALSE,
    verbose = FALSE
  ),
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = auto_agent,
  control = optimizer_control(
    max_trials = 4L,
    max_metric_calls = 20L,
    max_provider_calls = 40L,
    progress = FALSE
  ),
  .cache = FALSE,
  objective = "Answer from the context and preserve the complete value."
)
auto_task_calls <- length(task_audit$calls) - auto_call_start
auto_result <- optimization_result(auto_compiled)
result_summary(auto_result)
#>      optimizer    status baseline_score best_score  stop_reason
#> 1 AutoResearch completed              0          1 target_score
```

The result reports a 0.00 baseline and a 1.00 best validation score.
Inspect the candidate ledger to see that the first, single-stage edit
scored 0.75, then the coordinated edit won. `parent_id` records which
candidate the next experiment branched from.

``` r

auto_trials <- auto_result$trials
auto_trials[, c(
  "id",
  "parent_id",
  "name",
  "iteration",
  "score",
  "selected",
  "status"
)]
#> # A tibble: 3 × 7
#>   id                       parent_id       name  iteration score selected status
#>   <chr>                    <chr>           <chr>     <int> <dbl> <lgl>    <chr> 
#> 1 baseline-594b4430aa07    NA              base…         0  0    FALSE    evalu…
#> 2 candidate-1-3d25bbaa446c baseline-594b4… evid…         1  0.75 FALSE    evalu…
#> 3 candidate-2-7bae3460a9c0 candidate-1-3d… coor…         2  1    TRUE     evalu…
auto_result$lineage
#> $best_candidate_id
#> [1] "candidate-2-7bae3460a9c0"
auto_result$stop_reason
#> [1] "target_score"
budget_summary(auto_result)
#>   trials metric_calls provider_calls errors elapsed_seconds stopped max_trials
#> 1      3           12             26      0           1.329   FALSE          4
#>   max_metric_calls max_provider_calls
#> 1               20                 40
auto_agent_sessions <- vapply(
  auto_agent_audit$calls,
  `[[`,
  character(1),
  "session_id"
)
data.frame(
  proposer_call = seq_along(auto_agent_sessions),
  session_id = auto_agent_sessions,
  row.names = NULL
)
#>   proposer_call                    session_id
#> 1             1 offline-autoresearch-proposer
#> 2             2 offline-autoresearch-proposer

stopifnot(
  auto_result$baseline_score == 0,
  auto_trials$score[auto_trials$name == "evidence-only"] == 0.75,
  auto_result$best_score == 1,
  auto_trials$selected[auto_trials$name == "coordinated-pipeline"],
  length(unique(auto_agent_sessions)) == 1L,
  identical(auto_result$stop_reason, "target_score")
)
```

The displayed provider-call count is derived from in-process fixture
calls; there are no billable requests, token totals, or cost totals in
these runs.

The compile result’s scores come from `valset`. Run
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
on the original and returned programs to inspect their actual
predictions against the labels; these check calls are outside the
optimizer’s recorded budget.

``` r

auto_baseline_validation <- evaluate(
  qa_program,
  valset,
  metric = metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)
auto_best_validation <- evaluate(
  auto_compiled,
  valset,
  metric = metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)

auto_validation_rows <- data.frame(
  question = valset$question,
  expected = valset$answer,
  baseline = vapply(
    auto_baseline_validation$predictions,
    `[[`,
    character(1),
    "answer"
  ),
  returned_program = vapply(
    auto_best_validation$predictions,
    `[[`,
    character(1),
    "answer"
  ),
  score = auto_best_validation$scores
)
auto_validation_rows
#>                             question        expected baseline returned_program
#> 1     Which year did Cedar lab open?            2021      The             2021
#> 2 How long do standard returns take? 7 business days Standard  7 business days
#> 3      How many staff work in Porto?              18      The               18
#> 4    Which weekday do releases ship?        Thursday Releases         Thursday
#>   score
#> 1     1
#> 2     1
#> 3     1
#> 4     1

auto_baseline_holdout <- evaluate(
  qa_program,
  holdout,
  metric = metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)
auto_best_holdout <- evaluate(
  auto_compiled,
  holdout,
  metric = metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)
auto_holdout_rows <- data.frame(
  question = holdout$question,
  expected = holdout$answer,
  baseline = vapply(
    auto_baseline_holdout$predictions,
    `[[`,
    character(1),
    "answer"
  ),
  returned_program = vapply(
    auto_best_holdout$predictions,
    `[[`,
    character(1),
    "answer"
  ),
  score = auto_best_holdout$scores
)
auto_holdout_rows
#>                               question        expected    baseline
#> 1 Which year did the Field team start?            2016         The
#> 2  How long do replacement parts take? 3 business days Replacement
#> 3   How many engineers are in Tallinn?               9         The
#> 4   Which weekday are backups checked?          Friday     Backups
#>   returned_program score
#> 1             2016     1
#> 2  3 business days     1
#> 3                9     1
#> 4           Friday     1

auto_scores <- data.frame(
  set = c(
    "Validation baseline",
    "Validation returned program",
    "Holdout baseline",
    "Holdout returned program"
  ),
  rows = c(nrow(valset), nrow(valset), nrow(holdout), nrow(holdout)),
  score = c(
    auto_result$baseline_score,
    auto_best_validation$mean_score,
    auto_baseline_holdout$mean_score,
    auto_best_holdout$mean_score
  )
)
auto_scores
#>                           set rows score
#> 1         Validation baseline    4     0
#> 2 Validation returned program    4     1
#> 3            Holdout baseline    4     0
#> 4    Holdout returned program    4     1
stopifnot(
  auto_best_validation$mean_score == auto_result$best_score,
  auto_baseline_holdout$mean_score == 0,
  auto_best_holdout$mean_score == 1
)
```

The four holdout questions and contexts do not appear in the proposer
prompt; the code passes `trainset` and `valset` to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
and uses `holdout` only in the later
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
calls. The audit also derives the number of task and proposer requests
from executed fixture calls:

``` r

data.frame(
  executed_calls = c(auto_task_calls, length(auto_agent_audit$calls)),
  fixture = c("Task evaluator", "AutoResearch proposer"),
  row.names = NULL
)
#>   executed_calls               fixture
#> 1             24        Task evaluator
#> 2              2 AutoResearch proposer
auto_prompt_text <- paste(
  vapply(auto_agent_audit$calls, `[[`, character(1), "prompt"),
  collapse = "\n"
)
auto_holdout_seen <- vapply(
  holdout$context,
  function(context) grepl(context, auto_prompt_text, fixed = TRUE),
  logical(1)
)
data.frame(
  question = holdout$question,
  appeared_in_proposer_prompt = auto_holdout_seen
)
#>                                                                          question
#> The Field team launched in 2016.             Which year did the Field team start?
#> Replacement parts arrive in 3 business days.  How long do replacement parts take?
#> The Tallinn group has 9 engineers.             How many engineers are in Tallinn?
#> Backups are checked every Friday.              Which weekday are backups checked?
#>                                              appeared_in_proposer_prompt
#> The Field team launched in 2016.                                   FALSE
#> Replacement parts arrive in 3 business days.                       FALSE
#> The Tallinn group has 9 engineers.                                 FALSE
#> Backups are checked every Friday.                                  FALSE
stopifnot(!any(auto_holdout_seen))
```

No checkpoint is configured for these short examples. For a resumable
run, the same split rule keeps holdout out of
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
and its checkpoint state; evaluate holdout only after the final resumed
candidate has been selected.

## MetaHarness: fresh proposers and candidate batches

MetaHarness creates a fresh proposer clone for each outer iteration. The
first proposer submits a two-candidate batch: the evidence-only edit and
the coordinated edit. A second proposer sees the saved frontier and
tries a formatting variant of the winner. The bounded run evaluates four
trials in total: the baseline and three candidates.

``` r

meta_agent_audit <- new_offline_audit()
meta_script <- new.env(parent = emptyenv())
meta_script$next_action <- 0L
meta_actions <- list(
  action_propose(
    "Compare a local evidence edit with a coordinated answer contract.",
    list(
      candidate(
        "evidence-only",
        "This candidate leaves the answer step's first-word rule in place.",
        list(edit_evidence_only)
      ),
      candidate(
        "coordinated-pipeline",
        "Keep the complete multiword evidence through the second step.",
        list(edit_evidence_only, edit_answer)
      )
    )
  ),
  action_propose(
    "The winning contract works; test a harmless output label change.",
    list(candidate(
      "coordinated-format-variant",
      "Retain the full evidence value with a different final label.",
      list(list(
        path = "$/steps/2",
        template = paste(
          "Evidence block:",
          "{evidence}",
          "Return the full value:",
          sep = "\n"
        )
      ))
    ))
  )
)
meta_agent_responder <- function(prompt, type, session_id, prior_turns, ...) {
  meta_script$next_action <- meta_script$next_action + 1L
  meta_agent_audit$proposer_prompts <- append(
    if (is.null(meta_agent_audit$proposer_prompts)) {
      list()
    } else {
      meta_agent_audit$proposer_prompts
    },
    list(list(
      session_id = session_id,
      prior_turns = prior_turns,
      prompt = prompt
    ))
  )
  meta_actions[[meta_script$next_action]]
}
meta_agent <- new_offline_chat(
  "offline-meta-harness-proposer",
  meta_agent_responder,
  meta_agent_audit
)
meta_agent$set_turns(list(list(role = "user", content = "stale source turn")))

meta_call_start <- length(task_audit$calls)
meta_compiled <- compile(
  qa_program,
  MetaHarness(
    metric = metric,
    max_iterations = 2L,
    max_candidates_per_iteration = 2L,
    frontier_size = 2L,
    patience = 3L,
    seed = 42L,
    sandbox = FALSE,
    verbose = FALSE
  ),
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = meta_agent,
  control = optimizer_control(
    max_trials = 5L,
    max_metric_calls = 24L,
    max_provider_calls = 50L,
    progress = FALSE
  ),
  .cache = FALSE,
  objective = "Answer from the context and preserve the complete value."
)
meta_task_calls <- length(task_audit$calls) - meta_call_start
meta_result <- optimization_result(meta_compiled)
result_summary(meta_result)
#>     optimizer    status baseline_score best_score    stop_reason
#> 1 MetaHarness completed              0          1 max_iterations
```

Inspect the batch scores, candidate parents, selected result, final
frontier, stop reason, and budget. The second candidate in the first
batch branches from the improved evidence-only candidate; the second
iteration’s formatting variant branches from the coordinated winner.

``` r

meta_trials <- meta_result$trials
meta_trials[, c(
  "id",
  "parent_id",
  "name",
  "iteration",
  "score",
  "selected",
  "status"
)]
#> # A tibble: 4 × 7
#>   id                       parent_id       name  iteration score selected status
#>   <chr>                    <chr>           <chr>     <int> <dbl> <lgl>    <chr> 
#> 1 baseline-594b4430aa07    NA              base…         0  0    FALSE    evalu…
#> 2 candidate-1-3d25bbaa446c baseline-594b4… evid…         1  0.75 FALSE    evalu…
#> 3 candidate-1-7bae3460a9c0 candidate-1-3d… coor…         1  1    TRUE     evalu…
#> 4 candidate-2-ae5902503f42 candidate-1-7b… coor…         2  1    FALSE    evalu…
meta_result$lineage
#> $best_candidate_id
#> [1] "candidate-1-7bae3460a9c0"
meta_frontier <- meta_result$extensions$meta_harness$frontier_ids
meta_frontier
#> [1] "candidate-1-7bae3460a9c0" "candidate-2-ae5902503f42"
meta_events <- meta_result$extensions$meta_harness$events
data.frame(
  type = vapply(meta_events, `[[`, character(1), "type"),
  iteration = vapply(
    meta_events,
    function(event) {
      if (is.null(event$iteration)) NA_integer_ else as.integer(event$iteration)
    },
    integer(1)
  ),
  candidate_id = vapply(
    meta_events,
    function(event) {
      if (is.null(event$candidate_id)) NA_character_ else event$candidate_id
    },
    character(1)
  ),
  score = vapply(
    meta_events,
    function(event) {
      if (is.null(event$score)) NA_real_ else as.numeric(event$score)
    },
    numeric(1)
  ),
  row.names = NULL
)
#>                  type iteration             candidate_id score
#> 1 candidate_evaluated         0    baseline-594b4430aa07  0.00
#> 2 candidate_evaluated         1 candidate-1-3d25bbaa446c  0.75
#> 3 candidate_evaluated         1 candidate-1-7bae3460a9c0  1.00
#> 4 candidate_evaluated         2 candidate-2-ae5902503f42  1.00
meta_result$stop_reason
#> [1] "max_iterations"
budget_summary(meta_result)
#>   trials metric_calls provider_calls errors elapsed_seconds stopped max_trials
#> 1      4           16             34      0            1.67   FALSE          5
#>   max_metric_calls max_provider_calls
#> 1               24                 50

proposer_sessions <- data.frame(
  session_id = vapply(
    meta_agent_audit$proposer_prompts,
    `[[`,
    character(1),
    "session_id"
  ),
  prior_turns = vapply(
    meta_agent_audit$proposer_prompts,
    `[[`,
    integer(1),
    "prior_turns"
  ),
  row.names = NULL
)
proposer_sessions
#>                              session_id prior_turns
#> 1 offline-meta-harness-proposer-fresh-1           0
#> 2 offline-meta-harness-proposer-fresh-2           0

stopifnot(
  meta_result$baseline_score == 0,
  meta_trials$score[meta_trials$name == "evidence-only"] == 0.75,
  meta_trials$score[meta_trials$name == "coordinated-pipeline"] == 1,
  meta_trials$selected[meta_trials$name == "coordinated-pipeline"],
  meta_trials$parent_id[meta_trials$name == "coordinated-pipeline"] ==
    meta_trials$id[meta_trials$name == "evidence-only"],
  meta_trials$parent_id[meta_trials$name == "coordinated-format-variant"] ==
    meta_trials$id[meta_trials$name == "coordinated-pipeline"],
  length(meta_frontier) == 2L,
  identical(meta_result$stop_reason, "max_iterations"),
  length(unique(proposer_sessions$session_id)) == 2L,
  all(proposer_sessions$prior_turns == 0L),
  length(meta_agent$turns) == 1L
)
```

Each MetaHarness iteration received a different Chat clone with an empty
turn history, even though the source Chat held a stale turn. The fixture
records the actual clone ids and empty histories; it does not infer
freshness from the iteration number. Candidate scores and the frontier
come from the executed evaluator and host-owned ledger.

The returned program’s validation and independent holdout predictions
are computed from the actual compiled module:

``` r

meta_baseline_validation <- evaluate(
  qa_program,
  valset,
  metric = metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)
meta_best_validation <- evaluate(
  meta_compiled,
  valset,
  metric = metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)
meta_baseline_holdout <- evaluate(
  qa_program,
  holdout,
  metric = metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)
meta_best_holdout <- evaluate(
  meta_compiled,
  holdout,
  metric = metric,
  .llm = task_chat,
  .cache = FALSE,
  .progress = FALSE
)

meta_validation_rows <- data.frame(
  question = valset$question,
  expected = valset$answer,
  baseline = vapply(
    meta_baseline_validation$predictions,
    `[[`,
    character(1),
    "answer"
  ),
  returned_program = vapply(
    meta_best_validation$predictions,
    `[[`,
    character(1),
    "answer"
  ),
  score = meta_best_validation$scores
)
meta_validation_rows
#>                             question        expected baseline returned_program
#> 1     Which year did Cedar lab open?            2021      The             2021
#> 2 How long do standard returns take? 7 business days Standard  7 business days
#> 3      How many staff work in Porto?              18      The               18
#> 4    Which weekday do releases ship?        Thursday Releases         Thursday
#>   score
#> 1     1
#> 2     1
#> 3     1
#> 4     1

meta_holdout_rows <- data.frame(
  question = holdout$question,
  expected = holdout$answer,
  baseline = vapply(
    meta_baseline_holdout$predictions,
    `[[`,
    character(1),
    "answer"
  ),
  returned_program = vapply(
    meta_best_holdout$predictions,
    `[[`,
    character(1),
    "answer"
  ),
  score = meta_best_holdout$scores
)
meta_holdout_rows
#>                               question        expected    baseline
#> 1 Which year did the Field team start?            2016         The
#> 2  How long do replacement parts take? 3 business days Replacement
#> 3   How many engineers are in Tallinn?               9         The
#> 4   Which weekday are backups checked?          Friday     Backups
#>   returned_program score
#> 1             2016     1
#> 2  3 business days     1
#> 3                9     1
#> 4           Friday     1

meta_scores <- data.frame(
  set = c(
    "Validation baseline",
    "Validation returned program",
    "Holdout baseline",
    "Holdout returned program"
  ),
  rows = c(nrow(valset), nrow(valset), nrow(holdout), nrow(holdout)),
  score = c(
    meta_result$baseline_score,
    meta_best_validation$mean_score,
    meta_baseline_holdout$mean_score,
    meta_best_holdout$mean_score
  )
)
meta_scores
#>                           set rows score
#> 1         Validation baseline    4     0
#> 2 Validation returned program    4     1
#> 3            Holdout baseline    4     0
#> 4    Holdout returned program    4     1
stopifnot(
  meta_best_validation$mean_score == meta_result$best_score,
  meta_baseline_holdout$mean_score == 0,
  meta_best_holdout$mean_score == 1
)
```

The holdout remains outside the proposer context and the compile-time
selection data. The following audit checks every holdout context against
the actual proposer prompts:

``` r

meta_prompt_text <- paste(
  vapply(meta_agent_audit$proposer_prompts, `[[`, character(1), "prompt"),
  collapse = "\n"
)
meta_holdout_seen <- vapply(
  holdout$context,
  function(context) grepl(context, meta_prompt_text, fixed = TRUE),
  logical(1)
)
data.frame(
  question = holdout$question,
  appeared_in_proposer_prompt = meta_holdout_seen
)
#>                                                                          question
#> The Field team launched in 2016.             Which year did the Field team start?
#> Replacement parts arrive in 3 business days.  How long do replacement parts take?
#> The Tallinn group has 9 engineers.             How many engineers are in Tallinn?
#> Backups are checked every Friday.              Which weekday are backups checked?
#>                                              appeared_in_proposer_prompt
#> The Field team launched in 2016.                                   FALSE
#> Replacement parts arrive in 3 business days.                       FALSE
#> The Tallinn group has 9 engineers.                                 FALSE
#> Backups are checked every Friday.                                  FALSE
data.frame(
  executed_calls = c(meta_task_calls, length(meta_agent_audit$calls)),
  fixture = c("Task evaluator", "MetaHarness proposers"),
  row.names = NULL
)
#>   executed_calls               fixture
#> 1             32        Task evaluator
#> 2              2 MetaHarness proposers
stopifnot(!any(meta_holdout_seen))
```

## Sandbox and trust boundaries

The executed fixture journeys set `sandbox = FALSE` so they need no
external interpreter. That setting disables sandbox actions: the
proposer can submit validated instruction and template edits but cannot
run exploratory R code.

With the default `sandbox = TRUE`, each harness requires a runner that
advertises a verified operating-system sandbox.
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
starts Posit’s [mcp-console](https://github.com/posit-dev/mcp-console),
formerly named `mcp-repl`, with the `workspace-write` policy by default:
network access is disabled and writes are limited to the workspace and
temporary paths. It also needs the `mcptools` R package and `mcp-repl`
executable. A user-managed connection passed through `repl =` is
unverified because dsprrr did not start it, so the harness rejects it
when sandboxing is required.
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
uses the current process permissions and is not a security boundary.

The runner handles only agent-authored analysis code. Candidate
evaluation and selection stay in the host R process under the package
evaluator. The agent may replace complete `instructions` and `template`
fields at listed module paths; it cannot edit R source or the evaluator.
Limits on action code, output, iterations, trials, metric calls, and
provider calls are separate from the operating-system sandbox. Treat
values sent to a live model provider according to that provider’s data
policy.

To run analysis in a sandbox, install the two requirements and use a
bounded runner:

``` r

runner <- mcp_repl_runner(
  sandbox = "workspace-write",
  timeout = 30
)
runner$policy()
```

## Optional live-model run

The earlier journeys are the complete offline path. For a live run,
create ellmer Chats and supply a sandbox runner. This example stays
unevaluated so a documentation build never makes provider calls or
incurs charges. Keep holdout out of both
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
and the prompts.

``` r

task_chat <- ellmer::chat_openai(model = "gpt-6-luna")
proposer_chat <- ellmer::chat_anthropic(model = "claude-sonnet-4-5")
runner <- mcp_repl_runner(sandbox = "workspace-write", timeout = 30)

live_result <- compile(
  qa_program,
  AutoResearch(metric = metric, max_iterations = 4L, target_score = 1),
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = proposer_chat,
  runner = runner,
  control = optimizer_control(
    max_trials = 5L,
    max_metric_calls = 20L,
    max_provider_calls = 50L,
    max_cost = 5,
    checkpoint_path = file.path(tempdir(), "autoresearch-checkpoint.rds")
  ),
  objective = "Answer from supplied evidence and preserve complete values."
)

optimization_result(live_result)
```

Use
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md)
to compare baseline and best scores, inspect the trial ledger and
lineage, and see the budget and `stop_reason`. A checkpoint restores the
optimizer ledger and random-number state; it does not restore the model
conversation or runner session. For both deterministic fixtures and live
models, treat any holdout score as a final report after candidate
selection, not as input to the search.

View the deterministic offline fixture implementation

``` r
 offline_chat_class <- R6::R6Class(
  "Chat",
  public = list(
    turns = NULL,
    session_id = NULL,
    initialize = function(model, responder, audit, session_id) {
      self$turns <- list()
      self$session_id <- session_id
      private$model <- ellmer::Model(name = model, extra_args = list())
      private$provider <- ellmer::Provider(
        name = "offline-fixture",
        base_url = ""
      )
      private$responder <- responder
      private$audit <- audit
    },
    chat = function(prompt, ...) {
      private$call(prompt, type = NULL, ...)
    },
    chat_structured = function(prompt, type, ...) {
      private$call(prompt, type = type, ...)
    },
    stream = function(...) character(),
    get_turns = function() self$turns,
    set_turns = function(turns) {
      self$turns <- turns
      invisible(self)
    },
    last_turn = function(role = NULL) {
      if (length(self$turns) == 0L) NULL else self$turns[[length(self$turns)]]
    },
    get_model = function() private$model@name,
    get_provider = function() private$provider,
    get_model_object = function() private$model
  ),
  private = list(
    model = NULL,
    provider = NULL,
    responder = NULL,
    audit = NULL,
    call = function(prompt, type, ...) {
      prompt <- paste(as.character(prompt), collapse = "\n")
      prior_turns <- length(self$turns)
      private$audit$calls <- append(
        private$audit$calls,
        list(list(
          session_id = self$session_id,
          prior_turns = prior_turns,
          prompt = prompt
        ))
      )
      response <- private$responder(
        prompt,
        type,
        session_id = self$session_id,
        prior_turns = prior_turns,
        ...
      )
      response
    }
  ),
  lock_objects = FALSE,
  parent_env = globalenv()
)

new_offline_audit <- function() {
  audit <- new.env(parent = emptyenv())
  audit$calls <- list()
  audit$clone_count <- 0L
  audit$clone_sessions <- character()
  audit
}

new_offline_chat <- function(model, responder, audit, session_id = model) {
  chat <- offline_chat_class$new(model, responder, audit, session_id)
  clone_chat <- function(deep = TRUE) {
    audit$clone_count <- audit$clone_count + 1L
    new_session <- paste0(model, "-fresh-", audit$clone_count)
    audit$clone_sessions <- c(audit$clone_sessions, new_session)
    cloned <- new_offline_chat(model, responder, audit, new_session)
    cloned$set_turns(chat$get_turns())
    cloned
  }
  unlockBinding("clone", chat)
  assign("clone", clone_chat, envir = chat)
  lockBinding("clone", chat)
  chat
}

read_prompt_line <- function(prompt, prefix) {
  lines <- strsplit(prompt, "\n", fixed = TRUE)[[1L]]
  matches <- grep(paste0("^", prefix), lines, value = TRUE)
  if (length(matches) != 1L) {
    stop("Offline task fixture expected one prompt line beginning: ", prefix)
  }
  sub(paste0("^", prefix), "", matches[[1L]])
}

extract_requested_value <- function(context, question) {
  if (grepl("year", question, ignore.case = TRUE)) {
    value <- regmatches(context, regexpr("\\b20[0-9]{2}\\b", context))
    return(if (length(value) == 0L) "unknown" else value)
  }
  if (grepl("business days", context, ignore.case = TRUE)) {
    value <- regmatches(
      context,
      regexpr("\\b[0-9]+ business days\\b", context, ignore.case = TRUE)
    )
    return(if (length(value) == 0L) "unknown" else value)
  }
  if (grepl("weekday", question, ignore.case = TRUE)) {
    weekdays <- c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )
    matches <- weekdays[vapply(
      weekdays,
      grepl,
      logical(1),
      x = context,
      ignore.case = TRUE
    )]
    return(if (length(matches) == 0L) "unknown" else matches[[1L]])
  }
  if (grepl("how many", question, ignore.case = TRUE)) {
    value <- regmatches(context, regexpr("\\b[0-9]+\\b", context))
    return(if (length(value) == 0L) "unknown" else value)
  }
  "unknown"
}

task_audit <- new_offline_audit()
task_responder <- function(prompt, type, ...) {
  lines <- strsplit(prompt, "\n", fixed = TRUE)[[1L]]
  evidence_prompt <- any(grepl("^Question: ", lines)) &&
    any(grepl("^Context: ", lines))
  if (evidence_prompt) {
    context <- read_prompt_line(prompt, "Context: ")
    question <- read_prompt_line(prompt, "Question: ")
    if (
      grepl("write only that value", prompt, fixed = TRUE) &&
        any(grepl("^Extracted value:$", lines))
    ) {
      extract_requested_value(context, question)
    } else {
      context
    }
  } else {
    evidence <- if (any(grepl("^Evidence block:$", lines))) {
      index <- match("Evidence block:", lines) + 1L
      lines[[index]]
    } else {
      read_prompt_line(prompt, "Evidence: ")
    }
    if (
      grepl("complete evidence value unchanged", prompt, fixed = TRUE) &&
        any(grepl("^Evidence block:$", lines))
    ) {
      evidence
    } else {
      strsplit(evidence, " ", fixed = TRUE)[[1L]][[1L]]
    }
  }
}

task_chat <- new_offline_chat(
  "offline-task",
  task_responder,
  task_audit
)

evidence_instructions <- paste(
  "Extract the exact fact requested by the question from the context;",
  "write only that value."
)
evidence_template <- paste(
  "Question: {question}",
  "Context: {context}",
  "Extracted value:",
  sep = "\n"
)
answer_instructions <- paste(
  "Return the complete evidence value unchanged, preserving multiple words."
)
answer_template <- paste(
  "Evidence block:",
  "{evidence}",
  "Copy the complete value:",
  sep = "\n"
)

edit_evidence_only <- list(
  path = "$/steps/1",
  instructions = evidence_instructions,
  template = evidence_template
)
edit_answer <- list(
  path = "$/steps/2",
  instructions = answer_instructions,
  template = answer_template
)

candidate <- function(name, rationale, edits, parent_id = NULL) {
  proposal <- list(name = name, rationale = rationale, edits = edits)
  if (!is.null(parent_id)) {
    proposal$parent_id <- parent_id
  }
  proposal
}

action_propose <- function(rationale, candidates) {
  list(action = "propose", rationale = rationale, candidates = candidates)
}

result_summary <- function(result) {
  data.frame(
    optimizer = result$optimizer,
    status = result$status,
    baseline_score = result$baseline_score,
    best_score = result$best_score,
    stop_reason = result$stop_reason,
    row.names = NULL
  )
}

budget_summary <- function(result) {
  budget <- result$budget
  limits <- budget$limits
  data.frame(
    trials = budget$trials,
    metric_calls = budget$metric_calls,
    provider_calls = budget$provider_calls,
    errors = budget$total_errors,
    elapsed_seconds = budget$elapsed_seconds,
    stopped = budget$stopped,
    max_trials = limits$max_trials,
    max_metric_calls = limits$max_metric_calls,
    max_provider_calls = limits$max_provider_calls,
    row.names = NULL
  )
} 
```
