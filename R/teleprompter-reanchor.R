#' ReAnchor: calibrate decision outputs against a metric
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ReAnchor` fits the numeric settings of a module's decision outputs (Boolean
#' `threshold`s, Score `cuts`, and Choice `weights`; see [decision_types])
#' against a metric. It mirrors the experimental `ReAnchor` optimizer in DSPy
#' 3.4. It never changes instructions, demonstrations, or the questions sent to
#' the model.
#'
#' @details
#' Compilation runs in four steps.
#'
#' 1. Baseline: the module runs on `trainset` as given, and the metric scores
#'    each row.
#' 2. Evidence: every compatible output is switched to evidence decoding.
#'    That includes fields already configured with [with_decisions()], plus
#'    described `type_boolean()` and `type_enum()` outputs, which become
#'    [decision_bool()] and [decision_choice()] decisions. The module runs
#'    once more and records the probabilities behind each decision.
#' 3. Fitting: every setting is searched locally against that recorded
#'    evidence, so the search makes no further provider calls. A setting
#'    anywhere between two neighboring observed values makes the same
#'    decisions, so the candidates are the midpoints of those gaps: between
#'    P(TRUE) values for a threshold, between mean level indexes for a cut, and
#'    between the points where an option's pick flips for a weight (on a log
#'    scale). At most `max_candidates` gaps are tried per step, thinned to
#'    evenly spaced quantiles. Among equal scores, the candidate in the widest
#'    gap wins.
#' 4. Acceptance: a new setting replaces the current one only if it scores
#'    strictly better and passes a fold check. The check splits `trainset`
#'    into up to `folds` parts. For each part, it picks a setting on the other
#'    parts and scores that pick on the held-out part. The combined held-out
#'    score must beat the current setting's. The fully fitted module must pass
#'    the same check against the step 1 baseline. Otherwise the original
#'    decision configuration is restored unchanged.
#'
#' `valset`, when given, is scored before and after calibration for the report
#' and is never used for fitting.
#'
#' The fit is exact for a single Predict module, because the decoded outputs
#' depend only on the recorded evidence. Pipelines and other composite
#' programs are rejected, because an upstream decision can change the requests
#' made downstream.
#'
#' The report is stored in the compiled module's optimization result:
#' `optimization_result(compiled)$extensions$re_anchor`. It contains the train
#' (and validation) scores before and after calibration, whether the fitted
#' settings were accepted, and one entry per field with the fitted value, the
#' number of candidates tried, and the fold-check outcomes. Use
#' [decision_settings()] to see the resulting settings.
#'
#' @param metric A metric function `function(prediction, expected_row)`, such
#'   as `metric_exact_match(field = "match")`. Required.
#' @param fields Optional character vector naming the output fields to
#'   calibrate. `NULL` (the default) calibrates every compatible field.
#' @param folds Integer maximum number of folds for the held-out acceptance
#'   check (default `5L`, at least `2L`).
#' @param max_candidates Integer maximum number of candidate settings tried
#'   per search step (default `40L`, at least `3L`).
#' @param metric_threshold,max_errors Inherited teleprompter settings. They are
#'   not used by `ReAnchor`.
#'
#' @return A `ReAnchor` object to pass to [compile()].
#' @family teleprompters
#' @family decisions
#' @export
#' @examples
#' ReAnchor(metric = metric_exact_match(field = "match"), folds = 3L)
#'
#' \dontrun{
#' sig <- signature(
#'   inputs = list(input("pair", description = "Two product listings")),
#'   output_type = ellmer::type_object(
#'     match = ellmer::type_boolean("Do the listings describe the same item?")
#'   )
#' )
#' matcher <- module(sig) |> with_decisions(match = decision_bool())
#'
#' tuned <- compile(
#'   matcher,
#'   ReAnchor(metric = metric_exact_match(field = "match")),
#'   trainset,
#'   valset = valset,
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' decision_settings(tuned)
#' optimization_result(tuned)$extensions$re_anchor
#' }
ReAnchor <- S7::new_class(
  "ReAnchor",
  parent = Teleprompter,
  properties = list(
    fields = S7::new_property(
      S7::new_union(NULL, S7::class_character),
      default = NULL,
      validator = function(value) {
        if (
          !is.null(value) &&
            (length(value) == 0L || anyNA(value) || !all(nzchar(value)))
        ) {
          return("fields must be NULL or non-empty field names")
        }
        NULL
      }
    ),
    folds = S7::new_property(
      S7::class_integer,
      default = 5L,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || value < 2L) {
          return("folds must be a single integer of at least 2")
        }
        NULL
      }
    ),
    max_candidates = S7::new_property(
      S7::class_integer,
      default = 40L,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || value < 3L) {
          return("max_candidates must be a single integer of at least 3")
        }
        NULL
      }
    )
  )
)

#' @noRd
reanchor_abort <- function(message, ..., .envir = parent.frame()) {
  cli::cli_abort(
    message,
    ...,
    class = "dsprrr_reanchor_error",
    .envir = .envir
  )
}

#' Compile method for ReAnchor
#' @noRd
compile_reanchor <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  ...
) {
  metric <- teleprompter@metric
  if (!is.function(metric)) {
    reanchor_abort("{.cls ReAnchor} requires a {.arg metric} function")
  }
  if (nrow(trainset) < 2L) {
    # The held-out fold check needs at least one row to fit on and another
    # to score, so a single row could never accept a new setting.
    reanchor_abort(c(
      "{.arg trainset} must contain at least two rows",
      "i" = "ReAnchor accepts a setting only after a held-out fold check."
    ))
  }
  if (!is.null(valset) && nrow(valset) == 0L) {
    valset <- NULL
  }
  if (
    !inherits(program, "PredictModule") ||
      !identical(class(program)[1], "PredictModule")
  ) {
    reanchor_abort(c(
      "{.cls ReAnchor} calibrates a single Predict module",
      "x" = "Got {.cls {class(program)[1]}}.",
      "i" = "Composite programs can change downstream requests when an upstream decision moves, so they are not calibrated exactly."
    ))
  }

  optimized <- copy_module(program)
  original <- module_decisions(optimized)
  plan <- reanchor_plan(optimized, teleprompter@fields)
  if (length(plan$targets) == 0L) {
    skipped <- paste0(names(plan$skipped), ": ", plan$skipped)
    reanchor_abort(c(
      "No output can be calibrated",
      "i" = "Calibration needs described boolean or enum outputs, or fields configured with {.fn with_decisions}.",
      stats::setNames(skipped, rep("x", length(skipped)))
    ))
  }

  before <- reanchor_evaluate(optimized, trainset, metric, .llm)
  if (identical(plan$decisions, original)) {
    evidence_run <- before
  } else {
    optimized$config$decisions <- plan$decisions
    evidence_run <- reanchor_evaluate(optimized, trainset, metric, .llm)
  }

  score <- function(decisions) {
    reanchor_rescore(evidence_run, decisions, trainset, metric)
  }
  folds <- reanchor_folds(nrow(trainset), teleprompter@folds)
  decisions <- plan$decisions
  current <- evidence_run$scores
  fitted_rows <- list()
  # Only the targeted fields are fitted; other configured decisions keep
  # their settings unchanged.
  for (field in plan$targets) {
    fit <- reanchor_fit_field(
      field = field,
      decisions = decisions,
      evidence = lapply(evidence_run$evidence, `[[`, field),
      score = score,
      base = current,
      folds = folds,
      max_candidates = teleprompter@max_candidates
    )
    decisions <- fit$decisions
    current <- fit$scores
    fitted_rows[[field]] <- fit$report
  }

  acceptance <- reanchor_select(
    before$scores,
    list(list(scores = current, key = 0, setting = decisions)),
    folds
  )
  accepted <- !is.null(acceptance$kept)
  optimized$config$decisions <- if (accepted) {
    decisions
  } else if (length(original) > 0L) {
    original
  } else {
    NULL
  }
  train_score <- if (accepted) mean(current) else mean(before$scores)

  report <- list(
    train_score_before = mean(before$scores),
    train_score_at_start = mean(evidence_run$scores),
    train_score = train_score,
    accepted = accepted,
    fitted = unname(fitted_rows),
    skipped = if (length(plan$skipped) > 0L) as.list(plan$skipped) else NULL
  )
  if (!accepted) {
    report$reason <- if (isTRUE(acceptance$refused)) {
      "fitted settings failed the fold check against the original behavior"
    } else {
      "fitted settings did not beat the original behavior"
    }
  }
  if (!is.null(valset)) {
    report$val_score_before <- mean(
      reanchor_evaluate(program, valset, metric, .llm)$scores
    )
    report$val_score <- if (accepted) {
      mean(reanchor_evaluate(optimized, valset, metric, .llm)$scores)
    } else {
      report$val_score_before
    }
  }

  record_optimization_result(
    optimized,
    optimizer = "ReAnchor",
    baseline_score = report$train_score_before,
    best_score = train_score,
    best_params = list(
      decisions = reanchor_settings(module_decisions(optimized))
    ),
    stop_reason = if (accepted) "completed" else "no_improvement",
    extensions = report
  )
  optimized
}

#' Decide which fields to calibrate and with which starting settings
#' @noRd
reanchor_plan <- function(module, fields = NULL) {
  output_type <- module$signature@output_type
  existing <- module_decisions(module)
  if (!inherits(output_type, "ellmer::TypeObject")) {
    return(list(
      decisions = existing,
      targets = character(),
      skipped = character()
    ))
  }
  candidates <- names(output_type@properties)
  if (!is.null(fields)) {
    unknown <- setdiff(fields, candidates)
    if (length(unknown) > 0L) {
      reanchor_abort(
        "Unknown output field{?s} to calibrate: {.field {unknown}}"
      )
    }
    candidates <- fields
  }
  decisions <- existing[intersect(names(existing), candidates)]
  skipped <- character()
  for (field in setdiff(candidates, names(existing))) {
    field_type <- output_type@properties[[field]]
    spec <- if (
      inherits(field_type, "ellmer::TypeBasic") &&
        identical(field_type@type, "boolean")
    ) {
      decision_bool()
    } else if (inherits(field_type, "ellmer::TypeEnum")) {
      decision_choice()
    } else {
      NULL
    }
    if (is.null(spec)) {
      if (!is.null(fields)) {
        skipped[[field]] <- "not a boolean or enum output"
      }
      next
    }
    if (
      is.null(field_type@description) || !nzchar(trimws(field_type@description))
    ) {
      skipped[[field]] <- "output has no description to ask as a question"
      next
    }
    decisions[[field]] <- resolve_decision_spec(spec, field, output_type)
  }
  # Preserve existing configuration for fields outside the calibration set.
  list(
    decisions = c(
      decisions,
      existing[setdiff(names(existing), names(decisions))]
    ),
    targets = names(decisions) %||% character(),
    skipped = skipped
  )
}

#' Run a module over a data frame and score every row
#' @noRd
reanchor_evaluate <- function(module, data, metric, .llm) {
  trace_count_before <- evaluation_trace_cursor(module)
  results <- run_dataset(
    module,
    data,
    .llm = .llm,
    .progress = FALSE,
    .return_format = "structured"
  )
  errors <- results$.error
  failed <- which(!is.na(errors) & nzchar(errors))
  if (length(failed) > 0L) {
    reanchor_abort(c(
      "Row {failed[[1]]} failed during calibration",
      "x" = "{errors[[failed[[1]]]]}",
      "i" = "ReAnchor stops at the first program error so every candidate is scored on the same rows."
    ))
  }
  # Metrics see the same row-aligned trace events as they would in evaluate().
  events <- attr(results, "dsprrr_row_trace_events", exact = TRUE)
  if (!is.list(events) || length(events) != nrow(results)) {
    events <- align_evaluation_trace_events(
      new_evaluation_trace_events(module, trace_count_before),
      nrow(results)
    )
  }
  run <- list(
    outputs = results$result,
    metadata = results$.metadata,
    events = events,
    evidence = lapply(results$.metadata, function(item) item$decisions)
  )
  run$scores <- reanchor_score_outputs(
    run$outputs,
    run$metadata,
    run$events,
    data,
    metric
  )
  run
}

#' @noRd
reanchor_score_outputs <- function(outputs, metadata, events, data, metric) {
  scores <- vapply(
    seq_along(outputs),
    function(i) {
      trace <- new_program_trace(
        events = events[[i]] %||% list(),
        metadata = metadata[[i]],
        row_id = i,
        epoch = 1L
      )
      normalize_metric_result(
        invoke_metric(metric, outputs[[i]], data[i, , drop = FALSE], trace)
      )$score
    },
    numeric(1)
  )
  if (!all(is.finite(scores))) {
    reanchor_abort("ReAnchor requires finite metric scores")
  }
  scores
}

#' Re-decode recorded evidence with new settings and score it
#' @noRd
reanchor_rescore <- function(run, decisions, data, metric) {
  outputs <- run$outputs
  metadata <- run$metadata
  for (i in seq_along(outputs)) {
    for (field in names(decisions)) {
      if (is.null(run$evidence[[i]][[field]])) {
        # An omitted optional decision has nothing to re-decode.
        next
      }
      record <- decision_decode_field(
        run$evidence[[i]][[field]],
        decisions[[field]],
        field
      )
      outputs[[i]][[field]] <- record$value
      # Keep trace metadata consistent with the candidate's decoded values.
      metadata[[i]]$decisions[[field]] <- record
    }
  }
  reanchor_score_outputs(outputs, metadata, run$events, data, metric)
}

#' Example indexes split into up to `k` parts, the same way on every run
#' @noRd
reanchor_folds <- function(n, k) {
  order <- withr::with_seed(0L, sample.int(n))
  k <- min(k, n)
  lapply(seq_len(k), function(i) order[seq.int(i, n, by = k)])
}

#' The shortest decimal strictly between `lo` and `hi`, rounded from `x`
#' @noRd
reanchor_tidy <- function(x, lo, hi) {
  for (digits in 1:11) {
    rounded <- round(x, digits)
    if (lo < rounded && rounded < hi) {
      return(rounded)
    }
  }
  x
}

#' Candidate settings and the width of the gap each sits in
#' @noRd
reanchor_gaps <- function(values, lo, hi, max_candidates, geometric = FALSE) {
  inner <- sort(unique(values[values > lo & values < hi]))
  if (length(inner) > max_candidates - 1L) {
    step <- (length(inner) - 1) / (max_candidates - 2)
    inner <- sort(unique(inner[
      round(seq.int(0, max_candidates - 2) * step) + 1L
    ]))
  }
  points <- c(lo, inner, hi)
  gaps <- list()
  for (i in seq_len(length(points) - 1L)) {
    a <- points[[i]]
    b <- points[[i + 1L]]
    if (geometric) {
      middle <- sqrt(a * b)
      width <- log(b / a)
    } else {
      middle <- (a + b) / 2
      width <- b - a
    }
    if (a < middle && middle < b) {
      gaps[[length(gaps) + 1L]] <- list(
        value = reanchor_tidy(middle, a, b),
        width = width
      )
    }
  }
  gaps
}

#' Lexicographic comparison of (total, key...) tuples
#' @noRd
reanchor_better <- function(a, b) {
  for (i in seq_along(a)) {
    if (a[[i]] > b[[i]]) {
      return(TRUE)
    }
    if (a[[i]] < b[[i]]) {
      return(FALSE)
    }
  }
  FALSE
}

#' The best candidate on `rows`, or NULL when none beats `start` there
#' @noRd
reanchor_pick <- function(start, tried, rows) {
  best <- NULL
  best_rank <- NULL
  for (candidate in tried) {
    rank <- c(sum(candidate$scores[rows]), candidate$key)
    if (is.null(best) || reanchor_better(rank, best_rank)) {
      best <- candidate
      best_rank <- rank
    }
  }
  if (!is.null(best) && best_rank[[1]] > sum(start[rows])) best else NULL
}

#' Keep the best candidate only when it also wins on held-out folds
#' @noRd
reanchor_select <- function(start, tried, folds) {
  all_rows <- seq_along(start)
  best <- reanchor_pick(start, tried, all_rows)
  if (is.null(best)) {
    return(list(kept = NULL, refused = FALSE))
  }
  held <- 0
  held_start <- 0
  for (fold in folds) {
    pick <- reanchor_pick(start, tried, setdiff(all_rows, fold))
    held <- held + sum((if (is.null(pick)) start else pick$scores)[fold])
    held_start <- held_start + sum(start[fold])
  }
  if (held > held_start) {
    list(kept = best, refused = FALSE)
  } else {
    list(kept = NULL, refused = TRUE)
  }
}

#' @noRd
reanchor_summary <- function(values) {
  distinct <- sort(unique(values))
  if (length(distinct) == 0L) {
    return(list(calls = 0L))
  }
  list(
    calls = length(values),
    distinct = length(distinct),
    min = round(distinct[[1]], 4),
    max = round(distinct[[length(distinct)]], 4)
  )
}

#' Fit one field's setting against recorded evidence
#' @noRd
reanchor_fit_field <- function(
  field,
  decisions,
  evidence,
  score,
  base,
  folds,
  max_candidates
) {
  decision <- decisions[[field]]
  evidence <- Filter(Negate(is.null), evidence)
  check <- list(passed = 0L, failed = 0L)
  best_scores <- base
  candidates <- 0L

  try_candidates <- function(tried) {
    candidates <<- candidates + length(tried)
    selected <- reanchor_select(best_scores, tried, folds)
    check$passed <<- check$passed + as.integer(!is.null(selected$kept))
    check$failed <<- check$failed + as.integer(selected$refused)
    if (!is.null(selected$kept)) {
      best_scores <<- selected$kept$scores
    }
    selected$kept$setting
  }
  with_setting <- function(parameter, value) {
    candidate <- decisions
    candidate[[field]][[parameter]] <- value
    candidate
  }

  if (identical(decision$kind, "bool")) {
    parameter <- "threshold"
    start <- decision$threshold
    probabilities <- vapply(evidence, `[[`, numeric(1), "probability")
    gaps <- reanchor_gaps(probabilities, 0, 1, max_candidates)
    if (any(probabilities == 0)) {
      # P(TRUE) >= 0 is the only threshold that classifies zero as TRUE.
      gaps[[length(gaps) + 1L]] <- list(value = 0, width = 0)
    }
    tried <- lapply(gaps, function(gap) {
      list(
        scores = score(with_setting(parameter, gap$value)),
        key = c(gap$width, -abs(gap$value - start), gap$value),
        setting = gap$value
      )
    })
    best <- try_candidates(tried) %||% start
    observed <- c(reanchor_summary(probabilities), candidates = candidates)
  } else if (identical(decision$kind, "score")) {
    parameter <- "cuts"
    start <- decision$cuts
    best <- start
    top <- length(decision$values) - 1
    means <- vapply(evidence, `[[`, numeric(1), "score")
    for (i in seq_along(best)) {
      below <- if (i > 1L) best[[i - 1L]] else 0
      above <- if (i < length(best)) best[[i + 1L]] else top
      tried <- list()
      for (gap in reanchor_gaps(means, below, above, max_candidates)) {
        if (gap$value == best[[i]]) {
          next
        }
        candidate <- best
        candidate[[i]] <- gap$value
        tried[[length(tried) + 1L]] <- list(
          scores = score(with_setting(parameter, candidate)),
          key = gap$width,
          setting = candidate
        )
      }
      best <- try_candidates(tried) %||% best
    }
    observed <- c(reanchor_summary(means), candidates = candidates)
  } else {
    parameter <- "weights"
    start <- unlist(decision$weights)
    best <- start
    labels <- names(best)
    for (label in if (length(labels) > 1L) labels else character()) {
      flips <- unlist(lapply(evidence, function(record) {
        p <- record$probabilities
        others <- setdiff(labels, label)
        rival <- max(best[others] * p[others])
        if (p[[label]] > 0 && rival > 0) rival / p[[label]] else NULL
      }))
      # Bracket every observed flip point so any pick can be reached.
      flips <- flips[is.finite(flips)]
      low <- 1e-3
      high <- 1e3
      if (length(flips) > 0L) {
        low <- min(flips) / 4
        high <- max(flips) * 4
      }
      tried <- list()
      if (low < high) {
        gaps <- reanchor_gaps(
          flips,
          low,
          high,
          max_candidates,
          geometric = TRUE
        )
        for (gap in gaps) {
          if (gap$value == best[[label]]) {
            next
          }
          candidate <- best
          candidate[[label]] <- gap$value
          tried[[length(tried) + 1L]] <- list(
            scores = score(with_setting(parameter, as.list(candidate))),
            key = gap$width,
            setting = candidate
          )
        }
      }
      best <- try_candidates(tried) %||% best
    }
    observed <- list(calls = length(evidence), candidates = candidates)
    best <- as.list(best)
    start <- as.list(start)
  }

  decisions[[field]][[parameter]] <- best
  list(
    decisions = decisions,
    scores = best_scores,
    report = list(
      field = field,
      kind = decision$kind,
      parameter = parameter,
      start = start,
      value = best,
      train_score_at_start = mean(base),
      train_score = mean(best_scores),
      observed = observed,
      fold_check = check
    )
  )
}

#' Compact numeric settings for optimization results
#' @noRd
reanchor_settings <- function(decisions) {
  lapply(decisions, function(decision) {
    decision[intersect(
      c("kind", "threshold", "cuts", "weights"),
      names(decision)
    )]
  })
}
