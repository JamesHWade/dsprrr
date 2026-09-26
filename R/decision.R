#' Calibrated decision outputs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Decision outputs ask the model for *probability evidence* instead of a bare
#' answer, then decode that evidence locally. They mirror the experimental
#' decision types introduced in DSPy 3.4 (`Noul`, `Score`, and `Choice`):
#'
#' * `decision_bool()` for a `type_boolean()` output. The model reports
#'   P(TRUE), and the output is `TRUE` when that probability reaches
#'   `threshold`.
#' * `decision_score()` for an ordered `type_enum()` output (a rubric). The
#'   model reports a probability for every level. Their probability-weighted
#'   mean level index is a continuous score, and `cuts` map that score to a
#'   returned level.
#' * `decision_choice()` for an unordered `type_enum()` output. The model
#'   reports a probability for every option, and the option with the largest
#'   `probability * weight` is returned.
#'
#' Attach these specifications to a module with [with_decisions()]. The
#' signature keeps its ordinary output types, so predictions still contain a
#' native logical or character value that existing metrics can compare. The
#' probability evidence is available through [decision_evidence()].
#'
#' The numeric settings (`threshold`, `cuts`, and `weights`) are never sent to
#' the model. Changing them re-reads cached evidence without new provider
#' calls, which is what lets [ReAnchor()] fit them cheaply against a metric.
#'
#' @param threshold Probability in `[0, 1]` at or above which a Boolean
#'   decision is `TRUE`. Defaults to `0.5`.
#' @param cuts Increasing boundaries on the continuous score, which runs from
#'   `0` (first level) to `N - 1` (last level). There must be `N - 1` cuts,
#'   each strictly inside that range. `NULL` (the default) uses the midpoints
#'   `0.5, 1.5, ...`, which select the level nearest the score.
#' @param weights Named non-negative multipliers for the option
#'   probabilities. Omitted options use `1`. A zero weight disables an option.
#'   `NULL` (the default) weights every option equally.
#' @param criteria Optional descriptions of the outcomes, sent to the model.
#'   For `decision_bool()`, a list or character vector with elements named
#'   `true` and/or `false`. For `decision_score()`, one description per level,
#'   in level order. For `decision_choice()`, descriptions named by option.
#' @param description The question the model answers for this field. When
#'   `NULL`, the output type's own description is used. One of the two is
#'   required, because a decision needs an explicit question.
#'
#' @return A `dsprrr_decision_spec` object for use with [with_decisions()].
#' @seealso [with_decisions()], [decision_evidence()], [ReAnchor()]
#' @name decision_types
#' @examples
#' decision_bool(threshold = 0.7, criteria = c(true = "Service blocked"))
#' decision_score(criteria = c("Cosmetic", "Degraded", "Outage"))
#' decision_choice(weights = c(other = 0.5))
NULL

#' @rdname decision_types
#' @export
decision_bool <- function(
  threshold = 0.5,
  criteria = NULL,
  description = NULL
) {
  new_decision_spec(
    "bool",
    threshold = threshold,
    criteria = criteria,
    description = description
  )
}

#' @rdname decision_types
#' @export
decision_score <- function(cuts = NULL, criteria = NULL, description = NULL) {
  new_decision_spec(
    "score",
    cuts = cuts,
    criteria = criteria,
    description = description
  )
}

#' @rdname decision_types
#' @export
decision_choice <- function(
  weights = NULL,
  criteria = NULL,
  description = NULL
) {
  new_decision_spec(
    "choice",
    weights = weights,
    criteria = criteria,
    description = description
  )
}

#' @noRd
new_decision_spec <- function(kind, ..., criteria = NULL, description = NULL) {
  if (
    !is.null(description) &&
      (!is.character(description) ||
        length(description) != 1L ||
        is.na(description) ||
        !nzchar(trimws(description)))
  ) {
    decision_abort("{.arg description} must be one non-empty string or NULL")
  }
  parameters <- list(...)
  spec <- c(
    list(kind = kind),
    parameters[!vapply(parameters, is.null, logical(1))],
    list(criteria = criteria, description = description)
  )
  spec <- spec[!vapply(spec, is.null, logical(1))]
  structure(spec, class = "dsprrr_decision_spec")
}

#' @export
print.dsprrr_decision_spec <- function(x, ...) {
  cli::cli_text("{.cls dsprrr_decision_spec} {.val {x$kind}} decision")
  for (name in intersect(c("threshold", "cuts", "weights"), names(x))) {
    value <- x[[name]]
    shown <- if (is.null(names(value))) {
      format(value)
    } else {
      paste0(names(value), " = ", format(value))
    }
    cli::cli_text("{name}: {paste(shown, collapse = ', ')}")
  }
  invisible(x)
}

#' @noRd
decision_abort <- function(message, ..., .envir = parent.frame()) {
  cli::cli_abort(
    message,
    ...,
    class = "dsprrr_decision_config_error",
    .envir = .envir
  )
}

#' Attach calibrated decision outputs to a module
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Returns a copy of a Predict module in which the named output fields are
#' decoded from probability evidence. Each field must be declared in the
#' module's object-shaped signature output: a `type_boolean()` field for
#' [decision_bool()], or a `type_enum()` field for [decision_score()] and
#' [decision_choice()].
#'
#' A decision field needs a question: either the output type's description
#' (for example `type_boolean("Is the service blocked?")`) or the spec's
#' `description`. Without one, `with_decisions()` fails rather than asking an
#' unspecified question.
#'
#' Pass `NULL` for a field to remove its decision configuration and return
#' that field to direct generation.
#'
#' Decision modules run on the sequential execution path. Concurrent batch
#' backends are rejected rather than returning undecoded evidence.
#'
#' @param module A module created by [module()] or [chain_of_thought()].
#' @param ... Named [decision_types] specifications, or `NULL`, keyed by
#'   output field name.
#'
#' @return A modified copy of `module`. The original is unchanged.
#' @seealso [decision_types], [decision_evidence()], [decision_settings()],
#'   [ReAnchor()]
#' @export
#' @examples
#' sig <- signature(
#'   inputs = list(input("ticket", description = "Customer report")),
#'   output_type = ellmer::type_object(
#'     urgent = ellmer::type_boolean("Is the service blocked?"),
#'     severity = ellmer::type_enum(
#'       c("minor", "disruptive", "blocking"),
#'       "How severe is the impact?"
#'     ),
#'     category = ellmer::type_enum(
#'       c("billing", "technical"),
#'       "Which team owns the issue?"
#'     )
#'   )
#' )
#'
#' triage <- module(sig) |>
#'   with_decisions(
#'     urgent = decision_bool(threshold = 0.7),
#'     severity = decision_score(),
#'     category = decision_choice(
#'       criteria = c(billing = "Payments", technical = "Product faults")
#'     )
#'   )
#'
#' decision_settings(triage)
with_decisions <- function(module, ...) {
  assert_decision_module(module)
  specs <- rlang::list2(...)
  if (length(specs) == 0L) {
    return(module)
  }
  spec_names <- names(specs)
  if (
    is.null(spec_names) || !all(nzchar(spec_names)) || anyDuplicated(spec_names)
  ) {
    decision_abort("All decision specifications must have unique field names")
  }

  decisions <- module_decisions(module)
  output_type <- module$signature@output_type
  for (field in spec_names) {
    spec <- specs[[field]]
    if (is.null(spec)) {
      decisions[[field]] <- NULL
      next
    }
    if (!inherits(spec, "dsprrr_decision_spec")) {
      decision_abort(c(
        "Decision for {.field {field}} must be created with a decision helper",
        "i" = "Use {.fn decision_bool}, {.fn decision_score}, or {.fn decision_choice}."
      ))
    }
    decisions[[field]] <- resolve_decision_spec(spec, field, output_type)
  }

  updated <- artifact_copy_runtime(module, module$deepcopy())
  updated$config$decisions <- if (length(decisions) > 0L) decisions else NULL
  updated
}

#' @noRd
assert_decision_module <- function(module) {
  if (
    !inherits(module, "PredictModule") ||
      !identical(class(module)[1], "PredictModule")
  ) {
    decision_abort(c(
      "Decision outputs require a Predict module",
      "x" = "Got {.cls {class(module)[1]}}.",
      "i" = "Create the module with {.fn module} or {.fn chain_of_thought}."
    ))
  }
  invisible(module)
}

#' Stored decision configuration for a module
#' @noRd
module_decisions <- function(module) {
  decisions <- tryCatch(module$config$decisions, error = function(e) NULL)
  if (is.null(decisions) || length(decisions) == 0L) {
    return(list())
  }
  decisions
}

#' Validate a spec against its output field and fill in defaults
#' @noRd
resolve_decision_spec <- function(spec, field, output_type) {
  if (!inherits(output_type, "ellmer::TypeObject")) {
    decision_abort(c(
      "Decision outputs must be fields of an object-shaped signature output",
      "i" = "Declare the output with {.fn ellmer::type_object} or string notation such as {.code \"text -> urgent: bool\"}."
    ))
  }
  field_type <- output_type@properties[[field]]
  if (is.null(field_type)) {
    decision_abort(
      "Decision field {.field {field}} is not a declared signature output"
    )
  }
  description <- spec$description %||% field_type@description
  if (
    is.null(description) ||
      !is.character(description) ||
      !nzchar(trimws(description))
  ) {
    decision_abort(c(
      "Decision field {.field {field}} needs a question",
      "i" = "Describe the output type or pass {.arg description} to the decision helper."
    ))
  }

  kind <- spec$kind
  resolved <- list(kind = kind, description = description)
  if (!isTRUE(field_type@required)) {
    # Keep the signature's optionality in the evidence schema and decoding.
    resolved$optional <- TRUE
  }
  if (identical(kind, "bool")) {
    if (
      !inherits(field_type, "ellmer::TypeBasic") ||
        !identical(field_type@type, "boolean")
    ) {
      decision_abort(
        "{.fn decision_bool} requires {.field {field}} to be a boolean output"
      )
    }
    resolved$threshold <- validate_decision_threshold(spec$threshold, field)
    criteria <- spec$criteria
    if (!is.null(criteria)) {
      criteria <- as.list(criteria)
      if (
        is.null(names(criteria)) ||
          !all(names(criteria) %in% c("true", "false")) ||
          anyDuplicated(names(criteria))
      ) {
        decision_abort(
          "Boolean criteria for {.field {field}} must be named {.val true} and/or {.val false}"
        )
      }
      resolved$criteria <- lapply(
        criteria,
        validate_decision_text,
        field = field
      )
    }
    return(resolved)
  }

  if (!inherits(field_type, "ellmer::TypeEnum")) {
    decision_abort(
      "{.fn decision_{kind}} requires {.field {field}} to be an enum output"
    )
  }
  values <- field_type@values
  if (anyDuplicated(values)) {
    decision_abort("Enum values for {.field {field}} must be distinct")
  }
  resolved$values <- values

  if (identical(kind, "score")) {
    n <- length(values)
    if (n < 2L || n > 10L) {
      decision_abort(
        "{.fn decision_score} requires 2 to 10 ordered levels for {.field {field}}"
      )
    }
    resolved$cuts <- validate_decision_cuts(
      spec$cuts %||% (seq_len(n - 1L) - 0.5),
      n,
      field
    )
    if (!is.null(spec$criteria)) {
      criteria <- as.list(spec$criteria)
      if (length(criteria) != n || !is.null(names(spec$criteria))) {
        decision_abort(
          "Score criteria for {.field {field}} must be an unnamed list or vector with one entry per level"
        )
      }
      resolved$criteria <- unname(lapply(
        criteria,
        validate_decision_text,
        field = field
      ))
    }
    return(resolved)
  }

  resolved$weights <- validate_decision_weights(spec$weights, values, field)
  if (!is.null(spec$criteria)) {
    criteria <- as.list(spec$criteria)
    if (
      is.null(names(criteria)) ||
        !all(names(criteria) %in% values) ||
        anyDuplicated(names(criteria))
    ) {
      decision_abort(
        "Choice criteria for {.field {field}} must be named by its enum values"
      )
    }
    resolved$criteria <- lapply(criteria, validate_decision_text, field = field)
  }
  resolved
}

#' @noRd
validate_decision_text <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    decision_abort(
      "Criteria for {.field {field}} must be single non-missing strings"
    )
  }
  x
}

#' @noRd
validate_decision_threshold <- function(threshold, field) {
  if (
    !is.numeric(threshold) ||
      length(threshold) != 1L ||
      is.na(threshold) ||
      threshold < 0 ||
      threshold > 1
  ) {
    decision_abort(
      "Threshold for {.field {field}} must be one number in [0, 1]"
    )
  }
  as.numeric(threshold)
}

#' @noRd
validate_decision_cuts <- function(cuts, n, field) {
  if (
    !is.numeric(cuts) ||
      length(cuts) != n - 1L ||
      anyNA(cuts) ||
      any(cuts <= 0) ||
      any(cuts >= n - 1L) ||
      (length(cuts) > 1L && any(diff(cuts) <= 0))
  ) {
    decision_abort(c(
      "Invalid cuts for {.field {field}}",
      "i" = "Supply {n - 1} increasing values strictly between 0 and {n - 1}."
    ))
  }
  as.numeric(unname(cuts))
}

#' @noRd
validate_decision_weights <- function(weights, values, field) {
  resolved <- stats::setNames(rep(1, length(values)), values)
  if (is.null(weights)) {
    return(as.list(resolved))
  }
  weights <- unlist(weights)
  if (
    !is.numeric(weights) ||
      is.null(names(weights)) ||
      !all(names(weights) %in% values) ||
      anyDuplicated(names(weights)) ||
      anyNA(weights) ||
      !all(is.finite(weights)) ||
      any(weights < 0)
  ) {
    decision_abort(c(
      "Invalid weights for {.field {field}}",
      "i" = "Weights must be finite, non-negative, and named by enum values."
    ))
  }
  resolved[names(weights)] <- weights
  if (!any(resolved > 0)) {
    decision_abort("At least one weight for {.field {field}} must be positive")
  }
  as.list(resolved)
}

#' Decision settings of a module
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Lists the effective numeric settings of every decision output configured
#' with [with_decisions()], including values fitted by [ReAnchor()].
#'
#' @param module A module.
#' @return A tibble with one row per decision field and columns `field`,
#'   `kind`, `threshold` (Boolean decisions), `cuts` (Score decisions, a
#'   list-column), and `weights` (Choice decisions, a list-column of named
#'   numeric vectors).
#' @seealso [with_decisions()]
#' @export
decision_settings <- function(module) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a dsprrr module")
  }
  decisions <- module_decisions(module)
  tibble::tibble(
    field = names(decisions) %||% character(),
    kind = vapply(decisions, `[[`, character(1), "kind", USE.NAMES = FALSE),
    threshold = vapply(
      decisions,
      function(x) x$threshold %||% NA_real_,
      numeric(1),
      USE.NAMES = FALSE
    ),
    cuts = unname(lapply(decisions, function(x) x$cuts)),
    weights = unname(lapply(decisions, function(x) {
      if (is.null(x$weights)) NULL else unlist(x$weights)
    }))
  )
}

#' Build the provider request type for a module's decision fields
#'
#' Decision fields are replaced with closed evidence schemas. The numeric
#' settings are deliberately absent, so they never change the request or the
#' cache key.
#' @noRd
decision_request_type <- function(output_type, decisions) {
  if (length(decisions) == 0L) {
    return(output_type)
  }
  properties <- output_type@properties
  for (field in names(decisions)) {
    properties[[field]] <- decision_evidence_type(decisions[[field]])
  }
  ellmer::TypeObject(
    properties = properties,
    description = output_type@description,
    required = output_type@required,
    additional_properties = output_type@additional_properties
  )
}

#' @noRd
decision_question <- function(decision) {
  question <- list(
    type = switch(
      decision$kind,
      bool = "boolean",
      score = "score",
      choice = "choice"
    ),
    question = decision$description
  )
  if (identical(decision$kind, "score")) {
    question$levels <- lapply(seq_along(decision$values), function(i) {
      level <- list(index = i - 1L, level = decision$values[[i]])
      if (!is.null(decision$criteria)) {
        level$description <- decision$criteria[[i]]
      }
      level
    })
  } else if (identical(decision$kind, "choice")) {
    question$options <- lapply(decision$values, function(value) {
      option <- list(option = value)
      if (!is.null(decision$criteria[[value]])) {
        option$description <- decision$criteria[[value]]
      }
      option
    })
  } else if (!is.null(decision$criteria)) {
    question$criteria <- decision$criteria
  }
  as.character(jsonlite::toJSON(question, auto_unbox = TRUE))
}

#' @noRd
decision_evidence_type <- function(decision) {
  description <- paste(
    "Report probability evidence for this decision rather than a bare answer.",
    decision_question(decision)
  )
  required <- !isTRUE(decision$optional)
  if (identical(decision$kind, "bool")) {
    return(ellmer::type_object(
      .description = description,
      probability = ellmer::type_number(
        "Probability, from 0 to 1, that the answer is true."
      ),
      .required = required
    ))
  }
  labels <- if (identical(decision$kind, "score")) {
    as.character(seq_along(decision$values) - 1L)
  } else {
    decision$values
  }
  probability_fields <- stats::setNames(
    lapply(labels, function(label) ellmer::type_number()),
    labels
  )
  probabilities <- do.call(
    ellmer::type_object,
    c(
      list(
        .description = if (identical(decision$kind, "score")) {
          "Probability of each level index; the probabilities sum to 1."
        } else {
          "Probability of each option; the probabilities sum to 1."
        }
      ),
      probability_fields
    )
  )
  ellmer::type_object(
    .description = description,
    probabilities = probabilities,
    confidence = ellmer::type_number(
      "Confidence in the decision, from 0 to 1."
    ),
    .required = required
  )
}

#' @noRd
decision_evidence_abort <- function(message, ..., .envir = parent.frame()) {
  cli::cli_abort(
    message,
    ...,
    class = "dsprrr_decision_evidence_error",
    .envir = .envir
  )
}

#' @noRd
decision_probability <- function(x, field, what) {
  if (is.list(x) && length(x) == 1L) {
    x <- x[[1]]
  }
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 0 ||
      x > 1
  ) {
    decision_evidence_abort(
      "Invalid {what} for decision {.field {field}}: expected a probability in [0, 1]"
    )
  }
  as.numeric(x)
}

#' Parse one field's raw evidence into a normalized record
#' @noRd
decision_parse_evidence <- function(raw, decision, field) {
  if (!is.list(raw)) {
    decision_evidence_abort(
      "Missing probability evidence for decision {.field {field}}"
    )
  }
  if (identical(decision$kind, "bool")) {
    return(list(
      probability = decision_probability(raw$probability, field, "probability")
    ))
  }
  labels <- if (identical(decision$kind, "score")) {
    as.character(seq_along(decision$values) - 1L)
  } else {
    decision$values
  }
  supplied <- raw$probabilities
  if (!is.list(supplied) || !all(labels %in% names(supplied))) {
    decision_evidence_abort(
      "Decision {.field {field}} needs a probability for every option"
    )
  }
  probabilities <- vapply(
    labels,
    function(label) {
      decision_probability(supplied[[label]], field, "option probability")
    },
    numeric(1)
  )
  if (sum(probabilities) <= 0) {
    decision_evidence_abort(
      "Decision {.field {field}} reported no probability mass"
    )
  }
  names(probabilities) <- decision$values
  list(
    probabilities = probabilities,
    confidence = decision_probability(raw$confidence, field, "confidence")
  )
}

#' Decode one field from normalized evidence and current settings
#' @noRd
decision_decode_field <- function(evidence, decision, field) {
  if (identical(decision$kind, "bool")) {
    p <- evidence$probability
    threshold <- decision$threshold
    return(list(
      kind = "bool",
      value = p >= threshold,
      probability = p,
      confidence = abs(p - threshold) / max(threshold, 1 - threshold)
    ))
  }
  probabilities <- evidence$probabilities
  if (identical(decision$kind, "score")) {
    score <- sum((seq_along(probabilities) - 1) * probabilities) /
      sum(probabilities)
    level <- sum(score >= decision$cuts)
    return(list(
      kind = "score",
      value = decision$values[[level + 1L]],
      score = score,
      level = as.integer(level),
      probabilities = probabilities,
      confidence = evidence$confidence
    ))
  }
  weights <- unlist(decision$weights)[names(probabilities)]
  weights[is.na(weights)] <- 1
  weighted <- probabilities * weights
  raw_winner <- which.max(probabilities)
  best <- max(weighted)
  if (best <= 0) {
    decision_evidence_abort(
      "Weights for decision {.field {field}} leave no positive probability mass"
    )
  }
  # Weighted ties prefer the raw winner, then declaration order.
  tied <- which(weighted == best)
  selected <- if (raw_winner %in% tied) raw_winner else tied[[1]]
  list(
    kind = "choice",
    value = names(probabilities)[[selected]],
    probabilities = probabilities,
    confidence = evidence$confidence
  )
}

#' Decode every decision field in a raw structured response
#'
#' @return `list(output = <response with native values>, decisions = <named
#'   list of decoded evidence records>)`.
#' @noRd
decision_decode_response <- function(response, decisions) {
  if (length(decisions) == 0L) {
    return(list(output = response, decisions = NULL))
  }
  if (!is.list(response)) {
    decision_evidence_abort(
      "Decision outputs require an object-shaped structured response"
    )
  }
  records <- list()
  for (field in names(decisions)) {
    if (is.null(response[[field]]) && isTRUE(decisions[[field]]$optional)) {
      # An omitted optional decision stays absent, as it would without
      # evidence decoding.
      next
    }
    evidence <- decision_parse_evidence(
      response[[field]],
      decisions[[field]],
      field
    )
    record <- decision_decode_field(evidence, decisions[[field]], field)
    response[[field]] <- record$value
    records[[field]] <- record
  }
  list(output = response, decisions = records)
}

#' Call the provider for a module and decode any decision fields
#' @noRd
module_structured_call <- function(module, call) {
  decisions <- module_decisions(module)
  request_type <- decision_request_type(
    module$signature@output_type,
    decisions
  )
  decision_decode_response(call(request_type), decisions)
}

#' Reject execution paths that cannot decode decision evidence
#' @noRd
assert_decisions_supported <- function(module, path) {
  if (length(module_decisions(module)) > 0L) {
    cli::cli_abort(
      c(
        "Decision outputs are not supported by {path}",
        "x" = "This path would return undecoded probability evidence.",
        "i" = "Use sequential execution for modules configured with {.fn with_decisions}."
      ),
      class = c(
        "dsprrr_decision_unsupported_error",
        "dsprrr_concurrency_unsupported_error"
      )
    )
  }
  invisible(module)
}

#' Probability evidence behind decision outputs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Extracts the decoded evidence for decision outputs from structured results.
#'
#' @param x A structured result from `run(..., .return_format = "structured")`,
#'   a list of such results from a batch call, or a tibble returned by
#'   `run_dataset(..., .return_format = "structured")` or [evaluate()]'s
#'   `metadata` element.
#'
#' @return A tibble with one row per result and decision field. Columns:
#'   `row`, `field`, `kind`, `value` (the decoded native value, a list-column),
#'   `probability` (P(TRUE) for Boolean decisions), `score` (the continuous
#'   mean level index for Score decisions), `level` (the zero-based selected
#'   level), `confidence`, and `probabilities` (a list-column of named
#'   probabilities for Score and Choice decisions).
#'
#'   For Boolean decisions, `confidence` is the distance from the threshold,
#'   `abs(p - threshold) / max(threshold, 1 - threshold)`, not a calibrated
#'   probability. For Score and Choice decisions it is the model's
#'   self-reported confidence.
#' @seealso [with_decisions()]
#' @export
decision_evidence <- function(x) {
  metadata <- decision_result_metadata(x)
  rows <- list()
  for (i in seq_along(metadata)) {
    records <- metadata[[i]]$decisions
    for (field in names(records)) {
      record <- records[[field]]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        row = i,
        field = field,
        kind = record$kind,
        value = list(record$value),
        probability = record$probability %||% NA_real_,
        score = record$score %||% NA_real_,
        level = record$level %||% NA_integer_,
        confidence = record$confidence,
        probabilities = list(record$probabilities)
      )
    }
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(
      row = integer(),
      field = character(),
      kind = character(),
      value = list(),
      probability = numeric(),
      score = numeric(),
      level = integer(),
      confidence = numeric(),
      probabilities = list()
    ))
  }
  do.call(rbind, rows)
}

#' @noRd
decision_result_metadata <- function(x) {
  if (is.data.frame(x)) {
    if (!".metadata" %in% names(x)) {
      cli::cli_abort(c(
        "{.arg x} has no {.field .metadata} column",
        "i" = "Use {.code run_dataset(..., .return_format = \"structured\")}."
      ))
    }
    return(x$.metadata)
  }
  if (is.list(x) && !is.null(x$metadata) && !is.null(x$output)) {
    return(list(x$metadata))
  }
  if (
    is.list(x) &&
      length(x) > 0L &&
      all(vapply(
        x,
        function(item) is.list(item) && !is.null(item$metadata),
        logical(1)
      ))
  ) {
    return(lapply(x, `[[`, "metadata"))
  }
  if (is.list(x) && all(vapply(x, is.list, logical(1)))) {
    return(x)
  }
  cli::cli_abort(c(
    "Cannot find decision evidence in {.arg x}",
    "i" = "Use a structured result from {.fn run} or {.fn run_dataset}."
  ))
}
