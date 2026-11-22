# Resource limits are intentionally independent from execution concurrency.
# Optimizers schedule bounded work units and record the effective in-flight
# count reported by the shared concurrency contract.
optimizer_budget_limit_map <- function() {
  c(
    max_trials = "trials",
    max_metric_calls = "metric_calls",
    max_provider_calls = "provider_calls",
    max_input_tokens = "input_tokens",
    max_output_tokens = "output_tokens",
    max_total_tokens = "total_tokens",
    max_cost = "known_cost",
    max_elapsed_seconds = "elapsed_seconds"
  )
}

optimizer_control_limits <- function(control) {
  list(
    max_trials = control@max_trials,
    max_metric_calls = control@max_metric_calls,
    max_provider_calls = control@max_provider_calls,
    max_input_tokens = control@max_input_tokens,
    max_output_tokens = control@max_output_tokens,
    max_total_tokens = control@max_total_tokens,
    max_cost = control@max_cost,
    max_elapsed_seconds = control@max_elapsed_seconds
  )
}

optimizer_monotonic_clock <- function() {
  unname(proc.time()[["elapsed"]])
}

optimizer_budget_counter_names <- function() {
  c(
    "trials",
    "metric_calls",
    "provider_calls",
    "input_tokens",
    "output_tokens",
    "total_tokens",
    "known_cost"
  )
}

# Create mutable state for one optimizer-wide monotonic resource ledger.
new_optimizer_budget <- function(control = NULL, state = NULL, clock = NULL) {
  if (is.null(control)) {
    control <- optimizer_control()
  }
  if (!inherits(control, "dsprrr::OptimizerControl")) {
    cli::cli_abort(
      "{.arg control} must be created by {.fn optimizer_control}",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  if (is.null(clock)) {
    clock <- getOption("dsprrr.optimizer_clock", optimizer_monotonic_clock)
  }
  if (!is.function(clock)) {
    cli::cli_abort(
      "The optimizer monotonic clock must be a function",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  budget <- new.env(parent = emptyenv())
  budget$max_errors <- as.integer(control@max_errors)
  budget$limits <- optimizer_control_limits(control)
  budget$attempts <- 0L
  budget$successes <- 0L
  budget$total_errors <- 0L
  budget$consecutive_errors <- 0L
  for (name in optimizer_budget_counter_names()) {
    budget[[name]] <- if (name == "known_cost") 0 else 0L
  }
  budget$unknown_metric_calls <- 0L
  budget$unknown_provider_calls <- 0L
  budget$unknown_input_tokens <- 0L
  budget$unknown_output_tokens <- 0L
  budget$unknown_total_tokens <- 0L
  budget$unknown_cost_calls <- 0L
  budget$trial_units <- character()
  budget$completed_units <- character()
  budget$overshoots <- list()
  budget$stop_reason <- NULL
  budget$clock <- clock
  budget$started_tick <- as.numeric(clock())
  budget$elapsed_offset <- 0
  budget$last_elapsed <- 0
  class(budget) <- c("dsprrr_optimizer_budget", "environment")

  if (!is.null(state)) {
    optimizer_budget_restore_state(budget, state)
    optimizer_budget_reconcile_current_limits(budget)
  }

  budget
}

optimizer_budget_elapsed <- function(budget) {
  if (!inherits(budget, "dsprrr_optimizer_budget")) {
    cli::cli_abort(
      "{.arg budget} must be an optimizer budget",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  tick <- as.numeric(budget$clock())
  delta <- max(0, tick - budget$started_tick)
  elapsed <- max(budget$last_elapsed, budget$elapsed_offset + delta)
  budget$last_elapsed <- elapsed
  elapsed
}

optimizer_budget_reason <- function(
  code,
  stage,
  resource,
  limit,
  observed,
  unit_id = NULL,
  overshoot = 0,
  work_unit = "optimizer_work_unit",
  max_started = 1L,
  message = NULL
) {
  if (is.null(message)) {
    message <- sprintf("Reached %s limit (%s)", code, format(limit))
  }
  structure(
    list(
      code = code,
      stage = as.character(stage)[1L],
      resource = resource,
      limit = limit,
      observed = observed,
      unit_id = unit_id,
      overshoot = list(
        amount = overshoot,
        unit = work_unit,
        max_started = as.integer(max_started)
      ),
      message = message
    ),
    class = c("dsprrr_optimizer_stop_reason", "list")
  )
}

optimizer_budget_set_stop <- function(budget, reason) {
  if (is.null(budget$stop_reason)) {
    budget$stop_reason <- reason
  }
  invisible(budget)
}

optimizer_budget_validate_stage <- function(stage) {
  if (
    !is.character(stage) ||
      length(stage) != 1L ||
      is.na(stage) ||
      !nzchar(stage)
  ) {
    cli::cli_abort(
      "{.arg stage} must be a single non-empty string",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  stage
}

optimizer_budget_increment_abort <- function(
  resource,
  stage,
  current,
  increment
) {
  cli::cli_abort(
    c(
      "Optimizer budget counter {.field {resource}} cannot be incremented",
      "x" = "The increment would exceed its representable range.",
      "i" = "The budget ledger was not changed."
    ),
    class = c(
      "dsprrr_optimizer_budget_overflow",
      "dsprrr_optimizer_invariant_error"
    ),
    resource = resource,
    stage = stage,
    current = current,
    increment = increment
  )
}

optimizer_budget_checked_increment <- function(
  current,
  increment,
  resource,
  stage,
  integer = TRUE
) {
  if (
    !is.numeric(increment) ||
      length(increment) != 1L ||
      is.na(increment) ||
      !is.finite(increment) ||
      increment < 0 ||
      (integer &&
        (increment != floor(increment) ||
          increment > .Machine$integer.max))
  ) {
    cli::cli_abort(
      "Optimizer budget increment for {.field {resource}} is invalid",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  if (integer) {
    increment <- as.integer(increment)
    if (
      !is.integer(current) ||
        length(current) != 1L ||
        is.na(current) ||
        current < 0L
    ) {
      cli::cli_abort(
        "Optimizer budget counter {.field {resource}} is invalid",
        class = "dsprrr_optimizer_invariant_error"
      )
    }
    if (increment > .Machine$integer.max - current) {
      optimizer_budget_increment_abort(
        resource,
        stage,
        current,
        increment
      )
    }
    return(current + increment)
  }

  if (
    !is.numeric(current) ||
      length(current) != 1L ||
      is.na(current) ||
      !is.finite(current) ||
      current < 0
  ) {
    cli::cli_abort(
      "Optimizer budget amount {.field {resource}} is invalid",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  if (increment > .Machine$double.xmax - current) {
    optimizer_budget_increment_abort(resource, stage, current, increment)
  }
  current + increment
}

optimizer_budget_require_outcome_capacity <- function(
  budget,
  successes,
  total_errors,
  stage
) {
  attempts <- optimizer_budget_checked_increment(
    budget$attempts,
    successes,
    "attempts",
    stage
  )
  optimizer_budget_checked_increment(
    attempts,
    total_errors,
    "attempts",
    stage
  )
  optimizer_budget_checked_increment(
    budget$successes,
    successes,
    "successes",
    stage
  )
  optimizer_budget_checked_increment(
    budget$total_errors,
    total_errors,
    "total_errors",
    stage
  )
  streak <- if (successes > 0) 0L else budget$consecutive_errors
  optimizer_budget_checked_increment(
    streak,
    total_errors,
    "consecutive_errors",
    stage
  )
  invisible(budget)
}

optimizer_budget_limit_reason <- function(
  budget,
  limit_name,
  stage,
  observed,
  unit_id,
  overshoot = 0,
  work_unit = "optimizer_work_unit",
  max_started = 1L
) {
  resource <- optimizer_budget_limit_map()[[limit_name]]
  optimizer_budget_reason(
    code = limit_name,
    stage = stage,
    resource = resource,
    limit = budget$limits[[limit_name]],
    observed = observed,
    unit_id = unit_id,
    overshoot = overshoot,
    work_unit = work_unit,
    max_started = max_started
  )
}

# Check every known counter before scheduling an expensive work unit. Planned
# values are conservative lower bounds; postflight accounting may cross a cap
# only within the explicitly named, already-started work-unit bound.
optimizer_budget_preflight <- function(
  budget,
  stage,
  planned = list(),
  unit_id = NULL,
  work_unit = "optimizer_work_unit",
  max_started = 1L,
  planned_outcomes = 1L
) {
  optimizer_budget_validate_stage(stage)
  if (!inherits(budget, "dsprrr_optimizer_budget")) {
    cli::cli_abort(
      "{.arg budget} must be an optimizer budget",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  if (optimizer_budget_stopped(budget)) {
    return(FALSE)
  }

  optimizer_budget_require_outcome_capacity(
    budget,
    successes = planned_outcomes,
    total_errors = 0L,
    stage = stage
  )

  allowed <- optimizer_budget_counter_names()
  if (length(setdiff(names(planned), allowed)) > 0L) {
    cli::cli_abort(
      "{.arg planned} contains unknown optimizer resources",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  limit_map <- optimizer_budget_limit_map()
  projected <- list()
  amounts <- list()
  for (limit_name in setdiff(names(limit_map), "max_elapsed_seconds")) {
    resource <- limit_map[[limit_name]]
    amount <- planned[[resource]] %||% 0
    if (
      !is.numeric(amount) ||
        length(amount) != 1L ||
        is.na(amount) ||
        !is.finite(amount) ||
        amount < 0 ||
        (resource != "known_cost" && amount != floor(amount))
    ) {
      cli::cli_abort(
        "Planned optimizer resource {.field {resource}} is invalid",
        class = "dsprrr_optimizer_invariant_error"
      )
    }
    observed <- budget[[resource]]
    amounts[[limit_name]] <- amount
    projected[[limit_name]] <- optimizer_budget_checked_increment(
      observed,
      amount,
      resource,
      stage,
      integer = resource != "known_cost"
    )
  }

  elapsed <- optimizer_budget_elapsed(budget)
  elapsed_limit <- budget$limits$max_elapsed_seconds
  if (!is.null(elapsed_limit) && elapsed >= elapsed_limit) {
    optimizer_budget_set_stop(
      budget,
      optimizer_budget_limit_reason(
        budget,
        "max_elapsed_seconds",
        stage,
        observed = elapsed,
        unit_id = unit_id,
        work_unit = work_unit,
        max_started = 0L
      )
    )
    return(FALSE)
  }

  for (limit_name in names(projected)) {
    limit <- budget$limits[[limit_name]]
    resource <- limit_map[[limit_name]]
    observed <- budget[[resource]]
    if (
      !is.null(limit) &&
        amounts[[limit_name]] > 0 &&
        projected[[limit_name]] > limit
    ) {
      optimizer_budget_set_stop(
        budget,
        optimizer_budget_limit_reason(
          budget,
          limit_name,
          stage,
          observed = observed,
          unit_id = unit_id,
          work_unit = work_unit,
          max_started = 0L
        )
      )
      return(FALSE)
    }
  }

  TRUE
}

optimizer_budget_note_unknown <- function(
  budget,
  resource,
  stage,
  unit_id,
  work_unit,
  max_started
) {
  limit_name <- names(optimizer_budget_limit_map())[
    optimizer_budget_limit_map() == sub("_calls$", "", resource)
  ]
  if (resource == "cost_calls") {
    limit_name <- "max_cost"
  } else if (resource == "metric_calls") {
    limit_name <- "max_metric_calls"
  } else if (resource == "provider_calls") {
    limit_name <- "max_provider_calls"
  } else if (resource == "input_tokens") {
    limit_name <- "max_input_tokens"
  } else if (resource == "output_tokens") {
    limit_name <- "max_output_tokens"
  } else if (resource == "total_tokens") {
    limit_name <- "max_total_tokens"
  }
  limit_name <- limit_name[[1L]] %||% NULL
  if (!is.null(limit_name) && !is.null(budget$limits[[limit_name]])) {
    code <- paste0("unknown_", sub("_calls$", "", resource))
    if (resource == "cost_calls") {
      code <- "unknown_cost"
    }
    optimizer_budget_set_stop(
      budget,
      optimizer_budget_reason(
        code = code,
        stage = stage,
        resource = resource,
        limit = budget$limits[[limit_name]],
        observed = NA_real_,
        unit_id = unit_id,
        overshoot = NA_real_,
        work_unit = work_unit,
        max_started = max_started,
        message = sprintf(
          "Cannot enforce %s because the completed work unit reported unknown usage",
          limit_name
        )
      )
    )
  }
  invisible(budget)
}

# Record verified postflight usage. NA is a first-class unknown value and is
# never silently converted to zero.
record_optimizer_usage <- function(
  budget,
  usage,
  stage,
  unit_id = NULL,
  work_unit = "optimizer_work_unit",
  max_started = 1L
) {
  optimizer_budget_validate_stage(stage)
  if (!inherits(budget, "dsprrr_optimizer_budget") || !is.list(usage)) {
    cli::cli_abort(
      "Optimizer usage requires a budget and a named list",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  updates <- list()
  unknown_resources <- character()
  for (resource in optimizer_budget_counter_names()) {
    if (!resource %in% names(usage)) {
      next
    }
    value <- usage[[resource]]
    if (is.null(value) || length(value) != 1L || is.na(value)) {
      if (resource == "trials") {
        cli::cli_abort(
          "Observed optimizer resource {.field trials} cannot be unknown",
          class = "dsprrr_optimizer_invariant_error"
        )
      }
      unknown_name <- if (resource == "known_cost") {
        "cost_calls"
      } else {
        resource
      }
      counter <- paste0("unknown_", unknown_name)
      updates[[counter]] <- optimizer_budget_checked_increment(
        budget[[counter]],
        1L,
        counter,
        stage,
        integer = TRUE
      )
      unknown_resources <- c(unknown_resources, unknown_name)
      next
    }
    if (
      !is.numeric(value) ||
        !is.finite(value) ||
        value < 0 ||
        (resource != "known_cost" && value != floor(value))
    ) {
      cli::cli_abort(
        "Observed optimizer resource {.field {resource}} is invalid",
        class = "dsprrr_optimizer_invariant_error"
      )
    }
    updates[[resource]] <- optimizer_budget_checked_increment(
      budget[[resource]],
      value,
      resource,
      stage,
      integer = resource != "known_cost"
    )
  }

  for (name in names(updates)) {
    budget[[name]] <- updates[[name]]
  }
  for (resource in unknown_resources) {
    optimizer_budget_note_unknown(
      budget,
      resource,
      stage,
      unit_id,
      work_unit,
      max_started
    )
  }

  elapsed <- optimizer_budget_elapsed(budget)
  limit_map <- optimizer_budget_limit_map()
  for (limit_name in names(limit_map)) {
    resource <- limit_map[[limit_name]]
    observed <- if (resource == "elapsed_seconds") {
      elapsed
    } else {
      budget[[resource]]
    }
    limit <- budget$limits[[limit_name]]
    if (!is.null(limit) && observed >= limit) {
      overshoot <- max(0, observed - limit)
      if (overshoot > 0) {
        budget$overshoots[[length(budget$overshoots) + 1L]] <- list(
          resource = resource,
          amount = overshoot,
          unit = work_unit,
          max_started = as.integer(max_started),
          unit_id = unit_id
        )
      }
      optimizer_budget_set_stop(
        budget,
        optimizer_budget_limit_reason(
          budget,
          limit_name,
          stage,
          observed = observed,
          unit_id = unit_id,
          overshoot = overshoot,
          work_unit = work_unit,
          max_started = max_started
        )
      )
    }
  }

  invisible(budget)
}

optimizer_budget_complete_unit <- function(budget, unit_id) {
  if (
    !is.character(unit_id) ||
      length(unit_id) != 1L ||
      is.na(unit_id) ||
      !nzchar(unit_id)
  ) {
    cli::cli_abort(
      "{.arg unit_id} must be one non-empty string",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  if (!unit_id %in% budget$completed_units) {
    budget$completed_units <- c(budget$completed_units, unit_id)
  }
  invisible(budget)
}

optimizer_budget_count_trial <- function(budget, stage, unit_id) {
  if (!unit_id %in% budget$trial_units) {
    record_optimizer_usage(
      budget,
      list(trials = 1L),
      stage,
      unit_id = unit_id,
      work_unit = "optimizer_trial",
      max_started = 1L
    )
    budget$trial_units <- c(budget$trial_units, unit_id)
  }
  invisible(budget)
}

optimizer_budget_unit_completed <- function(budget, unit_id) {
  unit_id %in% budget$completed_units
}

optimizer_budget_state <- function(budget) {
  elapsed <- optimizer_budget_elapsed(budget)
  list(
    attempts = as.integer(budget$attempts),
    successes = as.integer(budget$successes),
    total_errors = as.integer(budget$total_errors),
    consecutive_errors = as.integer(budget$consecutive_errors),
    trials = as.integer(budget$trials),
    metric_calls = as.integer(budget$metric_calls),
    provider_calls = as.integer(budget$provider_calls),
    input_tokens = as.integer(budget$input_tokens),
    output_tokens = as.integer(budget$output_tokens),
    total_tokens = as.integer(budget$total_tokens),
    known_cost = as.numeric(budget$known_cost),
    unknown_metric_calls = as.integer(budget$unknown_metric_calls),
    unknown_provider_calls = as.integer(budget$unknown_provider_calls),
    unknown_input_tokens = as.integer(budget$unknown_input_tokens),
    unknown_output_tokens = as.integer(budget$unknown_output_tokens),
    unknown_total_tokens = as.integer(budget$unknown_total_tokens),
    unknown_cost_calls = as.integer(budget$unknown_cost_calls),
    elapsed_seconds = as.numeric(elapsed),
    trial_units = budget$trial_units,
    completed_units = budget$completed_units,
    overshoots = budget$overshoots,
    stop_reason = if (is.null(budget$stop_reason)) {
      NULL
    } else {
      unclass(budget$stop_reason)
    }
  )
}

optimizer_budget_checkpoint_abort <- function(message) {
  cli::cli_abort(
    message,
    class = "dsprrr_optimizer_checkpoint_malformed"
  )
}

optimizer_budget_checkpoint_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

optimizer_budget_checkpoint_unit_id <- function(value) {
  is.null(value) || optimizer_budget_checkpoint_text(value)
}

optimizer_budget_has_exact_outcome_partition <- function(
  attempts,
  successes,
  total_errors
) {
  # Subtraction is safe after non-negative integer validation; adding the two
  # partition counters could overflow before an inconsistent state is rejected.
  successes <= attempts &&
    total_errors <= attempts &&
    identical(attempts - successes, total_errors)
}

optimizer_budget_validate_overshoot <- function(record, nested = FALSE) {
  expected <- if (nested) {
    c("amount", "unit", "max_started")
  } else {
    c("resource", "amount", "unit", "max_started", "unit_id")
  }
  if (
    !artifact_is_plain_list(record) ||
      !identical(names(record), expected)
  ) {
    return(FALSE)
  }
  amount <- record$amount
  if (
    !is.numeric(amount) ||
      length(amount) != 1L ||
      (!is.na(amount) && (!is.finite(amount) || amount < 0)) ||
      !optimizer_budget_checkpoint_text(record$unit) ||
      !is.integer(record$max_started) ||
      length(record$max_started) != 1L ||
      is.na(record$max_started) ||
      record$max_started < 0L
  ) {
    return(FALSE)
  }
  if (nested) {
    return(TRUE)
  }
  optimizer_budget_checkpoint_text(record$resource) &&
    record$resource %in% unname(optimizer_budget_limit_map()) &&
    !is.na(amount) &&
    amount > 0 &&
    optimizer_budget_checkpoint_unit_id(record$unit_id)
}

optimizer_budget_validate_stop_reason <- function(reason) {
  if (is.null(reason)) {
    return(TRUE)
  }
  if (!artifact_is_plain_list(reason)) {
    return(FALSE)
  }
  if (identical(reason$code, "max_errors")) {
    expected <- c(
      "code",
      "stage",
      "limit",
      "observed",
      "total_errors",
      "attempts",
      "message"
    )
    if ("condition_class" %in% names(reason)) {
      expected <- c(expected, "condition_class")
    }
    counts <- reason[c("limit", "observed", "total_errors", "attempts")]
    return(
      identical(names(reason), expected) &&
        optimizer_budget_checkpoint_text(reason$stage) &&
        optimizer_budget_checkpoint_text(reason$message) &&
        all(vapply(
          counts,
          function(value) {
            is.integer(value) &&
              length(value) == 1L &&
              !is.na(value) &&
              value >= 0L
          },
          logical(1)
        )) &&
        reason$total_errors <= reason$attempts &&
        reason$observed <= reason$total_errors &&
        reason$observed >= max(1L, reason$limit) &&
        (!"condition_class" %in% names(reason) ||
          optimizer_budget_checkpoint_text(reason$condition_class))
    )
  }

  expected <- c(
    "code",
    "stage",
    "resource",
    "limit",
    "observed",
    "unit_id",
    "overshoot",
    "message"
  )
  if (!identical(names(reason), expected)) {
    return(FALSE)
  }
  unknown_resources <- c(
    unknown_cost = "cost_calls",
    unknown_provider = "provider_calls",
    unknown_metric = "metric_calls",
    unknown_input_tokens = "input_tokens",
    unknown_output_tokens = "output_tokens",
    unknown_total_tokens = "total_tokens"
  )
  limit_resources <- optimizer_budget_limit_map()
  allowed_codes <- c(names(limit_resources), names(unknown_resources))
  if (
    !optimizer_budget_checkpoint_text(reason$code) ||
      !reason$code %in% allowed_codes ||
      !optimizer_budget_checkpoint_text(reason$stage) ||
      !optimizer_budget_checkpoint_text(reason$resource) ||
      !is.numeric(reason$limit) ||
      length(reason$limit) != 1L ||
      is.na(reason$limit) ||
      !is.finite(reason$limit) ||
      reason$limit < 0 ||
      !is.numeric(reason$observed) ||
      length(reason$observed) != 1L ||
      !optimizer_budget_checkpoint_unit_id(reason$unit_id) ||
      !optimizer_budget_validate_overshoot(reason$overshoot, nested = TRUE) ||
      !optimizer_budget_checkpoint_text(reason$message)
  ) {
    return(FALSE)
  }
  expected_resource <- if (reason$code %in% names(limit_resources)) {
    unname(limit_resources[[reason$code]])
  } else {
    unname(unknown_resources[[reason$code]])
  }
  is_unknown <- reason$code %in% names(unknown_resources)
  identical(reason$resource, expected_resource) &&
    if (is_unknown) {
      is.na(reason$observed) && is.na(reason$overshoot$amount)
    } else {
      !is.na(reason$observed) &&
        is.finite(reason$observed) &&
        reason$observed >= 0 &&
        !is.na(reason$overshoot$amount)
    }
}

optimizer_budget_restore_state <- function(budget, state) {
  count_names <- c(
    "attempts",
    "successes",
    "total_errors",
    "consecutive_errors",
    setdiff(optimizer_budget_counter_names(), "known_cost"),
    "unknown_metric_calls",
    "unknown_provider_calls",
    "unknown_input_tokens",
    "unknown_output_tokens",
    "unknown_total_tokens",
    "unknown_cost_calls"
  )
  expected_names <- c(
    "attempts",
    "successes",
    "total_errors",
    "consecutive_errors",
    optimizer_budget_counter_names(),
    "unknown_metric_calls",
    "unknown_provider_calls",
    "unknown_input_tokens",
    "unknown_output_tokens",
    "unknown_total_tokens",
    "unknown_cost_calls",
    "elapsed_seconds",
    "trial_units",
    "completed_units",
    "overshoots",
    "stop_reason"
  )
  if (
    !artifact_is_plain_list(state) ||
      !identical(names(state), expected_names)
  ) {
    optimizer_budget_checkpoint_abort(
      "Optimizer budget checkpoint has unknown, missing, or reordered fields"
    )
  }
  for (name in count_names) {
    value <- state[[name]]
    if (
      !is.integer(value) ||
        length(value) != 1L ||
        is.na(value) ||
        value < 0L
    ) {
      optimizer_budget_checkpoint_abort(
        paste0("Optimizer budget checkpoint count ", name, " is invalid")
      )
    }
  }
  for (name in c("known_cost", "elapsed_seconds")) {
    value <- state[[name]]
    if (
      !is.double(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 0
    ) {
      optimizer_budget_checkpoint_abort(
        paste0("Optimizer budget checkpoint amount ", name, " is invalid")
      )
    }
  }
  if (
    !optimizer_budget_has_exact_outcome_partition(
      state$attempts,
      state$successes,
      state$total_errors
    ) ||
      state$consecutive_errors > state$total_errors
  ) {
    optimizer_budget_checkpoint_abort(
      "Optimizer budget outcome counters are inconsistent"
    )
  }
  valid_unit_ids <- function(value) {
    is.character(value) &&
      !anyNA(value) &&
      !any(!nzchar(value)) &&
      !anyDuplicated(value)
  }
  if (!valid_unit_ids(state$trial_units)) {
    optimizer_budget_checkpoint_abort(
      "Optimizer checkpoint trial unit IDs are invalid"
    )
  }
  if (!valid_unit_ids(state$completed_units)) {
    optimizer_budget_checkpoint_abort(
      "Optimizer checkpoint completed unit IDs are invalid"
    )
  }
  if (
    !artifact_is_plain_list(state$overshoots) ||
      !is.null(names(state$overshoots)) ||
      !all(vapply(
        state$overshoots,
        optimizer_budget_validate_overshoot,
        logical(1)
      ))
  ) {
    optimizer_budget_checkpoint_abort(
      "Optimizer checkpoint overshoot records are invalid"
    )
  }
  if (!optimizer_budget_validate_stop_reason(state$stop_reason)) {
    optimizer_budget_checkpoint_abort(
      "Optimizer checkpoint stop reason is invalid"
    )
  }
  if (!is.null(state$stop_reason) && state$stop_reason$code == "max_errors") {
    reason <- state$stop_reason
    if (
      reason$attempts > state$attempts ||
        reason$total_errors > state$total_errors ||
        state$total_errors - reason$total_errors >
          state$attempts - reason$attempts
    ) {
      optimizer_budget_checkpoint_abort(
        "Optimizer checkpoint stop reason counters are inconsistent"
      )
    }
  }
  for (name in c(count_names, "known_cost")) {
    budget[[name]] <- state[[name]]
  }
  budget$trial_units <- state$trial_units
  budget$completed_units <- state$completed_units
  budget$overshoots <- state$overshoots
  budget$elapsed_offset <- state$elapsed_seconds
  budget$last_elapsed <- state$elapsed_seconds
  budget$started_tick <- as.numeric(budget$clock())
  budget$stop_reason <- if (is.null(state$stop_reason)) {
    NULL
  } else {
    structure(
      state$stop_reason,
      class = c("dsprrr_optimizer_stop_reason", "list")
    )
  }
  invisible(budget)
}

# Reconcile restored counters against the *current* control before any new
# work can start. This permits raised/removed caps while failing closed for
# stricter caps and for unknown restored usage under a newly finite cap.
optimizer_budget_reconcile_current_limits <- function(
  budget,
  stage = "checkpoint_resume"
) {
  restored_stop <- budget$stop_reason
  budget$stop_reason <- NULL

  # A stop reached inside a completed batch remains sticky even when a later,
  # already-started success reset the final streak. Checkpoint state does not
  # retain the order of outcomes after the stop, so use the largest peak still
  # possible from the recorded counters and fail closed when a raised cap could
  # already have been reached.
  post_stop_errors <- if (
    !is.null(restored_stop) &&
      identical(restored_stop$code, "max_errors")
  ) {
    max(0, budget$total_errors - restored_stop$total_errors)
  } else {
    0
  }
  possible_error_peak <- if (
    !is.null(restored_stop) &&
      identical(restored_stop$code, "max_errors")
  ) {
    as.numeric(restored_stop$observed) + as.numeric(post_stop_errors)
  } else {
    0
  }
  if (
    !is.null(restored_stop) &&
      identical(restored_stop$code, "max_errors") &&
      budget$max_errors <= possible_error_peak
  ) {
    budget$stop_reason <- restored_stop
    return(invisible(budget))
  }

  if (budget$consecutive_errors >= max(1L, budget$max_errors)) {
    optimizer_budget_set_stop(
      budget,
      new_optimizer_stop_reason(budget, stage)
    )
    return(invisible(budget))
  }

  unknown_limits <- list(
    max_metric_calls = list(
      counter = "unknown_metric_calls",
      code = "unknown_metric",
      resource = "metric_calls"
    ),
    max_provider_calls = list(
      counter = "unknown_provider_calls",
      code = "unknown_provider",
      resource = "provider_calls"
    ),
    max_input_tokens = list(
      counter = "unknown_input_tokens",
      code = "unknown_input_tokens",
      resource = "input_tokens"
    ),
    max_output_tokens = list(
      counter = "unknown_output_tokens",
      code = "unknown_output_tokens",
      resource = "output_tokens"
    ),
    max_total_tokens = list(
      counter = "unknown_total_tokens",
      code = "unknown_total_tokens",
      resource = "total_tokens"
    ),
    max_cost = list(
      counter = "unknown_cost_calls",
      code = "unknown_cost",
      resource = "cost_calls"
    )
  )
  for (limit_name in names(unknown_limits)) {
    entry <- unknown_limits[[limit_name]]
    limit <- budget$limits[[limit_name]]
    if (!is.null(limit) && budget[[entry$counter]] > 0L) {
      optimizer_budget_set_stop(
        budget,
        optimizer_budget_reason(
          code = entry$code,
          stage = stage,
          resource = entry$resource,
          limit = limit,
          observed = NA_real_,
          overshoot = NA_real_,
          work_unit = "checkpoint_history",
          max_started = 0L,
          message = sprintf(
            "Cannot enforce %s because restored work reported unknown usage",
            limit_name
          )
        )
      )
      return(invisible(budget))
    }
  }

  elapsed <- optimizer_budget_elapsed(budget)
  limit_map <- optimizer_budget_limit_map()
  for (limit_name in names(limit_map)) {
    limit <- budget$limits[[limit_name]]
    if (is.null(limit)) {
      next
    }
    resource <- limit_map[[limit_name]]
    observed <- if (identical(resource, "elapsed_seconds")) {
      elapsed
    } else {
      budget[[resource]]
    }
    if (observed >= limit) {
      optimizer_budget_set_stop(
        budget,
        optimizer_budget_limit_reason(
          budget,
          limit_name,
          stage,
          observed = observed,
          unit_id = NULL,
          overshoot = max(0, observed - limit),
          work_unit = "checkpoint_history",
          max_started = 0L
        )
      )
      return(invisible(budget))
    }
  }

  invisible(budget)
}

new_optimizer_stop_reason <- function(budget, stage, condition = NULL) {
  limit <- budget$max_errors
  message <- if (limit == 0L) {
    "Stopped after the first error because max_errors is 0"
  } else {
    sprintf(
      "Reached max_errors limit (%d consecutive errors)",
      limit
    )
  }

  reason <- list(
    code = "max_errors",
    stage = as.character(stage)[1L],
    limit = limit,
    observed = budget$consecutive_errors,
    total_errors = budget$total_errors,
    attempts = budget$attempts,
    message = message
  )

  if (inherits(condition, "condition")) {
    reason$condition_class <- class(condition)[1L]
  }

  structure(
    reason,
    class = c("dsprrr_optimizer_stop_reason", "list")
  )
}

# Record one ordered optimizer outcome and update the sticky stop reason.
record_optimizer_outcome <- function(
  budget,
  success,
  stage,
  condition = NULL
) {
  if (!inherits(budget, "dsprrr_optimizer_budget")) {
    cli::cli_abort(
      "{.arg budget} must be an optimizer budget",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  if (!is.logical(success) || length(success) != 1L || is.na(success)) {
    cli::cli_abort(
      "{.arg success} must be a single non-missing logical value",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  if (
    !is.character(stage) ||
      length(stage) != 1L ||
      is.na(stage) ||
      !nzchar(stage)
  ) {
    cli::cli_abort(
      "{.arg stage} must be a single non-empty string",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  # A completed batch can cross the boundary before all returned rows are
  # recorded. Keep accounting those executed outcomes while preserving the
  # first stop reason; callers use that sticky reason to prevent new work.
  next_attempts <- optimizer_budget_checked_increment(
    budget$attempts,
    1L,
    "attempts",
    stage
  )

  if (isTRUE(success)) {
    next_successes <- optimizer_budget_checked_increment(
      budget$successes,
      1L,
      "successes",
      stage
    )
    budget$attempts <- next_attempts
    budget$successes <- next_successes
    budget$consecutive_errors <- 0L
    return(invisible(budget))
  }

  next_total_errors <- optimizer_budget_checked_increment(
    budget$total_errors,
    1L,
    "total_errors",
    stage
  )
  next_consecutive_errors <- optimizer_budget_checked_increment(
    budget$consecutive_errors,
    1L,
    "consecutive_errors",
    stage
  )
  budget$attempts <- next_attempts
  budget$total_errors <- next_total_errors
  budget$consecutive_errors <- next_consecutive_errors

  effective_limit <- max(1L, budget$max_errors)
  if (
    is.null(budget$stop_reason) &&
      budget$consecutive_errors >= effective_limit
  ) {
    budget$stop_reason <- new_optimizer_stop_reason(
      budget,
      stage = stage,
      condition = condition
    )
  }

  invisible(budget)
}

optimizer_error_present <- function(error) {
  if (is.null(error) || length(error) == 0L) {
    return(FALSE)
  }

  if (inherits(error, "condition")) {
    return(TRUE)
  }

  if (length(error) == 1L && is.atomic(error) && is.na(error)) {
    return(FALSE)
  }

  if (is.character(error)) {
    return(any(nzchar(trimws(error[!is.na(error)]))))
  }

  TRUE
}

optimizer_error_condition <- function(error) {
  if (inherits(error, "condition")) {
    return(error)
  }

  simpleError(paste(as.character(error), collapse = ", "))
}

# Record the ordered row outcomes represented by an EvalResult.
record_eval_result_outcomes <- function(budget, eval_result, stage) {
  if (!inherits(eval_result, "dsprrr::EvalResult")) {
    cli::cli_abort(
      "{.arg eval_result} must be an EvalResult",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  examples <- eval_result@examples
  reported_successes <- as.integer(eval_result@n_evaluated)
  if (length(reported_successes) != 1L || is.na(reported_successes)) {
    reported_successes <- 0L
  }
  reported_successes <- max(0L, reported_successes)

  reported_errors <- as.integer(eval_result@n_errors)
  if (length(reported_errors) != 1L || is.na(reported_errors)) {
    reported_errors <- 0L
  }
  reported_errors <- max(0L, reported_errors)

  has_ordered_errors <- is.data.frame(examples) &&
    nrow(examples) > 0L &&
    "error" %in% names(examples)

  if (has_ordered_errors) {
    row_errors <- examples$error
    error_flags <- vapply(row_errors, optimizer_error_present, logical(1))
    if ("score" %in% names(examples)) {
      error_flags <- error_flags | is.na(examples$score)
    }

    # Reconcile summaries inside the known row set. Never synthesize extra
    # attempts: if detail omitted some failures, promote trailing clean rows.
    missing_failures <- max(0L, reported_errors - sum(error_flags))
    if (missing_failures > 0L) {
      clean_rows <- which(!error_flags)
      promote_count <- min(missing_failures, length(clean_rows))
      if (promote_count > 0L) {
        promoted <- utils::tail(clean_rows, promote_count)
        error_flags[promoted] <- TRUE
      }
    }

    optimizer_budget_require_outcome_capacity(
      budget,
      successes = sum(!error_flags),
      total_errors = sum(error_flags),
      stage = stage
    )

    for (index in seq_along(error_flags)) {
      is_error <- error_flags[[index]]
      condition <- if (is_error) {
        if (optimizer_error_present(row_errors[[index]])) {
          optimizer_error_condition(row_errors[[index]])
        } else {
          simpleError("Evaluation row failed without error detail")
        }
      } else {
        NULL
      }
      record_optimizer_outcome(
        budget,
        success = !is_error,
        stage = stage,
        condition = condition
      )
    }

    return(invisible(budget))
  }

  if (reported_successes == 0L && reported_errors == 0L) {
    optimizer_budget_require_outcome_capacity(
      budget,
      successes = 1L,
      total_errors = 0L,
      stage = stage
    )
    record_optimizer_outcome(budget, TRUE, stage)
    return(invisible(budget))
  }

  optimizer_budget_require_outcome_capacity(
    budget,
    successes = reported_successes,
    total_errors = reported_errors,
    stage = stage
  )

  # Without row detail, the actual ordering is unknowable. Record successes
  # first and group failures at the end so the reported failures form the
  # conservative terminal streak.
  if (reported_successes > 0L) {
    for (index in seq_len(reported_successes)) {
      record_optimizer_outcome(budget, TRUE, stage)
    }
  }

  for (index in seq_len(reported_errors)) {
    record_optimizer_outcome(budget, FALSE, stage)
  }

  invisible(budget)
}

optimizer_budget_stopped <- function(budget) {
  !is.null(budget$stop_reason)
}

optimizer_budget_summary <- function(budget) {
  elapsed <- optimizer_budget_elapsed(budget)
  list(
    attempts = budget$attempts,
    successes = budget$successes,
    total_errors = budget$total_errors,
    consecutive_errors = budget$consecutive_errors,
    max_errors = budget$max_errors,
    trials = budget$trials,
    metric_calls = if (budget$unknown_metric_calls > 0L) {
      NA_integer_
    } else {
      as.integer(budget$metric_calls)
    },
    known_metric_calls = as.integer(budget$metric_calls),
    provider_calls = if (budget$unknown_provider_calls > 0L) {
      NA_integer_
    } else {
      as.integer(budget$provider_calls)
    },
    known_provider_calls = as.integer(budget$provider_calls),
    input_tokens = if (budget$unknown_input_tokens > 0L) {
      NA_integer_
    } else {
      as.integer(budget$input_tokens)
    },
    known_input_tokens = as.integer(budget$input_tokens),
    output_tokens = if (budget$unknown_output_tokens > 0L) {
      NA_integer_
    } else {
      as.integer(budget$output_tokens)
    },
    known_output_tokens = as.integer(budget$output_tokens),
    total_tokens = if (budget$unknown_total_tokens > 0L) {
      NA_integer_
    } else {
      as.integer(budget$total_tokens)
    },
    known_total_tokens = as.integer(budget$total_tokens),
    total_cost = if (budget$unknown_cost_calls > 0L) {
      NA_real_
    } else {
      budget$known_cost
    },
    known_cost = budget$known_cost,
    unknown_usage = list(
      metric_calls = budget$unknown_metric_calls,
      provider_calls = budget$unknown_provider_calls,
      input_tokens = budget$unknown_input_tokens,
      output_tokens = budget$unknown_output_tokens,
      total_tokens = budget$unknown_total_tokens,
      cost = budget$unknown_cost_calls
    ),
    elapsed_seconds = elapsed,
    limits = budget$limits,
    completed_units = budget$completed_units,
    overshoots = budget$overshoots,
    stopped = optimizer_budget_stopped(budget),
    stop_reason = budget$stop_reason
  )
}
