#' Control how batches run in parallel
#'
#' @description
#' `concurrency_control()` creates a policy for the `.concurrency` argument of
#' [run()], [run_dataset()] and [evaluate()]: which backend runs the rows,
#' how many requests may be active at once, and what happens on errors and
#' timeouts. Without a policy, rows run one after another.
#'
#' @param backend Where rows run. `"sequential"` runs them one at a time.
#'   `"ellmer"` sends them with ellmer's parallel requests. `"mirai"` runs them
#'   in a pool of background R processes. `"auto"` (the default) is sequential
#'   when `max_active = 1`; otherwise it uses ellmer when the chat and the
#'   limits allow, then mirai, then sequential. Only `"auto"` ever picks a
#'   different backend; an explicit backend that cannot honour the policy is
#'   an error before any request is made.
#' @param max_active The maximum number of requests active at once: ellmer's
#'   `max_active` or the size of the mirai pool.
#' @param task_timeout Seconds allowed per row, or `Inf`. A finite value
#'   needs the mirai backend.
#' @param total_timeout Seconds allowed for the whole batch, or `Inf`. A
#'   finite value needs the mirai backend. At the deadline, active work is
#'   stopped, which can add a short cleanup delay.
#' @param max_errors How many failed rows to tolerate before no new rows are
#'   started, or `Inf`. With `0`, work stops after the first failure. With
#'   ellmer, a wave of up to `max_active` rows that has already started
#'   finishes first.
#' @param cancel If `TRUE` (the default), active mirai tasks are stopped when
#'   the error budget or a timeout is reached. If `FALSE`, no new rows start,
#'   but rows already running finish. A total timeout always stops active
#'   work.
#'
#' @details
#' The ellmer and mirai backends do not use dsprrr's response cache (the
#' metadata reports `cache = "bypass"`); use the sequential backend when
#' cached responses matter. Parallel backends are available for prediction
#' modules from [module()] and [chain_of_thought()] and for some
#' code-running modules; for other modules, a parallel backend is an error.
#'
#' @return A policy object of class `dsprrr_concurrency_control`.
#' @export
#' @family execution
#'
#' @examples
#' control <- concurrency_control(
#'   backend = "mirai",
#'   max_active = 2L,
#'   task_timeout = 30,
#'   total_timeout = 120,
#'   max_errors = 1L
#' )
#' control
#'
#' \dontrun{
#' classify <- module(signature("text -> sentiment"))
#' reviews <- data.frame(text = c("Great!", "Broken on arrival", "Fine"))
#' run_dataset(
#'   classify,
#'   reviews,
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   .concurrency = concurrency_control(max_active = 3L)
#' )
#' }
concurrency_control <- function(
  backend = c("auto", "sequential", "ellmer", "mirai"),
  max_active = 1L,
  task_timeout = Inf,
  total_timeout = Inf,
  max_errors = Inf,
  cancel = TRUE
) {
  backend <- match.arg(backend)
  max_active <- validate_concurrency_integer(
    max_active,
    "max_active",
    minimum = 1L
  )
  task_timeout <- validate_concurrency_timeout(task_timeout, "task_timeout")
  total_timeout <- validate_concurrency_timeout(total_timeout, "total_timeout")
  max_errors <- validate_concurrency_error_budget(max_errors)

  if (!is.logical(cancel) || length(cancel) != 1L || is.na(cancel)) {
    cli::cli_abort(
      "{.arg cancel} must be one non-missing logical value",
      class = "dsprrr_concurrency_config_error"
    )
  }

  structure(
    list(
      backend = backend,
      max_active = max_active,
      task_timeout = task_timeout,
      total_timeout = total_timeout,
      max_errors = max_errors,
      cancel = cancel
    ),
    class = "dsprrr_concurrency_control"
  )
}

#' @export
print.dsprrr_concurrency_control <- function(x, ...) {
  cli::cli_text("<dsprrr_concurrency_control>")
  cli::cli_dl(c(
    "Backend" = x$backend,
    "Maximum active" = x$max_active,
    "Task timeout" = format_concurrency_limit(x$task_timeout, " seconds"),
    "Total timeout" = format_concurrency_limit(x$total_timeout, " seconds"),
    "Maximum errors" = format_concurrency_limit(x$max_errors),
    "Cancel active work" = x$cancel
  ))
  invisible(x)
}

#' Format a finite or unlimited concurrency limit
#' @noRd
format_concurrency_limit <- function(value, suffix = "") {
  if (is.infinite(value)) {
    "unlimited"
  } else {
    paste0(format(value, trim = TRUE), suffix)
  }
}

#' Validate a whole-number concurrency field
#' @noRd
validate_concurrency_integer <- function(value, name, minimum = 0L) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value == floor(value) &&
    value >= minimum &&
    value <= .Machine$integer.max
  if (!valid) {
    cli::cli_abort(
      "{.arg {name}} must be one whole number greater than or equal to {minimum}",
      class = "dsprrr_concurrency_config_error"
    )
  }
  as.integer(value)
}

#' Validate a timeout field
#' @noRd
validate_concurrency_timeout <- function(value, name) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    (identical(as.numeric(value), Inf) || (is.finite(value) && value > 0))
  if (!valid) {
    cli::cli_abort(
      "{.arg {name}} must be one positive number or {.code Inf}",
      class = "dsprrr_concurrency_config_error"
    )
  }
  as.numeric(value)
}

#' Validate the row error budget
#' @noRd
validate_concurrency_error_budget <- function(value) {
  if (
    is.numeric(value) &&
      length(value) == 1L &&
      identical(as.numeric(value), Inf)
  ) {
    return(Inf)
  }
  validate_concurrency_integer(value, "max_errors", minimum = 0L)
}

#' Validate a concurrency control supplied by a caller
#' @noRd
validate_concurrency_control <- function(control) {
  if (!inherits(control, "dsprrr_concurrency_control")) {
    cli::cli_abort(
      c(
        "{.arg .concurrency} must be created by {.fn concurrency_control}",
        "x" = "Got {.cls {class(control)[1]}}."
      ),
      class = "dsprrr_concurrency_config_error"
    )
  }

  # Reconstructing catches mutated list fields as well as malformed subclasses.
  concurrency_control(
    backend = control$backend,
    max_active = control$max_active,
    task_timeout = control$task_timeout,
    total_timeout = control$total_timeout,
    max_errors = control$max_errors,
    cancel = control$cancel
  )
}

#' Resolve a concurrency policy, defaulting to sequential execution
#' @noRd
resolve_concurrency_control <- function(.concurrency = NULL) {
  if (is.null(.concurrency)) {
    return(concurrency_control(backend = "sequential", max_active = 1L))
  }
  validate_concurrency_control(.concurrency)
}

#' Report whether a batch backend is installed and callable
#' @noRd
concurrency_backend_available <- function(backend) {
  switch(
    backend,
    sequential = TRUE,
    ellmer = exists(
      "parallel_chat_structured",
      envir = asNamespace("ellmer"),
      inherits = FALSE
    ),
    mirai = exists("mirai", envir = asNamespace("mirai"), inherits = FALSE),
    FALSE
  )
}

#' Normalize a validated control into an executable batch contract
#' @noRd
normalize_concurrency_runtime <- function(control, .llm = NULL, .chat = .llm) {
  control <- validate_concurrency_control(control)
  requested_backend <- control$backend
  effective_backend <- requested_backend
  fallback_reason <- NA_character_
  finite_timeout <- is.finite(control$task_timeout) ||
    is.finite(control$total_timeout)
  ellmer_chat_compatible <- is.null(.chat) ||
    cache_is_trusted_ellmer_chat(.chat)

  if (identical(requested_backend, "auto")) {
    if (finite_timeout) {
      if (is.null(.llm) && concurrency_backend_available("mirai")) {
        effective_backend <- "mirai"
        fallback_reason <- paste(
          "mirai selected because finite timeouts cannot be enforced by ellmer"
        )
      } else {
        cli::cli_abort(
          c(
            "No available backend can enforce the requested timeouts",
            "i" = "Finite timeouts require {.code backend = \"mirai\"} and {.code .llm = NULL}."
          ),
          class = "dsprrr_concurrency_unsupported_error"
        )
      }
    } else if (control$max_active == 1L) {
      effective_backend <- "sequential"
      fallback_reason <- "max_active = 1 requires no concurrent backend"
    } else if (
      ellmer_chat_compatible && concurrency_backend_available("ellmer")
    ) {
      effective_backend <- "ellmer"
    } else if (is.null(.llm) && concurrency_backend_available("mirai")) {
      effective_backend <- "mirai"
      fallback_reason <- if (ellmer_chat_compatible) {
        "ellmer parallel execution is unavailable"
      } else {
        "the configured Chat does not implement the trusted ellmer parallel contract"
      }
    } else {
      effective_backend <- "sequential"
      fallback_reason <- if (!ellmer_chat_compatible) {
        "the supplied Chat does not implement the trusted ellmer parallel contract"
      } else if (is.null(.llm)) {
        "ellmer and mirai parallel execution are unavailable"
      } else {
        "ellmer parallel execution is unavailable and the supplied Chat cannot be sent to mirai"
      }
    }
  }

  if (
    !identical(requested_backend, "auto") &&
      !concurrency_backend_available(effective_backend)
  ) {
    cli::cli_abort(
      "Requested concurrency backend {.val {requested_backend}} is unavailable",
      class = "dsprrr_concurrency_backend_unavailable"
    )
  }

  if (identical(effective_backend, "mirai") && !is.null(.llm)) {
    cli::cli_abort(
      c(
        "The mirai backend requires {.code .llm = NULL}",
        "i" = "Attach a serializable default Chat to the module or choose the ellmer backend."
      ),
      class = "dsprrr_concurrency_chat_error"
    )
  }

  finite_timeout <- is.finite(control$task_timeout) ||
    is.finite(control$total_timeout)
  if (effective_backend %in% c("ellmer", "sequential") && finite_timeout) {
    cli::cli_abort(
      c(
        "The {.val {effective_backend}} backend cannot enforce finite timeouts",
        "i" = "Use {.code backend = \"mirai\"} or set both timeouts to {.code Inf}."
      ),
      class = "dsprrr_concurrency_unsupported_error"
    )
  }

  effective_workers <- if (identical(effective_backend, "sequential")) {
    1L
  } else {
    control$max_active
  }

  structure(
    c(
      unclass(control),
      list(
        requested_backend = requested_backend,
        effective_backend = effective_backend,
        requested_workers = control$max_active,
        effective_workers = effective_workers,
        fallback_reason = fallback_reason
      )
    ),
    class = "dsprrr_concurrency_runtime"
  )
}

#' Stable per-row metadata for a concurrency runtime
#' @noRd
concurrency_metadata <- function(
  runtime = NULL,
  cancelled = FALSE,
  cancellation_reason = NA_character_
) {
  if (is.null(runtime)) {
    runtime <- list(
      requested_backend = "sequential",
      effective_backend = "sequential",
      requested_workers = 1L,
      effective_workers = 1L,
      task_timeout = Inf,
      total_timeout = Inf,
      max_errors = Inf,
      cancel = TRUE,
      fallback_reason = NA_character_
    )
  }
  values <- list(
    requested_backend = runtime$requested_backend,
    effective_backend = runtime$effective_backend,
    requested_workers = as.integer(runtime$requested_workers),
    effective_workers = as.integer(runtime$effective_workers),
    task_timeout = runtime$task_timeout,
    total_timeout = runtime$total_timeout,
    max_errors = runtime$max_errors,
    cancel_on_limit = runtime$cancel,
    cancelled = isTRUE(cancelled),
    cancellation_reason = cancellation_reason,
    fallback_reason = runtime$fallback_reason
  )
  c(values, list(concurrency = values))
}

#' Determine whether the total row-failure budget has been reached
#' @noRd
concurrency_error_budget_reached <- function(error_count, max_errors) {
  is.finite(max_errors) && error_count >= max(1, max_errors)
}

#' Report whether a mirai profile is safe for dsprrr to claim
#' @noRd
mirai_profile_is_unoccupied <- function(profile) {
  state <- tryCatch(
    mirai::status(.compute = profile),
    error = function(error) NULL
  )
  if (is.null(state)) {
    return(FALSE)
  }
  no_connections <- identical(as.integer(state$connections %||% 0L), 0L)
  daemons <- state$daemons
  no_daemons <- is.null(daemons) ||
    length(daemons) == 0L ||
    (is.numeric(daemons) && length(daemons) == 1L && daemons == 0)
  work <- state$mirai
  no_work <- is.null(work) ||
    sum(work[c("awaiting", "executing")], na.rm = TRUE) == 0L
  no_connections && no_daemons && no_work
}

#' Allocate a collision-resistant name for a dsprrr-owned mirai profile
#' @noRd
new_dsprrr_mirai_profile <- local({
  counter <- 0L
  function(max_attempts = 32L) {
    for (attempt in seq_len(max_attempts)) {
      counter <<- counter + 1L
      entropy <- paste(
        Sys.getpid(),
        counter,
        format(Sys.time(), digits = 22L),
        basename(tempfile()),
        sep = "-"
      )
      nonce <- substr(
        digest::digest(entropy, algo = "sha256", serialize = FALSE),
        1L,
        24L
      )
      profile <- paste0("dsprrr-", Sys.getpid(), "-", nonce)
      if (mirai_profile_is_unoccupied(profile)) {
        return(profile)
      }
    }
    cli::cli_abort(
      "Could not allocate an unoccupied dsprrr mirai profile",
      class = "dsprrr_mirai_profile_collision"
    )
  }
})

#' Read a monotonic elapsed clock for scheduler deadlines
#' @noRd
concurrency_elapsed <- function() {
  unname(proc.time()[["elapsed"]])
}

#' Whether a named mirai profile has no queued or executing work
#' @noRd
mirai_profile_is_drained <- function(profile) {
  state <- tryCatch(
    mirai::status(.compute = profile),
    error = function(error) NULL
  )
  if (is.null(state)) {
    return(FALSE)
  }
  no_connections <- identical(as.integer(state$connections %||% 0L), 0L)
  work <- state$mirai
  no_work <- is.null(work) ||
    sum(work[c("awaiting", "executing")], na.rm = TRUE) == 0L
  no_connections && no_work
}

#' Stop and verify a dsprrr-owned mirai profile
#' @noRd
shutdown_dsprrr_mirai_profile <- function(
  profile,
  tasks = list(),
  strict = TRUE,
  deadline = NULL,
  timeout = getOption("dsprrr.mirai_teardown_timeout", 5)
) {
  if (is.null(deadline)) {
    timeout <- tryCatch(
      suppressWarnings(as.numeric(timeout)),
      error = function(error) NA_real_
    )
    if (
      length(timeout) != 1L ||
        is.na(timeout) ||
        !is.finite(timeout) ||
        timeout <= 0
    ) {
      timeout <- 5
    }
    deadline <- concurrency_elapsed() + timeout
  } else {
    deadline <- tryCatch(
      suppressWarnings(as.numeric(deadline)),
      error = function(error) NA_real_
    )
    if (length(deadline) != 1L || is.na(deadline) || !is.finite(deadline)) {
      deadline <- concurrency_elapsed() + 5
    }
  }

  for (task in Filter(Negate(is.null), tasks)) {
    if (isTRUE(tryCatch(mirai::unresolved(task), error = function(e) FALSE))) {
      try(mirai::stop_mirai(task), silent = TRUE)
    }
  }

  # The native reset call itself cannot be safely pre-empted from R. Request a
  # non-synchronous reset, then bound every retry and verification after it
  # returns by the caller's absolute monotonic deadline.
  last_error <- tryCatch(
    {
      mirai::daemons(0L, sync = FALSE, .compute = profile)
      NULL
    },
    error = function(error) error
  )
  attempts <- 1L
  timed_out <- FALSE

  repeat {
    if (is.null(last_error) && mirai_profile_is_drained(profile)) {
      return(TRUE)
    }

    remaining <- deadline - concurrency_elapsed()
    if (remaining <= 0) {
      timed_out <- TRUE
      break
    }

    if (!is.null(last_error) && attempts < 2L) {
      Sys.sleep(min(0.02, remaining))
      attempts <- attempts + 1L
      last_error <- tryCatch(
        {
          mirai::daemons(0L, sync = FALSE, .compute = profile)
          NULL
        },
        error = function(error) error
      )
      next
    }

    if (!is.null(last_error)) {
      break
    }
    Sys.sleep(min(0.02, remaining))
  }

  if (strict) {
    detail <- if (is.null(last_error)) {
      "the profile still reports queued or executing work"
    } else {
      conditionMessage(last_error)
    }
    cli::cli_abort(
      c(
        "Could not fully stop the dsprrr-owned mirai worker pool",
        "x" = detail
      ),
      class = if (timed_out) {
        c(
          "dsprrr_mirai_teardown_timeout",
          "dsprrr_mirai_teardown_error"
        )
      } else {
        "dsprrr_mirai_teardown_error"
      },
      profile = profile,
      deadline = deadline
    )
  }
  FALSE
}

#' Emit a teardown warning without replacing an in-flight primary error
#' @noRd
warn_mirai_teardown_failure <- function(profile) {
  message <- paste0(
    "Could not verify cleanup of dsprrr-owned mirai profile '",
    profile,
    "' after an execution error"
  )
  warning <- structure(
    list(message = message, call = NULL, profile = profile),
    class = c(
      "dsprrr_mirai_teardown_warning",
      "warning",
      "condition"
    )
  )
  tryCatch(
    base::warning(warning),
    error = function(error) {
      # `options(warn = 2)` must not let cleanup diagnostics replace the
      # primary scheduler error. A message is the last-resort visible signal.
      base::message("Warning: ", message)
    }
  )
  invisible(warning)
}
