#' Control Batch Concurrency
#'
#' @description
#' Create a validated execution policy for batch calls made by [run()] and
#' [run_dataset()]. The policy controls the backend, the exact maximum number
#' of active requests, error budgets, timeouts, and cancellation behavior.
#'
#' `backend = "auto"` is the only mode that may select a different backend.
#' An explicitly requested backend either runs with the requested contract or
#' fails before provider work begins.
#'
#' @param backend Execution backend. `"auto"` uses sequential execution when
#'   `max_active = 1`, otherwise preferring ellmer when the configured Chat and
#'   requested limits are compatible, then mirai, then sequential execution.
#'   Explicit `"ellmer"`, `"mirai"`, and `"sequential"` requests never fall
#'   back.
#' @param max_active Positive integer giving the exact maximum number of active
#'   requests. It maps to ellmer's `max_active` argument and the size of a
#'   dsprrr-owned mirai pool.
#' @param task_timeout Per-task timeout in seconds, or `Inf` for no timeout.
#'   Finite task timeouts currently require the mirai backend.
#' @param total_timeout Total batch timeout in seconds, or `Inf` for no timeout.
#'   Finite total timeouts currently require the mirai backend.
#' @param max_errors Non-negative integer error budget, or `Inf`. Zero permits
#'   work to begin and stops new work after the first failure. With ellmer, an
#'   already-started wave of at most `max_active` rows completes before the
#'   budget is observed.
#' @param cancel Logical. When `TRUE`, active mirai tasks are stopped when an
#'   error or timeout limit is reached. When `FALSE`, no new work is scheduled,
#'   but already-started tasks are drained before return. A total timeout always
#'   stops active work so no task continues after [run()] returns.
#'
#' @return A `dsprrr_concurrency_control` object for `.concurrency` in [run()],
#'   [run_dataset()], or [evaluate()].
#'
#' @details Native ellmer and mirai batch execution currently bypass dsprrr's
#'   response cache; structured metadata reports this as `cache = "bypass"`.
#'   Choose `backend = "sequential"` when response-cache reuse is required.
#' @export
#'
#' @examples
#' control <- concurrency_control(
#'   backend = "mirai",
#'   max_active = 2L,
#'   task_timeout = 30,
#'   total_timeout = 120,
#'   max_errors = 1L
#' )
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

#' Validate one legacy parallel flag
#' @noRd
validate_parallel_flag <- function(value) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    cli::cli_abort(
      "{.arg .parallel} must be one non-missing logical value",
      class = "dsprrr_concurrency_config_error"
    )
  }
  value
}

#' Resolve explicit concurrency and legacy parallel arguments
#' @noRd
resolve_concurrency_control <- function(
  .concurrency,
  concurrency_missing,
  .parallel,
  parallel_missing,
  .parallel_method,
  parallel_method_missing
) {
  explicit_control <- !concurrency_missing && !is.null(.concurrency)
  if (explicit_control && (!parallel_missing || !parallel_method_missing)) {
    cli::cli_abort(
      c(
        "{.arg .concurrency} cannot be combined with legacy parallel arguments",
        "i" = "Configure backend and workers with {.fn concurrency_control} only."
      ),
      class = "dsprrr_concurrency_argument_conflict"
    )
  }

  if (explicit_control) {
    control <- validate_concurrency_control(.concurrency)
    attr(control, "legacy") <- FALSE
    return(control)
  }

  .parallel <- validate_parallel_flag(.parallel)
  .parallel_method <- match.arg(.parallel_method, c("ellmer", "mirai"))
  if (!.parallel) {
    control <- concurrency_control(backend = "sequential", max_active = 1L)
  } else {
    max_active <- getOption("dsprrr.max_active", 10L)
    total_timeout <- if (identical(.parallel_method, "mirai")) {
      getOption("dsprrr.parallel_timeout", 600)
    } else {
      Inf
    }
    control <- concurrency_control(
      backend = .parallel_method,
      max_active = max_active,
      total_timeout = total_timeout
    )
  }
  attr(control, "legacy") <- TRUE
  control
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
  legacy <- isTRUE(attr(control, "legacy"))
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
    if (legacy && identical(effective_backend, "ellmer")) {
      effective_backend <- if (
        is.null(.llm) && concurrency_backend_available("mirai")
      ) {
        "mirai"
      } else {
        "sequential"
      }
      fallback_reason <- "ellmer parallel execution is unavailable"
      cli::cli_warn(c(
        "ellmer parallel execution is unavailable",
        "i" = "Falling back to {effective_backend} processing."
      ))
    } else {
      cli::cli_abort(
        "Requested concurrency backend {.val {requested_backend}} is unavailable",
        class = "dsprrr_concurrency_backend_unavailable"
      )
    }
  }

  if (identical(effective_backend, "mirai") && !is.null(.llm)) {
    if (legacy) {
      effective_backend <- "sequential"
      # The legacy timeout option governed mirai collection only. Once this
      # compatibility path falls back, it must not be misrepresented as an
      # enforceable sequential deadline.
      control$task_timeout <- Inf
      control$total_timeout <- Inf
      fallback_reason <- paste(
        "mirai requires .llm = NULL so each worker owns an isolated Chat"
      )
      cli::cli_warn(c(
        "mirai parallel execution requires {.code .llm = NULL} so each worker can create an independent client",
        "i" = "Falling back to sequential processing",
        "i" = "To enable parallel: remove {.arg .llm}, set {.code .llm = NULL}, or use {.code .parallel_method = \"ellmer\"}"
      ))
    } else {
      cli::cli_abort(
        c(
          "The mirai backend requires {.code .llm = NULL}",
          "i" = "Attach a serializable default Chat to the module or choose the ellmer backend."
        ),
        class = "dsprrr_concurrency_chat_error"
      )
    }
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
        fallback_reason = fallback_reason,
        legacy = legacy
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
  strict = TRUE
) {
  for (task in Filter(Negate(is.null), tasks)) {
    if (isTRUE(tryCatch(mirai::unresolved(task), error = function(e) FALSE))) {
      try(mirai::stop_mirai(task), silent = TRUE)
    }
  }

  last_error <- NULL
  for (attempt in seq_len(2L)) {
    last_error <- tryCatch(
      {
        mirai::daemons(0L, sync = TRUE, .compute = profile)
        NULL
      },
      error = function(error) error
    )
    if (is.null(last_error) && mirai_profile_is_drained(profile)) {
      return(TRUE)
    }
    Sys.sleep(0.02)
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
      class = "dsprrr_mirai_teardown_error"
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
