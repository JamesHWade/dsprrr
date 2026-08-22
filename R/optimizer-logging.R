# Optimizer Logging Infrastructure
#
# Trial logging and persistence for optimizers:
# - Trial records
# - Trial log collections
# - JSONL persistence
# - Log directory management

# Normalize persisted or in-memory trial costs to one scalar representation.
normalize_trial_cost <- function(cost) {
  if (is.null(cost)) {
    return(NA_real_)
  }
  if (!is.numeric(cost) || length(cost) != 1L) {
    cli::cli_abort(
      "Trial cost must be one numeric value or NULL",
      class = "dsprrr_trial_record_malformed"
    )
  }
  if (is.na(cost)) {
    return(NA_real_)
  }
  if (!is.finite(cost) || cost < 0) {
    cli::cli_abort(
      "Trial cost must be finite and non-negative",
      class = "dsprrr_trial_record_malformed"
    )
  }
  as.numeric(cost)
}

trial_record_schema_version <- function() 1L

trial_record_fields <- function() {
  c(
    "schema_version",
    "trial_id",
    "optimizer_name",
    "params",
    "metric_summary",
    "cost_summary",
    "start_time",
    "end_time",
    "notes",
    "status",
    "trace_context"
  )
}

validate_trial_record <- function(
  record,
  class = "dsprrr_trial_record_malformed"
) {
  record_names <- names(record)
  valid_shape <- is.list(record) &&
    !is.null(record_names) &&
    !anyNA(record_names) &&
    !anyDuplicated(record_names) &&
    setequal(record_names, trial_record_fields()) &&
    length(record_names) == length(trial_record_fields())
  valid_scalar <- function(value, nonempty = FALSE) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      (!nonempty || nzchar(value))
  }
  valid_time <- function(value) {
    is.null(value) || valid_scalar(value, nonempty = TRUE)
  }
  valid <- valid_shape &&
    identical(record$schema_version, trial_record_schema_version()) &&
    valid_scalar(record$trial_id, nonempty = TRUE) &&
    valid_scalar(record$optimizer_name, nonempty = TRUE) &&
    is.list(record$params) &&
    is.list(record$metric_summary) &&
    is.list(record$cost_summary) &&
    valid_time(record$start_time) &&
    valid_time(record$end_time) &&
    valid_scalar(record$notes) &&
    valid_scalar(record$status, nonempty = TRUE) &&
    record$status %in% c("pending", "running", "completed", "failed") &&
    is.list(record$trace_context)
  if (!isTRUE(valid)) {
    reason <- trial_record_schema_reason(record)
    cli::cli_abort(
      c(
        "Trial record does not match the current schema",
        "i" = "{reason}"
      ),
      class = class
    )
  }
  invisible(record)
}

#' Explain why a trial record failed schema validation
#'
#' Records written before schema versioning carry no `schema_version` at all,
#' which is by far the most common cause. Name that case explicitly so the
#' remedy (re-run the optimization, or keep the old dsprrr) is obvious.
#' @noRd
trial_record_schema_reason <- function(record) {
  current <- format(trial_record_schema_version())
  version <- if (is.list(record)) record$schema_version else NULL
  if (is.null(version)) {
    return(paste0(
      "It has no schema_version, so it was written by a dsprrr ",
      "version older than record schema ",
      current,
      ". Re-run the optimization to write a current log."
    ))
  }
  if (!is.atomic(version) || length(version) != 1L || is.na(version)) {
    return(paste0(
      "Its schema_version must be one non-missing scalar; this dsprrr reads ",
      "only ",
      current,
      "."
    ))
  }
  if (!identical(version, trial_record_schema_version())) {
    return(paste0(
      "It declares schema_version ",
      format(version)[[1]],
      "; this dsprrr reads only ",
      current,
      "."
    ))
  }
  "Its fields do not match the current record schema."
}

format_trial_cost <- function(cost) {
  if (length(cost) != 1L || is.na(cost)) {
    return("Unknown")
  }

  sprintf("$%.4f", cost)
}

format_trial_created_at <- function(value) {
  if (inherits(value, c("POSIXct", "POSIXlt"))) {
    return(format(value, "%Y-%m-%d %H:%M:%S"))
  }
  if (is.character(value) && length(value) == 1L && nzchar(value)) {
    return(value)
  }
  "Unknown"
}

normalize_trial_count <- function(value) {
  if (is.null(value) || length(value) != 1L) {
    return(NA_integer_)
  }

  value <- suppressWarnings(as.integer(value))
  if (length(value) != 1L || is.na(value)) {
    return(NA_integer_)
  }

  value
}

normalize_trial_flag <- function(value) {
  if (is.null(value) || length(value) != 1L) {
    return(NA)
  }

  if (is.logical(value)) {
    return(value)
  }
  if (is.character(value) && value %in% c("TRUE", "FALSE")) {
    return(identical(value, "TRUE"))
  }

  NA
}

sum_trial_counts <- function(values) {
  values <- as.integer(values)
  if (length(values) == 0L) {
    return(0L)
  }
  if (anyNA(values)) {
    return(NA_integer_)
  }
  as.integer(sum(values))
}

trial_json_record <- function(trial) {
  record <- list(
    schema_version = trial_record_schema_version(),
    trial_id = trial@trial_id,
    optimizer_name = trial@optimizer_name,
    params = trial@params,
    metric_summary = trial@metric_summary,
    cost_summary = trial@cost_summary,
    start_time = if (!is.null(trial@start_time)) {
      format(trial@start_time, "%Y-%m-%dT%H:%M:%S")
    } else {
      NULL
    },
    end_time = if (!is.null(trial@end_time)) {
      format(trial@end_time, "%Y-%m-%dT%H:%M:%S")
    } else {
      NULL
    },
    notes = trial@notes,
    status = trial@status,
    trace_context = trace_context_validate(trial@trace_context)
  )
  validate_trial_record(record)
  record
}

trial_json_line <- function(trial) {
  record <- trial_json_record(trial)
  context <- record$trace_context
  record$trace_context <- NULL
  line <- as.character(jsonlite::toJSON(
    record,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))

  context_json <- as.character(jsonlite::toJSON(
    context,
    auto_unbox = TRUE,
    null = "null",
    digits = 17
  ))
  paste0(
    substr(line, 1L, nchar(line) - 1L),
    ",\"trace_context\":",
    context_json,
    "}"
  )
}

trial_records_identical <- function(x, y) {
  identical(trial_json_line(x), trial_json_line(y))
}

trial_log_merge_unique <- function(existing, incoming, source = "trial log") {
  for (trial in incoming) {
    matches <- which(vapply(
      existing,
      function(candidate) identical(candidate@trial_id, trial@trial_id),
      logical(1)
    ))
    if (length(matches) == 0L) {
      existing <- append(existing, list(trial))
      next
    }

    matches_record <- vapply(
      existing[matches],
      trial_records_identical,
      logical(1),
      y = trial
    )
    if (!all(matches_record)) {
      cli::cli_abort(
        c(
          "Trial ID {.val {trial@trial_id}} has conflicting records",
          "i" = "Trial IDs are immutable within a persisted log.",
          "i" = "Conflict found in {source}."
        ),
        class = "dsprrr_trial_id_conflict"
      )
    }

    # A resumed in-memory trial may carry the compiled program that is omitted
    # from JSONL. Retain that runtime reference without adding another record.
    first <- matches[[1L]]
    if (
      is.null(existing[[first]]@compiled_artifact_ref) &&
        !is.null(trial@compiled_artifact_ref)
    ) {
      existing[[first]] <- trial
    }
  }

  existing
}

trial_log_trust_abort <- function(message, parent = NULL, remedy = NULL) {
  cli::cli_abort(
    c(
      "Trial log path trust verification failed",
      "x" = "{message}",
      if (!is.null(remedy)) c("i" = "{remedy}")
    ),
    parent = parent,
    class = c(
      "dsprrr_trial_log_trust_error",
      "dsprrr_trial_log_io_error"
    )
  )
}

#' Build a chmod remediation hint for a rejected log path
#'
#' dsprrr no longer widens or narrows stored permissions on the user's behalf,
#' so the abort has to say what to run instead.
#' @noRd
trial_log_chmod_remedy <- function(path, mode) {
  paste0(
    "Restrict it yourself, then retry: chmod ",
    mode,
    " ",
    shQuote(path)
  )
}

trial_log_absolute_path <- function(path) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    cli::cli_abort(
      "{.arg path} must be a single non-empty path",
      class = "dsprrr_trial_log_io_error"
    )
  }
  tryCatch(
    as.character(fs::path_abs(path.expand(path))),
    error = function(e) {
      trial_log_trust_abort(
        paste0("the path could not be resolved: ", conditionMessage(e)),
        parent = e
      )
    }
  )
}

trial_log_path_info <- function(path) {
  tryCatch(
    suppressWarnings(fs::file_info(path, follow = FALSE, fail = FALSE)),
    error = function(e) NULL
  )
}

trial_log_stable_device_inode <- function(info) {
  if (
    is.null(info) ||
      nrow(info) != 1L ||
      !all(c("device_id", "inode") %in% names(info))
  ) {
    return(NULL)
  }
  device_id <- tryCatch(
    suppressWarnings(as.numeric(info$device_id[[1L]])),
    error = function(e) NULL
  )
  inode <- tryCatch(
    suppressWarnings(as.numeric(info$inode[[1L]])),
    error = function(e) NULL
  )
  if (
    length(device_id) != 1L ||
      length(inode) != 1L ||
      !is.finite(device_id) ||
      !is.finite(inode)
  ) {
    return(NULL)
  }
  list(device_id = device_id, inode = inode)
}

trial_log_audit_parent_capability <- function(target) {
  if (!cache_private_modes_supported()) {
    return(list(ok = TRUE))
  }
  effective_owner <- cache_effective_owner_id()
  if (is.na(effective_owner)) {
    return(list(
      ok = FALSE,
      reason = "the effective user ID could not be established"
    ))
  }

  child <- target
  current <- dirname(target)
  writable_mask <- as.integer(as.octmode("0022"))
  sticky_mask <- as.integer(as.octmode("1000"))
  repeat {
    if (dir.exists(current)) {
      mode <- cache_path_permission_bits(current)
      if (is.na(mode)) {
        return(list(
          ok = FALSE,
          reason = paste0(
            "ancestor permissions could not be inspected: ",
            current
          )
        ))
      }
      owner <- cache_path_owner_id(current)
      if (is.na(owner) || !owner %in% c(0L, effective_owner)) {
        return(list(
          ok = FALSE,
          reason = paste0(
            "a trial log ancestor is not owned by the effective user or root: ",
            current
          )
        ))
      }
      if (bitwAnd(mode, writable_mask) != 0L) {
        if (bitwAnd(mode, sticky_mask) == 0L) {
          return(list(
            ok = FALSE,
            reason = paste0(
              "a non-sticky trial log ancestor is writable: ",
              current
            )
          ))
        }
        if (file.exists(child) || dir.exists(child)) {
          child_owner <- cache_path_owner_id(child)
          if (
            is.na(child_owner) ||
              !child_owner %in% c(0L, effective_owner)
          ) {
            return(list(
              ok = FALSE,
              reason = paste0(
                "a sticky writable trial log ancestor has a child not ",
                "owned by the effective user or root: ",
                child
              )
            ))
          }
        }
      }
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    child <- current
    current <- parent
  }
  list(ok = TRUE)
}

trial_log_directory_identity <- function(path) {
  if (cache_path_is_symlink(path) || !dir.exists(path)) {
    return(NULL)
  }
  canonical <- tryCatch(
    as.character(fs::path_real(path)),
    error = function(e) NULL
  )
  info <- trial_log_path_info(path)
  stable <- trial_log_stable_device_inode(info)
  if (
    is.null(canonical) ||
      is.null(info) ||
      nrow(info) != 1L ||
      is.null(stable) ||
      !identical(as.character(info$type[[1L]]), "directory")
  ) {
    return(NULL)
  }

  identity <- list(
    path = canonical,
    device_id = stable$device_id,
    inode = stable$inode
  )
  if (cache_private_modes_supported()) {
    owner <- cache_path_owner_id(path)
    mode <- cache_path_mode(path)
    if (is.na(owner) || is.na(mode)) {
      return(NULL)
    }
    identity$owner_id <- owner
    identity$mode <- mode
  } else {
    identity$windows_unverified_acl <- TRUE
  }
  identity
}

trial_log_directory_reason <- function(trust) {
  current <- trial_log_directory_identity(trust$path)
  if (is.null(current) || !identical(current, trust)) {
    return("the log directory identity changed after it was audited")
  }
  if (cache_private_modes_supported()) {
    parent_audit <- trial_log_audit_parent_capability(trust$path)
    if (!isTRUE(parent_audit$ok)) {
      return(parent_audit$reason)
    }
  }
  NULL
}

trial_log_audit_directory <- function(path, private) {
  if (cache_path_is_symlink(path)) {
    return(list(ok = FALSE, reason = "the log directory is a symbolic link"))
  }
  if (!dir.exists(path)) {
    return(list(ok = FALSE, reason = "the log path is not a directory"))
  }
  if (!cache_private_modes_supported()) {
    return(list(ok = TRUE))
  }

  effective_owner <- cache_effective_owner_id()
  owner <- cache_path_owner_id(path)
  if (is.na(effective_owner) || is.na(owner)) {
    return(list(
      ok = FALSE,
      reason = "the log directory owner could not be verified"
    ))
  }
  if (!identical(owner, effective_owner)) {
    return(list(
      ok = FALSE,
      reason = "the log directory is not owned by the effective user"
    ))
  }

  mode <- cache_path_mode(path)
  if (is.na(mode)) {
    return(list(
      ok = FALSE,
      reason = "the log directory permissions could not be inspected"
    ))
  }
  if (bitwAnd(mode, as.integer(as.octmode("0022"))) != 0L) {
    return(list(
      ok = FALSE,
      reason = "the log directory is writable by another local account"
    ))
  }
  if (
    isTRUE(private) &&
      !identical(mode, as.integer(as.octmode("0700")))
  ) {
    return(list(
      ok = FALSE,
      reason = paste0(
        "a pre-existing private log directory must have mode exactly 0700"
      ),
      remedy = trial_log_chmod_remedy(path, "700")
    ))
  }

  list(ok = TRUE)
}

trial_log_prepare_directory <- function(
  path,
  create = TRUE,
  expected_trust = NULL,
  private = TRUE
) {
  absolute <- trial_log_absolute_path(path)
  if (cache_path_is_symlink(absolute)) {
    trial_log_trust_abort("the log directory is a symbolic link")
  }
  if (file.exists(absolute) && !dir.exists(absolute)) {
    trial_log_trust_abort("the log path is not a directory")
  }

  canonical_target <- cache_canonical_target_path(absolute)
  if (is.null(canonical_target)) {
    trial_log_trust_abort("the log directory cannot be resolved canonically")
  }
  if (cache_private_modes_supported()) {
    capability <- trial_log_audit_parent_capability(canonical_target)
    if (!isTRUE(capability$ok)) {
      trial_log_trust_abort(capability$reason)
    }
  }

  directory_created <- FALSE
  if (!dir.exists(canonical_target)) {
    if (!isTRUE(create)) {
      trial_log_trust_abort("the log directory does not exist")
    }
    created <- tryCatch(
      dir.create(
        canonical_target,
        recursive = TRUE,
        showWarnings = FALSE,
        mode = "0700"
      ),
      error = function(e) e
    )
    if (!dir.exists(canonical_target)) {
      parent <- if (inherits(created, "condition")) created else NULL
      trial_log_trust_abort(
        paste0("the log directory could not be created: ", canonical_target),
        parent = parent
      )
    }
    if (!isTRUE(created)) {
      parent <- if (inherits(created, "condition")) created else NULL
      trial_log_trust_abort(
        "the log directory appeared while it was being created",
        parent = parent
      )
    }
    directory_created <- TRUE
  }
  if (cache_path_is_symlink(canonical_target)) {
    trial_log_trust_abort("the log directory became a symbolic link")
  }

  canonical <- tryCatch(
    as.character(fs::path_real(canonical_target)),
    error = function(e) NULL
  )
  if (is.null(canonical)) {
    trial_log_trust_abort("the created log directory is not canonical")
  }

  if (cache_private_modes_supported()) {
    if (
      directory_created &&
        !cache_set_private_mode(canonical, "0700")
    ) {
      trial_log_trust_abort(
        "the created log directory could not be restricted to mode 0700"
      )
    }
    directory_audit <- trial_log_audit_directory(canonical, private)
    if (!isTRUE(directory_audit$ok)) {
      trial_log_trust_abort(
        directory_audit$reason,
        remedy = directory_audit$remedy
      )
    }
    parent_audit <- trial_log_audit_parent_capability(canonical)
    if (!isTRUE(parent_audit$ok)) {
      trial_log_trust_abort(parent_audit$reason)
    }
  }

  trust <- trial_log_directory_identity(canonical)
  if (is.null(trust)) {
    trial_log_trust_abort("the log directory identity is unavailable")
  }
  if (!is.null(expected_trust) && !identical(trust, expected_trust)) {
    trial_log_trust_abort(
      "the log directory was replaced after the TrialLog was created"
    )
  }
  list(path = canonical, trust = trust)
}

trial_log_assert_parent <- function(guard) {
  reason <- trial_log_directory_reason(guard$trust)
  if (!is.null(reason)) {
    trial_log_trust_abort(reason)
  }
  invisible(TRUE)
}

trial_log_child_path <- function(path, guard) {
  trial_log_assert_parent(guard)
  absolute <- trial_log_absolute_path(path)
  parent <- tryCatch(
    as.character(fs::path_real(dirname(absolute))),
    error = function(e) NULL
  )
  if (is.null(parent) || !identical(parent, guard$path)) {
    trial_log_trust_abort("a trial log file escaped its trusted directory")
  }
  file.path(guard$path, basename(absolute))
}

trial_log_abort_corrupt <- function(
  path,
  message,
  torn = FALSE,
  parent = NULL
) {
  classes <- c(
    if (torn) "dsprrr_trial_log_torn_write",
    "dsprrr_trial_log_corrupt"
  )
  cli::cli_abort(
    c(
      message,
      "x" = "Journal: {.path {path}}",
      "i" = "The journal was not modified. Repair or restore it before retrying."
    ),
    parent = parent,
    class = classes
  )
}

trial_log_assert_regular_file <- function(path, what, guard) {
  path <- trial_log_child_path(path, guard)
  info <- trial_log_path_info(path)
  type <- if (!is.null(info) && nrow(info) == 1L) {
    as.character(info$type[[1L]])
  } else {
    NA_character_
  }
  if (!identical(type, "file")) {
    cli::cli_abort(
      c(
        "Invalid {what} path",
        "x" = "Expected a regular file at {.path {path}}."
      ),
      class = c(
        "dsprrr_trial_log_trust_error",
        "dsprrr_trial_log_io_error"
      )
    )
  }
  invisible(path)
}

trial_log_file_identity <- function(path, guard) {
  path <- trial_log_child_path(path, guard)
  if (!file.exists(path)) {
    return(list(ok = FALSE, missing = TRUE, reason = "the file is missing"))
  }
  if (cache_path_is_symlink(path) || !cache_path_is_regular(path)) {
    return(list(ok = FALSE, reason = "a trial log entry is not a regular file"))
  }
  if (cache_private_modes_supported()) {
    return(cache_private_file_identity(path, guard$trust))
  }

  info <- trial_log_path_info(path)
  stable <- trial_log_stable_device_inode(info)
  if (is.null(info) || nrow(info) != 1L || is.null(stable)) {
    return(list(ok = FALSE, reason = "the file identity is unavailable"))
  }
  list(
    ok = TRUE,
    identity = list(
      device_id = stable$device_id,
      inode = stable$inode,
      size = as.numeric(info$size[[1L]]),
      modification_time = as.numeric(info$modification_time[[1L]]),
      change_time = as.numeric(info$change_time[[1L]])
    )
  )
}

trial_log_assert_private_file <- function(path, what, guard) {
  path <- trial_log_assert_regular_file(path, what, guard)
  if (!cache_private_modes_supported()) {
    return(invisible(path))
  }

  owner <- cache_path_owner_id(path)
  effective_owner <- cache_effective_owner_id()
  prior_mode <- cache_path_mode(path)
  if (
    is.na(owner) ||
      is.na(effective_owner) ||
      !identical(owner, effective_owner) ||
      is.na(prior_mode)
  ) {
    trial_log_trust_abort(
      paste0(what, " is not owned by the effective user")
    )
  }
  if (bitwAnd(prior_mode, as.integer(as.octmode("0022"))) != 0L) {
    trial_log_trust_abort(
      paste0(what, " was writable by another local account")
    )
  }
  if (!identical(prior_mode, as.integer(as.octmode("0600")))) {
    trial_log_trust_abort(
      paste0(what, " must have mode exactly 0600"),
      remedy = trial_log_chmod_remedy(path, "600")
    )
  }
  identity <- trial_log_file_identity(path, guard)
  if (!isTRUE(identity$ok)) {
    trial_log_trust_abort(identity$reason)
  }
  invisible(path)
}

trial_log_assert_existing_files <- function(guard, files) {
  for (what in names(files)) {
    path <- file.path(guard$path, files[[what]])
    if (file.exists(path) || cache_path_is_symlink(path)) {
      trial_log_assert_private_file(path, what, guard)
    }
  }
  invisible(TRUE)
}

trial_log_known_files <- function() {
  c(
    "trial log lock" = ".trials.lock",
    "trial log journal" = "trials.jsonl",
    "trial log metadata" = "metadata.json",
    "trial log summary" = "README.md",
    "best program artifact" = "best_program.rds"
  )
}

trial_log_identity_value <- function(
  identity,
  include_content = FALSE,
  include_change_time = TRUE
) {
  fields <- c("device_id", "inode", "owner_id", "mode")
  if (isTRUE(include_content)) {
    fields <- c(fields, "size", "modification_time")
    if (isTRUE(include_change_time)) {
      fields <- c(fields, "change_time")
    }
  }
  identity[intersect(fields, names(identity))]
}

trial_log_assert_same_file <- function(
  before,
  after,
  message,
  content = FALSE,
  include_change_time = TRUE
) {
  if (
    !identical(
      trial_log_identity_value(
        before,
        include_content = content,
        include_change_time = include_change_time
      ),
      trial_log_identity_value(
        after,
        include_content = content,
        include_change_time = include_change_time
      )
    )
  ) {
    trial_log_trust_abort(message)
  }
  invisible(TRUE)
}

trial_log_with_lock <- function(
  save_dir,
  code,
  expected_trust = NULL,
  private_directory = TRUE
) {
  guard <- trial_log_prepare_directory(
    save_dir,
    create = TRUE,
    expected_trust = expected_trust,
    private = private_directory
  )
  timeout <- getOption("dsprrr.trial_log_lock_timeout", 10)
  if (
    length(timeout) != 1L ||
      !is.numeric(timeout) ||
      is.na(timeout) ||
      timeout < 0
  ) {
    timeout <- 10
  }
  lock_path <- file.path(guard$path, ".trials.lock")
  if (cache_path_is_symlink(lock_path)) {
    trial_log_trust_abort("the trial log lock is a symbolic link")
  }
  before_lock <- NULL
  if (file.exists(lock_path)) {
    trial_log_assert_private_file(lock_path, "trial log lock", guard)
    before_lock <- trial_log_file_identity(lock_path, guard)$identity
  }

  old_umask <- Sys.umask("0077")
  on.exit(Sys.umask(old_umask), add = TRUE)
  lock <- tryCatch(
    filelock::lock(lock_path, timeout = timeout * 1000),
    error = function(e) e
  )
  Sys.umask(old_umask)
  if (inherits(lock, "condition")) {
    cli::cli_abort(
      c(
        "Failed to acquire the trial log lock",
        "x" = "Lock: {.path {lock_path}}",
        "i" = "Error: {conditionMessage(lock)}"
      ),
      parent = lock,
      class = "dsprrr_trial_log_lock_error"
    )
  }
  if (is.null(lock)) {
    cli::cli_abort(
      c(
        "Timed out waiting for the trial log lock",
        "x" = "Lock: {.path {lock_path}}",
        "i" = "Another process may still be publishing this log."
      ),
      class = "dsprrr_trial_log_lock_error"
    )
  }
  on.exit(filelock::unlock(lock), add = TRUE)
  trial_log_assert_parent(guard)
  trial_log_assert_private_file(lock_path, "trial log lock", guard)
  locked_identity <- trial_log_file_identity(lock_path, guard)
  if (!isTRUE(locked_identity$ok)) {
    trial_log_trust_abort(locked_identity$reason)
  }
  if (!is.null(before_lock)) {
    trial_log_assert_same_file(
      before_lock,
      locked_identity$identity,
      "the trial log lock changed while it was being acquired"
    )
  }
  hook <- getOption("dsprrr.trial_log_lock_hook")
  if (is.function(hook)) {
    hook()
  }
  trial_log_assert_parent(guard)
  result <- code(guard)
  trial_log_assert_parent(guard)
  result
}

trial_log_verified_read <- function(path, what, guard, reader) {
  path <- trial_log_assert_private_file(path, what, guard)
  before <- trial_log_file_identity(path, guard)
  if (!isTRUE(before$ok)) {
    trial_log_trust_abort(before$reason)
  }
  value <- reader(path)
  trial_log_assert_parent(guard)
  after <- trial_log_file_identity(path, guard)
  if (!isTRUE(after$ok)) {
    trial_log_trust_abort(after$reason)
  }
  trial_log_assert_same_file(
    before$identity,
    after$identity,
    paste0(what, " changed while it was being read"),
    content = TRUE
  )
  value
}

trial_log_read_raw <- function(path, guard, what = "trial log journal") {
  trial_log_verified_read(path, what, guard, function(candidate) {
    size <- file.info(candidate, extra_cols = FALSE)$size[[1L]]
    if (is.na(size) || size == 0L) {
      return(raw())
    }
    connection <- file(candidate, open = "rb")
    on.exit(close(connection), add = TRUE)
    readBin(connection, what = "raw", n = size)
  })
}

trial_log_publication_hook <- function(phase, path, temporary, what) {
  hook <- getOption("dsprrr.trial_log_publication_hook")
  if (is.function(hook)) {
    hook(
      phase = phase,
      path = path,
      temporary = temporary,
      what = what
    )
  }
  invisible(NULL)
}

trial_log_atomic_publish <- function(path, what, guard, writer, verifier) {
  path <- trial_log_child_path(path, guard)
  if (cache_path_is_symlink(path)) {
    trial_log_trust_abort(paste0(what, " target is a symbolic link"))
  }
  existing <- NULL
  existing_hold <- NULL
  on.exit(artifact_file_hold_release(existing_hold), add = TRUE)
  if (file.exists(path)) {
    trial_log_assert_private_file(path, what, guard)
    existing <- trial_log_file_identity(path, guard)$identity
    existing_hold <- tryCatch(
      artifact_file_hold(path),
      error = function(e) e
    )
    if (inherits(existing_hold, "condition")) {
      trial_log_trust_abort(
        paste0(what, " target identity could not be held"),
        parent = existing_hold
      )
    }
  }

  temporary <- artifact_private_stage(path)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  staged_empty <- trial_log_file_identity(temporary, guard)
  if (!isTRUE(staged_empty$ok)) {
    trial_log_trust_abort(staged_empty$reason)
  }
  staging_hold <- tryCatch(
    artifact_file_hold(temporary),
    error = function(e) e
  )
  on.exit(artifact_file_hold_release(staging_hold), add = TRUE)
  if (inherits(staging_hold, "condition")) {
    trial_log_trust_abort(
      paste0(what, " staging identity could not be held"),
      parent = staging_hold
    )
  }
  trial_log_publication_hook("stage_created", path, temporary, what)
  writer(temporary)
  staged_written <- trial_log_file_identity(temporary, guard)
  if (!isTRUE(staged_written$ok)) {
    trial_log_trust_abort(staged_written$reason)
  }
  trial_log_assert_same_file(
    staged_empty$identity,
    staged_written$identity,
    paste0(what, " staging file changed while it was being written")
  )
  verifier(temporary)
  staged_verified <- trial_log_file_identity(temporary, guard)
  if (!isTRUE(staged_verified$ok)) {
    trial_log_trust_abort(staged_verified$reason)
  }
  trial_log_assert_same_file(
    staged_written$identity,
    staged_verified$identity,
    paste0(what, " staging file changed while it was being verified"),
    content = TRUE
  )

  trial_log_publication_hook("before_publish", path, temporary, what)
  trial_log_assert_parent(guard)
  if (is.null(existing)) {
    if (file.exists(path) || cache_path_is_symlink(path)) {
      trial_log_trust_abort(
        paste0(what, " target appeared during publication")
      )
    }
  } else {
    current <- trial_log_file_identity(path, guard)
    if (!isTRUE(current$ok)) {
      trial_log_trust_abort(current$reason)
    }
    trial_log_assert_same_file(
      existing,
      current$identity,
      paste0(what, " target changed before publication"),
      content = TRUE
    )
  }

  artifact_file_hold_release(staging_hold)
  staging_hold <- NULL
  artifact_file_hold_release(existing_hold)
  existing_hold <- NULL
  tryCatch(
    artifact_atomic_replace(temporary, path, what = what),
    error = function(e) {
      cli::cli_abort(
        "Could not atomically publish {what}",
        parent = e,
        class = "dsprrr_trial_log_io_error"
      )
    }
  )
  trial_log_publication_hook("after_publish", path, temporary, what)
  trial_log_assert_parent(guard)
  installed <- trial_log_file_identity(path, guard)
  if (!isTRUE(installed$ok)) {
    trial_log_trust_abort(installed$reason)
  }
  trial_log_assert_same_file(
    staged_verified$identity,
    installed$identity,
    paste0("the published ", what, " is not the verified staging file"),
    content = TRUE,
    include_change_time = FALSE
  )
  verifier(path)
  final <- trial_log_file_identity(path, guard)
  if (!isTRUE(final$ok)) {
    trial_log_trust_abort(final$reason)
  }
  trial_log_assert_same_file(
    installed$identity,
    final$identity,
    paste0("the published ", what, " changed during final verification"),
    content = TRUE
  )
  invisible(path)
}

trial_log_validate_raw_journal <- function(path, guard) {
  raw <- trial_log_read_raw(path, guard)
  if (
    length(raw) > 0L &&
      !identical(raw[[length(raw)]], charToRaw("\n")[[1L]])
  ) {
    trial_log_abort_corrupt(
      path,
      "The trial log ends with an incomplete record",
      torn = TRUE
    )
  }

  lines <- trial_log_verified_read(
    path,
    "trial log journal",
    guard,
    function(candidate) readLines(candidate, warn = FALSE)
  )
  if (!all(nzchar(trimws(lines)))) {
    trial_log_abort_corrupt(path, "The trial log contains an empty record")
  }
  if (length(lines) == 0L) {
    return(list())
  }

  trials <- withCallingHandlers(
    trial_log_verified_read(
      path,
      "trial log journal",
      guard,
      trial_log_parse_jsonl_file
    ),
    dsprrr_parse_warning = function(w) {
      trial_log_abort_corrupt(
        path,
        "The trial log contains an invalid JSON record",
        parent = w
      )
    }
  )
  if (length(trials) != length(lines)) {
    trial_log_abort_corrupt(
      path,
      "The trial log could not be read without dropping records"
    )
  }

  ids <- vapply(trials, function(trial) trial@trial_id, character(1))
  if (anyDuplicated(ids)) {
    trial_log_abort_corrupt(
      path,
      "The trial log contains duplicate trial IDs"
    )
  }
  trial_log_merge_unique(list(), trials, source = path)
}

trial_log_atomic_write_raw <- function(value, path, guard) {
  trial_log_atomic_publish(
    path = path,
    what = "trial log journal",
    guard = guard,
    writer = function(temporary) {
      connection <- file(temporary, open = "wb")
      tryCatch(writeBin(value, connection), finally = close(connection))
    },
    verifier = function(candidate) {
      actual <- trial_log_read_raw(candidate, guard)
      if (!identical(actual, value)) {
        cli::cli_abort(
          "Could not verify the trial log journal bytes",
          class = "dsprrr_trial_log_verification_error"
        )
      }
      trial_log_validate_raw_journal(candidate, guard)
    }
  )
}

trial_log_ensure_private_journal <- function(path, guard) {
  path <- trial_log_child_path(path, guard)
  if (!file.exists(path)) {
    if (cache_path_is_symlink(path)) {
      trial_log_trust_abort("the trial log journal is a symbolic link")
    }
    trial_log_atomic_write_raw(raw(), path, guard)
  }
  trial_log_assert_private_file(path, "trial log journal", guard)
}

trial_log_read_journal <- function(path, guard) {
  trial_log_ensure_private_journal(path, guard)
  trial_log_validate_raw_journal(path, guard)
}

trial_log_append_verified <- function(trials, path, existing, guard) {
  if (length(trials) == 0L) {
    return(existing)
  }
  expected <- trial_log_merge_unique(existing, trials, source = path)
  new_lines <- vapply(trials, trial_json_line, character(1))
  before_lines <- trial_log_verified_read(
    path,
    "trial log journal",
    guard,
    function(candidate) readLines(candidate, warn = FALSE)
  )
  before_raw <- trial_log_read_raw(path, guard)
  expected_lines <- c(before_lines, new_lines)
  payload <- paste0(paste(new_lines, collapse = "\n"), "\n")
  next_raw <- c(before_raw, charToRaw(enc2utf8(payload)))

  failure <- tryCatch(
    {
      trial_log_atomic_write_raw(next_raw, path, guard)
      verified <- trial_log_read_journal(path, guard)
      verified_lines <- trial_log_verified_read(
        path,
        "trial log journal",
        guard,
        function(candidate) readLines(candidate, warn = FALSE)
      )
      if (!identical(verified_lines, expected_lines)) {
        cli::cli_abort(
          "The appended trial log bytes did not match the requested records",
          class = "dsprrr_trial_log_verification_error"
        )
      }
      expected_records <- vapply(expected, trial_json_line, character(1))
      verified_records <- vapply(verified, trial_json_line, character(1))
      if (!identical(verified_records, expected_records)) {
        cli::cli_abort(
          "The merged trial log did not match the requested records",
          class = "dsprrr_trial_log_verification_error"
        )
      }
      NULL
    },
    error = function(e) e
  )
  if (is.null(failure)) {
    return(expected)
  }
  cli::cli_abort(
    c(
      "Failed to atomically publish the trial log journal",
      "x" = "Journal: {.path {path}}",
      "i" = "Derived metadata was not published."
    ),
    parent = failure,
    class = c(
      if (inherits(failure, "dsprrr_trial_log_trust_error")) {
        "dsprrr_trial_log_trust_error"
      },
      "dsprrr_trial_log_append_error",
      "dsprrr_trial_log_io_error"
    )
  )
}

trial_log_atomic_write_json <- function(value, path, guard) {
  trial_log_atomic_publish(
    path = path,
    what = "trial log metadata",
    guard = guard,
    writer = function(temporary) {
      jsonlite::write_json(
        value,
        temporary,
        auto_unbox = TRUE,
        pretty = TRUE
      )
    },
    verifier = function(candidate) {
      trial_log_verified_read(
        candidate,
        "trial log metadata",
        guard,
        function(file) jsonlite::fromJSON(file, simplifyVector = FALSE)
      )
    }
  )
}

trial_log_atomic_write_lines <- function(lines, path, guard) {
  trial_log_atomic_publish(
    path = path,
    what = "trial log summary",
    guard = guard,
    writer = function(temporary) {
      writeLines(lines, temporary, useBytes = TRUE)
    },
    verifier = function(candidate) {
      actual <- trial_log_verified_read(
        candidate,
        "trial log summary",
        guard,
        function(file) readLines(file, warn = FALSE)
      )
      if (!identical(actual, lines)) {
        cli::cli_abort(
          "Could not verify staged trial log text",
          class = "dsprrr_trial_log_verification_error"
        )
      }
    }
  )
}

trial_log_atomic_write_program <- function(program, path, guard) {
  artifact <- program_artifact(program)
  trial_log_atomic_publish(
    path = path,
    what = "trial log best program",
    guard = guard,
    writer = function(temporary) artifact_write_rds(artifact, temporary),
    verifier = function(candidate) {
      restored <- trial_log_verified_read(
        candidate,
        "trial log best program",
        guard,
        readRDS
      )
      artifact_validate_manifest(restored)
    }
  )
}

trial_log_read_metadata <- function(path, guard) {
  path <- trial_log_child_path(path, guard)
  if (!file.exists(path)) {
    if (cache_path_is_symlink(path)) {
      trial_log_trust_abort("the trial log metadata is a symbolic link")
    }
    return(list())
  }
  tryCatch(
    trial_log_verified_read(
      path,
      "trial log metadata",
      guard,
      function(candidate) {
        jsonlite::fromJSON(candidate, simplifyVector = FALSE)
      }
    ),
    error = function(e) {
      if (inherits(e, "dsprrr_trial_log_trust_error")) {
        stop(e)
      }
      cli::cli_warn(
        c(
          "Ignored invalid derived trial log metadata",
          "x" = "Path: {.path {path}}",
          "i" = "The authoritative trials.jsonl journal was not affected."
        ),
        parent = e,
        class = "dsprrr_save_warning"
      )
      list()
    }
  )
}

#' Internal optimization-trial record class
#' @noRd
Trial <- S7::new_class(
  "Trial",
  properties = list(
    trial_id = S7::new_property(S7::class_character, default = ""),
    optimizer_name = S7::new_property(S7::class_character, default = ""),
    params = S7::new_property(S7::class_list, default = list()),
    metric_summary = S7::new_property(S7::class_list, default = list()),
    cost_summary = S7::new_property(S7::class_list, default = list()),
    start_time = S7::new_property(S7::class_any, default = NULL),
    end_time = S7::new_property(S7::class_any, default = NULL),
    notes = S7::new_property(S7::class_character, default = ""),
    compiled_artifact_ref = S7::new_property(S7::class_any, default = NULL),
    status = S7::new_property(
      S7::class_character,
      default = "pending",
      validator = function(value) {
        valid <- c("pending", "running", "completed", "failed")
        if (!value %in% valid) {
          return(sprintf(
            "status must be one of: %s",
            paste(valid, collapse = ", ")
          ))
        }
        NULL
      }
    ),
    trace_context = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        trace_context_validate(value, arg = "trace_context")
        NULL
      }
    )
  )
)

#' Create a Trial Record
#'
#' @description
#' Create an optimization trial record with an automatically generated ID.
#'
#' @param optimizer_name Name of the optimizer.
#' @param params List of parameters for this trial.
#' @param trial_id Optional trial ID. If NULL, auto-generated.
#' @param notes Optional notes.
#' @param trace_context A named, JSON-compatible correlation context. When
#'   omitted during [compile()], the active compilation context is inherited;
#'   supply `list()` explicitly to clear it.
#'
#' @return An optimization trial record.
#' @export
#'
#' @examples
#' trial <- create_trial(
#'   optimizer_name = "BootstrapFewShot",
#'   params = list(max_demos = 4, temperature = 0.7)
#' )
create_trial <- function(
  optimizer_name,
  params = list(),
  trial_id = NULL,
  notes = "",
  trace_context = list()
) {
  trace_context_missing <- missing(trace_context)
  if (is.null(trial_id)) {
    trial_id <- generate_trial_id()
  }
  if (trace_context_missing) {
    trace_context <- current_trace_context()
  }
  trace_context <- trace_context_validate(
    trace_context,
    arg = "trace_context"
  )

  Trial(
    trial_id = trial_id,
    optimizer_name = optimizer_name,
    params = params,
    start_time = Sys.time(),
    notes = notes,
    trace_context = trace_context,
    status = "pending"
  )
}

#' Start a Trial
#'
#' @description
#' Mark a trial as running and record the start time.
#'
#' @param trial A trial record created by [create_trial()].
#'
#' @return Updated Trial object with status "running".
#' @noRd
start_trial <- function(trial) {
  Trial(
    trial_id = trial@trial_id,
    optimizer_name = trial@optimizer_name,
    params = trial@params,
    metric_summary = trial@metric_summary,
    cost_summary = trial@cost_summary,
    start_time = Sys.time(),
    end_time = trial@end_time,
    notes = trial@notes,
    compiled_artifact_ref = trial@compiled_artifact_ref,
    trace_context = trace_context_validate(
      trial@trace_context,
      arg = "trace_context"
    ),
    status = "running"
  )
}

#' Complete a Trial
#'
#' @description
#' Mark a trial as completed with evaluation results.
#'
#' @param trial A trial record created by [create_trial()].
#' @param eval_result An EvalResult object from eval_program().
#' @param compiled_artifact_ref Optional compiled module to persist as the best
#'   safe program artifact when this trial wins.
#' @param notes Optional additional notes.
#'
#' @return The updated trial record with status `"completed"`.
#' @export
complete_trial <- function(
  trial,
  eval_result,
  compiled_artifact_ref = NULL,
  notes = NULL
) {
  metric_summary <- list(
    mean_score = eval_result@mean_score,
    std_error = eval_result@std_error,
    n_evaluated = eval_result@n_evaluated,
    n_errors = eval_result@n_errors
  )

  cost_summary <- list(
    input_tokens = eval_result@input_tokens,
    output_tokens = eval_result@output_tokens,
    total_tokens = eval_result@total_tokens,
    total_cost = eval_result@total_cost,
    provider_calls = eval_result@provider_calls,
    metric_calls = eval_result@metric_calls,
    provider_usage_unknown = eval_result@provider_usage_unknown,
    token_usage_unknown = eval_result@token_usage_unknown,
    latency_ms = eval_result@total_latency_ms
  )

  Trial(
    trial_id = trial@trial_id,
    optimizer_name = trial@optimizer_name,
    params = trial@params,
    metric_summary = metric_summary,
    cost_summary = cost_summary,
    start_time = trial@start_time,
    end_time = Sys.time(),
    notes = notes %||% trial@notes,
    compiled_artifact_ref = compiled_artifact_ref,
    trace_context = trace_context_validate(
      trial@trace_context,
      arg = "trace_context"
    ),
    status = "completed"
  )
}

#' Fail a Trial
#'
#' @description
#' Mark a trial as failed with an error message.
#'
#' @param trial A Trial object.
#' @param error_message Error message explaining the failure.
#'
#' @return Updated Trial object with status "failed".
#' @noRd
fail_trial <- function(trial, error_message) {
  Trial(
    trial_id = trial@trial_id,
    optimizer_name = trial@optimizer_name,
    params = trial@params,
    metric_summary = trial@metric_summary,
    cost_summary = trial@cost_summary,
    start_time = trial@start_time,
    end_time = Sys.time(),
    notes = paste0(trial@notes, "\nError: ", error_message),
    compiled_artifact_ref = trial@compiled_artifact_ref,
    trace_context = trace_context_validate(
      trial@trace_context,
      arg = "trace_context"
    ),
    status = "failed"
  )
}

trial_log_best <- function(trials, objective = "maximize") {
  completed <- Filter(function(trial) trial@status == "completed", trials)
  if (length(completed) == 0L) {
    return(NULL)
  }

  scores <- vapply(
    completed,
    function(trial) trial@metric_summary$mean_score %||% NA_real_,
    numeric(1)
  )
  valid <- !is.na(scores)
  if (!any(valid)) {
    return(NULL)
  }

  valid_trials <- completed[valid]
  valid_scores <- scores[valid]
  best_index <- if (identical(objective, "maximize")) {
    which.max(valid_scores)
  } else {
    which.min(valid_scores)
  }
  valid_trials[[best_index]]
}

trial_log_summary <- function(trials) {
  completed <- Filter(function(trial) trial@status == "completed", trials)
  scores <- vapply(
    completed,
    function(trial) trial@metric_summary$mean_score %||% NA_real_,
    numeric(1)
  )
  usage_count <- function(field) {
    vapply(
      completed,
      function(trial) normalize_trial_count(trial@cost_summary[[field]]),
      integer(1)
    )
  }
  costs <- vapply(
    completed,
    function(trial) normalize_trial_cost(trial@cost_summary$total_cost),
    numeric(1)
  )

  list(
    n_trials = length(trials),
    n_completed = length(completed),
    n_failed = sum(vapply(
      trials,
      function(trial) trial@status == "failed",
      logical(1)
    )),
    best_score = if (length(scores) > 0L && !all(is.na(scores))) {
      max(scores, na.rm = TRUE)
    } else {
      NA_real_
    },
    mean_score = if (length(scores) > 0L && !all(is.na(scores))) {
      mean(scores, na.rm = TRUE)
    } else {
      NA_real_
    },
    input_tokens = sum_trial_counts(usage_count("input_tokens")),
    output_tokens = sum_trial_counts(usage_count("output_tokens")),
    total_tokens = sum_trial_counts(usage_count("total_tokens")),
    provider_calls = sum_trial_counts(usage_count("provider_calls")),
    metric_calls = sum_trial_counts(usage_count("metric_calls")),
    total_cost = sum_cost_values(costs)
  )
}

persist_trial_log_derived <- function(
  optimizer_name,
  metadata,
  trials,
  guard
) {
  save_dir <- guard$path
  summary <- trial_log_summary(trials)

  metadata_path <- file.path(save_dir, "metadata.json")
  meta <- metadata
  meta$optimizer_name <- optimizer_name
  meta$n_trials <- length(trials)
  meta$saved_at <- Sys.time()
  tryCatch(
    trial_log_atomic_write_json(meta, metadata_path, guard),
    error = function(e) {
      cli::cli_warn(
        c(
          "Failed to save metadata",
          "x" = "Path: {.path {metadata_path}}",
          "i" = "Error: {conditionMessage(e)}"
        ),
        class = "dsprrr_save_warning"
      )
    }
  )

  best <- trial_log_best(trials)
  if (
    !is.null(best) &&
      inherits(best@compiled_artifact_ref, "Module")
  ) {
    tryCatch(
      trial_log_atomic_write_program(
        best@compiled_artifact_ref,
        file.path(save_dir, "best_program.rds"),
        guard
      ),
      error = function(e) {
        cli::cli_warn(
          c(
            "Failed to save best program artifact",
            "i" = "Error: {conditionMessage(e)}"
          ),
          class = "dsprrr_save_warning"
        )
      }
    )
  }

  readme_path <- file.path(save_dir, "README.md")
  readme_content <- c(
    sprintf("# Optimizer Log: %s", optimizer_name),
    "",
    sprintf("- Trials: %d", summary$n_trials),
    sprintf("- Best Score: %.4f", summary$best_score),
    sprintf("- Total Cost: %s", format_trial_cost(summary$total_cost)),
    sprintf(
      "- Created: %s",
      format_trial_created_at(metadata$created_at)
    )
  )
  tryCatch(
    trial_log_atomic_write_lines(readme_content, readme_path, guard),
    error = function(e) {
      cli::cli_warn(
        c("Failed to save README", "i" = "Error: {conditionMessage(e)}"),
        class = "dsprrr_save_warning"
      )
    }
  )

  invisible(save_dir)
}

trial_log_restore_runtime_refs <- function(persisted, runtime) {
  if (length(persisted) == 0L || length(runtime) == 0L) {
    return(persisted)
  }
  runtime_ids <- vapply(runtime, function(trial) trial@trial_id, character(1))
  for (i in seq_along(persisted)) {
    matches <- which(runtime_ids == persisted[[i]]@trial_id)
    if (length(matches) == 0L) {
      next
    }
    candidate <- runtime[[matches[[1L]]]]
    if (
      trial_records_identical(persisted[[i]], candidate) &&
        !is.null(candidate@compiled_artifact_ref)
    ) {
      persisted[[i]] <- candidate
    }
  }
  persisted
}

trial_log_sync_locked <- function(
  guard,
  memory_trials,
  requested_trials,
  optimizer_name,
  metadata
) {
  save_dir <- guard$path
  trials_path <- file.path(save_dir, "trials.jsonl")
  disk_trials <- trial_log_read_journal(trials_path, guard)
  memory_trials <- trial_log_merge_unique(
    memory_trials,
    disk_trials,
    source = trials_path
  )
  memory_trials <- trial_log_merge_unique(
    memory_trials,
    requested_trials,
    source = "in-memory trial log"
  )
  persisted <- trial_log_merge_unique(
    disk_trials,
    requested_trials,
    source = trials_path
  )

  disk_ids <- vapply(disk_trials, function(trial) trial@trial_id, character(1))
  missing <- Filter(
    function(trial) !trial@trial_id %in% disk_ids,
    requested_trials
  )
  persisted <- trial_log_append_verified(
    missing,
    trials_path,
    disk_trials,
    guard
  )
  persisted <- trial_log_restore_runtime_refs(
    persisted,
    c(requested_trials, memory_trials)
  )
  persist_trial_log_derived(
    optimizer_name = optimizer_name,
    metadata = metadata,
    trials = persisted,
    guard = guard
  )

  list(memory = memory_trials, persisted = persisted)
}

#' Trial Log
#'
#' @description
#' R6 class for managing a collection of trials with optional persistence.
#' Existing JSONL records are loaded when `log_dir` already contains a log.
#' New trials are appended one record at a time; matching trial IDs are
#' idempotent, while conflicting records with the same ID are rejected. The
#' `trials.jsonl` journal is authoritative. `metadata.json`, `README.md`, and
#' `best_program.rds` are independently refreshed, best-effort derived views;
#' they may lag after an interruption and are rebuilt by a later successful
#' save. On Unix, a pre-existing private log directory must be owned by the
#' effective user with exactly mode `0700`, and every pre-existing log file must
#' have exactly mode `0600`; special mode bits are rejected. Every existing
#' ancestor must be owned by root or the effective user, including sticky
#' shared parents. Before initialization or a save to another directory locks,
#' reads, or mutates storage, dsprrr preflights every known target: the lock,
#' journal, metadata, summary, and best-program artifact. Unsafe paths are
#' rejected without repair or reads. Directories and files created for the
#' current operation are enforced as owner-only. Non-symbolic regular files
#' remain required. Windows uses the account's filesystem ACLs, which base R
#' cannot verify as owner-only, and fails closed if stable device and file
#' identifiers are unavailable.
#'
#' @export
TrialLog <- R6::R6Class(
  "TrialLog",
  public = list(
    #' @field optimizer_name Name of the optimizer using this log.
    optimizer_name = NULL,

    #' @field log_dir Directory for persistence (NULL for in-memory only).
    log_dir = NULL,

    #' @field trials List of optimization trial records.
    trials = NULL,

    #' @field metadata Additional metadata about the optimization run.
    metadata = NULL,

    #' @description
    #' Create or resume a TrialLog. Existing JSONL records are loaded without
    #' rewriting the file.
    #'
    #' @param optimizer_name Name of the optimizer.
    #' @param log_dir Optional directory for persistence.
    #' @param metadata Optional metadata list.
    initialize = function(optimizer_name, log_dir = NULL, metadata = NULL) {
      self$optimizer_name <- optimizer_name
      self$log_dir <- log_dir
      self$trials <- list()

      persisted_metadata <- list()
      if (!is.null(log_dir)) {
        log_guard <- trial_log_prepare_directory(log_dir)
        self$log_dir <- log_guard$path
        private$log_guard <- log_guard
        trial_log_assert_existing_files(
          log_guard,
          trial_log_known_files()
        )
        restored <- trial_log_with_lock(
          self$log_dir,
          function(guard) {
            trials_path <- file.path(guard$path, "trials.jsonl")
            metadata_path <- file.path(guard$path, "metadata.json")
            readme_path <- file.path(guard$path, "README.md")
            if (file.exists(metadata_path)) {
              trial_log_assert_private_file(
                metadata_path,
                "trial log metadata",
                guard
              )
            }
            if (file.exists(readme_path)) {
              trial_log_assert_private_file(
                readme_path,
                "trial log summary",
                guard
              )
            }
            list(
              metadata = trial_log_read_metadata(metadata_path, guard),
              trials = trial_log_read_journal(trials_path, guard)
            )
          },
          expected_trust = log_guard$trust
        )
        persisted_metadata <- restored$metadata
        self$trials <- restored$trials
      }

      self$metadata <- if (is.null(metadata)) {
        persisted_metadata
      } else {
        utils::modifyList(persisted_metadata, metadata)
      }
      if (is.null(self$metadata$created_at)) {
        self$metadata$created_at <- Sys.time()
      }
      if (is.null(self$metadata$r_version)) {
        self$metadata$r_version <- R.version.string
      }
      if (
        identical(optimizer_name, "unknown") &&
          is.character(self$metadata$optimizer_name) &&
          length(self$metadata$optimizer_name) == 1L &&
          nzchar(self$metadata$optimizer_name)
      ) {
        self$optimizer_name <- self$metadata$optimizer_name
      }
      if (
        identical(self$optimizer_name, "unknown") &&
          length(self$trials) > 0L
      ) {
        persisted_names <- unique(vapply(
          self$trials,
          function(trial) trial@optimizer_name,
          character(1)
        ))
        persisted_names <- persisted_names[nzchar(persisted_names)]
        if (length(persisted_names) == 1L) {
          self$optimizer_name <- persisted_names[[1L]]
        }
      }

      invisible(self)
    },

    #' @description
    #' Add a trial to the log. Persisted trials atomically append one
    #' authoritative JSONL record. Derived metadata, summaries, and the best
    #' program are then refreshed independently on a best-effort basis.
    #'
    #' @param trial A trial record created by [create_trial()].
    #' @param persist Whether to immediately persist to disk if log_dir is set.
    add_trial = function(trial, persist = TRUE) {
      if (!inherits(trial, "dsprrr::Trial")) {
        cli::cli_abort("{.arg trial} must be a Trial object")
      }

      if (persist && !is.null(self$log_dir)) {
        synced <- trial_log_with_lock(
          self$log_dir,
          function(guard) {
            trial_log_sync_locked(
              guard = guard,
              memory_trials = self$trials,
              requested_trials = list(trial),
              optimizer_name = self$optimizer_name,
              metadata = self$metadata
            )
          },
          expected_trust = private$log_guard$trust
        )
        self$trials <- synced$memory
      } else {
        self$trials <- trial_log_merge_unique(
          self$trials,
          list(trial),
          source = "in-memory trial log"
        )
      }

      invisible(self)
    },

    #' @description
    #' Get the number of trials.
    #'
    #' @return Integer count of trials.
    n_trials = function() {
      length(self$trials)
    },

    #' @description
    #' Get trials as a tibble.
    #'
    #' @return A tibble with one row per trial.
    as_tibble = function() {
      if (length(self$trials) == 0) {
        return(tibble::tibble(
          trial_id = character(),
          optimizer_name = character(),
          status = character(),
          mean_score = numeric(),
          n_evaluated = integer(),
          n_errors = integer(),
          input_tokens = integer(),
          output_tokens = integer(),
          total_tokens = integer(),
          total_cost = numeric(),
          provider_calls = integer(),
          metric_calls = integer(),
          provider_usage_unknown = logical(),
          token_usage_unknown = logical(),
          latency_ms = numeric(),
          start_time = .POSIXct(numeric()),
          end_time = .POSIXct(numeric()),
          params = list(),
          trace_context = list(),
          notes = character()
        ))
      }

      tibble::tibble(
        trial_id = vapply(
          self$trials,
          function(t) t@trial_id,
          character(1)
        ),
        optimizer_name = vapply(
          self$trials,
          function(t) t@optimizer_name,
          character(1)
        ),
        status = vapply(
          self$trials,
          function(t) t@status,
          character(1)
        ),
        mean_score = vapply(
          self$trials,
          function(t) t@metric_summary$mean_score %||% NA_real_,
          numeric(1)
        ),
        n_evaluated = vapply(
          self$trials,
          function(t) as.integer(t@metric_summary$n_evaluated %||% 0L),
          integer(1)
        ),
        n_errors = vapply(
          self$trials,
          function(t) as.integer(t@metric_summary$n_errors %||% 0L),
          integer(1)
        ),
        input_tokens = vapply(
          self$trials,
          function(t) normalize_trial_count(t@cost_summary$input_tokens),
          integer(1)
        ),
        output_tokens = vapply(
          self$trials,
          function(t) normalize_trial_count(t@cost_summary$output_tokens),
          integer(1)
        ),
        total_tokens = vapply(
          self$trials,
          function(t) normalize_trial_count(t@cost_summary$total_tokens),
          integer(1)
        ),
        total_cost = vapply(
          self$trials,
          function(t) normalize_trial_cost(t@cost_summary$total_cost),
          numeric(1)
        ),
        provider_calls = vapply(
          self$trials,
          function(t) normalize_trial_count(t@cost_summary$provider_calls),
          integer(1)
        ),
        metric_calls = vapply(
          self$trials,
          function(t) normalize_trial_count(t@cost_summary$metric_calls),
          integer(1)
        ),
        provider_usage_unknown = vapply(
          self$trials,
          function(t) {
            normalize_trial_flag(
              t@cost_summary$provider_usage_unknown
            )
          },
          logical(1)
        ),
        token_usage_unknown = vapply(
          self$trials,
          function(t) normalize_trial_flag(t@cost_summary$token_usage_unknown),
          logical(1)
        ),
        latency_ms = vapply(
          self$trials,
          function(t) t@cost_summary$latency_ms %||% NA_real_,
          numeric(1)
        ),
        start_time = do.call(
          c,
          lapply(
            self$trials,
            function(t) t@start_time %||% as.POSIXct(NA)
          )
        ),
        end_time = do.call(
          c,
          lapply(
            self$trials,
            function(t) t@end_time %||% as.POSIXct(NA)
          )
        ),
        params = lapply(self$trials, function(t) t@params),
        trace_context = lapply(
          self$trials,
          function(t) {
            trace_context_validate(t@trace_context, arg = "trace_context")
          }
        ),
        notes = vapply(
          self$trials,
          function(t) t@notes,
          character(1)
        )
      )
    },

    #' @description
    #' Get the best trial by score.
    #'
    #' @param objective "maximize" or "minimize".
    #' @return The best optimization trial record, or `NULL` if no trials have
    #'   completed.
    best_trial = function(objective = "maximize") {
      trial_log_best(self$trials, objective = objective)
    },

    #' @description
    #' Get summary statistics for all trials.
    #'
    #' @return A list with summary statistics.
    summary = function() {
      trial_log_summary(self$trials)
    },

    #' @description
    #' Save the trial log to disk. Only records missing from the destination are
    #' appended to the authoritative journal. Derived files are independently
    #' refreshed on a best-effort basis and may lag if that refresh warns.
    #'
    #' @param dir Optional directory override.
    #' @return Invisibly returns self. Throws error on critical failure.
    save = function(dir = NULL) {
      save_dir <- dir %||% self$log_dir

      if (is.null(save_dir)) {
        cli::cli_warn("No log_dir specified; cannot save trial log")
        return(invisible(self))
      }

      expected_trust <- if (is.null(dir) && !is.null(private$log_guard)) {
        private$log_guard$trust
      } else {
        NULL
      }
      save_guard <- trial_log_prepare_directory(
        save_dir,
        expected_trust = expected_trust
      )
      trial_log_assert_existing_files(
        save_guard,
        trial_log_known_files()
      )
      synced <- trial_log_with_lock(
        save_guard$path,
        function(guard) {
          trial_log_sync_locked(
            guard = guard,
            memory_trials = self$trials,
            requested_trials = self$trials,
            optimizer_name = self$optimizer_name,
            metadata = self$metadata
          )
        },
        expected_trust = save_guard$trust
      )
      self$trials <- synced$memory
      if (is.null(dir)) {
        self$log_dir <- save_guard$path
        private$log_guard <- save_guard
      }

      if (isTRUE(getOption("dsprrr.verbose"))) {
        cli::cli_alert_success("Saved trial log to {.path {save_dir}}")
      }

      invisible(self)
    },

    #' @description
    #' Print the trial log summary.
    print = function() {
      cli::cli_h3("Trial Log: {self$optimizer_name}")

      summary <- self$summary()
      cli::cli_text(
        "{.field Trials}: {summary$n_trials} ({summary$n_completed} completed, {summary$n_failed} failed)"
      )

      if (!is.na(summary$best_score)) {
        cli::cli_text("{.field Best Score}: {round(summary$best_score, 4)}")
      }

      if (is.na(summary$total_tokens)) {
        cli::cli_text("{.field Total Tokens}: Unknown")
      } else if (summary$total_tokens > 0) {
        cli::cli_text("{.field Total Tokens}: {summary$total_tokens}")
      }

      if (summary$n_completed > 0) {
        cost_label <- format_trial_cost(summary$total_cost)
        cli::cli_text("{.field Total Cost}: {cost_label}")
      }

      if (!is.null(self$log_dir)) {
        cli::cli_text("{.field Log Dir}: {.path {self$log_dir}}")
      }

      invisible(self)
    }
  ),
  private = list(log_guard = NULL)
)

#' Write Trials to JSONL File
#'
#' @description
#' Write a list of optimization trial records to a JSONL (JSON Lines) file.
#' Each trial is written as a single JSON object on its own line. On Unix, an
#' existing target must already be owned by the effective user with mode
#' exactly `0600`, without special bits, and every existing ancestor must be
#' owned by root or the effective user. Unsafe paths are rejected rather than
#' repaired.
#'
#' @param trials Trial records created by [create_trial()].
#' @param path File path for the JSONL file.
#' @param append Whether to append to existing file. Default is FALSE.
#'
#' @return Invisibly returns the path.
#' @export
#'
#' @examples
#' \dontrun{
#' trials <- list(
#'   create_trial("BootstrapFewShot", list(k = 4)),
#'   create_trial("BootstrapFewShot", list(k = 8))
#' )
#' write_trials_jsonl(trials, "trials.jsonl")
#' }
write_trials_jsonl <- function(trials, path, append = FALSE) {
  original_path <- path
  absolute <- trial_log_absolute_path(path)
  directory <- dirname(absolute)
  if (!dir.exists(directory)) {
    cli::cli_abort(
      "Trial log directory does not exist: {.path {directory}}",
      class = "dsprrr_trial_log_io_error"
    )
  }
  initial_guard <- trial_log_prepare_directory(
    directory,
    create = FALSE,
    private = FALSE
  )
  path <- file.path(initial_guard$path, basename(absolute))
  if (file.exists(path) || cache_path_is_symlink(path)) {
    trial_log_assert_private_file(path, "trial log journal", initial_guard)
  }
  trial_log_with_lock(
    directory,
    function(guard) {
      if (append) {
        existing <- trial_log_read_journal(path, guard)
        disk_ids <- vapply(
          existing,
          function(trial) trial@trial_id,
          character(1)
        )
        requested <- trial_log_merge_unique(
          existing,
          trials,
          source = path
        )
        missing <- Filter(
          function(trial) !trial@trial_id %in% disk_ids,
          requested
        )
        missing_ids <- vapply(
          missing,
          function(trial) trial@trial_id,
          character(1)
        )
        missing <- requested[vapply(
          requested,
          function(trial) trial@trial_id %in% missing_ids,
          logical(1)
        )]
        trial_log_append_verified(missing, path, existing, guard)
        return(invisible(original_path))
      }

      if (file.exists(path)) {
        trial_log_assert_private_file(path, "trial log journal", guard)
      } else if (cache_path_is_symlink(path)) {
        trial_log_trust_abort("the trial log journal is a symbolic link")
      }
      unique_trials <- trial_log_merge_unique(list(), trials, source = path)
      lines <- vapply(unique_trials, trial_json_line, character(1))
      payload <- if (length(lines) == 0L) {
        raw()
      } else {
        charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n")))
      }
      trial_log_atomic_write_raw(payload, path, guard)
      invisible(original_path)
    },
    expected_trust = initial_guard$trust,
    private_directory = FALSE
  )

  invisible(original_path)
}

trial_log_parse_jsonl_file <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))] # Remove empty lines

  if (length(lines) == 0) {
    return(list())
  }

  # Helper to check if a value is valid for timestamp parsing
  is_valid_timestamp <- function(x) {
    !is.null(x) && length(x) > 0 && !is.na(x) && nzchar(x)
  }

  # Parse each line with error handling
  parsed_trials <- lapply(seq_along(lines), function(i) {
    line <- lines[[i]]

    tryCatch(
      {
        data <- jsonlite::fromJSON(line)
        trace_data <- jsonlite::fromJSON(line, simplifyVector = FALSE)
        validate_trial_record(trace_data)

        # Parse timestamps
        start_time <- if (is_valid_timestamp(data$start_time)) {
          as.POSIXct(data$start_time, format = "%Y-%m-%dT%H:%M:%S")
        } else {
          NULL
        }

        end_time <- if (is_valid_timestamp(data$end_time)) {
          as.POSIXct(data$end_time, format = "%Y-%m-%dT%H:%M:%S")
        } else {
          NULL
        }

        cost_summary <- as.list(data$cost_summary)
        if ("total_cost" %in% names(cost_summary)) {
          cost_summary$total_cost <- normalize_trial_cost(
            cost_summary$total_cost
          )
        }
        for (field in c(
          "input_tokens",
          "output_tokens",
          "total_tokens",
          "provider_calls",
          "metric_calls"
        )) {
          if (field %in% names(cost_summary)) {
            cost_summary[[field]] <- normalize_trial_count(
              cost_summary[[field]]
            )
          }
        }
        for (field in c("provider_usage_unknown", "token_usage_unknown")) {
          if (field %in% names(cost_summary)) {
            cost_summary[[field]] <- normalize_trial_flag(
              cost_summary[[field]]
            )
          }
        }

        Trial(
          trial_id = data$trial_id,
          optimizer_name = data$optimizer_name,
          params = as.list(data$params),
          metric_summary = as.list(data$metric_summary),
          cost_summary = cost_summary,
          start_time = start_time,
          end_time = end_time,
          notes = data$notes,
          trace_context = trace_context_validate(
            trace_data$trace_context,
            arg = "trace_context"
          ),
          status = data$status
        )
      },
      error = function(e) {
        # Only schema diagnostics are safe to echo: they name fields and the
        # record's own version, never stored values. Any other failure could
        # quote record content, which must not reach the console.
        detail <- if (inherits(e, "dsprrr_trial_record_malformed")) {
          conditionMessage(e)
        } else {
          NULL
        }
        cli::cli_warn(
          c(
            "Failed to parse trial on line {i}",
            "i" = "The record was skipped because it is invalid or unsafe.",
            if (!is.null(detail)) c("x" = detail)
          ),
          class = "dsprrr_parse_warning"
        )
        NULL
      }
    )
  })

  # Filter out failed parses (NULL values)
  trials <- Filter(Negate(is.null), parsed_trials)

  # Returning an empty list after rejecting every record is indistinguishable
  # from reading an empty log, so a total failure has to be loud.
  if (length(trials) == 0L && length(lines) > 0L) {
    cli::cli_abort(
      c(
        "No readable trial records in {.path {path}}",
        "x" = "All {length(lines)} record{?s} {?was/were} rejected.",
        "i" = "See the warnings above for the reason for each record."
      ),
      class = "dsprrr_trial_log_unreadable"
    )
  }
  trials
}

#' Read Trials from JSONL File
#'
#' @description
#' Read optimization trial records from a JSONL file.
#'
#' @param path File path for the JSONL file.
#'
#' @return A list of optimization trial records.
#' @export
#'
#' @examples
#' \dontrun{
#' trials <- read_trials_jsonl("trials.jsonl")
#' }
read_trials_jsonl <- function(path) {
  trial_log_parse_jsonl_file(path)
}

#' Load Trial Log from Directory
#'
#' @description
#' Load a TrialLog from a directory that was previously saved.
#'
#' @param log_dir Path to the log directory.
#'
#' @return A TrialLog object.
#' @export
#'
#' @examples
#' \dontrun{
#' log <- load_trial_log("logs/my_optimizer/")
#' log$as_tibble()
#' }
load_trial_log <- function(log_dir) {
  if (!dir.exists(log_dir)) {
    cli::cli_abort("Directory not found: {.path {log_dir}}")
  }
  TrialLog$new(optimizer_name = "unknown", log_dir = log_dir)
}

# Print a Trial object through its S7 method.
print_trial <- function(x, ...) {
  cli::cli_h3("Trial: {x@trial_id}")

  status_icon <- switch(
    x@status,
    completed = cli::symbol$tick,
    failed = cli::symbol$cross,
    running = cli::symbol$pointer,
    cli::symbol$bullet
  )

  cli::cli_text("{status_icon} {.field Status}: {x@status}")
  cli::cli_text("{.field Optimizer}: {x@optimizer_name}")

  if (length(x@params) > 0) {
    cli::cli_text("{.field Params}: {paste(names(x@params), collapse = ', ')}")
  }

  if (!is.null(x@metric_summary$mean_score)) {
    cli::cli_text("{.field Score}: {round(x@metric_summary$mean_score, 4)}")
  }

  if (!is.null(x@start_time)) {
    cli::cli_text(
      "{.field Started}: {format(x@start_time, '%Y-%m-%d %H:%M:%S')}"
    )
  }

  invisible(x)
}

# Register S7 print method
S7::method(print, Trial) <- print_trial
