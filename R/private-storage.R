# Shared private-storage locking, ancestor auditing, file identity, and atomic
# publication helpers used by cache, optimizer checkpoints, and trial logs.

private_storage_with_lock <- function(
  lock_path,
  timeout_option,
  on_error,
  on_timeout,
  code,
  default_timeout = 10
) {
  timeout <- getOption(timeout_option, default_timeout)
  if (
    !is.numeric(timeout) ||
      length(timeout) != 1L ||
      is.na(timeout) ||
      timeout < 0
  ) {
    timeout <- default_timeout
  }

  old_umask <- Sys.umask("0077")
  on.exit(Sys.umask(old_umask), add = TRUE)
  lock <- tryCatch(
    filelock::lock(lock_path, timeout = timeout * 1000),
    error = function(e) e
  )
  Sys.umask(old_umask)

  if (inherits(lock, "condition")) {
    return(on_error(lock))
  }
  if (is.null(lock)) {
    return(on_timeout())
  }
  on.exit(filelock::unlock(lock), add = TRUE)

  code()
}

private_storage_non_posix_file_identity <- function(
  path,
  unavailable_reason = "the file identity is unavailable"
) {
  info <- tryCatch(
    suppressWarnings(fs::file_info(path, follow = FALSE, fail = FALSE)),
    error = function(e) NULL
  )
  stable <- file_identity_exact(path)
  if (is.null(info) || nrow(info) != 1L || is.null(stable)) {
    return(list(ok = FALSE, reason = unavailable_reason))
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

private_storage_identity_fields <- function(
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

private_storage_same_file <- function(
  before,
  after,
  include_content = FALSE,
  include_change_time = TRUE
) {
  identical(
    private_storage_identity_fields(
      before,
      include_content = include_content,
      include_change_time = include_change_time
    ),
    private_storage_identity_fields(
      after,
      include_content = include_content,
      include_change_time = include_change_time
    )
  )
}

private_storage_audit_ancestor_pair <- function(
  parent,
  child,
  effective_owner,
  label = "cache",
  child_label = label,
  require_existing_child = FALSE
) {
  writable_mask <- as.integer(as.octmode("0022"))
  sticky_mask <- as.integer(as.octmode("1000"))

  mode <- cache_path_mode(parent)
  if (is.na(mode)) {
    return(list(
      ok = FALSE,
      reason = paste0("ancestor permissions could not be inspected: ", parent)
    ))
  }
  owner <- cache_path_owner_id(parent)
  if (is.na(owner) || !owner %in% c(0L, effective_owner)) {
    return(list(
      ok = FALSE,
      reason = paste0(
        "a ",
        label,
        " ancestor is not owned by the effective user or root: ",
        parent
      )
    ))
  }
  if (bitwAnd(mode, writable_mask) == 0L) {
    return(list(ok = TRUE))
  }
  if (bitwAnd(mode, sticky_mask) == 0L) {
    return(list(
      ok = FALSE,
      reason = paste0("a non-sticky ", label, " ancestor is writable: ", parent)
    ))
  }
  if (
    !isTRUE(require_existing_child) || file.exists(child) || dir.exists(child)
  ) {
    child_owner <- cache_path_owner_id(child)
    if (is.na(child_owner) || !child_owner %in% c(0L, effective_owner)) {
      qualifier <- if (nzchar(child_label)) {
        paste0(" ", child_label)
      } else {
        ""
      }
      return(list(
        ok = FALSE,
        reason = paste0(
          "a sticky writable",
          qualifier,
          " ancestor has a child not owned by the effective user or root: ",
          child
        )
      ))
    }
  }
  list(ok = TRUE)
}

private_storage_audit_existing_ancestors <- function(
  target,
  label = "cache"
) {
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
  repeat {
    if (dir.exists(current)) {
      audit <- private_storage_audit_ancestor_pair(
        parent = current,
        child = child,
        effective_owner = effective_owner,
        label = label,
        child_label = label,
        require_existing_child = TRUE
      )
      if (!isTRUE(audit$ok)) {
        return(audit)
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

private_storage_atomic_publish <- function(
  path,
  what,
  existing = NULL,
  existing_hold = NULL,
  get_identity,
  assert_parent,
  abort_trust,
  writer,
  verifier,
  replace_error,
  post_verifier = NULL,
  hook = NULL,
  hold_staging_msg,
  written_msg,
  verified_msg,
  appeared_msg,
  changed_before_msg,
  mismatch_msg,
  post_changed_msg = NULL,
  check_symlink_on_absent = FALSE
) {
  temporary <- artifact_private_stage(path)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)

  staged_before <- get_identity(temporary)
  if (!isTRUE(staged_before$ok)) {
    abort_trust(staged_before$reason)
  }

  staging_hold <- tryCatch(
    artifact_file_hold(temporary),
    error = function(e) e
  )
  on.exit(artifact_file_hold_release(staging_hold), add = TRUE)
  if (inherits(staging_hold, "condition")) {
    abort_trust(hold_staging_msg, parent = staging_hold)
  }

  if (is.function(hook)) {
    hook("stage_created", path, temporary, what)
  }
  writer(temporary)

  staged_written <- get_identity(temporary)
  if (!isTRUE(staged_written$ok)) {
    abort_trust(staged_written$reason)
  }
  if (
    !private_storage_same_file(staged_before$identity, staged_written$identity)
  ) {
    abort_trust(written_msg)
  }

  verifier(temporary)

  staged_verified <- get_identity(temporary)
  if (!isTRUE(staged_verified$ok)) {
    abort_trust(staged_verified$reason)
  }
  if (
    !private_storage_same_file(
      staged_written$identity,
      staged_verified$identity,
      include_content = TRUE
    )
  ) {
    abort_trust(verified_msg)
  }

  if (is.function(hook)) {
    hook("before_publish", path, temporary, what)
  }
  assert_parent()
  if (is.null(existing)) {
    target_appeared <- file.exists(path) ||
      (isTRUE(check_symlink_on_absent) && cache_path_is_symlink(path))
    if (target_appeared) {
      abort_trust(appeared_msg)
    }
  } else {
    current <- get_identity(path)
    if (!isTRUE(current$ok)) {
      abort_trust(current$reason)
    }
    if (
      !private_storage_same_file(
        existing,
        current$identity,
        include_content = TRUE
      )
    ) {
      abort_trust(changed_before_msg)
    }
  }

  artifact_file_hold_release(staging_hold)
  staging_hold <- NULL
  if (!is.null(existing_hold)) {
    artifact_file_hold_release(existing_hold)
  }

  tryCatch(
    artifact_atomic_replace(temporary, path, what = what),
    error = replace_error
  )

  if (is.function(hook)) {
    hook("after_publish", path, temporary, what)
  }
  assert_parent()
  installed <- get_identity(path)
  if (!isTRUE(installed$ok)) {
    abort_trust(installed$reason)
  }
  if (
    !private_storage_same_file(
      staged_verified$identity,
      installed$identity,
      include_content = TRUE,
      include_change_time = FALSE
    )
  ) {
    abort_trust(mismatch_msg)
  }

  if (is.function(post_verifier)) {
    post_verifier(path)
    final <- get_identity(path)
    if (!isTRUE(final$ok)) {
      abort_trust(final$reason)
    }
    if (
      !private_storage_same_file(
        installed$identity,
        final$identity,
        include_content = TRUE
      )
    ) {
      abort_trust(post_changed_msg)
    }
    return(invisible(final$identity))
  }

  invisible(installed$identity)
}
