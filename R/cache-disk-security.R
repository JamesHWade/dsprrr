# Disk cache directory security, guards, and private file I/O

#' Create Disk Cache
#'
#' @description
#' Attempt to create a disk cache, handling directory creation and errors.
#'
#' @param disk_path Character path for disk cache
#' @param config Cache configuration list
#'
#' @return A disk cache object or NULL if creation failed
#'
#' @noRd
create_disk_cache <- function(disk_path, config) {
  prepared <- prepare_cache_directory(
    disk_path,
    private = isTRUE(config$disk_private)
  )
  if (!prepared$ok) {
    return(cache_disk_degrade(prepared$path, prepared$reason))
  }

  tryCatch(
    {
      private_guard <- if (
        isTRUE(config$disk_private) && cache_private_modes_supported()
      ) {
        new_cache_disk_guard(prepared$trust)
      } else {
        NULL
      }
      write_fn <- if (is.environment(private_guard)) {
        function(value, file) {
          tryCatch(
            write_private_cache_value(value, file, guard = private_guard),
            error = function(error) {
              cache_record_disk_guard_failure(
                private_guard,
                conditionMessage(error)
              )
              rlang::cnd_signal(error)
            }
          )
        }
      } else {
        NULL
      }
      read_fn <- if (is.environment(private_guard)) {
        function(file) read_private_cache_value(file, private_guard)
      } else {
        NULL
      }
      disk_cache <- cachem::cache_disk(
        dir = prepared$path,
        max_size = config$disk_max_size,
        max_age = config$disk_max_age,
        read_fn = read_fn,
        write_fn = write_fn
      )
      if (is.environment(private_guard)) {
        disk_cache <- guarded_cache_disk(disk_cache, private_guard)
        .dsprrr_env$cache_disk_guard <- private_guard
      } else {
        .dsprrr_env$cache_disk_guard <- NULL
      }
      .dsprrr_env$cache_degraded <- FALSE
      .dsprrr_env$cache_degraded_reason <- NULL
      disk_cache
    },
    error = function(e) {
      cache_disk_degrade(prepared$path, conditionMessage(e))
    }
  )
}

#' Whether POSIX owner-only modes can be enforced and verified
#' @noRd
cache_private_modes_supported <- function() {
  identical(.Platform$OS.type, "unix")
}

#' Resolve the effective POSIX user id without relying on shell commands
#' @noRd
cache_effective_owner_id <- function() {
  if (!cache_private_modes_supported()) {
    return(NA_integer_)
  }
  effective_user <- unname(Sys.info()[["effective_user"]])
  if (
    is.null(effective_user) || is.na(effective_user) || !nzchar(effective_user)
  ) {
    return(NA_integer_)
  }
  users <- tryCatch(fs::user_ids(), error = function(e) NULL)
  if (is.null(users)) {
    return(NA_integer_)
  }
  ids <- unique(users$user_id[users$user_name == effective_user])
  if (length(ids) != 1L || is.na(ids[[1]])) {
    return(NA_integer_)
  }
  as.integer(ids[[1]])
}

#' Read the POSIX owner id for one path
#' @noRd
cache_path_owner_id <- function(path) {
  info <- suppressWarnings(file.info(path, extra_cols = TRUE))
  if (nrow(info) != 1L || is.na(info$uid[[1]])) {
    return(NA_integer_)
  }
  as.integer(info$uid[[1]])
}

#' Verify paths are owned by the effective user
#' @noRd
cache_paths_owned_by_effective_user <- function(paths) {
  if (!cache_private_modes_supported() || length(paths) == 0L) {
    return(TRUE)
  }
  effective_owner <- cache_effective_owner_id()
  if (is.na(effective_owner)) {
    return(FALSE)
  }
  owners <- vapply(paths, cache_path_owner_id, integer(1))
  !anyNA(owners) && all(owners == effective_owner)
}

#' Resolve a cache target through its nearest existing ancestor
#'
#' Future cache operations use the returned canonical path, rather than a
#' caller-supplied alias containing a replaceable symlink component.
#' @noRd
cache_canonical_target_path <- function(path) {
  absolute <- tryCatch(
    as.character(fs::path_abs(path.expand(path))),
    error = function(e) NA_character_
  )
  if (length(absolute) != 1L || is.na(absolute) || !nzchar(absolute)) {
    return(NULL)
  }

  current <- absolute
  missing <- character()
  while (!file.exists(current) && !dir.exists(current)) {
    parent <- dirname(current)
    if (identical(parent, current)) {
      return(NULL)
    }
    missing <- c(basename(current), missing)
    current <- parent
  }
  if (!dir.exists(current)) {
    return(NULL)
  }
  canonical_parent <- tryCatch(
    as.character(fs::path_real(current)),
    error = function(e) NULL
  )
  if (is.null(canonical_parent)) {
    return(NULL)
  }
  as.character(do.call(file.path, c(list(canonical_parent), as.list(missing))))
}

#' Audit whether an ancestor can replace a descendant cache path
#'
#' Sticky shared directories such as /tmp are safe only when the next path
#' component belongs to root or the effective user. Writable non-sticky
#' ancestors are rejected because another local account can rename or replace
#' descendants.
#' @noRd
audit_cache_parent_chain <- function(disk_path) {
  if (!cache_private_modes_supported()) {
    return(list(ok = TRUE))
  }
  canonical <- tryCatch(
    as.character(fs::path_real(disk_path)),
    error = function(e) NULL
  )
  if (is.null(canonical)) {
    return(list(ok = FALSE, reason = "the canonical cache path is unavailable"))
  }

  chain <- canonical
  repeat {
    parent <- dirname(chain[[1]])
    if (identical(parent, chain[[1]])) {
      break
    }
    chain <- c(parent, chain)
  }
  if (length(chain) < 2L) {
    return(list(ok = TRUE))
  }

  effective_owner <- cache_effective_owner_id()
  if (is.na(effective_owner)) {
    return(list(
      ok = FALSE,
      reason = "the effective user ID could not be established"
    ))
  }
  for (i in seq_len(length(chain) - 1L)) {
    audit <- private_storage_audit_ancestor_pair(
      parent = chain[[i]],
      child = chain[[i + 1L]],
      effective_owner = effective_owner,
      label = "cache",
      child_label = "",
      require_existing_child = FALSE
    )
    if (!isTRUE(audit$ok)) {
      return(audit)
    }
  }
  list(ok = TRUE)
}

#' Reject writable non-sticky ancestors before creating a cache directory
#' @noRd
audit_existing_cache_parent_capability <- function(disk_path) {
  private_storage_audit_existing_ancestors(disk_path, label = "cache")
}

#' Capture the canonical identity of one trusted cache directory
#' @noRd
cache_directory_identity <- function(path) {
  if (cache_path_is_symlink(path) || !dir.exists(path)) {
    return(NULL)
  }
  canonical <- tryCatch(
    as.character(fs::path_real(path)),
    error = function(e) NULL
  )
  file_identity <- file_identity_exact(path)
  owner <- cache_path_owner_id(path)
  mode <- cache_path_mode(path)
  if (
    is.null(canonical) ||
      is.null(file_identity) ||
      is.na(owner) ||
      is.na(mode)
  ) {
    return(NULL)
  }
  list(
    path = canonical,
    device_id = file_identity$device_id,
    inode = file_identity$inode,
    owner_id = owner,
    mode = mode
  )
}

#' Verify a directory still has its audited identity and safe ancestors
#' @noRd
cache_directory_trust_error <- function(trust) {
  current <- cache_directory_identity(trust$path)
  if (is.null(current) || !identical(current, trust)) {
    return("the cache directory identity changed after it was audited")
  }
  parent_audit <- audit_cache_parent_chain(trust$path)
  if (!isTRUE(parent_audit$ok)) {
    return(parent_audit$reason)
  }
  NULL
}

#' Create shared state for guarded cachem hooks and their adapter
#' @noRd
new_cache_disk_guard <- function(trust) {
  guard <- new.env(parent = emptyenv())
  guard$trust <- trust
  guard$invalid <- FALSE
  guard$reason <- NULL
  guard$warning_emitted <- FALSE
  guard$destroyed <- FALSE
  guard
}

#' Record a sticky disk trust failure and detach global disk handles
#' @noRd
cache_record_disk_guard_failure <- function(guard, reason) {
  if (!is.environment(guard)) {
    return(invisible(NULL))
  }
  if (!isTRUE(guard$invalid)) {
    guard$invalid <- TRUE
    guard$reason <- as.character(reason)[1]
  }
  if (identical(.dsprrr_env$cache_disk_guard, guard)) {
    .dsprrr_env$cache <- .dsprrr_env$cache_memory
    .dsprrr_env$cache_disk <- NULL
    .dsprrr_env$cache_degraded <- TRUE
    .dsprrr_env$cache_degraded_reason <- guard$reason
    .dsprrr_env$cache_privacy_status <- "degraded"
    .dsprrr_env$cache_privacy_reason <- guard$reason
  }
  invisible(NULL)
}

#' Describe why a path failed an exact-mode check
#'
#' A setgid bit inherited from a shared parent leaves the permission triplet
#' looking correct, so name that case rather than reporting a bare mismatch the
#' user cannot see in the obvious places.
#' @noRd
cache_exact_mode_reason <- function(mode, expected, what) {
  special <- bitwAnd(mode, as.integer(as.octmode("7000")))
  permissions <- bitwAnd(mode, as.integer(as.octmode("0777")))
  if (
    special != 0L &&
      identical(permissions, as.integer(as.octmode(expected)))
  ) {
    names <- c("sticky", "setgid", "setuid")[
      bitwAnd(special, c(1L, 2L, 4L) * 512L) != 0L
    ]
    return(paste0(
      "an existing private ",
      what,
      " carries the ",
      paste(names, collapse = " and "),
      " bit, which dsprrr does not accept even with owner-only permissions"
    ))
  }
  paste0(
    "an existing private ",
    what,
    " must have mode exactly ",
    expected,
    ", but it is ",
    format(as.octmode(mode))
  )
}

#' Build a chmod remediation hint for a rejected cache path
#'
#' dsprrr no longer repairs stored permissions before reusing a cache, so the
#' reported reason has to say what to run instead.
#' @noRd
cache_chmod_remedy <- function(mode, path) {
  paste0(
    "restrict it yourself, then retry: chmod ",
    mode,
    " ",
    shQuote(path)
  )
}

#' Abort a guarded operation after recording its trust failure
#' @noRd
cache_abort_disk_guard <- function(guard, reason) {
  cache_record_disk_guard_failure(guard, reason)
  cli::cli_abort(
    "Disk cache trust verification failed: {reason}",
    class = "dsprrr_cache_trust_error"
  )
}

#' Verify a guard before touching a cache path
#' @noRd
cache_assert_disk_guard <- function(guard) {
  if (!is.environment(guard)) {
    return(invisible(TRUE))
  }
  if (isTRUE(guard$invalid)) {
    cache_abort_disk_guard(guard, guard$reason %||% "the cache was invalidated")
  }
  reason <- cache_directory_trust_error(guard$trust)
  if (!is.null(reason)) {
    cache_abort_disk_guard(guard, reason)
  }
  invisible(TRUE)
}

#' Inspect filesystem entry type without following symlinks
#' @noRd
cache_path_type <- function(path) {
  info <- tryCatch(
    suppressWarnings(fs::file_info(path, follow = FALSE, fail = FALSE)),
    error = function(e) NULL
  )
  if (is.null(info) || nrow(info) != 1L || is.na(info$type[[1]])) {
    return(NA_character_)
  }
  as.character(info$type[[1]])
}

#' Whether a path is a symbolic link
#' @noRd
cache_path_is_symlink <- function(path) {
  identical(cache_path_type(path), "symlink")
}

#' Whether a path is a regular file rather than a device, FIFO, or socket
#' @noRd
cache_path_is_regular <- function(path) {
  identical(cache_path_type(path), "file")
}

#' Read all POSIX permission and special bits for one path
#' @noRd
cache_path_mode <- function(path) {
  info <- suppressWarnings(file.info(path, extra_cols = FALSE))
  if (nrow(info) != 1L || is.na(info$mode[[1]])) {
    return(NA_integer_)
  }
  bitwAnd(as.integer(info$mode[[1]]), as.integer(as.octmode("7777")))
}

#' Verify one path has exactly the requested POSIX mode
#' @noRd
cache_mode_is <- function(path, mode) {
  identical(cache_path_mode(path), as.integer(as.octmode(mode)))
}

#' Set and verify an exact POSIX mode without consulting process umask
#' @noRd
cache_set_private_mode <- function(paths, mode) {
  if (length(paths) == 0L) {
    return(TRUE)
  }
  changed <- tryCatch(
    suppressWarnings(Sys.chmod(paths, mode = mode, use_umask = FALSE)),
    error = function(e) rep(FALSE, length(paths))
  )
  if (length(changed) != length(paths) || anyNA(changed) || !all(changed)) {
    return(FALSE)
  }
  all(vapply(paths, cache_mode_is, logical(1), mode = mode))
}

#' Inspect a cache directory itself before listing it
#' @noRd
audit_private_cache_directory <- function(disk_path) {
  if (cache_path_is_symlink(disk_path)) {
    return(list(
      ok = FALSE,
      reason = "the cache directory is a symbolic link"
    ))
  }
  if (!dir.exists(disk_path)) {
    return(list(ok = FALSE, reason = "the cache path is not a directory"))
  }

  if (!cache_private_modes_supported()) {
    return(list(ok = TRUE))
  }

  effective_owner <- cache_effective_owner_id()
  owner <- cache_path_owner_id(disk_path)
  if (is.na(effective_owner) || is.na(owner)) {
    return(list(
      ok = FALSE,
      reason = "the cache directory owner could not be verified"
    ))
  }
  if (owner != effective_owner) {
    return(list(
      ok = FALSE,
      reason = "the cache directory is not owned by the effective user"
    ))
  }

  mode <- cache_path_mode(disk_path)
  if (is.na(mode)) {
    return(list(
      ok = FALSE,
      reason = "existing cache directory permissions could not be inspected"
    ))
  }
  group_or_other_write <- as.integer(as.octmode("0022"))
  if (bitwAnd(mode, group_or_other_write) != 0L) {
    return(list(
      ok = FALSE,
      reason = paste0(
        "the cache was writable by another local account; ",
        "existing RDS responses are untrusted"
      )
    ))
  }

  if (mode != as.integer(as.octmode("0700"))) {
    return(list(
      ok = FALSE,
      reason = paste0(
        cache_exact_mode_reason(mode, "0700", "cache directory"),
        "; ",
        cache_chmod_remedy("700", disk_path)
      )
    ))
  }

  list(ok = TRUE)
}

#' Enumerate a cache directory and detect silent permission failures
#' @noRd
list_private_cache_entries <- function(disk_path) {
  marker <- tempfile(
    pattern = ".dsprrr-audit-marker-",
    tmpdir = disk_path
  )
  on.exit(unlink(marker, force = TRUE), add = TRUE)

  created <- tryCatch(
    suppressWarnings(file.create(marker, showWarnings = FALSE)),
    error = function(e) FALSE
  )
  if (!isTRUE(created)) {
    return(list(
      ok = FALSE,
      reason = "the cache directory could not be enumerated safely"
    ))
  }
  if (
    cache_private_modes_supported() &&
      !cache_set_private_mode(marker, "0600")
  ) {
    return(list(
      ok = FALSE,
      reason = "the cache audit marker could not be made owner-only"
    ))
  }
  if (
    cache_private_modes_supported() &&
      !cache_paths_owned_by_effective_user(marker)
  ) {
    return(list(
      ok = FALSE,
      reason = "the cache audit marker owner could not be verified"
    ))
  }

  entries <- tryCatch(
    suppressWarnings(list.files(
      disk_path,
      all.files = TRUE,
      full.names = TRUE,
      no.. = TRUE
    )),
    error = function(e) NULL
  )
  marker_seen <- !is.null(entries) &&
    sum(basename(entries) == basename(marker)) == 1L
  if (!marker_seen) {
    return(list(
      ok = FALSE,
      reason = "the cache directory could not be enumerated completely"
    ))
  }

  entries <- entries[basename(entries) != basename(marker)]
  if (unlink(marker, force = TRUE) != 0L) {
    return(list(
      ok = FALSE,
      reason = "the cache audit marker could not be removed"
    ))
  }
  list(ok = TRUE, entries = entries)
}

#' Inspect every existing cache entry before reading serialized values
#' @noRd
audit_private_cache_entries <- function(disk_path) {
  listed <- list_private_cache_entries(disk_path)
  if (!isTRUE(listed$ok)) {
    return(list(ok = FALSE, reason = listed$reason))
  }
  entries <- listed$entries

  if (any(vapply(entries, cache_path_is_symlink, logical(1)))) {
    return(list(
      ok = FALSE,
      reason = "the cache contains a symbolic link"
    ))
  }
  regular <- vapply(entries, cache_path_is_regular, logical(1))
  if (!all(regular)) {
    return(list(
      ok = FALSE,
      reason = "the cache contains a non-regular filesystem entry"
    ))
  }

  if (!cache_private_modes_supported()) {
    return(list(ok = TRUE, files = entries))
  }

  if (!cache_paths_owned_by_effective_user(entries)) {
    return(list(
      ok = FALSE,
      reason = "the cache contains files not owned by the effective user"
    ))
  }

  modes <- vapply(entries, cache_path_mode, integer(1))
  if (anyNA(modes)) {
    return(list(
      ok = FALSE,
      reason = "existing cache permissions could not be inspected"
    ))
  }

  group_or_other_write <- as.integer(as.octmode("0022"))
  if (any(bitwAnd(modes, group_or_other_write) != 0L)) {
    return(list(
      ok = FALSE,
      reason = paste0(
        "the cache was writable by another local account; ",
        "existing RDS responses are untrusted"
      )
    ))
  }

  if (any(modes != as.integer(as.octmode("0600")))) {
    return(list(
      ok = FALSE,
      reason = local({
        offender <- which(modes != as.integer(as.octmode("0600")))[[1]]
        paste0(
          cache_exact_mode_reason(modes[[offender]], "0600", "cache file"),
          "; ",
          cache_chmod_remedy("600", entries[[offender]])
        )
      })
    ))
  }

  list(ok = TRUE, files = entries)
}

#' Create one cache directory without changing parent permissions
#' @noRd
create_cache_directory <- function(
  disk_path,
  private = TRUE,
  must_create = FALSE
) {
  if (dir.exists(disk_path)) {
    return(!isTRUE(must_create))
  }
  mode <- if (isTRUE(private)) "0700" else "0777"
  tryCatch(
    {
      created <- dir.create(
        disk_path,
        recursive = TRUE,
        showWarnings = FALSE,
        mode = mode
      )
      dir.exists(disk_path) && (!isTRUE(must_create) || isTRUE(created))
    },
    error = function(e) FALSE
  )
}

#' Inspect one private cache file without reading serialized content
#' @noRd
cache_private_file_identity <- function(file, trust) {
  parent <- tryCatch(
    as.character(fs::path_real(dirname(file))),
    error = function(e) NULL
  )
  if (is.null(parent) || !identical(parent, trust$path)) {
    return(list(
      ok = FALSE,
      reason = "a cache file escaped its trusted directory"
    ))
  }
  if (!file.exists(file)) {
    return(list(
      ok = FALSE,
      missing = TRUE,
      reason = "the cache file is missing"
    ))
  }
  if (cache_path_is_symlink(file) || !cache_path_is_regular(file)) {
    return(list(ok = FALSE, reason = "a cache entry is not a regular file"))
  }
  owner <- cache_path_owner_id(file)
  effective_owner <- cache_effective_owner_id()
  mode <- cache_path_mode(file)
  info <- tryCatch(
    suppressWarnings(fs::file_info(file, follow = FALSE, fail = FALSE)),
    error = function(e) NULL
  )
  file_identity <- file_identity_exact(file)
  if (
    is.na(owner) ||
      is.na(effective_owner) ||
      owner != effective_owner ||
      is.na(mode) ||
      mode != as.integer(as.octmode("0600")) ||
      is.null(info) ||
      nrow(info) != 1L ||
      is.null(file_identity)
  ) {
    return(list(
      ok = FALSE,
      reason = "a cache entry is not an effective-user-owned 0600 file"
    ))
  }
  if (!identical(file_identity$device_id, trust$device_id)) {
    return(list(
      ok = FALSE,
      reason = "a cache entry is on an unexpected device"
    ))
  }
  list(
    ok = TRUE,
    identity = list(
      device_id = file_identity$device_id,
      inode = file_identity$inode,
      owner_id = owner,
      mode = mode,
      size = as.numeric(info$size[[1]]),
      modification_time = as.numeric(info$modification_time[[1]]),
      change_time = as.numeric(info$change_time[[1]])
    )
  )
}

#' Read one serialized value through the private disk trust boundary
#' @noRd
read_private_cache_value <- function(file, guard) {
  cache_assert_disk_guard(guard)
  before <- cache_private_file_identity(file, guard$trust)
  if (isTRUE(before$missing)) {
    stop("Cache entry is missing")
  }
  if (!isTRUE(before$ok)) {
    cache_abort_disk_guard(guard, before$reason)
  }
  value <- tryCatch(
    readRDS(file),
    error = function(error) {
      cache_abort_disk_guard(
        guard,
        paste0(
          "a verified cache entry could not be read: ",
          conditionMessage(error)
        )
      )
    }
  )
  cache_assert_disk_guard(guard)
  after <- cache_private_file_identity(file, guard$trust)
  if (!isTRUE(after$ok) || !identical(after$identity, before$identity)) {
    cache_abort_disk_guard(
      guard,
      "a cache entry changed identity while it was being read"
    )
  }
  value
}

#' Mockable serialization boundary for private cache writes
#' @noRd
cache_save_rds <- function(value, file) {
  saveRDS(value, file)
}

#' Atomically write one owner-only serialized cache value on Unix
#' @noRd
write_private_cache_value <- function(value, file, guard = NULL) {
  if (is.environment(guard)) {
    cache_assert_disk_guard(guard)
    if (file.exists(file)) {
      existing <- cache_private_file_identity(file, guard$trust)
      if (!isTRUE(existing$ok)) {
        cache_abort_disk_guard(guard, existing$reason)
      }
    }
  }
  temp_file <- tempfile(
    pattern = paste0(".", basename(file), "-temp-"),
    tmpdir = dirname(file)
  )
  on.exit(unlink(temp_file, force = TRUE), add = TRUE)

  old_umask <- Sys.umask("0077")
  on.exit(Sys.umask(old_umask), add = TRUE)
  created <- tryCatch(
    suppressWarnings(file.create(temp_file, showWarnings = FALSE)),
    error = function(e) FALSE
  )
  Sys.umask(old_umask)
  if (
    !isTRUE(created) ||
      !cache_set_private_mode(temp_file, "0600") ||
      !cache_paths_owned_by_effective_user(temp_file) ||
      cache_path_is_symlink(temp_file) ||
      !cache_path_is_regular(temp_file)
  ) {
    stop("Could not pre-create an effective-user-owned 0600 cache file")
  }

  # No sensitive bytes are serialized until the staging file is verified.
  staging_before <- if (is.environment(guard)) {
    cache_private_file_identity(temp_file, guard$trust)
  } else {
    NULL
  }
  cache_save_rds(value, temp_file)
  if (
    !cache_mode_is(temp_file, "0600") ||
      !cache_paths_owned_by_effective_user(temp_file) ||
      !cache_path_is_regular(temp_file)
  ) {
    stop("Could not verify the private cache staging file")
  }
  if (is.environment(guard)) {
    staging_after <- cache_private_file_identity(temp_file, guard$trust)
    if (
      !isTRUE(staging_before$ok) ||
        !isTRUE(staging_after$ok) ||
        !identical(
          staging_before$identity$device_id,
          staging_after$identity$device_id
        ) ||
        !identical(
          staging_before$identity$inode,
          staging_after$identity$inode
        ) ||
        !identical(
          staging_before$identity$owner_id,
          staging_after$identity$owner_id
        ) ||
        !identical(staging_before$identity$mode, staging_after$identity$mode)
    ) {
      cache_abort_disk_guard(
        guard,
        "the private cache staging file changed identity during serialization"
      )
    }
  }
  if (is.environment(guard)) {
    cache_assert_disk_guard(guard)
  }
  if (!isTRUE(file.rename(temp_file, file))) {
    stop("Could not atomically install a cache file")
  }
  if (is.environment(guard)) {
    cache_assert_disk_guard(guard)
    installed <- cache_private_file_identity(file, guard$trust)
    installed_ok <- isTRUE(installed$ok)
  } else {
    installed_ok <- cache_mode_is(file, "0600") &&
      cache_paths_owned_by_effective_user(file) &&
      cache_path_is_regular(file)
  }
  if (!installed_ok) {
    unlink(file, force = TRUE)
    stop("Could not verify the installed private cache file")
  }
  invisible(NULL)
}

#' Emit one degradation warning for a sticky disk guard failure
#' @noRd
cache_report_disk_guard_failure <- function(guard) {
  if (!is.environment(guard) || !isTRUE(guard$invalid)) {
    return(FALSE)
  }
  if (!identical(.dsprrr_env$cache_disk_guard, guard)) {
    return(TRUE)
  }
  # Detach before emitting diagnostics: options(warn = 2) may promote the
  # warning below to an error, but must not interrupt global state cleanup.
  .dsprrr_env$cache_disk_guard <- NULL
  if (!isTRUE(guard$warning_emitted)) {
    guard$warning_emitted <- TRUE
    cache_disk_degrade(guard$trust$path, guard$reason)
  }
  TRUE
}

#' Guard a cachem disk object, including the stale handle retained by layers
#' @noRd
guarded_cache_disk <- function(cache, guard) {
  guarded_call <- function(method, ..., missing_on_failure = FALSE) {
    if (isTRUE(guard$destroyed)) {
      stop("Attempted to use a disk cache which has been destroyed")
    }
    if (isTRUE(guard$invalid)) {
      cache_report_disk_guard_failure(guard)
      if (missing_on_failure) {
        return(cachem::key_missing())
      }
      return(invisible(FALSE))
    }
    value <- tryCatch(
      {
        cache_assert_disk_guard(guard)
        cache[[method]](...)
      },
      error = function(error) {
        if (!isTRUE(guard$invalid)) {
          cache_record_disk_guard_failure(guard, conditionMessage(error))
        }
        NULL
      }
    )
    if (isTRUE(guard$invalid)) {
      cache_report_disk_guard_failure(guard)
      if (missing_on_failure) {
        return(cachem::key_missing())
      }
      return(invisible(FALSE))
    }
    if (identical(method, "destroy")) {
      guard$destroyed <- TRUE
    }
    value
  }

  structure(
    list(
      get = function(key, ...) {
        guarded_call("get", key, ..., missing_on_failure = TRUE)
      },
      set = function(key, value) guarded_call("set", key, value),
      exists = function(key) guarded_call("exists", key),
      keys = function() guarded_call("keys"),
      remove = function(key) guarded_call("remove", key),
      reset = function() guarded_call("reset"),
      prune = function() guarded_call("prune"),
      size = function() guarded_call("size"),
      destroy = function() guarded_call("destroy"),
      is_destroyed = cache$is_destroyed,
      info = cache$info
    ),
    class = c("dsprrr_guarded_cache_disk", "cache_disk", "cachem")
  )
}

#' Reject cleanup through a disk handle whose path identity changed
#' @noRd
cache_verify_disk_cleanup_guard <- function(guard) {
  if (is.environment(guard)) {
    cache_assert_disk_guard(guard)
  }
  invisible(TRUE)
}

#' Verify the filesystem supports private response-file creation
#' @noRd
verify_private_cache_write <- function(disk_path) {
  probe <- tempfile(
    pattern = ".dsprrr-permission-probe-",
    tmpdir = disk_path
  )
  on.exit(unlink(probe, force = TRUE), add = TRUE)
  tryCatch(
    {
      write_private_cache_value(NULL, probe)
      cache_mode_is(probe, "0600") && unlink(probe, force = TRUE) == 0L
    },
    error = function(e) FALSE
  )
}

#' Prepare a cache directory and establish its privacy status
#' @noRd
prepare_cache_directory <- function(disk_path, private = TRUE) {
  if (!rlang::is_string(disk_path) || !nzchar(disk_path)) {
    return(list(
      ok = FALSE,
      path = "<invalid>",
      reason = "the configured disk path is not one non-empty string"
    ))
  }
  disk_path <- path.expand(disk_path)

  if (file.exists(disk_path) && !dir.exists(disk_path)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "the configured disk path is not a directory"
    ))
  }
  if (cache_path_is_symlink(disk_path)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "the cache directory is a symbolic link"
    ))
  }
  canonical_target <- cache_canonical_target_path(disk_path)
  if (is.null(canonical_target)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "the cache path could not be resolved canonically"
    ))
  }
  disk_path <- canonical_target
  directory_existed <- dir.exists(disk_path)

  if (!isTRUE(private)) {
    if (!create_cache_directory(disk_path, private = FALSE)) {
      return(list(
        ok = FALSE,
        path = disk_path,
        reason = "the cache directory could not be created"
      ))
    }
    .dsprrr_env$cache_privacy_status <- "disabled"
    .dsprrr_env$cache_privacy_reason <- paste0(
      "owner-only enforcement was disabled; all cache users must be trusted"
    )
    return(list(ok = TRUE, path = disk_path))
  }

  directory_audit <- if (directory_existed) {
    audit_private_cache_directory(disk_path)
  } else {
    list(ok = TRUE)
  }
  if (!isTRUE(directory_audit$ok)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = directory_audit$reason
    ))
  }

  pre_create_parent_audit <- audit_existing_cache_parent_capability(disk_path)
  if (!isTRUE(pre_create_parent_audit$ok)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = pre_create_parent_audit$reason
    ))
  }

  if (
    !create_cache_directory(
      disk_path,
      private = TRUE,
      must_create = !directory_existed
    )
  ) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "the cache directory could not be created"
    ))
  }

  if (
    !directory_existed &&
      cache_private_modes_supported() &&
      !cache_set_private_mode(disk_path, "0700")
  ) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "new private cache directory permissions could not be verified"
    ))
  }

  # Re-check directory trust after creation so a concurrent creator cannot
  # populate an externally writable path between the checks above.
  post_create_directory_audit <- audit_private_cache_directory(disk_path)
  if (!isTRUE(post_create_directory_audit$ok)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = post_create_directory_audit$reason
    ))
  }

  parent_audit <- audit_cache_parent_chain(disk_path)
  if (!isTRUE(parent_audit$ok)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = parent_audit$reason
    ))
  }

  entry_audit <- audit_private_cache_entries(disk_path)
  if (!isTRUE(entry_audit$ok)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = entry_audit$reason
    ))
  }

  if (!cache_private_modes_supported()) {
    .dsprrr_env$cache_privacy_status <- "unverified_windows"
    .dsprrr_env$cache_privacy_reason <- paste0(
      "the cache inherits filesystem ACLs that base R cannot verify"
    )
    return(list(ok = TRUE, path = disk_path, trust = NULL))
  }

  if (!verify_private_cache_write(disk_path)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "owner-only cache write permissions could not be verified"
    ))
  }

  # The write probe mutates the directory. Repeat every audit, then bind future
  # operations to the final canonical identity.
  final_directory_audit <- audit_private_cache_directory(disk_path)
  final_parent_audit <- audit_cache_parent_chain(disk_path)
  final_entry_audit <- audit_private_cache_entries(disk_path)
  trust <- cache_directory_identity(disk_path)
  if (
    !isTRUE(final_directory_audit$ok) ||
      !isTRUE(final_parent_audit$ok) ||
      !isTRUE(final_entry_audit$ok) ||
      is.null(trust) ||
      trust$mode != as.integer(as.octmode("0700")) ||
      trust$owner_id != cache_effective_owner_id()
  ) {
    reason <- final_directory_audit$reason %||%
      final_parent_audit$reason %||%
      final_entry_audit$reason %||%
      "the final cache directory identity could not be verified"
    return(list(ok = FALSE, path = disk_path, reason = reason))
  }

  .dsprrr_env$cache_privacy_status <- "verified_posix_modes"
  .dsprrr_env$cache_privacy_reason <- paste0(
    "effective ownership and POSIX modes were verified; extended ACLs were not checked"
  )

  list(ok = TRUE, path = disk_path, trust = trust)
}

#' Record a disk-cache failure and fall back to memory-only operation
#' @noRd
cache_disk_degrade <- function(disk_path, reason) {
  memory_available <- isTRUE(.dsprrr_env$cache_config$enable_memory) &&
    !is.null(.dsprrr_env$cache_memory)
  .dsprrr_env$cache <- if (memory_available) {
    .dsprrr_env$cache_memory
  } else {
    NULL
  }
  .dsprrr_env$cache_disk <- NULL
  .dsprrr_env$cache_degraded <- TRUE
  .dsprrr_env$cache_degraded_reason <- reason
  .dsprrr_env$cache_privacy_status <- "degraded"
  .dsprrr_env$cache_privacy_reason <- reason
  fallback <- if (memory_available) {
    "Falling back to memory-only caching for this session."
  } else {
    "No cache tier remains enabled for this session."
  }

  cli::cli_warn(
    c(
      "!" = "Disk caching is unavailable at {.path {disk_path}}",
      "x" = "{reason}",
      "i" = "{fallback}"
    ),
    class = c("dsprrr_cache_security_warning", "dsprrr_cache_warning"),
    .frequency = "once",
    .frequency_id = paste0(
      "cache-disk-degraded-",
      digest::digest(c(disk_path, reason), serialize = FALSE)
    )
  )
  NULL
}
