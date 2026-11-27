# Read artifacts and publish files with checked filesystem identity.

artifact_atomic_write_file <- function(
  path,
  writer,
  verifier,
  what,
  dir_label,
  write_error_msg
) {
  path <- artifact_validate_path(path)
  directory <- dirname(path)
  if (!dir.exists(directory)) {
    cli::cli_abort(
      "{dir_label} directory does not exist: {.path {directory}}",
      class = "dsprrr_artifact_io_error"
    )
  }
  temporary <- artifact_private_stage(path)
  on.exit(unlink(temporary), add = TRUE)

  tryCatch(
    writer(temporary),
    error = function(e) {
      cli::cli_abort(
        write_error_msg,
        parent = e,
        class = "dsprrr_artifact_io_error"
      )
    }
  )
  verifier(temporary)
  artifact_atomic_replace(temporary, path, what = what)
  invisible(path)
}

artifact_atomic_save_rds <- function(value, path) {
  artifact_atomic_write_file(
    path = path,
    writer = function(temporary) artifact_write_rds(value, temporary),
    verifier = function(temporary) {
      staged <- tryCatch(
        readRDS(temporary),
        error = function(e) {
          cli::cli_abort(
            "Could not verify staged program artifact",
            parent = e,
            class = "dsprrr_artifact_io_error"
          )
        }
      )
      artifact_validate_manifest(staged)
    },
    what = "program artifact",
    dir_label = "Artifact",
    write_error_msg = "Could not write program artifact"
  )
}

artifact_atomic_write_lines <- function(lines, path) {
  artifact_atomic_write_file(
    path = path,
    writer = function(temporary) artifact_write_lines(lines, temporary),
    verifier = function(temporary) {
      parsed <- tryCatch(
        parse(temporary),
        error = function(e) {
          cli::cli_abort(
            "Generated module code did not parse",
            parent = e,
            class = "dsprrr_artifact_io_error"
          )
        }
      )
      if (length(parsed) == 0L) {
        cli::cli_abort(
          "Generated module code was empty",
          class = "dsprrr_artifact_io_error"
        )
      }
    },
    what = "exported code",
    dir_label = "Output",
    write_error_msg = "Could not write exported module code"
  )
}

artifact_private_stage <- function(path) {
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
  created <- file.create(temporary, showWarnings = FALSE)
  private <- created && isTRUE(Sys.chmod(temporary, mode = "0600"))
  if (private && .Platform$OS.type == "unix") {
    private <- identical(
      as.character(as.octmode(file.info(temporary)$mode)),
      "600"
    )
  }
  if (!private) {
    unlink(temporary)
    cli::cli_abort(
      "Could not create a private staging file for {.path {path}}",
      class = "dsprrr_artifact_io_error"
    )
  }
  temporary
}

artifact_write_rds <- function(value, path) {
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  saveRDS(value, connection, version = 3)
  invisible(path)
}

artifact_write_lines <- function(lines, path) {
  connection <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  writeLines(lines, connection, useBytes = TRUE)
  invisible(path)
}

artifact_atomic_identity <- function(path) {
  info <- tryCatch(
    suppressWarnings(fs::file_info(path, follow = FALSE, fail = FALSE)),
    error = function(e) NULL
  )
  file_identity <- file_identity_exact(path)
  if (
    is.null(info) ||
      nrow(info) != 1L ||
      !all(
        c("type", "size", "modification_time") %in% names(info)
      ) ||
      is.na(info$type[[1L]]) ||
      !identical(as.character(info$type[[1L]]), "file") ||
      is.null(file_identity)
  ) {
    return(NULL)
  }
  identity <- list(
    device_id = file_identity$device_id,
    inode = file_identity$inode,
    size = suppressWarnings(as.numeric(info$size[[1L]])),
    modification_time = suppressWarnings(
      as.numeric(info$modification_time[[1L]])
    )
  )
  if (any(lengths(identity) != 1L)) {
    return(NULL)
  }
  if (!all(is.finite(unlist(identity[c("size", "modification_time")])))) {
    return(NULL)
  }
  identity
}

artifact_atomic_same_file <- function(left, right) {
  !is.null(left) &&
    !is.null(right) &&
    identical(left$device_id, right$device_id) &&
    identical(left$inode, right$inode)
}

artifact_file_hold <- function(path) {
  if (.Platform$OS.type != "unix") {
    return(NULL)
  }
  file(path, open = "rb")
}

artifact_file_hold_release <- function(connection) {
  if (inherits(connection, "connection") && isOpen(connection)) {
    close(connection)
  }
  invisible(NULL)
}

artifact_file_move <- function(source, destination) {
  fs::file_move(source, destination)
  invisible(destination)
}

artifact_atomic_replace <- function(source, destination, what) {
  requested_destination <- destination
  source <- artifact_validate_path(source)
  destination <- artifact_validate_path(destination)
  source_absolute <- tryCatch(
    as.character(fs::path_abs(path.expand(source))),
    error = function(e) NULL
  )
  destination_absolute <- tryCatch(
    as.character(fs::path_abs(path.expand(destination))),
    error = function(e) NULL
  )
  source_parent <- tryCatch(
    as.character(fs::path_real(dirname(source_absolute))),
    error = function(e) NULL
  )
  destination_parent <- tryCatch(
    as.character(fs::path_real(dirname(destination_absolute))),
    error = function(e) NULL
  )
  if (
    is.null(source_absolute) ||
      is.null(destination_absolute) ||
      is.null(source_parent) ||
      is.null(destination_parent) ||
      !identical(source_parent, destination_parent)
  ) {
    cli::cli_abort(
      c(
        "Could not atomically publish {what} at {.path {destination}}",
        "x" = "The staging file and destination are not in one canonical directory."
      ),
      class = "dsprrr_artifact_io_error"
    )
  }
  source <- file.path(source_parent, basename(source_absolute))
  destination <- file.path(destination_parent, basename(destination_absolute))
  if (identical(source, destination)) {
    cli::cli_abort(
      "Could not atomically publish {what}: staging and destination paths are identical",
      class = "dsprrr_artifact_io_error"
    )
  }

  source_identity <- artifact_atomic_identity(source)
  if (is.null(source_identity)) {
    cli::cli_abort(
      "Could not verify the {what} staging file at {.path {source}}",
      class = "dsprrr_artifact_io_error"
    )
  }
  source_hold <- tryCatch(
    artifact_file_hold(source),
    error = function(e) e
  )
  on.exit(artifact_file_hold_release(source_hold), add = TRUE)
  if (inherits(source_hold, "condition")) {
    cli::cli_abort(
      "Could not hold the {what} staging identity",
      parent = source_hold,
      class = "dsprrr_artifact_io_error"
    )
  }

  destination_exists <- file.exists(destination)
  destination_identity <- NULL
  destination_hold <- NULL
  on.exit(artifact_file_hold_release(destination_hold), add = TRUE)
  if (destination_exists || cache_path_is_symlink(destination)) {
    destination_identity <- artifact_atomic_identity(destination)
    if (is.null(destination_identity)) {
      cli::cli_abort(
        "Could not verify the existing {what} destination at {.path {destination}}",
        class = "dsprrr_artifact_io_error"
      )
    }
    if (artifact_atomic_same_file(source_identity, destination_identity)) {
      cli::cli_abort(
        "Could not atomically publish {what}: staging and destination paths reference the same existing file",
        class = "dsprrr_artifact_io_error"
      )
    }
    destination_hold <- tryCatch(
      artifact_file_hold(destination),
      error = function(e) e
    )
    if (inherits(destination_hold, "condition")) {
      cli::cli_abort(
        "Could not hold the existing {what} destination identity",
        parent = destination_hold,
        class = "dsprrr_artifact_io_error"
      )
    }
  }

  source_current <- artifact_atomic_identity(source)
  destination_current <- if (destination_exists) {
    artifact_atomic_identity(destination)
  } else {
    NULL
  }
  if (!identical(source_current, source_identity)) {
    cli::cli_abort(
      "Could not atomically publish {what}: the staging file identity changed",
      class = "dsprrr_artifact_io_error"
    )
  }
  if (
    destination_exists &&
      !identical(destination_current, destination_identity)
  ) {
    cli::cli_abort(
      "Could not atomically publish {what}: the destination identity changed",
      class = "dsprrr_artifact_io_error"
    )
  }
  if (
    !destination_exists &&
      (file.exists(destination) || cache_path_is_symlink(destination))
  ) {
    cli::cli_abort(
      "Could not atomically publish {what}: the destination appeared",
      class = "dsprrr_artifact_io_error"
    )
  }

  artifact_file_hold_release(source_hold)
  source_hold <- NULL
  artifact_file_hold_release(destination_hold)
  destination_hold <- NULL
  failure <- tryCatch(
    {
      artifact_file_move(source, destination)
      NULL
    },
    error = function(e) e
  )
  if (!is.null(failure)) {
    cli::cli_abort(
      c(
        "Could not atomically publish {what} at {.path {destination}}",
        "i" = if (destination_exists) {
          "The existing destination was left unchanged."
        } else {
          "No destination was published."
        }
      ),
      parent = failure,
      class = "dsprrr_artifact_io_error"
    )
  }

  installed_identity <- artifact_atomic_identity(destination)
  if (
    file.exists(source) ||
      cache_path_is_symlink(source) ||
      !identical(installed_identity, source_identity)
  ) {
    cli::cli_abort(
      "Could not verify the atomically published {what} at {.path {destination}}",
      class = "dsprrr_artifact_io_error"
    )
  }
  invisible(requested_destination)
}

artifact_validate_path <- function(path) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    cli::cli_abort("{.arg path} must be one non-empty file path")
  }
  path
}

artifact_read_rds <- function(path) {
  path <- artifact_validate_path(path)
  if (!file.exists(path)) {
    cli::cli_abort(
      "Program artifact does not exist: {.path {path}}",
      class = "dsprrr_artifact_io_error"
    )
  }
  tryCatch(
    readRDS(path),
    error = function(e) {
      cli::cli_abort(
        "Could not read program artifact",
        parent = e,
        class = "dsprrr_artifact_malformed"
      )
    }
  )
}
