# Package loading for real subprocess tests

# `pkgload::load_all()` is appropriate when tests run from a source checkout,
# but the relative source path does not exist when `R CMD check` runs tests
# against the installed package. Carry the parent process library paths into
# callr and select the matching loading strategy there.
callr_dsprrr_context <- function() {
  package_path <- normalizePath(
    getNamespaceInfo(asNamespace("dsprrr"), "path"),
    mustWork = TRUE
  )

  list(
    package_path = package_path,
    lib_paths = .libPaths()
  )
}

callr_load_dsprrr <- function(context) {
  source_marker <- file.path(context$package_path, "R", "optimizer-core.R")
  if (file.exists(source_marker)) {
    pkgload::load_all(context$package_path, quiet = TRUE)
  } else {
    library_path <- dirname(context$package_path)
    .libPaths(unique(c(library_path, context$lib_paths)))
    loadNamespace("dsprrr", lib.loc = library_path)
  }
  asNamespace("dsprrr")
}

callr_readiness_timeout <- 60

callr_wait_for_files <- function(
  processes,
  files,
  timeout = callr_readiness_timeout
) {
  deadline <- Sys.time() + timeout
  repeat {
    ready <- file.exists(files)
    if (all(ready)) {
      return(invisible(NULL))
    }

    for (i in which(!ready)) {
      process <- processes[[i]]
      if (!process$is_alive()) {
        process$get_result()
        stop(
          "Subprocess exited before signaling readiness for ",
          files[[i]],
          call. = FALSE
        )
      }
    }

    remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (remaining <= 0) {
      stop(
        "Timed out after ",
        timeout,
        " seconds waiting for subprocess readiness: ",
        paste(files[!ready], collapse = ", "),
        call. = FALSE
      )
    }

    Sys.sleep(min(0.01, remaining))
  }
}
