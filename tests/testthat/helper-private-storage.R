# A private child cannot make a writable, non-sticky ancestor trustworthy.
# Keep persistence fixtures inside the R session's temporary directory.
local_private_storage_dir <- function(.local_envir = parent.frame()) {
  path <- tempfile(tmpdir = normalizePath(tempdir()))
  capability <- dsprrr:::audit_existing_cache_parent_capability(path)
  if (
    !capability$ok &&
      startsWith(capability$reason, "a non-sticky cache ancestor is writable:")
  ) {
    testthat::skip(capability$reason)
  }

  stopifnot(dir.create(path, mode = "0700"))
  withr::defer(unlink(path, recursive = TRUE), envir = .local_envir)
  path
}

checkpoint_fixture <- function(path, control = NULL) {
  program <- module(signature("x -> y"))
  metric <- metric_exact_match(field = "y")
  data <- data.frame(x = c("a", "b"), y = c("a", "b"))
  if (is.null(control)) {
    control <- optimizer_control(checkpoint_path = path)
  }
  context <- optimizer_checkpoint_begin(
    "FixtureOptimizer",
    1L,
    program,
    data,
    metric,
    config = list(population_size = 2L),
    control = control,
    initial_state = list(index = 0L)
  )
  list(
    context = context,
    program = program,
    metric = metric,
    data = data
  )
}
