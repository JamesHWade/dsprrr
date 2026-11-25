# Keep these storage boundary checks in both package-check tiers.

test_that("a writable non-sticky parent forces a disk cache to fall back to memory", {
  skip_on_os("windows")
  local_reset_cache()

  root <- tempfile("cache_parent_safety_")
  withr::defer(unlink(root, recursive = TRUE))
  dir.create(root)
  Sys.chmod(root, mode = "0777", use_umask = FALSE)
  path <- file.path(root, "unsafe-cache")
  configure_cache(enable_memory = TRUE, enable_disk = TRUE, disk_path = path)
  expect_warning(
    cache <- dsprrr:::get_cache(),
    "non-sticky cache ancestor is writable",
    class = "dsprrr_cache_security_warning"
  )
  expect_s3_class(cache, "cache_mem")
  cache$set("example", 42)
  expect_equal(cache$get("example"), 42)
  expect_false(dir.exists(path))
})

test_that("TrialLog rejects creation below writable non-sticky parents", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  unsafe_parent <- file.path(root, "unsafe")
  dir.create(unsafe_parent, mode = "0777")
  Sys.chmod(unsafe_parent, mode = "0777", use_umask = FALSE)

  condition <- rlang::catch_cnd(TrialLog$new(
    "WritableParentOptimizer",
    log_dir = file.path(unsafe_parent, "log")
  ))

  expect_s3_class(condition, "dsprrr_trial_log_trust_error")
  expect_identical(dir.exists(file.path(unsafe_parent, "log")), FALSE)
})

test_that("optimizer checkpoints refuse a private directory below a writable parent", {
  skip_on_os("windows")
  parent <- withr::local_tempdir()
  Sys.chmod(parent, mode = "0777", use_umask = FALSE)
  directory <- file.path(parent, "private")
  dir.create(directory, mode = "0700")
  path <- file.path(directory, "checkpoint.rds")

  expect_error(
    checkpoint_fixture(path),
    "non-sticky cache ancestor is writable",
    class = "dsprrr_optimizer_checkpoint_trust_error"
  )
  expect_false(file.exists(path))
})

test_that("optimizer checkpoints roundtrip through a private atomic manifest", {
  path <- file.path(local_private_storage_dir(), "checkpoint.rds")
  fixture <- checkpoint_fixture(path)
  record_optimizer_usage(
    fixture$context$budget,
    list(trials = 1L, metric_calls = 2L, known_cost = 0.03),
    "trial"
  )
  optimizer_budget_complete_unit(fixture$context$budget, "trial:1")
  set.seed(42)
  expected_rng <- optimizer_checkpoint_capture_rng()

  optimizer_checkpoint_write(
    fixture$context,
    phase = "search",
    search_state = list(index = 1L, scores = 0.75),
    lineage = list(list(child = "candidate:1", parent = "base")),
    best_program = fixture$program
  )

  checkpoint <- optimizer_checkpoint_read(path)
  expect_identical(checkpoint$format, "dsprrr-optimizer-checkpoint")
  expect_identical(checkpoint$format_version, 1L)
  expect_identical(checkpoint$rng, expected_rng)
  expect_identical(checkpoint$progress$phase, "search")
  expect_equal(checkpoint$budget$trials, 1L)
  expect_identical(checkpoint$budget$completed_units, "trial:1")
  expect_silent(artifact_validate_manifest(checkpoint$best_program))
  if (.Platform$OS.type == "unix") {
    expect_identical(as.character(as.octmode(file.info(path)$mode)), "600")
  }

  resumed_control <- optimizer_control(
    checkpoint_path = path,
    resume = TRUE,
    max_trials = 4L
  )
  resumed <- checkpoint_fixture(path, resumed_control)$context
  expect_true(resumed$resumed)
  expect_identical(resumed$phase, "search")
  expect_equal(resumed$search_state$index, 1L)
  expect_equal(optimizer_budget_summary(resumed$budget)$trials, 1L)
  expect_true(inherits(resumed$best_program, "Module"))
})
