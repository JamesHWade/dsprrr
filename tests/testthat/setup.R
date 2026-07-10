# Isolate the on-disk cache during the test suite.
#
# Without this, the default disk cache writes into the working directory
# (tests/testthat/.dsprrr_cache during R CMD check), which pollutes the package
# source tree and leaks state across runs -- a documented source of flaky tests.
#
# We redirect the cache to a temporary directory via DSPRRR_CACHE_PATH (so even
# bare configure_cache() calls land there) and disable the disk tier by default.
# Cache-behavior tests that need disk caching opt back in with their own
# tempdirs, which take precedence over this default.

dsprrr_test_cache_dir <- file.path(tempdir(), "dsprrr-test-cache")
dir.create(dsprrr_test_cache_dir, showWarnings = FALSE, recursive = TRUE)

withr::local_envvar(
  DSPRRR_CACHE_PATH = dsprrr_test_cache_dir,
  .local_envir = testthat::teardown_env()
)

# vitals otherwise warns once per Task construction when no user-level log
# directory is configured. Tests must not depend on or write to user state.
dsprrr_test_vitals_dir <- file.path(tempdir(), "dsprrr-vitals-logs")
dir.create(dsprrr_test_vitals_dir, showWarnings = FALSE, recursive = TRUE)
withr::local_envvar(
  VITALS_LOG_DIR = dsprrr_test_vitals_dir,
  .local_envir = testthat::teardown_env()
)

dsprrr::configure_cache(
  enable_disk = FALSE,
  disk_path = dsprrr_test_cache_dir
)

withr::defer(
  {
    dsprrr::clear_cache()
    unlink(dsprrr_test_cache_dir, recursive = TRUE)
    unlink(dsprrr_test_vitals_dir, recursive = TRUE)
    # Restore the production default explicitly. teardown defers run LIFO, so
    # DSPRRR_CACHE_PATH set by local_envvar() above is still in scope here; a
    # bare configure_cache() would otherwise re-read it and point disk_path at
    # the just-unlinked test dir.
    dsprrr::configure_cache(disk_path = ".dsprrr_cache")
  },
  envir = testthat::teardown_env()
)
