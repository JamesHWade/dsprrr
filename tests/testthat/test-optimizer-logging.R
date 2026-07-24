test_that("TrialLog resumes existing JSONL without rewriting records", {
  log_dir <- withr::local_tempdir()
  trials_path <- file.path(log_dir, "trials.jsonl")

  first <- complete_trial(
    create_trial("ResumeOptimizer", trial_id = "trial-1"),
    EvalResult(mean_score = 0.5, n_evaluated = 1L)
  )
  log <- TrialLog$new("ResumeOptimizer", log_dir = log_dir)
  log$add_trial(first)

  first_line <- readLines(trials_path, warn = FALSE)
  before_resume <- readBin(
    trials_path,
    what = "raw",
    n = file.info(trials_path)$size
  )
  resumed <- TrialLog$new("ResumeOptimizer", log_dir = log_dir)

  expect_identical(resumed$n_trials(), 1L)
  expect_identical(
    readBin(trials_path, what = "raw", n = file.info(trials_path)$size),
    before_resume
  )

  second <- complete_trial(
    create_trial("ResumeOptimizer", trial_id = "trial-2"),
    EvalResult(mean_score = 0.75, n_evaluated = 1L)
  )
  resumed$add_trial(second)

  lines <- readLines(trials_path, warn = FALSE)
  expect_identical(length(lines), 2L)
  expect_identical(lines[[1L]], first_line[[1L]])
  expect_identical(
    jsonlite::fromJSON(file.path(log_dir, "metadata.json"))$n_trials,
    2L
  )

  loaded <- load_trial_log(log_dir)
  expect_identical(loaded$n_trials(), 2L)
  expect_identical(length(readLines(trials_path, warn = FALSE)), 2L)
})

test_that("duplicate trial IDs are idempotent or rejected on conflict", {
  log_dir <- withr::local_tempdir()
  trials_path <- file.path(log_dir, "trials.jsonl")
  trial <- complete_trial(
    create_trial("DuplicateOptimizer", trial_id = "stable-id"),
    EvalResult(mean_score = 0.8, n_evaluated = 1L)
  )

  log <- TrialLog$new("DuplicateOptimizer", log_dir = log_dir)
  log$add_trial(trial)
  persisted <- readLines(trials_path, warn = FALSE)
  log$add_trial(trial)

  expect_identical(log$n_trials(), 1L)
  expect_identical(readLines(trials_path, warn = FALSE), persisted)

  conflicting <- Trial(
    trial_id = "stable-id",
    optimizer_name = "DuplicateOptimizer",
    params = list(changed = TRUE),
    start_time = trial@start_time,
    status = "pending"
  )
  condition <- rlang::catch_cnd(log$add_trial(conflicting))

  expect_s3_class(condition, "dsprrr_trial_id_conflict")
  expect_identical(log$n_trials(), 1L)
  expect_identical(readLines(trials_path, warn = FALSE), persisted)
})

test_that("save synchronizes missing trials with append-only persistence", {
  log_dir <- withr::local_tempdir()
  trials_path <- file.path(log_dir, "trials.jsonl")
  log <- TrialLog$new("SaveOptimizer")

  for (id in c("first", "second")) {
    trial <- complete_trial(
      create_trial("SaveOptimizer", trial_id = id),
      EvalResult(mean_score = 0.5, n_evaluated = 1L)
    )
    log$add_trial(trial, persist = FALSE)
  }

  log$save(log_dir)
  initial <- readLines(trials_path, warn = FALSE)
  log$save(log_dir)

  expect_identical(length(initial), 2L)
  expect_identical(readLines(trials_path, warn = FALSE), initial)
  expect_identical(TrialLog$new("SaveOptimizer", log_dir)$n_trials(), 2L)
})

test_that("usage persistence keeps unknown values distinct from zero", {
  log_dir <- withr::local_tempdir()
  log <- TrialLog$new("UsageOptimizer", log_dir = log_dir)

  unknown <- complete_trial(
    create_trial("UsageOptimizer", trial_id = "unknown"),
    EvalResult(
      mean_score = 0.5,
      n_evaluated = 1L,
      input_tokens = NA_integer_,
      output_tokens = NA_integer_,
      total_tokens = NA_integer_,
      total_cost = NA_real_,
      provider_calls = NA_integer_,
      metric_calls = 1L,
      provider_usage_unknown = TRUE,
      token_usage_unknown = TRUE
    )
  )
  zero <- complete_trial(
    create_trial("UsageOptimizer", trial_id = "zero"),
    EvalResult(
      mean_score = 0.5,
      n_evaluated = 1L,
      input_tokens = 0L,
      output_tokens = 0L,
      total_tokens = 0L,
      total_cost = 0,
      provider_calls = 0L,
      metric_calls = 1L,
      provider_usage_unknown = FALSE,
      token_usage_unknown = FALSE
    )
  )
  log$add_trial(unknown)
  log$add_trial(zero)

  resumed <- TrialLog$new("UsageOptimizer", log_dir = log_dir)
  usage <- resumed$as_tibble()

  expect_identical(usage$input_tokens, c(NA_integer_, 0L))
  expect_identical(usage$total_tokens, c(NA_integer_, 0L))
  expect_equal(usage$total_cost, c(NA_real_, 0))
  expect_identical(usage$provider_calls, c(NA_integer_, 0L))
  expect_identical(usage$metric_calls, c(1L, 1L))
  expect_identical(usage$provider_usage_unknown, c(TRUE, FALSE))
  expect_identical(usage$token_usage_unknown, c(TRUE, FALSE))
  expect_identical(resumed$summary()$total_tokens, NA_integer_)
  expect_identical(resumed$summary()$total_cost, NA_real_)
})

test_that("best programs are stored as safe program artifacts", {
  log_dir <- withr::local_tempdir()
  program <- module(signature("question -> answer"))
  trial <- complete_trial(
    create_trial("ArtifactOptimizer", trial_id = "best"),
    EvalResult(mean_score = 1, n_evaluated = 1L),
    compiled_artifact_ref = program
  )

  TrialLog$new("ArtifactOptimizer", log_dir = log_dir)$add_trial(trial)
  artifact_path <- file.path(log_dir, "best_program.rds")
  stored <- readRDS(artifact_path)

  expect_s3_class(stored, "dsprrr_program_artifact")
  expect_identical(inherits(stored, "Module"), FALSE)
  expect_s3_class(load_program(artifact_path), "Module")
})

test_that("persisted journals and derived files are owner-only", {
  skip_on_os("windows")
  log_dir <- withr::local_tempdir()
  trial <- complete_trial(
    create_trial("PrivateOptimizer", trial_id = "private"),
    EvalResult(mean_score = 1, n_evaluated = 1L)
  )

  TrialLog$new("PrivateOptimizer", log_dir = log_dir)$add_trial(trial)
  paths <- file.path(
    log_dir,
    c(".trials.lock", "trials.jsonl", "metadata.json", "README.md")
  )
  modes <- vapply(
    paths,
    function(path) {
      mode <- bitwAnd(
        as.integer(file.info(path, extra_cols = FALSE)$mode[[1L]]),
        as.integer(as.octmode("0777"))
      )
      as.character(as.octmode(mode))
    },
    character(1)
  )

  expect_identical(unname(modes), rep("600", length(paths)))
})

test_that("TrialLog rejects symbolic-link directories and journals", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  real_dir <- file.path(root, "real")
  dir.create(real_dir, mode = "0700")
  alias <- file.path(root, "alias")
  skip_if_not(file.symlink(real_dir, alias), "symbolic links unavailable")

  directory_condition <- rlang::catch_cnd(
    TrialLog$new("SymlinkOptimizer", log_dir = alias)
  )

  expect_s3_class(directory_condition, "dsprrr_trial_log_trust_error")

  log_dir <- file.path(root, "journal-log")
  dir.create(log_dir, mode = "0700")
  external <- file.path(root, "external.jsonl")
  writeLines("external sentinel", external)
  Sys.chmod(external, mode = "0600", use_umask = FALSE)
  journal <- file.path(log_dir, "trials.jsonl")
  skip_if_not(file.symlink(external, journal), "symbolic links unavailable")
  before <- readLines(external, warn = FALSE)

  journal_condition <- rlang::catch_cnd(
    TrialLog$new("SymlinkOptimizer", log_dir = log_dir)
  )

  expect_s3_class(journal_condition, "dsprrr_trial_log_trust_error")
  expect_identical(readLines(external, warn = FALSE), before)
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

test_that("TrialLog rejects attacker-owned sticky ancestors and children", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  original_owner <- cache_path_owner_id
  effective_owner <- cache_effective_owner_id()
  skip_if(is.na(effective_owner), "effective user ID unavailable")
  attacker_owner <- effective_owner + 1000L

  attacker_sticky <- file.path(root, "attacker-sticky")
  dir.create(attacker_sticky, mode = "0700")
  Sys.chmod(attacker_sticky, mode = "01777", use_umask = FALSE)
  canonical_attacker <- as.character(fs::path_real(attacker_sticky))
  mocked_owner <- canonical_attacker
  testthat::local_mocked_bindings(
    cache_path_owner_id = function(path) {
      canonical <- tryCatch(
        as.character(fs::path_real(path)),
        error = function(e) as.character(fs::path_abs(path))
      )
      if (identical(canonical, mocked_owner)) {
        return(attacker_owner)
      }
      original_owner(path)
    },
    .package = "dsprrr"
  )

  creation_condition <- rlang::catch_cnd(TrialLog$new(
    "StickyOwnerOptimizer",
    log_dir = file.path(attacker_sticky, "log")
  ))

  expect_s3_class(creation_condition, "dsprrr_trial_log_trust_error")
  expect_match(
    conditionMessage(creation_condition),
    "not owned by the effective user or root"
  )
  expect_identical(dir.exists(file.path(attacker_sticky, "log")), FALSE)

  mocked_owner <- character()
  safe_parent <- file.path(root, "safe-parent")
  log_dir <- file.path(safe_parent, "log")
  dir.create(safe_parent, mode = "0700")
  log <- TrialLog$new("StickyOwnerOptimizer", log_dir = log_dir)
  Sys.chmod(safe_parent, mode = "01777", use_umask = FALSE)
  withr::defer(Sys.chmod(safe_parent, mode = "0700", use_umask = FALSE))
  mocked_owner <- as.character(fs::path_real(safe_parent))

  recurring_condition <- rlang::catch_cnd(log$add_trial(
    create_trial("StickyOwnerOptimizer", trial_id = "blocked")
  ))

  expect_s3_class(recurring_condition, "dsprrr_trial_log_trust_error")
  expect_identical(log$n_trials(), 0L)

  mocked_owner <- character()
  sticky_parent <- file.path(root, "trusted-sticky")
  child <- file.path(sticky_parent, "child")
  dir.create(child, recursive = TRUE, mode = "0700")
  Sys.chmod(sticky_parent, mode = "01777", use_umask = FALSE)
  withr::defer(Sys.chmod(sticky_parent, mode = "0700", use_umask = FALSE))
  mocked_owner <- as.character(fs::path_real(child))

  child_condition <- rlang::catch_cnd(TrialLog$new(
    "StickyChildOptimizer",
    log_dir = file.path(child, "log")
  ))

  expect_s3_class(child_condition, "dsprrr_trial_log_trust_error")
  expect_match(
    conditionMessage(child_condition),
    "child not owned by the effective user"
  )
})

test_that("TrialLog rejects missing or non-finite directory identities", {
  log_dir <- withr::local_tempdir()
  original_info <- trial_log_path_info
  invalid_identity <- NA_real_
  testthat::local_mocked_bindings(
    trial_log_path_info = function(path) {
      info <- original_info(path)
      if (!is.null(info) && nrow(info) == 1L && dir.exists(path)) {
        info$device_id[[1L]] <- invalid_identity
      }
      info
    },
    .package = "dsprrr"
  )

  missing <- rlang::catch_cnd(
    TrialLog$new("MissingIdentityOptimizer", log_dir = log_dir)
  )
  invalid_identity <- Inf
  non_finite <- rlang::catch_cnd(
    TrialLog$new("MissingIdentityOptimizer", log_dir = log_dir)
  )

  expect_s3_class(missing, "dsprrr_trial_log_trust_error")
  expect_s3_class(non_finite, "dsprrr_trial_log_trust_error")
  expect_match(conditionMessage(missing), "identity is unavailable")
  expect_match(conditionMessage(non_finite), "identity is unavailable")
})

test_that("non-POSIX TrialLog files require stable platform identities", {
  log_dir <- withr::local_tempdir()
  journal <- file.path(log_dir, "trials.jsonl")
  writeLines(character(), journal)
  original_info <- trial_log_path_info
  invalid_identity <- NA_real_
  testthat::local_mocked_bindings(
    cache_private_modes_supported = function() FALSE,
    trial_log_path_info = function(path) {
      info <- original_info(path)
      if (
        !is.null(info) &&
          nrow(info) == 1L &&
          identical(basename(path), "trials.jsonl")
      ) {
        info$inode[[1L]] <- invalid_identity
      }
      info
    },
    .package = "dsprrr"
  )
  guard <- trial_log_prepare_directory(log_dir)

  missing <- rlang::catch_cnd(trial_log_read_raw(journal, guard))
  invalid_identity <- Inf
  non_finite <- rlang::catch_cnd(trial_log_read_raw(journal, guard))

  expect_s3_class(missing, "dsprrr_trial_log_trust_error")
  expect_s3_class(non_finite, "dsprrr_trial_log_trust_error")
  expect_match(conditionMessage(missing), "file identity is unavailable")
  expect_match(conditionMessage(non_finite), "file identity is unavailable")
})

test_that("TrialLog retains its original canonical directory identity", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  log_dir <- file.path(root, "log")
  log <- TrialLog$new("ReplacementOptimizer", log_dir = log_dir)
  displaced <- file.path(root, "displaced")
  expect_identical(file.rename(log_dir, displaced), TRUE)
  dir.create(log_dir, mode = "0700")

  condition <- rlang::catch_cnd(log$add_trial(
    create_trial("ReplacementOptimizer", trial_id = "blocked")
  ))

  expect_s3_class(condition, "dsprrr_trial_log_trust_error")
  expect_identical(file.exists(file.path(log_dir, "trials.jsonl")), FALSE)
  expect_identical(
    length(readLines(file.path(displaced, "trials.jsonl"), warn = FALSE)),
    0L
  )
})

test_that("publication rejects staging and target replacement races", {
  skip_on_os("windows")
  log_dir <- withr::local_tempdir()
  log <- TrialLog$new("RaceOptimizer", log_dir = log_dir)
  replaced_stage <- FALSE
  withr::local_options(
    dsprrr.trial_log_publication_hook = function(
      phase,
      path,
      temporary,
      what
    ) {
      if (
        !replaced_stage &&
          identical(phase, "stage_created") &&
          identical(what, "trial log journal")
      ) {
        replaced_stage <<- TRUE
        unlink(temporary)
        writeLines(character(), temporary)
        Sys.chmod(temporary, mode = "0600", use_umask = FALSE)
      }
    }
  )

  stage_condition <- rlang::catch_cnd(log$add_trial(
    create_trial("RaceOptimizer", trial_id = "stage-race")
  ))

  expect_s3_class(stage_condition, "dsprrr_trial_log_trust_error")
  expect_identical(log$n_trials(), 0L)
  expect_identical(
    length(readLines(file.path(log_dir, "trials.jsonl"), warn = FALSE)),
    0L
  )

  replaced_target <- FALSE
  withr::local_options(
    dsprrr.trial_log_publication_hook = function(
      phase,
      path,
      temporary,
      what
    ) {
      if (
        !replaced_target &&
          identical(phase, "before_publish") &&
          identical(what, "trial log journal")
      ) {
        replaced_target <<- TRUE
        unlink(path)
        writeLines("replacement", path)
        Sys.chmod(path, mode = "0600", use_umask = FALSE)
      }
    }
  )

  target_condition <- rlang::catch_cnd(log$add_trial(
    create_trial("RaceOptimizer", trial_id = "target-race")
  ))

  expect_s3_class(target_condition, "dsprrr_trial_log_trust_error")
  expect_identical(log$n_trials(), 0L)
  expect_identical(
    readLines(file.path(log_dir, "trials.jsonl"), warn = FALSE),
    "replacement"
  )
})

test_that("publication detects log directory replacement while locked", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  log_dir <- file.path(root, "log")
  log <- TrialLog$new("RenameRaceOptimizer", log_dir = log_dir)
  displaced <- file.path(root, "published-away")
  replaced <- FALSE
  withr::local_options(
    dsprrr.trial_log_publication_hook = function(
      phase,
      path,
      temporary,
      what
    ) {
      if (
        !replaced &&
          identical(phase, "before_publish") &&
          identical(what, "trial log journal")
      ) {
        replaced <<- TRUE
        file.rename(log_dir, displaced)
        dir.create(log_dir, mode = "0700")
      }
    }
  )

  condition <- rlang::catch_cnd(log$add_trial(
    create_trial("RenameRaceOptimizer", trial_id = "rename-race")
  ))

  expect_s3_class(condition, "dsprrr_trial_log_trust_error")
  expect_identical(file.exists(file.path(log_dir, "trials.jsonl")), FALSE)
  expect_identical(
    length(readLines(file.path(displaced, "trials.jsonl"), warn = FALSE)),
    0L
  )
})

test_that("derived symlink failures do not roll back the journal", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  log_dir <- file.path(root, "log")
  log <- TrialLog$new("DerivedOptimizer", log_dir = log_dir)
  metadata_external <- file.path(root, "metadata-external")
  readme_external <- file.path(root, "readme-external")
  writeLines("metadata sentinel", metadata_external)
  writeLines("readme sentinel", readme_external)
  Sys.chmod(
    c(metadata_external, readme_external),
    mode = "0600",
    use_umask = FALSE
  )
  expect_identical(
    file.symlink(metadata_external, file.path(log_dir, "metadata.json")),
    TRUE
  )
  expect_identical(
    file.symlink(readme_external, file.path(log_dir, "README.md")),
    TRUE
  )
  warnings <- list()

  withCallingHandlers(
    log$add_trial(create_trial("DerivedOptimizer", trial_id = "durable")),
    dsprrr_save_warning = function(warning) {
      warnings[[length(warnings) + 1L]] <<- warning
      rlang::cnd_muffle(warning)
    }
  )

  expect_identical(log$n_trials(), 1L)
  expect_identical(
    vapply(warnings, inherits, logical(1), "dsprrr_save_warning"),
    c(TRUE, TRUE)
  )
  expect_identical(
    readLines(metadata_external, warn = FALSE),
    "metadata sentinel"
  )
  expect_identical(readLines(readme_external, warn = FALSE), "readme sentinel")
  expect_identical(
    vapply(
      trial_log_parse_jsonl_file(file.path(log_dir, "trials.jsonl")),
      function(trial) trial@trial_id,
      character(1)
    ),
    "durable"
  )
  unlink(file.path(log_dir, c("metadata.json", "README.md")))
  recovered <- load_trial_log(log_dir)
  recovered$save()

  expect_identical(recovered$optimizer_name, "DerivedOptimizer")
  expect_identical(
    jsonlite::fromJSON(file.path(log_dir, "metadata.json"))$n_trials,
    1L
  )
})

test_that("legacy public journals are hardened with disclosure warning", {
  skip_on_os("windows")
  log_dir <- withr::local_tempdir()
  journal <- file.path(log_dir, "trials.jsonl")
  writeLines(character(), journal)
  Sys.chmod(journal, mode = "0644", use_umask = FALSE)

  condition <- rlang::catch_cnd(
    TrialLog$new("LegacyOptimizer", log_dir = log_dir)
  )
  mode <- bitwAnd(
    as.integer(file.info(journal, extra_cols = FALSE)$mode[[1L]]),
    as.integer(as.octmode("0777"))
  )

  expect_s3_class(
    condition,
    "dsprrr_trial_log_permission_repair_warning"
  )
  expect_identical(mode, as.integer(as.octmode("0600")))
})

test_that("torn journal tails are rejected without mutation", {
  log_dir <- withr::local_tempdir()
  TrialLog$new("TornOptimizer", log_dir = log_dir)
  journal <- file.path(log_dir, "trials.jsonl")
  connection <- file(journal, open = "wb")
  writeBin(charToRaw('{"trial_id":"partial"'), connection)
  close(connection)
  before <- readBin(journal, what = "raw", n = file.info(journal)$size)

  condition <- rlang::catch_cnd(
    TrialLog$new("TornOptimizer", log_dir = log_dir)
  )

  expect_s3_class(condition, "dsprrr_trial_log_torn_write")
  expect_s3_class(condition, "dsprrr_trial_log_corrupt")
  expect_identical(
    readBin(journal, what = "raw", n = file.info(journal)$size),
    before
  )
})

test_that("failed atomic publication leaves journal and derived state intact", {
  log_dir <- withr::local_tempdir()
  log <- TrialLog$new("AtomicOptimizer", log_dir = log_dir)
  first <- complete_trial(
    create_trial("AtomicOptimizer", trial_id = "first"),
    EvalResult(mean_score = 0.5, n_evaluated = 1L)
  )
  log$add_trial(first)
  journal <- file.path(log_dir, "trials.jsonl")
  metadata <- file.path(log_dir, "metadata.json")
  before_journal <- readBin(
    journal,
    what = "raw",
    n = file.info(journal)$size
  )
  before_metadata <- readBin(
    metadata,
    what = "raw",
    n = file.info(metadata)$size
  )
  testthat::local_mocked_bindings(
    trial_log_atomic_write_raw = function(value, path) {
      stage <- paste0(path, ".partial")
      connection <- file(stage, open = "wb")
      on.exit(close(connection), add = TRUE)
      writeBin(value[seq_len(max(1L, length(value) %/% 2L))], connection)
      stop("injected staging failure")
    },
    .package = "dsprrr"
  )
  second <- complete_trial(
    create_trial("AtomicOptimizer", trial_id = "second"),
    EvalResult(mean_score = 0.75, n_evaluated = 1L)
  )

  condition <- rlang::catch_cnd(log$add_trial(second))

  expect_s3_class(condition, "dsprrr_trial_log_append_error")
  expect_identical(log$n_trials(), 1L)
  expect_identical(
    readBin(journal, what = "raw", n = file.info(journal)$size),
    before_journal
  )
  expect_identical(
    readBin(metadata, what = "raw", n = file.info(metadata)$size),
    before_metadata
  )
  expect_identical(TrialLog$new("AtomicOptimizer", log_dir)$n_trials(), 1L)
})

test_that("two writers persist one identical trial idempotently", {
  skip_if_not_installed("callr")
  log_dir <- withr::local_tempdir()
  package_context <- callr_dsprrr_context()
  package_loader <- callr_load_dsprrr
  gate <- tempfile(tmpdir = log_dir)
  ready <- file.path(log_dir, c("ready-1", "ready-2"))
  worker <- function(
    package_context,
    package_loader,
    log_dir,
    gate,
    ready,
    value
  ) {
    namespace <- package_loader(package_context)
    Trial <- get("Trial", envir = namespace, inherits = FALSE)
    TrialLog <- get("TrialLog", envir = namespace, inherits = FALSE)
    options(dsprrr.trial_log_lock_hook = function() Sys.sleep(0.1))
    file.create(ready)
    deadline <- Sys.time() + 20
    while (!file.exists(gate) && Sys.time() < deadline) {
      Sys.sleep(0.01)
    }
    if (!file.exists(gate)) {
      stop("writer gate timed out")
    }
    fixed_time <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    trial <- Trial(
      trial_id = "shared-id",
      optimizer_name = "ParallelOptimizer",
      params = list(value = value),
      metric_summary = list(
        mean_score = 1,
        std_error = 0,
        n_evaluated = 1L,
        n_errors = 0L
      ),
      start_time = fixed_time,
      end_time = fixed_time,
      status = "completed"
    )
    tryCatch(
      {
        TrialLog$new("ParallelOptimizer", log_dir)$add_trial(trial)
        list(status = "ok", classes = character())
      },
      error = function(e) {
        list(status = "error", classes = class(e))
      }
    )
  }
  processes <- lapply(seq_along(ready), function(i) {
    callr::r_bg(
      worker,
      args = list(
        package_context,
        package_loader,
        log_dir,
        gate,
        ready[[i]],
        "same"
      ),
      supervise = TRUE
    )
  })
  withr::defer(lapply(processes, function(process) {
    if (process$is_alive()) {
      process$kill()
    }
  }))
  deadline <- Sys.time() + 20
  while (!all(file.exists(ready)) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_identical(all(file.exists(ready)), TRUE)
  file.create(gate)
  lapply(processes, function(process) process$wait(timeout = 20000))
  results <- lapply(processes, function(process) process$get_result())

  expect_identical(
    vapply(results, function(result) result$status, character(1)),
    c("ok", "ok")
  )
  expect_identical(
    length(readLines(file.path(log_dir, "trials.jsonl"), warn = FALSE)),
    1L
  )
  expect_identical(
    TrialLog$new("ParallelOptimizer", log_dir)$n_trials(),
    1L
  )
})

test_that("two writers reject conflicting records for the same trial ID", {
  skip_if_not_installed("callr")
  log_dir <- withr::local_tempdir()
  package_context <- callr_dsprrr_context()
  package_loader <- callr_load_dsprrr
  gate <- tempfile(tmpdir = log_dir)
  ready <- file.path(log_dir, c("ready-1", "ready-2"))
  worker <- function(
    package_context,
    package_loader,
    log_dir,
    gate,
    ready,
    value
  ) {
    namespace <- package_loader(package_context)
    Trial <- get("Trial", envir = namespace, inherits = FALSE)
    TrialLog <- get("TrialLog", envir = namespace, inherits = FALSE)
    options(dsprrr.trial_log_lock_hook = function() Sys.sleep(0.1))
    file.create(ready)
    deadline <- Sys.time() + 20
    while (!file.exists(gate) && Sys.time() < deadline) {
      Sys.sleep(0.01)
    }
    if (!file.exists(gate)) {
      stop("writer gate timed out")
    }
    fixed_time <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
    trial <- Trial(
      trial_id = "conflicting-id",
      optimizer_name = "ParallelOptimizer",
      params = list(value = value),
      metric_summary = list(
        mean_score = 1,
        std_error = 0,
        n_evaluated = 1L,
        n_errors = 0L
      ),
      start_time = fixed_time,
      end_time = fixed_time,
      status = "completed"
    )
    tryCatch(
      {
        TrialLog$new("ParallelOptimizer", log_dir)$add_trial(trial)
        list(status = "ok", classes = character())
      },
      error = function(e) {
        list(status = "error", classes = class(e))
      }
    )
  }
  processes <- Map(
    function(ready_path, value) {
      callr::r_bg(
        worker,
        args = list(
          package_context,
          package_loader,
          log_dir,
          gate,
          ready_path,
          value
        ),
        supervise = TRUE
      )
    },
    ready,
    c("first", "second")
  )
  withr::defer(lapply(processes, function(process) {
    if (process$is_alive()) {
      process$kill()
    }
  }))
  deadline <- Sys.time() + 20
  while (!all(file.exists(ready)) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_identical(all(file.exists(ready)), TRUE)
  file.create(gate)
  lapply(processes, function(process) process$wait(timeout = 20000))
  results <- lapply(processes, function(process) process$get_result())
  statuses <- vapply(results, function(result) result$status, character(1))
  conflicts <- vapply(
    results,
    function(result) {
      "dsprrr_trial_id_conflict" %in% result$classes
    },
    logical(1)
  )

  expect_identical(sum(statuses == "ok"), 1L)
  expect_identical(sum(conflicts), 1L)
  expect_identical(
    length(readLines(file.path(log_dir, "trials.jsonl"), warn = FALSE)),
    1L
  )
  expect_identical(
    jsonlite::fromJSON(file.path(log_dir, "metadata.json"))$n_trials,
    1L
  )
})

test_that("process death releases the interprocess trial log lock", {
  skip_if_not_installed("callr")
  log_dir <- withr::local_tempdir()
  package_context <- callr_dsprrr_context()
  package_loader <- callr_load_dsprrr
  ready <- file.path(log_dir, "lock-held")
  holder <- callr::r_bg(
    function(package_context, package_loader, log_dir, ready) {
      namespace <- package_loader(package_context)
      TrialLog <- get("TrialLog", envir = namespace, inherits = FALSE)
      options(dsprrr.trial_log_lock_hook = function() {
        file.create(ready)
        Sys.sleep(60)
      })
      TrialLog$new("CrashOptimizer", log_dir)
    },
    args = list(package_context, package_loader, log_dir, ready),
    supervise = TRUE
  )
  withr::defer(if (holder$is_alive()) holder$kill())
  deadline <- Sys.time() + 20
  while (!file.exists(ready) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_identical(file.exists(ready), TRUE)
  holder$kill()
  holder$wait(timeout = 10000)
  withr::local_options(dsprrr.trial_log_lock_timeout = 2)

  resumed <- TrialLog$new("CrashOptimizer", log_dir)

  expect_identical(resumed$n_trials(), 0L)
})

test_that("public JSONL writes are atomic, private, and idempotent", {
  path <- withr::local_tempfile(fileext = ".jsonl")
  unlink(path)
  first <- create_trial("PublicWriter", trial_id = "first")
  second <- create_trial("PublicWriter", trial_id = "second")

  write_trials_jsonl(list(first), path)
  write_trials_jsonl(list(second), path, append = TRUE)
  write_trials_jsonl(list(second), path, append = TRUE)

  expect_identical(
    vapply(
      read_trials_jsonl(path),
      function(trial) trial@trial_id,
      character(1)
    ),
    c("first", "second")
  )
  if (.Platform$OS.type == "unix") {
    mode <- bitwAnd(
      as.integer(file.info(path, extra_cols = FALSE)$mode[[1L]]),
      as.integer(as.octmode("0777"))
    )
    expect_identical(mode, as.integer(as.octmode("0600")))
  }
})

test_that("public append rejects a torn journal without changing it", {
  path <- withr::local_tempfile(fileext = ".jsonl")
  connection <- file(path, open = "wb")
  writeBin(charToRaw('{"trial_id":"partial"'), connection)
  close(connection)
  Sys.chmod(path, mode = "0600", use_umask = FALSE)
  before <- readBin(path, what = "raw", n = file.info(path)$size)

  condition <- rlang::catch_cnd(write_trials_jsonl(
    list(create_trial("PublicWriter", trial_id = "next")),
    path,
    append = TRUE
  ))

  expect_s3_class(condition, "dsprrr_trial_log_torn_write")
  expect_identical(
    readBin(path, what = "raw", n = file.info(path)$size),
    before
  )
})

test_that("public append failure leaves the previous journal intact", {
  path <- withr::local_tempfile(fileext = ".jsonl")
  unlink(path)
  write_trials_jsonl(
    list(create_trial("PublicWriter", trial_id = "first")),
    path
  )
  before <- readBin(path, what = "raw", n = file.info(path)$size)
  testthat::local_mocked_bindings(
    artifact_atomic_replace = function(source, destination, what) {
      cli::cli_abort(
        "injected publication failure",
        class = "dsprrr_artifact_io_error"
      )
    },
    .package = "dsprrr"
  )

  condition <- rlang::catch_cnd(write_trials_jsonl(
    list(create_trial("PublicWriter", trial_id = "second")),
    path,
    append = TRUE
  ))

  expect_s3_class(condition, "dsprrr_trial_log_append_error")
  expect_identical(
    readBin(path, what = "raw", n = file.info(path)$size),
    before
  )
})
