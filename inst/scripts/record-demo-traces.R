# record-demo-traces.R
# Records fresh RLM traces for the interactive demo app.
#
# Requirements:
# - OPENAI_API_KEY environment variable set
# - git available (to clone bslib source)
#
# Usage:
#   Rscript inst/scripts/record-demo-traces.R
#
# Takes ~15-20 minutes (5 runs x 2-5 min each)

library(dsprrr)
library(ellmer)
library(jsonlite)

output_dir <- file.path("inst", "shiny-demo", "data", "runs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ---- Helper: Read package source from a directory ----
read_package_source <- function(pkg_path) {
  r_files <- list.files(
    file.path(pkg_path, "R"),
    pattern = "[.][rR]$",
    full.names = TRUE,
    recursive = TRUE
  )

  scss_files <- list.files(
    pkg_path,
    pattern = "[.](scss|sass|css)$",
    full.names = TRUE,
    recursive = TRUE
  )

  all_files <- c(r_files, scss_files)
  # Use paths relative to pkg_path for cleaner file labels
  rel_paths <- sub(paste0("^", normalizePath(pkg_path), "/?"), "", all_files)
  contents <- vapply(
    all_files,
    function(f) paste(readLines(f, warn = FALSE), collapse = "\n"),
    character(1)
  )

  paste(
    sprintf("# ---- FILE: %s ----\n%s", rel_paths, contents),
    collapse = "\n\n"
  )
}

# ---- Helper: Clone a GitHub repo to a temp directory ----
clone_source <- function(repo, ref = "main") {
  dest <- file.path(tempdir(), basename(repo))
  if (dir.exists(dest)) {
    unlink(dest, recursive = TRUE)
  }
  system2(
    "git",
    c(
      "clone",
      "--depth=1",
      paste0("--branch=", ref),
      paste0("https://github.com/", repo, ".git"),
      dest
    ),
    stdout = FALSE,
    stderr = FALSE
  )
  if (!dir.exists(dest)) {
    cli::cli_abort("Failed to clone {repo}")
  }
  dest
}

# ---- Helper: Convert repl_history to TraceData JSON ----
trace_to_json <- function(
  run_id,
  history_entry,
  question,
  model,
  context_vars,
  llm_calls = 0L
) {
  iterations <- lapply(history_entry$history, function(h) {
    list(
      iteration = h$iteration,
      reasoning = h$reasoning,
      code = h$code,
      output = h$output,
      success = h$success,
      is_final = h$is_final
    )
  })

  list(
    run_id = run_id,
    timestamp = as.character(history_entry$timestamp),
    question = question,
    model = model,
    context_variables = context_vars,
    iterations = iterations,
    final_answer = if (is.list(history_entry$final_answer)) {
      history_entry$final_answer$answer
    } else {
      as.character(history_entry$final_answer)
    },
    iterations_used = history_entry$iterations_used,
    llm_calls_used = llm_calls
  )
}

# ---- Main ----
main <- function() {
  cli::cli_h1("Recording RLM Demo Traces")

  # Check for API key
  if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    cli::cli_abort("OPENAI_API_KEY environment variable not set")
  }

  question <- paste(
    "How does bslib process the `foreground` color from brand.yml?",
    "Specifically, trace the path from `_brand.yml` foreground to the",
    "final CSS output. What happens to this color during Sass compilation?"
  )

  # Clone source repos
  cli::cli_alert_info("Cloning bslib source from GitHub...")
  bslib_path <- clone_source("rstudio/bslib")
  cli::cli_alert_success("Cloned bslib to {bslib_path}")

  cli::cli_alert_info("Reading source files...")
  bslib_source <- read_package_source(bslib_path)
  bslib_r_files <- list.files(file.path(bslib_path, "R"), pattern = "[.][rR]$")
  bslib_scss <- list.files(
    bslib_path,
    pattern = "[.](scss|sass|css)$",
    recursive = TRUE
  )

  context_vars <- list(
    list(
      name = "bslib_source",
      size_chars = nchar(bslib_source),
      n_files = length(bslib_r_files) + length(bslib_scss)
    )
  )
  cli::cli_alert_info(
    "bslib: {nchar(bslib_source)} chars, {length(bslib_r_files)} R + {length(bslib_scss)} scss files"
  )

  runner <- r_code_runner(timeout = 30)

  # ---- Run 1-4: Standard runs ----
  for (i in 1:4) {
    cli::cli_alert_info("Recording Run {i}/5...")

    llm <- chat_openai(model = "gpt-5-mini")
    rlm <- rlm_module(
      signature = "bslib_source, question -> answer",
      runner = runner,
      max_iterations = 15L,
      verbose = TRUE
    )

    tryCatch(
      {
        result <- run(
          rlm,
          bslib_source = bslib_source,
          question = question,
          .llm = llm
        )

        history <- rlm$get_repl_history()
        last_run <- history[[length(history)]]

        trace_data <- trace_to_json(
          run_id = paste0("bslib-run-", i),
          history_entry = last_run,
          question = question,
          model = "gpt-5-mini",
          context_vars = context_vars
        )

        filepath <- file.path(output_dir, paste0("bslib-run-", i, ".json"))
        write_json(trace_data, filepath, auto_unbox = TRUE, pretty = TRUE)
        cli::cli_alert_success("Saved {filepath}")
      },
      error = function(e) {
        cli::cli_alert_danger("Run {i} failed: {e$message}")
      }
    )
  }

  # ---- Run 5: Recursive (with sub_lm) ----
  cli::cli_alert_info("Recording Run 5/5 (recursive)...")

  llm <- chat_openai(model = "gpt-5-mini")
  sub_llm <- chat_openai(model = "gpt-5-mini")

  rlm_recursive <- rlm_module(
    signature = "bslib_source, question -> answer",
    runner = runner,
    max_iterations = 15L,
    sub_lm = sub_llm,
    max_llm_calls = 10L,
    verbose = TRUE
  )

  tryCatch(
    {
      result <- run(
        rlm_recursive,
        bslib_source = bslib_source,
        question = question,
        .llm = llm
      )

      history <- rlm_recursive$get_repl_history()
      last_run <- history[[length(history)]]

      trace_data <- trace_to_json(
        run_id = "bslib-recursive",
        history_entry = last_run,
        question = question,
        model = "gpt-5-mini",
        context_vars = context_vars,
        llm_calls = last_run$llm_calls_used
      )

      filepath <- file.path(output_dir, "bslib-recursive.json")
      write_json(trace_data, filepath, auto_unbox = TRUE, pretty = TRUE)
      cli::cli_alert_success("Saved {filepath}")
    },
    error = function(e) {
      cli::cli_alert_danger("Recursive run failed: {e$message}")
    }
  )

  cli::cli_alert_success("All traces recorded!")
}

if (interactive() || identical(Sys.getenv("TESTTHAT"), "")) {
  main()
}
