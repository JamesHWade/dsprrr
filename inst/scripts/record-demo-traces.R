# record-demo-traces.R
# Records fresh RLM traces for the interactive demo app.
#
# Requirements:
# - OPENAI_API_KEY environment variable set
# - bslib, shiny, and brand.yml source packages available
#
# Usage:
#   Rscript inst/scripts/record-demo-traces.R
#
# Takes ~15-20 minutes (5 runs x 2-5 min each)

library(dsprrr)
library(ellmer)
library(jsonlite)

output_dir <- file.path("inst", "shiny-demo", "data", "runs")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---- Helper: Read package source ----
read_package_source <- function(pkg_path) {
  r_files <- list.files(
    file.path(pkg_path, "R"),
    pattern = "\\.[rR]$",
    full.names = TRUE,
    recursive = TRUE
  )

  scss_files <- list.files(
    pkg_path,
    pattern = "\\.(scss|sass|css)$",
    full.names = TRUE,
    recursive = TRUE
  )

  all_files <- c(r_files, scss_files)
  contents <- vapply(all_files, function(f) {
    paste(readLines(f, warn = FALSE), collapse = "\n")
  }, character(1))

  paste(contents, collapse = "\n\n# ---- FILE: %s ----\n\n")
}

# ---- Helper: Convert repl_history to TraceData JSON ----
trace_to_json <- function(run_id, history_entry, question, model,
                          context_vars, llm_calls = 0L) {
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

  # Load source packages (adjust paths as needed)
  cli::cli_alert_info("Loading source packages...")

  # You'll need to adjust these paths to where the packages are installed
  bslib_source <- read_package_source(system.file(package = "bslib"))
  shiny_source <- read_package_source(system.file(package = "shiny"))

  context_vars <- list(
    list(
      name = "bslib_source",
      size_chars = nchar(bslib_source),
      n_files = length(list.files(
        file.path(system.file(package = "bslib"), "R"),
        recursive = TRUE
      ))
    ),
    list(
      name = "shiny_source",
      size_chars = nchar(shiny_source),
      n_files = length(list.files(
        file.path(system.file(package = "shiny"), "R"),
        recursive = TRUE
      ))
    )
  )

  runner <- r_code_runner(timeout = 30)

  # ---- Run 1-4: Standard runs ----
  for (i in 1:4) {
    cli::cli_alert_info("Recording Run {i}/5...")

    llm <- chat_openai(model = "gpt-4o-mini")
    rlm <- rlm_module(
      signature = "bslib_source, shiny_source, question -> answer",
      runner = runner,
      max_iterations = 15L,
      verbose = TRUE
    )

    tryCatch(
      {
        result <- run(
          rlm,
          bslib_source = bslib_source,
          shiny_source = shiny_source,
          question = question,
          .llm = llm
        )

        history <- rlm$get_repl_history()
        last_run <- history[[length(history)]]

        trace_data <- trace_to_json(
          run_id = paste0("bslib-run-", i),
          history_entry = last_run,
          question = question,
          model = "gpt-4o-mini",
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

  llm <- chat_openai(model = "gpt-4o-mini")
  sub_llm <- chat_openai(model = "gpt-4o-mini")

  rlm_recursive <- rlm_module(
    signature = "bslib_source, shiny_source, question -> answer",
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
        shiny_source = shiny_source,
        question = question,
        .llm = llm
      )

      history <- rlm_recursive$get_repl_history()
      last_run <- history[[length(history)]]

      trace_data <- trace_to_json(
        run_id = "bslib-recursive",
        history_entry = last_run,
        question = question,
        model = "gpt-4o-mini",
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
