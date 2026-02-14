# rlm-live.R - Live mode: async RLM execution

#' Read package source code (R + SCSS/CSS files)
#' @param pkg_path Path to installed package
#' @return Named list with `source` (concatenated text) and `n_files`
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
  if (length(all_files) == 0) {
    return(list(source = "", n_files = 0L))
  }

  # Use relative paths for file headers
  rel_paths <- sub(
    paste0("^", normalizePath(pkg_path), "/?"),
    "",
    normalizePath(all_files)
  )
  contents <- vapply(
    all_files,
    function(f) {
      paste(readLines(f, warn = FALSE), collapse = "\n")
    },
    character(1)
  )

  combined <- paste(
    sprintf("# ---- FILE: %s ----\n%s", rel_paths, contents),
    collapse = "\n\n"
  )
  list(source = combined, n_files = length(all_files))
}

#' Load source code context for the bslib question
#'
#' Returns a list with:
#' - Named source strings (e.g., bslib_source, shiny_source)
#' - context_vars: metadata list for the UI
#' - sig_inputs: comma-separated input names for the RLM signature
load_bslib_context <- function() {
  pkgs <- list(
    bslib = system.file(package = "bslib"),
    shiny = system.file(package = "shiny")
  )

  # Filter out packages that aren't installed
  pkgs <- Filter(nzchar, pkgs)

  if (length(pkgs) == 0) {
    return(list(
      sources = list(),
      context_vars = list(),
      sig_inputs = "question"
    ))
  }

  sources <- lapply(pkgs, read_package_source)

  context_vars <- lapply(names(sources), function(nm) {
    list(
      name = paste0(nm, "_source"),
      size_chars = nchar(sources[[nm]]$source),
      n_files = sources[[nm]]$n_files
    )
  })

  source_data <- setNames(
    lapply(sources, function(s) s$source),
    paste0(names(sources), "_source")
  )

  source_names <- names(source_data)
  sig_inputs <- paste(c(source_names, "question"), collapse = ", ")

  list(
    sources = source_data,
    context_vars = context_vars,
    sig_inputs = sig_inputs
  )
}

# ---- Load context eagerly at app startup ----
# This is ~4.7M chars of source code; load once, reuse for every live run.
.live_context <- load_bslib_context()


#' Run an RLM query asynchronously
#' @param session Shiny session
#' @param config List with provider, model, api_key, question fields
run_live_rlm <- function(session, config) {
  post_message(session, "live_status", list(status = "running"))

  # Capture everything the future needs as plain data
  source_data <- .live_context$sources
  context_vars <- .live_context$context_vars
  sig_str <- paste0(.live_context$sig_inputs, " -> answer")
  question <- config$question
  provider <- config$provider %||% "openai"
  model <- config$model %||% "gpt-5-mini"
  api_key <- config$api_key

  missing_pkgs <- c("future", "promises")[
    !vapply(
      c("future", "promises"),
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]
  if (length(missing_pkgs) > 0) {
    post_message(
      session,
      "live_status",
      list(
        status = "error",
        message = sprintf(
          "Live mode requires: %s. Install with: install.packages(c(%s))",
          paste(missing_pkgs, collapse = ", "),
          paste(sprintf("'%s'", missing_pkgs), collapse = ", ")
        )
      )
    )
    return(invisible())
  }

  promises::future_promise(
    {
      library(dsprrr)
      library(ellmer)

      # Provider → ellmer constructor lookup
      chat_fns <- list(
        openai = ellmer::chat_openai,
        anthropic = ellmer::chat_anthropic,
        google = ellmer::chat_google_gemini,
        groq = ellmer::chat_groq,
        deepseek = ellmer::chat_deepseek,
        mistral = ellmer::chat_mistral,
        perplexity = ellmer::chat_perplexity,
        openrouter = ellmer::chat_openrouter,
        huggingface = ellmer::chat_huggingface,
        github = ellmer::chat_github,
        ollama = ellmer::chat_ollama
      )

      chat_fn <- chat_fns[[provider]]
      if (is.null(chat_fn)) {
        stop(paste("Unknown provider:", provider))
      }

      chat_args <- list(model = model)
      if (provider != "ollama" && !is.null(api_key) && nzchar(api_key)) {
        chat_args$api_key <- api_key
      }
      llm <- do.call(chat_fn, chat_args)

      runner <- r_code_runner(timeout = 30)

      rlm <- rlm_module(
        signature = sig_str,
        runner = runner,
        max_iterations = 15L,
        verbose = FALSE
      )

      # Build run args: source variables + question
      run_args <- source_data
      run_args$question <- question
      run_args$.llm <- llm
      run_args <- c(list(rlm), run_args)

      result <- do.call(dsprrr::run, run_args)

      history <- rlm$get_repl_history()
      last_run <- history[[length(history)]]

      # Extract total token usage from the chat object.
      # Note: token rows may not map 1:1 to iterations (retries, sub-queries),
      # so we only report totals rather than per-iteration attribution.
      token_tbl <- tryCatch(llm$get_tokens(), error = function(e) NULL)
      total_input <- 0L
      total_output <- 0L

      if (!is.null(token_tbl) && nrow(token_tbl) > 0) {
        total_input <- sum(token_tbl$input, na.rm = TRUE)
        total_output <- sum(token_tbl$output, na.rm = TRUE)
      }

      iterations_out <- lapply(seq_along(last_run$history), function(j) {
        h <- last_run$history[[j]]
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
        run_id = paste0("live-", format(Sys.time(), "%H%M%S")),
        timestamp = as.character(Sys.time()),
        question = question,
        model = model,
        context_variables = context_vars,
        iterations = iterations_out,
        final_answer = dsprrr::get_output(result)$answer,
        iterations_used = last_run$iterations_used,
        llm_calls_used = last_run$llm_calls_used,
        total_tokens = list(input = total_input, output = total_output)
      )
    },
    seed = NULL
  ) |>
    promises::then(
      onFulfilled = function(trace_result) {
        post_message(session, "live_result", trace_result)
        post_message(session, "live_status", list(status = "complete"))
      },
      onRejected = function(err) {
        post_message(
          session,
          "live_status",
          list(
            status = "error",
            message = conditionMessage(err)
          )
        )
      }
    )
}
