#' Compile S7 Generic and Methods
#'
#' This file defines the compile generic and its methods for optimizing
#' DSPrrr modules using teleprompters.

#' Compile Generic
#'
#' @description
#' Generic method for compiling/optimizing a module using a teleprompter.
#'
#' @param teleprompter A Teleprompter object
#' @param program A module to optimize
#' @param trainset Training data
#' @param ... Additional arguments
#'
#' @return An optimized module
#' @export
compile <- S7::new_generic("compile", c("teleprompter", "program"))

# Method registration moved to zzz.R to ensure proper loading order

#' Compile a DSPrrr Program
#'
#' @description
#' Main user-facing function to compile/optimize a DSPrrr module using
#' a teleprompter optimization strategy.
#'
#' @param program A DSPrrr module to optimize (e.g., from `module()`)
#' @param teleprompter A Teleprompter object defining the optimization strategy
#' @param trainset Training data as a data frame
#' @param valset Optional validation set for evaluation
#' @param ... Additional arguments passed to the teleprompter
#'
#' @return An optimized module with updated demonstrations and/or instructions
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a simple module
#' classifier <- signature("text -> sentiment") |>
#'   module(type = "predict")
#'
#' # Prepare training data
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience"),
#'   sentiment = c("positive", "negative")
#' )
#'
#' # Compile with LabeledFewShot
#' tp <- LabeledFewShot(k = 2)
#' optimized <- compile_module(classifier, tp, trainset)
#'
#' # Compile with GridSearch
#' variants <- data.frame(
#'   id = c("terse", "detailed"),
#'   instructions_suffix = c(
#'     "Be concise.",
#'     "Provide detailed reasoning."
#'   )
#' )
#' tp <- GridSearchTeleprompter(
#'   variants = variants,
#'   metric = metric_exact_match(field = "sentiment")
#' )
#' optimized <- compile_module(classifier, tp, trainset)
#' }
compile_module <- function(program, teleprompter, trainset, valset = NULL, ...) {
  # Validate inputs
  if (!inherits(teleprompter, "dsprrr::Teleprompter")) {
    cli::cli_abort(c(
      "teleprompter must be a Teleprompter object",
      "i" = "Got: {.cls {class(teleprompter)[1]}}"
    ))
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("trainset must be a data frame")
  }

  if (!is.null(valset) && !is.data.frame(valset)) {
    valset <- tryCatch(
      as.data.frame(valset),
      error = function(e) {
        cli::cli_abort(c(
          "valset must be convertible to a data frame",
          "x" = e$message
        ))
      }
    )
  }

  # Check if program is already compiled and warn
  if (!is.null(program@config$compiled) && program@config$compiled) {
    cli::cli_warn(c(
      "Program appears to be already compiled",
      "i" = "Previous teleprompter: {program@config$teleprompter}",
      "i" = "Recompiling with: {class(teleprompter)[1]}"
    ))
  }

  # Dispatch to appropriate compile method
  compile(teleprompter, program, trainset, valset = valset, ...)
}

#' Create Training Data for DSPrrr
#'
#' @description
#' Helper function to create properly formatted training data for
#' DSPrrr compilation.
#'
#' @param ... Named vectors or lists representing input/output pairs
#' @param .data Optional data frame to use as base
#'
#' @return A data frame suitable for use as trainset
#' @export
#' @examples
#' # Create training data from scratch
#' trainset <- dsp_trainset(
#'   text = c("Great product!", "Awful service"),
#'   sentiment = c("positive", "negative")
#' )
#'
#' # Add to existing data frame
#' df <- data.frame(text = c("Hello", "World"))
#' trainset <- dsp_trainset(.data = df, label = c("greeting", "other"))
dsp_trainset <- function(..., .data = NULL) {
  dots <- list(...)

  if (length(dots) == 0 && is.null(.data)) {
    cli::cli_abort("Must provide either data arguments or .data parameter")
  }

  # Start with .data if provided
  if (!is.null(.data)) {
    result <- as.data.frame(.data)
  } else {
    result <- data.frame(stringsAsFactors = FALSE)
  }

  # Add columns from dots
  if (length(dots) > 0) {
    # Check all have same length
    lengths <- vapply(dots, length, integer(1))
    if (length(unique(lengths)) > 1) {
      cli::cli_abort(c(
        "All arguments must have the same length",
        "i" = "Lengths: {lengths}"
      ))
    }

    # If result is empty, create with proper number of rows
    if (nrow(result) == 0 && length(dots) > 0) {
      n_rows <- lengths[1]
      result <- data.frame(row.names = seq_len(n_rows), stringsAsFactors = FALSE)
    }

    # Add to result
    for (name in names(dots)) {
      result[[name]] <- dots[[name]]
    }
  }

  # Validate result has at least one row
  if (nrow(result) == 0) {
    cli::cli_warn("Created empty training set")
  }

  result
}

#' Evaluate a Compiled Module
#'
#' @description
#' Evaluate the performance of a compiled module on a test dataset.
#'
#' @param module A DSPrrr module (compiled or not)
#' @param dataset Test dataset as a data frame
#' @param metric A metric function from `metric_*()` functions
#' @param llm Optional LLM connection for running the module
#' @param verbose Whether to show progress
#'
#' @return A list with evaluation results including mean score and per-example scores
#' @export
#' @examples
#' \dontrun{
#' # Evaluate a module
#' results <- evaluate_dsp(
#'   module = optimized_classifier,
#'   dataset = test_data,
#'   metric = metric_exact_match(field = "sentiment"),
#'   llm = llm_connection
#' )
#'
#' print(results$mean_score)
#' }
evaluate_dsp <- function(module, dataset, metric, llm = NULL, verbose = TRUE) {
  if (!is.data.frame(dataset)) {
    cli::cli_abort("dataset must be a data frame")
  }

  if (!is.function(metric)) {
    cli::cli_abort("metric must be a function")
  }

  n <- nrow(dataset)
  if (n == 0) {
    cli::cli_warn("Empty dataset provided")
    return(list(
      mean_score = NA_real_,
      scores = numeric(0),
      n_evaluated = 0,
      n_errors = 0
    ))
  }

  scores <- numeric(n)
  errors <- character(n)

  if (verbose) {
    cli::cli_progress_bar("Evaluating", total = n)
  }

  for (i in seq_len(n)) {
    row <- dataset[i, , drop = FALSE]

    tryCatch({
      # Extract inputs
      input_names <- vapply(module@signature@inputs, function(x) x$name, character(1))
      inputs <- list()
      for (name in input_names) {
        if (name %in% names(row)) {
          inputs[[name]] <- row[[name]]
        } else {
          cli::cli_warn("Missing input '{name}' in row {i}")
        }
      }

      # Run module (would need actual LLM connection)
      # prediction <- do.call(run, c(list(module), inputs, list(.llm = llm)))
      # For now, mock the prediction
      prediction <- list(result = "mock")  # This would be replaced with actual run()

      # Calculate score
      score <- metric(prediction, row)
      scores[i] <- if (is.logical(score)) as.numeric(score) else score

    }, error = function(e) {
      errors[i] <- e$message
      scores[i] <- NA_real_
    })

    if (verbose) {
      cli::cli_progress_update()
    }
  }

  if (verbose) {
    cli::cli_progress_done()

    # Report results
    n_success <- sum(!is.na(scores))
    n_errors <- sum(is.na(scores))
    mean_score <- mean(scores, na.rm = TRUE)

    cli::cli_alert_success("Evaluated {n_success}/{n} examples")
    if (n_errors > 0) {
      cli::cli_alert_warning("{n_errors} examples failed")
    }
    cli::cli_alert_info("Mean score: {round(mean_score, 3)}")
  }

  list(
    mean_score = mean(scores, na.rm = TRUE),
    scores = scores,
    errors = errors[errors != ""],
    n_evaluated = sum(!is.na(scores)),
    n_errors = sum(is.na(scores)),
    dataset = dataset
  )
}