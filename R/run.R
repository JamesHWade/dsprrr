#' Execute an LLM Module
#'
#' @description
#' Execute a module with the provided inputs to generate LLM output.
#' This is the primary function for running modules created with `module()`.
#'
#' Supports both single inputs and batch processing. Batch execution can be
#' parallelised, but is conservative by default to avoid reusing LLM clients
#' across workers.
#'
#' @param module A DSPrrr module (e.g., created with `module()`)
#' @param ... Named arguments corresponding to the module's signature inputs.
#'   Can be single values or vectors for batch processing. Additional parameters:
#'   \describe{
#'     \item{.llm}{An ellmer chat object for LLM interaction (optional)}
#'     \item{.verbose}{Logical indicating whether to print debug information}
#'     \item{.parallel}{Logical indicating whether to process batch inputs in parallel (default FALSE).
#'       When `TRUE`, a fresh LLM client is created per worker unless a custom
#'       `.llm` is supplied (in which case the call falls back to sequential
#'       execution).}
#'     \item{.progress}{Logical indicating whether to show progress bar for batch processing (default TRUE)}
#'     \item{.return_format}{Character, either "simple" (default) or "structured".
#'       "simple" returns just the output, "structured" returns list with output, chat, and metadata.}
#'   }
#'
#' @return For single inputs with .return_format="simple": The parsed output according to the module's signature.
#'   For single inputs with .return_format="structured": A list with components:
#'   - output: The parsed output
#'   - chat: The ellmer chat object used
#'   - metadata: Additional metadata (tokens used, latency, etc.)
#'   For batch inputs: A list of results matching the input length
#' @export
#' @examples
#' \dontrun{
#' # Single input
#' llm <- ellmer::chat_openai()
#' result <- signature("text -> sentiment") |>
#'   module(type = "predict") |>
#'   run(text = "I love this!", .llm = llm)
#'
#' # Batch processing
#' results <- signature("text -> sentiment") |>
#'   module(type = "predict") |>
#'   run(text = c("I love this!", "This is bad"), .llm = llm)
#'
#' # Structured return
#' result <- signature("text -> sentiment") |>
#'   module(type = "predict") |>
#'   run(text = "Great!", .llm = llm, .return_format = "structured")
#' # Access: result$output, result$chat, result$metadata
#' }
run <- function(module, ...) {
  UseMethod("run")
}

#' @export
run.Module <- function(
  module,
  ...,
  .llm = NULL,
  .verbose = FALSE,
  .parallel = FALSE,
  .progress = TRUE,
  .return_format = "simple"
) {
  # Delegate to the module's run method
  module$run(
    ...,
    .llm = .llm,
    .verbose = .verbose,
    .parallel = .parallel,
    .progress = .progress,
    .return_format = .return_format
  )
}

#' @export
run.PredictModule <- function(
  module,
  ...,
  .llm = NULL,
  .verbose = FALSE,
  .parallel = FALSE,
  .progress = TRUE,
  .return_format = "simple"
) {
  # Capture input arguments
  inputs <- list(...)

  # Validate return format
  .return_format <- match.arg(.return_format, c("simple", "structured"))

  # Validate inputs against signature
  sig_inputs <- module$signature@inputs
  if (length(sig_inputs) > 0) {
    required_names <- vapply(sig_inputs, function(x) x$name, character(1))
    missing_inputs <- setdiff(required_names, names(inputs))

    if (length(missing_inputs) > 0) {
      cli::cli_abort(
        "Missing required inputs: {.field {missing_inputs}}"
      )
    }
  }

  # Check if this is batch processing
  input_lengths <- vapply(inputs, length, integer(1))
  is_batch <- any(input_lengths > 1)


  if (is_batch) {
    if (.parallel && !is.null(.llm)) {
      cli::cli_warn(
        "Parallel execution requires a NULL .llm so each worker can create an independent client; falling back to sequential processing"
      )
      .parallel <- FALSE
    }

    # Validate all inputs have same length or length 1
    max_length <- max(input_lengths)
    invalid_lengths <- input_lengths[
      input_lengths != 1 & input_lengths != max_length
    ]

    if (length(invalid_lengths) > 0) {
      cli::cli_abort(
        "All inputs must have the same length or length 1 for batch processing"
      )
    }

    # Expand scalar inputs to match batch size
    inputs <- lapply(inputs, function(x) {
      if (length(x) == 1) rep(x, max_length) else x
    })

    # Process batch
    return(run_batch(
      module,
      inputs,
      max_length,
      .llm,
      .verbose,
      .parallel,
      .progress,
      .return_format
    ))
  }

  # Single input processing - use module$forward
  result <- module$forward(inputs, .llm = .llm, trace = TRUE)

  if (.verbose && !is.null(result$metadata[[1]]$prompt)) {
    cli::cli_h3("Generated Prompt")
    cli::cli_code(result$metadata[[1]]$prompt)
  }

  # Return based on format
  if (.return_format == "simple") {
    result$output[[1]]
  } else {
    structure(
      list(
        output = result$output[[1]],
        chat = result$chat[[1]],
        metadata = result$metadata[[1]]
      ),
      class = "dsprrr_result"
    )
  }
}

#' Process batch inputs
#' @noRd
run_batch <- function(
  module,
  inputs,
  n,
  .llm,
  .verbose,
  .parallel,
  .progress,
  .return_format
) {
  # Use forward() method for each example
  # This is the proper interface and works for both standard and custom modules
  input_sets <- lapply(seq_len(n), function(i) lapply(inputs, `[[`, i))

  if (.progress && n > 1) {
    cli::cli_progress_bar(
      format = "Processing {cli::pb_current}/{cli::pb_total} | {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = n,
      clear = FALSE
    )
  }

  results <- vector("list", n)
  for (i in seq_len(n)) {
    input_set <- input_sets[[i]]

    result <- tryCatch(
      module$forward(input_set, .llm = .llm, trace = TRUE),
      error = function(e) {
        cli::cli_warn("Failed to process item {i}: {e$message}")
        NULL
      }
    )

    if (is.null(result)) {
      # Error occurred
      if (.return_format == "simple") {
        results[[i]] <- NA
      } else {
        results[[i]] <- list(
          output = NA,
          chat = .llm,
          metadata = list(
            error = "Processing failed",
            batch_index = i
          )
        )
      }
    } else {
      if (.return_format == "simple") {
        results[[i]] <- result$output[[1]]
      } else {
        results[[i]] <- list(
          output = result$output[[1]],
          chat = result$chat[[1]],
          metadata = result$metadata[[1]]
        )
      }
    }

    if (.progress && n > 1) {
      cli::cli_progress_update()
    }
  }

  if (.progress && n > 1) {
    cli::cli_progress_done()
  }

  if (.return_format == "structured") {
    structure(results, class = c("dsprrr_batch_result", "list"))
  } else {
    results
  }
}

#' Build a prompt from a module and inputs
#'
#' @noRd
build_prompt <- function(module, inputs) {
  prompt_parts <- character()

  # Add demonstrations if present
  if (length(module$demos) > 0) {
    demo_text <- format_demos(module$demos, module$signature)
    prompt_parts <- c(prompt_parts, demo_text, "")
  }

  # Add the main template with inputs
  if (nchar(module$template) > 0) {
    filled_template <- glue::glue_data(
      .x = inputs,
      module$template,
      .open = "{",
      .close = "}",
      .envir = parent.frame()
    )
    prompt_parts <- c(prompt_parts, filled_template)
  } else {
    # Auto-generate template from inputs
    input_text <- format_inputs(inputs, module$signature@inputs)
    prompt_parts <- c(prompt_parts, input_text)
  }

  paste(prompt_parts, collapse = "\n")
}

#' Format demonstrations for inclusion in prompt
#'
#' @noRd
format_demos <- function(demos, signature) {
  demo_lines <- character()

  for (i in seq_along(demos)) {
    demo <- demos[[i]]
    demo_lines <- c(demo_lines, paste0("Example ", i, ":"))

    # Format inputs
    if (!is.null(demo$inputs)) {
      for (name in names(demo$inputs)) {
        demo_lines <- c(demo_lines, paste0(name, ": ", demo$inputs[[name]]))
      }
    }

    # Format output
    if (!is.null(demo$output)) {
      demo_lines <- c(
        demo_lines,
        paste0("Output: ", format_output(demo$output))
      )
    }

    demo_lines <- c(demo_lines, "")
  }

  paste(demo_lines, collapse = "\n")
}

#' Format inputs when no template is provided
#'
#' @noRd
format_inputs <- function(inputs, sig_inputs) {
  input_lines <- character()

  for (name in names(inputs)) {
    value <- inputs[[name]]
    # Find the corresponding signature input for description
    sig_input <- Find(function(x) x$name == name, sig_inputs)

    if (!is.null(sig_input) && !is.null(sig_input$description)) {
      input_lines <- c(input_lines, paste0("# ", sig_input$description))
    }

    input_lines <- c(input_lines, paste0(name, ": ", value))
  }

  paste(input_lines, collapse = "\n")
}

#' Format output for display
#'
#' @noRd
format_output <- function(output) {
  if (is.list(output)) {
    jsonlite::toJSON(output, auto_unbox = TRUE, pretty = FALSE)
  } else {
    as.character(output)
  }
}

#' Get default LLM configuration
#'
#' @noRd
get_default_llm <- function(config) {
  # Check for LLM in config
  if (!is.null(config$llm)) {
    return(config$llm)
  }

  # Otherwise create a default ellmer chat object
  # This will use ellmer's default configuration
  ellmer::chat_openai(model = "gpt-5-mini")
}

#' Call the LLM with structured output
#'
#' @noRd
call_llm <- function(
  llm,
  prompt,
  output_type,
  instructions = "",
  verbose = FALSE
) {
  # Build the full prompt with instructions
  full_prompt <- if (nchar(instructions) > 0) {
    paste(instructions, prompt, sep = "\n\n")
  } else {
    prompt
  }

  # Make the API call through ellmer's chat_structured method
  tryCatch(
    {
      # Note: echo="text" doesn't work with chat_structured for some providers
      # So we disable echo for structured calls even in verbose mode
      result <- llm$chat_structured(
        full_prompt,
        type = output_type,
        echo = "none"
      )

      result
    },
    error = function(e) {
      cli::cli_abort(
        "LLM call failed: {e$message}",
        parent = e
      )
    }
  )
}

#' Execute Module on Dataset
#'
#' @description
#' Execute a module on a dataset (tibble/data.frame) with optimized batch processing.
#'
#' @param module A DSPrrr module (e.g., created with `module()`)
#' @param dataset A tibble or data frame with columns matching the module's inputs
#' @param .llm An ellmer chat object for LLM interaction (optional)
#' @param .verbose Logical indicating whether to print debug information
#' @param .parallel Logical indicating whether to process in parallel (default FALSE)
#' @param .progress Logical indicating whether to show progress bar (default TRUE)
#' @param .return_format Character, either "simple" or "structured" (default "simple")
#' @param ... Additional arguments passed to methods
#'
#' @return A tibble with the input columns plus a result column containing outputs
#' @export
#' @examples
#' \dontrun{
#' # Process a dataset
#' data <- tibble::tibble(
#'   text = c("I love this!", "This is bad", "Okay product")
#' )
#'
#' llm <- ellmer::chat_openai()
#' results <- signature("text -> sentiment") |>
#'   module(type = "predict") |>
#'   run_dataset(data, .llm = llm)
#' }
run_dataset <- function(module, ...) {
  UseMethod("run_dataset")
}

#' @rdname run_dataset
#' @export
run_dataset.Module <- function(
  module,
  dataset,
  .llm = NULL,
  .verbose = FALSE,
  .parallel = FALSE,
  .progress = TRUE,
  .return_format = "simple",
  ...
) {
  # Validate dataset
  if (!is.data.frame(dataset)) {
    cli::cli_abort("dataset must be a data frame or tibble")
  }

  # Get required input names from signature
  sig_inputs <- module$signature@inputs
  if (length(sig_inputs) > 0) {
    required_names <- vapply(sig_inputs, function(x) x$name, character(1))
    missing_cols <- setdiff(required_names, names(dataset))

    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "Dataset missing required columns: {.field {missing_cols}}"
      )
    }
  } else {
    required_names <- character(0)
  }

  # Extract input columns as list
  if (length(required_names) > 0) {
    input_args <- as.list(dataset[required_names])
  } else {
    # If no specific inputs, try to use all columns
    input_args <- as.list(dataset)
  }

  # Run batch processing
  results <- do.call(
    run,
    c(
      list(module = module),
      input_args,
      list(
        .llm = .llm,
        .verbose = .verbose,
        .parallel = .parallel,
        .progress = .progress,
        .return_format = .return_format
      )
    )
  )

  if (.return_format == "structured" && inherits(results, "dsprrr_result")) {
    results <- list(results)
  }

  # Add results to dataset
  if (.return_format == "simple") {
    dataset$result <- results
  } else {
    # For structured format, extract outputs and add metadata columns
    dataset$result <- lapply(results, `[[`, "output")
    dataset$.metadata <- lapply(results, `[[`, "metadata")
    dataset$.chat <- lapply(results, `[[`, "chat")
  }

  tibble::as_tibble(dataset)
}
