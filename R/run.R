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
#' @details
#' **Retry Behavior:** ellmer automatically retries failed requests up to 3 times
#' (configurable via `options(ellmer_max_tries = n)`). This handles transient
#' errors like rate limits and connection failures. See ellmer documentation
#' for more details.
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
#'
#' # Configure ellmer retry behavior (if needed)
#' options(ellmer_max_tries = 5)
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
  .return_format = "simple",
  .show_prompt = FALSE
) {
  # Show prompt preview if requested
  if (.show_prompt) {
    show_prompt_preview(module)
  }

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
  .return_format = "simple",
  .show_prompt = FALSE
) {
  # Show prompt preview if requested
  if (.show_prompt) {
    show_prompt_preview(module)
  }

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

  # Single input processing
  # Note: ellmer handles retries internally (configurable via options(ellmer_max_tries))
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

#' Process a single batch item
#'
#' Core processing logic shared by both parallel and sequential execution.
#'
#' @param input_set Named list of inputs for this item
#' @param module The module to execute
#' @param llm The LLM client to use
#' @param index The batch index (for metadata)
#' @param .verbose Whether to print debug output
#' @param .return_format "simple" or "structured"
#' @return Processed result (simple value or structured list)
#' @noRd
process_batch_item <- function(
  input_set,
  module,
  llm,
  index,
  .verbose,
  .return_format
) {
  prompt <- build_prompt(module, input_set)

  if (.verbose) {
    cli::cli_h3("Prompt {index}")
    cli::cli_code(prompt)
  }

  start_time <- Sys.time()

  response <- call_llm(
    llm = llm,
    prompt = prompt,
    output_type = module$signature@output_type,
    instructions = module$signature@instructions,
    verbose = .verbose
  )

  end_time <- Sys.time()
  latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
    1000

  if (.return_format == "simple") {
    extract_simple_output(response, module$signature@output_type)
  } else {
    list(
      output = response,
      chat = llm,
      metadata = list(
        latency_ms = latency_ms,
        prompt_length = nchar(prompt),
        prompt = prompt,
        instructions = module$signature@instructions,
        timestamp = end_time,
        batch_index = index
      )
    )
  }
}

#' Extract simple output from LLM response
#'
#' For single-field outputs, extract just the field value.
#'
#' @param response The LLM response
#' @param output_type The signature output type
#' @return Extracted value or full response
#' @noRd
extract_simple_output <- function(response, output_type) {
  if (
    inherits(output_type, "ellmer::TypeObject") &&
      length(output_type@properties) == 1
  ) {
    field_name <- names(output_type@properties)[1]
    if (!is.null(response[[field_name]])) {
      return(response[[field_name]])
    }
  }
  response
}

#' Create error result for batch processing
#'
#' @param error The error object
#' @param index The batch index
#' @param prompt The prompt that failed (for structured format)
#' @param instructions The instructions (for structured format)
#' @param llm The LLM client (for structured format, can be NULL)
#' @param .return_format "simple" or "structured"
#' @return Error result in appropriate format
#' @noRd
create_error_result <- function(
  error,
  index,
  prompt,
  instructions,
  llm,
  .return_format
) {
  if (.return_format == "simple") {
    structure(
      NA,
      error_message = paste0(
        "Failed to process item ",
        index,
        ": ",
        error$message
      )
    )
  } else {
    list(
      output = NA,
      chat = llm,
      metadata = list(
        error = error$message,
        batch_index = index,
        instructions = instructions,
        prompt = prompt
      )
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
  parallel_mode <- .parallel && n > 1
  input_sets <- lapply(seq_len(n), function(i) lapply(inputs, `[[`, i))

  if (parallel_mode) {
    results <- run_batch_parallel(
      module,
      input_sets,
      n,
      .llm,
      .verbose,
      .return_format,
      .progress
    )
  } else {
    results <- run_batch_sequential(
      module,
      input_sets,
      n,
      .llm,
      .verbose,
      .return_format,
      .progress
    )
  }

  if (.return_format == "structured") {
    structure(results, class = c("dsprrr_batch_result", "list"))
  } else {
    results
  }
}

#' Run batch processing sequentially
#' @noRd
run_batch_sequential <- function(
  module,
  input_sets,
  n,
  .llm,
  .verbose,
  .return_format,
  .progress
) {
  shared_llm <- .llm %||% module$chat %||% get_default_llm(module)
  results <- vector("list", n)

  # Create progress bar if requested
  progress_id <- NULL
  if (.progress && n > 1) {
    progress_id <- cli::cli_progress_bar(
      format = "Processing {cli::pb_current}/{cli::pb_total} | {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = n,
      clear = FALSE
    )
  }

  for (i in seq_len(n)) {
    prompt <- build_prompt(module, input_sets[[i]])

    results[[i]] <- tryCatch(
      {
        process_batch_item(
          input_set = input_sets[[i]],
          module = module,
          llm = shared_llm,
          index = i,
          .verbose = .verbose,
          .return_format = .return_format
        )
      },
      error = function(e) {
        cli::cli_warn("Failed to process item {i}: {e$message}")
        create_error_result(
          error = e,
          index = i,
          prompt = prompt,
          instructions = module$signature@instructions,
          llm = shared_llm,
          .return_format = .return_format
        )
      }
    )

    if (!is.null(progress_id)) {
      cli::cli_progress_update(id = progress_id)
    }
  }

  if (!is.null(progress_id)) {
    cli::cli_progress_done(id = progress_id)
  }

  results
}

#' Run batch processing in parallel using mirai
#' @noRd
run_batch_parallel <- function(
  module,
  input_sets,
  n,
  .llm,
  .verbose,
  .return_format,
  .progress
) {
  llm_factory <- if (!is.null(.llm)) {
    function() .llm
  } else if (!is.null(module$chat)) {
    # Use the module's stored Chat - note: each worker gets the same reference
    # For true parallel isolation, users should not provide a Chat
    function() module$chat
  } else {
    function() get_default_llm(module)
  }

  # Ensure mirai daemons are running
  current_daemons <- mirai::daemons(NULL)
  if (is.null(current_daemons) || current_daemons == 0) {
    mirai::daemons(n = max(1L, parallel::detectCores() - 1L))
  }

  # Launch parallel tasks
  mirai_tasks <- mirai::mirai_map(
    .x = seq_len(n),
    .f = function(
      i,
      input_sets,
      module,
      .verbose,
      .return_format,
      process_batch_item_fn,
      extract_simple_output_fn,
      build_prompt_fn,
      call_llm_fn,
      llm_factory,
      create_error_result_fn
    ) {
      input_set <- input_sets[[i]]
      prompt <- build_prompt_fn(module, input_set)
      worker_llm <- llm_factory()

      tryCatch(
        {
          process_batch_item_fn(
            input_set = input_set,
            module = module,
            llm = worker_llm,
            index = i,
            .verbose = .verbose,
            .return_format = .return_format
          )
        },
        error = function(e) {
          create_error_result_fn(
            error = e,
            index = i,
            prompt = prompt,
            instructions = module$signature@instructions,
            llm = NULL, # Can't serialize LLM in parallel mode
            .return_format = .return_format
          )
        }
      )
    },
    .args = list(
      input_sets = input_sets,
      module = module,
      .verbose = .verbose,
      .return_format = .return_format,
      process_batch_item_fn = process_batch_item,
      extract_simple_output_fn = extract_simple_output,
      build_prompt_fn = build_prompt,
      call_llm_fn = call_llm,
      llm_factory = llm_factory,
      create_error_result_fn = create_error_result
    )
  )

  # Collect results
  results <- vector("list", n)
  completed <- 0
  warnings_to_emit <- character(0)

  while (completed < n) {
    for (i in seq_len(n)) {
      if (is.null(results[[i]]) && !mirai::unresolved(mirai_tasks[[i]])) {
        result <- mirai_tasks[[i]][["data"]]

        # Handle errors from parallel execution
        if (
          .return_format == "simple" &&
            length(result) == 1 &&
            is.na(result) &&
            !is.null(attr(result, "error_message"))
        ) {
          warnings_to_emit <- c(warnings_to_emit, attr(result, "error_message"))
          result <- NA
        }

        results[[i]] <- result
        completed <- completed + 1

        if (.progress && n > 1) {
          cli::cli_progress_update()
        }
      }
    }
    Sys.sleep(0.01)
  }

  # Emit accumulated warnings
  for (warning_msg in warnings_to_emit) {
    cli::cli_warn(warning_msg)
  }

  results
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
#' Checks module's stored Chat, then config$llm, then auto-detects from
#' environment variables using get_default_chat().
#'
#' @param module The module to get an LLM for
#' @return An ellmer Chat object
#' @noRd
get_default_llm <- function(module) {
  # Check for Chat stored on module
  if (!is.null(module$chat)) {
    return(module$chat)
  }

  # Check for LLM in config (legacy support)
  if (!is.null(module$config$llm)) {
    return(module$config$llm)
  }

  # Use the new auto-detection from chat-default.R
  get_default_chat(create = TRUE)
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

#' Execute Module on Data
#'
#' @description
#' Execute a module on a data frame/tibble with optimized batch processing.
#'
#' @param module A DSPrrr module (e.g., created with `module()`)
#' @param data A tibble or data frame with columns matching the module's inputs.
#' @param ... Additional arguments passed to [run()].
#'
#' @return A tibble with the input columns plus a result column containing outputs
#' @export
#' @examples
#' \dontrun{
#' # Process data
#' df <- tibble::tibble(
#'   text = c("I love this!", "This is bad", "Okay product")
#' )
#'
#' llm <- ellmer::chat_openai()
#' results <- signature("text -> sentiment") |>
#'   module(type = "predict") |>
#'   run_dataset(df, .llm = llm)
#' }
run_dataset <- function(module, ...) {
  UseMethod("run_dataset")
}

#' @rdname run_dataset
#' @param .llm Optional ellmer Chat object for LLM calls
#' @param .verbose Logical whether to print verbose output
#' @param .parallel Logical whether to enable parallel processing
#' @param .progress Logical whether to show progress bar
#' @param .return_format Character either "simple" or "structured"
#' @export
run_dataset.Module <- function(
  module,
  data,
  .llm = NULL,
  .verbose = FALSE,
  .parallel = FALSE,
  .progress = TRUE,
  .return_format = "simple",
  ...
) {
  # Validate data
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame or tibble")
  }

  # Get required input names from signature
  sig_inputs <- module$signature@inputs
  if (length(sig_inputs) > 0) {
    required_names <- vapply(sig_inputs, function(x) x$name, character(1))
    missing_cols <- setdiff(required_names, names(data))

    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "{.arg data} missing required columns: {.field {missing_cols}}"
      )
    }
  } else {
    required_names <- character(0)
  }

  # Extract input columns as list
  if (length(required_names) > 0) {
    input_args <- as.list(data[required_names])
  } else {
    # If no specific inputs, try to use all columns
    input_args <- as.list(data)
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

  # Add results to data
  if (.return_format == "simple") {
    data$result <- results
  } else {
    # For structured format, extract outputs and add metadata columns
    data$result <- lapply(results, `[[`, "output")
    data$.metadata <- lapply(results, `[[`, "metadata")
    data$.chat <- lapply(results, `[[`, "chat")
  }

  tibble::as_tibble(data)
}

# Internal: Show prompt preview before LLM call
show_prompt_preview <- function(module) {
  cli::cli_h3("Prompt Preview")

  # Show instructions
  instructions <- module$signature@instructions
  if (nzchar(instructions)) {
    cli::cli_text("{.emph Instructions:}")
    # Truncate if very long
    if (nchar(instructions) > 200) {
      instructions <- paste0(substr(instructions, 1, 200), "...")
    }
    cli::cat_line(instructions)
    cli::cat_line()
  }

  # Show input fields expected
  input_names <- vapply(
    module$signature@inputs,
    function(x) x$name,
    character(1)
  )
  if (length(input_names) > 0) {
    cli::cli_text("{.emph Input fields:} {.field {input_names}}")
  }

  # Show output type
  output_type <- module$signature@output_type
  cli::cli_text("{.emph Output type:} {.cls {class(output_type)[1]}}")

  # Show if module has demos
  if (inherits(module, "PredictModule") && length(module$demos) > 0) {
    cli::cli_text("{.emph Demos:} {length(module$demos)} example(s)")
  }

  cli::cat_line()
}
