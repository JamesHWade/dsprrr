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
#'     \item{.parallel}{Logical indicating whether to process batch inputs in parallel (default FALSE).}
#'     \item{.parallel_method}{Character, either "ellmer" (default) or "mirai".
#'       "ellmer" uses ellmer's `parallel_chat_structured()` for native async HTTP
#'       parallelism (more efficient, single process).
#'       "mirai" uses mirai for multi-process parallelism (requires `.llm = NULL`
#'       so each worker can create an independent client).}
#'     \item{.progress}{Logical indicating whether to show progress bar for batch processing (default TRUE)}
#'     \item{.return_format}{Character, either "simple" (default) or "structured".
#'       "simple" returns just the output, "structured" returns list with output, chat, and metadata.}
#'     \item{.cache}{Logical or NULL. Per-call cache control. If NULL (default), uses global config.
#'       If TRUE, attempts to use cache (no effect if caching globally disabled).
#'       If FALSE, bypasses cache for this call only.}
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
#' @seealso
#' * [dsp()] for one-shot LLM calls without creating a module
#' * [run_dataset()] for running a module on a data frame
#' * [evaluate()] for running with metric evaluation
#' * [module()] for creating modules
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
  .parallel_method = c("ellmer", "mirai"),
  .progress = TRUE,
  .return_format = "simple",
  .show_prompt = FALSE,
  .cache = NULL
) {
  .parallel_method <- match.arg(.parallel_method)

  # Validate .cache parameter
  if (!is.null(.cache)) {
    if (!is.logical(.cache) || length(.cache) != 1 || is.na(.cache)) {
      cache_value <- .cache
      cli::cli_abort(c(
        "{.arg .cache} must be {.code TRUE}, {.code FALSE}, or {.code NULL}",
        "x" = "You provided: {.cls {class(cache_value)}} with value {.val {cache_value}}",
        "i" = "{.code TRUE} attempts to use cache (if available)",
        "i" = "{.code FALSE} bypasses cache for this call",
        "i" = "{.code NULL} uses global cache configuration (default)"
      ))
    }
  }

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
      provided <- names(inputs)

      # Build error message with "Did you mean?" suggestions
      msg <- c("Missing required inputs: {.field {missing_inputs}}")

      # Check for typos in provided names
      for (missing in missing_inputs) {
        suggestion <- suggest_match(missing, provided)
        if (!is.null(suggestion)) {
          msg <- c(msg, "i" = suggestion)
        }
      }

      msg <- c(
        msg,
        "i" = "Signature expects: {.field {required_names}}",
        if (length(provided) > 0) c("i" = "You provided: {.field {provided}}")
      )

      cli::cli_abort(msg)
    }
  }

  # Check if this is batch processing
  input_lengths <- lengths(inputs)
  is_batch <- any(input_lengths > 1)

  if (is_batch) {
    if (.parallel && !is.null(.llm) && .parallel_method == "mirai") {
      cli::cli_warn(c(
        "mirai parallel execution requires {.code .llm = NULL} so each worker can create an independent client",
        "i" = "Falling back to sequential processing",
        "i" = "To enable parallel: remove {.arg .llm}, set {.code .llm = NULL}, or use {.code .parallel_method = \"ellmer\"}"
      ))
      .parallel <- FALSE
    }

    # Validate all inputs have same length or length 1
    max_length <- max(input_lengths)
    invalid_lengths <- input_lengths[
      input_lengths != 1 & input_lengths != max_length
    ]

    if (length(invalid_lengths) > 0) {
      cli::cli_abort(c(
        "All inputs must have the same length or length 1 for batch processing",
        "x" = "Got lengths: {.val {input_lengths}}",
        "i" = "Either make all inputs the same length, or use length 1 for scalar values"
      ))
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
      .return_format,
      .parallel_method,
      .cache
    ))
  }

  # Single input processing
  # Note: ellmer handles retries internally (configurable via options(ellmer_max_tries))
  result <- module$forward(inputs, .llm = .llm, trace = TRUE, .cache = .cache)

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

#' Normalize module runtime configuration
#' @noRd
normalize_module_config <- function(config) {
  if (is.null(config)) {
    config <- list()
  }

  if (!is.list(config)) {
    cli::cli_abort("{.arg config} must be a list")
  }

  params <- config$params %||% list()
  for (name in runtime_param_names()) {
    if (is.null(params[[name]]) && !is.null(config[[name]])) {
      params[[name]] <- config[[name]]
    }
  }

  if (length(params) > 0) {
    config$params <- params
  }

  config
}

#' Runtime parameter names forwarded through ellmer
#' @noRd
runtime_param_names <- function() {
  c(
    "temperature",
    "top_p",
    "reasoning_effort",
    "frequency_penalty",
    "presence_penalty",
    "max_output_tokens",
    "service_tier"
  )
}

#' Legacy config fields that should no longer create Chat clients
#' @noRd
legacy_chat_config_fields <- function() {
  c("provider", "model", "api_args", "base_url", "credentials", "max_tokens")
}

#' Infer the logical module kind
#' @noRd
module_kind <- function(module) {
  module$config$.module_kind %||% switch(
    class(module)[1],
    "ReactModule" = "react",
    "MultiChainComparisonModule" = "multichain",
    "PredictModule" = "predict",
    "predict"
  )
}

#' Resolve the Chat to use for module execution
#' @noRd
resolve_module_llm <- function(
  module,
  .llm = NULL,
  create = TRUE,
  extra_params = NULL
) {
  ignored_fields <- intersect(names(module$config %||% list()), legacy_chat_config_fields())
  llm <- .llm %||% module$chat %||% get_default_chat(create = FALSE)

  if (is.null(llm)) {
    if (length(ignored_fields) > 0) {
      cli::cli_abort(c(
        "Module config no longer creates Chat clients",
        "i" = "Ignored fields: {.field {ignored_fields}}",
        "i" = "Attach a Chat with {.code module(..., chat = chat)} or pass {.code .llm = chat}",
        "i" = "Or configure a default Chat with {.code set_default_chat()} or {.code dsp_configure()}"
      ))
    }

    if (!create) {
      return(NULL)
    }

    llm <- get_default_chat(create = TRUE)
  } else if (length(ignored_fields) > 0) {
    cli::cli_warn(
      c(
        "Ignoring module config fields that no longer create Chats",
        "i" = "Ignored fields: {.field {ignored_fields}}",
        "i" = "Runtime now comes from {.arg .llm}, {.code module$chat}, or the default Chat"
      ),
      .frequency = "once",
      .frequency_id = paste0("legacy-chat-config-", module_kind(module))
    )
  }

  params <- module_runtime_params(module, extra_params = extra_params)
  if (length(params) == 0) {
    return(llm)
  }

  apply_chat_params(llm, params)
}

#' Collect runtime params from module config
#' @noRd
module_runtime_params <- function(module, extra_params = NULL) {
  config <- normalize_module_config(module$config %||% list())
  params <- config$params %||% list()

  if (!is.null(extra_params) && length(extra_params) > 0) {
    for (name in names(extra_params)) {
      params[[name]] <- extra_params[[name]]
    }
  }

  params[!vapply(params, is.null, logical(1))]
}

#' Clone a Chat and apply runtime params to its provider
#' @noRd
apply_chat_params <- function(chat, params) {
  if (is.null(chat) || length(params) == 0) {
    return(chat)
  }

  cloned <- tryCatch(
    {
      if (is.function(chat$clone)) {
        chat$clone(deep = TRUE)
      } else {
        chat
      }
    },
    error = function(e) chat
  )

  provider <- tryCatch(
    cloned$.__enclos_env__$private$provider,
    error = function(e) NULL
  )
  if (is.null(provider)) {
    return(cloned)
  }

  existing_args <- tryCatch(provider@extra_args, error = function(e) list())
  if (is.null(existing_args)) {
    existing_args <- list()
  }

  for (name in names(params)) {
    existing_args[[name]] <- params[[name]]
  }

  tryCatch(
    {
      cloned$.__enclos_env__$private$provider@extra_args <- existing_args
    },
    error = function(e) NULL
  )

  cloned
}

#' Determine if a value is an ellmer content object
#' @noRd
is_content_input <- function(x) {
  inherits(x, "Content") || any(grepl("^Content", class(x)))
}

#' Build the canonical request payload for a module execution
#' @noRd
build_module_request <- function(module, inputs) {
  prompt <- build_prompt(module, inputs)
  instructions <- module$signature@instructions %||% ""
  full_prompt <- if (nzchar(instructions) && nzchar(prompt)) {
    paste(instructions, prompt, sep = "\n\n")
  } else if (nzchar(instructions)) {
    instructions
  } else {
    prompt
  }

  content_inputs <- unname(Filter(is_content_input, inputs))
  payload <- if (length(content_inputs) > 0) {
    c(list(ellmer::ContentText(full_prompt)), content_inputs)
  } else {
    full_prompt
  }

  list(
    prompt = prompt,
    instructions = instructions,
    full_prompt = full_prompt,
    payload = payload,
    is_multimodal = length(content_inputs) > 0
  )
}

#' Execute a structured ellmer call from a prepared request
#' @noRd
call_llm_request <- function(llm, request, output_type, .cache = NULL) {
  if (isTRUE(request$is_multimodal)) {
    llm$chat_structured(
      request$payload,
      type = output_type,
      echo = "none"
    )
  } else {
    cached_chat_structured(
      llm = llm,
      prompt = request$full_prompt,
      output_type = output_type,
      .cache = .cache
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
  .return_format,
  .cache = NULL
) {
  request <- build_module_request(module, input_set)
  prompt <- request$prompt

  if (.verbose) {
    cli::cli_h3("Prompt {index}")
    cli::cli_code(prompt)
  }

  start_time <- Sys.time()

  response <- call_llm_request(
    llm = llm,
    request = request,
    output_type = module$signature@output_type,
    .cache = .cache
  )

  end_time <- Sys.time()
  latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
    1000

  if (.return_format == "simple") {
    extract_simple_output(response, module$signature@output_type)
  } else {
    instructions <- request$instructions

    list(
      output = response,
      chat = mock_batch_chat(request$payload, response, llm),
      metadata = list(
        latency_ms = latency_ms,
        prompt_length = nchar(prompt),
        prompt = prompt,
        instructions = instructions,
        timestamp = end_time,
        batch_index = index
      )
    )
  }
}

#' Create a mock chat with recorded prompt/response for logging
#'
#' Creates a Chat object with synthetic turns representing the prompt and response.
#' This is used when using parallel_chat_structured which doesn't return per-request
#' chat histories. Follows the same pattern as vitals::generate_structured().
#'
#' @param prompt The prompt that was sent
#' @param response The response received (will be JSON-serialized if not string)
#' @param chat A Chat object to clone and populate with the mock turns
#' @return A cloned Chat with UserTurn and AssistantTurn representing the exchange
#' @noRd
mock_batch_chat <- function(prompt, response, chat) {
  mock <- tryCatch(
    {
      if (is.function(chat$clone)) chat$clone() else chat
    },
    error = function(e) chat
  )

  prompt_contents <- if (
    is.list(prompt) &&
      length(prompt) > 0 &&
      all(vapply(prompt, is_content_input, logical(1)))
  ) {
    prompt
  } else {
    list(ellmer::ContentText(as.character(prompt)))
  }

  user_turn <- ellmer::UserTurn(contents = prompt_contents)

  # Serialize response to JSON if it's structured data
  response_text <- if (is.character(response) && length(response) == 1) {
    response
  } else {
    as.character(jsonlite::toJSON(response, auto_unbox = TRUE))
  }

  assistant_turn <- ellmer::AssistantTurn(
    contents = list(ellmer::ContentText(response_text))
  )

  if (is.function(mock$set_turns)) {
    mock$set_turns(list(user_turn, assistant_turn))
  }
  mock
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
    # Safely check if response is a list or environment with the field
    if (
      (is.list(response) || is.environment(response)) &&
        field_name %in% names(response)
    ) {
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
  .return_format,
  .parallel_method = "mirai",
  .cache = NULL
) {
  input_sets <- lapply(seq_len(n), function(i) lapply(inputs, `[[`, i))

  # Determine execution method
  if (!.parallel || n <= 1) {
    results <- run_batch_sequential(
      module,
      input_sets,
      n,
      .llm,
      .verbose,
      .return_format,
      .progress,
      .cache
    )
  } else if (.parallel_method == "ellmer") {
    # Use ellmer's parallel_chat_structured for native parallelism
    results <- run_batch_ellmer_parallel(
      module,
      input_sets,
      n,
      .llm,
      .verbose,
      .return_format,
      .progress,
      .cache
    )
  } else {
    # Default: mirai-based parallelism
    results <- run_batch_parallel(
      module,
      input_sets,
      n,
      .llm,
      .verbose,
      .return_format,
      .progress,
      .cache
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
  .progress,
  .cache = NULL
) {
  shared_llm <- resolve_module_llm(module, .llm = .llm)
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
    prompt <- build_module_request(module, input_sets[[i]])$prompt

    results[[i]] <- tryCatch(
      {
        process_batch_item(
          input_set = input_sets[[i]],
          module = module,
          llm = shared_llm,
          index = i,
          .verbose = .verbose,
          .return_format = .return_format,
          .cache = .cache
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

#' Run batch processing using ellmer's parallel_chat_structured
#'
#' Uses ellmer's native parallel processing for structured outputs.
#' This approach is more efficient as it handles parallel HTTP requests
#' internally without the overhead of spawning R processes.
#'
#' @noRd
run_batch_ellmer_parallel <- function(
  module,
  input_sets,
  n,
  .llm,
  .verbose,
  .return_format,
  .progress,
  .cache = NULL
) {
  # Build prompts for all inputs (as list, required by ellmer::parallel_chat_structured)
  requests <- lapply(input_sets, function(input_set) {
    build_module_request(module, input_set)
  })
  prompts <- lapply(requests, `[[`, "payload")

  # Get the Chat provider - need to clone for parallel use
  chat <- resolve_module_llm(module, .llm = .llm)

  # Check if ellmer has parallel_chat_structured
  if (!exists("parallel_chat_structured", envir = asNamespace("ellmer"))) {
    if (!is.null(.llm)) {
      # Can't use mirai with a custom .llm (not safe to share across workers)
      cli::cli_warn(c(
        "ellmer::parallel_chat_structured() not available",
        "i" = "Falling back to sequential processing",
        "i" = "Update ellmer to enable native parallel processing: {.code pak::pak('tidyverse/ellmer')}"
      ))
      return(run_batch_sequential(
        module,
        input_sets,
        n,
        .llm,
        .verbose,
        .return_format,
        .progress,
        .cache
      ))
    } else {
      cli::cli_warn(c(
        "ellmer::parallel_chat_structured() not available",
        "i" = "Falling back to mirai-based parallelism",
        "i" = "Update ellmer to enable native parallel processing: {.code pak::pak('tidyverse/ellmer')}"
      ))
      return(run_batch_parallel(
        module,
        input_sets,
        n,
        .llm,
        .verbose,
        .return_format,
        .progress,
        .cache
      ))
    }
  }

  # Show progress indication
  if (.progress && n > 1) {
    cli::cli_progress_step(
      "Processing {n} items with ellmer parallel...",
      spinner = TRUE
    )
  }

  start_time <- Sys.time()

  # Call ellmer's parallel_chat_structured
  responses <- tryCatch(
    {
      ellmer::parallel_chat_structured(
        chat = chat,
        prompts = prompts,
        type = module$signature@output_type
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Parallel LLM call failed",
          "x" = e$message,
          "i" = "Try sequential processing with {.code .parallel = FALSE}"
        ),
        parent = e
      )
    }
  )

  end_time <- Sys.time()
  total_latency <- as.numeric(difftime(end_time, start_time, units = "secs")) *
    1000

  if (.progress && n > 1) {
    cli::cli_progress_done()
  }

  # Validate response format

  if (is.null(responses)) {
    cli::cli_abort(c(
      "parallel_chat_structured() returned NULL",
      "i" = "This may indicate an API failure or empty response"
    ))
  }

  # Normalize responses to list-of-lists format
  # parallel_chat_structured returns a tibble where each row is a response
  if (is.data.frame(responses)) {
    if (nrow(responses) != n) {
      cli::cli_abort(c(
        "Response count mismatch from parallel_chat_structured()",
        "x" = "Expected {n} responses, got {nrow(responses)}",
        "i" = "Some requests may have failed silently"
      ))
    }
    responses_list <- lapply(
      seq_len(nrow(responses)),
      function(i) as.list(responses[i, ])
    )
  } else if (is.list(responses)) {
    if (length(responses) != n) {
      cli::cli_abort(c(
        "Response count mismatch from parallel_chat_structured()",
        "x" = "Expected {n} responses, got {length(responses)}",
        "i" = "Some requests may have failed silently"
      ))
    }
    responses_list <- responses
  } else {
    cli::cli_abort(c(
      "Unexpected response format from parallel_chat_structured()",
      "x" = "Got {.cls {class(responses)[1]}} instead of data.frame or list",
      "i" = "This may be a version mismatch with ellmer"
    ))
  }

  # Format results
  if (.return_format == "simple") {
    results <- purrr::map(responses_list, function(response) {
      extract_simple_output(response, module$signature@output_type)
    })
  } else {
    # Create mock chats for each request with recorded prompt/response
    # This follows the same pattern as vitals::generate_structured()
    results <- purrr::map2(
      responses_list,
      seq_along(responses_list),
      function(response, i) {
        list(
          output = response,
          chat = mock_batch_chat(prompts[[i]], response, chat),
          metadata = list(
            latency_ms = total_latency / n,
            prompt_length = nchar(requests[[i]]$prompt),
            prompt = requests[[i]]$prompt,
            instructions = requests[[i]]$instructions,
            timestamp = end_time,
            batch_index = i,
            parallel_method = "ellmer"
          )
        )
      }
    )
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
  .progress,
  .cache = NULL
) {
  llm_factory <- if (!is.null(.llm)) {
    function() resolve_module_llm(module, .llm = .llm)
  } else if (!is.null(module$chat) || !is.null(get_default_chat(create = FALSE))) {
    function() resolve_module_llm(module)
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
      .cache,
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
            .return_format = .return_format,
            .cache = .cache
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
      .cache = .cache,
      process_batch_item_fn = process_batch_item,
      extract_simple_output_fn = extract_simple_output,
      build_prompt_fn = build_prompt,
      call_llm_fn = call_llm,
      llm_factory = llm_factory,
      create_error_result_fn = create_error_result
    )
  )

  # Collect results with timeout protection
  results <- vector("list", n)
  completed <- 0

  # Create progress bar for parallel processing
  progress_id <- NULL
  if (.progress && n > 1) {
    progress_id <- cli::cli_progress_bar(
      format = "Processing {cli::pb_current}/{cli::pb_total} | {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = n,
      clear = FALSE
    )
  }

  warnings_to_emit <- character(0)
  max_wait_seconds <- getOption("dsprrr.parallel_timeout", 600) # 10 min default
  max_iterations <- max_wait_seconds * 100 # 0.01s sleep per iteration
  iterations <- 0

  while (completed < n && iterations < max_iterations) {
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

        if (!is.null(progress_id)) {
          cli::cli_progress_update(id = progress_id)
        }
      }
    }
    Sys.sleep(0.01)
    iterations <- iterations + 1
  }

  # Handle timeout

  if (completed < n) {
    n_incomplete <- n - completed
    cli::cli_warn(c(
      "Parallel processing timed out after {max_wait_seconds} seconds",
      "x" = "{n_incomplete} of {n} tasks did not complete",
      "i" = "Increase timeout with {.code options(dsprrr.parallel_timeout = seconds)}"
    ))
    # Fill incomplete results with error markers
    for (i in seq_len(n)) {
      if (is.null(results[[i]])) {
        results[[i]] <- create_error_result(
          error = list(message = "Task timed out"),
          index = i,
          prompt = NA_character_,
          instructions = NA_character_,
          llm = NULL,
          .return_format = .return_format
        )
      }
    }
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
    if (grepl("\\{\\{[^}]+\\}\\}", module$template)) {
      filled_template <- rlang::inject(
        ellmer::interpolate(module$template, !!!inputs)
      )
    } else {
      filled_template <- glue::glue_data(
        .x = inputs,
        module$template,
        .open = "{",
        .close = "}",
        .envir = parent.frame()
      )
    }
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
        demo_lines <- c(
          demo_lines,
          paste0(name, ": ", format_prompt_value(demo$inputs[[name]]))
        )
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
    value <- format_prompt_value(inputs[[name]])
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

#' Format a value for prompt rendering
#' @noRd
format_prompt_value <- function(value) {
  if (is_content_input(value)) {
    return(paste0("<", class(value)[1], ">"))
  }

  if (is.list(value) && !is.null(names(value))) {
    return(jsonlite::toJSON(value, auto_unbox = TRUE, pretty = FALSE))
  }

  paste(value, collapse = ", ")
}

#' Get default LLM configuration
#'
#' Checks module's stored Chat, then auto-detects from environment
#' variables using get_default_chat().
#'
#' @param module The module to get an LLM for
#' @return An ellmer Chat object
#' @noRd
get_default_llm <- function(module) {
  resolve_module_llm(module, create = TRUE)
}

#' Call the LLM with structured output
#'
#' @noRd
call_llm <- function(
  llm,
  prompt,
  output_type,
  instructions = "",
  verbose = FALSE,
  inputs = list(),
  .cache = NULL
) {
  request <- list(
    prompt = prompt,
    instructions = instructions,
    full_prompt = if (nchar(instructions) > 0) {
      paste(instructions, prompt, sep = "\n\n")
    } else {
      prompt
    },
    payload = if (length(Filter(is_content_input, inputs)) > 0) {
      c(
        list(ellmer::ContentText(if (nchar(instructions) > 0) {
          paste(instructions, prompt, sep = "\n\n")
        } else {
          prompt
        })),
        unname(Filter(is_content_input, inputs))
      )
    } else {
      if (nchar(instructions) > 0) {
        paste(instructions, prompt, sep = "\n\n")
      } else {
        prompt
      }
    },
    is_multimodal = length(Filter(is_content_input, inputs)) > 0
  )

  tryCatch(
    call_llm_request(
      llm = llm,
      request = request,
      output_type = output_type,
      .cache = .cache
    ),
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
#' @param .parallel_method Character, either "ellmer" (default) or "mirai".
#'   "ellmer" uses ellmer's `parallel_chat_structured()` for native async HTTP
#'   parallelism (more efficient, single process).
#'   "mirai" uses mirai for multi-process parallelism (requires `.llm = NULL`).
#' @param .progress Logical whether to show progress bar
#' @param .return_format Character either "simple" or "structured"
#' @export
run_dataset.Module <- function(
  module,
  data,
  .llm = NULL,
  .verbose = FALSE,
  .parallel = FALSE,
  .parallel_method = c("ellmer", "mirai"),
  .progress = TRUE,
  .return_format = "simple",
  ...
) {
  .parallel_method <- match.arg(.parallel_method)
  # Validate data
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "{.arg data} must be a data frame or tibble",
      "x" = "Got {.cls {class(data)[1]}}",
      "i" = "Use {.code data.frame()} or {.code tibble::tibble()} to create one"
    ))
  }

  # Get required input names from signature
  sig_inputs <- module$signature@inputs
  if (length(sig_inputs) > 0) {
    required_names <- vapply(sig_inputs, function(x) x$name, character(1))
    missing_cols <- setdiff(required_names, names(data))

    if (length(missing_cols) > 0) {
      available_cols <- names(data)

      # Build error message with "Did you mean?" suggestions
      msg <- c("{.arg data} missing required columns: {.field {missing_cols}}")

      # Check for typos in column names
      for (missing in missing_cols) {
        suggestion <- suggest_match(missing, available_cols)
        if (!is.null(suggestion)) {
          msg <- c(msg, "i" = suggestion)
        }
      }

      msg <- c(
        msg,
        "i" = "Signature expects: {.field {required_names}}",
        if (length(available_cols) > 0) {
          c("i" = "Data has: {.field {available_cols}}")
        }
      )

      cli::cli_abort(msg)
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
        .parallel_method = .parallel_method,
        .progress = .progress,
        .return_format = .return_format
      ),
      list(...) # Pass through additional arguments like .cache
    )
  )

  if (.return_format == "structured" && inherits(results, "dsprrr_result")) {
    results <- list(results)
  }

  # Add results to data
  if (.return_format == "simple") {
    if (length(results) != nrow(data)) {
      results <- list(results)
    }
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

#' Print method for dsprrr_batch_result
#' @param x A dsprrr_batch_result object
#' @param ... Additional arguments (unused)
#' @export
print.dsprrr_batch_result <- function(x, ...) {
  n <- length(x)

  cli::cli_h3("DSPrrr Batch Results")
  cli::cli_text("{.field Items}: {n}")

  # Count successes vs errors
  n_errors <- sum(vapply(
    x,
    function(item) {
      isTRUE(item$error) || inherits(item$output, "error")
    },
    logical(1)
  ))

  if (n_errors > 0) {
    cli::cli_alert_warning("{.field Errors}: {n_errors} of {n} items")
  } else {
    cli::cli_alert_success("All items completed successfully")
  }

  # Show first few results
  if (n > 0 && n <= 3) {
    cli::cli_text("{.field Outputs}:")
    for (i in seq_len(n)) {
      output <- x[[i]]$output
      if (is.character(output)) {
        cli::cli_text(
          "  [{i}] {substr(output, 1, 50)}{if (nchar(output) > 50) '...' else ''}"
        )
      } else if (is.list(output)) {
        cli::cli_text("  [{i}] <list> with {length(output)} elements")
      } else {
        cli::cli_text("  [{i}] <{class(output)[1]}>")
      }
    }
  } else if (n > 3) {
    cli::cli_text("{.field First 3 outputs}:")
    for (i in 1:3) {
      output <- x[[i]]$output
      if (is.character(output)) {
        cli::cli_text(
          "  [{i}] {substr(output, 1, 50)}{if (nchar(output) > 50) '...' else ''}"
        )
      } else if (is.list(output)) {
        cli::cli_text("  [{i}] <list> with {length(output)} elements")
      } else {
        cli::cli_text("  [{i}] <{class(output)[1]}>")
      }
    }
    cli::cli_text("  ... and {n - 3} more")
  }

  invisible(x)
}
