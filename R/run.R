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
#'     \item{.concurrency}{A validated policy created by
#'       [concurrency_control()]. When supplied, do not also pass `.parallel` or
#'       `.parallel_method`.}
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
#' Zero-length inputs form an empty batch only when every input is zero length.
#' Empty batches return immediately without resolving a Chat or touching cache,
#' trace, or prompt-history state. Mixing zero-length and non-empty inputs is an
#' error.
#'
#' Scalar and batch Predict calls record one trace per attempted row. Structured
#' metadata reports usage, error, cache, backend, and batch-index fields. Native
#' ellmer and mirai workers return row records that are committed to module and
#' global trace state by the parent in input order. Specialized Predict
#' subclasses, such as ReAct, preserve their scalar `forward()` method and
#' currently reject vectorized inputs rather than bypassing specialized logic.
#'
#' @return For single inputs with .return_format="simple": The parsed output according to the module's signature.
#'   For single inputs with .return_format="structured": A list with components:
#'   - output: The parsed output
#'   - chat: The ellmer chat object used
#'   - metadata: Additional metadata (tokens used, latency, etc.)
#'
#'   For batch inputs: A list of results matching the input length. Empty
#'   batches return a zero-length list (with class `dsprrr_batch_result` for
#'   structured output).
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

#' Validate the `.cache` argument
#'
#' Shared by [run()] methods so every module type rejects malformed `.cache`
#' values the same way.
#' @noRd
validate_cache_arg <- function(.cache) {
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
  invisible(.cache)
}

#' Validate and classify vectorized module inputs
#'
#' Scalar values may be recycled across a positive-size batch. Zero-length
#' values are compatible only with other zero-length values: recycling a scalar
#' into an empty batch would hide a likely data-shape bug and used to trigger a
#' provider call for a dataset with no rows.
#' @noRd
batch_input_contract <- function(inputs) {
  input_lengths <- lengths(inputs)
  if (length(input_lengths) == 0L) {
    return(list(kind = "scalar", size = 1L, lengths = input_lengths))
  }

  if (any(input_lengths == 0L)) {
    if (all(input_lengths == 0L)) {
      return(list(kind = "empty", size = 0L, lengths = input_lengths))
    }

    cli::cli_abort(
      c(
        "Zero-length inputs cannot be mixed with non-empty inputs",
        "x" = "Got lengths: {.val {input_lengths}}",
        "i" = "Use zero-length values for every input in an empty batch."
      ),
      class = "dsprrr_batch_length_error",
      input_lengths = input_lengths
    )
  }

  max_length <- max(input_lengths)
  invalid <- input_lengths != 1L & input_lengths != max_length
  if (any(invalid)) {
    cli::cli_abort(
      c(
        "All batch inputs must have the same length or length 1",
        "x" = "Got lengths: {.val {input_lengths}}",
        "i" = "Use length 1 only for values that should be recycled."
      ),
      class = "dsprrr_batch_length_error",
      input_lengths = input_lengths
    )
  }

  list(
    kind = if (max_length > 1L) "batch" else "scalar",
    size = as.integer(max_length),
    lengths = input_lengths
  )
}

#' Construct an empty batch result without touching runtime state
#' @noRd
empty_batch_result <- function(.return_format) {
  result <- list()
  if (identical(.return_format, "structured")) {
    class(result) <- c("dsprrr_batch_result", "list")
  }
  result
}

#' @export
run.Module <- function(
  module,
  ...,
  .llm = NULL,
  .verbose = FALSE,
  .parallel = FALSE,
  .parallel_method = c("ellmer", "mirai"),
  .concurrency = NULL,
  .progress = TRUE,
  .return_format = "simple",
  .show_prompt = FALSE,
  .cache = NULL
) {
  parallel_missing <- missing(.parallel)
  parallel_method_missing <- missing(.parallel_method)
  concurrency_missing <- missing(.concurrency)
  concurrency <- resolve_concurrency_control(
    .concurrency = .concurrency,
    concurrency_missing = concurrency_missing,
    .parallel = .parallel,
    parallel_missing = parallel_missing,
    .parallel_method = .parallel_method,
    parallel_method_missing = parallel_method_missing
  )
  explicit_concurrency <- !concurrency_missing && !is.null(.concurrency)
  .parallel_method <- match.arg(.parallel_method)
  validate_cache_arg(.cache)

  inputs <- list(...)
  input_contract <- batch_input_contract(inputs)
  concurrency_runtime <- NULL
  if (identical(input_contract$kind, "batch")) {
    runtime_chat <- .llm %||% module$chat %||%
      get_default_chat(create = FALSE)
    concurrency_runtime <- normalize_concurrency_runtime(
      concurrency,
      .llm = .llm,
      .chat = runtime_chat
    )
    if (!identical(concurrency_runtime$effective_backend, "sequential")) {
      cli::cli_abort(
        c(
          "Concurrent batch execution is not supported for this module",
          "x" = "{.cls {class(module)[1]}} does not implement the isolated Predict row contract.",
          "i" = "Use sequential execution or run scalar inputs."
        ),
        class = c(
          "dsprrr_batch_unsupported_module",
          "dsprrr_concurrency_unsupported_error"
        ),
        module_class = class(module)[1]
      )
    }
  }

  # Show prompt preview if requested
  if (.show_prompt) {
    show_prompt_preview(module)
  }

  # Delegate to the module's run method
  execution_args <- list(
    .llm = .llm,
    .verbose = .verbose,
    .progress = .progress,
    .return_format = .return_format,
    .cache = .cache
  )
  if (!is.null(concurrency_runtime)) {
    execution_args$.concurrency_runtime <- concurrency_runtime
  } else if (explicit_concurrency) {
    execution_args$.concurrency <- concurrency
  } else {
    execution_args$.parallel <- .parallel
    execution_args$.parallel_method <- .parallel_method
  }
  do.call(module$run, c(inputs, execution_args))
}

#' @export
run.PredictModule <- function(
  module,
  ...,
  .llm = NULL,
  .verbose = FALSE,
  .parallel = FALSE,
  .parallel_method = c("ellmer", "mirai"),
  .concurrency = NULL,
  .progress = TRUE,
  .return_format = "simple",
  .show_prompt = FALSE,
  .cache = NULL
) {
  parallel_missing <- missing(.parallel)
  parallel_method_missing <- missing(.parallel_method)
  concurrency_missing <- missing(.concurrency)
  concurrency <- resolve_concurrency_control(
    .concurrency = .concurrency,
    concurrency_missing = concurrency_missing,
    .parallel = .parallel,
    parallel_missing = parallel_missing,
    .parallel_method = .parallel_method,
    parallel_method_missing = parallel_method_missing
  )

  # Validate .cache parameter
  validate_cache_arg(.cache)

  # Show prompt preview if requested
  if (.show_prompt) {
    show_prompt_preview(module)
  }

  # Capture input arguments
  inputs <- list(...)

  # Validate return format
  .return_format <- match.arg(.return_format, c("simple", "structured"))

  # Validate inputs against signature
  validate_signature_inputs(
    module$signature,
    inputs,
    missing = "error",
    extra = "warn",
    type = "warn",
    context = "inputs"
  )

  input_contract <- batch_input_contract(inputs)

  if (identical(input_contract$kind, "empty")) {
    return(empty_batch_result(.return_format))
  }

  if (identical(input_contract$kind, "batch")) {
    if (!identical(class(module)[1], "PredictModule")) {
      cli::cli_abort(
        c(
          "Batch execution is not yet supported for specialized Predict modules",
          "x" = "{.cls {class(module)[1]}} overrides the row execution contract.",
          "i" = "Run scalar inputs so the module's specialized {.fn forward} method is preserved."
        ),
        class = "dsprrr_batch_unsupported_module",
        module_class = class(module)[1]
      )
    }

    runtime_chat <- .llm %||% module$chat %||%
      get_default_chat(create = FALSE)
    concurrency <- normalize_concurrency_runtime(
      concurrency,
      .llm = .llm,
      .chat = runtime_chat
    )

    # Expand scalar inputs to match batch size
    inputs <- lapply(inputs, function(x) {
      if (length(x) == 1L) rep(x, input_contract$size) else x
    })

    # Process batch
    return(run_batch(
      module,
      inputs,
      input_contract$size,
      .llm,
      .verbose,
      .progress,
      .return_format,
      .cache,
      concurrency
    ))
  }

  if (!identical(class(module)[1], "PredictModule")) {
    return(run_predict_forward(
      module = module,
      inputs = inputs,
      .llm = .llm,
      .verbose = .verbose,
      .return_format = .return_format,
      .cache = .cache
    ))
  }

  run_predict_scalar(
    module = module,
    inputs = inputs,
    .llm = .llm,
    .verbose = .verbose,
    .return_format = .return_format,
    .cache = .cache
  )
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
    "max_tokens",
    "max_output_tokens",
    "service_tier"
  )
}

#' Legacy config fields that should no longer create Chat clients
#' @noRd
legacy_chat_config_fields <- function() {
  c("provider", "model", "api_args", "base_url", "credentials")
}

#' Infer the logical module kind
#' @noRd
module_kind <- function(module) {
  module$config$.module_kind %||%
    switch(
      class(module)[1],
      "ReactModule" = "react",
      "MultiChainComparisonModule" = "multichain",
      "PredictModule" = "predict",
      # Fall back to the actual class name rather than silently claiming
      # "predict" -- otherwise unsupported module types (pipelines, ensembles,
      # RAG, ...) pass the persistence allow-list and serialize as a bare
      # PredictModule, losing all of their structure.
      class(module)[1]
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
  ignored_fields <- intersect(
    names(module$config %||% list()),
    legacy_chat_config_fields()
  )
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
        cli::cli_warn(
          c(
            "Chat object does not support cloning",
            "i" = "Runtime parameters will be applied to the original Chat",
            "i" = "This may cause unexpected behavior in batch/optimization contexts"
          ),
          .frequency = "once",
          .frequency_id = "chat-clone-unsupported"
        )
        chat
      }
    },
    error = function(e) {
      cli::cli_warn(
        c(
          "Failed to clone Chat for parameter isolation",
          "x" = e$message,
          "i" = "Runtime parameters will be applied to the original Chat"
        ),
        .frequency = "once",
        .frequency_id = "chat-clone-failed"
      )
      chat
    }
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
    error = function(e) {
      param_names <- paste(names(params), collapse = ", ")
      cli::cli_warn(
        c(
          "Failed to apply runtime parameters to Chat provider",
          "x" = "Parameters not applied: {.field {param_names}}",
          "i" = "The module will run with the provider's default settings",
          "i" = "Error: {e$message}"
        ),
        .frequency = "once",
        .frequency_id = "chat-params-failed"
      )
    }
  )

  cloned
}

#' Determine if a value is an ellmer content object
#' @noRd
is_content_input <- function(x) {
  inherits(x, "ellmer::Content") ||
    any(grepl("(^|::)Content", class(x)))
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
call_llm_request <- function(
  llm,
  request,
  output_type,
  .cache = NULL,
  rollout_id = NULL,
  .observer = NULL
) {
  cached_chat_structured(
    llm = llm,
    prompt = request$payload,
    output_type = output_type,
    rollout_id = rollout_id,
    .cache = .cache,
    .observer = .observer
  )
}

#' Return only the verified turn delta created by the current invocation
#' @noRd
verified_chat_turn_delta <- function(chat, turns_before) {
  if (is.null(turns_before)) {
    return(NULL)
  }
  turns_after <- batch_chat_turns(chat)
  if (is.null(turns_after) || length(turns_after) < length(turns_before)) {
    return(NULL)
  }
  before_n <- length(turns_before)
  if (
    before_n > 0L &&
      !identical(turns_after[seq_len(before_n)], turns_before)
  ) {
    return(NULL)
  }
  if (length(turns_after) == before_n) {
    return(list())
  }
  turns_after[seq.int(before_n + 1L, length(turns_after))]
}

#' Extract usage metadata from a verified current-call assistant turn
#'
#' @param chat An ellmer Chat or compatible object
#' @param turns_before Verified Chat history captured immediately before the call
#' @return Named list of token and cost fields; unknown values remain `NA`
#' @noRd
chat_usage_metadata <- function(chat, turns_before = NULL) {
  turn_delta <- verified_chat_turn_delta(chat, turns_before)
  assistant_turns <- if (is.null(turn_delta)) {
    list()
  } else {
    Filter(
      function(turn) inherits(turn, "ellmer::AssistantTurn"),
      turn_delta
    )
  }
  assistant_turn <- if (length(assistant_turns) > 0L) {
    assistant_turns[[length(assistant_turns)]]
  } else {
    NULL
  }
  if (is.null(assistant_turn)) {
    return(list(
      input_tokens = NA_integer_,
      output_tokens = NA_integer_,
      cached_input_tokens = NA_integer_,
      total_tokens = NA_integer_,
      cost = NA_real_,
      duration_s = NA_real_
    ))
  }

  tokens <- tryCatch(assistant_turn@tokens, error = function(e) NULL)
  input_tokens <- as.integer(tokens[1] %||% NA_integer_)
  output_tokens <- as.integer(tokens[2] %||% NA_integer_)
  cached_input_tokens <- as.integer(tokens[3] %||% NA_integer_)
  total_tokens <- if (anyNA(c(input_tokens, output_tokens))) {
    NA_integer_
  } else {
    input_tokens + output_tokens
  }

  list(
    input_tokens = input_tokens,
    output_tokens = output_tokens,
    cached_input_tokens = cached_input_tokens,
    total_tokens = total_tokens,
    cost = tryCatch(assistant_turn@cost, error = function(e) NA_real_),
    duration_s = tryCatch(
      assistant_turn@duration,
      error = function(e) NA_real_
    )
  )
}

#' Normalize usage fields across every execution backend
#' @noRd
canonical_usage_metadata <- function(usage = list()) {
  defaults <- list(
    input_tokens = NA_integer_,
    output_tokens = NA_integer_,
    cached_input_tokens = NA_integer_,
    total_tokens = NA_integer_,
    cost = NA_real_,
    duration_s = NA_real_
  )
  for (name in intersect(names(usage), names(defaults))) {
    defaults[[name]] <- usage[[name]]
  }
  defaults
}

#' Normalize backend error values without assuming condition structure
#' @noRd
run_error_message <- function(error) {
  if (is.null(error)) {
    return(NA_character_)
  }
  if (inherits(error, "condition")) {
    return(conditionMessage(error))
  }
  if (is.list(error) && !is.null(error$message)) {
    return(as.character(error$message)[1])
  }
  value <- as.character(error)
  if (length(value) == 0L) NA_character_ else value[[1]]
}

#' Extract a stable error class from heterogeneous backend values
#' @noRd
run_error_class <- function(error) {
  if (is.null(error)) {
    return(NA_character_)
  }
  class(error)[1] %||% typeof(error)
}

#' Normalize backend missing-error sentinels to NULL
#' @noRd
normalize_backend_error <- function(error) {
  if (is.null(error) || length(error) == 0L) {
    return(NULL)
  }
  if (is.atomic(error) && length(error) == 1L && is.na(error)) {
    return(NULL)
  }
  error
}

#' Whether an error field contains a real, non-missing failure
#' @noRd
run_error_present <- function(error) {
  if (is.null(error) || length(error) == 0L) {
    return(FALSE)
  }
  if (inherits(error, "condition")) {
    return(TRUE)
  }
  if (is.atomic(error)) {
    values <- error[!is.na(error)]
    if (length(values) == 0L) {
      return(FALSE)
    }
    if (is.character(values)) {
      return(any(nzchar(trimws(values))))
    }
  }
  TRUE
}

#' Create the stable metadata contract shared by scalar and batch calls
#' @noRd
canonical_run_metadata <- function(
  request,
  started_at,
  ended_at,
  usage = list(),
  model = NA_character_,
  error = NULL,
  backend = "sequential",
  batch_index = 1L,
  cache = "unknown"
) {
  usage <- canonical_usage_metadata(usage)
  error_message <- run_error_message(error)
  error_class <- run_error_class(error)
  latency_ms <- as.numeric(difftime(ended_at, started_at, units = "secs")) *
    1000

  c(
    list(
      timestamp = ended_at,
      model = model,
      prompt = request$prompt,
      instructions = request$instructions,
      prompt_length = nchar(request$prompt),
      usage = usage,
      error = error_message,
      error_class = error_class,
      error_stage = if (is.null(error)) NA_character_ else "llm",
      cache = cache,
      backend = backend,
      batch_index = as.integer(batch_index),
      latency_ms = latency_ms
    ),
    concurrency_metadata(),
    usage
  )
}

#' Build canonical trace turns without relying on shared Chat mutation
#' @noRd
canonical_trace_turns <- function(
  request,
  response,
  chat,
  turns_before,
  error = NULL
) {
  turns <- verified_chat_turn_delta(chat, turns_before)
  if (is.null(turns) || (length(turns) == 0L && !is.null(error))) {
    prompt_contents <- if (
      is.list(request$payload) &&
        length(request$payload) > 0L &&
        all(vapply(request$payload, is_content_input, logical(1)))
    ) {
      request$payload
    } else {
      list(ellmer::ContentText(as.character(request$payload)))
    }
    synthetic <- list(ellmer::UserTurn(contents = prompt_contents))
    if (is.null(error)) {
      response_text <- if (is.character(response) && length(response) == 1L) {
        response
      } else {
        as.character(jsonlite::toJSON(response, auto_unbox = TRUE))
      }
      synthetic <- c(
        synthetic,
        list(ellmer::AssistantTurn(
          contents = list(ellmer::ContentText(response_text))
        ))
      )
    }
    turns <- synthetic
  }

  user_turns <- Filter(
    function(turn) inherits(turn, "ellmer::UserTurn"),
    turns
  )
  assistant_turns <- Filter(
    function(turn) inherits(turn, "ellmer::AssistantTurn"),
    turns
  )
  list(
    turns = turns,
    user_turn = if (length(user_turns) > 0L) user_turns[[1]] else NULL,
    assistant_turn = if (length(assistant_turns) > 0L) {
      assistant_turns[[length(assistant_turns)]]
    } else {
      NULL
    }
  )
}

#' Create the canonical trace stored by run()
#' @noRd
canonical_run_trace <- function(
  inputs,
  response,
  request,
  chat,
  turns_before,
  metadata,
  error = NULL,
  store_chat = FALSE
) {
  turn_data <- canonical_trace_turns(
    request = request,
    response = response,
    chat = chat,
    turns_before = turns_before,
    error = error
  )
  trace <- list(
    timestamp = metadata$timestamp,
    inputs = inputs,
    output = response,
    prompt = request$full_prompt,
    instructions = request$instructions,
    user_turn = turn_data$user_turn,
    assistant_turn = turn_data$assistant_turn,
    turns = turn_data$turns,
    latency_ms = metadata$latency_ms,
    tokens = metadata$usage[c(
      "input_tokens",
      "output_tokens",
      "cached_input_tokens",
      "total_tokens"
    )],
    cost = metadata$cost,
    model = metadata$model,
    metadata = metadata
  )
  if (isTRUE(store_chat)) {
    trace$chat <- chat
  }
  trace
}

#' Attach a private trace envelope to an internal row result
#' @noRd
attach_run_trace <- function(result, trace, error = NULL) {
  attr(result, "dsprrr_trace") <- trace
  if (!is.null(error)) {
    attr(result, "dsprrr_error_condition") <- error
  }
  result
}

#' Remove private execution attributes before returning a public value
#' @noRd
strip_run_trace <- function(result) {
  attr(result, "dsprrr_trace") <- NULL
  attr(result, "dsprrr_error_condition") <- NULL
  attr(result, "error_message") <- NULL
  result
}

#' Commit worker traces once, in deterministic input order
#' @noRd
commit_run_traces <- function(module, traces) {
  if (length(traces) == 0L) {
    return(invisible(module))
  }
  missing <- which(vapply(traces, is.null, logical(1)))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "Batch execution did not return a trace for every attempted row",
        "x" = "Missing trace rows: {.val {missing}}"
      ),
      class = "dsprrr_trace_contract_error",
      missing_rows = missing
    )
  }

  module$state$traces <- append(module$state$traces, traces)
  for (trace in traces) {
    add_to_global_history(trace, source = "PredictModule")
  }
  invisible(module)
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
  .cache = NULL,
  backend = "sequential",
  .capture_trace = FALSE
) {
  request <- build_module_request(module, input_set)
  prompt <- request$prompt

  if (.verbose) {
    cli::cli_h3("Prompt {index}")
    cli::cli_code(prompt)
  }

  started_at <- Sys.time()
  turns_before <- batch_chat_turns(llm)
  cache_state <- new.env(parent = emptyenv())
  cache_state$status <- "unknown"
  cache_observer <- function(status, ...) {
    cache_state$status <- status
    invisible(NULL)
  }

  response <- tryCatch(
    call_llm_request(
      llm = llm,
      request = request,
      output_type = module$signature@output_type,
      .cache = .cache,
      .observer = cache_observer
    ),
    error = function(e) e
  )
  ended_at <- Sys.time()
  error <- if (inherits(response, "condition")) response else NULL
  usage <- if (is.null(error)) {
    chat_usage_metadata(llm, turns_before = turns_before)
  } else {
    canonical_usage_metadata()
  }
  model <- tryCatch(llm$get_model(), error = function(e) NA_character_)
  metadata <- canonical_run_metadata(
    request = request,
    started_at = started_at,
    ended_at = ended_at,
    usage = usage,
    model = model,
    error = error,
    backend = backend,
    batch_index = index,
    cache = cache_state$status
  )

  if (!is.null(error)) {
    return(create_error_result(
      error = error,
      index = index,
      prompt = request$prompt,
      instructions = request$instructions,
      llm = llm,
      .return_format = .return_format,
      inputs = input_set,
      request = request,
      turns_before = turns_before,
      metadata = metadata,
      module = module,
      .capture_trace = .capture_trace
    ))
  }

  completed_chat <- completed_batch_chat(
    request$payload,
    response,
    llm,
    turns_before = turns_before
  )
  trace <- canonical_run_trace(
    inputs = input_set,
    response = response,
    request = request,
    chat = completed_chat,
    turns_before = turns_before,
    metadata = metadata,
    store_chat = isTRUE(module$config$store_chat_in_traces)
  )
  result <- if (.return_format == "simple") {
    extract_simple_output(response, module$signature@output_type)
  } else {
    list(output = response, chat = completed_chat, metadata = metadata)
  }
  if (isTRUE(.capture_trace)) {
    result <- attach_run_trace(result, trace)
  }
  result
}

#' Preserve a completed stateful batch branch
#'
#' Sequential rows already run on isolated Chat branches. When post-call history
#' contains the original baseline plus a provider-recorded delta, return a deep
#' clone of that completed branch. Chats that record no delta use a synthetic
#' logging Chat instead.
#' @noRd
batch_chat_turns <- function(chat) {
  get_turns <- tryCatch(chat$get_turns, error = function(e) NULL)
  if (!is.function(get_turns)) {
    return(NULL)
  }
  tryCatch(
    {
      turns <- get_turns()
      if (is.list(turns)) turns else NULL
    },
    error = function(e) NULL
  )
}

completed_batch_chat <- function(prompt, response, chat, turns_before = NULL) {
  turns_after <- batch_chat_turns(chat)
  before_n <- length(turns_before)
  recorded <- !is.null(turns_before) &&
    !is.null(turns_after) &&
    length(turns_after) > before_n &&
    (before_n == 0 ||
      identical(
        turns_after[seq_len(before_n)],
        turns_before
      ))
  if (!recorded) {
    return(mock_batch_chat(
      prompt,
      response,
      chat,
      turns_before = turns_before
    ))
  }

  batch_chat_copy(chat, "completed batch result")
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
#' @param turns_before Exact Chat history from before the provider call
#' @return A cloned Chat with the baseline plus synthetic UserTurn and
#'   AssistantTurn representing the exchange
#' @noRd
mock_batch_chat <- function(prompt, response, chat, turns_before = NULL) {
  provider <- if (cache_is_trusted_ellmer_chat(chat)) {
    chat$get_provider()
  } else {
    ellmer::Provider(
      name = "dsprrr",
      model = "synthetic-batch-history",
      base_url = ""
    )
  }
  mock <- getFromNamespace("Chat", "ellmer")$new(provider = provider)

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

  baseline <- if (is.list(turns_before)) {
    rlang::duplicate(turns_before, shallow = FALSE)
  } else {
    rlang::duplicate(
      batch_chat_history(chat) %||% list(),
      shallow = FALSE
    )
  }
  updated <- c(baseline, list(user_turn, assistant_turn))
  tryCatch(
    mock$set_turns(updated),
    error = function(e) {
      cli::cli_abort(
        "Cannot record synthetic batch history",
        class = "dsprrr_chat_isolation_error",
        parent = e
      )
    }
  )
  recorded <- batch_chat_history(mock)
  if (is.null(recorded) || !identical(recorded, updated)) {
    cli::cli_abort(
      "Synthetic batch history could not be verified",
      class = "dsprrr_chat_isolation_error"
    )
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
  .return_format,
  inputs = NULL,
  request = NULL,
  turns_before = NULL,
  metadata = NULL,
  module = NULL,
  .capture_trace = FALSE
) {
  error_message <- run_error_message(error)

  result <- if (.return_format == "simple") {
    structure(
      NA,
      error_message = error_message
    )
  } else {
    list(
      output = NA,
      chat = llm,
      metadata = metadata %||%
        list(
          error = error_message,
          error_class = run_error_class(error),
          error_stage = "llm",
          batch_index = index,
          instructions = instructions,
          prompt = prompt
        )
    )
  }

  if (isTRUE(.capture_trace)) {
    request <- request %||%
      list(
        prompt = prompt,
        instructions = instructions,
        full_prompt = paste(
          Filter(nzchar, c(instructions, prompt)),
          collapse = "\n\n"
        ),
        payload = prompt
      )
    metadata <- metadata %||%
      canonical_run_metadata(
        request = request,
        started_at = Sys.time(),
        ended_at = Sys.time(),
        error = error,
        batch_index = index
      )
    trace <- canonical_run_trace(
      inputs = inputs %||% list(),
      response = NA,
      request = request,
      chat = llm,
      turns_before = turns_before,
      metadata = metadata,
      error = error,
      store_chat = !is.null(module) &&
        isTRUE(module$config$store_chat_in_traces)
    )
    result <- attach_run_trace(result, trace, error = error)
  }
  result
}

#' Construct a typed concurrency boundary condition
#' @noRd
concurrency_condition <- function(message, classes) {
  condition <- simpleError(as.character(message)[1])
  class(condition) <- unique(c(classes, class(condition)))
  condition
}

#' Attach the normalized concurrency contract to one row and its trace
#' @noRd
annotate_concurrency_result <- function(
  result,
  runtime,
  .return_format,
  cancelled = FALSE,
  cancellation_reason = NA_character_
) {
  fields <- concurrency_metadata(
    runtime,
    cancelled = cancelled,
    cancellation_reason = cancellation_reason
  )
  trace <- attr(result, "dsprrr_trace", exact = TRUE)
  if (!is.null(trace)) {
    trace$metadata[names(fields)] <- fields
    trace$metadata$backend <- runtime$effective_backend
    attr(result, "dsprrr_trace") <- trace
  }
  if (identical(.return_format, "structured")) {
    result$metadata[names(fields)] <- fields
    result$metadata$backend <- runtime$effective_backend
  }
  result
}

#' Replace display latency with a monotonic scheduler measurement
#' @noRd
set_concurrency_result_latency <- function(result, latency_ms, .return_format) {
  trace <- attr(result, "dsprrr_trace", exact = TRUE)
  if (!is.null(trace)) {
    trace$latency_ms <- latency_ms
    trace$metadata$latency_ms <- latency_ms
    attr(result, "dsprrr_trace") <- trace
  }
  if (identical(.return_format, "structured")) {
    result$metadata$latency_ms <- latency_ms
  }
  result
}

#' Whether an internal row result represents a failed execution
#' @noRd
concurrency_row_failed <- function(result, .return_format) {
  if (!is.null(attr(result, "dsprrr_error_condition", exact = TRUE))) {
    return(TRUE)
  }
  identical(.return_format, "structured") &&
    run_error_present(result$metadata$error)
}

#' Create an ordered typed row for work rejected by the scheduler
#' @noRd
create_concurrency_error_result <- function(
  module,
  input_set,
  index,
  llm,
  .return_format,
  runtime,
  message,
  classes,
  cancellation_reason,
  started_at = Sys.time()
) {
  request <- build_module_request(module, input_set)
  error <- concurrency_condition(message, classes)
  ended_at <- Sys.time()
  metadata <- canonical_run_metadata(
    request = request,
    started_at = started_at,
    ended_at = ended_at,
    error = error,
    backend = runtime$effective_backend,
    batch_index = index,
    cache = "bypass"
  )
  metadata$error_stage <- "concurrency"
  result <- create_error_result(
    error = error,
    index = index,
    prompt = request$prompt,
    instructions = request$instructions,
    llm = llm,
    .return_format = .return_format,
    inputs = input_set,
    request = request,
    turns_before = batch_chat_turns(llm),
    metadata = metadata,
    module = module,
    .capture_trace = TRUE
  )
  annotate_concurrency_result(
    result,
    runtime,
    .return_format,
    cancelled = TRUE,
    cancellation_reason = cancellation_reason
  )
}

#' Move private row traces onto a backend result container
#' @noRd
collect_backend_traces <- function(results) {
  traces <- lapply(results, attr, which = "dsprrr_trace", exact = TRUE)
  error_conditions <- lapply(
    results,
    attr,
    which = "dsprrr_error_condition",
    exact = TRUE
  )
  results <- lapply(results, strip_run_trace)
  attr(results, "dsprrr_traces") <- traces
  attr(results, "dsprrr_error_conditions") <- error_conditions
  results
}

#' Strip backend bookkeeping attributes from a result list
#' @noRd
strip_backend_traces <- function(results) {
  attr(results, "dsprrr_traces") <- NULL
  attr(results, "dsprrr_error_conditions") <- NULL
  results
}

#' Preserve specialized Predict subclass forward semantics
#' @noRd
run_predict_forward <- function(
  module,
  inputs,
  .llm,
  .verbose,
  .return_format,
  .cache
) {
  result <- module$forward(
    inputs,
    .llm = .llm,
    trace = TRUE,
    .cache = .cache
  )
  if (.verbose && !is.null(result$metadata[[1]]$prompt)) {
    cli::cli_h3("Generated Prompt")
    cli::cli_code(result$metadata[[1]]$prompt)
  }

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

#' Execute one Predict row through the same observable contract as a batch
#' @noRd
run_predict_scalar <- function(
  module,
  inputs,
  .llm,
  .verbose,
  .return_format,
  .cache
) {
  llm <- resolve_module_llm(module, .llm = .llm)
  item <- process_batch_item(
    input_set = inputs,
    module = module,
    llm = llm,
    index = 1L,
    .verbose = .verbose,
    .return_format = "structured",
    .cache = .cache,
    backend = "sequential",
    .capture_trace = TRUE
  )
  trace <- attr(item, "dsprrr_trace", exact = TRUE)
  error <- attr(item, "dsprrr_error_condition", exact = TRUE)
  commit_run_traces(module, list(trace))
  item <- strip_run_trace(item)
  # Scalar calls historically return the caller's stateful Chat. The completed
  # branch is needed only for canonical trace reconstruction.
  item$chat <- llm

  if (!is.null(error)) {
    stop(error)
  }

  if (.verbose && !is.null(item$metadata$prompt)) {
    cli::cli_h3("Generated Prompt")
    cli::cli_code(item$metadata$prompt)
  }

  if (.return_format == "simple") {
    extract_simple_output(item$output, module$signature@output_type)
  } else {
    structure(item, class = "dsprrr_result")
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
  .progress,
  .return_format,
  .cache = NULL,
  .concurrency
) {
  if (n == 0L) {
    return(empty_batch_result(.return_format))
  }
  input_sets <- lapply(seq_len(n), function(i) lapply(inputs, `[[`, i))

  # The backend is fully normalized before any Chat or topology is resolved.
  if (identical(.concurrency$effective_backend, "sequential")) {
    results <- run_batch_sequential(
      module,
      input_sets,
      n,
      .llm,
      .verbose,
      .return_format,
      .progress,
      .cache,
      .concurrency
    )
  } else if (identical(.concurrency$effective_backend, "ellmer")) {
    # Use ellmer's parallel_chat_structured for native parallelism
    results <- run_batch_ellmer_parallel(
      module,
      input_sets,
      n,
      .llm,
      .verbose,
      .return_format,
      .progress,
      .cache,
      .concurrency
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
      .cache,
      .concurrency
    )
  }

  traces <- attr(results, "dsprrr_traces", exact = TRUE)
  commit_run_traces(module, traces)
  results <- strip_backend_traces(results)

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
  .cache = NULL,
  .concurrency = NULL
) {
  if (is.null(.concurrency)) {
    .concurrency <- normalize_concurrency_runtime(
      concurrency_control(backend = "sequential")
    )
  }
  baseline_llm <- resolve_module_llm(module, .llm = .llm)
  row_llms <- batch_chat_branches(baseline_llm, n)
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

  error_count <- 0L

  for (i in seq_len(n)) {
    request <- build_module_request(module, input_sets[[i]])
    row_llm <- row_llms[[i]]

    results[[i]] <- tryCatch(
      {
        process_batch_item(
          input_set = input_sets[[i]],
          module = module,
          llm = row_llm,
          index = i,
          .verbose = .verbose,
          .return_format = .return_format,
          .cache = .cache,
          backend = "sequential",
          .capture_trace = TRUE
        )
      },
      error = function(e) {
        create_error_result(
          error = e,
          index = i,
          prompt = request$prompt,
          instructions = request$instructions,
          llm = row_llm,
          .return_format = .return_format,
          inputs = input_sets[[i]],
          request = request,
          turns_before = batch_chat_turns(row_llm),
          module = module,
          .capture_trace = TRUE
        )
      }
    )
    results[[i]] <- annotate_concurrency_result(
      results[[i]],
      .concurrency,
      .return_format
    )

    error_message <- if (.return_format == "simple") {
      attr(results[[i]], "error_message", exact = TRUE)
    } else {
      value <- results[[i]]$metadata$error
      if (run_error_present(value)) value else NULL
    }
    if (!is.null(error_message)) {
      cli::cli_warn("Failed to process item {i}: {error_message}")
    }
    if (concurrency_row_failed(results[[i]], .return_format)) {
      error_count <- error_count + 1L
    }

    if (!is.null(progress_id)) {
      cli::cli_progress_update(id = progress_id)
    }

    if (
      concurrency_error_budget_reached(
        error_count,
        .concurrency$max_errors
      ) &&
        i < n
    ) {
      queued <- seq.int(i + 1L, n)
      for (j in queued) {
        results[[j]] <- create_concurrency_error_result(
          module = module,
          input_set = input_sets[[j]],
          index = j,
          llm = row_llms[[j]],
          .return_format = .return_format,
          runtime = .concurrency,
          message = paste0(
            "Item ",
            j,
            " was not started because the batch error budget was reached"
          ),
          classes = c(
            "dsprrr_concurrency_cancelled_error",
            "dsprrr_concurrency_error"
          ),
          cancellation_reason = "max_errors"
        )
        if (!is.null(progress_id)) {
          cli::cli_progress_update(id = progress_id)
        }
      }
      cli::cli_warn(c(
        "Batch error budget reached after item {i}",
        "x" = "{length(queued)} queued item{?s} were not started."
      ))
      break
    }
  }

  if (!is.null(progress_id)) {
    cli::cli_progress_done(id = progress_id)
  }

  collect_backend_traces(results)
}

#' Find mutable environments reachable from a Chat's state surface
#' @noRd
batch_chat_state_environments <- function(chat) {
  found <- character()
  seen <- new.env(hash = TRUE, parent = emptyenv())
  expanded <- new.env(hash = TRUE, parent = emptyenv())
  trusted_ellmer <- cache_is_trusted_ellmer_chat(chat)

  immutable_environment <- function(env) {
    identical(env, emptyenv()) ||
      identical(env, baseenv()) ||
      (trusted_ellmer && isNamespace(env))
  }

  shared_scope <- function(env) {
    identical(env, baseenv()) ||
      identical(env, globalenv()) ||
      isNamespace(env) ||
      startsWith(environmentName(env), "package:")
  }

  binding_is_lazy <- function(env, name) {
    isTRUE(unname(rlang::env_binding_are_lazy(env, name))[[1]])
  }

  check_binding <- function(env, name) {
    if (bindingIsActive(name, env)) {
      cli::cli_abort(
        c(
          "Cannot prove opaque Chat isolation",
          "x" = "State references active binding {.field {name}}."
        ),
        class = "dsprrr_chat_isolation_error"
      )
    }
    if (binding_is_lazy(env, name)) {
      cli::cli_abort(
        c(
          "Cannot prove opaque Chat isolation",
          "x" = "State references delayed binding {.field {name}}."
        ),
        class = "dsprrr_chat_isolation_error"
      )
    }
    invisible(NULL)
  }

  known_safe_shared_binding <- function(env, name) {
    base_scope <- identical(env, baseenv()) ||
      identical(env, asNamespace("base"))
    package_scope <- isNamespace(env) ||
      startsWith(environmentName(env), "package:")
    if (!base_scope && !package_scope) {
      return(FALSE)
    }
    check_binding(env, name)
    if (!bindingIsLocked(name, env)) {
      return(FALSE)
    }
    value <- get(name, envir = env, inherits = FALSE)
    if (is.atomic(value) || is.null(value)) {
      return(TRUE)
    }
    if (!is.function(value)) {
      return(FALSE)
    }
    function_environment <- environment(value)
    is.null(function_environment) ||
      identical(function_environment, baseenv()) ||
      isNamespace(function_environment)
  }

  mark_seen <- function(value) {
    address <- rlang::obj_address(value)
    already_seen <- exists(address, envir = seen, inherits = FALSE)
    if (!already_seen) {
      assign(address, TRUE, envir = seen)
    }
    already_seen
  }

  record <- function(env) {
    if (immutable_environment(env)) {
      return(invisible(NULL))
    }
    address <- rlang::obj_address(env)
    if (!mark_seen(env)) {
      found <<- c(found, address)
    }
    invisible(NULL)
  }

  unsupported <- function(value) {
    cli::cli_abort(
      c(
        "Cannot prove opaque Chat isolation",
        "x" = "State contains unsupported {.code {typeof(value)}} data."
      ),
      class = "dsprrr_chat_isolation_error"
    )
  }

  visit <- function(value) {
    if (is.null(value)) {
      return(invisible(NULL))
    }
    if (
      trusted_ellmer &&
        (inherits(value, "ellmer::Provider") ||
          inherits(value, "ellmer::ToolDef"))
    ) {
      return(invisible(NULL))
    }
    if (is.function(value)) {
      if (mark_seen(value)) {
        return(invisible(NULL))
      }
      env <- environment(value)
      if (!is.null(env)) {
        # A closure's enclosing package/namespace is not itself proof of
        # mutable state. Inspect every referenced binding below and allow only
        # locked scalar/function bindings. Local environments remain part of
        # the identity proof because they are copied per branch.
        shared_function_scope <- !trusted_ellmer && shared_scope(env)
        if (!immutable_environment(env) && !shared_function_scope) {
          record(env)
        }
        self <- NULL
        if (exists("self", envir = env, inherits = FALSE)) {
          check_binding(env, "self")
          self <- get("self", envir = env, inherits = FALSE)
        }
        r6_clone_method <- inherits(self, "R6") &&
          identical(
            value,
            tryCatch(self$clone, error = function(e) NULL)
          )
        if (!trusted_ellmer && !r6_clone_method) {
          calls <- all.names(body(value), functions = TRUE, unique = TRUE)
          dynamic_state_calls <- c(
            ":::",
            "as.environment",
            "asNamespace",
            "assign",
            "baseenv",
            "bquote",
            "delayedAssign",
            "do.call",
            "dynGet",
            "env_bind",
            "env_bind_active",
            "env_bind_lazy",
            "env_get",
            "env_get_list",
            "env_parent",
            "env_parents",
            "env_poke",
            "env_unbind",
            "environment",
            "environment<-",
            "eval",
            "eval.parent",
            "exists",
            "get",
            "get0",
            "getAnywhere",
            "getExportedValue",
            "getFromNamespace",
            "getLoadedDLLs",
            "getNativeSymbolInfo",
            "getNamespace",
            "getNamespaceExports",
            "getNamespaceImports",
            "getNamespaceInfo",
            "getNamespaceName",
            "getNamespaceUsers",
            "getNamespaceVersion",
            "globalenv",
            "global_env",
            "library",
            "loadNamespace",
            "loadedNamespaces",
            "lockBinding",
            "makeActiveBinding",
            "mget",
            "ns_env",
            "namespaceExport",
            "namespaceImport",
            "parse",
            "parent.env",
            "parent.env<-",
            "parent.frame",
            "pos.to.env",
            "pkg_env",
            "require",
            "rm",
            "source",
            "substitute",
            "sys.source",
            "sys.call",
            "sys.calls",
            "sys.frame",
            "topenv",
            "unlockBinding",
            "assignInNamespace",
            "attach",
            "detach",
            "dyn.load",
            "dyn.unload"
          )
          if (any(calls %in% dynamic_state_calls)) {
            cli::cli_abort(
              c(
                "Cannot prove opaque Chat isolation",
                "x" = "A Chat closure uses dynamic environment access."
              ),
              class = "dsprrr_chat_isolation_error"
            )
          }
          globals <- codetools::findGlobals(
            value,
            merge = FALSE
          )
          property_roots <- character()
          find_property_roots <- function(expr) {
            if (is.call(expr)) {
              operator <- if (is.symbol(expr[[1]])) {
                as.character(expr[[1]])
              } else {
                ""
              }
              if (operator %in% c("$", "$<-", "@", "@<-")) {
                target <- expr[[2]]
                while (
                  is.call(target) &&
                    is.symbol(target[[1]]) &&
                    as.character(target[[1]]) %in% c("$", "@")
                ) {
                  target <- target[[2]]
                }
                if (is.symbol(target)) {
                  property_roots <<- c(
                    property_roots,
                    as.character(target)
                  )
                }
              }
              lapply(as.list(expr)[-1], find_property_roots)
            } else if (is.pairlist(expr) || is.expression(expr)) {
              lapply(expr, find_property_roots)
            }
            invisible(NULL)
          }
          find_property_roots(body(value))
          find_property_roots(formals(value))
          referenced <- unique(c(
            globals$variables,
            globals$functions,
            property_roots
          ))
          referenced <- setdiff(referenced, names(formals(value)))
          for (name in referenced) {
            current <- env
            repeat {
              if (identical(current, emptyenv())) {
                break
              }
              if (exists(name, envir = current, inherits = FALSE)) {
                check_binding(current, name)
                if (shared_scope(current)) {
                  if (!known_safe_shared_binding(current, name)) {
                    cli::cli_abort(
                      c(
                        "Cannot prove opaque Chat isolation",
                        "x" = "Closure state {.field {name}} resolves from shared environment {.envvar {environmentName(current)}}."
                      ),
                      class = "dsprrr_chat_isolation_error"
                    )
                  }
                } else {
                  record(current)
                  visit(get(name, envir = current, inherits = FALSE))
                }
                break
              }
              current <- parent.env(current)
            }
          }
        }
      }
      lapply(attributes(value), visit)
      return(invisible(NULL))
    }
    if (is.environment(value)) {
      if (immutable_environment(value)) {
        return(invisible(NULL))
      }
      if (!trusted_ellmer && shared_scope(value)) {
        cli::cli_abort(
          c(
            "Cannot prove opaque Chat isolation",
            "x" = "State reaches shared environment {.envvar {environmentName(value)}}."
          ),
          class = "dsprrr_chat_isolation_error"
        )
      }
      record(value)
      address <- rlang::obj_address(value)
      if (exists(address, envir = expanded, inherits = FALSE)) {
        return(invisible(NULL))
      }
      assign(address, TRUE, envir = expanded)
      members <- ls(value, all.names = TRUE)
      for (name in members) {
        check_binding(value, name)
        member <- tryCatch(
          get(name, envir = value, inherits = FALSE),
          error = function(e) unsupported(value)
        )
        visit(member)
      }
      return(invisible(NULL))
    }
    if (inherits(value, "S7_object")) {
      if (!any(startsWith(class(value), "ellmer::"))) {
        unsupported(value)
      }
      if (mark_seen(value)) {
        return(invisible(NULL))
      }
      properties <- tryCatch(
        S7::props(value),
        error = function(e) unsupported(value)
      )
      lapply(properties, visit)
      return(invisible(NULL))
    }
    if (isS4(value)) {
      if (mark_seen(value)) {
        return(invisible(NULL))
      }
      lapply(methods::slotNames(value), function(name) {
        visit(methods::slot(value, name))
      })
      lapply(attributes(value), visit)
      return(invisible(NULL))
    }
    if (is.list(value) || is.pairlist(value) || is.expression(value)) {
      if (mark_seen(value)) {
        return(invisible(NULL))
      }
      lapply(value, visit)
      lapply(attributes(value), visit)
      return(invisible(NULL))
    }
    if (is.language(value)) {
      visit(as.list(value))
      lapply(attributes(value), visit)
      return(invisible(NULL))
    }
    if (typeof(value) %in% c("externalptr", "weakref")) {
      unsupported(value)
    }
    if (
      is.atomic(value) ||
        typeof(value) %in% c("symbol", "builtin", "special")
    ) {
      lapply(attributes(value), visit)
      return(invisible(NULL))
    }
    unsupported(value)
  }

  visit(chat)
  unique(found)
}

#' Read Chat turns when an inspection method is available
#' @noRd
batch_chat_history <- function(chat) {
  getter <- tryCatch(chat$get_turns, error = function(e) NULL)
  if (!is.function(getter)) {
    return(NULL)
  }
  turns <- tryCatch(getter(), error = function(e) e)
  if (inherits(turns, "condition") || !is.list(turns)) {
    cli::cli_abort(
      "Cannot inspect Chat history for batch isolation",
      class = "dsprrr_chat_isolation_error",
      parent = if (inherits(turns, "condition")) turns else NULL
    )
  }
  turns
}

#' Create one isolated Chat without invoking opaque clone methods
#' @noRd
batch_chat_copy <- function(chat, stage, source_state = NULL) {
  if (is.null(source_state)) {
    source_state <- list(
      environments = batch_chat_state_environments(chat),
      history = batch_chat_history(chat)
    )
  }
  branch <- if (cache_is_trusted_ellmer_chat(chat)) {
    tryCatch(
      chat$clone(deep = TRUE),
      error = function(e) {
        cli::cli_abort(
          "Cannot deep-clone ellmer Chat for the {stage}",
          class = "dsprrr_chat_isolation_error",
          parent = e
        )
      }
    )
  } else {
    tryCatch(
      unserialize(serialize(chat, connection = NULL, version = 3)),
      error = function(e) {
        cli::cli_abort(
          c(
            "Cannot isolate the Chat for batch execution",
            "x" = "The opaque Chat could not be copied for the {stage}."
          ),
          class = "dsprrr_chat_isolation_error",
          parent = e
        )
      }
    )
  }

  if (
    is.null(branch) ||
      identical(rlang::obj_address(branch), rlang::obj_address(chat))
  ) {
    cli::cli_abort(
      c(
        "Cannot isolate the Chat for batch execution",
        "x" = "Copying returned the original mutable Chat for the {stage}."
      ),
      class = "dsprrr_chat_isolation_error"
    )
  }

  branch_envs <- batch_chat_state_environments(branch)
  if (length(intersect(source_state$environments, branch_envs)) > 0L) {
    cli::cli_abort(
      c(
        "Cannot isolate the Chat for batch execution",
        "x" = "The {stage} still shares mutable environment-backed state."
      ),
      class = "dsprrr_chat_isolation_error"
    )
  }

  source_history <- source_state$history
  branch_history <- batch_chat_history(branch)
  if (
    !is.null(source_history) &&
      !identical(source_history, branch_history)
  ) {
    setter <- tryCatch(branch$set_turns, error = function(e) NULL)
    if (is.function(setter)) {
      tryCatch(
        setter(rlang::duplicate(source_history, shallow = FALSE)),
        error = function(e) NULL
      )
      branch_history <- batch_chat_history(branch)
    }
  }
  if (
    xor(is.null(source_history), is.null(branch_history)) ||
      (!is.null(source_history) && !identical(source_history, branch_history))
  ) {
    cli::cli_abort(
      c(
        "Cannot isolate the Chat for batch execution",
        "x" = "The {stage} did not preserve the exact starting history."
      ),
      class = "dsprrr_chat_isolation_error"
    )
  }

  branch
}

#' Create independent Chat branches for sequential batch rows
#'
#' Every row receives an isolated copy of the caller's starting state. Canonical
#' ellmer Chats use their deep-clone contract; opaque Chats are copied without
#' calling custom clone methods and rejected if any mutable environments remain
#' shared.
#' @noRd
batch_chat_branches <- function(chat, n) {
  if (n == 0) {
    return(list())
  }

  source_state <- list(
    environments = batch_chat_state_environments(chat),
    history = batch_chat_history(chat)
  )
  branches <- lapply(seq_len(n), function(i) {
    batch_chat_copy(
      chat,
      paste0("branch for row ", i),
      source_state = source_state
    )
  })

  branch_ids <- vapply(branches, rlang::obj_address, character(1))
  branch_envs <- lapply(branches, batch_chat_state_environments)
  shared_branch_state <- anyDuplicated(branch_ids) > 0L ||
    any(vapply(
      seq_along(branches),
      function(i) {
        if (i == 1L) {
          return(FALSE)
        }
        length(intersect(
          branch_envs[[i]],
          unique(unlist(branch_envs[seq_len(i - 1L)], use.names = FALSE))
        )) >
          0L
      },
      logical(1)
    ))
  if (shared_branch_state) {
    cli::cli_abort(
      c(
        "Cannot isolate the Chat for batch execution",
        "x" = "Multiple rows received shared mutable Chat state."
      ),
      class = "dsprrr_chat_isolation_error"
    )
  }

  branches
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
  .cache = NULL,
  .concurrency = NULL,
  .batch_indices = seq_len(n),
  .single_wave = FALSE
) {
  if (is.null(.concurrency)) {
    .concurrency <- normalize_concurrency_runtime(concurrency_control(
      backend = "ellmer",
      max_active = max(1L, n)
    ))
  }

  if (!.single_wave) {
    if (!concurrency_backend_available("ellmer")) {
      cli::cli_abort(
        "Requested concurrency backend {.val ellmer} is unavailable",
        class = "dsprrr_concurrency_backend_unavailable"
      )
    }

    progress_started <- isTRUE(.progress) && n > 1L
    if (progress_started) {
      cli::cli_progress_step(
        "Processing {n} items with ellmer (max active: {(.concurrency$requested_workers)})...",
        spinner = TRUE
      )
      on.exit(try(cli::cli_progress_done(), silent = TRUE), add = TRUE)
    }

    results <- vector("list", n)
    traces <- vector("list", n)
    conditions <- vector("list", n)
    error_count <- 0L
    wave_id <- ceiling(seq_len(n) / .concurrency$effective_workers)
    waves <- split(seq_len(n), wave_id)

    for (indices in waves) {
      wave <- run_batch_ellmer_parallel(
        module = module,
        input_sets = input_sets[indices],
        n = length(indices),
        .llm = .llm,
        .verbose = .verbose,
        .return_format = .return_format,
        .progress = FALSE,
        .cache = .cache,
        .concurrency = .concurrency,
        .batch_indices = indices,
        .single_wave = TRUE
      )
      results[indices] <- wave[seq_along(indices)]
      traces[indices] <- attr(wave, "dsprrr_traces", exact = TRUE)
      wave_conditions <- attr(
        wave,
        "dsprrr_error_conditions",
        exact = TRUE
      )
      conditions[indices] <- wave_conditions
      error_count <- error_count +
        sum(
          !vapply(
            wave_conditions,
            is.null,
            logical(1)
          )
        )

      if (
        concurrency_error_budget_reached(
          error_count,
          .concurrency$max_errors
        ) &&
          max(indices) < n
      ) {
        queued <- seq.int(max(indices) + 1L, n)
        for (index in queued) {
          cancelled <- create_concurrency_error_result(
            module = module,
            input_set = input_sets[[index]],
            index = index,
            llm = .llm %||% module$chat,
            .return_format = .return_format,
            runtime = .concurrency,
            message = paste0(
              "Item ",
              index,
              " was not started because the batch error budget was reached"
            ),
            classes = c(
              "dsprrr_concurrency_cancelled_error",
              "dsprrr_concurrency_error"
            ),
            cancellation_reason = "max_errors"
          )
          traces[[index]] <- attr(
            cancelled,
            "dsprrr_trace",
            exact = TRUE
          )
          conditions[[index]] <- attr(
            cancelled,
            "dsprrr_error_condition",
            exact = TRUE
          )
          results[[index]] <- strip_run_trace(cancelled)
        }
        cli::cli_warn(c(
          "Batch error budget reached after an ellmer wave",
          "x" = "Queued items not started: {length(queued)}."
        ))
        break
      }
    }

    attr(results, "dsprrr_traces") <- traces
    attr(results, "dsprrr_error_conditions") <- conditions
    return(results)
  }

  # Build prompts for all inputs (as list, required by ellmer::parallel_chat_structured)
  requests <- lapply(input_sets, function(input_set) {
    build_module_request(module, input_set)
  })
  prompts <- lapply(requests, `[[`, "payload")

  caller_chat <- resolve_module_llm(module, .llm = .llm)

  baseline_turns <- batch_chat_turns(caller_chat)
  chat <- batch_chat_copy(caller_chat, "ellmer parallel execution")

  start_time <- Sys.time()

  output_fields <- output_field_names(module$signature@output_type)
  token_fields <- c("input_tokens", "output_tokens", "cached_input_tokens")
  include_tokens <- !any(token_fields %in% output_fields)
  include_cost <- !"cost" %in% output_fields

  # Call ellmer's parallel_chat_structured
  responses <- tryCatch(
    {
      ellmer::parallel_chat_structured(
        chat = chat,
        prompts = prompts,
        type = module$signature@output_type,
        include_tokens = include_tokens,
        include_cost = include_cost,
        max_active = .concurrency$requested_workers,
        on_error = "continue"
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

  # Validate response format

  if (is.null(responses)) {
    cli::cli_abort(c(
      "parallel_chat_structured() returned NULL",
      "i" = "This may indicate an API failure or empty response"
    ))
  }

  # Normalize responses to list-of-lists format
  # parallel_chat_structured returns a tibble where each row is a response
  response_errors <- vector("list", n)
  response_usage <- replicate(n, list(), simplify = FALSE)

  if (is.data.frame(responses)) {
    if (nrow(responses) != n) {
      cli::cli_abort(c(
        "Response count mismatch from parallel_chat_structured()",
        "x" = "Expected {n} responses, got {nrow(responses)}",
        "i" = "Some requests may have failed silently"
      ))
    }
    if (".error" %in% names(responses)) {
      response_errors <- lapply(responses$.error, normalize_backend_error)
      responses$.error <- NULL
    }
    telemetry_fields <- c(
      if (include_tokens) token_fields else character(),
      if (include_cost) "cost" else character()
    )
    usage_fields <- intersect(telemetry_fields, names(responses))
    if (length(usage_fields) > 0) {
      response_usage <- lapply(seq_len(n), function(i) {
        usage <- as.list(responses[i, usage_fields, drop = FALSE])
        usage$total_tokens <- if (
          all(c("input_tokens", "output_tokens") %in% names(usage)) &&
            !anyNA(c(usage$input_tokens, usage$output_tokens))
        ) {
          as.integer(usage$input_tokens + usage$output_tokens)
        } else {
          NA_integer_
        }
        usage
      })
      responses[usage_fields] <- NULL
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
    response_errors <- lapply(responses, function(response) {
      if (inherits(response, "error")) {
        normalize_backend_error(response)
      } else {
        NULL
      }
    })
    responses_list <- responses
  } else {
    cli::cli_abort(c(
      "Unexpected response format from parallel_chat_structured()",
      "x" = "Got {.cls {class(responses)[1]}} instead of data.frame or list",
      "i" = "This may be a version mismatch with ellmer"
    ))
  }

  model <- tryCatch(chat$get_model(), error = function(e) NA_character_)
  results <- purrr::map2(
    responses_list,
    seq_along(responses_list),
    function(response, i) {
      batch_index <- .batch_indices[[i]]
      error <- response_errors[[i]]
      row_started_at <- end_time - total_latency / 1000
      metadata <- canonical_run_metadata(
        request = requests[[i]],
        started_at = row_started_at,
        ended_at = end_time,
        usage = response_usage[[i]],
        model = model,
        error = error,
        backend = "ellmer",
        batch_index = batch_index,
        cache = "bypass"
      )
      metadata[intersect(
        output_fields,
        c(token_fields, "total_tokens", "cost", "duration_s")
      )] <- NULL
      # Ellmer reports total wall time for the parallel group. Preserve the
      # historical per-row estimate while keeping one canonical metadata shape.
      metadata$latency_ms <- total_latency / n

      if (!is.null(error)) {
        error_chat <- batch_chat_copy(
          caller_chat,
          "ellmer parallel error result"
        )
        return(annotate_concurrency_result(
          create_error_result(
            error = error,
            index = batch_index,
            prompt = requests[[i]]$prompt,
            instructions = requests[[i]]$instructions,
            llm = error_chat,
            .return_format = .return_format,
            inputs = input_sets[[i]],
            request = requests[[i]],
            turns_before = baseline_turns,
            metadata = metadata,
            module = module,
            .capture_trace = TRUE
          ),
          .concurrency,
          .return_format
        ))
      }

      completed_chat <- mock_batch_chat(
        prompts[[i]],
        response,
        chat,
        turns_before = baseline_turns
      )
      trace <- canonical_run_trace(
        inputs = input_sets[[i]],
        response = response,
        request = requests[[i]],
        chat = completed_chat,
        turns_before = baseline_turns,
        metadata = metadata,
        store_chat = isTRUE(module$config$store_chat_in_traces)
      )
      result <- if (.return_format == "simple") {
        extract_simple_output(response, module$signature@output_type)
      } else {
        list(output = response, chat = completed_chat, metadata = metadata)
      }
      annotate_concurrency_result(
        attach_run_trace(result, trace),
        .concurrency,
        .return_format
      )
    }
  )

  collect_backend_traces(results)
}

#' Construct a typed mirai boundary condition
#' @noRd
mirai_boundary_condition <- function(message, classes) {
  condition <- simpleError(as.character(message)[1])
  class(condition) <- unique(c(classes, class(condition)))
  condition
}

#' Whether mirai reported its documented timeout error value
#' @noRd
is_mirai_timeout_record <- function(record) {
  inherits(record, "errorValue") &&
    length(record) == 1L &&
    isTRUE(as.integer(record) == 5L)
}

#' Validate an untrusted record returned across the mirai process boundary
#' @noRd
validate_mirai_worker_record <- function(
  record,
  fallback_started_at,
  fallback_ended_at
) {
  invalid <- function(message) {
    list(
      record = NULL,
      error = mirai_boundary_condition(
        message,
        c("dsprrr_mirai_record_error", "dsprrr_mirai_worker_error")
      ),
      started_at = fallback_started_at,
      ended_at = fallback_ended_at,
      usage = list(),
      model = NA_character_,
      turns_before = NULL
    )
  }

  if (inherits(record, "miraiError")) {
    message <- attr(record, "message", exact = TRUE)
    if (is.null(message) || !nzchar(as.character(message)[1])) {
      message <- as.character(record)[1]
    }
    return(list(
      record = NULL,
      error = mirai_boundary_condition(
        message,
        c("dsprrr_mirai_worker_error", "dsprrr_mirai_error")
      ),
      started_at = fallback_started_at,
      ended_at = fallback_ended_at,
      usage = list(),
      model = NA_character_,
      turns_before = NULL
    ))
  }
  if (!is.list(record) || length(record) == 0L || is.null(names(record))) {
    return(invalid("mirai worker returned a non-record value"))
  }
  if (anyDuplicated(names(record)) > 0L) {
    return(invalid("mirai worker record contains duplicate fields"))
  }

  required <- c(
    "ok",
    "started_at",
    "ended_at",
    "usage",
    "model",
    "turns_before"
  )
  missing <- setdiff(required, names(record))
  if (length(missing) > 0L) {
    return(invalid(paste0(
      "mirai worker record is missing fields: ",
      paste(missing, collapse = ", ")
    )))
  }
  if (
    !is.logical(record$ok) ||
      length(record$ok) != 1L ||
      is.na(record$ok)
  ) {
    return(invalid("mirai worker record has an invalid ok field"))
  }
  valid_time <- function(value) {
    inherits(value, "POSIXt") && length(value) == 1L && !is.na(value)
  }
  if (!valid_time(record$started_at) || !valid_time(record$ended_at)) {
    return(invalid("mirai worker record has invalid timestamps"))
  }
  if (record$ended_at < record$started_at) {
    return(invalid("mirai worker record ends before it starts"))
  }
  if (!is.list(record$usage)) {
    return(invalid("mirai worker record has invalid usage metadata"))
  }
  usage_names <- names(record$usage)
  known_usage <- c(
    "input_tokens",
    "output_tokens",
    "cached_input_tokens",
    "total_tokens",
    "cost",
    "duration_s"
  )
  if (
    length(record$usage) > 0L &&
      (is.null(usage_names) ||
        anyDuplicated(usage_names) > 0L ||
        length(setdiff(usage_names, known_usage)) > 0L ||
        any(
          !vapply(
            record$usage,
            function(value) {
              is.numeric(value) && length(value) == 1L
            },
            logical(1)
          )
        ))
  ) {
    return(invalid("mirai worker record has invalid usage fields"))
  }
  if (
    !is.character(record$model) ||
      length(record$model) != 1L
  ) {
    return(invalid("mirai worker record has an invalid model field"))
  }
  if (!is.null(record$turns_before) && !is.list(record$turns_before)) {
    return(invalid("mirai worker record has invalid baseline turns"))
  }

  if (isTRUE(record$ok)) {
    success_required <- c("response", "usage_verified")
    missing <- setdiff(success_required, names(record))
    if (length(missing) > 0L) {
      return(invalid(paste0(
        "successful mirai worker record is missing fields: ",
        paste(missing, collapse = ", ")
      )))
    }
    if (
      !is.logical(record$usage_verified) ||
        length(record$usage_verified) != 1L ||
        is.na(record$usage_verified)
    ) {
      return(invalid(
        "successful mirai worker record has invalid usage verification"
      ))
    }
    if (is.null(record$response) || inherits(record$response, "condition")) {
      return(invalid("successful mirai worker record has an invalid response"))
    }
    allowed <- c(required, success_required)
    if (length(setdiff(names(record), allowed)) > 0L) {
      return(invalid("successful mirai worker record contains unknown fields"))
    }
    if (!identical(record$usage_verified, TRUE)) {
      record$usage <- list()
    }
    return(list(
      record = record,
      error = NULL,
      started_at = record$started_at,
      ended_at = record$ended_at,
      usage = record$usage,
      model = record$model,
      turns_before = record$turns_before
    ))
  }

  failure_required <- c("error_message", "error_class", "error_kind")
  missing <- setdiff(failure_required, names(record))
  if (length(missing) > 0L) {
    return(invalid(paste0(
      "failed mirai worker record is missing fields: ",
      paste(missing, collapse = ", ")
    )))
  }
  if (
    !is.character(record$error_message) ||
      length(record$error_message) != 1L ||
      is.na(record$error_message) ||
      !nzchar(record$error_message)
  ) {
    return(invalid("failed mirai worker record has no error message"))
  }
  if (
    !is.character(record$error_class) ||
      length(record$error_class) != 1L ||
      is.na(record$error_class) ||
      !nzchar(record$error_class)
  ) {
    return(invalid("failed mirai worker record has no error class"))
  }
  if (
    !is.character(record$error_kind) ||
      length(record$error_kind) != 1L ||
      !record$error_kind %in% c("provider", "serialization")
  ) {
    return(invalid("failed mirai worker record has an invalid error kind"))
  }
  allowed <- c(required, failure_required)
  if (length(setdiff(names(record), allowed)) > 0L) {
    return(invalid("failed mirai worker record contains unknown fields"))
  }
  typed_class <- if (identical(record$error_kind, "serialization")) {
    "dsprrr_mirai_serialization_error"
  } else {
    record$error_class
  }
  error <- mirai_boundary_condition(
    record$error_message,
    c(typed_class, record$error_class, "dsprrr_mirai_worker_error")
  )
  list(
    record = record,
    error = error,
    started_at = record$started_at,
    ended_at = record$ended_at,
    usage = list(),
    model = record$model,
    turns_before = record$turns_before
  )
}

#' Normalize one serializable mirai worker record in the parent process
#' @noRd
mirai_worker_result <- function(
  record,
  module,
  input_set,
  request,
  chat,
  index,
  .return_format,
  fallback_started_at = Sys.time(),
  fallback_ended_at = Sys.time()
) {
  validated <- validate_mirai_worker_record(
    record,
    fallback_started_at = fallback_started_at,
    fallback_ended_at = fallback_ended_at
  )
  record <- validated$record
  error <- validated$error
  metadata <- canonical_run_metadata(
    request = request,
    started_at = validated$started_at,
    ended_at = validated$ended_at,
    usage = validated$usage,
    model = validated$model,
    error = error,
    backend = "mirai",
    batch_index = index,
    cache = "bypass"
  )

  if (!is.null(error)) {
    return(create_error_result(
      error = error,
      index = index,
      prompt = request$prompt,
      instructions = request$instructions,
      llm = NULL,
      .return_format = .return_format,
      inputs = input_set,
      request = request,
      turns_before = validated$turns_before,
      metadata = metadata,
      module = module,
      .capture_trace = TRUE
    ))
  }

  completed_chat <- mock_batch_chat(
    request$payload,
    record$response,
    chat,
    turns_before = record$turns_before
  )
  trace <- canonical_run_trace(
    inputs = input_set,
    response = record$response,
    request = request,
    chat = completed_chat,
    turns_before = record$turns_before,
    metadata = metadata,
    store_chat = isTRUE(module$config$store_chat_in_traces)
  )
  result <- if (.return_format == "simple") {
    extract_simple_output(record$response, module$signature@output_type)
  } else {
    list(output = record$response, chat = completed_chat, metadata = metadata)
  }
  attach_run_trace(result, trace)
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
  .cache = NULL,
  .concurrency = NULL
) {
  if (is.null(.concurrency)) {
    .concurrency <- normalize_concurrency_runtime(concurrency_control(
      backend = "mirai",
      max_active = getOption("dsprrr.max_active", 10L),
      total_timeout = getOption("dsprrr.parallel_timeout", 600)
    ))
  }
  requests <- lapply(input_sets, function(input_set) {
    build_module_request(module, input_set)
  })
  llm_template <- resolve_module_llm(module, .llm = .llm)
  baseline_turns <- batch_chat_turns(llm_template)
  batch_started_at <- Sys.time()
  batch_started_elapsed <- concurrency_elapsed()
  compute_profile <- new_dsprrr_mirai_profile()
  profile_owned <- FALSE
  tasks <- vector("list", n)
  if (!mirai_profile_is_unoccupied(compute_profile)) {
    cli::cli_abort(
      c(
        "Refusing to replace an occupied mirai compute profile",
        "x" = "Profile {.val {compute_profile}} is already owned by another caller."
      ),
      class = "dsprrr_mirai_profile_collision",
      profile = compute_profile
    )
  }
  tryCatch(
    {
      mirai::daemons(
        n = .concurrency$requested_workers,
        dispatcher = TRUE,
        .compute = compute_profile
      )
      profile_owned <- TRUE
    },
    error = function(error) {
      cli::cli_abort(
        c(
          "Could not create the dsprrr mirai worker pool",
          "x" = conditionMessage(error)
        ),
        class = "dsprrr_concurrency_backend_error",
        parent = error
      )
    }
  )
  shutdown_owned <- function(strict = TRUE) {
    if (!profile_owned) {
      return(invisible(TRUE))
    }
    stopped <- shutdown_dsprrr_mirai_profile(
      profile = compute_profile,
      tasks = tasks,
      strict = strict
    )
    if (isTRUE(stopped)) {
      profile_owned <<- FALSE
    }
    invisible(stopped)
  }
  on.exit(
    {
      if (profile_owned) {
        cleaned <- tryCatch(
          shutdown_owned(strict = FALSE),
          error = function(error) FALSE
        )
        if (!isTRUE(cleaned)) {
          warn_mirai_teardown_failure(compute_profile)
        }
      }
    },
    add = TRUE
  )

  worker <- function(
    i,
    requests,
    output_type,
    llm_template
  ) {
    started_at <- Sys.time()
    worker_llm <- tryCatch(
      unserialize(serialize(llm_template, connection = NULL, version = 2)),
      error = function(e) e
    )
    if (inherits(worker_llm, "condition")) {
      return(list(
        ok = FALSE,
        error_message = conditionMessage(worker_llm),
        error_class = class(worker_llm)[1],
        error_kind = "serialization",
        started_at = started_at,
        ended_at = Sys.time(),
        usage = list(),
        model = NA_character_,
        turns_before = NULL
      ))
    }

    get_turns <- tryCatch(worker_llm$get_turns, error = function(e) NULL)
    turns_before <- if (is.function(get_turns)) {
      tryCatch(get_turns(), error = function(e) NULL)
    } else {
      NULL
    }
    response <- tryCatch(
      worker_llm$chat_structured(
        requests[[i]]$payload,
        type = output_type,
        echo = "none"
      ),
      error = function(e) e
    )
    ended_at <- Sys.time()
    if (inherits(response, "condition")) {
      return(list(
        ok = FALSE,
        error_message = conditionMessage(response),
        error_class = class(response)[1],
        error_kind = "provider",
        started_at = started_at,
        ended_at = ended_at,
        usage = list(),
        model = NA_character_,
        turns_before = turns_before
      ))
    }

    turns_after <- if (is.function(get_turns)) {
      tryCatch(get_turns(), error = function(e) NULL)
    } else {
      NULL
    }
    before_n <- length(turns_before)
    usage_verified <- !is.null(turns_before) &&
      !is.null(turns_after) &&
      length(turns_after) > before_n &&
      (before_n == 0L ||
        identical(
          turns_after[seq_len(before_n)],
          turns_before
        ))
    turn_delta <- if (usage_verified) {
      turns_after[seq.int(before_n + 1L, length(turns_after))]
    } else {
      list()
    }
    assistant_turns <- Filter(
      function(turn) inherits(turn, "ellmer::AssistantTurn"),
      turn_delta
    )
    assistant_turn <- if (length(assistant_turns) > 0L) {
      assistant_turns[[length(assistant_turns)]]
    } else {
      NULL
    }
    tokens <- tryCatch(assistant_turn@tokens, error = function(e) NULL)
    input_tokens <- as.integer(
      if (length(tokens) >= 1L) {
        tokens[[1]]
      } else {
        NA_integer_
      }
    )
    output_tokens <- as.integer(
      if (length(tokens) >= 2L) {
        tokens[[2]]
      } else {
        NA_integer_
      }
    )
    cached_input_tokens <- as.integer(
      if (length(tokens) >= 3L) {
        tokens[[3]]
      } else {
        NA_integer_
      }
    )
    usage <- list(
      input_tokens = input_tokens,
      output_tokens = output_tokens,
      cached_input_tokens = cached_input_tokens,
      total_tokens = if (anyNA(c(input_tokens, output_tokens))) {
        NA_integer_
      } else {
        input_tokens + output_tokens
      },
      cost = tryCatch(assistant_turn@cost, error = function(e) NA_real_),
      duration_s = tryCatch(
        assistant_turn@duration,
        error = function(e) NA_real_
      )
    )
    list(
      ok = TRUE,
      response = response,
      started_at = started_at,
      ended_at = ended_at,
      usage = usage,
      usage_verified = usage_verified,
      model = tryCatch(
        worker_llm$get_model(),
        error = function(e) NA_character_
      ),
      turns_before = turns_before
    )
  }

  results <- vector("list", n)
  active <- integer()
  next_index <- 1L
  completed <- 0L
  error_count <- 0L
  stop_scheduling <- FALSE
  warnings_to_emit <- character()
  task_timeout_count <- 0L
  task_started_at <- rep(list(NULL), n)
  task_started_elapsed <- rep(NA_real_, n)

  progress_id <- NULL
  if (.progress && n > 1L) {
    progress_id <- cli::cli_progress_bar(
      format = "Processing {cli::pb_current}/{cli::pb_total} | {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = n,
      clear = FALSE
    )
    on.exit(
      try(cli::cli_progress_done(id = progress_id), silent = TRUE),
      add = TRUE
    )
  }

  update_progress <- function() {
    if (!is.null(progress_id)) {
      cli::cli_progress_update(id = progress_id)
    }
  }

  task_timeout_ms <- if (is.finite(.concurrency$task_timeout)) {
    max(1, ceiling(.concurrency$task_timeout * 1000))
  } else {
    NULL
  }
  deadline <- if (is.finite(.concurrency$total_timeout)) {
    batch_started_elapsed + .concurrency$total_timeout
  } else {
    Inf
  }

  launch_one <- function(index) {
    task_started_at[[index]] <<- Sys.time()
    task_started_elapsed[[index]] <<- concurrency_elapsed()
    tasks[[index]] <<- mirai::mirai(
      .expr = worker(i, requests, output_type, llm_template),
      .args = list(
        worker = worker,
        i = index,
        requests = requests,
        output_type = module$signature@output_type,
        llm_template = llm_template
      ),
      .timeout = task_timeout_ms,
      .compute = compute_profile
    )
    active <<- c(active, index)
  }

  launch_available <- function() {
    while (
      !stop_scheduling &&
        next_index <= n &&
        length(active) < .concurrency$effective_workers
    ) {
      launch_one(next_index)
      next_index <<- next_index + 1L
    }
  }

  fill_concurrency_rows <- function(
    indices,
    message,
    classes,
    reason
  ) {
    for (index in indices) {
      if (!is.null(results[[index]])) {
        next
      }
      results[[index]] <<- create_concurrency_error_result(
        module = module,
        input_set = input_sets[[index]],
        index = index,
        llm = NULL,
        .return_format = .return_format,
        runtime = .concurrency,
        message = message(index),
        classes = classes,
        cancellation_reason = reason,
        started_at = batch_started_at
      )
      completed <<- completed + 1L
      update_progress()
    }
  }

  stop_active <- function(indices) {
    for (index in indices) {
      if (!is.null(tasks[[index]]) && mirai::unresolved(tasks[[index]])) {
        try(mirai::stop_mirai(tasks[[index]]), silent = TRUE)
      }
    }
    # Stopping the owned profile is the hard synchronization boundary: after
    # this returns, no dsprrr worker from this batch can continue executing.
    shutdown_owned(strict = TRUE)
  }

  launch_available()

  while (completed < n) {
    if (concurrency_elapsed() >= deadline) {
      unfinished <- which(vapply(results, is.null, logical(1)))
      active_at_timeout <- active
      stop_scheduling <- TRUE
      stop_active(active_at_timeout)
      active <- integer()
      fill_concurrency_rows(
        unfinished,
        message = function(index) {
          paste0(
            "Item ",
            index,
            " did not finish before the batch total timeout of ",
            .concurrency$total_timeout,
            " seconds"
          )
        },
        classes = c(
          "dsprrr_mirai_timeout_error",
          "dsprrr_concurrency_total_timeout_error",
          "dsprrr_concurrency_error"
        ),
        reason = "total_timeout"
      )
      cli::cli_warn(c(
        "Parallel processing timed out after {(.concurrency$total_timeout)} seconds",
        "x" = "{length(unfinished)} of {n} tasks did not complete."
      ))
      break
    }

    resolved <- active[
      !vapply(
        tasks[active],
        mirai::unresolved,
        logical(1)
      )
    ]
    if (length(resolved) == 0L) {
      Sys.sleep(0.005)
      next
    }

    for (index in resolved) {
      record <- tasks[[index]][["data"]]
      result <- if (is_mirai_timeout_record(record)) {
        # Mirai's timeout record is resolved only after dispatcher
        # cancellation. Issue an explicit stop as a defensive barrier before
        # releasing this worker slot or scheduling replacement work.
        try(mirai::stop_mirai(tasks[[index]]), silent = TRUE)
        stop_deadline <- concurrency_elapsed() + 1
        stopped <- tryCatch(
          !mirai::unresolved(tasks[[index]]),
          error = function(error) FALSE
        )
        while (!stopped && concurrency_elapsed() < stop_deadline) {
          Sys.sleep(0.001)
          stopped <- tryCatch(
            !mirai::unresolved(tasks[[index]]),
            error = function(error) FALSE
          )
        }
        if (!stopped) {
          shutdown_owned(strict = TRUE)
          cli::cli_abort(
            c(
              "Timed-out mirai task did not stop cleanly",
              "x" = "The dsprrr-owned worker pool was shut down before returning."
            ),
            class = c(
              "dsprrr_mirai_task_stop_error",
              "dsprrr_concurrency_error"
            )
          )
        }
        task_timeout_count <- task_timeout_count + 1L
        timeout_result <- create_concurrency_error_result(
          module = module,
          input_set = input_sets[[index]],
          index = index,
          llm = NULL,
          .return_format = .return_format,
          runtime = .concurrency,
          message = paste0(
            "Task timed out after ",
            .concurrency$task_timeout,
            " seconds"
          ),
          classes = c(
            "dsprrr_mirai_timeout_error",
            "dsprrr_concurrency_task_timeout_error",
            "dsprrr_mirai_worker_error"
          ),
          cancellation_reason = "task_timeout",
          started_at = task_started_at[[index]]
        )
        set_concurrency_result_latency(
          timeout_result,
          latency_ms = (concurrency_elapsed() -
            task_started_elapsed[[index]]) *
            1000,
          .return_format = .return_format
        )
      } else {
        annotate_concurrency_result(
          mirai_worker_result(
            record = record,
            module = module,
            input_set = input_sets[[index]],
            request = requests[[index]],
            chat = llm_template,
            index = index,
            .return_format = .return_format,
            fallback_started_at = batch_started_at,
            fallback_ended_at = Sys.time()
          ),
          .concurrency,
          .return_format
        )
      }

      if (
        identical(.return_format, "simple") &&
          length(result) == 1L &&
          is.na(result) &&
          !is.null(attr(result, "error_message", exact = TRUE))
      ) {
        warnings_to_emit <- c(
          warnings_to_emit,
          paste0(
            "Failed to process item ",
            index,
            ": ",
            attr(result, "error_message", exact = TRUE)
          )
        )
      }

      results[[index]] <- result
      completed <- completed + 1L
      if (concurrency_row_failed(result, .return_format)) {
        error_count <- error_count + 1L
      }
      update_progress()
    }
    active <- setdiff(active, resolved)

    if (
      !stop_scheduling &&
        concurrency_error_budget_reached(
          error_count,
          .concurrency$max_errors
        )
    ) {
      stop_scheduling <- TRUE
      queued <- if (next_index <= n) seq.int(next_index, n) else integer()
      if (isTRUE(.concurrency$cancel)) {
        cancelled <- c(active, queued)
        stop_active(active)
        active <- integer()
      } else {
        cancelled <- queued
      }
      next_index <- n + 1L
      fill_concurrency_rows(
        cancelled,
        message = function(index) {
          paste0(
            "Item ",
            index,
            " was cancelled because the batch error budget was reached"
          )
        },
        classes = c(
          "dsprrr_concurrency_cancelled_error",
          "dsprrr_concurrency_error"
        ),
        reason = "max_errors"
      )
      cli::cli_warn(c(
        "Batch error budget reached during mirai execution",
        "x" = "Items cancelled or not started: {length(cancelled)}."
      ))
    }

    launch_available()
  }

  if (task_timeout_count > 0L) {
    cli::cli_warn(
      "{task_timeout_count} mirai task{?s} timed out after {(.concurrency$task_timeout)} seconds"
    )
  }

  if (profile_owned) {
    shutdown_owned(strict = TRUE)
  }

  # Emit accumulated warnings
  for (warning_msg in warnings_to_emit) {
    cli::cli_warn(warning_msg)
  }

  collect_backend_traces(results)
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
        list(ellmer::ContentText(
          if (nchar(instructions) > 0) {
            paste(instructions, prompt, sep = "\n\n")
          } else {
            prompt
          }
        )),
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
#' Zero-row data frames return a zero-row tibble with the same result columns
#' as a non-empty call, without resolving a Chat or changing runtime state.
#'
#' @param module A DSPrrr module (e.g., created with `module()`)
#' @param data A tibble or data frame with columns matching the module's inputs.
#' @param ... Additional arguments passed to [run()].
#'
#' @return A tibble with the input columns plus a `result` list-column. With
#'   `.return_format = "structured"`, the tibble also contains `.error`,
#'   `.metadata`, and `.chat`; `.error` is `NA` for successful rows and contains
#'   the LLM execution error message for failed rows.
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
#' @param .concurrency Optional batch policy created by
#'   [concurrency_control()]. Do not combine it with `.parallel` or
#'   `.parallel_method`.
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
  .concurrency = NULL,
  .progress = TRUE,
  .return_format = "simple",
  ...
) {
  parallel_missing <- missing(.parallel)
  parallel_method_missing <- missing(.parallel_method)
  concurrency_missing <- missing(.concurrency)
  concurrency <- resolve_concurrency_control(
    .concurrency = .concurrency,
    concurrency_missing = concurrency_missing,
    .parallel = .parallel,
    parallel_missing = parallel_missing,
    .parallel_method = .parallel_method,
    parallel_method_missing = parallel_method_missing
  )
  explicit_concurrency <- !concurrency_missing && !is.null(.concurrency)
  .parallel_method <- match.arg(.parallel_method)
  .return_format <- match.arg(.return_format, c("simple", "structured"))
  dots <- list(...)
  if (".cache" %in% names(dots)) {
    validate_cache_arg(dots$.cache)
  }
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

  if (nrow(data) == 0L) {
    data$result <- vector("list", 0L)
    if (.return_format == "structured") {
      data$.error <- character(0L)
      data$.metadata <- vector("list", 0L)
      data$.chat <- vector("list", 0L)
    }
    return(tibble::as_tibble(data))
  }

  # Extract input columns as list
  if (length(required_names) > 0) {
    input_args <- as.list(data[required_names])
  } else {
    # If no specific inputs, try to use all columns
    input_args <- as.list(data)
  }

  # Run batch processing
  execution_args <- if (explicit_concurrency) {
    list(.concurrency = concurrency)
  } else {
    list(
      .parallel = .parallel,
      .parallel_method = .parallel_method
    )
  }
  results <- do.call(
    run,
    c(
      list(module = module),
      input_args,
      c(
        list(
          .llm = .llm,
          .verbose = .verbose,
          .progress = .progress,
          .return_format = .return_format
        ),
        execution_args
      ),
      dots # Pass through additional arguments like .cache
    )
  )

  if (.return_format == "structured" && inherits(results, "dsprrr_result")) {
    results <- list(results)
  }

  # Add results to data
  if (.return_format == "simple") {
    if (nrow(data) == 1L || length(results) != nrow(data)) {
      results <- list(results)
    }
    data$result <- results
  } else {
    # For structured format, extract outputs and add metadata columns
    data$result <- lapply(results, `[[`, "output")
    data$.error <- vapply(
      results,
      function(result) result$metadata$error %||% NA_character_,
      character(1)
    )
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
      # Errors from create_error_result() are stored in metadata$error with
      # output = NA; also tolerate other shapes defensively.
      run_error_present(item$metadata$error) ||
        isTRUE(item$error) ||
        inherits(item$output, "error")
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
