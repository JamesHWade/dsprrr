#' Wrap an R function as a module
#'
#' @description
#' `module_fn()` turns an ordinary R function into a module, so it can be used
#' with [run()], [run_dataset()], [evaluate()] and [pipeline()] like any other.
#' Use it for rule-based baselines, for steps that call other code or
#' services, or for your own logic around several model calls.
#'
#' @param signature A signature from [signature()], or a signature string.
#' @param forward A function called once per input row with the inputs as
#'   named arguments. If it has a `.llm` argument or `...`, it also receives a
#'   chat as `.llm`: the one passed to [run()], else the module's chat, else
#'   the scoped or default chat, else `NULL` (no chat is auto-detected from
#'   API keys). It returns a named list with the signature's output fields,
#'   or a single value when there is one output field. Missing or unknown
#'   fields, and values of the wrong type, are an error.
#' @param chat An ellmer Chat stored on the module and passed to `forward` as
#'   `.llm`.
#' @param name Optional module name, stored in `config$name`.
#' @param config Optional list of settings stored on the module.
#'
#' @details
#' Function-backed modules record traces but not token counts or costs.
#' [compile()] and the optimizers refuse them, because there is no prompt to
#' optimize.
#'
#' @return A module (an R6 object of class `FnModule`).
#' @export
#' @family program constructors
#'
#' @examples
#' truncate <- module_fn(
#'   "text -> summary",
#'   function(text) substr(text, 1, 20)
#' )
#' run(truncate, text = "A long piece of text that needs a summary")
#'
#' # Return a named list for several outputs
#' stats <- module_fn(
#'   "text -> n_words: int, n_chars: int",
#'   function(text) {
#'     list(n_words = length(strsplit(text, "\\s+")[[1]]), n_chars = nchar(text))
#'   }
#' )
#' run(stats, text = "Four words right here")
#'
#' \dontrun{
#' # Take `.llm` to call a model yourself
#' translate <- module_fn(
#'   "text -> translation",
#'   function(text, .llm) {
#'     .llm$chat(paste("Translate to French:", text), echo = "none")
#'   }
#' )
#' run(translate, text = "Good morning", .llm = ellmer::chat_openai(model = "gpt-6-luna"))
#' }
module_fn <- function(
  signature,
  forward,
  chat = NULL,
  name = NULL,
  config = list()
) {
  if (is.character(signature)) {
    signature <- parse_signature(signature)
  }

  if (!inherits(signature, "dsprrr::Signature")) {
    cli::cli_abort("{.arg signature} must be a Signature object or string")
  }

  if (!is.function(forward)) {
    cli::cli_abort("{.arg forward} must be a function")
  }

  if (!is.list(config)) {
    cli::cli_abort("{.arg config} must be a list")
  }

  FnModule$new(
    signature = signature,
    forward_fn = forward,
    chat = chat,
    name = name,
    config = config
  )
}

#' R6 class for function-backed modules
#' @noRd
FnModule <- R6::R6Class(
  "FnModule",
  inherit = Module,
  private = list(
    .forward_fn = NULL
  ),
  public = list(
    initialize = function(
      signature,
      forward_fn,
      chat = NULL,
      name = NULL,
      config = list()
    ) {
      super$initialize(signature = signature, config = config, chat = chat)
      private$.forward_fn <- forward_fn
      if (!is.null(name)) {
        self$config$name <- name
      }
    },
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      inputs <- if (is.data.frame(batch)) {
        if (nrow(batch) != 1L) {
          cli::cli_abort(c(
            "{.cls FnModule}'s {.fn forward} expects a single row of inputs",
            "x" = "Got {.val {nrow(batch)}} rows",
            "i" = "Use {.fn run_dataset} or {.fn evaluate} to process multi-row data"
          ))
        }
        as.list(batch[1, , drop = FALSE])
      } else {
        batch
      }

      validate_signature_inputs(
        self$signature,
        inputs,
        missing = "error",
        extra = "warn",
        type = "warn",
        context = "inputs"
      )

      llm <- resolve_module_llm(self, .llm = .llm, create = FALSE)
      start_time <- Sys.time()
      raw_result <- call_fn_module_forward(
        private$.forward_fn,
        inputs,
        llm,
        ...
      )
      end_time <- Sys.time()

      normalized <- normalize_fn_module_output(raw_result, self$signature)
      output <- normalized$output
      metadata <- normalized$metadata
      metadata$timestamp <- end_time
      metadata$latency_ms <- as.numeric(
        difftime(end_time, start_time, units = "secs")
      ) *
        1000

      if (trace) {
        trace_entry <- list(
          timestamp = end_time,
          inputs = inputs,
          output = output,
          prompt = NA_character_,
          latency_ms = metadata$latency_ms,
          tokens = list(
            input_tokens = NA_integer_,
            output_tokens = NA_integer_,
            cached_input_tokens = NA_integer_,
            total_tokens = NA_integer_
          ),
          cost = NA_real_,
          model = fn_module_model_name(llm)
        )
        self$state$traces <- append(self$state$traces, list(trace_entry))
      }

      tibble::tibble(
        output = list(output),
        chat = list(llm),
        metadata = list(metadata)
      )
    }
  )
)

#' Extract the model name from a Chat for trace recording
#' @noRd
fn_module_model_name <- function(llm) {
  if (is.null(llm)) {
    return(NA_character_)
  }
  assert_ellmer_chat(llm, arg = ".llm")
  tryCatch(
    llm$get_model() %||% NA_character_,
    error = function(e) {
      cli::cli_warn(
        "Could not retrieve model name from Chat: {conditionMessage(e)}",
        .frequency = "once",
        .frequency_id = "fn_module_get_model_failure"
      )
      NA_character_
    }
  )
}

#' Call a function-backed module's user function
#' @noRd
call_fn_module_forward <- function(forward, inputs, .llm = NULL, ...) {
  fn_formals <- names(formals(forward))
  accepts_dots <- "..." %in% fn_formals

  args <- inputs
  if (".llm" %in% fn_formals || accepts_dots) {
    args$.llm <- .llm
  }
  if (accepts_dots) {
    args <- c(args, list(...))
  }

  do.call(forward, args)
}

#' Normalize function-backed module output to the signature contract
#' @noRd
normalize_fn_module_output <- function(result, signature) {
  metadata <- list()
  if (is.list(result) && ".metadata" %in% names(result)) {
    metadata <- result$.metadata
    result$.metadata <- NULL
    if (!is.list(metadata)) {
      cli::cli_abort("{.field .metadata} must be a list when supplied")
    }
  }

  output_type <- signature@output_type
  output_names <- output_field_names(output_type)

  if (length(output_names) == 0) {
    validate_ellmer_output_value(result, output_type, field = "output")
    return(list(output = result, metadata = metadata))
  }

  if (!is.list(result) || is.null(names(result))) {
    if (length(output_names) == 1) {
      result <- rlang::set_names(list(result), output_names)
    } else {
      cli::cli_abort(c(
        "Callable module returned a scalar for a multi-field signature",
        "i" = "Return a named list with fields: {.field {output_names}}"
      ))
    }
  }

  properties <- output_type@properties
  required_names <- output_names[vapply(
    output_names,
    function(field) isTRUE(properties[[field]]@required),
    logical(1)
  )]

  missing <- setdiff(required_names, names(result))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Callable module result is missing required output fields",
      "x" = "Missing: {.field {missing}}",
      "i" = "Expected fields: {.field {output_names}}"
    ))
  }

  extra <- setdiff(names(result), output_names)
  if (length(extra) > 0) {
    cli::cli_abort(c(
      "Callable module result includes unknown output fields",
      "x" = "Unknown: {.field {extra}}",
      "i" = "Expected fields: {.field {output_names}}"
    ))
  }

  # Preserve signature order, keeping only the fields the function returned.
  # Optional fields may legitimately be absent.
  present_names <- intersect(output_names, names(result))
  result <- result[present_names]
  for (field in present_names) {
    validate_ellmer_output_value(
      result[[field]],
      properties[[field]],
      field = field
    )
  }

  list(output = result, metadata = metadata)
}

#' Validate a returned value against an ellmer type where practical
#' @noRd
validate_ellmer_output_value <- function(value, type, field) {
  if (!isTRUE(type@required) && is.null(value)) {
    return(invisible(NULL))
  }

  if (inherits(type, "ellmer::TypeBasic")) {
    type_name <- type@type
    ok <- switch(
      type_name,
      string = is.character(value),
      number = is.numeric(value),
      integer = is.integer(value) ||
        (is.numeric(value) && all(is.na(value) | value == as.integer(value))),
      boolean = is.logical(value),
      TRUE
    )
    if (!isTRUE(ok)) {
      cli::cli_abort(c(
        "Callable module result has the wrong type",
        "x" = "{.field {field}} must be {.val {type_name}}"
      ))
    }
  } else if (inherits(type, "ellmer::TypeEnum")) {
    if (!is.character(value) || !all(value %in% type@values)) {
      cli::cli_abort(c(
        "Callable module result has a value outside the enum",
        "x" = "{.field {field}} must be one of {.val {type@values}}"
      ))
    }
  } else if (inherits(type, "ellmer::TypeArray")) {
    if (!is.vector(value) && !is.list(value)) {
      cli::cli_abort(c(
        "Callable module result has the wrong type",
        "x" = "{.field {field}} must be an array or list"
      ))
    }
    invisible(lapply(
      value,
      validate_ellmer_output_value,
      type = type@items,
      field = field
    ))
  } else if (inherits(type, "ellmer::TypeObject")) {
    if (!is.list(value)) {
      cli::cli_abort(c(
        "Callable module result has the wrong type",
        "x" = "{.field {field}} must be an object/list"
      ))
    }
  }

  invisible(NULL)
}

#' Abort when a callable module is sent to an optimizer
#' @noRd
abort_if_fn_module <- function(program) {
  if (inherits(program, "FnModule")) {
    cli::cli_abort(c(
      "Callable modules created with {.fn module_fn} do not support optimization",
      "i" = "Use {.fn run} or {.fn evaluate} with this module, or wrap an optimizable dsprrr module instead."
    ))
  }

  invisible(program)
}
