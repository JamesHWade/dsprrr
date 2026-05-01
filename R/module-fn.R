#' Callable Module
#'
#' @description
#' `module_fn()` wraps an ordinary R function in a dsprrr Module. This gives
#' custom callables the same `run()` and `evaluate()` surface as built-in
#' modules without requiring users to subclass dsprrr internals.
#'
#' @param signature A [Signature] object or signature string.
#' @param forward Function called with named signature inputs. If the function
#'   accepts `.llm` or `...`, the active chat object is passed as `.llm`.
#'   Return a named list matching the signature output fields, or a scalar when
#'   the signature has exactly one output field.
#' @param chat Optional ellmer Chat object stored on the module.
#' @param name Optional module name stored in `config$name`.
#' @param config Optional configuration metadata.
#'
#' @return An R6 `FnModule` object inheriting from the internal Module base class.
#' @export
#'
#' @examples
#' summarizer <- module_fn(
#'   "text -> summary",
#'   function(text, ...) list(summary = substr(text, 1, 20))
#' )
#' run(summarizer, text = "A long piece of text")
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

  if (!is.null(config) && !is.list(config)) {
    cli::cli_abort("{.arg config} must be a list")
  }

  FnModule$new(
    signature = signature,
    forward_fn = forward,
    chat = chat,
    name = name,
    config = config %||% list()
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

      llm <- .llm %||% self$chat
      start_time <- Sys.time()
      raw_result <- call_fn_module_forward(private$.forward_fn, inputs, llm, ...)
      end_time <- Sys.time()

      normalized <- normalize_fn_module_output(raw_result, self$signature)
      output <- normalized$output
      metadata <- normalized$metadata
      metadata$timestamp <- end_time
      metadata$latency_ms <- as.numeric(
        difftime(end_time, start_time, units = "secs")
      ) * 1000

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
          model = tryCatch(llm$get_model(), error = function(e) NA_character_)
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

  missing <- setdiff(output_names, names(result))
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

  result <- result[output_names]
  properties <- output_type@properties
  for (field in output_names) {
    validate_ellmer_output_value(result[[field]], properties[[field]], field = field)
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
    if (!is.character(value) || any(!value %in% type@values)) {
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
    invisible(lapply(value, validate_ellmer_output_value, type = type@items, field = field))
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
      "Callable modules created with {.fn module_fn} do not support optimization yet",
      "i" = "Use {.fn run} or {.fn evaluate} with this module, or wrap an optimizable dsprrr module instead."
    ))
  }

  invisible(program)
}
