#' Compose a per-attempt cache-partition id that nests cleanly
#'
#' Retrying wrappers (BestOfN, Refine, Assert) tag each attempt with a
#' `rollout_id` so retries get distinct cache keys. When wrappers are nested
#' (e.g. `refine(best_of_n(mod))`) the outer wrapper passes its own id down;
#' combining it with the inner attempt index keeps every (outer, inner) pair
#' unique while consuming the inherited id (so it is not matched twice).
#'
#' @param rollout_id Inherited id from an enclosing wrapper, or NULL.
#' @param i Integer attempt index for this wrapper.
#' @return A character id, e.g. `"2"` at the top level or `"2.1"` when nested.
#' @noRd
compose_rollout_id <- function(rollout_id, i) {
  if (is.null(rollout_id)) as.character(i) else paste0(rollout_id, ".", i)
}

#' Evaluate code with an extra cache partition
#'
#' Responses cached inside `code` are keyed by `scope` as well, so repeated
#' evaluation epochs get fresh responses instead of replaying the first epoch.
#' `NULL` leaves the current partition unchanged.
#' @noRd
with_rollout_scope <- function(scope, code) {
  old <- .dsprrr_env$rollout_scope
  if (!is.null(scope)) {
    .dsprrr_env$rollout_scope <- compose_rollout_id(old, scope)
  }
  on.exit(assign("rollout_scope", old, envir = .dsprrr_env), add = TRUE)
  force(code)
}

#' Combine the active rollout scope with a call's own rollout id
#' @noRd
scoped_rollout_id <- function(rollout_id) {
  scope <- .dsprrr_env$rollout_scope
  if (is.null(scope)) {
    rollout_id
  } else {
    compose_rollout_id(scope, rollout_id %||% "")
  }
}

#' Reject R's partial matching for public constructor arguments
#' @noRd
reject_partial_argument_matches <- function(call, fn) {
  supplied <- names(as.list(call)[-1L])
  if (is.null(supplied)) {
    return(invisible(NULL))
  }

  supplied <- supplied[nzchar(supplied)]
  formal_names <- setdiff(names(formals(fn)), "...")
  partial <- supplied[
    !supplied %in% formal_names &
      vapply(
        supplied,
        function(name) sum(startsWith(formal_names, name)) == 1L,
        logical(1)
      )
  ]
  if (length(partial) == 0L) {
    return(invisible(NULL))
  }

  matched <- vapply(
    partial,
    function(name) formal_names[startsWith(formal_names, name)][[1L]],
    character(1)
  )
  details <- paste0("`", partial, "` -> `", matched, "`")
  cli::cli_abort(
    c(
      "Argument names must match exactly",
      "x" = "Partial matches: {details}"
    ),
    class = "dsprrr_argument_name_error"
  )
}

#' Check if object inherits from ellmer type
#'
#' @noRd
is_ellmer_type <- function(x) {
  inherits(x, "ellmer::Type") ||
    inherits(x, "ellmer::TypeBasic") ||
    inherits(x, "ellmer::TypeEnum") ||
    inherits(x, "ellmer::TypeArray") ||
    inherits(x, "ellmer::TypeObject") ||
    inherits(x, "ellmer::TypeJsonSchema")
}

#' Decide whether a vignette's LLM examples run
#'
#' Articles with recorded provider responses (vcr cassettes in `_vcr/`) replay
#' them, so the pkgdown site shows real output. On CI that is the only way code
#' runs: pkgdown site builds replay cassettes, and every other build (including
#' `R CMD build` and `R CMD check`) stays offline. Locally, an interactive
#' session or `DSPRRR_VIGNETTES_EVAL=true` also runs examples live when
#' provider credentials are available, which is how cassettes are recorded.
#' @noRd
eval_vignette <- function() {
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    return(FALSE)
  }

  name <- tools::file_path_sans_ext(knitr::current_input())
  cassettes <- dir("_vcr", pattern = paste0("^", name, "-.*\\.yml$"))
  has_cassette <- length(cassettes) > 0

  if (nzchar(Sys.getenv("CI"))) {
    should_eval <- has_cassette && identical(Sys.getenv("IN_PKGDOWN"), "true")
  } else {
    force_eval <- identical(Sys.getenv("DSPRRR_VIGNETTES_EVAL"), "true") ||
      identical(Sys.getenv("VITALS_SHOULD_EVAL"), "true")
    should_eval <- (interactive() || force_eval) &&
      (has_cassette || has_ellmer_credentials())
  }

  if (should_eval) {
    # Keep console chatter out of the rendered page: ellmer's streaming echo,
    # progress bars, masking notices from library(), and the one-time cache
    # notice.
    options(
      ellmer_echo = "none",
      cli.progress_show_after = Inf,
      conflicts.policy = list(warn = FALSE)
    )
    .dsprrr_env$cache_first_hit_shown <- TRUE
  }
  should_eval
}

#' Check for ellmer credentials
#'
#' @return Logical indicating if any LLM API keys are available
#' @noRd
has_ellmer_credentials <- function() {
  any(
    nzchar(Sys.getenv("OPENAI_API_KEY")),
    nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
    nzchar(Sys.getenv("GOOGLE_GEMINI_API_KEY"))
  )
}

#' Find closest match for "Did you mean?" suggestions
#'
#' Uses Levenshtein distance to find the closest match to a given string
#' from a set of valid options.
#'
#' @param input The input string to match
#' @param valid_options Character vector of valid options
#' @param max_distance Maximum edit distance to consider (default 3)
#' @return The closest match, or NULL if no match within max_distance
#' @noRd
find_closest_match <- function(input, valid_options, max_distance = 3L) {
  if (length(valid_options) == 0) {
    return(NULL)
  }

  # Calculate distances
  distances <- vapply(
    valid_options,
    function(opt) {
      as.integer(utils::adist(tolower(input), tolower(opt))[1, 1])
    },
    integer(1)
  )

  # Find minimum distance

  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]

  # Only suggest if within max_distance
  if (min_dist <= max_distance) {
    valid_options[min_idx]
  } else {
    NULL
  }
}

#' Format "Did you mean?" suggestion for error message
#'
#' @param input The input that didn't match
#' @param valid_options Character vector of valid options
#' @param max_distance Maximum edit distance to consider
#' @return A cli-formatted string, or NULL if no suggestion
#' @noRd
suggest_match <- function(input, valid_options, max_distance = 3L) {
  match <- find_closest_match(input, valid_options, max_distance)
  if (!is.null(match)) {
    paste0("Did you mean {.field ", match, "}?")
  } else {
    NULL
  }
}

#' Runtime arguments that were removed from the public calling contract
#'
#' Dot-prefixed names never reach `...` while they are formal arguments, so an
#' undeclared dot-prefixed input is always a mistyped or removed runtime
#' argument rather than a signature field.
#' @noRd
removed_runtime_arguments <- function() {
  c(
    ".parallel" = "Use {.arg .concurrency} with {.fn concurrency_control}.",
    ".parallel_method" = "Use {.arg .concurrency} with {.fn concurrency_control}."
  )
}

#' Reject undeclared dot-prefixed inputs
#'
#' Runtime arguments are formal parameters, so anything dot-prefixed that lands
#' in `...` is a typo or an argument this version no longer accepts. Absorbing
#' it as a template field would silently change behaviour, so fail closed.
#' @noRd
validate_reserved_input_names <- function(provided_names, declared_names) {
  dotted <- setdiff(
    grep("^\\.", provided_names, value = TRUE),
    declared_names
  )
  if (length(dotted) == 0L) {
    return(invisible(NULL))
  }

  removed <- removed_runtime_arguments()
  hints <- unname(removed[intersect(dotted, names(removed))])
  cli::cli_abort(
    c(
      "Unknown dot-prefixed argument{?s}: {.arg {dotted}}",
      "x" = "These are not treated as signature fields.",
      rlang::set_names(hints, rep("i", length(hints)))
    ),
    class = c(
      "dsprrr_reserved_input_error",
      "dsprrr_input_validation_error"
    )
  )
}

# Validate runtime-only arguments from an unforced `...` pairlist.
validate_runtime_dot_arguments <- function(call, allowed_names = character()) {
  dots <- call[["..."]]
  provided_names <- if (is.null(dots)) {
    character()
  } else {
    names(dots) %||% rep("", length(dots))
  }
  validate_reserved_input_names(provided_names, allowed_names)
}

#' Validate call inputs against a signature
#' @noRd
validate_signature_inputs <- function(
  sig,
  inputs,
  missing = c("error", "warn", "ignore"),
  extra = c("warn", "error", "ignore"),
  type = c("warn", "error", "ignore"),
  context = "inputs",
  supplied = character()
) {
  missing <- match.arg(missing)
  extra <- match.arg(extra)
  type <- match.arg(type)

  declared_names <- if (length(sig@inputs) == 0) {
    character()
  } else {
    vapply(sig@inputs, function(x) x$name, character(1))
  }
  validate_reserved_input_names(names(inputs) %||% character(), declared_names)

  if (length(sig@inputs) == 0) {
    return(invisible(NULL))
  }

  required <- vapply(
    sig@inputs,
    function(x) tryCatch(isTRUE(x$type@required), error = function(e) TRUE),
    logical(1)
  )
  # Inputs the module fills in itself are never required from the caller.
  required_names <- setdiff(declared_names[required], supplied)
  provided_names <- names(inputs) %||% character()

  missing_names <- setdiff(required_names, provided_names)
  if (length(missing_names) > 0 && missing != "ignore") {
    message <- build_missing_input_message(
      missing_names = missing_names,
      provided_names = provided_names,
      required_names = required_names,
      context = context
    )

    if (missing == "error") {
      cli::cli_abort(message)
    } else {
      cli::cli_warn(message, class = "dsprrr_missing_input_warning")
    }
  }

  extra_names <- setdiff(provided_names, declared_names)
  if (length(extra_names) > 0 && extra != "ignore") {
    for (field in extra_names) {
      suggestion <- find_closest_match(field, declared_names)
      message <- if (!is.null(suggestion)) {
        c(
          "Unknown input: {.field {field}}",
          "i" = "Did you mean: {.field {suggestion}}?",
          "i" = "Available fields: {.field {declared_names}}"
        )
      } else {
        c(
          "Extra input not declared in the signature: {.field {field}}",
          "i" = "The field remains available to custom templates but is not declared in the signature.",
          "i" = "Signature fields: {.field {declared_names}}"
        )
      }
      if (extra == "error") {
        cli::cli_abort(
          message,
          class = c(
            "dsprrr_extra_input_error",
            "dsprrr_input_validation_error"
          )
        )
      } else {
        cli::cli_warn(
          message,
          class = "dsprrr_extra_input_warning"
        )
      }
    }
  }

  if (type == "error") {
    warn_signature_type_mismatches(sig, inputs, action = "error")
  } else if (
    type == "warn" && isTRUE(getOption("dsprrr.warn_on_type_mismatch", TRUE))
  ) {
    warn_signature_type_mismatches(sig, inputs, action = "warn")
  }

  invisible(NULL)
}

#' Build a missing-input message with suggestions
#' @noRd
build_missing_input_message <- function(
  missing_names,
  provided_names,
  required_names,
  context = "inputs"
) {
  msg <- c("Missing required {context}: {.field {missing_names}}")

  for (missing in missing_names) {
    suggestion <- suggest_match(missing, provided_names)
    if (!is.null(suggestion)) {
      msg <- c(msg, "i" = suggestion)
    }
  }

  c(
    msg,
    "i" = "Signature expects: {.field {required_names}}",
    if (length(provided_names) > 0) {
      c("i" = "You provided: {.field {provided_names}}")
    }
  )
}

#' Warn when provided values do not match declared input types
#' @noRd
warn_signature_type_mismatches <- function(
  sig,
  inputs,
  action = c("warn", "error")
) {
  action <- match.arg(action)
  for (input_spec in sig@inputs) {
    name <- input_spec$name
    if (!name %in% names(inputs)) {
      next
    }

    value <- inputs[[name]]
    expected_type <- input_spec$type

    if (
      !isTRUE(input_spec$.type_explicit) ||
        is.null(value) ||
        is_content_input(value)
    ) {
      next
    }

    if (!input_value_matches_type(value, expected_type)) {
      expected <- format_ellmer_type(expected_type, verbose = TRUE)
      actual <- paste(class(value), collapse = "/")
      message <- c(
        "Type mismatch for input {.field {name}}",
        "i" = "Expected {.val {expected}} from the signature, got {.cls {actual}}."
      )
      if (action == "error") {
        cli::cli_abort(
          c(
            message,
            "i" = "Provide a value compatible with the declared signature."
          ),
          class = c(
            "dsprrr_type_mismatch_error",
            "dsprrr_input_validation_error"
          )
        )
      } else {
        cli::cli_warn(
          c(
            message,
            "i" = "Disable with {.code options(dsprrr.warn_on_type_mismatch = FALSE)}."
          ),
          class = "dsprrr_type_mismatch_warning"
        )
      }
    }
  }

  invisible(NULL)
}

#' Check whether an R value is compatible with an ellmer type
#' @noRd
input_value_matches_type <- function(value, expected_type) {
  if (
    is.null(expected_type) || inherits(expected_type, "ellmer::TypeJsonSchema")
  ) {
    return(TRUE)
  }

  if (inherits(expected_type, "ellmer::TypeBasic")) {
    type_name <- expected_type@type
    return(switch(
      type_name,
      "string" = is.character(value),
      "number" = is.numeric(value),
      "integer" = is.integer(value) ||
        (is.numeric(value) && all(is.na(value) | value == floor(value))),
      "boolean" = is.logical(value),
      TRUE
    ))
  }

  if (inherits(expected_type, "ellmer::TypeEnum")) {
    return(
      is.character(value) && all(is.na(value) | value %in% expected_type@values)
    )
  }

  if (inherits(expected_type, "ellmer::TypeArray")) {
    return(is.atomic(value) || is.list(value))
  }

  if (inherits(expected_type, "ellmer::TypeObject")) {
    return(is.list(value) || is.data.frame(value) || is.environment(value))
  }

  TRUE
}

#' Test whether a model name is a reasoning model
#'
#' @description
#' `is_reasoning_model()` guesses from its name whether a model is a
#' reasoning model: OpenAI's o-series (`o1`, `o3`, `o4-mini`, ...), the
#' GPT-5 and GPT-6 families (such as `gpt-6-luna`), and any name containing
#' "reasoning". Reasoning models are tuned with `reasoning_effort` rather
#' than `temperature` or `top_p`; gpt-6-luna, for example, accepts
#' `temperature` and `top_p` only with `reasoning_effort = "none"`.
#'
#' @details
#' [module_parameters()] uses this check to decide which parameters to
#' offer for tuning. dsprrr does not change the parameters of calls made with
#' [run()] for reasoning models.
#'
#' @param model_name A model name, such as `"gpt-6-luna"` or `"o3"`.
#' @return `TRUE` or `FALSE`. `NULL`, `NA` and empty names give `FALSE`.
#' @export
#' @family configuration
#' @examples
#' is_reasoning_model("gpt-6-luna")
#' is_reasoning_model("o4-mini")
#' is_reasoning_model("gpt-4o")
is_reasoning_model <- function(model_name) {
  if (is.null(model_name) || is.na(model_name) || !nzchar(model_name)) {
    return(FALSE)
  }
  model_lower <- tolower(model_name)

  reasoning_patterns <- c(
    "^o[0-9]", # o1, o3, o4-mini
    "^gpt-5", # gpt-5 series
    "^gpt-6", # gpt-6 series (reasoning on by default)
    "-reasoning", # explicit reasoning suffix
    "reasoning" # generic reasoning indicator
  )

  any(vapply(reasoning_patterns, function(p) grepl(p, model_lower), logical(1)))
}

#' Render an ellmer Turn as plain text
#' @noRd
render_turn_text <- function(turn) {
  render_turn_content(turn, format = "text")
}

#' Render an ellmer Turn as markdown
#' @noRd
render_turn_markdown <- function(turn) {
  render_turn_content(turn, format = "markdown")
}

#' Render an ellmer Turn as HTML
#' @noRd
render_turn_html <- function(turn) {
  render_turn_content(turn, format = "html")
}

#' Render an ellmer Turn with fallbacks for non-text content
#' @noRd
render_turn_content <- function(turn, format = c("text", "markdown", "html")) {
  format <- match.arg(format)

  if (is.null(turn)) {
    return(NA_character_)
  }

  rendered <- tryCatch(
    {
      switch(
        format,
        text = ellmer::contents_text(turn),
        markdown = ellmer::contents_markdown(turn),
        html = ellmer::contents_html(turn)
      )
    },
    error = function(e) NULL
  )

  if (!is.null(rendered) && length(rendered) == 1 && nzchar(rendered)) {
    return(as.character(rendered))
  }

  contents <- tryCatch(turn@contents, error = function(e) list())
  if (length(contents) == 0) {
    return(NA_character_)
  }

  parts <- vapply(
    contents,
    render_content_summary,
    character(1),
    format = format
  )
  parts <- parts[nzchar(parts)]

  if (length(parts) == 0) {
    NA_character_
  } else {
    paste(parts, collapse = if (identical(format, "html")) "" else "\n")
  }
}

#' Summarise a single ellmer content object
#' @noRd
render_content_summary <- function(
  content,
  format = c("text", "markdown", "html")
) {
  format <- match.arg(format)

  wrap <- function(text) {
    if (identical(format, "html")) {
      safe_text <- gsub("&", "&amp;", text, fixed = TRUE)
      safe_text <- gsub("<", "&lt;", safe_text, fixed = TRUE)
      safe_text <- gsub(">", "&gt;", safe_text, fixed = TRUE)
      paste0("<p>", safe_text, "</p>")
    } else {
      text
    }
  }

  is_content_class <- function(name) {
    any(grepl(paste0(name, "$"), class(content)))
  }

  if (is_content_class("ContentText")) {
    return(content@text %||% "")
  }

  if (is_content_class("ContentToolRequest")) {
    args <- jsonlite::toJSON(
      content@arguments,
      auto_unbox = TRUE,
      pretty = FALSE
    )
    return(wrap(paste0("[tool request] ", content@name, " ", args)))
  }

  if (is_content_class("ContentToolResult")) {
    result <- if (!is.null(content@error)) {
      paste0("error: ", content@error)
    } else {
      format_output(content@value)
    }
    tool_name <- tryCatch(content@request@name, error = function(e) "tool")
    return(wrap(paste0("[tool result] ", tool_name, " ", result)))
  }

  if (
    is_content_class("ContentImageRemote") ||
      is_content_class("ContentImageInline")
  ) {
    label <- paste0("[image] ", class(content)[1])
    return(wrap(label))
  }

  if (is_content_class("ContentPDF")) {
    return(wrap("[pdf]"))
  }

  wrap(paste0("[content] ", class(content)[1]))
}

#' Extract token metrics from a trace entry
#' @noRd
trace_tokens <- function(trace) {
  if (is.list(trace$tokens)) {
    return(list(
      input_tokens = as.integer(trace$tokens$input_tokens %||% NA_integer_),
      output_tokens = as.integer(trace$tokens$output_tokens %||% NA_integer_),
      cached_input_tokens = as.integer(
        trace$tokens$cached_input_tokens %||% NA_integer_
      ),
      total_tokens = as.integer(trace$tokens$total_tokens %||% NA_integer_)
    ))
  }

  assistant_turn <- trace$assistant_turn
  if (!is.null(assistant_turn) && !is.null(assistant_turn@tokens)) {
    tokens <- assistant_turn@tokens
    return(list(
      input_tokens = as.integer(tokens[1] %||% NA_integer_),
      output_tokens = as.integer(tokens[2] %||% NA_integer_),
      cached_input_tokens = as.integer(tokens[3] %||% NA_integer_),
      total_tokens = as.integer(sum(tokens[1:2], na.rm = TRUE))
    ))
  }

  list(
    input_tokens = as.integer(trace$input_tokens %||% NA_integer_),
    output_tokens = as.integer(trace$output_tokens %||% NA_integer_),
    cached_input_tokens = as.integer(
      trace$cached_input_tokens %||% NA_integer_
    ),
    total_tokens = as.integer(trace$total_tokens %||% NA_integer_)
  )
}

#' Extract cost from a trace entry
#' @noRd
trace_cost <- function(trace) {
  trace$cost %||%
    tryCatch(trace$assistant_turn@cost, error = function(e) NA_real_)
}

#' Sum cost values without treating missing prices as free
#' @noRd
sum_cost_values <- function(costs) {
  costs <- as.numeric(costs)
  if (length(costs) == 0) {
    return(0)
  }
  if (anyNA(costs)) {
    return(NA_real_)
  }
  sum(costs)
}

#' Extract latency from a trace entry
#' @noRd
trace_latency_ms <- function(trace) {
  trace$latency_ms %||%
    tryCatch(
      (trace$assistant_turn@duration %||% NA_real_) * 1000,
      error = function(e) NA_real_
    )
}

#' Extract prompt text from a trace entry
#' @noRd
trace_prompt_text <- function(trace) {
  trace$prompt %||% render_turn_text(trace$user_turn)
}

#' Extract prompt markdown from a trace entry
#' @noRd
trace_prompt_markdown <- function(trace) {
  render_turn_markdown(trace$user_turn)
}

#' Extract prompt HTML from a trace entry
#' @noRd
trace_prompt_html <- function(trace) {
  render_turn_html(trace$user_turn)
}

#' Extract response text from a trace entry
#' @noRd
trace_response_text <- function(trace) {
  if (!is.null(trace$assistant_turn)) {
    render_turn_text(trace$assistant_turn)
  } else if (!is.null(trace$output)) {
    format_output(trace$output)
  } else {
    NA_character_
  }
}

#' Extract response markdown from a trace entry
#' @noRd
trace_response_markdown <- function(trace) {
  if (!is.null(trace$assistant_turn)) {
    render_turn_markdown(trace$assistant_turn)
  } else {
    NA_character_
  }
}

#' Extract response HTML from a trace entry
#' @noRd
trace_response_html <- function(trace) {
  if (!is.null(trace$assistant_turn)) {
    render_turn_html(trace$assistant_turn)
  } else {
    NA_character_
  }
}
