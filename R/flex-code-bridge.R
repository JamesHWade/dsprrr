# Interpreter-backed Flex execution
#
# This file contains the opt-in executable Flex runtime. Optimizer-authored R
# source is parsed on the host but is only evaluated by a caller-supplied code
# interpreter. Predictor and tool calls cross a versioned JSON bridge and
# execute in the host process; the sandbox is replayed with immutable responses
# until the program returns its final output.

.flex_code_control_prefix <- "__DSPR_FLEX_CONTROL_V1__:"

flex_resolve_source_format <- function(
  source_format,
  module_src,
  tools,
  interpreter_factory
) {
  source_format <- match.arg(source_format, c("auto", "json", "r"))
  if (!identical(source_format, "auto")) {
    return(source_format)
  }
  if (length(tools) > 0L || !is.null(interpreter_factory)) {
    return("r")
  }
  if (is.null(module_src)) {
    return("json")
  }
  if (
    is.character(module_src) &&
      length(module_src) == 1L &&
      !is.na(module_src) &&
      grepl("^[[:space:]]*[\\{\\[]", module_src)
  ) {
    return("json")
  }
  "r"
}

flex_code_baseline_source <- function(use_rlm = FALSE) {
  constructor <- if (isTRUE(use_rlm)) "RLM" else "Predict"
  paste0(
    "predictor <- ",
    constructor,
    "(\"$outer\")\n",
    "forward <- function(...) predictor(...)\n"
  )
}

flex_validate_code_source <- function(module_src) {
  if (
    !is.character(module_src) ||
      length(module_src) != 1L ||
      is.na(module_src) ||
      !nzchar(trimws(module_src))
  ) {
    flex_source_abort(
      "Executable Flex {.arg module_src} must be one non-empty string",
      class = "dsprrr_flex_code_source_error"
    )
  }
  if (nchar(module_src, type = "bytes") > .flex_max_source_bytes) {
    flex_source_abort(
      "Executable Flex source exceeds the one-megabyte source limit",
      class = "dsprrr_flex_source_size_error"
    )
  }
  parsed <- tryCatch(
    parse(text = module_src, keep.source = FALSE),
    error = function(error) {
      flex_source_abort(
        "Executable Flex source is not valid R code",
        class = "dsprrr_flex_code_parse_error",
        parent = error
      )
    }
  )
  assigns_forward <- vapply(
    as.list(parsed),
    function(expression) {
      is.call(expression) &&
        identical(as.character(expression[[1L]]), "<-") &&
        length(expression) == 3L &&
        identical(expression[[2L]], as.name("forward")) &&
        is.call(expression[[3L]]) &&
        identical(expression[[3L]][[1L]], as.name("function"))
    },
    logical(1)
  )
  if (!any(assigns_forward)) {
    flex_source_abort(
      c(
        "Executable Flex source must define {.code forward <- function(...) ...}",
        "i" = "The complete source is evaluated only inside the configured interpreter."
      ),
      class = "dsprrr_flex_code_contract_error"
    )
  }
  gsub("\\r\\n?", "\n", module_src)
}

flex_validate_host_tools <- function(tools) {
  if (!is.list(tools)) {
    cli::cli_abort(
      "{.arg tools} must be a named list",
      class = "dsprrr_flex_tools_error"
    )
  }
  if (length(tools) == 0L) {
    return(invisible(tools))
  }
  if (!flex_host_tool_names_valid(tools)) {
    cli::cli_abort(
      "{.arg tools} must have unique syntactic names that do not shadow the Flex DSL",
      class = "dsprrr_flex_tools_error"
    )
  }
  valid_tool <- vapply(
    tools,
    function(tool) {
      is.function(tool) ||
        inherits(tool, "ToolDef") ||
        inherits(tool, "ellmer::ToolDef")
    },
    logical(1)
  )
  if (!all(valid_tool)) {
    cli::cli_abort(
      "Every Flex tool must be a function or ellmer ToolDef",
      class = "dsprrr_flex_tools_error"
    )
  }
  invisible(tools)
}

flex_host_tool_names_valid <- function(tools) {
  if (!is.list(tools)) {
    return(FALSE)
  }
  if (length(tools) == 0L) {
    return(TRUE)
  }
  tool_names <- names(tools)
  !is.null(tool_names) &&
    length(tool_names) == length(tools) &&
    !anyNA(tool_names) &&
    all(nzchar(tool_names)) &&
    !anyDuplicated(tool_names) &&
    all(make.names(tool_names) == tool_names) &&
    !any(
      tool_names %in%
        c(
          "Predict",
          "ChainOfThought",
          "ReAct",
          "ReActV2",
          "RLM",
          "CodeAct",
          "ProgramOfThought",
          "Prediction",
          "Tool",
          "forward"
        )
    )
}

flex_tool_function <- function(tool, name) {
  if (is.function(tool)) {
    return(tool)
  }
  candidate <- tryCatch(tool@fun, error = function(error) NULL)
  if (!is.function(candidate)) {
    candidate <- tryCatch(
      methods::slot(tool, "function"),
      error = function(error) NULL
    )
  }
  if (!is.function(candidate)) {
    cli::cli_abort(
      "Flex host tool {.field {name}} does not expose a callable function",
      class = "dsprrr_flex_tools_error"
    )
  }
  candidate
}

flex_code_sandbox_template <- function() {
  .flex_context <- get(".context", inherits = TRUE)
  .flex_nonce <- .flex_context$nonce
  .flex_responses <- .flex_context$responses
  .flex_inputs <- .flex_context$inputs
  .flex_source <- .flex_context$module_src
  .flex_frame_limit <- .flex_context$frame_limit
  if (is.null(.flex_frame_limit)) {
    .flex_frame_limit <- Inf
  }
  .flex_output_fields <- unlist(
    .flex_context$output_fields,
    use.names = FALSE
  )
  .flex_tool_names <- unlist(.flex_context$tool_names, use.names = FALSE)

  .flex_envelope <- function(kind, payload) {
    list(
      .dsprrr_flex_control = TRUE,
      version = 1L,
      nonce = .flex_nonce,
      kind = kind,
      payload = payload
    )
  }

  .flex_encode <- function(envelope) {
    json <- jsonlite::toJSON(
      envelope,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      dataframe = "rows",
      digits = NA
    )
    paste0(
      "__DSPR_FLEX_CONTROL_V1__:",
      gsub(
        "[[:space:]]",
        "",
        jsonlite::base64_enc(charToRaw(as.character(json)))
      )
    )
  }

  .flex_emit <- function(kind, payload, stage) {
    envelope <- .flex_envelope(kind, payload)
    frame <- .flex_encode(envelope)
    frame_bytes <- nchar(frame, type = "bytes")
    if (is.finite(.flex_frame_limit) && frame_bytes > .flex_frame_limit) {
      envelope <- .flex_envelope(
        "overflow",
        list(
          stage = stage,
          encoded_bytes = frame_bytes,
          frame_limit = .flex_frame_limit
        )
      )
      frame <- .flex_encode(envelope)
      if (nchar(frame, type = "bytes") > .flex_frame_limit) {
        stop("Flex control-frame limit is too small", call. = FALSE)
      }
    }
    base::cat(frame)
    base::invisible(envelope)
  }

  # Keeping bridge state outside the guest's lexical parent chain limits
  # accidental reachability through public DSL closures. The nonce correlates
  # one response with the current replay step; it is not a secret or an
  # authentication boundary for guest code.
  .flex_bridge_env <- new.env(parent = baseenv())
  .flex_bridge_env$.responses <- .flex_responses
  .flex_bridge_env$.call_index <- 0L
  .flex_bridge_env$.bridge_call <- eval(
    quote(function(kind, descriptor, arguments) {
      argument_names <- names(arguments)
      if (
        length(arguments) > 0L &&
          (is.null(argument_names) ||
            any(!nzchar(argument_names)) ||
            anyDuplicated(argument_names))
      ) {
        stop("Flex bridge call requires unique named arguments", call. = FALSE)
      }
      .call_index <<- .call_index + 1L
      request <- list(
        index = .call_index,
        kind = kind,
        descriptor = descriptor,
        arguments = arguments
      )
      request_key <- as.character(jsonlite::toJSON(
        request,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        dataframe = "rows",
        digits = NA
      ))

      if (.call_index <= length(.responses)) {
        response <- .responses[[.call_index]]
        if (
          !is.list(response) ||
            !identical(response$request_key, request_key) ||
            !is.logical(response$success) ||
            length(response$success) != 1L ||
            is.na(response$success)
        ) {
          stop(
            "Flex bridge replay diverged from its recorded request",
            call. = FALSE
          )
        }
        if (!isTRUE(response$success)) {
          stop(response$error, call. = FALSE)
        }
        return(response$value)
      }

      stop(structure(
        list(
          message = "Flex bridge request",
          call = NULL,
          request = request,
          request_key = request_key
        ),
        class = c("dsprrr_flex_request", "error", "condition")
      ))
    }),
    envir = .flex_bridge_env
  )

  .flex_api_env <- new.env(parent = baseenv())
  .flex_api_env$.bridge_call <- .flex_bridge_env$.bridge_call
  .flex_api_env$.tool_names <- .flex_tool_names
  .flex_api_env$.named_args <- eval(
    quote(function(arguments, context) {
      argument_names <- names(arguments)
      if (
        length(arguments) > 0L &&
          (is.null(argument_names) ||
            any(!nzchar(argument_names)) ||
            anyDuplicated(argument_names))
      ) {
        stop(context, " requires unique named arguments", call. = FALSE)
      }
      arguments
    }),
    envir = .flex_api_env
  )
  .flex_api_env$.constructor <- eval(
    quote(function(primitive) {
      force(primitive)
      function(signature = "$outer", instructions = NULL, ...) {
        options <- .named_args(list(...), paste0(primitive, "()"))
        if (
          !is.character(signature) ||
            length(signature) != 1L ||
            is.na(signature) ||
            !nzchar(signature)
        ) {
          stop(
            primitive,
            "() signature must be one non-empty string",
            call. = FALSE
          )
        }
        if (
          !is.null(instructions) &&
            (!is.character(instructions) ||
              length(instructions) != 1L ||
              is.na(instructions))
        ) {
          stop(
            primitive,
            "() instructions must be one string or NULL",
            call. = FALSE
          )
        }
        descriptor <- list(
          primitive = primitive,
          signature = signature,
          instructions = instructions,
          options = options
        )
        force(descriptor)
        function(...) {
          .bridge_call("predictor", descriptor, list(...))
        }
      }
    }),
    envir = .flex_api_env
  )
  .flex_api_env$Prediction <- eval(
    quote(function(...) .named_args(list(...), "Prediction()")),
    envir = .flex_api_env
  )
  .flex_api_env$Tool <- eval(
    quote(function(name) {
      if (
        !is.character(name) ||
          length(name) != 1L ||
          is.na(name) ||
          !name %in% .tool_names
      ) {
        stop("Unknown Flex host tool", call. = FALSE)
      }
      force(name)
      function(...) {
        .bridge_call("tool", list(name = name), list(...))
      }
    }),
    envir = .flex_api_env
  )

  .flex_guest <- new.env(parent = baseenv())
  .flex_constructors <- c(
    Predict = "predict",
    ChainOfThought = "chain_of_thought",
    ReAct = "react",
    ReActV2 = "react_v2",
    RLM = "rlm",
    CodeAct = "codeact",
    ProgramOfThought = "program_of_thought"
  )
  for (.flex_name in names(.flex_constructors)) {
    assign(
      .flex_name,
      .flex_api_env$.constructor(.flex_constructors[[.flex_name]]),
      envir = .flex_guest
    )
  }
  assign("Prediction", .flex_api_env$Prediction, envir = .flex_guest)
  assign("Tool", .flex_api_env$Tool, envir = .flex_guest)
  for (.flex_tool_name in .flex_tool_names) {
    assign(
      .flex_tool_name,
      .flex_api_env$Tool(.flex_tool_name),
      envir = .flex_guest
    )
  }

  .flex_execution <- tryCatch(
    {
      eval(parse(text = .flex_source), envir = .flex_guest)
      if (
        !exists("forward", envir = .flex_guest, inherits = FALSE) ||
          !is.function(get("forward", envir = .flex_guest, inherits = FALSE))
      ) {
        stop("Flex source did not define forward()", call. = FALSE)
      }
      .flex_output <- do.call(
        get("forward", envir = .flex_guest, inherits = FALSE),
        .flex_inputs
      )
      if (
        length(.flex_output_fields) == 1L &&
          (!is.list(.flex_output) || is.null(names(.flex_output)))
      ) {
        .flex_output <- stats::setNames(
          list(.flex_output),
          .flex_output_fields
        )
      }
      list(
        kind = "final",
        payload = list(output = .flex_output),
        stage = "final output"
      )
    },
    dsprrr_flex_request = function(condition) {
      list(
        kind = "request",
        payload = list(
          request = condition$request,
          request_key = condition$request_key
        ),
        stage = paste0("bridge request ", condition$request$index)
      )
    }
  )
  .flex_emit(
    .flex_execution$kind,
    .flex_execution$payload,
    .flex_execution$stage
  )
}

flex_code_wrapper <- function() {
  prelude <- paste(
    deparse(body(flex_code_sandbox_template), width.cutoff = 500L),
    collapse = "\n"
  )
  paste0(
    "local({\n",
    prelude,
    "})\n"
  )
}

flex_code_decode_control <- function(values, nonce) {
  is_envelope <- function(value) {
    is.list(value) && identical(value$.dsprrr_flex_control, TRUE)
  }
  is_current <- function(envelope) {
    is_envelope(envelope) &&
      identical(envelope$version, 1L) &&
      identical(envelope$nonce, nonce) &&
      is.character(envelope$kind) &&
      length(envelope$kind) == 1L &&
      !is.na(envelope$kind) &&
      is.list(envelope$payload)
  }

  structured <- Filter(is_envelope, values)
  if (length(structured) > 0L) {
    if (length(structured) != 1L) {
      cli::cli_abort(
        "Flex execution returned an invalid structured control result",
        class = "dsprrr_flex_bridge_protocol_error"
      )
    }
    envelope <- tryCatch(
      jsonlite::fromJSON(
        jsonlite::toJSON(
          structured[[1L]],
          auto_unbox = TRUE,
          null = "null",
          na = "null",
          dataframe = "rows",
          digits = NA
        ),
        simplifyVector = FALSE
      ),
      error = function(error) {
        cli::cli_abort(
          "Flex structured control result is not JSON-compatible",
          class = "dsprrr_flex_bridge_protocol_error",
          parent = error
        )
      }
    )
    if (!is_current(envelope)) {
      cli::cli_abort(
        "Flex execution returned an invalid structured control result",
        class = "dsprrr_flex_bridge_protocol_error"
      )
    }
  } else {
    text_values <- unlist(
      lapply(values, function(value) {
        if (is.character(value)) value[!is.na(value)] else character()
      }),
      use.names = FALSE
    )
    if (length(text_values) == 0L) {
      return(NULL)
    }
    text_values <- unique(text_values)
    text <- paste(text_values, collapse = "\n")
    prefix_locations <- gregexpr(
      .flex_code_control_prefix,
      text,
      fixed = TRUE
    )[[1L]]
    prefix_count <- if (identical(prefix_locations[[1L]], -1L)) {
      0L
    } else {
      length(prefix_locations)
    }
    if (prefix_count == 0L) {
      return(NULL)
    }
    pattern <- paste0(.flex_code_control_prefix, "[A-Za-z0-9+/=]+")
    matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]]
    if (length(matches) != prefix_count || any(!nzchar(matches))) {
      cli::cli_abort(
        "Malformed Flex bridge control frame",
        class = "dsprrr_flex_bridge_protocol_error"
      )
    }
    envelopes <- lapply(matches, function(match) {
      token <- sub(.flex_code_control_prefix, "", match, fixed = TRUE)
      tryCatch(
        jsonlite::fromJSON(
          rawToChar(jsonlite::base64_dec(token)),
          simplifyVector = FALSE
        ),
        error = function(error) NULL
      )
    })
    current <- vapply(envelopes, is_current, logical(1))
    if (sum(current) != 1L) {
      cli::cli_abort(
        "Flex execution did not return exactly one current control frame",
        class = "dsprrr_flex_bridge_protocol_error"
      )
    }
    envelope <- envelopes[[which(current)]]
  }

  if (identical(envelope$kind, "overflow")) {
    payload <- envelope$payload
    cli::cli_abort(
      c(
        "Flex control frame exceeds the configured runner transport limit",
        "x" = "{payload$stage %||% 'Control value'} needs {payload$encoded_bytes %||% '?'} bytes; the limit is {payload$frame_limit %||% '?'} bytes.",
        "i" = "Reduce the bridged value or use a runner with structured result transport."
      ),
      class = "dsprrr_flex_bridge_frame_size_error",
      stage = payload$stage,
      encoded_bytes = payload$encoded_bytes,
      frame_limit = payload$frame_limit
    )
  }
  if (!envelope$kind %in% c("request", "final")) {
    cli::cli_abort(
      "Unknown Flex bridge control-frame kind",
      class = "dsprrr_flex_bridge_protocol_error"
    )
  }
  envelope
}

flex_code_request_key <- function(request) {
  as.character(jsonlite::toJSON(
    request,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA
  ))
}

flex_code_frame_limit <- function(policy) {
  value <- policy$flex_control_frame_limit %||%
    policy$rlm_control_frame_limit
  if (
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value >= 1L &&
      value == floor(value)
  ) {
    return(as.integer(value))
  }
  NULL
}

flex_code_descriptor_signature <- function(descriptor, outer_signature) {
  value <- descriptor$signature
  signature <- if (identical(value, "$outer")) {
    outer_signature
  } else {
    tryCatch(
      parse_signature(value),
      error = function(error) {
        cli::cli_abort(
          "Flex source constructed an invalid predictor signature",
          class = "dsprrr_flex_bridge_signature_error",
          parent = error
        )
      }
    )
  }
  instructions <- descriptor$instructions
  if (!is.null(instructions)) {
    signature <- with_instructions(signature, instructions)
  }
  signature
}

flex_code_predictor <- function(
  descriptor,
  outer_signature,
  tools,
  interpreter_factory,
  config
) {
  primitive <- descriptor$primitive
  if (
    !is.character(primitive) ||
      length(primitive) != 1L ||
      is.na(primitive) ||
      !primitive %in%
        c(
          "predict",
          "chain_of_thought",
          "react",
          "react_v2",
          "rlm",
          "codeact",
          "program_of_thought"
        )
  ) {
    cli::cli_abort(
      "Flex source requested an unsupported predictor primitive",
      class = "dsprrr_flex_bridge_primitive_error"
    )
  }
  options <- descriptor$options
  if (is.null(options)) {
    options <- list()
  }
  if (!is.list(options) || (length(options) > 0L && is.null(names(options)))) {
    cli::cli_abort(
      "Flex predictor options must be a named list",
      class = "dsprrr_flex_bridge_primitive_error"
    )
  }
  allowed_options <- switch(
    primitive,
    predict = character(),
    chain_of_thought = character(),
    react = c("max_iterations"),
    react_v2 = c("max_iterations"),
    rlm = c("max_iterations", "max_llm_calls", "max_output_chars", "verbose"),
    codeact = c("max_iterations"),
    program_of_thought = c("max_iters", "extract_answer")
  )
  unknown <- setdiff(names(options), allowed_options)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      "Unsupported option for Flex primitive {.val {primitive}}: {.field {unknown}}",
      class = "dsprrr_flex_bridge_primitive_error"
    )
  }

  signature <- flex_code_descriptor_signature(descriptor, outer_signature)
  type <- switch(
    primitive,
    react_v2 = "react",
    primitive
  )
  args <- c(
    list(
      signature = signature,
      type = type,
      config = flex_predictor_config(config),
      chat = NULL
    ),
    options
  )
  if (primitive %in% c("react", "react_v2", "codeact")) {
    # Agent modules need ellmer's schemas. Plain functions remain available as
    # direct host bridge calls but are not silently promoted to underspecified
    # provider tools.
    args$tools <- Filter(
      function(tool) inherits(tool, "ellmer::ToolDef"),
      tools
    )
  }
  if (identical(primitive, "rlm")) {
    args$tools <- tools
  }
  if (primitive %in% c("program_of_thought", "codeact", "rlm")) {
    args$interpreter_factory <- interpreter_factory
  }
  do.call(module, args)
}

flex_code_execute_request <- function(
  request,
  outer_signature,
  tools,
  interpreter_factory,
  config,
  llm,
  cache,
  rollout_id,
  call_metadata,
  ...
) {
  index <- request$index
  if (
    !is.numeric(index) ||
      length(index) != 1L ||
      is.na(index) ||
      index != length(call_metadata) + 1L
  ) {
    cli::cli_abort(
      "Flex bridge returned an out-of-order call index",
      class = "dsprrr_flex_bridge_protocol_error"
    )
  }
  arguments <- request$arguments
  if (!is.list(arguments)) {
    cli::cli_abort(
      "Flex bridge call arguments must be a JSON object",
      class = "dsprrr_flex_bridge_protocol_error"
    )
  }

  if (identical(request$kind, "tool")) {
    name <- request$descriptor$name
    if (!is.character(name) || length(name) != 1L || !name %in% names(tools)) {
      cli::cli_abort(
        "Flex source requested an unknown host tool",
        class = "dsprrr_flex_bridge_tool_error"
      )
    }
    started_at <- Sys.time()
    value <- do.call(flex_tool_function(tools[[name]], name), arguments)
    metadata <- list(
      index = as.integer(index),
      kind = "tool",
      name = name,
      latency_ms = as.numeric(difftime(
        Sys.time(),
        started_at,
        units = "secs"
      )) *
        1000
    )
    return(list(value = value, metadata = metadata, predictor = FALSE))
  }

  if (!identical(request$kind, "predictor")) {
    cli::cli_abort(
      "Flex bridge returned an unknown request kind",
      class = "dsprrr_flex_bridge_protocol_error"
    )
  }
  predictor <- flex_code_predictor(
    request$descriptor,
    outer_signature,
    tools,
    interpreter_factory,
    config
  )
  result <- predictor$forward(
    arguments,
    .llm = llm,
    trace = FALSE,
    .cache = cache,
    rollout_id = rollout_id,
    ...
  )
  output_types <- flex_signature_output_types(predictor$signature)
  value <- flex_output_record(
    result$output[[1L]],
    output_types,
    paste0("call_", index),
    output_type = predictor$signature@output_type
  )
  list(
    value = value,
    metadata = list(
      index = as.integer(index),
      kind = "predictor",
      primitive = request$descriptor$primitive,
      signature = request$descriptor$signature,
      inputs = arguments,
      output = value,
      metadata = result$metadata[[1L]]
    ),
    predictor = TRUE
  )
}

flex_code_forward <- function(
  module,
  inputs,
  llm,
  trace,
  cache,
  rollout_id,
  ...
) {
  started_at <- Sys.time()
  resolve_llm <- local({
    resolved <- FALSE
    value <- NULL
    function() {
      if (!resolved) {
        value <<- resolve_module_llm(module, .llm = llm)
        resolved <<- TRUE
      }
      value
    }
  })
  execution <- flex_code_execute(
    module_src = module$module_src,
    inputs = inputs,
    outer_signature = module$signature,
    tools = module$tools,
    interpreter_factory = module$interpreter_factory,
    max_predictor_calls = module$max_predictor_calls,
    max_tool_calls = module$max_tool_calls,
    require_sandbox = module$require_sandbox,
    config = module$config,
    llm = llm,
    llm_resolver = resolve_llm,
    cache = cache,
    rollout_id = rollout_id,
    ...
  )
  finished_at <- Sys.time()
  runtime_llm <- if (execution$predictor_calls > 0L) execution$llm else llm

  predictor_metadata <- Filter(
    function(call) identical(call$kind, "predictor"),
    execution$calls
  )
  step_metadata <- lapply(predictor_metadata, function(call) {
    list(
      name = paste0("call_", call$index),
      primitive = call$primitive,
      metadata = call$metadata
    )
  })
  step_events <- lapply(seq_along(predictor_metadata), function(index) {
    call <- predictor_metadata[[index]]
    event <- flex_predictor_trace_event(
      step = list(
        name = paste0("call_", call$index),
        primitive = call$primitive
      ),
      index = index,
      inputs = call$inputs,
      output = call$output,
      metadata = call$metadata,
      module_src = module$module_src
    )
    event$metadata$bridge_call_index <- as.integer(call$index)
    event
  })
  usage <- flex_aggregate_step_usage(step_metadata)
  model <- if (is.null(runtime_llm)) {
    NA_character_
  } else {
    tryCatch(runtime_llm$get_model(), error = function(error) NA_character_)
  }
  metadata <- list(
    timestamp = finished_at,
    model = model,
    predictor_calls = as.integer(execution$predictor_calls),
    tool_calls = as.integer(execution$tool_calls),
    latency_ms = as.numeric(difftime(
      finished_at,
      started_at,
      units = "secs"
    )) *
      1000,
    input_tokens = usage$input_tokens,
    output_tokens = usage$output_tokens,
    cached_input_tokens = usage$cached_input_tokens,
    total_tokens = usage$total_tokens,
    cost = usage$cost,
    steps = step_metadata,
    calls = execution$calls,
    program_trace_events = step_events,
    module_src = module$module_src,
    source_format = "r",
    runner_policy = execution$runner_policy
  )

  if (isTRUE(trace)) {
    module$state$traces <- append(module$state$traces, step_events)
    for (event in step_events) {
      add_to_global_history(event, source = "FlexModule")
    }
  }

  tibble::tibble(
    output = list(execution$output),
    chat = list(runtime_llm),
    metadata = list(metadata)
  )
}

flex_code_execute <- function(
  module_src,
  inputs,
  outer_signature,
  tools,
  interpreter_factory,
  max_predictor_calls,
  max_tool_calls = 100L,
  require_sandbox,
  config,
  llm,
  cache,
  rollout_id,
  llm_resolver = NULL,
  ...
) {
  wrapper <- flex_code_wrapper()
  output_fields <- names(flex_signature_output_types(outer_signature))

  with_code_runner_lease(
    runner = NULL,
    interpreter_factory = interpreter_factory,
    module_name = "Flex",
    code = function(runner, lease) {
      if (isTRUE(require_sandbox) && !isTRUE(lease$policy$sandboxed)) {
        cli::cli_abort(
          c(
            "Executable Flex requires a runner that advertises an enforced sandbox",
            "i" = "Set {.arg require_sandbox = FALSE} only for source you trust."
          ),
          class = "dsprrr_flex_sandbox_required_error"
        )
      }
      responses <- list()
      call_metadata <- list()
      predictor_calls <- 0L
      tool_calls <- 0L
      resolved_llm <- NULL
      llm_resolved <- FALSE
      frame_limit <- flex_code_frame_limit(lease$policy)

      repeat {
        step_nonce <- rlm_control_nonce()
        result <- execute_code_runner(
          runner,
          wrapper,
          context = list(
            nonce = step_nonce,
            inputs = inputs,
            module_src = module_src,
            frame_limit = frame_limit,
            output_fields = as.list(output_fields),
            tool_names = as.list(names(tools)),
            responses = responses
          ),
          .control_nonce = step_nonce,
          .control_protocol = "flex",
          .control_max_bytes = frame_limit
        )
        if (!isTRUE(result$success)) {
          cli::cli_abort(
            c(
              "Executable Flex execution failed",
              "x" = result$error
            ),
            class = "dsprrr_flex_code_execution_error",
            error_type = result$error_type
          )
        }
        control <- flex_code_decode_control(
          list(result$result, result$stdout, result$stderr, result$error),
          step_nonce
        )
        if (is.null(control)) {
          cli::cli_abort(
            c(
              "Executable Flex stopped without a versioned control result",
              "x" = result$error %||%
                "The interpreter returned no bridge frame."
            ),
            class = "dsprrr_flex_code_execution_error"
          )
        }
        if (identical(control$kind, "final")) {
          output <- control$payload$output
          output <- flex_validate_output_record(
            output,
            flex_signature_output_types(outer_signature),
            context = "Flex outer output",
            class = "dsprrr_flex_output_error",
            scalar = FALSE
          )
          return(list(
            output = output,
            calls = call_metadata,
            predictor_calls = predictor_calls,
            tool_calls = tool_calls,
            llm = resolved_llm,
            runner_policy = lease$policy_summary
          ))
        }

        payload <- control$payload
        if (
          !is.list(payload$request) ||
            !is.character(payload$request_key) ||
            length(payload$request_key) != 1L ||
            is.na(payload$request_key)
        ) {
          cli::cli_abort(
            "Malformed Flex bridge request",
            class = "dsprrr_flex_bridge_protocol_error"
          )
        }
        request <- payload$request
        expected_key <- flex_code_request_key(request)
        if (!identical(payload$request_key, expected_key)) {
          cli::cli_abort(
            "Flex bridge request key does not match its request payload",
            class = "dsprrr_flex_bridge_protocol_error"
          )
        }
        if (identical(request$kind, "predictor")) {
          predictor_calls <- predictor_calls + 1L
          if (
            !is.null(max_predictor_calls) &&
              predictor_calls > max_predictor_calls
          ) {
            cli::cli_abort(
              "Flex exceeded {.arg max_predictor_calls} at runtime",
              class = "dsprrr_flex_budget_error",
              predictor_calls = predictor_calls,
              max_predictor_calls = max_predictor_calls
            )
          }
          if (!llm_resolved) {
            resolved_llm <- if (is.function(llm_resolver)) {
              llm_resolver()
            } else {
              llm
            }
            llm_resolved <- TRUE
          }
        } else if (identical(request$kind, "tool")) {
          tool_calls <- tool_calls + 1L
          if (!is.null(max_tool_calls) && tool_calls > max_tool_calls) {
            cli::cli_abort(
              "Flex exceeded {.arg max_tool_calls} at runtime",
              class = "dsprrr_flex_tool_budget_error",
              tool_calls = tool_calls,
              max_tool_calls = max_tool_calls
            )
          }
        }
        executed <- flex_code_execute_request(
          request = request,
          outer_signature = outer_signature,
          tools = tools,
          interpreter_factory = interpreter_factory,
          config = config,
          llm = resolved_llm,
          cache = cache,
          rollout_id = rollout_id,
          call_metadata = call_metadata,
          ...
        )
        call_metadata[[length(call_metadata) + 1L]] <- executed$metadata
        responses[[length(responses) + 1L]] <- list(
          request_key = payload$request_key,
          success = TRUE,
          value = executed$value,
          error = NULL
        )
      }
    }
  )
}
