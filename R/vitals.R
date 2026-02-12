#' Convert a dsprrr module into a vitals solver
#'
#' @description
#' Creates a function compatible with vitals Tasks that executes a DSPrrr
#' module against batches of inputs. The solver uses [run_dataset()] internally,
#' ensuring that the module's demos, templates, and input descriptions are
#' properly used in prompt construction.
#'
#' For multi-input modules, the solver expects the vitals `input` column to
#' contain nested data (list of tibbles/lists) where each element has fields
#' matching the module's signature inputs. Use [as_vitals_task()] to
#' automatically create this structure from a flat dataset.
#'
#' The solver uses ellmer's parallel processing for efficiency. For structured
#' outputs, mock Chat objects are created for vitals logging compatibility
#' (following the same pattern as vitals' `generate_structured()`).
#'
#' @param module A DSPrrr module (e.g., created via [module()]).
#' @param .llm An ellmer chat object. If `NULL` (default), uses the module's
#'   stored chat or falls back to [get_default_chat()]. The chat is cloned
#'   for each batch invocation.
#' @param ... Additional arguments forwarded to [run_dataset()].
#'
#' @return A function accepting a list of input objects and returning a list
#'   with components `result`, `solver_chat`, and optionally `solver_metadata`.
#' @export
as_vitals_solver <- function(
  module,
  .llm = NULL,
  ...
) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("as_vitals_solver() requires an R6 Module object")
  }

  # Resolve LLM using standard dsprrr pattern: explicit > module > default
  .llm <- .llm %||% module$chat %||% get_default_chat(create = TRUE)

  # Get signature info for extracting inputs from nested vitals format

  sig_inputs <- module$signature@inputs
  sig_input_names <- vapply(sig_inputs, function(x) x$name, character(1))
  output_type <- module$signature@output_type

  # Check if output type produces a scalar string value that can be compared

  # directly by vitals scorers like detect_match(). Types that produce scalar

  # strings don't need JSON serialization; complex types (multi-field objects)
  # are JSON-encoded for scorer compatibility.
  is_scalar_output <- inherits(output_type, "ellmer::TypeEnum") ||
    (inherits(output_type, "ellmer::TypeBasic") &&
      identical(output_type@type, "string"))

  if (!is_scalar_output && inherits(output_type, "ellmer::TypeObject")) {
    props <- output_type@properties
    if (length(props) == 1) {
      prop <- props[[1]]
      is_scalar_output <- inherits(prop, "ellmer::TypeBasic") &&
        identical(prop@type, "string")
    }
  }

  # Capture extra args for run_dataset
  extra_args <- list(...)

  function(inputs, ..., solver_chat = .llm) {
    # Convert vitals nested inputs back to flat data frame for run_dataset
    # Each element in `inputs` is a tibble/list with signature input fields
    rows <- lapply(inputs, function(inp) {
      if (is.data.frame(inp)) {
        as.list(inp[1, sig_input_names, drop = FALSE])
      } else if (is.list(inp)) {
        inp[sig_input_names]
      } else {
        stats::setNames(list(inp), sig_input_names[[1]])
      }
    })

    # Build data frame from rows
    data <- tibble::as_tibble(do.call(rbind, lapply(rows, as.data.frame)))

    # Clone chat for this batch
    ch <- if (is.function(solver_chat)) solver_chat() else solver_chat$clone()

    # Merge extra args, with defaults for parallel processing
    # User-provided args override defaults
    call_args <- list(
      module = module,
      data = data,
      .llm = ch,
      .return_format = "structured",
      .progress = FALSE
    )

    # Add defaults for parallel processing (can be overridden by extra_args)
    defaults <- list(.parallel = TRUE, .parallel_method = "ellmer")
    for (nm in names(defaults)) {
      if (!nm %in% names(extra_args) && !nm %in% names(list(...))) {
        call_args[[nm]] <- defaults[[nm]]
      }
    }

    # Call run_dataset which properly uses demos, templates, and descriptions
    results <- do.call(
      run_dataset,
      c(call_args, extra_args, list(...))
    )

    # Extract results in vitals format
    if (is_scalar_output) {
      # For scalar outputs (strings, enums), extract the value directly
      result_values <- vapply(
        results$result,
        function(r) {
          if (is.list(r) && length(r) == 1) {
            as.character(r[[1]])
          } else {
            as.character(r)
          }
        },
        character(1)
      )

      list(
        result = result_values,
        solver_chat = results$.chat
      )
    } else {
      # For structured outputs, JSON-serialize for scorer compatibility
      result_strings <- vapply(
        results$result,
        function(r) as.character(jsonlite::toJSON(r, auto_unbox = TRUE)),
        character(1)
      )

      list(
        result = result_strings,
        solver_chat = results$.chat,
        solver_metadata = results$.metadata
      )
    }
  }
}

#' Adapt a vitals scorer for use as a dsprrr metric
#'
#' @description
#' Converts a vitals scorer function into a per-example metric compatible with
#' DSPrrr compilation and evaluation. The scorer is invoked on a single-row
#' tibble constructed from the prediction and the expected row.
#'
#' @param vitals_scorer A function that accepts a tibble/data frame and returns
#'   a tibble with a `score` column (following vitals conventions).
#' @param input_column Name of the column to populate with the example input.
#'   If the column is absent in `expected_row`, `NA` is supplied.
#' @param target_column Name of the column holding the ground-truth label inside
#'   the vitals sample tibble.
#' @param result_column Name of the column that receives the model prediction.
#'
#' @return A metric function with signature `function(prediction, expected_row)`
#'   returning numeric values in `[0, 1]` or `NA` when the scorer output cannot
#'   be interpreted.
#' @export
as_dsprrr_metric <- function(
  vitals_scorer,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  if (!is.function(vitals_scorer)) {
    cli::cli_abort("vitals_scorer must be a function")
  }

  function(prediction, expected_row) {
    sample <- tibble::tibble(
      !!input_column := list(
        if (input_column %in% names(expected_row)) {
          expected_row[[input_column]]
        } else {
          NA
        }
      ),
      !!target_column := list(
        if (target_column %in% names(expected_row)) {
          expected_row[[target_column]]
        } else {
          NA
        }
      ),
      !!result_column := list(prediction)
    )

    scores <- vitals_scorer(sample)

    # Handle both list (vitals native) and data.frame (mock) return formats
    if (is.list(scores) && !is.data.frame(scores)) {
      # vitals scorers return a list with $score element
      if (is.null(scores$score)) {
        cli::cli_warn("vitals scorer returned no score; treating as NA")
        return(NA_real_)
      }
      score_val <- scores$score
    } else if (is.data.frame(scores) && nrow(scores) > 0) {
      # Mock scorers return a data.frame with score column
      score_val <- scores$score[[1]]
    } else {
      cli::cli_warn("vitals scorer returned no results; treating as NA")
      return(NA_real_)
    }

    if (is.numeric(score_val)) {
      return(score_val)
    }

    if (is.logical(score_val)) {
      return(as.numeric(score_val))
    }

    # Handle factor (vitals uses ordered factors like "C", "I", "P")
    if (is.factor(score_val)) {
      score_val <- as.character(score_val)
    }

    if (is.character(score_val)) {
      score_lower <- tolower(score_val)
      if (score_lower %in% c("c", "correct", "pass")) {
        return(1)
      }
      if (score_lower %in% c("i", "incorrect", "fail")) {
        return(0)
      }
      if (score_lower %in% c("p", "partial")) {
        return(0.5)
      }
      suppressWarnings(num <- as.numeric(score_val))
      if (!is.na(num)) {
        return(num)
      }
    }

    cli::cli_warn("Unrecognised vitals score value ({score_val}); returning NA")
    NA_real_
  }
}

#' Pre-built Vitals-backed Metrics
#'
#' @description
#' These functions wrap common vitals scorers for direct use as dsprrr metrics,
#' eliminating the need to manually call `as_dsprrr_metric()`.
#'
#' @name vitals_metrics
#' @param template Grading template (glue string with `input`, `answer`,
#'   `criterion`, `instructions` substitutions)
#' @param instructions Grading instructions
#' @param grade_pattern Regex pattern to extract grade from judge response
#' @param partial_credit Whether to allow partial credit
#' @param scorer_chat An ellmer chat for grading (e.g., `ellmer::chat_openai()`)
#' @param input_column Column name for input in vitals sample
#' @param target_column Column name for target in vitals sample
#' @param result_column Column name for result in vitals sample
#'
#' @return A metric function with signature `function(prediction, expected_row)`
#' @export
#'
#' @examples
#' \dontrun{
#' # Model-graded QA metric
#' metric <- metric_model_graded_qa(scorer_chat = ellmer::chat_openai())
#' score <- metric("Paris", data.frame(target = "Paris"))
#'
#' # With custom grading chat
#' metric <- metric_model_graded_fact(
#'   scorer_chat = ellmer::chat_claude(),
#'   partial_credit = TRUE
#' )
#' }
metric_model_graded_qa <- function(
  template = NULL,
  instructions = NULL,

  grade_pattern = "(?i)GRADE\\s*:\\s*([CPI])(.*)$",
  partial_credit = FALSE,
  scorer_chat = NULL,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  rlang::check_installed("vitals", reason = "for model_graded_qa scorer")

  scorer <- vitals::model_graded_qa(
    template = template,
    instructions = instructions,
    grade_pattern = grade_pattern,
    partial_credit = partial_credit,
    scorer_chat = scorer_chat
  )

  as_dsprrr_metric(
    scorer,
    input_column = input_column,
    target_column = target_column,
    result_column = result_column
  )
}

#' @rdname vitals_metrics
#' @export
metric_model_graded_fact <- function(
  template = NULL,
  instructions = NULL,
  grade_pattern = "(?i)GRADE\\s*:\\s*([CPI])(.*)$",
  partial_credit = FALSE,
  scorer_chat = NULL,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  rlang::check_installed("vitals", reason = "for model_graded_fact scorer")

  scorer <- vitals::model_graded_fact(
    template = template,
    instructions = instructions,
    grade_pattern = grade_pattern,
    partial_credit = partial_credit,
    scorer_chat = scorer_chat
  )

  as_dsprrr_metric(
    scorer,
    input_column = input_column,
    target_column = target_column,
    result_column = result_column
  )
}

#' @rdname vitals_metrics
#' @param location Where to look for the target in the result: "end", "begin",
#'   "any", or "exact"
#' @param case_sensitive Whether matching is case-sensitive
#' @export
#' @examples
#' \dontrun{
#' # String detection metrics
#' metric <- metric_detect_match(location = "end")
#' metric("The answer is Paris", data.frame(target = "Paris"))  # 1
#'
#' metric <- metric_detect_includes()
#' metric("Paris is the capital", data.frame(target = "Paris"))  # 1
#' }
metric_detect_match <- function(
  location = c("end", "begin", "any", "exact"),
  case_sensitive = FALSE,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  rlang::check_installed("vitals", reason = "for detect_match scorer")
  location <- match.arg(location)

  scorer <- vitals::detect_match(
    location = location,
    case_sensitive = case_sensitive
  )

  as_dsprrr_metric(
    scorer,
    input_column = input_column,
    target_column = target_column,
    result_column = result_column
  )
}

#' @rdname vitals_metrics
#' @export
metric_detect_includes <- function(
  case_sensitive = FALSE,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  rlang::check_installed("vitals", reason = "for detect_includes scorer")

  scorer <- vitals::detect_includes(case_sensitive = case_sensitive)

  as_dsprrr_metric(
    scorer,
    input_column = input_column,
    target_column = target_column,
    result_column = result_column
  )
}

#' @rdname vitals_metrics
#' @param pattern Regex pattern with capture groups. The captured groups are
#'   extracted from the result and checked against the target. Use parentheses
#'   to define capture groups, e.g., `"([0-9]+)"` to extract numbers.
#' @param all Whether all captured groups must match the target (TRUE) or
#'   just one (FALSE, default).
#' @export
metric_detect_pattern <- function(
  pattern,
  case_sensitive = FALSE,
  all = FALSE,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  rlang::check_installed("vitals", reason = "for detect_pattern scorer")

  scorer <- vitals::detect_pattern(
    pattern = pattern,
    case_sensitive = case_sensitive,
    all = all
  )

  as_dsprrr_metric(
    scorer,
    input_column = input_column,
    target_column = target_column,
    result_column = result_column
  )
}

#' Convert dsprrr cost data to vitals format
#'
#' @description
#' Converts dsprrr cost summaries, trace data, or session costs into a
#' tibble matching the format returned by vitals [vitals::Task]`$get_cost()`.
#' This enables consistent cost reporting across dsprrr and vitals workflows.
#'
#' @param x A dsprrr cost object. Can be:
#'   - A `dsprrr_cost_summary` (from [get_cost()])
#'   - A `dsprrr_session_cost` (from [session_cost()])
#'   - A tibble of traces (from [export_traces()])
#'   - A `dsprrr_evaluation` result (from [evaluate()])
#' @param source Character string identifying the source of costs.
#'   Defaults to `"solver"` to match vitals convention.
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble with columns matching vitals cost format:
#'   - `source`: Character, either "solver" or "scorer"
#'   - `provider`: Character, the API provider name
#'   - `model`: Character, the model name
#'   - `input`: Integer, input token count
#'   - `output`: Integer, output token count
#'   - `price`: Character, formatted cost string (e.g., "$0.01")
#'
#' @export
#' @examples
#' \dontrun{
#' # From session cost
#' as_vitals_cost(session_cost())
#'
#' # From evaluation result
#' eval_result <- evaluate(mod, test_data, metric = metric_exact_match())
#' as_vitals_cost(eval_result)
#'
#' # From module traces
#' traces <- export_traces(my_module)
#' as_vitals_cost(traces)
#' }
as_vitals_cost <- function(x, source = "solver", ...) {
  UseMethod("as_vitals_cost")
}

#' @export
as_vitals_cost.dsprrr_session_cost <- function(x, source = "solver", ...) {
  if (nrow(x$by_model) == 0) {
    return(tibble::tibble(
      source = character(0),
      provider = character(0),
      model = character(0),
      input = integer(0),
      output = integer(0),
      price = character(0)
    ))
  }

  tibble::tibble(
    source = rep(source, nrow(x$by_model)),
    provider = vapply(
      x$by_model$model,
      infer_provider_from_model,
      character(1)
    ),
    model = x$by_model$model,
    input = as.integer(x$by_model$tokens_in),
    output = as.integer(x$by_model$tokens_out),
    price = vapply(
      x$by_model$cost,
      format_price,
      character(1)
    )
  )
}

#' @export
as_vitals_cost.dsprrr_cost_summary <- function(x, source = "solver", ...) {
  # Cost summary doesn't have per-model breakdown, aggregate to single row
  if (nrow(x$costs) == 0 || x$total == 0) {
    return(tibble::tibble(
      source = character(0),
      provider = character(0),
      model = character(0),
      input = integer(0),
      output = integer(0),
      price = character(0)
    ))
  }

  tibble::tibble(
    source = source,
    provider = "unknown",
    model = "unknown",
    input = NA_integer_,
    output = NA_integer_,
    price = format_price(x$total)
  )
}

#' @export
as_vitals_cost.dsprrr_evaluation <- function(x, source = "solver", ...) {
  cost_summary <- get_cost(x)
  as_vitals_cost(cost_summary, source = source, ...)
}

#' @export
as_vitals_cost.data.frame <- function(x, source = "solver", ...) {
  # Assume this is a traces tibble from export_traces()
  required_cols <- c("model", "input_tokens", "output_tokens", "cost")
  if (!all(required_cols %in% names(x))) {
    cli::cli_abort(c(
      "Data frame must have trace columns",
      "i" = "Required: {.field {required_cols}}",
      "x" = "Missing: {.field {setdiff(required_cols, names(x))}}"
    ))
  }

  if (nrow(x) == 0) {
    return(tibble::tibble(
      source = character(0),
      provider = character(0),
      model = character(0),
      input = integer(0),
      output = integer(0),
      price = character(0)
    ))
  }

  # Aggregate by model
  models <- unique(x$model)

  tibble::tibble(
    source = rep(source, length(models)),
    provider = vapply(models, infer_provider_from_model, character(1)),
    model = models,
    input = vapply(
      models,
      function(m) as.integer(sum(x$input_tokens[x$model == m], na.rm = TRUE)),
      integer(1)
    ),
    output = vapply(
      models,
      function(m) as.integer(sum(x$output_tokens[x$model == m], na.rm = TRUE)),
      integer(1)
    ),
    price = vapply(
      models,
      function(m) format_price(sum(x$cost[x$model == m], na.rm = TRUE)),
      character(1)
    )
  )
}

#' @export
as_vitals_cost.default <- function(x, source = "solver", ...) {
  cli::cli_abort(c(
    "Cannot convert {.cls {class(x)[1]}} to vitals cost format",
    "i" = "Expected: dsprrr_session_cost, dsprrr_cost_summary, dsprrr_evaluation, or traces data frame"
  ))
}

#' Format price as string
#' @noRd
format_price <- function(cost) {
  if (is.na(cost) || cost == 0) {
    return("$0.00")
  }
  sprintf("$%.2f", cost)
}

#' Infer provider from model name
#' @noRd
infer_provider_from_model <- function(model) {
  if (is.na(model) || model == "unknown") {
    return("unknown")
  }

  model_lower <- tolower(model)

  if (grepl("^gpt|^o1|^o3|^text-|^davinci|^curie|^babbage|^ada", model_lower)) {
    return("OpenAI")
  }
  if (grepl("^claude", model_lower)) {
    return("Anthropic")
  }
  if (grepl("^gemini|^palm", model_lower)) {
    return("Google")
  }
  if (grepl("^llama|^mistral|^mixtral", model_lower)) {
    return("Meta/Mistral")
  }

  "unknown"
}

#' Create a vitals Task from a dsprrr module
#'
#' @description
#' Convenience function that builds a vitals [vitals::Task] from a dsprrr
#' module and dataset. This makes it trivial to evaluate dsprrr modules
#' using vitals infrastructure without manual solver wrapping.
#'
#' For multi-input modules, the function automatically nests all signature
#' input columns into a single `input` list column that vitals expects.
#' The solver then extracts these fields when processing each sample.
#'
#' @param module A DSPrrr module (e.g., created via [module()]).
#' @param dataset A tibble/data frame with columns matching the module's
#'   signature inputs plus a `target` column. The function will nest
#'   signature inputs into the `input` column format vitals requires.
#' @param scorer A vitals scorer function (e.g., `vitals::model_graded_qa()`,
#'   `vitals::detect_match()`). Defaults to `vitals::model_graded_qa()`.
#' @param .llm Optional ellmer chat object for the solver. When `NULL`,
#'   each invocation will create a fresh default client.
#' @param name Optional name for the task. Defaults to the dataset name.
#' @param epochs Number of times to repeat each sample for statistical
#'   significance. Defaults to 1L.
#' @param metrics Optional named list of metric functions. Each function
#'   takes a vector of scores and returns a single numeric value.
#' @param dir Directory for evaluation logs. Defaults to `vitals::vitals_log_dir()`.
#' @param .parallel Logical; whether to run solver in parallel. Defaults to FALSE.
#' @param ... Additional arguments passed to [as_vitals_solver()].
#'
#' @return A vitals [vitals::Task] object ready for evaluation.
#'
#' @details
#' The returned Task object can be evaluated by calling its `$eval()` method,
#' which runs the solver, scores results, computes metrics, and logs output.
#' Use `$view()` to see results interactively.
#'
#' @export
#' @examples
#' \dontrun{
#' # Single-input module
#' mod <- module(signature("question -> answer"))
#' test_data <- tibble::tibble(
#'   question = c("What is 2+2?", "Capital of France?"),
#'   target = c("4", "Paris")
#' )
#' task <- as_vitals_task(mod, test_data, scorer = vitals::detect_includes())
#'
#' # Multi-input module
#' mod <- module(signature("shapes, pick -> answer"))
#' test_data <- tibble::tibble(
#'   shapes = c("square, circle", "triangle, star"),
#'   pick = c("square", "star"),
#'   target = c("square", "star")
#' )
#' task <- as_vitals_task(mod, test_data, scorer = vitals::detect_includes())
#' }
as_vitals_task <- function(
  module,
  dataset,
  scorer = NULL,
  .llm = NULL,
  name = NULL,
  epochs = 1L,
  metrics = NULL,
  dir = NULL,
  .parallel = FALSE,
  ...
) {
  rlang::check_installed("vitals", reason = "for Task creation")

  if (!inherits(module, "Module")) {
    cli::cli_abort(c(
      "as_vitals_task() requires a dsprrr Module",
      "x" = "Got: {.cls {class(module)[1]}}"
    ))
  }

  if (!is.data.frame(dataset)) {
    cli::cli_abort(c(
      "dataset must be a data frame or tibble",
      "x" = "Got: {.cls {class(dataset)[1]}}"
    ))
  }

  # Get required input columns from module's signature
  sig_input_names <- vapply(
    module$signature@inputs,
    function(x) x$name,
    character(1)
  )

  # Require signature inputs + "target" for scoring
  required_cols <- c(sig_input_names, "target")
  missing_cols <- setdiff(required_cols, names(dataset))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "dataset must have columns matching signature inputs plus 'target'",
      "i" = "Signature inputs: {.field {sig_input_names}}",
      "x" = "Missing: {.field {missing_cols}}"
    ))
  }

  # Nest signature input columns into a single `input` list column for vitals
  # Each element is a tibble with all signature inputs for that row
  input_list <- lapply(seq_len(nrow(dataset)), function(i) {
    dataset[i, sig_input_names, drop = FALSE]
  })

  vitals_dataset <- tibble::tibble(
    input = input_list,
    target = dataset$target
  )

  # Preserve any extra columns (excluding signature inputs and target)
  extra_cols <- setdiff(names(dataset), c(sig_input_names, "target"))
  for (col in extra_cols) {
    vitals_dataset[[col]] <- dataset[[col]]
  }

  # Default scorer
  scorer <- scorer %||% vitals::model_graded_qa()

  # Create solver from module
  solver <- as_vitals_solver(
    module = module,
    .llm = .llm,
    .parallel = .parallel,
    ...
  )

  # Use default name if not provided
  name <- name %||% deparse(substitute(dataset))

  # Use default log directory if not provided
  dir <- dir %||% vitals::vitals_log_dir()

  # Create and return the Task
  tryCatch(
    vitals::Task$new(
      dataset = vitals_dataset,
      solver = solver,
      scorer = scorer,
      metrics = metrics,
      epochs = epochs,
      name = name,
      dir = dir
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to create vitals Task",
          "i" = "Check that your scorer and module are compatible",
          "x" = conditionMessage(e)
        ),
        parent = e
      )
    }
  )
}

#' Convert dsprrr traces to vitals samples format
#'
#' @description
#' Converts dsprrr module traces to a tibble format compatible with vitals
#' `Task$get_samples()` output. This enables viewing dsprrr traces in
#' vitals' Inspect viewer or combining with vitals samples using `vitals_bind()`.
#'
#' @param traces A traces tibble from [export_traces()] or module `$get_traces()`.
#' @param input_column Column name containing the input prompts. If `NULL`,
#'   attempts to extract from the prompt field.
#' @param include_chats Logical; if `TRUE` and solver_chat column exists,
#'   include chat objects. Defaults to `FALSE`.
#'
#' @return A tibble with columns matching vitals samples format:
#'   - `id`: Unique identifier for each trace
#'   - `input`: Input prompt text
#'   - `result`: Model output
#'   - `solver_metadata`: List column with trace metadata (latency, tokens, cost)
#'   - `model`: Model name used
#'   - `epoch`: Always 1 (dsprrr doesn't use epochs)
#'
#' @export
#' @examples
#' \dontrun{
#' # Export traces from a module and convert
#' traces <- export_traces(my_module, include_outputs = TRUE)
#' samples <- as_vitals_samples(traces)
#'
#' # Use with vitals_bind for combined analysis
#' vitals::vitals_bind(
#'   task1 = task1,
#'   dsprrr = samples
#' )
#' }
as_vitals_samples <- function(
  traces,
  input_column = NULL,
  include_chats = FALSE
) {
  if (!is.data.frame(traces)) {
    cli::cli_abort(c(
      "traces must be a data frame",
      "x" = "Got: {.cls {class(traces)[1]}}"
    ))
  }

  if (nrow(traces) == 0) {
    return(tibble::tibble(
      id = character(0),
      input = character(0),
      result = list(),
      solver_metadata = list(),
      model = character(0),
      epoch = integer(0)
    ))
  }

  # Generate IDs
  n <- nrow(traces)
  ids <- sprintf("trace_%04d", seq_len(n))

  # Extract input from prompt field if not specified
  inputs <- if (!is.null(input_column) && input_column %in% names(traces)) {
    traces[[input_column]]
  } else if ("prompt" %in% names(traces)) {
    traces$prompt
  } else {
    rep(NA_character_, n)
  }

  # Extract results from output field if present
  results <- if ("output" %in% names(traces)) {
    as.list(traces$output)
  } else {
    replicate(n, NULL, simplify = FALSE)
  }

  # Build solver_metadata from trace fields
  solver_metadata <- lapply(seq_len(n), function(i) {
    meta <- list()
    if ("latency_ms" %in% names(traces)) {
      meta$latency_ms <- traces$latency_ms[i]
    }
    if ("input_tokens" %in% names(traces)) {
      meta$input_tokens <- traces$input_tokens[i]
    }
    if ("output_tokens" %in% names(traces)) {
      meta$output_tokens <- traces$output_tokens[i]
    }
    if ("total_tokens" %in% names(traces)) {
      meta$total_tokens <- traces$total_tokens[i]
    }
    if ("cost" %in% names(traces)) {
      meta$cost <- traces$cost[i]
    }
    if ("timestamp" %in% names(traces)) {
      meta$timestamp <- traces$timestamp[i]
    }
    meta
  })

  # Build result tibble
  result <- tibble::tibble(
    id = ids,
    input = inputs,
    result = results,
    solver_metadata = solver_metadata,
    model = if ("model" %in% names(traces)) traces$model else NA_character_,
    epoch = rep(1L, n)
  )

  # Optionally include chat objects if present
  if (include_chats && "solver_chat" %in% names(traces)) {
    result$solver_chat <- traces$solver_chat
  }

  result
}

#' Convert vitals samples to dsprrr traces format
#'
#' @description
#' Converts vitals samples tibble (from `Task$get_samples()` or `vitals_bind()`)
#' to dsprrr traces format for use with dsprrr analysis functions like
#' [summarize_traces()].
#'
#' @param samples A tibble from `Task$get_samples()` or `vitals_bind()`.
#' @param include_prompts Logical; whether to extract prompts from input column.
#'   Defaults to `TRUE`.
#' @param include_outputs Logical; whether to extract outputs from result column.
#'   Defaults to `TRUE`.
#'
#' @return A tibble with dsprrr trace columns:
#'   - `timestamp`: Extracted from metadata or set to current time
#'   - `latency_ms`: Extracted from metadata or NA
#'   - `input_tokens`: Extracted from metadata or NA
#'   - `output_tokens`: Extracted from metadata or NA
#'   - `total_tokens`: Calculated or extracted from metadata
#'   - `cost`: Extracted from metadata or NA
#'   - `model`: Model name if available
#'   - `prompt_length`: Character length of prompt
#'   - `prompt`: Input text (if include_prompts = TRUE)
#'   - `output`: Result (if include_outputs = TRUE)
#'
#' @export
#' @examples
#' \dontrun{
#' # Get samples from a vitals task
#' samples <- task$get_samples()
#'
#' # Convert to dsprrr traces format
#' traces <- as_dsprrr_traces(samples)
#'
#' # Use dsprrr analysis functions
#' summary <- summarize_traces_df(traces)
#' }
as_dsprrr_traces <- function(
  samples,
  include_prompts = TRUE,
  include_outputs = TRUE
) {
  if (!is.data.frame(samples)) {
    cli::cli_abort(c(
      "samples must be a data frame",
      "x" = "Got: {.cls {class(samples)[1]}}"
    ))
  }

  if (nrow(samples) == 0) {
    result <- tibble::tibble(
      timestamp = as.POSIXct(character(0)),
      latency_ms = numeric(0),
      input_tokens = integer(0),
      output_tokens = integer(0),
      total_tokens = integer(0),
      cost = numeric(0),
      model = character(0),
      prompt_length = integer(0)
    )
    if (include_prompts) {
      result$prompt <- character(0)
    }
    if (include_outputs) {
      result$output <- list()
    }
    return(result)
  }

  n <- nrow(samples)

  # Extract metadata from solver_metadata or metadata column
  metadata_col <- if ("solver_metadata" %in% names(samples)) {
    samples$solver_metadata
  } else if ("metadata" %in% names(samples)) {
    samples$metadata
  } else {
    replicate(n, list(), simplify = FALSE)
  }

  # Extract values from metadata
  extract_meta <- function(field, default = NA) {
    vapply(
      metadata_col,
      function(m) {
        if (is.list(m) && field %in% names(m)) {
          m[[field]]
        } else {
          default
        }
      },
      FUN.VALUE = default
    )
  }

  # Build base trace tibble
  result <- tibble::tibble(
    timestamp = as.POSIXct(
      extract_meta("timestamp", NA_real_),
      origin = "1970-01-01"
    ),
    latency_ms = as.numeric(extract_meta("latency_ms", NA_real_)),
    input_tokens = as.integer(extract_meta("input_tokens", NA_integer_)),
    output_tokens = as.integer(extract_meta("output_tokens", NA_integer_)),
    total_tokens = NA_integer_,
    cost = as.numeric(extract_meta("cost", NA_real_)),
    model = if ("model" %in% names(samples)) {
      samples$model
    } else {
      NA_character_
    },
    prompt_length = NA_integer_
  )

  # Calculate total_tokens
  result$total_tokens <- ifelse(
    !is.na(result$input_tokens) & !is.na(result$output_tokens),
    result$input_tokens + result$output_tokens,
    as.integer(extract_meta("total_tokens", NA_integer_))
  )

  # Replace NA timestamps with current time
  # Use vectorized replacement instead of ifelse() to preserve POSIXct class
  na_timestamps <- is.na(result$timestamp)
  if (any(na_timestamps)) {
    result$timestamp[na_timestamps] <- Sys.time()
  }

  # Add prompts if requested
  if (include_prompts && "input" %in% names(samples)) {
    result$prompt <- as.character(samples$input)
    result$prompt_length <- nchar(result$prompt)
  }

  # Add outputs if requested
  if (include_outputs && "result" %in% names(samples)) {
    result$output <- as.list(samples$result)
  }

  result
}

#' Summarize a traces data frame
#'
#' @description
#' Provides summary statistics for a traces data frame. This is a standalone
#' version of [summarize_traces()] that works on a data frame rather than
#' requiring a Module object. Useful for analyzing converted vitals samples.
#'
#' @param traces A traces tibble (from [export_traces()], [as_dsprrr_traces()],
#'   or module `$get_traces()`).
#'
#' @return A list with:
#'   - `n_traces`: Number of traces
#'   - `total_tokens`: Total tokens used across all traces
#'   - `total_input_tokens`: Total input tokens
#'   - `total_output_tokens`: Total output tokens
#'   - `total_cost`: Total cost in USD
#'   - `total_latency_ms`: Sum of latencies
#'   - `avg_latency_ms`: Average latency per request
#'   - `avg_tokens_per_request`: Average tokens per request
#'   - `model_usage`: Data frame with per-model breakdown
#'
#' @export
#' @examples
#' \dontrun{
#' # Analyze traces from vitals samples
#' traces <- as_dsprrr_traces(task$get_samples())
#' summary <- summarize_traces_df(traces)
#' print(summary)
#' }
summarize_traces_df <- function(traces) {
  if (!is.data.frame(traces)) {
    cli::cli_abort("traces must be a data frame")
  }

  n <- nrow(traces)
  if (n == 0) {
    return(structure(
      list(
        n_traces = 0L,
        total_tokens = 0L,
        total_input_tokens = 0L,
        total_output_tokens = 0L,
        total_cost = 0,
        total_latency_ms = 0,
        avg_latency_ms = NA_real_,
        avg_tokens_per_request = NA_real_,
        model_usage = data.frame(model = character(0), n_requests = integer(0))
      ),
      class = "dsprrr_trace_summary"
    ))
  }

  total_input <- sum(traces$input_tokens, na.rm = TRUE)
  total_output <- sum(traces$output_tokens, na.rm = TRUE)
  total_tokens <- sum(traces$total_tokens, na.rm = TRUE)
  if (total_tokens == 0) {
    total_tokens <- total_input + total_output
  }

  total_latency <- sum(traces$latency_ms, na.rm = TRUE)
  total_cost <- sum(traces$cost, na.rm = TRUE)

  # Model usage breakdown
  if ("model" %in% names(traces)) {
    model_counts <- table(traces$model)
    model_usage <- data.frame(
      model = names(model_counts),
      n_requests = as.integer(model_counts)
    )
  } else {
    model_usage <- data.frame(model = character(0), n_requests = integer(0))
  }

  structure(
    list(
      n_traces = n,
      total_tokens = total_tokens,
      total_input_tokens = total_input,
      total_output_tokens = total_output,
      total_cost = total_cost,
      total_latency_ms = total_latency,
      avg_latency_ms = total_latency / n,
      avg_tokens_per_request = total_tokens / n,
      token_breakdown = list(
        input = total_input,
        output = total_output,
        ratio = if (total_input > 0) {
          round(total_output / total_input, 2)
        } else {
          NA
        }
      ),
      model_usage = model_usage
    ),
    class = "dsprrr_trace_summary"
  )
}
