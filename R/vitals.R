#' Use a dsprrr module as a vitals solver
#'
#' @description
#' `as_vitals_solver()` wraps a module in a function that a vitals `Task` can
#' call as its solver. The solver runs the module with [run_dataset()], so the
#' module's demonstrations, template and input descriptions are used as usual.
#' [as_vitals_task()] builds the whole task in one step.
#'
#' @details
#' vitals passes the solver a list of inputs. For a module with several
#' inputs, each element must be a one-row data frame or list with fields named
#' after the signature inputs; [as_vitals_task()] creates that structure from
#' a flat data set.
#'
#' String and enum outputs, including a single string or enum field, are
#' returned as plain text, which string scorers such as
#' `vitals::detect_match()` can compare. Other outputs are returned as JSON,
#' together with per-row metadata. Rows run one after another unless
#' `.concurrency` asks for more.
#'
#' @param module A module, such as one created with [module()].
#' @param .llm An ellmer Chat. `NULL` (the default) uses the module's own chat
#'   or the default chat from [get_default_chat()], resolved when the solver is
#'   created. Each batch runs on a fresh clone.
#' @param .concurrency Optional policy from [concurrency_control()].
#' @param ... Further arguments passed to [run_dataset()].
#'
#' @return A function `function(inputs, ..., solver_chat)` that returns a list
#'   with `result` (character vector), `solver_chat` (the chats used) and, for
#'   non-text outputs, `solver_metadata`.
#' @family integrations
#' @examplesIf rlang::is_installed("vitals")
#' solver <- as_vitals_solver(
#'   module(signature("question -> answer")),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#'
#' \dontrun{
#' tsk <- vitals::Task$new(
#'   dataset = tibble::tibble(input = "What is 2 + 2?", target = "4"),
#'   solver = solver,
#'   scorer = vitals::detect_includes()
#' )
#' tsk$eval()
#' }
#' @export
as_vitals_solver <- function(
  module,
  .llm = NULL,
  .concurrency = NULL,
  ...
) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("as_vitals_solver() requires an R6 Module object")
  }

  # Resolve and validate the canonical Chat once when creating the solver.
  .llm <- resolve_module_llm(module, .llm = .llm)

  # Get signature info for extracting inputs from nested vitals format

  sig_inputs <- module$signature@inputs
  sig_input_names <- vapply(sig_inputs, function(x) x$name, character(1))
  output_type <- module$signature@output_type

  # Check if output type produces a scalar string value that can be compared
  # directly by vitals scorers like detect_match(). Types that produce scalar
  # strings don't need JSON serialization; complex types (multi-field objects)
  # are JSON-encoded for scorer compatibility.
  is_scalar_string_type <- function(type) {
    inherits(type, "ellmer::TypeEnum") ||
      (inherits(type, "ellmer::TypeBasic") &&
        identical(type@type, "string"))
  }
  is_scalar_output <- is_scalar_string_type(output_type)

  if (!is_scalar_output && inherits(output_type, "ellmer::TypeObject")) {
    props <- output_type@properties
    if (length(props) == 1) {
      # Signatures like "text -> sentiment: enum(...)" compile to a single
      # object field; unwrap it so detect_match() compares plain values.
      is_scalar_output <- is_scalar_string_type(props[[1]])
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

    # Each batch gets an independent, history-free Chat.
    ch <- clone_ellmer_chat(
      solver_chat,
      arg = "solver_chat",
      reset_turns = TRUE
    )

    call_args <- list(
      module = module,
      data = data,
      .llm = ch,
      .concurrency = .concurrency,
      .return_format = "structured",
      .progress = FALSE
    )

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

#' Use a vitals scorer as a dsprrr metric
#'
#' @description
#' `as_dsprrr_metric()` turns a vitals scorer into a per-example metric that
#' [evaluate()], [optimize_grid()] and the optimizers can call. Each call
#' builds a one-row vitals sample from the prediction and the data row, runs
#' the scorer, and converts its grade to a number.
#'
#' @details
#' The sample has three columns. The `input_column` and `target_column`
#' columns are copied from the data column of the same name (`NA` when the row
#' has no such column), and `result_column` holds the prediction. vitals'
#' built-in scorers read the columns `input`, `target` and `result`, so keep
#' the defaults with them and give your data a `target` column (and an `input`
#' column for model-graded scorers). The other names are for scorers of your
#' own that read different columns.
#'
#' Grades are converted as follows: numbers are kept, `TRUE`/`FALSE` become
#' 1/0, and `"C"`/`"correct"`/`"pass"`, `"I"`/`"incorrect"`/`"fail"` and
#' `"P"`/`"partial"` become 1, 0 and 0.5. Anything else gives `NA` with a
#' warning.
#'
#' @param vitals_scorer A scorer function that takes a samples tibble and
#'   returns a list (or data frame) with a `score` element, such as
#'   `vitals::detect_includes()`.
#' @param input_column Name of the input column, in both the data and the
#'   sample (default `"input"`).
#' @param target_column Name of the expected-answer column, in both the data
#'   and the sample (default `"target"`).
#' @param result_column Name of the sample column that receives the prediction
#'   (default `"result"`).
#'
#' @return A metric function `function(prediction, expected_row)` that returns
#'   a number, usually in `[0, 1]`, or `NA`.
#' @family integrations
#' @family metrics
#' @examples
#' # A scorer written in the vitals style
#' exact_match <- function(samples) {
#'   list(score = as.numeric(samples$result[[1]] == samples$target[[1]]))
#' }
#' metric <- as_dsprrr_metric(exact_match)
#' metric("yes", data.frame(input = "Continue?", target = "yes"))
#'
#' @examplesIf rlang::is_installed("vitals")
#' # A vitals scorer; the data needs a `target` column
#' includes <- as_dsprrr_metric(vitals::detect_includes())
#' includes("The capital is Paris.", data.frame(target = "Paris"))
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

#' Metrics built on vitals scorers
#'
#' @description
#' These functions wrap vitals scorers with [as_dsprrr_metric()] so they can
#' be used directly as dsprrr metrics:
#'
#' * `metric_model_graded_qa()` and `metric_model_graded_fact()` ask a model to
#'   grade the answer against the target.
#' * `metric_detect_match()`, `metric_detect_includes()` and
#'   `metric_detect_pattern()` compare strings without a model.
#'
#' @details
#' The underlying vitals scorers read the expected answer from a `target`
#' column, and the graded metrics also show the grader the question from an
#' `input` column. Add those columns to your data, for example
#' `transform(data, input = question, target = answer)`, and keep the default
#' column arguments; see [as_dsprrr_metric()].
#'
#' The graded metrics need `scorer_chat`: outside a vitals `Task`, there is no
#' solver chat for vitals to fall back on.
#'
#' @name vitals_metrics
#' @param template Grading prompt template, a glue string with `input`,
#'   `answer`, `criterion` and `instructions` fields. `NULL` uses the vitals
#'   default.
#' @param instructions Grading instructions. `NULL` uses the vitals default.
#' @param grade_pattern Regular expression that extracts the grade from the
#'   grader's reply.
#' @param partial_credit Whether the grader may award partial credit (0.5).
#' @param scorer_chat The ellmer Chat that grades, such as
#'   `ellmer::chat_openai(model = "gpt-6-luna")`. Required; see Details.
#' @param input_column,target_column,result_column Column names passed to
#'   [as_dsprrr_metric()]. Keep the defaults for vitals scorers.
#'
#' @return A metric function `function(prediction, expected_row)`.
#' @family metrics
#' @family integrations
#' @export
#'
#' @examplesIf rlang::is_installed("vitals")
#' # String comparisons need no model
#' ends_with_answer <- metric_detect_match(location = "end")
#' ends_with_answer("The answer is Paris", data.frame(target = "Paris"))
#'
#' mentions <- metric_detect_includes()
#' mentions("Paris is the capital", data.frame(target = "Paris"))
#'
#' number <- metric_detect_pattern("([0-9]+)")
#' number("The total is 42", data.frame(target = "42"))
#'
#' \dontrun{
#' # Model-graded metrics call scorer_chat once per example
#' graded <- metric_model_graded_qa(
#'   scorer_chat = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' graded(
#'   "Paris",
#'   data.frame(input = "What is the capital of France?", target = "Paris")
#' )
#'
#' fact <- metric_model_graded_fact(
#'   scorer_chat = ellmer::chat_anthropic(model = "claude-sonnet-4-5"),
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
#' @param location Where to look for the target in the result: `"end"` (the
#'   default), `"begin"`, `"any"` or `"exact"`.
#' @param case_sensitive Whether matching is case-sensitive (default `FALSE`).
#' @export
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
#' @param pattern Regular expression with capture groups, such as
#'   `"([0-9]+)"`. The captured text is compared with the target.
#' @param all Whether every captured group must match the target (`TRUE`) or
#'   at least one (`FALSE`, the default).
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

#' Report dsprrr costs in the vitals format
#'
#' @description
#' `as_vitals_cost()` converts dsprrr cost information into the tibble that a
#' vitals `Task`'s `$get_cost()` method returns, so costs from both packages
#' can be reported together.
#'
#' @param x One of:
#'   * a session summary from [session_cost()] (one row per model);
#'   * a traces data frame, such as [export_traces()] output, with `model`,
#'     `input_tokens`, `output_tokens` and `cost` columns (one row per model);
#'   * a cost summary from [get_cost()] or an [evaluate()] result (a single
#'     row with the total price, and model and token counts unknown). This
#'     currently fails when the total cost is unknown, for example for a
#'     model without price data.
#' @param source Label for the `source` column (default `"solver"`, the
#'   vitals convention; vitals also uses `"scorer"`).
#' @param ... Unused.
#'
#' @return A tibble with columns `source`, `provider` (guessed from the model
#'   name), `model`, `input` and `output` (token counts), and `price` (text
#'   such as `"$0.01"`).
#'
#' @family integrations
#' @export
#' @examples
#' traces <- data.frame(
#'   model = c("gpt-6-luna", "gpt-6-luna", "claude-sonnet-4-5"),
#'   input_tokens = c(12000L, 8000L, 20000L),
#'   output_tokens = c(3000L, 2500L, 6000L),
#'   cost = c(0.21, 0.15, 0.42)
#' )
#' as_vitals_cost(traces)
#'
#' # Nothing has run in this session yet
#' as_vitals_cost(session_cost())
#'
#' \dontrun{
#' result <- evaluate(
#'   classifier,
#'   testset,
#'   metric = metric_exact_match(field = "sentiment"),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' as_vitals_cost(result)
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

#' Build a vitals Task from a module and a data set
#'
#' @description
#' `as_vitals_task()` creates a vitals `Task` that uses the module as its
#' solver (see [as_vitals_solver()]), so you can evaluate the module with
#' vitals' scoring, logging and viewer. Call the task's `$eval()` method to
#' run it and `$view()` to browse the results.
#'
#' @details
#' `dataset` needs one column per signature input and a `target` column. The
#' input columns are nested into the `input` list-column that vitals expects;
#' other columns are kept.
#'
#' @param module A module, such as one created with [module()].
#' @param dataset A data frame with the signature's input columns and a
#'   `target` column.
#' @param scorer A vitals scorer, such as `vitals::detect_includes()`. The
#'   default, `vitals::model_graded_qa()`, asks the solver's chat to grade.
#' @param .llm An ellmer Chat for the solver. `NULL` (the default) uses the
#'   module's own chat or the default chat, resolved when the task is created.
#' @param name Task name. Defaults to the expression passed as `dataset`.
#' @param epochs Integer number of times each sample is run (default `1L`).
#' @param metrics Optional named list of functions that summarize a vector of
#'   scores into one number.
#' @param dir Directory for the evaluation logs. Defaults to
#'   `vitals::vitals_log_dir()`.
#' @param .concurrency Optional policy from [concurrency_control()].
#' @param ... Further arguments passed to [as_vitals_solver()].
#'
#' @return A vitals `Task` object.
#' @family integrations
#' @export
#' @examplesIf rlang::is_installed("vitals")
#' qa <- module(signature("question -> answer"))
#' test_data <- data.frame(
#'   question = c("What is 2+2?", "Capital of France?"),
#'   target = c("4", "Paris")
#' )
#' tsk <- as_vitals_task(
#'   qa,
#'   test_data,
#'   scorer = vitals::detect_includes(),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   dir = tempdir()
#' )
#' tsk
#'
#' \dontrun{
#' tsk$eval()
#' tsk$get_cost()
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
  .concurrency = NULL,
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
    .concurrency = .concurrency,
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

#' Convert dsprrr traces to vitals samples
#'
#' @description
#' `as_vitals_samples()` reshapes a traces data frame into the samples format
#' that a vitals `Task`'s `$get_samples()` returns, for example to combine
#' dsprrr runs with vitals results using `vitals::vitals_bind()`.
#' [as_dsprrr_traces()] converts the other way.
#'
#' @param traces A traces data frame, such as [export_traces()] output. Export
#'   with `include_prompts = TRUE` and `include_outputs = TRUE`; otherwise
#'   `input` and `result` are empty.
#' @param input_column Name of the column to use as `input`. `NULL` (the
#'   default) uses `prompt`.
#' @param include_chats Whether to keep a `solver_chat` column when `traces`
#'   has one (default `FALSE`).
#'
#' @return A tibble with columns `id` (`"trace_0001"`, ...), `input`,
#'   `result` (list-column from the `output` column), `solver_metadata`
#'   (list-column with latency, tokens, cost and timestamp), `model` and
#'   `epoch` (always 1).
#'
#' @family integrations
#' @export
#' @examples
#' traces <- tibble::tibble(
#'   prompt = c("question: What is 2+2?", "question: Capital of France?"),
#'   output = list(list(answer = "4"), list(answer = "Paris")),
#'   model = "gpt-6-luna",
#'   latency_ms = c(820, 640),
#'   input_tokens = c(52L, 55L),
#'   output_tokens = c(6L, 7L),
#'   cost = c(0.0001, 0.0001)
#' )
#' as_vitals_samples(traces)
#'
#' \dontrun{
#' samples <- as_vitals_samples(
#'   export_traces(classifier, include_prompts = TRUE, include_outputs = TRUE)
#' )
#' vitals::vitals_bind(dsprrr = samples)
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

#' Convert vitals samples to dsprrr traces
#'
#' @description
#' `as_dsprrr_traces()` reshapes vitals samples (from a `Task`'s
#' `$get_samples()` or from [as_vitals_samples()]) into the traces format, so
#' that [summarize_traces_df()] and other trace tools can analyze them.
#'
#' @details
#' Latency, token counts, cost and timestamp are read from the
#' `solver_metadata` (or `metadata`) list-column; missing values are `NA` and
#' missing timestamps are set to the current time.
#'
#' @param samples A samples tibble.
#' @param include_prompts Whether to add a `prompt` column from `input`
#'   (default `TRUE`).
#' @param include_outputs Whether to add an `output` list-column from `result`
#'   (default `TRUE`).
#'
#' @return A tibble with columns `timestamp`, `latency_ms`, `input_tokens`,
#'   `output_tokens`, `total_tokens`, `cost`, `model` and `prompt_length`,
#'   plus `prompt` and `output` when requested.
#'
#' @family integrations
#' @export
#' @examples
#' samples <- as_vitals_samples(tibble::tibble(
#'   prompt = "question: What is 2+2?",
#'   output = list(list(answer = "4")),
#'   model = "gpt-6-luna",
#'   input_tokens = 52L,
#'   output_tokens = 6L,
#'   cost = 0.0001
#' ))
#' as_dsprrr_traces(samples)
#'
#' \dontrun{
#' traces <- as_dsprrr_traces(tsk$get_samples())
#' summarize_traces_df(traces)
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
#' `summarize_traces_df()` totals tokens, cost and latency over a traces data
#' frame. It is the data frame version of [summarize_traces()], which takes a
#' module; use it for traces exported with [export_traces()] or converted from
#' vitals with [as_dsprrr_traces()].
#'
#' @param traces A traces data frame with `input_tokens`, `output_tokens`,
#'   `total_tokens`, `latency_ms`, `cost` and, optionally, `model` columns.
#'
#' @return A `dsprrr_trace_summary` list with `n_traces`, `total_tokens`,
#'   `total_input_tokens`, `total_output_tokens`, `total_cost`,
#'   `total_latency_ms`, `avg_latency_ms`, `avg_tokens_per_request`,
#'   `token_breakdown` (input, output and their ratio) and `model_usage` (a
#'   data frame of requests per model).
#'
#' @family integrations
#' @export
#' @examples
#' traces <- data.frame(
#'   model = c("gpt-6-luna", "gpt-6-luna"),
#'   input_tokens = c(52L, 55L),
#'   output_tokens = c(6L, 7L),
#'   total_tokens = c(58L, 62L),
#'   latency_ms = c(820, 640),
#'   cost = c(0.0001, 0.0001)
#' )
#' summary <- summarize_traces_df(traces)
#' summary$total_tokens
#' summary$model_usage
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
