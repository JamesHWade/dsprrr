#' parsnip integration
#'
#' Registers an `llm_predict` model with a "dsprrr" engine; see
#' [llm_predict()].
#'
#' @name parsnip-integration
#' @noRd
NULL

#' LLM model specification for parsnip
#'
#' @description
#' `llm_predict()` creates a parsnip model specification whose "dsprrr"
#' engine predicts with a dsprrr module, for classification or regression on
#' text columns. `temperature` and `top_p` can be marked for tuning with
#' `tune()`; see [temperature()] and [top_p()].
#'
#' @details
#' The engine is registered automatically when parsnip is loaded (see
#' [register_dsprrr_engine()]). Fitting builds a Predict module from the
#' predictor columns and the outcome: an enum output with the outcome's levels
#' for classification, or a number for regression, unless you give a
#' `signature`. Prediction runs the module on the new data with the default
#' chat (see [dsp_configure()]); a fitted model makes no model calls until
#' it predicts.
#'
#' Known issue: the engine is registered without parsnip's encoding
#' information, so `parsnip::fit()` and `parsnip::fit_xy()` currently fail
#' with "no applicable method for 'filter'". Until that is fixed, the
#' specification cannot be fitted through parsnip, workflows or tune.
#'
#' @param mode `"classification"` (the default) or `"regression"`.
#' @param signature Optional signature string or [signature()] object for the
#'   module. `NULL` derives one from the data when fitting.
#' @param temperature Sampling temperature for the module, or `tune()`.
#' @param top_p Nucleus sampling parameter for the module, or `tune()`.
#'
#' @return A parsnip model specification of class `llm_predict`.
#'
#' @export
#' @family integrations
#' @examplesIf rlang::is_installed("parsnip")
#' spec <- llm_predict(
#'   mode = "classification",
#'   signature = "text -> sentiment: enum('positive', 'negative', 'neutral')"
#' ) |>
#'   parsnip::set_engine("dsprrr")
#' spec
#'
#' # Mark a parameter for tuning
#' llm_predict(signature = "text -> sentiment", temperature = parsnip::tune())
llm_predict <- function(
  mode = "classification",
  signature = NULL,
  temperature = NULL,
  top_p = NULL
) {
  # Check for parsnip
  rlang::check_installed("parsnip", reason = "for llm_predict()")

  args <- list(
    signature = rlang::enquo(signature),
    temperature = rlang::enquo(temperature),
    top_p = rlang::enquo(top_p)
  )

  parsnip::new_model_spec(
    "llm_predict",
    args = args,
    eng_args = NULL,
    mode = mode,
    user_specified_mode = !missing(mode),
    method = NULL,
    engine = NULL,
    user_specified_engine = FALSE
  )
}

#' @export
print.llm_predict <- function(x, ...) {
  cli::cli_h3("LLM Predict Model Specification")
  cli::cli_text("{.field Mode}: {x$mode}")

  if (!is.null(x$engine)) {
    cli::cli_text("{.field Engine}: {x$engine}")
  }

  # Show tunable parameters
  tuned <- vapply(
    x$args,
    function(a) {
      expr <- rlang::quo_get_expr(a)
      inherits(expr, "call") && identical(expr[[1]], quote(tune))
    },
    logical(1)
  )

  if (any(tuned)) {
    cli::cli_text("{.field Tuned}: {names(x$args)[tuned]}")
  }

  invisible(x)
}

#' Register the dsprrr engine with parsnip
#'
#' @description
#' `register_dsprrr_engine()` registers the `llm_predict` model and its
#' "dsprrr" engine with parsnip. dsprrr calls it automatically when parsnip
#' is loaded, so you rarely need it. It does nothing when parsnip is not
#' installed or the model is already registered.
#'
#' @return `NULL`, invisibly.
#'
#' @export
#' @family integrations
#' @examplesIf rlang::is_installed("parsnip")
#' register_dsprrr_engine()
#' parsnip::show_engines("llm_predict")
register_dsprrr_engine <- function() {
  if (!rlang::is_installed("parsnip")) {
    return(invisible(NULL))
  }

  # Check if already registered
  if ("llm_predict" %in% parsnip::get_model_env()$models) {
    return(invisible(NULL))
  }

  # Register model type
  parsnip::set_new_model("llm_predict")
  parsnip::set_model_mode("llm_predict", "classification")
  parsnip::set_model_mode("llm_predict", "regression")

  # Register dsprrr engine
  parsnip::set_model_engine(
    model = "llm_predict",
    mode = "classification",
    eng = "dsprrr"
  )

  parsnip::set_model_engine(
    model = "llm_predict",
    mode = "regression",
    eng = "dsprrr"
  )

  # Register arguments
  parsnip::set_model_arg(
    model = "llm_predict",
    eng = "dsprrr",
    parsnip = "signature",
    original = "signature",
    func = list(pkg = "dsprrr", fun = "signature"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "llm_predict",
    eng = "dsprrr",
    parsnip = "temperature",
    original = "temperature",
    func = list(pkg = "dials", fun = "temperature"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "llm_predict",
    eng = "dsprrr",
    parsnip = "top_p",
    original = "top_p",
    func = list(pkg = "dials", fun = "top_p"),
    has_submodel = FALSE
  )

  # Register fit function
  parsnip::set_fit(
    model = "llm_predict",
    eng = "dsprrr",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "dsprrr", fun = "fit_llm_predict"),
      defaults = list()
    )
  )

  parsnip::set_fit(
    model = "llm_predict",
    eng = "dsprrr",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "dsprrr", fun = "fit_llm_predict"),
      defaults = list()
    )
  )

  # Register predict function
  parsnip::set_pred(
    model = "llm_predict",
    eng = "dsprrr",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "dsprrr", fun = "predict_llm_class"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data)
      )
    )
  )

  parsnip::set_pred(
    model = "llm_predict",
    eng = "dsprrr",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "dsprrr", fun = "predict_llm_numeric"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data)
      )
    )
  )

  invisible(NULL)
}

#' Engine functions behind llm_predict()
#'
#' @description
#' These functions implement the "dsprrr" engine of [llm_predict()]. parsnip
#' calls them; they are exported for that purpose, not for direct use.
#'
#' * `fit_llm_predict()` builds a Predict module from the predictors `x` and
#'   outcome `y`. Without a `signature`, the inputs are the columns of `x` and
#'   the output is an enum of the outcome's levels (factor or character `y`)
#'   or a number.
#' * `predict_llm_class()` runs the module on `new_data` with the default chat
#'   and returns a tibble with a `.pred_class` factor column.
#' * `predict_llm_numeric()` does the same and returns a `.pred` column;
#'   outputs that cannot be converted to numbers become `NA` with a warning.
#'
#' @param x Data frame of predictors.
#' @param y Outcome vector.
#' @param signature Optional signature string or [signature()] object.
#' @param temperature,top_p Optional sampling settings stored in the module's
#'   config.
#' @param object A module returned by `fit_llm_predict()`.
#' @param new_data Data frame with the predictor columns.
#' @param ... Unused.
#'
#' @return `fit_llm_predict()` returns a Predict module. The predict functions
#'   return a tibble with one row per row of `new_data`.
#' @keywords internal
#' @examples
#' fit_llm_predict(
#'   data.frame(text = c("helpful", "unhelpful")),
#'   factor(c("positive", "negative"))
#' )
#'
#' \dontrun{
#' set_default_chat(ellmer::chat_openai(model = "gpt-6-luna"))
#' fit <- fit_llm_predict(
#'   data.frame(text = c("helpful", "unhelpful")),
#'   factor(c("positive", "negative"))
#' )
#' predict_llm_class(fit, data.frame(text = "clear and useful"))
#' }
#' @export
fit_llm_predict <- function(
  x,
  y,
  signature = NULL,
  temperature = NULL,
  top_p = NULL
) {
  # Auto-generate signature if not provided
  if (is.null(signature)) {
    input_names <- names(x)
    if (length(input_names) == 0) {
      input_names <- "input"
    }

    output_type <- if (is.factor(y) || is.character(y)) {
      levels <- unique(as.character(y))
      paste0("output: enum('", paste(levels, collapse = "', '"), "')")
    } else {
      "output: number"
    }

    signature <- paste(
      paste(input_names, collapse = ", "),
      "->",
      output_type
    )
  }

  if (is.character(signature) && length(signature) == 1L) {
    signature <- parse_signature(signature)
  }
  if (!inherits(signature, "dsprrr::Signature")) {
    cli::cli_abort(
      "{.arg signature} must be one signature string or the result of {.fn signature}"
    )
  }

  # Build config
  config <- list()
  if (!is.null(temperature)) {
    config$temperature <- temperature
  }
  if (!is.null(top_p)) {
    config$top_p <- top_p
  }
  # Create module
  mod <- module(
    signature = signature,
    config = config
  )

  # Store training data info for reference
  attr(mod, "input_names") <- names(x)
  attr(mod, "outcome_type") <- if (is.factor(y)) "factor" else class(y)[1]

  mod
}

#' @rdname fit_llm_predict
#' @export
predict_llm_class <- function(object, new_data, ...) {
  # Validate inputs
  if (!inherits(object, "Module")) {
    cli::cli_abort(c(
      "{.arg object} must be a dsprrr Module",
      "x" = "Got {.cls {class(object)[1]}}"
    ))
  }

  if (!is.data.frame(new_data)) {
    cli::cli_abort(c(
      "{.arg new_data} must be a data frame",
      "x" = "Got {.cls {class(new_data)[1]}}"
    ))
  }

  # Get predictions using the module
  input_names <- attr(object, "input_names") %||% names(new_data)

  # Check that required columns exist

  missing_cols <- setdiff(input_names, names(new_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Required columns missing from {.arg new_data}",
      "x" = "Missing: {.field {missing_cols}}"
    ))
  }

  results <- tryCatch(
    run_dataset(
      object,
      new_data[input_names],
      .progress = FALSE,
      .return_format = "simple"
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to generate predictions",
          "x" = conditionMessage(e)
        ),
        parent = e
      )
    }
  )

  # Extract predictions with error handling
  preds <- vapply(
    results$result,
    function(x) {
      if (is.list(x) && !is.null(x$output)) {
        as.character(x$output)
      } else if (is.na(x) || identical(x, NA_character_)) {
        NA_character_
      } else {
        as.character(x)
      }
    },
    character(1)
  )

  tibble::tibble(.pred_class = factor(preds))
}

#' @rdname fit_llm_predict
#' @export
predict_llm_numeric <- function(object, new_data, ...) {
  # Validate inputs
  if (!inherits(object, "Module")) {
    cli::cli_abort(c(
      "{.arg object} must be a dsprrr Module",
      "x" = "Got {.cls {class(object)[1]}}"
    ))
  }

  if (!is.data.frame(new_data)) {
    cli::cli_abort(c(
      "{.arg new_data} must be a data frame",
      "x" = "Got {.cls {class(new_data)[1]}}"
    ))
  }

  input_names <- attr(object, "input_names") %||% names(new_data)

  # Check that required columns exist
  missing_cols <- setdiff(input_names, names(new_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Required columns missing from {.arg new_data}",
      "x" = "Missing: {.field {missing_cols}}"
    ))
  }

  results <- tryCatch(
    run_dataset(
      object,
      new_data[input_names],
      .progress = FALSE,
      .return_format = "simple"
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to generate predictions",
          "x" = conditionMessage(e)
        ),
        parent = e
      )
    }
  )

  # Extract numeric predictions with error handling
  preds <- vapply(
    seq_along(results$result),
    function(i) {
      x <- results$result[[i]]
      if (is.list(x) && !is.null(x$output)) {
        val <- as.numeric(x$output)
        if (is.na(val)) {
          cli::cli_warn(
            "Could not convert LLM output to numeric for row {i}: {.val {x$output}}"
          )
          NA_real_
        } else {
          val
        }
      } else if (is.na(x)) {
        NA_real_
      } else {
        val <- as.numeric(x)
        if (is.na(val)) {
          cli::cli_warn(
            "Could not convert LLM output to numeric for row {i}: {.val {x}}"
          )
          NA_real_
        } else {
          val
        }
      }
    },
    numeric(1)
  )

  tibble::tibble(.pred = preds)
}

#' tunable() method for llm_predict
#'
#' Describes `temperature` and `top_p` as the tunable parameters of
#' [llm_predict()] specifications for the tune package.
#'
#' @param x An `llm_predict` model specification.
#' @param ... Unused.
#' @return A tibble describing the tunable parameters.
#' @noRd
tunable_llm_predict <- function(x, ...) {
  tibble::tibble(
    name = c("temperature", "top_p"),
    call_info = list(
      list(pkg = "dsprrr", fun = "temperature"),
      list(pkg = "dsprrr", fun = "top_p")
    ),
    source = c("model_spec", "model_spec"),
    component = c("main", "main"),
    component_id = c("main", "main")
  )
}

#' Temperature parameter for dials
#'
#' @description
#' `temperature()` creates a dials parameter for a model's sampling
#' temperature, for tuning [llm_predict()] specifications or building grids
#' for [optimize_grid()]. Reasoning models restrict it: gpt-6-luna, for
#' example, accepts `temperature` only when its reasoning effort is `"none"`.
#'
#' @param range Range of values (default `c(0, 1)`).
#' @param trans Optional transformation from the scales package; `NULL` (the
#'   default) means none.
#'
#' @return A dials quantitative parameter.
#' @export
#' @family integrations
#' @examplesIf rlang::is_installed("dials")
#' temperature()
#' temperature(range = c(0.1, 0.9))
#' dials::grid_regular(temperature(), levels = 3)
temperature <- function(range = c(0, 1), trans = NULL) {
  rlang::check_installed("dials", reason = "for temperature()")

  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(temperature = "Temperature"),
    finalize = NULL
  )
}

#' Top-p parameter for dials
#'
#' @description
#' `top_p()` creates a dials parameter for nucleus sampling (the share of
#' probability mass the model samples from), for tuning [llm_predict()]
#' specifications or building grids for [optimize_grid()]. Like
#' [temperature()], it applies to gpt-6-luna only when its reasoning effort is
#' `"none"`.
#'
#' @inheritParams temperature
#'
#' @return A dials quantitative parameter.
#' @export
#' @family integrations
#' @examplesIf rlang::is_installed("dials")
#' top_p()
#' top_p(range = c(0.5, 1))
top_p <- function(range = c(0, 1), trans = NULL) {
  rlang::check_installed("dials", reason = "for top_p()")

  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(top_p = "Top P"),
    finalize = NULL
  )
}

#' Reasoning effort parameter for dials
#'
#' @description
#' `reasoning_effort()` creates a dials parameter for the reasoning effort of
#' reasoning models such as gpt-6-luna, with the values `"low"`, `"medium"`
#' and `"high"`. Use it instead of [temperature()] when tuning a reasoning
#' model.
#'
#' @return A dials qualitative parameter.
#' @export
#' @family integrations
#' @examplesIf rlang::is_installed("dials")
#' reasoning_effort()
#' dials::grid_regular(reasoning_effort())
reasoning_effort <- function() {
  rlang::check_installed("dials", reason = "for reasoning_effort()")

  dials::new_qual_param(
    type = "character",
    values = c("low", "medium", "high"),
    label = c(reasoning_effort = "Reasoning Effort")
  )
}
