#' parsnip Integration for DSPrrr
#'
#' @description
#' Provides tidymodels/parsnip integration for dsprrr modules, enabling
#' LLM-based prediction within the tidymodels ecosystem.
#'
#' @details
#' This integration allows dsprrr modules to be used as parsnip model
#' specifications, making them compatible with tidymodels workflows including
#' `tune::tune_grid()`, `workflows::workflow()`, and `rsample` resampling.
#'
#' The integration registers a "dsprrr" engine for text classification and
#' generation tasks.
#'
#' @name parsnip-integration
#' @keywords internal
NULL

#' LLM Prediction Model Specification
#'
#' @description
#' Creates a parsnip model specification for LLM-based prediction using dsprrr.
#'
#' @param mode Model mode, typically "classification" or "regression" (for text
#'   tasks, classification is most common).
#' @param signature A dsprrr signature string or Signature object.
#' @param temperature Temperature parameter for LLM (tune-able).
#' @param top_p Top-p parameter for LLM (tune-able).
#' @param model LLM model name (e.g., "gpt-4o-mini").
#' @param provider LLM provider (e.g., "openai", "anthropic").
#'
#' @return A parsnip model specification object.
#'
#' @export
#' @examples
#' \dontrun{
#' library(parsnip)
#' library(tune)
#'
#' # Create LLM model spec
#' llm_spec <- llm_predict(
#'   mode = "classification",
#'   signature = "text -> sentiment: enum('positive', 'negative', 'neutral')"
#' ) |>
#'   set_engine("dsprrr", model = "gpt-4o-mini")
#'
#' # With tunable parameters
#' llm_spec_tuned <- llm_predict(
#'   mode = "classification",
#'   signature = "text -> sentiment",
#'   temperature = tune()
#' ) |>
#'   set_engine("dsprrr")
#' }
llm_predict <- function(
  mode = "classification",
  signature = NULL,
  temperature = NULL,
  top_p = NULL,
  model = NULL,
  provider = NULL
) {
  # Check for parsnip
  if (!requireNamespace("parsnip", quietly = TRUE)) {
    cli::cli_abort(c(
      "parsnip package required for llm_predict()",
      "i" = "Install with: {.code install.packages('parsnip')}"
    ))
  }

  args <- list(
    signature = rlang::enquo(signature),
    temperature = rlang::enquo(temperature),
    top_p = rlang::enquo(top_p),
    model = rlang::enquo(model),
    provider = rlang::enquo(provider)
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
  tuned <- vapply(x$args, function(a) {
    expr <- rlang::quo_get_expr(a)
    inherits(expr, "call") && identical(expr[[1]], quote(tune))
  }, logical(1))

  if (any(tuned)) {
    cli::cli_text("{.field Tuned}: {names(x$args)[tuned]}")
  }

  invisible(x)
}

#' Register dsprrr Engine with parsnip
#'
#' @description
#' Registers the dsprrr engine for use with llm_predict model specifications.
#' This function is called automatically when the package loads if parsnip
#' is available.
#'
#' @return NULL (invisibly), called for side effects.
#'
#' @export
#' @examples
#' \dontrun{
#' register_dsprrr_engine()
#' }
register_dsprrr_engine <- function() {
  if (!requireNamespace("parsnip", quietly = TRUE)) {
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

  parsnip::set_model_arg(
    model = "llm_predict",
    eng = "dsprrr",
    parsnip = "model",
    original = "model",
    func = list(pkg = "base", fun = "character"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "llm_predict",
    eng = "dsprrr",
    parsnip = "provider",
    original = "provider",
    func = list(pkg = "base", fun = "character"),
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

#' Fit LLM Predict Model
#'
#' @description
#' Internal function to fit an llm_predict model. Creates a dsprrr module
#' with the specified configuration.
#'
#' @param x Training data predictors (data frame).
#' @param y Training data outcomes.
#' @param signature Signature for the module.
#' @param temperature Temperature parameter.
#' @param top_p Top-p parameter.
#' @param model LLM model name.
#' @param provider LLM provider.
#' @param ... Additional arguments.
#'
#' @return A fitted dsprrr module.
#' @keywords internal
#' @export
fit_llm_predict <- function(
  x,
  y,
  signature = NULL,
  temperature = NULL,
  top_p = NULL,
  model = NULL,
  provider = NULL,
  ...
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

  # Build config
  config <- list()
  if (!is.null(temperature)) config$temperature <- temperature
  if (!is.null(top_p)) config$top_p <- top_p
  if (!is.null(model)) config$model <- model
  if (!is.null(provider)) config$provider <- provider

  # Create module
  mod <- module(
    signature = signature,
    type = "predict",
    config = config
  )

  # Store training data info for reference
  attr(mod, "input_names") <- names(x)
  attr(mod, "outcome_type") <- if (is.factor(y)) "factor" else class(y)[1]

  mod
}

#' Predict Class Labels with LLM
#'
#' @description
#' Internal function for class predictions with llm_predict models.
#'
#' @param object A fitted dsprrr module.
#' @param new_data New data for prediction.
#' @param ... Additional arguments.
#'
#' @return A tibble with .pred_class column.
#' @keywords internal
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
      cli::cli_abort(c(
        "Failed to generate predictions",
        "x" = conditionMessage(e)
      ), parent = e)
    }
  )

  # Extract predictions with error handling
  preds <- vapply(results$result, function(x) {
    if (is.list(x) && !is.null(x$output)) {
      as.character(x$output)
    } else if (is.na(x) || identical(x, NA_character_)) {
      NA_character_
    } else {
      as.character(x)
    }
  }, character(1))

  tibble::tibble(.pred_class = factor(preds))
}

#' Predict Numeric Values with LLM
#'
#' @description
#' Internal function for numeric predictions with llm_predict models.
#'
#' @param object A fitted dsprrr module.
#' @param new_data New data for prediction.
#' @param ... Additional arguments.
#'
#' @return A tibble with .pred column.
#' @keywords internal
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
      cli::cli_abort(c(
        "Failed to generate predictions",
        "x" = conditionMessage(e)
      ), parent = e)
    }
  )

  # Extract numeric predictions with error handling
  preds <- vapply(seq_along(results$result), function(i) {
    x <- results$result[[i]]
    if (is.list(x) && !is.null(x$output)) {
      val <- as.numeric(x$output)
      if (is.na(val)) {
        cli::cli_warn("Could not convert LLM output to numeric for row {i}: {.val {x$output}}")
        NA_real_
      } else {
        val
      }
    } else if (is.na(x)) {
      NA_real_
    } else {
      val <- as.numeric(x)
      if (is.na(val)) {
        cli::cli_warn("Could not convert LLM output to numeric for row {i}: {.val {x}}")
        NA_real_
      } else {
        val
      }
    }
  }, numeric(1))

  tibble::tibble(.pred = preds)
}

#' tunable Method for llm_predict
#'
#' @description
#' Returns information about tunable parameters for llm_predict models.
#' This method is only available when the tune package is loaded.
#'
#' @param x An llm_predict model specification.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble describing tunable parameters.
#' @keywords internal
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

#' Temperature Parameter for dials
#'
#' @description
#' Creates a dials parameter object for LLM temperature.
#'
#' @param range Range of temperature values (default c(0, 1)).
#' @param trans Transformation (default NULL for identity).
#'
#' @return A dials parameter object.
#' @export
#' @examples
#' \dontrun{
#' temperature()
#' temperature(range = c(0.1, 0.9))
#' }
temperature <- function(range = c(0, 1), trans = NULL) {
  if (!requireNamespace("dials", quietly = TRUE)) {
    cli::cli_abort("dials package required for temperature()")
  }

  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(temperature = "Temperature"),
    finalize = NULL
  )
}

#' Top-p Parameter for dials
#'
#' @description
#' Creates a dials parameter object for LLM top-p (nucleus sampling).
#'
#' @param range Range of top-p values (default c(0, 1)).
#' @param trans Transformation (default NULL for identity).
#'
#' @return A dials parameter object.
#' @export
#' @examples
#' \dontrun{
#' top_p()
#' top_p(range = c(0.5, 1))
#' }
top_p <- function(range = c(0, 1), trans = NULL) {
  if (!requireNamespace("dials", quietly = TRUE)) {
    cli::cli_abort("dials package required for top_p()")
  }

  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(top_p = "Top P"),
    finalize = NULL
  )
}

#' Reasoning Effort Parameter for dials
#'
#' @description
#' Creates a dials parameter object for reasoning model effort level.
#' Used with reasoning models like OpenAI o1/o3/o4-mini and GPT-5.
#'
#' @return A dials qualitative parameter object.
#' @export
#' @examples
#' \dontrun{
#' reasoning_effort()
#' }
reasoning_effort <- function() {
  if (!requireNamespace("dials", quietly = TRUE)) {
    cli::cli_abort("dials package required for reasoning_effort()")
  }

  dials::new_qual_param(
    type = "character",
    values = c("low", "medium", "high"),
    label = c(reasoning_effort = "Reasoning Effort")
  )
}

