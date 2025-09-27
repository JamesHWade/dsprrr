#' Teleprompter Base Class
#'
#' @description
#' Base S7 class for optimization strategies (teleprompters). Teleprompters
#' are responsible for optimizing modules by adjusting their prompts,
#' demonstrations, or other parameters based on training data.
#'
#' @param metric A metric function for evaluating predictions. If NULL,
#'   uses exact_match() by default.
#' @param metric_threshold Minimum score required to be considered successful.
#'   If NULL, uses the metric's default threshold.
#' @param max_errors Maximum number of errors allowed during optimization.
#'   Default is 5.
#'
#' @export
Teleprompter <- S7::new_class(
  "Teleprompter",
  properties = list(
    metric = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !is.function(value)) {
          return("metric must be a function or NULL")
        }
        NULL
      }
    ),
    metric_threshold = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value)) {
          if (!is.numeric(value) || length(value) != 1) {
            return("metric_threshold must be a single numeric value or NULL")
          }
          if (value < 0 || value > 1) {
            return("metric_threshold must be between 0 and 1")
          }
        }
        NULL
      }
    ),
    max_errors = S7::new_property(
      S7::class_integer,
      default = 5L,
      validator = function(value) {
        if (!is.null(value) && value < 0) {
          return("max_errors must be non-negative")
        }
        NULL
      }
    )
  )
)

#' Default compile method for Teleprompter
#' @noRd
compile_default <- function(teleprompter, program, trainset, ...) {
  cli::cli_abort(c(
    "compile() method not implemented for this teleprompter",
    "i" = "Teleprompter class: {.cls {class(teleprompter)[1]}}"
  ))
}

#' LabeledFewShot Teleprompter
#'
#' @description
#' A simple teleprompter that adds labeled examples from the training set
#' as demonstrations to the module. This is the simplest form of few-shot
#' learning.
#'
#' @param metric A metric function for evaluating predictions. If NULL,
#'   uses exact_match() by default.
#' @param metric_threshold Minimum score required to be considered successful.
#'   If NULL, uses the metric's default threshold.
#' @param max_errors Maximum number of errors allowed during optimization.
#'   Default is 5.
#' @param k Number of examples to include in few-shot prompts. Default is 4.
#' @param sample Whether to randomly sample examples. Default is TRUE.
#' @param seed Random seed for reproducibility. Default is 123.
#'
#' @export
LabeledFewShot <- S7::new_class(
  "LabeledFewShot",
  parent = Teleprompter,
  properties = list(
    k = S7::new_property(
      S7::class_integer,
      default = 4L,
      validator = function(value) {
        if (value < 0) {
          return("k must be non-negative")
        }
        NULL
      }
    ),
    sample = S7::new_property(
      S7::class_logical,
      default = TRUE,
      validator = function(value) {
        if (length(value) != 1) {
          return("sample must be a single logical value")
        }
        NULL
      }
    ),
    seed = S7::new_property(
      S7::class_integer,
      default = 123L,
      validator = function(value) {
        if (!is.null(value) && !is.na(value) && value < 0) {
          return("seed must be non-negative or NULL")
        }
        NULL
      }
    )
  )
)

#' Compile method for LabeledFewShot
#' @noRd
compile_labeled <- function(teleprompter, program, trainset, .llm = NULL, ...) {
  # Validate inputs
  if (!inherits(program, "Module")) {
    cli::cli_abort("LabeledFewShot currently only supports Predict modules")
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("trainset must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }

  # Create a copy of the program
  optimized <- copy_module(program)

  # Determine number of demos to use
  n_demos <- min(teleprompter@k, nrow(trainset))

  # Select demos
  if (teleprompter@sample && nrow(trainset) > n_demos) {
    # Set seed for reproducibility if provided
    if (!is.null(teleprompter@seed) && !is.na(teleprompter@seed)) {
      set.seed(teleprompter@seed)
    }
    selected_rows <- sample(nrow(trainset), n_demos)
    demos_data <- trainset[selected_rows, , drop = FALSE]
  } else {
    demos_data <- trainset[seq_len(n_demos), , drop = FALSE]
  }

  # Convert to demo format expected by the module
  demos <- format_trainset_as_demos(demos_data, program$signature)

  # Update the module's demos
  optimized$demos <- demos
  optimized$state$compiled <- TRUE
  optimized$config$compiled <- TRUE
  optimized$config$teleprompter <- "LabeledFewShot"
  optimized$config$compilation_k <- n_demos

  optimized
}

#' GridSearchTeleprompter
#'
#' @description
#' A teleprompter that performs grid search over different instruction
#' and template variants to find the best performing configuration.
#'
#' @param metric A metric function for evaluating predictions. If NULL,
#'   uses exact_match() by default.
#' @param metric_threshold Minimum score required to be considered successful.
#'   If NULL, uses the metric's default threshold.
#' @param max_errors Maximum number of errors allowed during optimization.
#'   Default is 5.
#' @param variants A data frame containing variant configurations to test.
#'   Must have an 'id' column. Other columns define parameter values.
#'   Default is a tibble with one row containing NA values for instructions and template.
#' @param k Number of examples to include in few-shot prompts. Default is 2.
#' @param eval_sample_size Number of examples to use for evaluation during
#'   grid search. Default is 50.
#' @param verbose Whether to print progress messages. Default is TRUE.
#'
#' @export
GridSearchTeleprompter <- S7::new_class(
  "GridSearchTeleprompter",
  parent = Teleprompter,
  properties = list(
    variants = S7::new_property(
      S7::class_data.frame,
      default = tibble::tibble(id = 1L, instructions = NA_character_, template = NA_character_),
      validator = function(value) {
        if (!is.data.frame(value)) {
          return("variants must be a data frame or tibble")
        }
        # Allow empty data frame for default, but require rows if provided
        if (!identical(value, tibble::tibble(id = 1L, instructions = NA_character_, template = NA_character_)) &&
            nrow(value) == 0) {
          return("variants must have at least one row when provided")
        }
        # Check for required columns
        required_cols <- c("id")
        if (!all(required_cols %in% names(value))) {
          return("variants must have an 'id' column")
        }
        NULL
      }
    ),
    k = S7::new_property(
      S7::class_integer,
      default = 2L,
      validator = function(value) {
        if (value < 0) {
          return("k must be non-negative")
        }
        NULL
      }
    ),
    eval_sample_size = S7::new_property(
      S7::class_integer,
      default = 50L,
      validator = function(value) {
        if (value < 1) {
          return("eval_sample_size must be positive")
        }
        NULL
      }
    ),
    verbose = S7::new_property(
      S7::class_logical,
      default = TRUE
    )
  )
)

#' Compile method for GridSearchTeleprompter
#' @noRd
compile_gridsearch <- function(teleprompter, program, trainset, valset = NULL,
                               .llm = NULL, ...) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("GridSearchTeleprompter currently only supports Predict modules")
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("trainset must be a data frame")
  }

  if (is.null(teleprompter@metric)) {
    cli::cli_abort("GridSearchTeleprompter requires a metric function")
  }

  # Use validation set if provided, otherwise use a portion of trainset
  if (is.null(valset)) {
    n_val <- min(teleprompter@eval_sample_size, ceiling(nrow(trainset) * 0.2))
    val_indices <- sample(nrow(trainset), n_val)
    valset <- trainset[val_indices, , drop = FALSE]
    train_indices <- setdiff(seq_len(nrow(trainset)), val_indices)
    trainset_for_demos <- trainset[train_indices, , drop = FALSE]
  } else {
    trainset_for_demos <- trainset
  }

  # Prepare demos from training set
  n_demos <- min(teleprompter@k, nrow(trainset_for_demos))
  if (n_demos > 0) {
    demo_indices <- sample(nrow(trainset_for_demos), n_demos)
    demos_data <- trainset_for_demos[demo_indices, , drop = FALSE]
    demos <- format_trainset_as_demos(demos_data, program$signature)
  } else {
    demos <- list()
  }

  # Evaluate each variant
  variants <- teleprompter@variants
  scores <- numeric(nrow(variants))

  if (teleprompter@verbose) {
    cli::cli_progress_bar("Testing variants", total = nrow(variants))
  }

  for (i in seq_len(nrow(variants))) {
    variant <- variants[i, , drop = FALSE]

    # Create a modified program for this variant
    test_program <- copy_module(program)
    test_program$demos <- demos

    # Apply modifications from variant
    if ("instructions" %in% names(variant)) {
      test_program$signature@instructions <- variant$instructions
    }
    if ("instructions_suffix" %in% names(variant)) {
      test_program$signature@instructions <- paste(
        program$signature@instructions,
        variant$instructions_suffix
      )
    }
    if ("template" %in% names(variant)) {
      test_program$template <- variant$template
    }

    # Evaluate on validation set
    eval_res <- evaluate(
      test_program,
      valset,
      teleprompter@metric,
      .llm = .llm,
      .parallel = FALSE,
      .progress = FALSE
    )
    scores[i] <- eval_res$mean_score

    if (teleprompter@verbose) {
      cli::cli_progress_update()
      cli::cli_alert_info("Variant {variant$id}: score = {round(score, 3)}")
    }
  }

  if (teleprompter@verbose) {
    cli::cli_progress_done()
  }

  # Select best variant
  best_idx <- which.max(scores)
  best_variant <- variants[best_idx, , drop = FALSE]

  if (teleprompter@verbose) {
    cli::cli_alert_success("Best variant: {best_variant$id} (score: {round(scores[best_idx], 3)})")
  }

  # Create optimized program with best variant
  optimized <- copy_module(program)
  optimized$demos <- demos

  # Apply best variant modifications
  if ("instructions" %in% names(best_variant)) {
    optimized$signature@instructions <- best_variant$instructions
  }
  if ("instructions_suffix" %in% names(best_variant)) {
    optimized$signature@instructions <- paste(
      program$signature@instructions,
      best_variant$instructions_suffix
    )
  }
  if ("template" %in% names(best_variant)) {
    optimized$template <- best_variant$template
  }

  # Mark as compiled
  optimized$state$compiled <- TRUE
  optimized$config$compiled <- TRUE
  optimized$config$teleprompter <- "GridSearchTeleprompter"
  optimized$config$best_variant <- best_variant$id
  optimized$config$best_score <- scores[best_idx]
  optimized$config$all_scores <- stats::setNames(scores, variants$id)

  optimized
}

# Helper functions

#' Copy a module (deep copy)
#' @noRd
copy_module <- function(module) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("Can only copy Module objects")
  }

  # Use the module's deepcopy method
  module$deepcopy()
}

#' Copy a signature
#' @noRd
copy_signature <- function(sig) {
  Signature(
    inputs = sig@inputs,  # Lists are copied by value
    output_type = sig@output_type,  # These are typically immutable
    instructions = sig@instructions
  )
}

#' Format training data as demonstrations
#' @noRd
format_trainset_as_demos <- function(trainset, signature) {
  demos <- list()

  # Get input names from signature
  input_names <- vapply(signature@inputs, function(x) x$name, character(1))

  # Determine output column name
  # Try common names first
  output_col <- NULL
  possible_output_names <- c("output", "label", "answer", "response", "result", "y")
  for (col in possible_output_names) {
    if (col %in% names(trainset)) {
      output_col <- col
      break
    }
  }

  # If no standard output column found, use any column not in inputs
  if (is.null(output_col)) {
    remaining_cols <- setdiff(names(trainset), input_names)
    if (length(remaining_cols) > 0) {
      output_col <- remaining_cols[1]
      if (length(remaining_cols) > 1) {
        cli::cli_warn(c(
          "Multiple potential output columns found",
          "i" = "Using: {output_col}",
          "i" = "Other columns: {remaining_cols[-1]}"
        ))
      }
    }
  }

  # Create demos
  for (i in seq_len(nrow(trainset))) {
    row <- trainset[i, , drop = FALSE]

    # Extract inputs
    demo_inputs <- list()
    for (name in input_names) {
      if (name %in% names(row)) {
        demo_inputs[[name]] <- row[[name]]
      }
    }

    # Extract output
    demo_output <- if (!is.null(output_col) && output_col %in% names(row)) {
      row[[output_col]]
    } else {
      NULL
    }

    demos[[i]] <- list(
      inputs = demo_inputs,
      output = demo_output
    )
  }

  demos
}

#' Evaluate a module on a dataset
#' @noRd
evaluate_module <- function(module, dataset, metric, .llm = NULL, ...) {
  evaluate(module, dataset, metric, .llm = .llm, .parallel = FALSE,
           .progress = FALSE, ...)
}
