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
  # Use the metric's field attribute if available to determine the output column
  output_col <- get_metric_field(teleprompter@metric)
  demos <- format_trainset_as_demos(
    demos_data,
    program$signature,
    output_col = output_col
  )

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
#' @usage NULL
#' @export
GridSearchTeleprompter <- S7::new_class(
  "GridSearchTeleprompter",
  parent = Teleprompter,
  properties = list(
    variants = S7::new_property(
      S7::class_data.frame,
      default = tibble::tibble(
        id = 1L,
        instructions = NA_character_,
        template = NA_character_
      ),
      validator = function(value) {
        if (!is.data.frame(value)) {
          return("variants must be a data frame or tibble")
        }
        # Allow empty data frame for default, but require rows if provided
        if (
          !identical(
            value,
            tibble::tibble(
              id = 1L,
              instructions = NA_character_,
              template = NA_character_
            )
          ) &&
            nrow(value) == 0
        ) {
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
compile_gridsearch <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  ...
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort(
      "GridSearchTeleprompter currently only supports Predict modules"
    )
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
  # Use the metric's field attribute if available to determine the output column
  output_col <- get_metric_field(teleprompter@metric)
  n_demos <- min(teleprompter@k, nrow(trainset_for_demos))
  if (n_demos > 0) {
    demo_indices <- sample(nrow(trainset_for_demos), n_demos)
    demos_data <- trainset_for_demos[demo_indices, , drop = FALSE]
    demos <- format_trainset_as_demos(
      demos_data,
      program$signature,
      output_col = output_col
    )
  } else {
    demos <- list()
  }

  variants <- tibble::as_tibble(teleprompter@variants)
  variants$id <- as.character(variants$id)

  base_instructions <- program$signature@instructions
  if ("instructions_suffix" %in% names(variants)) {
    variants$instructions <- ifelse(
      !is.na(variants$instructions_suffix),
      paste(base_instructions, variants$instructions_suffix),
      variants$instructions
    )
    variants$instructions_suffix <- NULL
  }

  # Ensure instructions column defaults to base instructions when missing/NA
  if (!"instructions" %in% names(variants)) {
    variants$instructions <- base_instructions
  } else {
    variants$instructions[is.na(variants$instructions)] <- base_instructions
  }

  optimized <- copy_module(program)
  optimized$demos <- demos

  optimized$optimize_grid(
    data = valset,
    metric = teleprompter@metric,
    grid = variants,
    .llm = .llm,
    control = list(
      progress = teleprompter@verbose,
      evaluation_progress = FALSE,
      parallel = FALSE
    )
  )

  optimized$config$compiled <- TRUE
  optimized$config$teleprompter <- "GridSearchTeleprompter"
  optimized$config$best_variant <- optimized$state$best_params$id %||%
    NA_character_
  optimized$config$best_score <- optimized$state$best_score
  optimized$config$all_variants <- variants
  optimized$config$all_scores <- stats::setNames(
    optimized$state$trials$score,
    vapply(
      optimized$state$trials$parameters,
      function(param) param$id %||% NA_character_,
      character(1)
    )
  )

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
    inputs = sig@inputs, # Lists are copied by value
    output_type = sig@output_type, # These are typically immutable
    instructions = sig@instructions
  )
}

#' Extract output value from a trainset row
#'
#' Searches for a field in a data frame row. If `field` is provided, it:
#' 1. First checks if field is a direct column name
#' 2. If not, searches within list columns for that field as a nested key
#'
#' @param row A single-row data frame
#' @param field The field name to search for (can be column name or nested key)
#' @param input_names Names of input columns to exclude from search
#' @return The extracted value, or NULL if not found
#' @noRd
extract_output_from_row <- function(row, field, input_names = character()) {
  # If field is provided, search for it specifically

  if (!is.null(field)) {
    # First: check if it's a direct column name
    if (field %in% names(row)) {
      return(row[[field]])
    }

    # Second: search within list columns for nested field
    for (col_name in names(row)) {
      col_value <- row[[col_name]]
      # Check if this column contains a list with our field
      if (is.list(col_value) && !is.data.frame(col_value)) {
        # Handle list columns (tibble stores as list of length 1 per row)
        val <- if (length(col_value) == 1 && is.list(col_value[[1]])) {
          col_value[[1]]
        } else {
          col_value
        }
        if (field %in% names(val)) {
          return(val[[field]])
        }
      }
    }

    # Field not found anywhere
    return(NULL)
  }

  # No field specified: fall back to finding any non-input column
  remaining_cols <- setdiff(names(row), input_names)
  if (length(remaining_cols) > 0) {
    return(row[[remaining_cols[1]]])
  }

  NULL
}

#' Unwrap tibble list column value
#'
#' Tibbles store list columns as list-of-lists. This helper unwraps
#' the extra layer when accessing a single row.
#'
#' @param value The value from row[[column]]
#' @return The unwrapped value
#' @noRd
unwrap_list_column <- function(value) {
  if (is.list(value) && length(value) == 1 && is.list(value[[1]])) {
    value[[1]]
  } else {
    value
  }
}

#' Detect the output column in a trainset
#'
#' @param trainset Data frame containing training examples
#' @param fields Optional field name(s) from metric. Can be:
#'   - NULL: auto-detect output column
#'   - Single string: column name or nested field name
#'   - Character vector: multiple nested field names to extract
#' @param input_names Names of input columns to exclude
#' @return List describing how to extract output values
#' @noRd
detect_output_source <- function(trainset, fields, input_names) {
  # Handle multiple fields case
  if (length(fields) > 1) {
    # Multiple fields: find which column contains them
    if (nrow(trainset) > 0) {
      row <- trainset[1, , drop = FALSE]
      for (col_name in names(row)) {
        col_value <- row[[col_name]]
        if (is.list(col_value) && !is.data.frame(col_value)) {
          val <- unwrap_list_column(col_value)
          # Check if all fields are present in this column
          if (all(fields %in% names(val))) {
            return(list(type = "multi", column = col_name, fields = fields))
          }
        }
      }
    }
    # Fields not found
    return(list(type = "not_found", fields = fields))
  }

  # Single field case
  field <- fields

  # If field is provided, check if it's a direct column
  if (!is.null(field) && field %in% names(trainset)) {
    return(list(type = "column", name = field))
  }

  # If field is provided but not a column, it might be nested
  if (!is.null(field)) {
    # Check first row for nested field
    if (nrow(trainset) > 0) {
      row <- trainset[1, , drop = FALSE]
      for (col_name in names(row)) {
        col_value <- row[[col_name]]
        if (is.list(col_value) && !is.data.frame(col_value)) {
          val <- unwrap_list_column(col_value)
          if (field %in% names(val)) {
            return(list(type = "nested", column = col_name, field = field))
          }
        }
      }
    }
    # Field not found - will return NULL for outputs
    return(list(type = "not_found", field = field))
  }

  # No field specified: try common output column names
  possible_output_names <- c(
    "output",
    "label",
    "answer",
    "response",
    "result",
    "y"
  )
  for (col in possible_output_names) {
    if (col %in% names(trainset)) {
      return(list(type = "column", name = col))
    }
  }

  # Fall back to first non-input column
  remaining_cols <- setdiff(names(trainset), input_names)
  if (length(remaining_cols) > 0) {
    if (length(remaining_cols) > 1) {
      cli::cli_warn(c(
        "Multiple potential output columns found",
        "i" = "Using: {remaining_cols[1]}",
        "i" = "Other columns: {remaining_cols[-1]}"
      ))
    }
    return(list(type = "column", name = remaining_cols[1]))
  }

  list(type = "none")
}

#' Format training data as demonstrations
#'
#' @param trainset Data frame containing training examples
#' @param signature The module's signature
#' @param output_col Optional field name(s) for output. This can be:
#'   - A direct column name in the trainset
#'   - A nested field name within a list column (e.g., "classification" inside
#'     an "output" column containing `list(classification = "positive")`)
#'   - A character vector of multiple field names to extract as a named list
#'   Typically extracted from the metric's field attribute via `get_metric_field()`.
#' @noRd
format_trainset_as_demos <- function(trainset, signature, output_col = NULL) {
  demos <- list()

  # Get input names from signature
  input_names <- vapply(signature@inputs, function(x) x$name, character(1))

  # Detect where output values come from
  output_source <- detect_output_source(trainset, output_col, input_names)

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

    # Extract output based on detected source
    demo_output <- switch(
      output_source$type,
      "column" = {
        val <- row[[output_source$name]]
        # Unwrap tibble list column if needed
        unwrap_list_column(val)
      },
      "nested" = {
        col_value <- row[[output_source$column]]
        val <- unwrap_list_column(col_value)
        val[[output_source$field]]
      },
      "multi" = {
        col_value <- row[[output_source$column]]
        val <- unwrap_list_column(col_value)
        # Extract only the specified fields as a named list
        result <- list()
        for (field_name in output_source$fields) {
          result[[field_name]] <- val[[field_name]]
        }
        result
      },
      "not_found" = NULL,
      "none" = NULL,
      NULL
    )
    demos[[i]] <- list(
      inputs = demo_inputs,
      output = demo_output
    )
  }

  demos
}

#' Evaluate a module on data
#' @noRd
evaluate_module <- function(module, data, metric, .llm = NULL, ...) {
  evaluate(
    module,
    data,
    metric,
    .llm = .llm,
    .parallel = FALSE,
    .progress = FALSE,
    ...
  )
}
