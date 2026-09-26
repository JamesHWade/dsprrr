#' Arguments shared by all optimizers
#'
#' @description
#' `Teleprompter()` is the parent class of every optimizer in dsprrr
#' ("teleprompter" is DSPy's original name for an optimizer). You never
#' compile with it directly: create one of the optimizers below and pass it to
#' [compile()]. This page documents the three arguments that every optimizer
#' accepts.
#'
#' @details
#' Most optimizers store counts as S7 integer properties, so write `5L` rather
#' than `5`. A double fails with an error such as
#' `@max_errors must be <integer>, not <double>`.
#'
#' ## Optimizers at a glance
#'
#' | Optimizer | What it changes |
#' |---|---|
#' | [LabeledFewShot()] | Demos: `k` training rows. No model calls. |
#' | [KNNFewShot()] | Demos: the training rows most similar to each input, chosen at run time. |
#' | [BootstrapFewShot()] | Demos: training rows plus the program's own outputs that pass the metric. |
#' | [BootstrapFewShotWithRandomSearch()] | Demos: the best of several bootstrap candidates on a validation set. |
#' | [GridSearchTeleprompter()] | Instructions, template or settings, from a table of variants. |
#' | [COPRO()] | Instructions, rewritten by a model over several rounds. |
#' | [MIPROv2()] | Instructions and demos, searched together. |
#' | [SIMBA()] | Instruction rules and demos taken from hard examples. |
#' | [GEPA()] | Instructions (and Flex source) evolved by reflecting on failures. |
#' | [ReAnchor()] | Thresholds, cuts and weights of decision outputs. |
#' | [BetterTogether()], [Omni()] | Run other optimizers in sequence or in competition. |
#' | [AutoResearch()], [MetaHarness()] | An agent proposes and tests program edits. |
#'
#' To sweep runtime settings such as `reasoning_effort` on a module in place,
#' use [optimize_grid()] instead.
#'
#' @param metric A metric function called as `metric(prediction, expected)`,
#'   such as `metric_exact_match(field = "answer")`, or `NULL`. Optimizers that
#'   score candidates require one.
#' @param metric_threshold A score between 0 and 1, or `NULL` (the default).
#'   Only some optimizers use it, and each optimizer's page says how.
#' @param max_errors Integer error budget (default `5L`). The optimizers that
#'   run under [optimizer_control()] (the two bootstrap optimizers, COPRO,
#'   MIPROv2, SIMBA, GEPA, AutoResearch and MetaHarness) stop after this many
#'   consecutive failed evaluations when [compile()] is not given a `control`
#'   object.
#'
#' @return A `Teleprompter` object.
#' @family teleprompters
#' @examples
#' Teleprompter()
#'
#' # Counts must be integers
#' try(Teleprompter(max_errors = 5))
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

#' Labeled few-shot: add training rows as demonstrations
#'
#' @description
#' `LabeledFewShot()` compiles a Predict module by attaching `k` rows of the
#' training set as few-shot demonstrations. It makes no model calls and scores
#' nothing, which makes it a fast, offline baseline for the other optimizers.
#'
#' @details
#' Rows are drawn at random (or the first `k` rows are taken when
#' `sample = FALSE`); nothing checks whether they are good examples. A
#' demonstration's inputs come from the columns named after the signature
#' inputs. Its output comes from the metric's `field` when the metric has one,
#' otherwise from the first column named `output`, `label`, `answer`,
#' `response`, `result` or `y`, otherwise from the first non-input column.
#'
#' The program must be a Predict module, such as one from [module()] or
#' [chain_of_thought()]. Other programs, including pipelines and RLM modules,
#' are rejected because training rows do not match the signatures of their
#' inner predictors. [BootstrapFewShot()] compiles pipelines.
#'
#' @param metric Optional. It is not used for scoring, but when it has a
#'   `field` (as in `metric_exact_match(field = "sentiment")`), that column
#'   supplies the demonstrations' outputs.
#' @param metric_threshold,max_errors Accepted for consistency with the other
#'   optimizers (see [Teleprompter()]); `LabeledFewShot()` does not use them.
#' @param k Integer number of demonstrations (default `4L`). When the training
#'   set has fewer rows, all of them are used.
#' @param sample If `TRUE` (the default), draw `k` rows at random. If `FALSE`,
#'   take the first `k` rows.
#' @param seed Integer seed for the random draw (default `123L`). It is passed
#'   to [set.seed()], which also resets the session's random number stream.
#'
#' @return A `LabeledFewShot` object to pass to [compile()].
#' @family teleprompters
#' @examples
#' classifier <- module(signature("text -> sentiment"))
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience", "It's okay", "Works well"),
#'   sentiment = c("positive", "negative", "neutral", "positive")
#' )
#'
#' tp <- LabeledFewShot(k = 2L)
#' tp
#'
#' # Compiling makes no model calls and leaves `classifier` unchanged
#' compiled <- compile(classifier, tp, trainset)
#' best_demos(compiled, as_tibble = TRUE)
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
    cli::cli_abort("LabeledFewShot requires a Module object")
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("trainset must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }

  if (!inherits(program, "PredictModule")) {
    cli::cli_abort(
      c(
        "LabeledFewShot cannot label nested predictors from root examples",
        "x" = "The program is {.cls {class(program)[1]}}, not a Predict module.",
        "i" = paste(
          "Root examples can have a different signature from child predictors;",
          "attaching them would create invalid demonstrations."
        ),
        "i" = paste(
          "Use an optimizer with predictor-local evidence, or compile each",
          "predictor with examples matching its own signature."
        )
      ),
      class = "dsprrr_labeled_graph_unsupported"
    )
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
    selected_rows <- seq_len(n_demos)
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
  optimized$config$compilation_k <- n_demos
  record_optimization_result(
    optimized,
    optimizer = "LabeledFewShot",
    best_params = list(k = n_demos),
    lineage = list(selected_rows = as.integer(selected_rows)),
    stop_reason = "completed",
    extensions = list(sample = teleprompter@sample, seed = teleprompter@seed)
  )

  optimized
}

#' Grid search over instructions and demos
#'
#' @description
#' `GridSearchTeleprompter()` tries each row of a `variants` table (different
#' instructions, prompt templates or settings) on one module, scores every
#' variant with `metric`, and returns a copy of the module with the best
#' variant applied. All variants share one set of `k` demonstrations drawn from
#' the training set.
#'
#' @details
#' Each row of `variants` is one candidate. These columns have an effect:
#'
#' * `id` (required) labels the variant.
#' * `instructions` replaces the signature's instructions (`NA` keeps them).
#' * `instructions_suffix` is appended to the original instructions.
#' * `template` replaces the prompt template.
#' * Runtime settings such as `temperature` or `top_p` are sent to the chat.
#'
#' Other columns are stored in the module's `config` but do not change the
#' prompt.
#'
#' When you pass `valset` to [compile()], variants are scored on it and the
#' demonstrations are drawn from all of `trainset`. Without `valset`,
#' `min(eval_sample_size, ceiling(0.2 * nrow(trainset)))` random rows of
#' `trainset` are held out for scoring and the demonstrations come from the
#' remaining rows. The drawn demonstrations replace any the module already had.
#' The split and the draw use R's random number generator without a fixed
#' seed, so call [set.seed()] first for reproducible results.
#'
#' The search runs [optimize_grid()] on a copy of the module, so the scores of
#' all variants are available from [optimization_result()] and [top_trials()].
#'
#' @param metric A metric function such as
#'   `metric_exact_match(field = "sentiment")`. Required when compiling.
#' @param metric_threshold,max_errors Accepted for consistency with the other
#'   optimizers (see [Teleprompter()]); grid search does not use them.
#' @param variants A data frame with one row per variant and an `id` column;
#'   see Details. The default is a single variant that keeps the module's
#'   instructions and template.
#' @param k Integer number of demonstrations attached to every variant
#'   (default `2L`). Use `0L` for none.
#' @param eval_sample_size Integer cap on the number of held-out scoring rows
#'   when no `valset` is given (default `50L`).
#' @param verbose Whether to show a progress bar (default `TRUE`).
#'
#' @return A `GridSearchTeleprompter` object to pass to [compile()].
#' @family teleprompters
#' @family grid search
#' @examples
#' variants <- data.frame(
#'   id = c("terse", "explicit"),
#'   instructions = c(
#'     "Answer with one word.",
#'     "Label the sentiment of the text as positive, negative or neutral."
#'   )
#' )
#' tp <- GridSearchTeleprompter(
#'   metric = metric_exact_match(field = "sentiment"),
#'   variants = variants,
#'   k = 1L
#' )
#' tp@variants
#'
#' \dontrun{
#' classifier <- module(signature("text -> sentiment"))
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience", "It's okay", "Works well"),
#'   sentiment = c("positive", "negative", "neutral", "positive")
#' )
#' set.seed(1)
#' optimized <- compile(
#'   classifier,
#'   tp,
#'   trainset,
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' top_trials(optimized)
#' }
#' @usage
#' \special{GridSearchTeleprompter(
#'   metric = NULL,
#'   metric_threshold = NULL,
#'   max_errors = 5L,
#'   variants = tibble::tibble(id = 1L, instructions = NA_character_,
#'     template = NA_character_),
#'   k = 2L,
#'   eval_sample_size = 50L,
#'   verbose = TRUE
#' )}
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

  optimize_grid(
    optimized,
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

  grid_result <- optimization_result(optimized)
  best_variant <- grid_result$best_params$id %||% NA_character_
  all_scores <- stats::setNames(
    grid_result$trials$score,
    vapply(
      grid_result$trials$parameters,
      function(param) param$id %||% NA_character_,
      character(1)
    )
  )
  optimized$config$best_variant <- best_variant
  optimized$config$best_score <- grid_result$best_score
  optimized$config$all_variants <- variants
  optimized$config$all_scores <- all_scores
  record_optimization_result(
    optimized,
    optimizer = "GridSearchTeleprompter",
    baseline_score = grid_result$baseline_score,
    best_score = grid_result$best_score,
    best_trial = grid_result$best_trial,
    best_params = grid_result$best_params,
    trials = grid_result$trials,
    lineage = list(best_variant = best_variant),
    stop_reason = "completed",
    extensions = list(variants = variants, all_scores = all_scores)
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
  artifact_copy_runtime(module, module$deepcopy())
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
#' @param value The value from a row's list column
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
    # Fields not found - warn the user
    cli::cli_warn(c(
      "Could not find all requested fields in trainset",
      "i" = "Requested fields: {.val {fields}}",
      "i" = "Available columns: {.val {names(trainset)}}",
      "!" = "Demo outputs will be set to NULL"
    ))
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
    # Field not found - warn the user
    cli::cli_warn(c(
      "Could not find output field {.val {field}} in trainset",
      "i" = "Available columns: {.val {names(trainset)}}",
      "!" = "Demo outputs will be set to NULL"
    ))
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

  # No output column found at all - this is likely a configuration error
  cli::cli_warn(c(
    "No output column found in trainset",
    "i" = "Trainset columns: {.val {names(trainset)}}",
    "i" = "Input columns (excluded): {.val {input_names}}",
    "!" = "All demos will have NULL output"
  ))
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
  # Validate output_col parameter
  if (!is.null(output_col) && !is.character(output_col)) {
    cli::cli_abort(c(
      "{.arg output_col} must be a character vector or NULL",
      "x" = "Got: {.cls {class(output_col)}}"
    ))
  }

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
      {
        cli::cli_warn(c(
          "Unknown output source type: {.val {output_source$type}}",
          "!" = "Demo output set to NULL"
        ))
        NULL
      }
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
    .progress = FALSE,
    ...
  )
}
