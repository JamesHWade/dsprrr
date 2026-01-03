#' Ensemble Teleprompter
#'
#' @description
#' A teleprompter that combines multiple compiled modules into an ensemble
#' that votes on outputs. Unlike other teleprompters, Ensemble doesn't
#' optimize a single module - it wraps multiple already-compiled modules.
#'
#' @name teleprompter-ensemble
NULL

#' Ensemble Teleprompter
#'
#' @description
#' Creates an ensemble from multiple compiled modules. The ensemble runs
#' all modules and combines their outputs using a reduce function
#' (default: majority voting).
#'
#' @param reduce_fn Function to combine outputs. Default is `reduce_majority()`.
#'   Other options include `reduce_weighted_vote()`, `reduce_first()`, and
#'   `reduce_best_by_metric()`.
#' @param size Optional integer. If provided, limits the number of modules
#'   to include in the ensemble (takes the first `size` from the programs list).
#' @param weights Optional numeric vector of weights for each module.
#'   Commonly set to validation scores from optimization.
#' @param metric A metric function for evaluating predictions. If NULL,
#'   uses exact_match() by default. Used for computing ensemble performance.
#' @param metric_threshold Minimum score required to be considered successful.
#' @param max_errors Maximum number of errors allowed during optimization.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create ensemble teleprompter
#' tp <- Ensemble(reduce_fn = reduce_majority())
#'
#' # Compile with list of modules
#' compiled_modules <- list(mod1, mod2, mod3)
#' ens <- compile(tp, programs = compiled_modules)
#'
#' # Or use top candidates from BootstrapFewShotWithRandomSearch
#' rs_compiled <- compile(rs_teleprompter, program, trainset)
#' candidates <- rs_compiled$config$optimizer$candidate_programs[1:3]
#' weights <- rs_compiled$config$optimizer$candidate_scores[1:3]
#' tp <- Ensemble(
#'   reduce_fn = reduce_weighted_vote(),
#'   weights = weights
#' )
#' ens <- compile(tp, programs = candidates)
#' }
Ensemble <- S7::new_class(
  "Ensemble",
  parent = Teleprompter,
  properties = list(
    reduce_fn = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !is.function(value)) {
          return("reduce_fn must be a function or NULL")
        }
        NULL
      }
    ),
    size = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value)) {
          if (!is.numeric(value) || length(value) != 1 || value < 1) {
            return("size must be a positive integer or NULL")
          }
        }
        NULL
      }
    ),
    weights = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !is.numeric(value)) {
          return("weights must be numeric or NULL")
        }
        NULL
      }
    )
  )
)

#' Compile method for Ensemble
#'
#' @description
#' Unlike other teleprompters that optimize a single module using training data,
#' the Ensemble teleprompter combines multiple already-compiled modules.
#'
#' @param teleprompter An Ensemble teleprompter object
#' @param program Either a single module (ignored) or NULL. When using Ensemble,
#'   the programs are passed via the `programs` argument.
#' @param trainset Training data (not used by Ensemble, but required for API
#'   compatibility). Can be NULL or an empty data frame.
#' @param programs List of compiled modules to combine into an ensemble.
#'   This is the primary input for Ensemble compilation.
#' @param .llm Optional ellmer Chat object (not used by Ensemble)
#' @param ... Additional arguments (ignored)
#'
#' @return An EnsembleModule combining the provided programs
#'
#' @noRd
compile_ensemble <- function(
  teleprompter,
  program,
  trainset,
  programs = NULL,
  .llm = NULL,
  ...
) {
  # Ensemble requires a list of programs
  if (is.null(programs)) {
    # Maybe program is actually a list of modules?
    if (
      is.list(program) &&
        length(program) > 0 &&
        inherits(program[[1]], "Module")
    ) {
      programs <- program
    } else {
      cli::cli_abort(c(
        "Ensemble teleprompter requires a list of modules",
        "i" = "Pass modules via the {.arg programs} parameter",
        "i" = "Example: {.code compile(tp, programs = list(mod1, mod2, mod3))}"
      ))
    }
  }

  # Validate programs
  if (!is.list(programs) || length(programs) == 0) {
    cli::cli_abort("programs must be a non-empty list of modules")
  }

  for (i in seq_along(programs)) {
    if (!inherits(programs[[i]], "Module")) {
      cli::cli_abort(c(
        "All programs must be Module objects",
        "x" = "Element {i} is: {.cls {class(programs[[i]])[1]}}"
      ))
    }
  }

  # Apply size limit if specified
  if (!is.null(teleprompter@size)) {
    n_programs <- min(as.integer(teleprompter@size), length(programs))
    programs <- programs[seq_len(n_programs)]
  }

  # Get weights
  weights <- teleprompter@weights
  if (!is.null(weights) && length(weights) != length(programs)) {
    # Warn about weight mismatch
    if (length(weights) > length(programs)) {
      cli::cli_warn(c(
        "More weights ({length(weights)}) than programs ({length(programs)})",
        "i" = "Truncating weights to match program count"
      ))
      weights <- weights[seq_along(programs)]
    } else {
      cli::cli_abort(c(
        "Fewer weights ({length(weights)}) than programs ({length(programs)})",
        "i" = "Provide exactly one weight per program, or omit weights entirely"
      ))
    }
  }

  # Get reduce function
  reduce_fn <- teleprompter@reduce_fn %||% reduce_majority()

  # Create ensemble module
  ens <- EnsembleModule$new(
    modules = programs,
    reduce_fn = reduce_fn,
    weights = weights
  )

  # Mark as compiled
  ens$config$compiled <- TRUE
  ens$config$teleprompter <- "Ensemble"
  ens$config$n_modules <- length(programs)

  ens
}

#' Compile Programs into an Ensemble
#'
#' @description
#' Convenience function to create an ensemble from a list of modules
#' without explicitly creating a teleprompter.
#'
#' @param programs List of Module objects to combine
#' @param reduce_fn Function to combine outputs. Default is `reduce_majority()`.
#' @param weights Optional numeric vector of weights for each module.
#' @param size Optional integer to limit number of modules included.
#'
#' @return An EnsembleModule
#'
#' @export
#' @examples
#' \dontrun{
#' # Quick ensemble creation
#' ens <- compile_ensemble(list(mod1, mod2, mod3))
#'
#' # With weights from optimization
#' ens <- compile_ensemble(
#'   programs = candidates,
#'   reduce_fn = reduce_weighted_vote(),
#'   weights = validation_scores
#' )
#' }
ensemble_from_programs <- function(
  programs,
  reduce_fn = NULL,
  weights = NULL,
  size = NULL
) {
  tp <- Ensemble(
    reduce_fn = reduce_fn,
    size = size,
    weights = weights
  )

  compile_ensemble(
    teleprompter = tp,
    program = NULL,
    trainset = NULL,
    programs = programs
  )
}
