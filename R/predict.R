#' Predict S7 Class (Internal)
#'
#' @description
#' Internal S7 class for prediction modules. Users should use the `module()`
#' function with `type = "predict"` instead of directly constructing Predict objects.
#'
#' @seealso [module()] for the user-facing function
#' @keywords internal
#' @export
Predict <- S7::new_class(
  "Predict",
  properties = list(
    signature = S7::new_property(
      S7::class_any,
      validator = function(value) {
        # We check the class name since Signature might not be loaded yet
        if (!inherits(value, "dsprrr::Signature")) {
          return("signature must be a Signature object")
        }
        NULL
      }
    ),
    template = S7::new_property(
      S7::class_character,
      default = "",
      validator = function(value) {
        if (!is.character(value) || length(value) != 1) {
          return("template must be a single character string")
        }
        NULL
      }
    ),
    demos = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        if (!is.list(value)) {
          return("demos must be a list")
        }
        NULL
      }
    ),
    config = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        if (!is.list(value)) {
          return("config must be a list")
        }
        NULL
      }
    )
  ),
  validator = function(self) {
    # Template validation would go here
    # For now, we'll skip complex template validation
    NULL
  }
)

#' Print method for Predict
#' @noRd
print_predict <- function(x, ...) {
  cli::cli_h2("Predict Module")

  cli::cli_h3("Signature")
  print(x@signature)

  if (nchar(x@template) > 0) {
    cli::cli_h3("Template")
    cli::cli_code(x@template)
  }

  if (length(x@demos) > 0) {
    cli::cli_h3("Demos")
    cli::cli_text("{length(x@demos)} demonstration(s) loaded")
  }

  if (!is.null(x@config$compiled) && x@config$compiled) {
    cli::cli_h3("Compilation Status")
    cli::cli_text("✓ Compiled with {x@config$teleprompter}")
    if (!is.null(x@config$best_score)) {
      cli::cli_text("  Best score: {round(x@config$best_score, 3)}")
    }
  }

  invisible(x)
}

#' Reset Copy Method for Modules
#'
#' @description
#' Create a fresh copy of a module without demonstrations or compilation state.
#' This is useful when you want to start optimization from scratch.
#'
#' @param module A Predict module
#' @return A new module with reset state
#' @export
reset_copy <- S7::new_generic("reset_copy", "module")

#' Reset copy method for Predict
#' @noRd
reset_copy_predict <- function(module) {
  Predict(
    signature = module@signature,
    template = module@template,
    demos = list(),
    config = list()
  )
}

#' Deep Copy Method for Modules
#'
#' @description
#' Create a complete copy of a module including all state.
#' This is useful when you want to preserve the current state while
#' making modifications.
#'
#' @param module A Predict module
#' @return A new module with copied state
#' @export
deepcopy <- S7::new_generic("deepcopy", "module")

#' Deep copy method for Predict
#' @noRd
deepcopy_predict <- function(module) {
  # Create new lists to avoid reference issues
  new_demos <- if (length(module@demos) > 0) {
    lapply(module@demos, function(x) x)  # Force copy
  } else {
    list()
  }

  new_config <- if (length(module@config) > 0) {
    lapply(module@config, function(x) x)  # Force copy
  } else {
    list()
  }

  # Copy the signature
  new_signature <- Signature(
    inputs = module@signature@inputs,
    output_type = module@signature@output_type,
    instructions = module@signature@instructions
  )

  Predict(
    signature = new_signature,
    template = module@template,
    demos = new_demos,
    config = new_config
  )
}

#' Check if Module is Compiled
#'
#' @description
#' Check whether a module has been compiled/optimized.
#'
#' @param module A Predict module
#' @return Logical indicating if the module is compiled
#' @export
is_compiled <- function(module) {
  if (!inherits(module, "dsprrr::Predict")) {
    cli::cli_abort("module must be a Predict object")
  }

  !is.null(module@config$compiled) && module@config$compiled
}

#' Convert Module to Function
#'
#' @description
#' Convert a module to a function that can be called directly.
#' This provides a more natural interface for module execution.
#'
#' @param module A DSPrrr module
#' @param .llm Default LLM to use (optional)
#'
#' @return A function that executes the module
#' @export
#' @examples
#' # Create a module and convert to function
#' classifier_module <- signature("text -> sentiment") |>
#'   module(type = "predict")
#'
#' # Convert to function
#' classifier <- as_function(classifier_module)
#'
#' # Call as function
#' result <- classifier(text = "Great!")
as_function <- S7::new_generic("as_function", "module")

#' Convert Predict module to function
#' @noRd
S7::method(as_function, Predict) <- function(module, .llm = NULL) {
  function(...) {
    run(module, ..., .llm = .llm)
  }
}