#' Package startup functions
#' @noRd

.onLoad <- function(libname, pkgname) {
  # Register S7 methods
  S7::method(run, Predict) <- run_predict
  S7::method(print, Signature) <- print_signature
  S7::method(print, Predict) <- print_predict

  # Register compile methods
  S7::method(compile, list(LabeledFewShot, S7::class_any)) <- function(teleprompter, program, trainset, ...) {
    compile_labeled(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(GridSearchTeleprompter, S7::class_any)) <- function(teleprompter, program, trainset, ...) {
    compile_gridsearch(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(Teleprompter, S7::class_any)) <- function(teleprompter, program, trainset, ...) {
    compile_default(teleprompter, program, trainset, ...)
  }

  # Register module state management methods
  S7::method(reset_copy, Predict) <- reset_copy_predict
  S7::method(deepcopy, Predict) <- deepcopy_predict

  invisible()
}