#' Package startup functions
#' @noRd

.onLoad <- function(libname, pkgname) {
  # Register S7 methods for Signature (still S7)
  S7::method(print, Signature) <- print_signature

  # Register compile methods for teleprompters (still S7)
  S7::method(compile, list(LabeledFewShot, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_labeled(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(GridSearchTeleprompter, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_gridsearch(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(Teleprompter, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_default(teleprompter, program, trainset, ...)
  }

  invisible()
}
