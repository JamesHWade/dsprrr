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

  S7::method(compile, list(BootstrapFewShot, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_bootstrap(teleprompter, program, trainset, ...)
  }

  S7::method(
    compile,
    list(BootstrapFewShotWithRandomSearch, S7::class_any)
  ) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_bootstrap_rs(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(Teleprompter, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_default(teleprompter, program, trainset, ...)
  }

  # Register parsnip engine if parsnip is available
  if (rlang::is_installed("parsnip")) {
    setHook(
      packageEvent("parsnip", "onLoad"),
      function(...) register_dsprrr_engine()
    )
    if ("parsnip" %in% loadedNamespaces()) {
      register_dsprrr_engine()
    }
  }

  invisible()
}
