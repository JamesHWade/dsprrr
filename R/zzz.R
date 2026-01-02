#' Package startup functions
#' @noRd

.onLoad <- function(libname, pkgname) {
  # Register S7 methods for Signature (still S7)
  S7::method(print, Signature) <- print_signature

  # Register print method for MIPROv2
  S7::method(print, MIPROv2) <- print_miprov2

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

  S7::method(compile, list(KNNFewShot, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_knn(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(SIMBA, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_simba(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(GEPA, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_gepa(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(MIPROv2, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_mipro(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(COPRO, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    compile_copro(teleprompter, program, trainset, ...)
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
