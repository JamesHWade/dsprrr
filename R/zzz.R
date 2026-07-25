#' Package startup functions
#' @noRd

.onLoad <- function(libname, pkgname) {
  registerS3method("print", "dsprrr_batch_result", print.dsprrr_batch_result)
  registerS3method("print", "dsprrr_trace_summary", print.dsprrr_trace_summary)

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
    abort_if_fn_module(program)
    compile_labeled(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(GridSearchTeleprompter, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_gridsearch(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(BootstrapFewShot, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
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
    abort_if_fn_module(program)
    compile_bootstrap_rs(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(BetterTogether, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_better_together(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(Omni, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_omni(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(AutoResearch, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_autoresearch(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(MetaHarness, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_meta_harness(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(KNNFewShot, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_knn(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(SIMBA, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_simba(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(GEPA, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_gepa(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(MIPROv2, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_mipro(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(COPRO, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_copro(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(Ensemble, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_ensemble(teleprompter, program, trainset, ...)
  }

  S7::method(compile, list(Teleprompter, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_default(teleprompter, program, trainset, ...)
  }

  # Register parsnip engine if parsnip is available. Wrap in tryCatch so a
  # parsnip API change cannot block the package from loading -- dsprrr modules
  # are usable without parsnip integration.
  if (rlang::is_installed("parsnip")) {
    setHook(
      packageEvent("parsnip", "onLoad"),
      function(...) try_register_dsprrr_engine()
    )
    if ("parsnip" %in% loadedNamespaces()) {
      try_register_dsprrr_engine()
    }
  }

  invisible()
}

#' Register the parsnip engine, downgrading failures to a startup message
#' @noRd
try_register_dsprrr_engine <- function() {
  tryCatch(
    register_dsprrr_engine(),
    error = function(e) {
      packageStartupMessage(
        "dsprrr: parsnip engine registration failed: ",
        conditionMessage(e),
        "\n  dsprrr will load without parsnip integration."
      )
    }
  )
}
