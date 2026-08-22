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
  S7::method(
    compile,
    list(S7::class_any, S7::class_any)
  ) <- function(program, teleprompter, trainset, ...) {
    validate_compile_inputs(program, teleprompter, trainset, list(...))
    cli::cli_abort(
      "No compiler is registered for {.cls {class(teleprompter)[1]}}"
    )
  }

  S7::method(compile, list(S7::class_any, LabeledFewShot)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_labeled,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, GridSearchTeleprompter)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_gridsearch,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, BootstrapFewShot)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_bootstrap,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(
    compile,
    list(S7::class_any, BootstrapFewShotWithRandomSearch)
  ) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_bootstrap_rs,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, BetterTogether)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_better_together,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, Omni)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_omni,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, AutoResearch)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_autoresearch,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, MetaHarness)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_meta_harness,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, KNNFewShot)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_knn,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, SIMBA)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_simba,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, GEPA)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_gepa,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, MIPROv2)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_mipro,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, COPRO)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_copro,
      program,
      teleprompter,
      trainset,
      ...
    )
  }

  S7::method(compile, list(S7::class_any, Teleprompter)) <- function(
    program,
    teleprompter,
    trainset,
    ...
  ) {
    abort_if_fn_module(program)
    compile_with_trace_context(
      compile_default,
      program,
      teleprompter,
      trainset,
      ...
    )
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
