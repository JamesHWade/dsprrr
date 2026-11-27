#' Package startup functions
#' @noRd

.onLoad <- function(libname, pkgname) {
  # Print methods for S7 classes are registered here rather than at top level:
  # `S7::method(print, X) <- f` assigns `print` in the calling environment, and
  # a `print` binding in the namespace sends every S3method(print, ...) entry
  # to the namespace's own methods table, where base::print() never looks.
  S7::method(print, Signature) <- print_signature
  S7::method(print, MIPROv2) <- print_miprov2
  S7::method(print, Assertion) <- print_assertion
  S7::method(print, AssertionSet) <- print_assertion_set
  S7::method(print, EvalResult) <- print_eval_result
  S7::method(print, Trial) <- print_trial
  S7::method(print, BetterTogether) <- print_better_together
  S7::method(
    print,
    BootstrapFewShotWithRandomSearch
  ) <- print_bootstrap_few_shot_random_search
  S7::method(print, BootstrapFewShot) <- print_bootstrap_few_shot
  S7::method(print, COPRO) <- print_copro
  S7::method(print, GEPA) <- print_gepa
  S7::method(print, AutoResearch) <- print_auto_research
  S7::method(print, MetaHarness) <- print_meta_harness
  S7::method(print, Omni) <- print_omni
  S7::method(print, SIMBA) <- print_simba

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

  compilers <- list(
    list(LabeledFewShot, "compile_labeled"),
    list(GridSearchTeleprompter, "compile_gridsearch"),
    list(BootstrapFewShot, "compile_bootstrap"),
    list(BootstrapFewShotWithRandomSearch, "compile_bootstrap_rs"),
    list(BetterTogether, "compile_better_together"),
    list(Omni, "compile_omni"),
    list(AutoResearch, "compile_autoresearch"),
    list(MetaHarness, "compile_meta_harness"),
    list(KNNFewShot, "compile_knn"),
    list(SIMBA, "compile_simba"),
    list(GEPA, "compile_gepa"),
    list(MIPROv2, "compile_mipro"),
    list(COPRO, "compile_copro"),
    list(ReAnchor, "compile_reanchor"),
    list(Teleprompter, "compile_default")
  )
  for (entry in compilers) {
    cls <- entry[[1L]]
    fn_sym <- as.name(entry[[2L]])
    S7::method(compile, list(S7::class_any, cls)) <- rlang::new_function(
      rlang::pairlist2(
        program = ,
        teleprompter = ,
        trainset = ,
        ... =
      ),
      bquote({
        abort_if_fn_module(program)
        compile_with_trace_context(
          .(fn_sym),
          program,
          teleprompter,
          trainset,
          ...
        )
      }),
      env = rlang::ns_env("dsprrr")
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
