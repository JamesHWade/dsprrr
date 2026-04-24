# BetterTogether Teleprompter
#
# DSPy-style meta-optimizer for chaining arbitrary teleprompters.

#' BetterTogether Teleprompter
#'
#' @include teleprompter.R optimizer-core.R
#'
#' @description
#' A meta-teleprompter that runs multiple optimization strategies in sequence.
#' This mirrors DSPy's `BetterTogether` optimizer surface for composing prompt
#' optimizers and future weight optimizers with a strategy string such as
#' `"p -> g -> p"`.
#'
#' `BetterTogether()` accepts optimizers either through the named `optimizers`
#' list or as named arguments in `...`. Strategy steps refer to those names.
#' Each intermediate program is evaluated on `valset` when available; the best
#' scored program is returned. Without a validation set, the latest program in
#' the strategy is returned.
#'
#' @param metric Metric function used to score candidate programs.
#' @param optimizers Named list of [Teleprompter] objects. If omitted, dsprrr
#'   defaults to `p = BootstrapFewShotWithRandomSearch(metric = metric)`.
#' @param ... Named [Teleprompter] objects, used as strategy keys. These are
#'   combined with `optimizers`.
#' @param metric_threshold Minimum score required to be considered successful.
#' @param max_errors Maximum number of errors allowed during evaluation.
#' @param default_strategy Strategy to use when `compile()` does not receive
#'   `strategy`. Defaults to `"p"`.
#' @param valset_ratio Fraction of `trainset` to hold out as validation when
#'   `valset` is not supplied. Set to `0` to skip validation.
#' @param shuffle_trainset_between_steps Whether to shuffle training rows before
#'   each optimizer step.
#' @param seed Optional random seed for reproducible splitting and shuffling.
#' @param verbose Whether to print progress messages.
#'
#' @export
#' @examples
#' \dontrun{
#' metric <- metric_exact_match(field = "answer")
#'
#' tp <- BetterTogether(
#'   metric = metric,
#'   optimizers = list(
#'     p = BootstrapFewShotWithRandomSearch(metric = metric),
#'     g = GEPA(metric = metric, population_size = 4L, generations = 2L)
#'   ),
#'   default_strategy = "p -> g -> p"
#' )
#'
#' compiled <- compile(tp, qa_module, trainset, valset = valset, .llm = llm)
#' compiled$config$optimizer$candidate_programs
#' }
BetterTogether <- S7::new_class(
  "BetterTogether",
  parent = Teleprompter,
  properties = list(
    optimizers = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        if (!is.list(value)) {
          return("optimizers must be a named list")
        }
        if (length(value) == 0) {
          return(NULL)
        }
        if (is.null(names(value)) || any(!nzchar(names(value)))) {
          return("optimizers must be named")
        }
        bad <- !vapply(value, is_teleprompter, logical(1))
        if (any(bad)) {
          return("all optimizers must be Teleprompter objects")
        }
        NULL
      }
    ),
    default_strategy = S7::new_property(
      S7::class_character,
      default = "p",
      validator = function(value) {
        if (length(value) != 1 || !nzchar(trimws(value))) {
          return("default_strategy must be a non-empty string")
        }
        NULL
      }
    ),
    valset_ratio = S7::new_property(
      S7::class_numeric,
      default = 0.1,
      validator = function(value) {
        if (length(value) != 1 || is.na(value) || value < 0 || value >= 1) {
          return("valset_ratio must be a single number in [0, 1)")
        }
        NULL
      }
    ),
    shuffle_trainset_between_steps = S7::new_property(
      S7::class_logical,
      default = TRUE,
      validator = function(value) {
        if (length(value) != 1 || is.na(value)) {
          return("shuffle_trainset_between_steps must be TRUE or FALSE")
        }
        NULL
      }
    ),
    seed = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && (!is.numeric(value) || length(value) != 1)) {
          return("seed must be a single numeric value or NULL")
        }
        NULL
      }
    ),
    verbose = S7::new_property(
      S7::class_logical,
      default = TRUE,
      validator = function(value) {
        if (length(value) != 1 || is.na(value)) {
          return("verbose must be TRUE or FALSE")
        }
        NULL
      }
    )
  ),
  constructor = function(
    metric = NULL,
    optimizers = list(),
    ...,
    metric_threshold = NULL,
    max_errors = 5L,
    default_strategy = "p",
    valset_ratio = 0.1,
    shuffle_trainset_between_steps = TRUE,
    seed = NULL,
    verbose = TRUE
  ) {
    dots <- list(...)
    if (length(dots) > 0) {
      if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
        cli::cli_abort(
          "Optimizer arguments in {.arg ...} must be named strategy keys"
        )
      }
      optimizers <- c(optimizers, dots)
    }

    S7::new_object(
      Teleprompter(
        metric = metric,
        metric_threshold = metric_threshold,
        max_errors = as.integer(max_errors)
      ),
      optimizers = optimizers,
      default_strategy = default_strategy,
      valset_ratio = valset_ratio,
      shuffle_trainset_between_steps = shuffle_trainset_between_steps,
      seed = seed,
      verbose = verbose
    )
  }
)

#' Compile method for BetterTogether
#' @noRd
compile_better_together <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  strategy = NULL,
  optimizer_compile_args = list(),
  valset_ratio = teleprompter@valset_ratio,
  shuffle_trainset_between_steps = teleprompter@shuffle_trainset_between_steps,
  seed = teleprompter@seed,
  ...
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("BetterTogether currently only supports Module objects")
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("{.arg trainset} must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_abort("trainset cannot be empty")
  }

  if (is.null(teleprompter@metric)) {
    cli::cli_abort("BetterTogether requires a metric function")
  }

  if (!is.null(valset) && !is.data.frame(valset)) {
    cli::cli_abort("{.arg valset} must be a data frame or NULL")
  }

  if (!is.list(optimizer_compile_args)) {
    cli::cli_abort("{.arg optimizer_compile_args} must be a named list")
  }

  if (length(optimizer_compile_args) > 0) {
    if (is.null(names(optimizer_compile_args)) ||
      any(!nzchar(names(optimizer_compile_args)))) {
      cli::cli_abort("{.arg optimizer_compile_args} must be named")
    }
  }

  optimizers <- better_together_optimizers(teleprompter)
  strategy <- strategy %||% teleprompter@default_strategy
  parsed_strategy <- parse_better_together_strategy(strategy, optimizers)

  bad_arg_keys <- setdiff(names(optimizer_compile_args), names(optimizers))
  if (length(bad_arg_keys) > 0) {
    cli::cli_abort(c(
      "{.arg optimizer_compile_args} contains unknown optimizer keys",
      "x" = "Unknown: {.field {bad_arg_keys}}",
      "i" = "Valid keys: {.field {names(optimizers)}}"
    ))
  }

  split <- better_together_train_val_split(
    trainset = trainset,
    valset = valset,
    valset_ratio = valset_ratio,
    seed = seed
  )
  working_trainset <- split$trainset
  working_valset <- split$valset

  if (nrow(working_trainset) == 0) {
    cli::cli_abort(c(
      "No training rows remain after validation split",
      "i" = "Decrease {.arg valset_ratio} or provide a separate {.arg valset}"
    ))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  current <- copy_module(program)
  candidates <- list()
  compilation_error <- FALSE

  baseline_score <- better_together_score(
    current,
    working_valset,
    teleprompter@metric,
    .llm = .llm,
    max_errors = teleprompter@max_errors,
    verbose = FALSE
  )
  candidates <- append(candidates, list(better_together_candidate(
    program = current,
    strategy = "",
    step = 0L,
    score = baseline_score
  )))

  if (teleprompter@verbose) {
    display_score <- format_better_together_score(baseline_score)
    cli::cli_alert_info("BetterTogether baseline score: {display_score}")
  }

  for (i in seq_along(parsed_strategy)) {
    key <- parsed_strategy[[i]]
    optimizer <- optimizers[[key]]
    current_strategy <- paste(parsed_strategy[seq_len(i)], collapse = " -> ")

    if (isTRUE(shuffle_trainset_between_steps) && nrow(working_trainset) > 1) {
      working_trainset <- working_trainset[
        sample.int(nrow(working_trainset)),
        ,
        drop = FALSE
      ]
    }

    if (teleprompter@verbose) {
      cli::cli_alert_info(
        "BetterTogether step {i}/{length(parsed_strategy)}: {.field {key}} ({class(optimizer)[1]})"
      )
    }

    step_args <- optimizer_compile_args[[key]] %||% list()

    current <- tryCatch(
      {
        better_together_compile_step(
          optimizer = optimizer,
          program = current,
          trainset = working_trainset,
          valset = working_valset,
          .llm = .llm,
          step_args = step_args
        )
      },
      error = function(e) {
        compilation_error <<- TRUE
        cli::cli_warn(c(
          "BetterTogether step failed; returning best program found so far",
          "x" = conditionMessage(e),
          "i" = "Failed strategy: {.val {current_strategy}}"
        ))
        NULL
      }
    )

    if (is.null(current)) {
      break
    }

    score <- better_together_score(
      current,
      working_valset,
      teleprompter@metric,
      .llm = .llm,
      max_errors = teleprompter@max_errors,
      verbose = FALSE
    )
    candidates <- append(candidates, list(better_together_candidate(
      program = current,
      strategy = current_strategy,
      step = as.integer(i),
      score = score
    )))

    if (teleprompter@verbose) {
      display_score <- format_better_together_score(score)
      cli::cli_alert_info(
        "BetterTogether strategy {.val {current_strategy}} score: {display_score}"
      )
    }
  }

  sorted_candidates <- sort_better_together_candidates(candidates)
  has_valset <- !is.null(working_valset) && nrow(working_valset) > 0
  best_candidate <- if (has_valset) {
    sorted_candidates[[1]]
  } else {
    candidates[[length(candidates)]]
  }

  best_program <- copy_module(best_candidate$program)
  best_program$state$compiled <- TRUE
  best_program$state$best_score <- best_candidate$score
  best_program$state$best_params <- list(strategy = best_candidate$strategy)
  best_program$config$compiled <- TRUE
  best_program$config$teleprompter <- "BetterTogether"
  best_program$config$best_score <- best_candidate$score
  best_program$config$best_strategy <- best_candidate$strategy
  best_program$config$optimizer <- list(
    name = "BetterTogether",
    strategy = strategy,
    candidate_programs = better_together_candidates_tbl(sorted_candidates),
    flag_compilation_error_occurred = compilation_error
  )

  best_program
}

is_teleprompter <- function(x) {
  inherits(x, "dsprrr::Teleprompter") || inherits(x, "Teleprompter")
}

better_together_optimizers <- function(teleprompter) {
  optimizers <- teleprompter@optimizers
  if (length(optimizers) > 0) {
    return(optimizers)
  }

  if (is.null(teleprompter@metric)) {
    cli::cli_abort(
      "Default BetterTogether optimizers require {.arg metric}"
    )
  }

  list(
    p = BootstrapFewShotWithRandomSearch(metric = teleprompter@metric)
  )
}

parse_better_together_strategy <- function(strategy, optimizers) {
  if (!is.character(strategy) || length(strategy) != 1 || !nzchar(trimws(strategy))) {
    cli::cli_abort("{.arg strategy} must be a non-empty string")
  }

  parts <- trimws(strsplit(strategy, "->", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    cli::cli_abort("{.arg strategy} must include at least one optimizer key")
  }

  invalid <- setdiff(parts, names(optimizers))
  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "{.arg strategy} contains unknown optimizer keys",
      "x" = "Unknown: {.field {invalid}}",
      "i" = "Valid keys: {.field {names(optimizers)}}"
    ))
  }

  parts
}

better_together_train_val_split <- function(
  trainset,
  valset = NULL,
  valset_ratio = 0.1,
  seed = NULL
) {
  if (length(valset_ratio) != 1 || is.na(valset_ratio) ||
    valset_ratio < 0 || valset_ratio >= 1) {
    cli::cli_abort("{.arg valset_ratio} must be in [0, 1)")
  }

  if (!is.null(valset)) {
    return(list(trainset = trainset, valset = valset))
  }

  if (valset_ratio == 0 || nrow(trainset) < 2) {
    return(list(trainset = trainset, valset = NULL))
  }

  n_val <- floor(nrow(trainset) * valset_ratio)
  if (n_val < 1) {
    return(list(trainset = trainset, valset = NULL))
  }

  indices <- seq_len(nrow(trainset))
  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }
    on.exit({
      if (is.null(old_seed)) {
        rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", old_seed, envir = globalenv())
      }
    }, add = TRUE)
    set.seed(seed)
  }

  val_idx <- sample(indices, n_val)
  list(
    trainset = trainset[setdiff(indices, val_idx), , drop = FALSE],
    valset = trainset[val_idx, , drop = FALSE]
  )
}

better_together_compile_step <- function(
  optimizer,
  program,
  trainset,
  valset,
  .llm,
  step_args
) {
  if (!is.list(step_args)) {
    cli::cli_abort("Each optimizer_compile_args entry must be a list")
  }

  blocked <- intersect(names(step_args), c("teleprompter", "program", "student"))
  if (length(blocked) > 0) {
    cli::cli_abort(c(
      "Optimizer compile arguments cannot override the current program",
      "x" = "Blocked arguments: {.field {blocked}}"
    ))
  }

  call_args <- list(
    teleprompter = optimizer,
    program = program,
    trainset = trainset,
    valset = valset,
    .llm = .llm
  )

  for (name in names(step_args)) {
    call_args[[name]] <- step_args[[name]]
  }

  do.call(compile, call_args)
}

better_together_score <- function(
  program,
  valset,
  metric,
  .llm = NULL,
  max_errors = 5L,
  verbose = FALSE
) {
  if (is.null(valset) || nrow(valset) == 0) {
    return(NA_real_)
  }

  result <- eval_program(
    program,
    dataset = valset,
    metric = metric,
    .llm = .llm,
    control = optimizer_control(
      max_errors = max_errors,
      progress = verbose
    )
  )

  result@mean_score
}

better_together_candidate <- function(program, strategy, step, score) {
  list(
    program = copy_module(program),
    strategy = strategy,
    step = step,
    score = score
  )
}

sort_better_together_candidates <- function(candidates) {
  if (length(candidates) <= 1) {
    return(candidates)
  }

  order_idx <- order(
    vapply(candidates, function(x) {
      score <- x$score
      if (is.na(score)) -Inf else score
    }, numeric(1)),
    seq_along(candidates) * -1,
    decreasing = TRUE
  )
  candidates[order_idx]
}

better_together_candidates_tbl <- function(candidates) {
  tibble::tibble(
    strategy = vapply(candidates, `[[`, character(1), "strategy"),
    step = vapply(candidates, `[[`, integer(1), "step"),
    score = vapply(candidates, `[[`, numeric(1), "score"),
    program = lapply(candidates, `[[`, "program")
  )
}

format_better_together_score <- function(score) {
  if (is.na(score)) {
    "NA"
  } else {
    format(round(score, 4), nsmall = 4)
  }
}

#' Print method for BetterTogether
#' @param x A BetterTogether object
#' @param ... Additional arguments
#' @export
print.BetterTogether <- function(x, ...) {
  cli::cli_h3("BetterTogether Teleprompter")
  cli::cli_text("{.field Default strategy}: {.val {x@default_strategy}}")
  cli::cli_text("{.field Validation split}: {x@valset_ratio}")
  optimizers <- if (length(x@optimizers) > 0) {
    x@optimizers
  } else if (!is.null(x@metric)) {
    better_together_optimizers(x)
  } else {
    list()
  }
  cli::cli_text("{.field Optimizers}: {.field {names(optimizers)}}")
  invisible(x)
}

S7::method(print, BetterTogether) <- print.BetterTogether
