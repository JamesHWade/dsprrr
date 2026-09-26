# BetterTogether Teleprompter
#
# DSPy-style meta-optimizer for chaining arbitrary teleprompters.

#' BetterTogether: run several optimizers in sequence
#'
#' @include teleprompter.R optimizer-core.R
#'
#' @description
#' `BetterTogether()` chains optimizers. A strategy string such as
#' `"p -> g -> p"` runs the optimizer named `p`, then `g` on its result, then
#' `p` again. Every intermediate program is scored on a validation set and the
#' best one is returned. It mirrors DSPy's `BetterTogether`.
#'
#' @details
#' Name the optimizers in `optimizers` or as named arguments in `...`; the
#' strategy refers to those names. Without any, `BetterTogether()` uses
#' `p = BootstrapFewShotWithRandomSearch(metric = metric)`.
#'
#' [compile()] accepts extra arguments for this optimizer: `strategy`
#' (overrides `default_strategy`), `optimizer_compile_args` (a list, named by
#' optimizer, of further arguments for that optimizer's [compile()] call), and
#' `valset_ratio`, `shuffle_trainset_between_steps` and `seed`, which override
#' the values stored here.
#'
#' The validation set is `valset` when given. Otherwise `floor(valset_ratio *
#' nrow(trainset))` rows are held out, which is none for fewer than 10 rows at
#' the default ratio. Each step receives the remaining training rows and the
#' validation set. The original program is scored as well, so the result can
#' be the unchanged program. Without a validation set, the last program in the
#' strategy is returned. A step that fails raises a warning and ends the run
#' with the best program so far; the default `p` optimizer fails this way when
#' there is no validation set.
#'
#' The scored candidates are stored in
#' `optimization_result(compiled)$extensions$better_together$candidate_programs`.
#'
#' @param metric A metric function (required) used to score every candidate
#'   on the validation set, such as `metric_exact_match(field = "answer")`.
#' @param optimizers Named list of optimizer objects, such as
#'   `list(p = BootstrapFewShotWithRandomSearch(metric = metric))`.
#' @param ... Further named optimizer objects, combined with `optimizers`.
#' @param metric_threshold Accepted for consistency with the other optimizers
#'   (see [Teleprompter()]); `BetterTogether()` does not use it.
#' @param max_errors Does not stop the run; set `max_errors` on each wrapped
#'   optimizer instead.
#' @param default_strategy Strategy used when [compile()] gets no `strategy`
#'   (default `"p"`): optimizer names joined by `->`.
#' @param valset_ratio Share of `trainset` held out for validation when no
#'   `valset` is given (default `0.1`). `0` skips validation.
#' @param shuffle_trainset_between_steps Whether to shuffle the training rows
#'   before each step (default `TRUE`).
#' @param seed Optional seed for the validation split and the shuffles.
#' @param verbose Whether to print progress messages (default `TRUE`).
#'
#' @return A `BetterTogether` object to pass to [compile()].
#' @family teleprompters
#' @export
#' @examples
#' metric <- metric_exact_match(field = "answer")
#'
#' tp <- BetterTogether(
#'   metric = metric,
#'   p = BootstrapFewShotWithRandomSearch(metric = metric),
#'   g = GEPA(metric = metric, population_size = 4L, generations = 2L),
#'   default_strategy = "p -> g -> p"
#' )
#' tp
#'
#' \dontrun{
#' qa <- module(signature("question -> answer"))
#' trainset <- data.frame(
#'   question = c("Capital of France?", "Capital of Peru?", "Capital of Chad?"),
#'   answer = c("Paris", "Lima", "N'Djamena")
#' )
#' valset <- data.frame(
#'   question = c("Capital of Japan?", "Capital of Kenya?"),
#'   answer = c("Tokyo", "Nairobi")
#' )
#' llm <- ellmer::chat_openai(model = "gpt-6-luna")
#'
#' compiled <- compile(qa, tp, trainset, valset = valset, .llm = llm)
#' optimization_result(compiled)$extensions$better_together$candidate_programs
#'
#' # Try another strategy without building a new optimizer
#' compiled_g <- compile(
#'   qa,
#'   tp,
#'   trainset,
#'   valset = valset,
#'   .llm = llm,
#'   strategy = "g"
#' )
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
        if (is.null(names(value)) || !all(nzchar(names(value)))) {
          return("optimizers must be named")
        }
        if (anyDuplicated(names(value))) {
          return("optimizer names must be unique")
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
        error <- validate_better_together_seed(value, arg = "seed")
        if (!is.null(error)) {
          return(error)
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
      if (is.null(names(dots)) || !all(nzchar(names(dots)))) {
        cli::cli_abort(
          "Optimizer arguments in {.arg ...} must be named strategy keys"
        )
      }
      if (anyDuplicated(names(dots))) {
        cli::cli_abort("Optimizer names in {.arg ...} must be unique")
      }
    }

    if (length(optimizers) > 0 && anyDuplicated(names(optimizers))) {
      cli::cli_abort("Optimizer names in {.arg optimizers} must be unique")
    }

    if (length(dots) > 0) {
      optimizers[names(dots)] <- dots
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
    if (
      is.null(names(optimizer_compile_args)) ||
        !all(nzchar(names(optimizer_compile_args)))
    ) {
      cli::cli_abort("{.arg optimizer_compile_args} must be named")
    }
  }

  seed_error <- validate_better_together_seed(seed, arg = "seed")
  if (!is.null(seed_error)) {
    cli::cli_abort(seed_error)
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
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }
    set.seed(seed)
    on.exit(
      {
        if (is.null(old_seed)) {
          rm(".Random.seed", envir = globalenv())
        } else {
          assign(".Random.seed", old_seed, envir = globalenv())
        }
      },
      add = TRUE
    )
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
  candidates <- append(
    candidates,
    list(better_together_candidate(
      program = current,
      strategy = "",
      step = 0L,
      score = baseline_score
    ))
  )

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
    candidates <- append(
      candidates,
      list(better_together_candidate(
        program = current,
        strategy = current_strategy,
        step = as.integer(i),
        score = score
      ))
    )

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

  candidate_programs <- better_together_candidates_tbl(sorted_candidates)
  candidate_trials <- candidate_programs
  candidate_trials$program_config <- lapply(
    candidate_programs$program,
    function(candidate) candidate$config
  )
  candidate_trials$program <- NULL
  best_trial <- match(
    best_candidate$strategy,
    candidate_trials$strategy
  )
  best_program <- copy_module(best_candidate$program)
  best_program$config$best_score <- best_candidate$score
  best_program$config$best_strategy <- best_candidate$strategy
  record_optimization_result(
    best_program,
    optimizer = "BetterTogether",
    baseline_score = baseline_score,
    best_score = best_candidate$score,
    best_trial = best_trial,
    best_params = list(strategy = best_candidate$strategy),
    trials = candidate_trials,
    lineage = list(selected_strategy = best_candidate$strategy),
    stop_reason = "completed",
    extensions = list(
      strategy = strategy,
      candidate_programs = candidate_trials,
      flag_compilation_error_occurred = compilation_error
    )
  )

  best_program
}

is_teleprompter <- function(x) {
  inherits(x, "dsprrr::Teleprompter") || inherits(x, "Teleprompter")
}

validate_better_together_seed <- function(seed, arg = "seed") {
  if (is.null(seed)) {
    return(NULL)
  }
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
    return(paste0(arg, " must be a single non-missing numeric value or NULL"))
  }
  NULL
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
  if (
    !is.character(strategy) ||
      length(strategy) != 1 ||
      !nzchar(trimws(strategy))
  ) {
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
  if (
    length(valset_ratio) != 1 ||
      is.na(valset_ratio) ||
      valset_ratio < 0 ||
      valset_ratio >= 1
  ) {
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
    on.exit(
      {
        if (is.null(old_seed)) {
          rm(".Random.seed", envir = globalenv())
        } else {
          assign(".Random.seed", old_seed, envir = globalenv())
        }
      },
      add = TRUE
    )
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

  blocked <- intersect(
    names(step_args),
    c("teleprompter", "program", "student", "trainset", "valset", ".llm")
  )
  if (length(blocked) > 0) {
    cli::cli_abort(c(
      "Optimizer compile arguments cannot override BetterTogether core inputs",
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
    vapply(
      candidates,
      function(x) {
        score <- x$score
        if (is.na(score)) -Inf else score
      },
      numeric(1)
    ),
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

# Print a BetterTogether object through its S7 method.
print_better_together <- function(x, ...) {
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
