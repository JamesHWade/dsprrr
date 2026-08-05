# GEPA support for declarative Flex components.
#
# This file deliberately stays separate from the GEPA-lite generation loop. It
# supplies a typed candidate protocol that compile_gepa() can opt into when a
# program contains Flex leaves, without making arbitrary source executable.

gepa_flex_paths <- function(program, mutable_only = TRUE) {
  module_graph_check_program(program)
  if (
    !is.logical(mutable_only) ||
      length(mutable_only) != 1L ||
      is.na(mutable_only)
  ) {
    cli::cli_abort(
      "{.arg mutable_only} must be TRUE or FALSE",
      class = "dsprrr_gepa_component_error"
    )
  }

  modules <- if (isTRUE(mutable_only)) {
    named_parameters(program, include_root = TRUE, boundaries = "respect")
  } else {
    named_modules(
      program,
      include_root = TRUE,
      aliases = FALSE,
      boundaries = "cross"
    )
  }
  names(Filter(function(module) inherits(module, "FlexModule"), modules))
}

gepa_program_component_specs <- function(program) {
  module_graph_check_program(program)
  parameters <- named_parameters(
    program,
    include_root = TRUE,
    boundaries = "respect"
  )
  components <- list()

  for (path in names(parameters)) {
    parameter <- parameters[[path]]
    if (inherits(parameter, "PredictModule")) {
      id <- gepa_component_id("instructions", path)
      components[[id]] <- list(
        id = id,
        kind = "instructions",
        path = path,
        value = parameter$signature@instructions
      )
    }
    if (inherits(parameter, "FlexModule")) {
      id <- gepa_component_id("module_src", path)
      components[[id]] <- list(
        id = id,
        kind = "module_src",
        path = path,
        value = parameter$module_src
      )
    }
  }

  components
}

gepa_component_id <- function(kind, path) paste0(kind, "::", path)

gepa_component_candidate <- function(program) {
  structure(
    list(
      components = gepa_program_component_specs(program),
      valid = TRUE,
      failure = NULL,
      history = list()
    ),
    class = c("dsprrr_gepa_component_candidate", "list")
  )
}

gepa_component_candidate_ids <- function(candidate) {
  gepa_validate_component_candidate(candidate)
  names(candidate$components)
}

gepa_validate_component_candidate <- function(candidate, program = NULL) {
  if (!inherits(candidate, "dsprrr_gepa_component_candidate")) {
    cli::cli_abort(
      "GEPA component candidate has an invalid class",
      class = "dsprrr_gepa_candidate_error"
    )
  }
  required <- c("components", "valid", "failure", "history")
  if (
    !is.list(candidate) ||
      !identical(sort(names(candidate)), sort(required))
  ) {
    cli::cli_abort(
      "GEPA component candidate is malformed",
      class = "dsprrr_gepa_candidate_error"
    )
  }
  if (
    !is.logical(candidate$valid) ||
      length(candidate$valid) != 1L ||
      is.na(candidate$valid)
  ) {
    cli::cli_abort(
      "GEPA component candidate validity must be TRUE or FALSE",
      class = "dsprrr_gepa_candidate_error"
    )
  }
  if (!is.list(candidate$history)) {
    cli::cli_abort(
      "GEPA component candidate history must be a list",
      class = "dsprrr_gepa_candidate_error"
    )
  }
  if (!is.null(candidate$failure)) {
    valid_failure <- is.list(candidate$failure) &&
      identical(
        sort(names(candidate$failure)),
        sort(c("error_class", "error_message"))
      ) &&
      all(vapply(
        candidate$failure,
        function(value) {
          is.character(value) && length(value) == 1L && !is.na(value)
        },
        logical(1)
      ))
    if (!valid_failure) {
      cli::cli_abort(
        "GEPA component candidate failure data is malformed",
        class = "dsprrr_gepa_candidate_error"
      )
    }
  }
  if (
    !is.list(candidate$components) ||
      (length(candidate$components) > 0L &&
        is.null(names(candidate$components)))
  ) {
    cli::cli_abort(
      "GEPA candidate components must be a named list",
      class = "dsprrr_gepa_candidate_error"
    )
  }
  if (anyDuplicated(names(candidate$components))) {
    cli::cli_abort(
      "GEPA candidate component IDs must be unique",
      class = "dsprrr_gepa_candidate_error"
    )
  }

  for (id in names(candidate$components)) {
    component <- candidate$components[[id]]
    if (
      !is.list(component) ||
        !identical(
          sort(names(component)),
          sort(c("id", "kind", "path", "value"))
        )
    ) {
      cli::cli_abort(
        "GEPA candidate component {.val {id}} is malformed",
        class = "dsprrr_gepa_candidate_error"
      )
    }
    scalar_strings <- vapply(
      component[c("id", "kind", "path", "value")],
      function(value) {
        is.character(value) &&
          length(value) == 1L &&
          !is.na(value)
      },
      logical(1)
    )
    if (!all(scalar_strings)) {
      cli::cli_abort(
        "GEPA candidate component {.val {id}} must contain scalar strings",
        class = "dsprrr_gepa_candidate_error"
      )
    }
    if (!identical(component$id, id)) {
      cli::cli_abort(
        "GEPA candidate component ID does not match its list key",
        class = "dsprrr_gepa_candidate_error"
      )
    }
    if (!component$kind %in% c("instructions", "module_src")) {
      cli::cli_abort(
        "Unknown GEPA component kind {.val {component$kind}}",
        class = "dsprrr_gepa_candidate_error"
      )
    }
    expected_id <- gepa_component_id(component$kind, component$path)
    if (!identical(component$id, expected_id)) {
      cli::cli_abort(
        "GEPA component identity is inconsistent",
        class = "dsprrr_gepa_candidate_error"
      )
    }
  }

  if (!is.null(program)) {
    expected <- gepa_program_component_specs(program)
    if (!identical(names(candidate$components), names(expected))) {
      cli::cli_abort(
        "GEPA candidate does not contain the complete program component set",
        class = "dsprrr_gepa_candidate_error"
      )
    }
    for (id in names(expected)) {
      actual <- candidate$components[[id]]
      spec <- expected[[id]]
      if (
        !identical(actual$kind, spec$kind) ||
          !identical(actual$path, spec$path)
      ) {
        cli::cli_abort(
          "GEPA candidate component layout does not match the program graph",
          class = "dsprrr_gepa_candidate_error"
        )
      }
    }
  }

  invisible(candidate)
}

gepa_component_target <- function(program, path) {
  modules <- named_modules(
    program,
    include_root = TRUE,
    aliases = FALSE,
    boundaries = "cross"
  )
  target <- modules[[path]]
  if (is.null(target)) {
    cli::cli_abort(
      "GEPA component path {.val {path}} disappeared from the copied program",
      class = c(
        "dsprrr_gepa_component_path_error",
        "dsprrr_optimizer_invariant_error"
      )
    )
  }
  target
}

gepa_failure_score <- function(value) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)
  ) {
    cli::cli_abort(
      "{.arg failure_score} must be one finite number",
      class = "dsprrr_gepa_component_error"
    )
  }
  as.numeric(value)
}

gepa_candidate_failure_condition <- function(message, parent = NULL) {
  rlang::error_cnd(
    class = c("dsprrr_gepa_invalid_candidate", "dsprrr_gepa_candidate_error"),
    message = message,
    parent = parent
  )
}

gepa_failed_materialization <- function(candidate, failure_score, condition) {
  structure(
    list(
      ok = FALSE,
      selectable = FALSE,
      program = NULL,
      candidate = candidate,
      failure_score = failure_score,
      failure_kind = "candidate",
      condition = condition,
      error_class = class(condition)[1L],
      error_message = conditionMessage(condition)
    ),
    class = c("dsprrr_gepa_candidate_materialization", "list")
  )
}

gepa_clone_component_program <- function(program) {
  module_graph_check_program(program)
  clones <- new.env(parent = emptyenv(), hash = TRUE)
  clone_value <- NULL

  clone_module <- function(module) {
    id <- rlang::obj_address(module)
    if (exists(id, envir = clones, inherits = FALSE)) {
      return(get(id, envir = clones, inherits = FALSE))
    }
    cloned <- tryCatch(
      module$clone(deep = FALSE),
      error = function(error) {
        cli::cli_abort(
          "Cannot copy {.cls {class(module)[1]}} for GEPA component evaluation",
          class = "dsprrr_optimizer_invariant_error",
          parent = error
        )
      }
    )
    if (!inherits(cloned, "Module") || identical(cloned, module)) {
      cli::cli_abort(
        "Module clone did not produce an independent program node",
        class = "dsprrr_optimizer_invariant_error"
      )
    }
    assign(id, cloned, envir = clones)

    original_adapter <- module_graph_child_adapter(module)
    children <- original_adapter$children
    if (length(children) > 0L) {
      cloned_children <- clone_value(children)
      cloned_adapter <- module_graph_child_adapter(cloned)
      if (!is.function(cloned_adapter$set)) {
        cli::cli_abort(
          c(
            "Cannot copy a module graph with read-only children",
            "x" = "{.cls {class(module)[1]}} exposes children but no replacement hook."
          ),
          class = c(
            "dsprrr_gepa_component_copy_error",
            "dsprrr_optimizer_invariant_error"
          )
        )
      }
      tryCatch(
        cloned_adapter$set(cloned_children),
        error = function(error) {
          cli::cli_abort(
            "Cannot restore copied children for {.cls {class(module)[1]}}",
            class = c(
              "dsprrr_gepa_component_copy_error",
              "dsprrr_optimizer_invariant_error"
            ),
            parent = error
          )
        }
      )
    }
    cloned
  }
  clone_value <- function(value) {
    if (inherits(value, "Module")) {
      return(clone_module(value))
    }
    if (!is.list(value)) {
      return(value)
    }
    lapply(value, clone_value) |>
      stats::setNames(names(value))
  }

  cloned <- clone_module(program)
  tryCatch(
    module_graph_refresh_all(cloned),
    error = function(error) {
      cli::cli_abort(
        "Cannot refresh the copied module graph for GEPA component evaluation",
        class = c(
          "dsprrr_gepa_component_copy_error",
          "dsprrr_optimizer_invariant_error"
        ),
        parent = error
      )
    }
  )
  cloned
}

gepa_materialize_component_candidate <- function(
  program,
  candidate,
  failure_score = 0
) {
  module_graph_check_program(program)
  failure_score <- gepa_failure_score(failure_score)

  validation_error <- tryCatch(
    {
      gepa_validate_component_candidate(candidate, program = program)
      NULL
    },
    error = function(error) error
  )
  if (!is.null(validation_error)) {
    if (!inherits(validation_error, "dsprrr_gepa_candidate_error")) {
      stop(validation_error)
    }
    return(gepa_failed_materialization(
      candidate,
      failure_score,
      gepa_candidate_failure_condition(
        conditionMessage(validation_error),
        parent = validation_error
      )
    ))
  }
  if (!isTRUE(candidate$valid)) {
    message <- candidate$failure$error_message %||%
      "GEPA candidate was marked invalid during proposal"
    return(gepa_failed_materialization(
      candidate,
      failure_score,
      gepa_candidate_failure_condition(message)
    ))
  }

  # Clone every graph identity once, then restore children through the graph
  # adapters. This keeps aliases shared and never mutates an original leaf,
  # including for composites without a bespoke `$deepcopy()` method.
  copied <- gepa_clone_component_program(program)
  kinds <- vapply(candidate$components, `[[`, character(1), "kind")
  apply_order <- c(which(kinds == "instructions"), which(kinds == "module_src"))
  bind_error <- NULL

  for (index in apply_order) {
    component <- candidate$components[[index]]
    target <- gepa_component_target(copied, component$path)
    if (identical(component$kind, "instructions")) {
      target$apply_optimization_params(list(instructions = component$value))
      next
    }
    if (!inherits(target, "FlexModule") || !is.function(target$bind)) {
      cli::cli_abort(
        "GEPA module_src component no longer targets a Flex module",
        class = c(
          "dsprrr_gepa_component_path_error",
          "dsprrr_optimizer_invariant_error"
        )
      )
    }
    bind_error <- tryCatch(
      {
        target$bind(component$value)
        NULL
      },
      error = function(error) error
    )
    if (!is.null(bind_error)) {
      if (!inherits(bind_error, "dsprrr_flex_source_error")) {
        stop(bind_error)
      }
      break
    }
  }

  if (!is.null(bind_error)) {
    return(gepa_failed_materialization(
      candidate,
      failure_score,
      gepa_candidate_failure_condition(
        conditionMessage(bind_error),
        parent = bind_error
      )
    ))
  }

  structure(
    list(
      ok = TRUE,
      selectable = TRUE,
      program = copied,
      candidate = candidate,
      failure_score = NA_real_,
      failure_kind = NULL,
      condition = NULL,
      error_class = NA_character_,
      error_message = NA_character_
    ),
    class = c("dsprrr_gepa_candidate_materialization", "list")
  )
}

gepa_invalid_candidate_eval <- function(
  program,
  dataset,
  materialization
) {
  if (!inherits(materialization, "dsprrr_gepa_candidate_materialization")) {
    cli::cli_abort(
      "Invalid GEPA candidate materialization result",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  n <- nrow(dataset)
  examples <- tibble::tibble(
    row_id = seq_len(n),
    score = rep(materialization$failure_score, n),
    error = rep(materialization$error_message, n),
    error_class = rep(materialization$error_class, n),
    predicted = rep(list(NA), n),
    feedback = rep(materialization$error_message, n),
    program_trace = rep(list(NULL), n)
  )
  for (name in get_input_names(program$signature)) {
    if (name %in% names(dataset)) {
      examples[[paste0("input_", name)]] <- dataset[[name]]
    }
  }

  EvalResult(
    examples = examples,
    mean_score = if (n > 0L) materialization$failure_score else NA_real_,
    n_evaluated = 0L,
    n_errors = as.integer(n),
    input_tokens = 0L,
    output_tokens = 0L,
    total_tokens = 0L,
    total_cost = 0,
    provider_calls = 0L,
    metric_calls = 0L,
    provider_usage_unknown = FALSE,
    token_usage_unknown = FALSE
  )
}

gepa_record_invalid_candidate <- function(
  eval_result,
  budget,
  stage,
  unit_id,
  condition = NULL
) {
  if (is.null(budget)) {
    return(TRUE)
  }
  can_start <- optimizer_budget_preflight(
    budget,
    stage = stage,
    planned = list(trials = 1L),
    unit_id = unit_id,
    work_unit = "optimizer_trial",
    max_started = 0L,
    planned_outcomes = 1L
  )
  if (!can_start) {
    return(FALSE)
  }
  # The row-aligned EvalResult is audit evidence for one malformed proposal,
  # not evidence of N independent runtime/provider failures.
  record_optimizer_outcome(
    budget,
    success = FALSE,
    stage = stage,
    condition = condition
  )
  optimizer_budget_count_trial(budget, stage, unit_id)
  optimizer_budget_complete_unit(budget, unit_id)
  TRUE
}

gepa_evaluate_component_candidate <- function(
  program,
  candidate,
  dataset,
  metric,
  .llm = NULL,
  control = NULL,
  budget = NULL,
  stage,
  unit_id,
  failure_score = 0,
  ...
) {
  materialization <- gepa_materialize_component_candidate(
    program,
    candidate,
    failure_score = failure_score
  )
  if (!materialization$ok) {
    eval_result <- gepa_invalid_candidate_eval(
      program,
      dataset,
      materialization
    )
    completed <- gepa_record_invalid_candidate(
      eval_result,
      budget,
      stage,
      unit_id,
      condition = materialization$condition
    )
    return(structure(
      list(
        eval_result = eval_result,
        program = NULL,
        materialization = materialization,
        selectable = FALSE,
        completed = completed,
        failure_kind = "candidate"
      ),
      class = c("dsprrr_gepa_candidate_evaluation", "list")
    ))
  }

  # Do not catch evaluation errors here. optimizer_eval_candidate() already
  # keeps ordinary row failures aligned, while provider/infrastructure errors
  # that it deliberately raises must retain their original condition class.
  eval_result <- optimizer_eval_candidate(
    materialization$program,
    dataset,
    metric,
    .llm = .llm,
    control = control,
    budget = budget,
    stage = stage,
    unit_id = unit_id,
    ...
  )
  completed <- if (is.null(budget)) {
    TRUE
  } else {
    optimizer_budget_unit_completed(budget, unit_id)
  }
  structure(
    list(
      eval_result = eval_result,
      program = materialization$program,
      materialization = materialization,
      selectable = isTRUE(completed),
      completed = completed,
      failure_kind = NULL
    ),
    class = c("dsprrr_gepa_candidate_evaluation", "list")
  )
}

gepa_flex_contract <- function(module) {
  inputs <- lapply(module$signature@inputs, function(input) {
    list(name = input$name, schema = ellmer_type_to_json_schema(input$type))
  })
  outputs <- lapply(
    flex_signature_output_types(module$signature),
    ellmer_type_to_json_schema
  )
  list(
    inputs = inputs,
    outputs = outputs,
    max_predictor_calls = module$max_predictor_calls
  )
}

gepa_flex_json <- function(value) {
  tryCatch(
    as.character(jsonlite::toJSON(
      value,
      auto_unbox = TRUE,
      dataframe = "rows",
      null = "null",
      na = "null",
      digits = NA,
      pretty = FALSE
    )),
    error = function(error) {
      as.character(jsonlite::toJSON(
        paste(
          utils::capture.output(utils::str(value, give.attr = FALSE)),
          collapse = " "
        ),
        auto_unbox = TRUE
      ))
    }
  )
}

gepa_flex_component_evidence <- function(
  program,
  component_path,
  example
) {
  root <- list(
    scope = "root_program",
    inputs = example$inputs %||% list(),
    output = example$predicted %||% NA
  )
  if (
    identical(component_path, "$") ||
      is.null(program) ||
      !inherits(program, "PipelineModule") ||
      is.null(example$program_trace)
  ) {
    return(root)
  }

  segments <- module_graph_list_segments(program$steps)
  step_root <- module_graph_append_path("$", "steps")
  step_paths <- vapply(
    segments,
    function(segment) module_graph_append_path(step_root, segment),
    character(1)
  )
  matches <- which(vapply(
    step_paths,
    function(path) {
      identical(component_path, path) ||
        startsWith(component_path, paste0(path, "/"))
    },
    logical(1)
  ))
  if (length(matches) != 1L) {
    return(root)
  }
  events <- example$program_trace$events
  if (!is.list(events) || length(events) == 0L) {
    return(root)
  }
  event <- events[[length(events)]]
  index <- matches[[1L]]
  if (
    !is.list(event$step_inputs) ||
      !is.list(event$step_outputs) ||
      index > length(event$step_inputs) ||
      index > length(event$step_outputs)
  ) {
    return(root)
  }
  list(
    scope = "pipeline_step",
    inputs = event$step_inputs[[index]],
    output = event$step_outputs[[index]]
  )
}

gepa_flex_failure_bundles <- function(
  failed_examples,
  program = NULL,
  component_path = "$"
) {
  lapply(seq_along(failed_examples), function(index) {
    example <- failed_examples[[index]]
    inputs <- example$inputs %||% list()
    if (is.data.frame(inputs)) {
      inputs <- as.list(inputs[1L, , drop = FALSE])
    }
    example$inputs <- inputs
    evidence <- gepa_flex_component_evidence(
      program,
      component_path,
      example
    )
    bundle <- list(
      row_id = example$row_id %||% index,
      component_path = component_path,
      evidence_scope = evidence$scope,
      component_inputs = evidence$inputs,
      component_observed_output = evidence$output,
      expected_program_output = example$expected %||% NA,
      predicted_program_output = example$predicted %||% NA,
      metric_feedback = example$feedback %||% NA_character_
    )
    if (identical(component_path, "$")) {
      bundle$inputs <- inputs
      bundle$expected_output <- example$expected %||% NA
      bundle$predicted_output <- example$predicted %||% NA
    } else {
      bundle$root_inputs <- inputs
    }
    bundle
  })
}

gepa_flex_reflection_prompt <- function(
  module,
  failed_examples,
  program = module,
  component_path = "$"
) {
  contract <- gepa_flex_json(gepa_flex_contract(module))
  failures <- gepa_flex_json(gepa_flex_failure_bundles(
    failed_examples,
    program = program,
    component_path = component_path
  ))
  paste(
    "You are proposing one complete declarative Flex module source.",
    "Return a JSON object field named module_src containing the complete JSON source string.",
    "Do not return a patch, Markdown fence, R expression, function, or executable code.",
    "The source is data only and must use exactly this version 1 schema:",
    '{"schema_version":1,"steps":[{"name":"safe_name","primitive":"predict or chain_of_thought","signature":"$outer or DSPy input -> output","instructions":"optional string","inputs":{"target":"$input.name or $step.earlier.field"}}],"outputs":{"every_outer_output":"$step.name.field"}}',
    "Top-level fields and step fields not shown above are forbidden.",
    "Step names must be unique safe identifiers. Steps are ordered, acyclic, and may reference only outer inputs or earlier step outputs.",
    "Every step input and every outer output must appear exactly once and have a compatible type.",
    paste0("Outer contract and call limit: ", contract),
    paste0("Target component graph path: ", component_path),
    "Current complete canonical source:",
    module$module_src,
    "Row-aligned failure bundles keep component observations, root-program truth and prediction, and metric feedback together.",
    "For a nested component, pipeline_step evidence is the observed input/output at its containing pipeline step; root_program evidence is explicitly labeled context, not a claimed leaf-level target.",
    failures,
    "Propose a complete source that addresses the failures while staying within the exact schema and call limit.",
    sep = "\n"
  )
}

gepa_flex_proposal <- function(
  status,
  module_src,
  condition = NULL,
  unit_id = NULL
) {
  structure(
    list(
      status = status,
      module_src = module_src,
      valid = status %in%
        c(
          "proposed",
          "unchanged_no_proposer",
          "unchanged_provider_error",
          "unchanged_budget_stop"
        ),
      condition = condition,
      error_class = if (is.null(condition)) {
        NA_character_
      } else {
        class(condition)[1L]
      },
      error_message = if (is.null(condition)) {
        NA_character_
      } else {
        conditionMessage(condition)
      },
      unit_id = unit_id
    ),
    class = c("dsprrr_gepa_flex_proposal", "list")
  )
}

gepa_flex_response_source <- function(value) {
  if (
    !is.list(value) ||
      !identical(names(value), "module_src") ||
      !is.character(value[["module_src"]]) ||
      length(value[["module_src"]]) != 1L ||
      is.na(value[["module_src"]])
  ) {
    return(NULL)
  }
  value[["module_src"]]
}

gepa_propose_flex_source <- function(
  module,
  failed_examples,
  .llm = NULL,
  verbose = FALSE,
  budget = NULL,
  unit_id = "gepa:flex_reflection",
  program = module,
  component_path = "$"
) {
  if (!inherits(module, "FlexModule")) {
    cli::cli_abort(
      "Flex source proposals require a FlexModule",
      class = "dsprrr_gepa_component_error"
    )
  }
  current <- module$module_src
  if (is.null(.llm) || !is.function(.llm$chat_structured)) {
    return(gepa_flex_proposal(
      "unchanged_no_proposer",
      current,
      unit_id = unit_id
    ))
  }

  prompt <- gepa_flex_reflection_prompt(
    module,
    failed_examples,
    program = program,
    component_path = component_path
  )
  type <- ellmer::type_object(
    module_src = ellmer::type_string(
      description = "Complete canonicalizable Flex schema version 1 JSON"
    )
  )
  if (!is.null(budget)) {
    request <- optimizer_budgeted_provider_call(
      budget = budget,
      model = .llm,
      stage = "gepa_flex_reflection",
      unit_id = unit_id,
      call = function() {
        .llm$chat_structured(prompt, type = type, echo = "none")
      },
      success = function(value, condition) {
        is.null(condition) && !is.null(gepa_flex_response_source(value))
      }
    )
    if (!request$started) {
      return(gepa_flex_proposal(
        "unchanged_budget_stop",
        current,
        unit_id = unit_id
      ))
    }
    result <- request$value
    provider_condition <- request$condition
  } else {
    provider_condition <- NULL
    result <- tryCatch(
      .llm$chat_structured(prompt, type = type, echo = "none"),
      error = function(error) {
        provider_condition <<- error
        NULL
      }
    )
  }

  if (!is.null(provider_condition)) {
    cli::cli_warn(
      c(
        "GEPA Flex proposer call failed; retaining the current source",
        "x" = conditionMessage(provider_condition)
      ),
      class = "dsprrr_gepa_flex_proposer_warning"
    )
    return(gepa_flex_proposal(
      "unchanged_provider_error",
      current,
      condition = provider_condition,
      unit_id = unit_id
    ))
  }

  raw_source <- gepa_flex_response_source(result)
  if (is.null(raw_source)) {
    condition <- gepa_candidate_failure_condition(
      "GEPA Flex proposer did not return one module_src string"
    )
    if (isTRUE(verbose)) {
      cli::cli_warn(conditionMessage(condition))
    }
    return(gepa_flex_proposal(
      "invalid_proposal",
      current,
      condition = condition,
      unit_id = unit_id
    ))
  }

  probe <- module$reset_copy()
  bind_error <- tryCatch(
    {
      probe$bind(raw_source)
      NULL
    },
    error = function(error) error
  )
  if (!is.null(bind_error)) {
    if (!inherits(bind_error, "dsprrr_flex_source_error")) {
      stop(bind_error)
    }
    return(gepa_flex_proposal(
      "invalid_proposal",
      current,
      condition = gepa_candidate_failure_condition(
        conditionMessage(bind_error),
        parent = bind_error
      ),
      unit_id = unit_id
    ))
  }

  gepa_flex_proposal(
    "proposed",
    probe$module_src,
    unit_id = unit_id
  )
}

gepa_component_history_entry <- function(component, status, proposal = NULL) {
  list(
    component_id = component$id,
    kind = component$kind,
    path = component$path,
    status = status,
    error_class = proposal$error_class %||% NA_character_,
    error_message = proposal$error_message %||% NA_character_,
    unit_id = proposal$unit_id %||% NULL
  )
}

gepa_mutate_component_candidate <- function(
  candidate,
  program,
  failed_examples,
  .llm = NULL,
  verbose = FALSE,
  budget = NULL,
  unit_id = "gepa:component_reflection",
  component_id = NULL,
  failure_score = 0
) {
  gepa_validate_component_candidate(candidate, program = program)
  if (!isTRUE(candidate$valid)) {
    return(candidate)
  }
  ids <- names(candidate$components)
  if (length(ids) == 0L) {
    return(candidate)
  }
  component_id <- component_id %||% sample(ids, size = 1L)
  if (
    !is.character(component_id) ||
      length(component_id) != 1L ||
      !component_id %in% ids
  ) {
    cli::cli_abort(
      "{.arg component_id} must identify one candidate component",
      class = "dsprrr_gepa_component_error"
    )
  }

  materialized <- gepa_materialize_component_candidate(
    program,
    candidate,
    failure_score = failure_score
  )
  if (!materialized$ok) {
    candidate$valid <- FALSE
    candidate$failure <- list(
      error_class = materialized$error_class,
      error_message = materialized$error_message
    )
    return(candidate)
  }
  component <- candidate$components[[component_id]]

  if (identical(component$kind, "instructions")) {
    value <- gepa_mutate_instruction(
      component$value,
      failed_examples = failed_examples,
      .llm = .llm,
      verbose = verbose,
      budget = budget,
      unit_id = unit_id
    )
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value)
    ) {
      condition <- gepa_candidate_failure_condition(
        "GEPA instruction proposer returned an invalid value"
      )
      candidate$valid <- FALSE
      candidate$failure <- list(
        error_class = class(condition)[1L],
        error_message = conditionMessage(condition)
      )
      candidate$history <- append(
        candidate$history,
        list(gepa_component_history_entry(
          component,
          "invalid_proposal",
          gepa_flex_proposal(
            "invalid_proposal",
            component$value,
            condition,
            unit_id
          )
        ))
      )
      return(candidate)
    }
    candidate$components[[component_id]]$value <- value
    candidate$history <- append(
      candidate$history,
      list(gepa_component_history_entry(component, "proposed"))
    )
    return(candidate)
  }

  target <- gepa_component_target(materialized$program, component$path)
  proposal <- gepa_propose_flex_source(
    target,
    failed_examples = failed_examples,
    .llm = .llm,
    verbose = verbose,
    budget = budget,
    unit_id = unit_id,
    program = materialized$program,
    component_path = component$path
  )
  candidate$history <- append(
    candidate$history,
    list(gepa_component_history_entry(
      component,
      proposal$status,
      proposal
    ))
  )
  if (!proposal$valid) {
    candidate$valid <- FALSE
    candidate$failure <- list(
      error_class = proposal$error_class,
      error_message = proposal$error_message
    )
    return(candidate)
  }
  candidate$components[[component_id]]$value <- proposal$module_src
  candidate
}

gepa_crossover_component_candidates <- function(parent1, parent2) {
  gepa_validate_component_candidate(parent1)
  gepa_validate_component_candidate(parent2)
  if (!identical(names(parent1$components), names(parent2$components))) {
    cli::cli_abort(
      "GEPA crossover parents have different component layouts",
      class = "dsprrr_gepa_candidate_error"
    )
  }
  child <- parent1
  for (id in names(child$components)) {
    left <- parent1$components[[id]]
    right <- parent2$components[[id]]
    if (
      !identical(left$kind, right$kind) ||
        !identical(left$path, right$path)
    ) {
      cli::cli_abort(
        "GEPA crossover parents have incompatible components",
        class = "dsprrr_gepa_candidate_error"
      )
    }
    selected <- if (stats::runif(1L) < 0.5) left else right
    # Complete component values are selected atomically. In particular, JSON
    # sources are never text-spliced or treated as executable code.
    child$components[[id]]$value <- selected$value
  }
  child$valid <- isTRUE(parent1$valid) && isTRUE(parent2$valid)
  # `x$failure <- NULL` would remove the required field from the candidate.
  child["failure"] <- list(NULL)
  child$history <- append(
    child$history,
    list(list(status = "component_crossover"))
  )
  child
}

gepa_initial_component_population <- function(
  program,
  population_size,
  .llm = NULL,
  verbose = FALSE,
  budget = NULL,
  failure_score = 0
) {
  population_size <- flex_positive_integer(population_size, "population_size")
  baseline <- gepa_component_candidate(program)
  population <- vector("list", population_size)
  population[[1L]] <- baseline
  ids <- names(baseline$components)

  if (length(ids) == 0L) {
    return(list(baseline))
  }
  if (population_size == 1L) {
    return(population)
  }
  for (index in seq.int(2L, population_size)) {
    if (!is.null(budget) && optimizer_budget_stopped(budget)) {
      break
    }
    component_id <- ids[((index - 2L) %% length(ids)) + 1L]
    population[[index]] <- gepa_mutate_component_candidate(
      baseline,
      program = program,
      failed_examples = list(),
      .llm = .llm,
      verbose = verbose,
      budget = budget,
      unit_id = paste0("gepa:initial_component_mutation:", index),
      component_id = component_id,
      failure_score = failure_score
    )
  }
  Filter(Negate(is.null), population)
}

gepa_component_select_parent <- function(records, ranks, crowding) {
  if (length(records) == 1L) {
    return(records[[1L]])
  }
  candidates <- sample(seq_along(records), size = 2L)
  left <- candidates[[1L]]
  right <- candidates[[2L]]
  if (ranks[[left]] < ranks[[right]]) {
    return(records[[left]])
  }
  if (ranks[[right]] < ranks[[left]]) {
    return(records[[right]])
  }
  if (crowding[[left]] >= crowding[[right]]) {
    records[[left]]
  } else {
    records[[right]]
  }
}

gepa_next_component_generation <- function(
  records,
  program,
  population_size,
  mutation_rate,
  crossover_rate,
  selection,
  .llm,
  verbose = FALSE,
  budget = NULL,
  generation = NA_integer_,
  failure_score = 0
) {
  records <- Filter(gepa_component_record_selectable, records)
  if (length(records) == 0L) {
    return(list())
  }
  scores_matrix <- do.call(
    rbind,
    lapply(records, function(record) record$scores)
  )
  if (selection == "pareto" && ncol(scores_matrix) > 1L) {
    ranks <- pareto_ranks(scores_matrix)
    crowding <- pareto_crowding_distance(scores_matrix, ranks)
  } else {
    primary <- scores_matrix[, 1L]
    primary[is.na(primary)] <- -Inf
    ranks <- rank(-primary, ties.method = "min")
    crowding <- primary
    crowding[is.infinite(crowding)] <- 0
  }

  population <- vector("list", population_size)
  for (index in seq_len(population_size)) {
    if (!is.null(budget) && optimizer_budget_stopped(budget)) {
      break
    }
    parent1 <- gepa_component_select_parent(records, ranks, crowding)
    parent2 <- gepa_component_select_parent(records, ranks, crowding)
    child <- parent1$candidate
    if (stats::runif(1L) < crossover_rate) {
      child <- gepa_crossover_component_candidates(
        parent1$candidate,
        parent2$candidate
      )
    }
    if (stats::runif(1L) < mutation_rate) {
      child <- gepa_mutate_component_candidate(
        child,
        program = program,
        failed_examples = parent1$failed_examples,
        .llm = .llm,
        verbose = verbose,
        budget = budget,
        unit_id = paste0(
          "gepa:generation:",
          generation,
          ":component_mutation:",
          index
        ),
        failure_score = failure_score
      )
    }
    population[[index]] <- child
  }
  Filter(Negate(is.null), population)
}

compile_gepa_components <- function(
  teleprompter,
  program,
  dataset,
  metrics,
  metric_names,
  .llm,
  control,
  budget,
  trial_log = NULL
) {
  component_specs <- gepa_program_component_specs(program)
  if (length(component_specs) == 0L) {
    optimized <- gepa_clone_component_program(program)
    budget_summary <- optimizer_budget_summary(budget)
    optimized$config$compiled <- TRUE
    optimized$config$teleprompter <- "GEPA"
    optimized$config$optimizer <- list(
      selection = teleprompter@selection,
      population_size = teleprompter@population_size,
      generations = teleprompter@generations,
      optimization_mode = "component_candidates",
      optimization_skipped = TRUE,
      skip_reason = "no_mutable_components",
      flex_paths = gepa_flex_paths(program, mutable_only = FALSE),
      mutable_flex_paths = character(),
      component_ids = character(),
      component_semantics = gepa_component_semantics(),
      best_candidate = NULL,
      best_scores = stats::setNames(
        rep(NA_real_, length(metrics)),
        metric_names
      ),
      pareto_frontier = list(),
      all_generations = list(),
      invalid_candidate_count = 0L,
      error_count = budget_summary$total_errors,
      budget_summary = budget_summary,
      stop_reason = budget_summary$stop_reason,
      partial = FALSE
    )
    if (!is.null(trial_log)) {
      trial_log$save()
    }
    return(optimized)
  }

  failure_score <- 0
  population <- gepa_initial_component_population(
    program,
    population_size = teleprompter@population_size,
    .llm = .llm,
    verbose = teleprompter@verbose,
    budget = budget,
    failure_score = failure_score
  )

  all_generations <- list()
  all_records <- list()
  selectable_records <- list()
  best_record <- NULL

  for (generation in seq_len(teleprompter@generations)) {
    if (teleprompter@verbose) {
      cli::cli_alert_info(
        "GEPA generation {generation}/{teleprompter@generations} (component candidates)"
      )
    }
    records <- list()

    for (index in seq_along(population)) {
      if (optimizer_budget_stopped(budget)) {
        break
      }

      candidate <- population[[index]]
      scores <- rep(NA_real_, length(metrics))
      failed_examples <- list()
      primary_eval <- NULL
      last_eval <- NULL
      completed_metrics <- 0L
      candidate_valid <- isTRUE(candidate$valid)
      metric_unit_ids <- paste0(
        "gepa:generation:",
        generation,
        ":candidate:",
        index,
        ":metric:",
        seq_along(metrics)
      )

      for (metric_index in seq_along(metrics)) {
        if (optimizer_budget_stopped(budget)) {
          break
        }
        evaluated <- gepa_evaluate_component_candidate(
          program,
          candidate,
          dataset,
          metric = metrics[[metric_index]],
          .llm = .llm,
          control = control,
          budget = budget,
          stage = paste0("gepa_flex_metric_", metric_index),
          unit_id = metric_unit_ids[[metric_index]],
          failure_score = failure_score
        )
        eval_result <- evaluated$eval_result
        if (!S7::S7_inherits(eval_result, EvalResult)) {
          cli::cli_abort(
            c(
              "GEPA Flex evaluation returned an invalid result",
              "i" = "Expected EvalResult, got {.cls {class(eval_result)}}"
            ),
            class = "dsprrr_optimizer_invariant_error"
          )
        }

        if (identical(evaluated$failure_kind, "candidate")) {
          candidate_valid <- FALSE
          if (isTRUE(candidate$valid)) {
            candidate$valid <- FALSE
            candidate$failure <- list(
              error_class = evaluated$materialization$error_class,
              error_message = evaluated$materialization$error_message
            )
          }
        }
        last_eval <- eval_result
        scores[[metric_index]] <- eval_result@mean_score
        if (isTRUE(evaluated$completed)) {
          completed_metrics <- metric_index
        }
        if (metric_index == 1L) {
          primary_eval <- eval_result
          failed_examples <- gepa_failed_examples(
            eval_result,
            dataset,
            program$signature,
            threshold = teleprompter@metric_threshold,
            output_col = get_metric_field(metrics[[1L]])
          )
        }
      }

      record <- list(
        candidate = candidate,
        candidate_valid = candidate_valid,
        scores = stats::setNames(scores, metric_names),
        failed_examples = failed_examples,
        generation = generation,
        index = index,
        completed_metrics = completed_metrics,
        complete = all(vapply(
          metric_unit_ids,
          function(unit_id) {
            optimizer_budget_unit_completed(budget, unit_id)
          },
          logical(1)
        )) &&
          !anyNA(scores)
      )
      record$selectable <- gepa_component_record_selectable(record)
      records[[length(records) + 1L]] <- record
      all_records[[length(all_records) + 1L]] <- record

      if (
        !is.null(trial_log) &&
          isTRUE(record$complete) &&
          !is.null(last_eval)
      ) {
        params <- c(
          list(
            generation = generation,
            index = index,
            optimization_mode = "component_candidates"
          ),
          gepa_component_candidate_params(candidate)
        )
        trial <- create_trial(optimizer_name = "GEPA", params = params)
        trial <- start_trial(trial)
        trial <- complete_trial(trial, primary_eval %||% last_eval)
        trial_log$add_trial(trial)
      }

      if (optimizer_budget_stopped(budget)) {
        break
      }
    }

    complete_records <- Filter(
      function(record) isTRUE(record$complete),
      records
    )
    generation_selectable <- Filter(
      gepa_component_record_selectable,
      complete_records
    )
    if (length(generation_selectable) > 0L) {
      selectable_records <- c(selectable_records, generation_selectable)
      scores_matrix <- do.call(
        rbind,
        lapply(selectable_records, function(record) record$scores)
      )
      if (teleprompter@selection == "pareto" && length(metrics) > 1L) {
        ranks <- pareto_ranks(scores_matrix)
        crowding <- pareto_crowding_distance(scores_matrix, ranks)
        best_index <- select_pareto_best(scores_matrix, ranks, crowding)
      } else {
        best_index <- which.max(scores_matrix[, 1L])
      }
      best_record <- selectable_records[[best_index]]
    }

    if (isTRUE(teleprompter@track_stats)) {
      all_generations[[generation]] <- list(
        generation = generation,
        population = complete_records,
        n_selectable = length(generation_selectable),
        n_invalid = sum(
          !vapply(
            complete_records,
            function(record) isTRUE(record$candidate_valid),
            logical(1)
          )
        )
      )
    }

    if (
      optimizer_budget_stopped(budget) ||
        generation == teleprompter@generations ||
        length(generation_selectable) == 0L
    ) {
      break
    }
    population <- gepa_next_component_generation(
      generation_selectable,
      program = program,
      population_size = teleprompter@population_size,
      mutation_rate = teleprompter@mutation_rate,
      crossover_rate = teleprompter@crossover_rate,
      selection = teleprompter@selection,
      .llm = .llm,
      verbose = teleprompter@verbose,
      budget = budget,
      generation = generation + 1L,
      failure_score = failure_score
    )
  }

  optimized <- if (is.null(best_record)) {
    gepa_clone_component_program(program)
  } else {
    materialized <- gepa_materialize_component_candidate(
      program,
      best_record$candidate,
      failure_score = failure_score
    )
    if (!materialized$ok || !materialized$selectable) {
      cli::cli_abort(
        "GEPA selected a component candidate that cannot be materialized",
        class = "dsprrr_optimizer_invariant_error",
        parent = materialized$condition
      )
    }
    materialized$program
  }
  if (is.null(best_record) && !optimizer_budget_stopped(budget)) {
    cli::cli_warn(c(
      "GEPA optimization failed to produce any valid component candidates",
      "!" = "Returning unmodified program",
      "i" = "Invalid Flex sources are scored for audit but are never selectable."
    ))
  }

  final_scores <- if (is.null(best_record)) {
    stats::setNames(rep(NA_real_, length(metrics)), metric_names)
  } else {
    best_record$scores
  }
  frontier <- list()
  if (length(selectable_records) > 0L) {
    scores_matrix <- do.call(
      rbind,
      lapply(selectable_records, function(record) record$scores)
    )
    frontier_index <- pareto_frontier(scores_matrix)
    frontier <- lapply(selectable_records[frontier_index], function(record) {
      list(
        candidate = gepa_component_candidate_params(record$candidate),
        scores = record$scores
      )
    })
  }

  semantics <- gepa_component_semantics()
  budget_summary <- optimizer_budget_summary(budget)
  optimized$config$compiled <- TRUE
  optimized$config$teleprompter <- "GEPA"
  optimized$config$optimizer <- list(
    selection = teleprompter@selection,
    population_size = teleprompter@population_size,
    generations = teleprompter@generations,
    optimization_mode = "component_candidates",
    flex_paths = gepa_flex_paths(program, mutable_only = FALSE),
    mutable_flex_paths = gepa_flex_paths(program),
    component_ids = names(gepa_program_component_specs(program)),
    component_semantics = semantics,
    best_candidate = if (is.null(best_record)) {
      NULL
    } else {
      gepa_component_candidate_params(best_record$candidate)
    },
    best_scores = final_scores,
    pareto_frontier = frontier,
    all_generations = all_generations,
    invalid_candidate_count = sum(
      !vapply(
        all_records,
        function(record) isTRUE(record$candidate_valid),
        logical(1)
      )
    ),
    error_count = budget_summary$total_errors,
    budget_summary = budget_summary,
    stop_reason = budget_summary$stop_reason,
    partial = optimizer_budget_stopped(budget)
  )

  if (!is.null(trial_log)) {
    trial_log$save()
  }
  optimized
}

gepa_component_record_selectable <- function(record) {
  complete <- isTRUE(record$complete)
  candidate_valid <- inherits(
    record$candidate,
    "dsprrr_gepa_component_candidate"
  ) &&
    isTRUE(record$candidate$valid)
  if (!is.null(record$candidate_valid)) {
    candidate_valid <- candidate_valid && isTRUE(record$candidate_valid)
  }
  complete && candidate_valid
}

gepa_component_candidate_params <- function(candidate) {
  gepa_validate_component_candidate(candidate)
  list(
    component_values = lapply(candidate$components, function(component) {
      list(
        kind = component$kind,
        path = component$path,
        value = component$value
      )
    }),
    candidate_valid = candidate$valid,
    failure = candidate$failure,
    history = candidate$history
  )
}

gepa_component_semantics <- function() {
  list(
    complete_component_candidates = TRUE,
    transactional_flex_binding = TRUE,
    whole_program_pareto_selection = TRUE,
    per_component_pareto_selection = FALSE,
    inference_time_search = FALSE,
    note = paste(
      "GEPA-lite ranks complete program candidates.",
      "It does not implement upstream GEPA's per-component Pareto frontier or inference-time search."
    )
  )
}
