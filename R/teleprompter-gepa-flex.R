# GEPA support for declarative and interpreter-backed Flex components.
#
# This file deliberately stays separate from the GEPA-lite generation loop. It
# supplies a typed candidate protocol that compile_gepa() can opt into when a
# program contains Flex leaves. Source binding stays transactional; executable
# source is evaluated only by the Flex interpreter bridge during candidate
# evaluation.

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
    if (inherits(parameter, "FlexModule")) {
      id <- gepa_component_id("module_src", path)
      components[[id]] <- list(
        id = id,
        kind = "module_src",
        path = path,
        value = parameter$module_src
      )
    } else if (inherits(parameter, "PredictModule")) {
      # A Flex source is one complete GEPA component. Its outer instructions
      # remain task context for reflection, but are not evolved independently
      # from the source that implements the module.
      id <- gepa_component_id("instructions", path)
      components[[id]] <- list(
        id = id,
        kind = "instructions",
        path = path,
        value = parameter$signature@instructions
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

gepa_component_candidate_id <- function(candidate) {
  gepa_validate_component_candidate(candidate)
  values <- lapply(candidate$components, function(component) {
    component[c("kind", "path", "value")]
  })
  paste0(
    "candidate:",
    substr(
      digest::digest(values, algo = "sha256", serialize = TRUE),
      1L,
      16L
    )
  )
}

gepa_component_candidate_lineage <- function(candidate) {
  gepa_validate_component_candidate(candidate)
  attr(candidate, "gepa_lineage", exact = TRUE) %||%
    list(
      parents = character(),
      ancestors = character(),
      ancestor = NULL,
      tag = "seed"
    )
}

gepa_set_component_candidate_lineage <- function(
  candidate,
  parents = character(),
  ancestors = character(),
  ancestor = NULL,
  tag
) {
  gepa_validate_component_candidate(candidate)
  parents <- unique(as.character(parents))
  ancestors <- unique(as.character(ancestors))
  if (
    anyNA(c(parents, ancestors)) ||
      any(!nzchar(c(parents, ancestors)))
  ) {
    cli::cli_abort(
      "GEPA candidate lineage IDs must be non-empty strings",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  attr(candidate, "gepa_lineage") <- list(
    parents = parents,
    ancestors = setdiff(ancestors, parents),
    ancestor = ancestor,
    tag = tag
  )
  candidate
}

gepa_component_selector_ids <- function(
  selector,
  candidate,
  failed_examples = list(),
  context = list()
) {
  gepa_validate_component_candidate(candidate)
  ids <- names(candidate$components)
  if (length(ids) == 0L) {
    return(character())
  }

  selected <- if (is.function(selector)) {
    selector(
      component_ids = ids,
      candidate = candidate,
      failed_examples = failed_examples,
      context = context
    )
  } else {
    if (
      !is.character(selector) ||
        length(selector) != 1L ||
        is.na(selector) ||
        !selector %in% c("round_robin", "all")
    ) {
      cli::cli_abort(
        c(
          "Invalid GEPA component selector",
          "i" = "Use {.val round_robin}, {.val all}, or a selector function."
        ),
        class = "dsprrr_gepa_component_selector_error"
      )
    }
    if (identical(selector, "all")) {
      ids
    } else {
      prior <- sum(vapply(
        candidate$history,
        function(entry) {
          is.list(entry) &&
            !is.null(entry$component_id) &&
            entry$component_id %in% ids
        },
        logical(1)
      ))
      ids[[(prior %% length(ids)) + 1L]]
    }
  }

  if (
    !is.character(selected) ||
      length(selected) == 0L ||
      anyNA(selected) ||
      any(!nzchar(selected)) ||
      anyDuplicated(selected) ||
      !all(selected %in% ids)
  ) {
    cli::cli_abort(
      "GEPA component selector must return one or more unique component IDs",
      class = "dsprrr_gepa_component_selector_error"
    )
  }
  selected
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
  .propagate_provider_errors = TRUE,
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
    .propagate_provider_errors = .propagate_provider_errors,
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

gepa_signature_contract <- function(signature) {
  inputs <- lapply(signature@inputs, function(input) {
    list(
      name = input$name,
      description = input$description %||% "",
      schema = ellmer_type_to_json_schema(input$type)
    )
  })
  outputs <- lapply(
    names(flex_signature_output_types(signature)),
    function(name) {
      schema <- ellmer_type_to_json_schema(
        flex_signature_output_types(signature)[[name]]
      )
      list(
        name = name,
        description = schema$description %||% "",
        schema = schema
      )
    }
  )
  list(
    instructions = signature@instructions,
    inputs = inputs,
    outputs = outputs
  )
}

gepa_flex_tool_contract <- function(tool, name) {
  if (is.function(tool)) {
    tool_formals <- as.list(formals(tool))
    formal_names <- names(tool_formals)
    defaults <- lapply(tool_formals, function(value) {
      text <- paste(deparse(value, width.cutoff = 120L), collapse = " ")
      if (nzchar(text)) text else NULL
    })
    names(defaults) <- formal_names
    return(list(
      name = name,
      kind = "function",
      arguments = formal_names,
      required = formal_names[
        vapply(
          defaults,
          is.null,
          logical(1)
        ) &
          formal_names != "..."
      ],
      defaults = Filter(Negate(is.null), defaults)
    ))
  }

  props <- tryCatch(S7::props(tool), error = function(error) list())
  arguments <- props$arguments %||% NULL
  list(
    name = name,
    kind = "tool_def",
    description = props$description %||% "",
    arguments_schema = if (is.null(arguments)) {
      NULL
    } else {
      ellmer_type_to_json_schema(arguments)
    }
  )
}

gepa_flex_contract <- function(module, program = module) {
  list(
    task_objective = program$signature@instructions,
    root_signature = gepa_signature_contract(program$signature),
    component_signature = gepa_signature_contract(module$signature),
    source_format = module$source_format,
    max_predictor_calls = module$max_predictor_calls,
    max_tool_calls = module$max_tool_calls,
    require_sandbox = module$require_sandbox,
    available_tools = lapply(names(module$tools), function(name) {
      gepa_flex_tool_contract(module$tools[[name]], name)
    }),
    available_primitives = c(
      "Predict",
      "ChainOfThought",
      "ReAct",
      "ReActV2",
      "RLM",
      "CodeAct",
      "ProgramOfThought",
      "Prediction",
      "Tool"
    )
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
  contract <- gepa_flex_json(gepa_flex_contract(module, program = program))
  failures <- gepa_flex_json(gepa_flex_failure_bundles(
    failed_examples,
    program = program,
    component_path = component_path
  ))
  common <- c(
    "Return a JSON object field named module_src containing one complete source string.",
    "Return the complete source, not a patch or Markdown fence.",
    "The task objective, signature instructions, field descriptions, types, runtime policy, tools, and call limits are authoritative:",
    contract,
    paste0("Target component graph path: ", component_path),
    "Current complete source:",
    module$module_src,
    "Row-aligned failure bundles keep component observations, root-program truth and prediction, and metric feedback together.",
    "For a nested component, pipeline_step evidence is the observed input/output at its containing pipeline step; root_program evidence is explicitly labeled context, not a claimed leaf-level target.",
    failures
  )
  if (identical(module$source_format, "r")) {
    return(paste(
      c(
        "You are proposing one complete executable R Flex module source.",
        common,
        "The source must define a top-level forward <- function(...) whose named arguments match the outer inputs and whose result supplies every outer output field.",
        "Optimizer-authored source runs only in a fresh configured interpreter. Keep all loops and recursion explicitly bounded and do not rely on files, packages, network access, environment variables, global state, or the host dsprrr namespace.",
        "The guest DSL constructors Predict, ChainOfThought, ReAct, ReActV2, RLM, CodeAct, and ProgramOfThought return callable predictor functions. Use string-form signatures or \"$outer\". Prediction(...) constructs a named result.",
        "Explicit host tools may be called by their listed names or through Tool(\"name\") across the versioned bridge. No other host functions are available.",
        "Predictor calls count against max_predictor_calls; host-tool calls count against max_tool_calls; direct deterministic R consumes neither budget. Values crossing the bridge must be JSON-compatible.",
        "Propose complete R source that addresses the failures and remains within this restricted DSL and runtime policy."
      ),
      collapse = "\n"
    ))
  }
  paste(
    c(
      "You are proposing one complete declarative Flex module source.",
      common,
      "The module_src value itself must be canonicalizable JSON data, not an R expression, function, or executable code.",
      "The source is data only and must use exactly this version 1 schema:",
      '{"schema_version":1,"steps":[{"name":"safe_name","primitive":"predict or chain_of_thought","signature":"$outer or DSPy input -> output","instructions":"optional string","inputs":{"target":"$input.name or $step.earlier.field"}}],"outputs":{"every_outer_output":"$step.name.field"}}',
      "Top-level fields and step fields not shown above are forbidden.",
      "Step names must be unique safe identifiers. Steps are ordered, acyclic, and may reference only outer inputs or earlier step outputs.",
      "Every step input and every outer output must appear exactly once and have a compatible type.",
      "Propose a complete source that addresses the failures while staying within the exact schema and call limit."
    ),
    collapse = "\n"
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
      description = if (identical(module$source_format, "r")) {
        "Complete executable R Flex source defining forward()"
      } else {
        "Complete canonicalizable Flex schema version 1 JSON"
      }
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
    result <- .llm$chat_structured(prompt, type = type, echo = "none")
  }

  if (!is.null(provider_condition)) {
    # The budget ledger has already recorded this provider outcome. Re-signal
    # the original condition so infrastructure outages cannot masquerade as a
    # valid unchanged candidate.
    stop(provider_condition)
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

gepa_atomic_invalid_candidate <- function(original, staged, failure) {
  original$valid <- FALSE
  original$failure <- failure
  original$history <- staged$history
  original
}

gepa_propose_component_instructions <- function(
  instruction,
  failed_examples,
  .llm = NULL,
  verbose = FALSE,
  budget = NULL,
  unit_id = "gepa:component_reflection"
) {
  if (is.null(.llm) || !is.function(.llm$chat_structured)) {
    return(gepa_fallback_mutation(instruction, failed_examples))
  }

  prompt <- gepa_reflection_prompt(instruction, failed_examples)
  type <- ellmer::type_object(instructions = ellmer::type_string())
  if (is.null(budget)) {
    result <- .llm$chat_structured(prompt, type = type, echo = "none")
  } else {
    request <- optimizer_budgeted_provider_call(
      budget = budget,
      model = .llm,
      stage = "gepa_reflection",
      unit_id = unit_id,
      call = function() {
        .llm$chat_structured(prompt, type = type, echo = "none")
      },
      success = function(value, condition) {
        is.null(condition) &&
          is.list(value) &&
          is.character(value$instructions) &&
          length(value$instructions) == 1L &&
          !is.na(value$instructions)
      }
    )
    if (!request$started) {
      return(instruction)
    }
    if (!is.null(request$condition)) {
      stop(request$condition)
    }
    result <- request$value
  }

  value <- if (is.list(result)) result[["instructions"]] else NULL
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value)
  ) {
    if (isTRUE(verbose)) {
      cli::cli_warn(
        "GEPA instruction proposer returned an invalid structured response"
      )
    }
    return(NULL)
  }
  value
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
  component_selector = "round_robin",
  selector_context = list(),
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
  component_ids <- if (is.null(component_id)) {
    gepa_component_selector_ids(
      component_selector,
      candidate,
      failed_examples = failed_examples,
      context = selector_context
    )
  } else {
    component_id
  }
  if (
    !is.character(component_ids) ||
      length(component_ids) == 0L ||
      anyNA(component_ids) ||
      anyDuplicated(component_ids) ||
      !all(component_ids %in% ids)
  ) {
    cli::cli_abort(
      "{.arg component_id} must identify one or more unique candidate components",
      class = "dsprrr_gepa_component_error"
    )
  }
  parent_id <- gepa_component_candidate_id(candidate)
  parent_lineage <- gepa_component_candidate_lineage(candidate)
  original <- candidate

  for (component_index in seq_along(component_ids)) {
    if (!is.null(budget) && optimizer_budget_stopped(budget)) {
      return(original)
    }
    component_id <- component_ids[[component_index]]
    component_unit_id <- if (length(component_ids) == 1L) {
      unit_id
    } else {
      paste0(unit_id, ":component:", component_index)
    }
    materialized <- gepa_materialize_component_candidate(
      program,
      candidate,
      failure_score = failure_score
    )
    if (!materialized$ok) {
      return(gepa_atomic_invalid_candidate(
        original,
        candidate,
        list(
          error_class = materialized$error_class,
          error_message = materialized$error_message
        )
      ))
    }
    component <- candidate$components[[component_id]]

    if (identical(component$kind, "instructions")) {
      value <- gepa_propose_component_instructions(
        component$value,
        failed_examples = failed_examples,
        .llm = .llm,
        verbose = verbose,
        budget = budget,
        unit_id = component_unit_id
      )
      if (
        !is.character(value) ||
          length(value) != 1L ||
          is.na(value)
      ) {
        condition <- gepa_candidate_failure_condition(
          "GEPA instruction proposer returned an invalid value"
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
              component_unit_id
            )
          ))
        )
        return(gepa_atomic_invalid_candidate(
          original,
          candidate,
          list(
            error_class = class(condition)[1L],
            error_message = conditionMessage(condition)
          )
        ))
      }
      candidate$components[[component_id]]$value <- value
      candidate$history <- append(
        candidate$history,
        list(gepa_component_history_entry(
          component,
          "proposed",
          gepa_flex_proposal(
            "proposed",
            value,
            unit_id = component_unit_id
          )
        ))
      )
      if (
        component_index < length(component_ids) &&
          !is.null(budget) &&
          optimizer_budget_stopped(budget)
      ) {
        return(original)
      }
      next
    }

    target <- gepa_component_target(materialized$program, component$path)
    proposal <- gepa_propose_flex_source(
      target,
      failed_examples = failed_examples,
      .llm = .llm,
      verbose = verbose,
      budget = budget,
      unit_id = component_unit_id,
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
    if (identical(proposal$status, "unchanged_budget_stop")) {
      return(original)
    }
    if (!proposal$valid) {
      return(gepa_atomic_invalid_candidate(
        original,
        candidate,
        list(
          error_class = proposal$error_class,
          error_message = proposal$error_message
        )
      ))
    }
    candidate$components[[component_id]]$value <- proposal$module_src
  }

  child_id <- gepa_component_candidate_id(candidate)
  if (!identical(child_id, parent_id)) {
    candidate <- gepa_set_component_candidate_lineage(
      candidate,
      parents = parent_id,
      ancestors = c(parent_lineage$parents, parent_lineage$ancestors),
      tag = "reflective_mutation"
    )
  }
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
    list(list(
      status = "component_crossover",
      parent_ids = c(
        gepa_component_candidate_id(parent1),
        gepa_component_candidate_id(parent2)
      )
    ))
  )
  gepa_set_component_candidate_lineage(
    child,
    parents = c(
      gepa_component_candidate_id(parent1),
      gepa_component_candidate_id(parent2)
    ),
    ancestors = c(
      gepa_component_candidate_lineage(parent1)$parents,
      gepa_component_candidate_lineage(parent1)$ancestors,
      gepa_component_candidate_lineage(parent2)$parents,
      gepa_component_candidate_lineage(parent2)$ancestors
    ),
    tag = "component_crossover"
  )
}

gepa_component_candidate_registry <- function(records) {
  registry <- list()
  for (record in records) {
    candidate <- record$candidate %||% NULL
    if (!inherits(candidate, "dsprrr_gepa_component_candidate")) {
      next
    }
    id <- gepa_component_candidate_id(candidate)
    if (is.null(registry[[id]])) {
      registry[[id]] <- list(
        candidate = candidate,
        score = as.numeric(record$scores[[1L]] %||% NA_real_),
        generation = as.integer(record$generation %||% 0L),
        index = as.integer(record$index %||% 0L)
      )
    }
  }
  registry
}

gepa_component_candidate_ancestors <- function(candidate, registry) {
  lineage <- gepa_component_candidate_lineage(candidate)
  pending <- c(lineage$parents, lineage$ancestors)
  found <- character()
  while (length(pending) > 0L) {
    id <- pending[[1L]]
    pending <- pending[-1L]
    if (id %in% found) {
      next
    }
    found <- c(found, id)
    ancestor <- registry[[id]]$candidate %||% NULL
    if (inherits(ancestor, "dsprrr_gepa_component_candidate")) {
      pending <- c(
        pending,
        gepa_component_candidate_lineage(ancestor)$parents,
        gepa_component_candidate_lineage(ancestor)$ancestors
      )
    }
  }
  found
}

gepa_merge_component_candidates <- function(
  parent1,
  parent2,
  registry,
  parent1_score,
  parent2_score
) {
  gepa_validate_component_candidate(parent1)
  gepa_validate_component_candidate(parent2)
  parent1_id <- gepa_component_candidate_id(parent1)
  parent2_id <- gepa_component_candidate_id(parent2)
  ancestors1 <- gepa_component_candidate_ancestors(parent1, registry)
  ancestors2 <- gepa_component_candidate_ancestors(parent2, registry)
  if (parent1_id %in% ancestors2 || parent2_id %in% ancestors1) {
    return(NULL)
  }
  common <- intersect(ancestors1, ancestors2)
  if (length(common) == 0L) {
    return(NULL)
  }
  order_key <- vapply(
    common,
    function(id) {
      entry <- registry[[id]]
      1000000 * (entry$generation %||% 0L) + (entry$index %||% 0L)
    },
    numeric(1)
  )
  ancestor_id <- common[[which.max(order_key)]]
  ancestor_entry <- registry[[ancestor_id]]
  ancestor <- ancestor_entry$candidate %||% NULL
  if (!inherits(ancestor, "dsprrr_gepa_component_candidate")) {
    return(NULL)
  }
  ancestor_score <- ancestor_entry$score %||% NA_real_
  if (
    anyNA(c(ancestor_score, parent1_score, parent2_score)) ||
      ancestor_score > parent1_score ||
      ancestor_score > parent2_score
  ) {
    return(NULL)
  }
  if (
    !identical(names(parent1$components), names(parent2$components)) ||
      !identical(names(parent1$components), names(ancestor$components))
  ) {
    return(NULL)
  }

  values <- function(candidate) {
    vapply(candidate$components, `[[`, character(1), "value")
  }
  ancestor_values <- values(ancestor)
  left_values <- values(parent1)
  right_values <- values(parent2)
  desirable <- (left_values == ancestor_values) !=
    (right_values == ancestor_values)
  if (!any(desirable)) {
    return(NULL)
  }

  child <- ancestor
  for (id in names(child$components)) {
    ancestor_value <- ancestor_values[[id]]
    left <- left_values[[id]]
    right <- right_values[[id]]
    selected <- if (identical(left, right)) {
      left
    } else if (identical(left, ancestor_value)) {
      right
    } else if (identical(right, ancestor_value)) {
      left
    } else if (parent1_score > parent2_score) {
      left
    } else if (parent2_score > parent1_score) {
      right
    } else if (stats::runif(1L) < 0.5) {
      left
    } else {
      right
    }
    child$components[[id]]$value <- selected
  }
  child$valid <- TRUE
  child["failure"] <- list(NULL)
  child$history <- append(
    child$history,
    list(list(
      status = "lineage_merge",
      parent_ids = c(parent1_id, parent2_id),
      ancestor_id = ancestor_id
    ))
  )
  gepa_set_component_candidate_lineage(
    child,
    parents = c(parent1_id, parent2_id),
    ancestors = c(
      gepa_component_candidate_lineage(parent1)$parents,
      gepa_component_candidate_lineage(parent1)$ancestors,
      gepa_component_candidate_lineage(parent2)$parents,
      gepa_component_candidate_lineage(parent2)$ancestors,
      ancestor_id
    ),
    ancestor = ancestor_id,
    tag = "lineage_merge"
  )
}

gepa_initial_component_population <- function(
  program,
  population_size,
  .llm = NULL,
  verbose = FALSE,
  budget = NULL,
  failure_score = 0,
  component_selector = "round_robin"
) {
  if (
    !is.numeric(population_size) ||
      length(population_size) != 1L ||
      is.na(population_size) ||
      !is.finite(population_size) ||
      population_size < 1 ||
      population_size != floor(population_size)
  ) {
    cli::cli_abort(
      "{.arg population_size} must be one positive integer",
      class = "dsprrr_gepa_component_error"
    )
  }
  population_size <- as.integer(population_size)
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
      component_id = if (identical(component_selector, "round_robin")) {
        component_id
      } else {
        NULL
      },
      component_selector = component_selector,
      selector_context = list(
        phase = "initialization",
        generation = 0L,
        index = index
      ),
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

gepa_component_parent_records <- function(records, selection) {
  records <- Filter(gepa_component_record_selectable, records)
  if (!identical(selection, "pareto") || length(records) <= 1L) {
    return(records)
  }
  frontier <- gepa_component_validation_result(
    records,
    track_best_outputs = FALSE
  )$per_val_instance_best_candidates
  candidate_ids <- unique(unlist(frontier, use.names = FALSE))
  scores_matrix <- do.call(
    rbind,
    lapply(records, function(record) record$scores)
  )
  objective_ids <- vapply(
    records[pareto_frontier(scores_matrix)],
    function(record) {
      record$candidate_id %||%
        gepa_component_candidate_id(record$candidate)
    },
    character(1)
  )
  candidate_ids <- union(candidate_ids, objective_ids)
  if (length(candidate_ids) == 0L) {
    return(records)
  }
  selected <- Filter(
    function(record) {
      (record$candidate_id %||%
        gepa_component_candidate_id(record$candidate)) %in%
        candidate_ids
    },
    records
  )
  if (length(selected) > 0L) selected else records
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
  failure_score = 0,
  component_selector = "round_robin",
  use_merge = TRUE,
  max_merges = Inf,
  all_records = records
) {
  records <- gepa_component_parent_records(records, selection)
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

  registry <- gepa_component_candidate_registry(all_records)
  merge_invocations <- 0L
  lineage_merges <- 0L
  population <- vector("list", population_size)
  for (index in seq_len(population_size)) {
    if (!is.null(budget) && optimizer_budget_stopped(budget)) {
      break
    }
    parent1 <- gepa_component_select_parent(records, ranks, crowding)
    parent2 <- gepa_component_select_parent(records, ranks, crowding)
    child <- parent1$candidate
    if (stats::runif(1L) < crossover_rate) {
      merged <- NULL
      if (isTRUE(use_merge) && merge_invocations < max_merges) {
        merge_invocations <- merge_invocations + 1L
        merged <- gepa_merge_component_candidates(
          parent1$candidate,
          parent2$candidate,
          registry = registry,
          parent1_score = parent1$scores[[1L]],
          parent2_score = parent2$scores[[1L]]
        )
      }
      if (is.null(merged)) {
        child <- gepa_crossover_component_candidates(
          parent1$candidate,
          parent2$candidate
        )
      } else {
        child <- merged
        lineage_merges <- lineage_merges + 1L
      }
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
        component_selector = component_selector,
        selector_context = list(
          phase = "search",
          generation = generation,
          index = index,
          parent_ids = c(
            gepa_component_candidate_id(parent1$candidate),
            gepa_component_candidate_id(parent2$candidate)
          )
        ),
        failure_score = failure_score
      )
    }
    population[[index]] <- child
  }
  population <- Filter(Negate(is.null), population)
  attr(population, "merge_invocations") <- merge_invocations
  attr(population, "lineage_merges") <- lineage_merges
  population
}

gepa_teleprompter_property <- function(teleprompter, name, default) {
  tryCatch(
    S7::prop(teleprompter, name),
    error = function(error) default
  )
}

gepa_component_selector_label <- function(selector) {
  if (is.function(selector)) "custom" else selector
}

gepa_unique_component_records <- function(records) {
  unique <- list()
  for (record in records) {
    id <- record$candidate_id %||%
      gepa_component_candidate_id(record$candidate)
    if (is.null(unique[[id]])) {
      record$candidate_id <- id
      unique[[id]] <- record
    }
  }
  unname(unique)
}

gepa_component_validation_result <- function(
  records,
  track_best_outputs = TRUE
) {
  records <- gepa_unique_component_records(
    Filter(gepa_component_record_selectable, records)
  )
  if (length(records) == 0L) {
    return(list(
      candidates = list(),
      parents = list(),
      val_aggregate_scores = numeric(),
      val_subscores = list(),
      per_val_instance_best_candidates = list(),
      validation_frontier_scores = numeric(),
      best_outputs_valset = if (isTRUE(track_best_outputs)) list() else NULL,
      discovery_eval_counts = integer()
    ))
  }

  candidates <- list()
  parents <- list()
  aggregate_scores <- numeric()
  val_subscores <- list()
  outputs <- list()
  discovery_eval_counts <- integer()
  row_order <- character()

  for (record in records) {
    id <- record$candidate_id
    candidates[[id]] <- gepa_component_candidate_params(record$candidate)
    parents[[id]] <- record$parents %||% character()
    aggregate_scores[[id]] <- as.numeric(record$scores[[1L]])
    discovery_eval_counts[[id]] <- as.integer(
      record$discovery_metric_calls %||% 0L
    )
    evaluation <- record$primary_eval
    if (!S7::S7_inherits(evaluation, EvalResult)) {
      val_subscores[[id]] <- numeric()
      outputs[[id]] <- list()
      next
    }
    examples <- evaluation@examples
    row_ids <- if ("row_id" %in% names(examples)) {
      as.character(examples$row_id)
    } else {
      as.character(seq_len(nrow(examples)))
    }
    row_order <- unique(c(row_order, row_ids))
    scores <- as.numeric(examples$score)
    names(scores) <- row_ids
    val_subscores[[id]] <- scores
    predicted <- if ("predicted" %in% names(examples)) {
      examples$predicted
    } else {
      rep(list(NA), length(row_ids))
    }
    names(predicted) <- row_ids
    outputs[[id]] <- predicted
  }

  winners <- list()
  frontier_scores <- numeric()
  best_outputs <- if (isTRUE(track_best_outputs)) list() else NULL
  for (row_id in row_order) {
    scores <- vapply(
      val_subscores,
      function(candidate_scores) {
        candidate_scores[[row_id]] %||% NA_real_
      },
      numeric(1)
    )
    available <- which(!is.na(scores))
    if (length(available) == 0L) {
      winners[[row_id]] <- character()
      frontier_scores[[row_id]] <- NA_real_
      if (isTRUE(track_best_outputs)) {
        best_outputs[[row_id]] <- list()
      }
      next
    }
    best <- max(scores[available])
    winner_ids <- names(scores)[available[scores[available] == best]]
    winners[[row_id]] <- winner_ids
    frontier_scores[[row_id]] <- best
    if (isTRUE(track_best_outputs)) {
      best_outputs[[row_id]] <- lapply(winner_ids, function(id) {
        list(candidate_id = id, output = outputs[[id]][[row_id]])
      })
    }
  }

  list(
    candidates = candidates,
    parents = parents,
    val_aggregate_scores = aggregate_scores,
    val_subscores = val_subscores,
    per_val_instance_best_candidates = winners,
    validation_frontier_scores = frontier_scores,
    best_outputs_valset = best_outputs,
    discovery_eval_counts = discovery_eval_counts
  )
}

compile_gepa_components <- function(
  teleprompter,
  program,
  discovery_dataset,
  validation_dataset,
  metrics,
  metric_names,
  .llm,
  control,
  budget,
  trial_log = NULL
) {
  if (!is.data.frame(discovery_dataset)) {
    cli::cli_abort(
      "{.arg trainset} must be a data frame",
      class = "dsprrr_gepa_config_error"
    )
  }
  if (!is.data.frame(validation_dataset)) {
    cli::cli_abort(
      "{.arg valset} must be a data frame or NULL",
      class = "dsprrr_gepa_config_error"
    )
  }
  component_selector <- gepa_teleprompter_property(
    teleprompter,
    "component_selector",
    "round_robin"
  )
  use_merge <- isTRUE(gepa_teleprompter_property(
    teleprompter,
    "use_merge",
    TRUE
  ))
  max_merge_invocations <- gepa_teleprompter_property(
    teleprompter,
    "max_merge_invocations",
    5L
  )
  track_best_outputs <- isTRUE(gepa_teleprompter_property(
    teleprompter,
    "track_best_outputs",
    TRUE
  ))
  if (is.null(max_merge_invocations)) {
    max_merge_invocations <- Inf
  } else {
    if (
      !is.numeric(max_merge_invocations) ||
        length(max_merge_invocations) != 1L ||
        is.na(max_merge_invocations) ||
        !is.finite(max_merge_invocations) ||
        max_merge_invocations < 0 ||
        max_merge_invocations != floor(max_merge_invocations)
    ) {
      cli::cli_abort(
        "GEPA max_merge_invocations must be one non-negative integer or NULL",
        class = "dsprrr_gepa_component_error"
      )
    }
    max_merge_invocations <- as.integer(max_merge_invocations)
  }
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
      component_selector = gepa_component_selector_label(component_selector),
      use_merge = use_merge,
      max_merge_invocations = if (is.infinite(max_merge_invocations)) {
        NULL
      } else {
        max_merge_invocations
      },
      merge_invocations = 0L,
      successful_merge_count = 0L,
      track_best_outputs = track_best_outputs,
      optimization_skipped = TRUE,
      skip_reason = "no_mutable_components",
      flex_paths = gepa_flex_paths(program, mutable_only = FALSE),
      mutable_flex_paths = character(),
      component_ids = character(),
      component_semantics = gepa_component_semantics(track_best_outputs),
      best_candidate = NULL,
      best_candidate_id = NULL,
      best_scores = stats::setNames(
        rep(NA_real_, length(metrics)),
        metric_names
      ),
      candidates = list(),
      parents = list(),
      val_aggregate_scores = numeric(),
      val_subscores = list(),
      per_val_instance_best_candidates = list(),
      validation_frontier_scores = numeric(),
      best_outputs_valset = if (track_best_outputs) list() else NULL,
      discovery_eval_counts = integer(),
      objective_pareto_front = list(),
      pareto_frontier = list(),
      resume_supported = FALSE,
      search_state = list(
        schema_version = 1L,
        generations_run = 0L,
        candidate_ids = character(),
        next_generation = 1L
      ),
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
    failure_score = failure_score,
    component_selector = component_selector
  )

  all_generations <- list()
  all_records <- list()
  selectable_records <- list()
  best_record <- NULL
  merge_invocations <- 0L
  lineage_merges <- 0L
  generations_run <- 0L

  for (generation in seq_len(teleprompter@generations)) {
    generations_run <- generation
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
      discovery_eval <- NULL
      discovery_metric_calls <- 0L
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
          validation_dataset,
          metric = metrics[[metric_index]],
          .llm = .llm,
          control = control,
          budget = budget,
          stage = paste0("gepa_metric_", metric_index),
          unit_id = metric_unit_ids[[metric_index]],
          failure_score = failure_score,
          .propagate_provider_errors = TRUE
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
          if (identical(discovery_dataset, validation_dataset)) {
            discovery_eval <- eval_result
            discovery_metric_calls <- as.integer(eval_result@metric_calls)
          }
        }
      }

      if (
        isTRUE(candidate_valid) &&
          is.null(discovery_eval) &&
          !optimizer_budget_stopped(budget)
      ) {
        discovery_unit_id <- paste0(
          "gepa:generation:",
          generation,
          ":candidate:",
          index,
          ":discovery"
        )
        discovery <- gepa_evaluate_component_candidate(
          program,
          candidate,
          discovery_dataset,
          metric = metrics[[1L]],
          .llm = .llm,
          control = control,
          budget = budget,
          stage = "gepa_discovery_metric",
          unit_id = discovery_unit_id,
          failure_score = failure_score,
          .propagate_provider_errors = TRUE
        )
        discovery_eval <- discovery$eval_result
        if (!S7::S7_inherits(discovery_eval, EvalResult)) {
          cli::cli_abort(
            "GEPA discovery evaluation returned an invalid result",
            class = "dsprrr_optimizer_invariant_error"
          )
        }
        discovery_metric_calls <- as.integer(discovery_eval@metric_calls)
      }

      if (S7::S7_inherits(discovery_eval, EvalResult)) {
        failed_examples <- gepa_failed_examples(
          discovery_eval,
          discovery_dataset,
          program$signature,
          threshold = teleprompter@metric_threshold,
          output_col = get_metric_field(metrics[[1L]])
        )
      }

      record <- list(
        candidate_id = gepa_component_candidate_id(candidate),
        candidate = candidate,
        parents = gepa_component_candidate_lineage(candidate)$parents,
        lineage = gepa_component_candidate_lineage(candidate),
        candidate_valid = candidate_valid,
        scores = stats::setNames(scores, metric_names),
        primary_eval = primary_eval,
        discovery_eval = discovery_eval,
        failed_examples = failed_examples,
        generation = generation,
        index = index,
        discovery_metric_calls = discovery_metric_calls,
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
      selectable_records,
      program = program,
      population_size = teleprompter@population_size,
      mutation_rate = teleprompter@mutation_rate,
      crossover_rate = teleprompter@crossover_rate,
      selection = teleprompter@selection,
      .llm = .llm,
      verbose = teleprompter@verbose,
      budget = budget,
      generation = generation + 1L,
      failure_score = failure_score,
      component_selector = component_selector,
      use_merge = use_merge,
      max_merges = max(
        0,
        max_merge_invocations - merge_invocations
      ),
      all_records = selectable_records
    )
    merge_invocations <- merge_invocations +
      (attr(population, "merge_invocations", exact = TRUE) %||% 0L)
    lineage_merges <- lineage_merges +
      (attr(population, "lineage_merges", exact = TRUE) %||% 0L)
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
  objective_frontier <- list()
  if (length(selectable_records) > 0L) {
    objective_records <- gepa_unique_component_records(selectable_records)
    scores_matrix <- do.call(
      rbind,
      lapply(objective_records, function(record) record$scores)
    )
    frontier_index <- pareto_frontier(scores_matrix)
    objective_frontier <- lapply(
      objective_records[frontier_index],
      function(record) {
        list(
          candidate_id = record$candidate_id,
          candidate = gepa_component_candidate_params(record$candidate),
          scores = record$scores
        )
      }
    )
  }

  validation_result <- gepa_component_validation_result(
    selectable_records,
    track_best_outputs = track_best_outputs
  )
  semantics <- gepa_component_semantics(track_best_outputs)
  budget_summary <- optimizer_budget_summary(budget)
  optimized$config$compiled <- TRUE
  optimized$config$teleprompter <- "GEPA"
  optimized$config$optimizer <- list(
    selection = teleprompter@selection,
    population_size = teleprompter@population_size,
    generations = teleprompter@generations,
    optimization_mode = "component_candidates",
    component_selector = gepa_component_selector_label(component_selector),
    use_merge = use_merge,
    max_merge_invocations = if (is.infinite(max_merge_invocations)) {
      NULL
    } else {
      max_merge_invocations
    },
    merge_invocations = merge_invocations,
    successful_merge_count = lineage_merges,
    track_best_outputs = track_best_outputs,
    flex_paths = gepa_flex_paths(program, mutable_only = FALSE),
    mutable_flex_paths = gepa_flex_paths(program),
    component_ids = names(gepa_program_component_specs(program)),
    component_semantics = semantics,
    best_candidate = if (is.null(best_record)) {
      NULL
    } else {
      gepa_component_candidate_params(best_record$candidate)
    },
    best_candidate_id = best_record$candidate_id %||% NULL,
    best_scores = final_scores,
    candidates = validation_result$candidates,
    parents = validation_result$parents,
    val_aggregate_scores = validation_result$val_aggregate_scores,
    val_subscores = validation_result$val_subscores,
    per_val_instance_best_candidates = validation_result$per_val_instance_best_candidates,
    validation_frontier_scores = validation_result$validation_frontier_scores,
    best_outputs_valset = validation_result$best_outputs_valset,
    discovery_eval_counts = validation_result$discovery_eval_counts,
    objective_pareto_front = objective_frontier,
    # Compatibility alias for releases that exposed the multi-metric front as
    # `pareto_frontier`; the validation-instance frontier is recorded above.
    pareto_frontier = objective_frontier,
    resume_supported = FALSE,
    search_state = list(
      schema_version = 1L,
      generations_run = generations_run,
      candidate_ids = names(validation_result$candidates),
      next_generation = min(
        teleprompter@generations + 1L,
        generations_run + 1L
      )
    ),
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

gepa_component_semantics <- function(track_best_outputs = FALSE) {
  list(
    complete_program_candidates = TRUE,
    flex_source_is_single_component = TRUE,
    transactional_flex_binding = TRUE,
    validation_instance_frontier = TRUE,
    validation_instance_candidate_selection = TRUE,
    objective_frontier = TRUE,
    component_selection = c("round_robin", "all", "custom"),
    lineage_aware_merge = TRUE,
    supports_retained_best_outputs = TRUE,
    retained_best_outputs = isTRUE(track_best_outputs),
    inference_time_candidate_selection = FALSE,
    fine_grained_resume = FALSE,
    note = paste(
      "GEPA ranks complete program candidates and records the best candidate",
      "set for each validation example. Inference-time candidate selection and",
      "fine-grained checkpoint resume are not yet implemented."
    )
  )
}
