# GEPA resumable search state
#
# Checkpointed GEPA uses a closed, data-only state machine. The existing GEPA
# loop remains the path for runs without optimizer checkpoints.

gepa_checkpoint_abort_unsupported <- function(message, path = NULL) {
  details <- c(
    "GEPA checkpoint resume is unsupported for this configuration",
    "i" = message
  )
  if (!is.null(path)) {
    details <- c(details, "x" = "Unsupported value at {.field {path}}.")
  }
  cli::cli_abort(
    details,
    class = "dsprrr_optimizer_checkpoint_unsupported_optimizer"
  )
}

gepa_checkpoint_chat_identity <- function(chat, path) {
  if (!cache_is_trusted_ellmer_chat(chat)) {
    gepa_checkpoint_abort_unsupported(
      "Use an unmodified ellmer Chat so GEPA can fingerprint its request configuration.",
      path
    )
  }
  turns <- cache_chat_get(
    chat,
    "get_turns",
    required = TRUE
  )
  if (!is.list(turns) || length(turns) > 0L) {
    gepa_checkpoint_abort_unsupported(
      "Checkpointed GEPA requires an empty initial Chat history.",
      path
    )
  }
  tools <- cache_chat_get(
    chat,
    "get_tools",
    required = TRUE
  )
  if (!is.list(tools) || length(tools) > 0L) {
    gepa_checkpoint_abort_unsupported(
      "Checkpointed GEPA does not support Chats with registered tools.",
      path
    )
  }
  system_prompt <- cache_chat_get(
    chat,
    "get_system_prompt",
    required = TRUE
  )
  list(
    kind = "ellmer_chat_v1",
    provider = cache_provider_fingerprint(chat),
    system_prompt = cache_opaque_value(system_prompt),
    tools = list(),
    ellmer_version = as.character(utils::packageVersion("ellmer"))
  )
}

gepa_checkpoint_leaf_modules <- function(program) {
  graph <- module_graph(program, boundaries = "cross", cycles = "error")
  leaf <- vapply(
    graph$path,
    function(path) {
      prefix <- if (identical(path, "$")) "$/" else paste0(path, "/")
      !any(startsWith(graph$path, prefix))
    },
    logical(1)
  )
  graph[leaf, , drop = FALSE]
}

gepa_checkpoint_attach_default_chats <- function(program) {
  graph <- gepa_checkpoint_leaf_modules(program)
  needs_default <- vapply(
    graph$module,
    function(module) {
      is.null(tryCatch(module$chat, error = function(e) NULL))
    },
    logical(1)
  )
  if (!any(needs_default)) {
    return(program)
  }
  default_chat <- get_default_chat(create = FALSE)
  if (is.null(default_chat)) {
    cli::cli_abort(
      "No default Chat is available for GEPA checkpoint row evaluation",
      class = "dsprrr_optimizer_checkpoint_fingerprint_error"
    )
  }
  for (index in which(needs_default)) {
    graph$module[[index]]$chat <- clone_ellmer_chat(
      default_chat,
      arg = "default_chat",
      reset_turns = TRUE
    )
  }
  program
}

gepa_checkpoint_task_runtime_identity <- function(program, .llm) {
  if (!is.null(.llm)) {
    return(list(
      kind = "override",
      runtime = gepa_checkpoint_chat_identity(.llm, "task_runtime.override")
    ))
  }

  graph <- gepa_checkpoint_leaf_modules(program)
  default_chat <- get_default_chat(create = FALSE)
  identities <- lapply(seq_len(nrow(graph)), function(index) {
    module <- graph$module[[index]]
    chat <- tryCatch(module$chat, error = function(e) NULL) %||% default_chat
    if (is.null(chat)) {
      cli::cli_abort(
        c(
          "No effective task Chat is available for GEPA checkpointing",
          "x" = "{.field {paste0('task_runtime.', graph$path[[index]])}} has no attached or configured default Chat.",
          "i" = "Attach an ellmer Chat, pass {.arg .llm}, or configure a default Chat."
        ),
        class = "dsprrr_optimizer_checkpoint_fingerprint_error"
      )
    }
    gepa_checkpoint_chat_identity(
      chat,
      paste0("task_runtime.", graph$path[[index]])
    )
  })
  names(identities) <- graph$path
  list(kind = "program", nodes = identities)
}

gepa_checkpoint_supported_program <- function(program) {
  modules <- named_modules(
    program,
    include_root = TRUE,
    aliases = FALSE,
    boundaries = "cross",
    cycles = "error"
  )
  classes <- vapply(modules, function(module) class(module)[[1L]], character(1))
  unsupported <- which(!classes %in% c("PredictModule", "PipelineModule"))
  if (length(unsupported) > 0L) {
    gepa_checkpoint_abort_unsupported(
      paste(
        "Checkpointed GEPA supports Predict leaves and built-in predictor",
        "pipelines only."
      ),
      names(modules)[unsupported[[1L]]]
    )
  }
  specs <- gepa_program_component_specs(program)
  if (
    any(vapply(
      specs,
      function(spec) !identical(spec$kind, "instructions"),
      logical(1)
    ))
  ) {
    gepa_checkpoint_abort_unsupported(
      "Flex source components are not supported by the first GEPA resume slice."
    )
  }
  if (length(specs) == 0L) {
    return(specs)
  }
  if (
    any(vapply(
      specs,
      function(spec) {
        !is.character(spec$value) ||
          length(spec$value) != 1L ||
          is.na(spec$value)
      },
      logical(1)
    ))
  ) {
    gepa_checkpoint_abort_unsupported(
      "Only scalar text instructions can be resumed."
    )
  }
  specs
}

gepa_checkpoint_encode_candidate <- function(candidate) {
  gepa_validate_component_candidate(candidate)
  list(
    components = candidate$components,
    valid = candidate$valid,
    failure = candidate$failure,
    history = candidate$history,
    lineage = gepa_component_candidate_lineage(candidate)
  )
}

gepa_checkpoint_decode_candidate <- function(
  value,
  program,
  component_specs
) {
  expected <- c("components", "valid", "failure", "history", "lineage")
  if (
    !is.list(value) ||
      !identical(names(value), expected) ||
      !is.list(value$components) ||
      !identical(names(value$components), names(component_specs)) ||
      !is.logical(value$valid) ||
      length(value$valid) != 1L ||
      is.na(value$valid) ||
      !is.list(value$history) ||
      !is.list(value$lineage) ||
      !identical(
        names(value$lineage),
        c("parents", "ancestors", "ancestor", "tag")
      )
  ) {
    cli::cli_abort(
      "GEPA checkpoint candidate is malformed",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  lineage <- value$lineage
  if (
    !is.character(lineage$parents) ||
      anyNA(lineage$parents) ||
      !all(nzchar(lineage$parents)) ||
      anyDuplicated(lineage$parents) ||
      !is.character(lineage$ancestors) ||
      anyNA(lineage$ancestors) ||
      !all(nzchar(lineage$ancestors)) ||
      anyDuplicated(lineage$ancestors) ||
      !is.null(lineage$ancestor) &&
        (!is.character(lineage$ancestor) ||
          length(lineage$ancestor) != 1L ||
          is.na(lineage$ancestor) ||
          !nzchar(lineage$ancestor)) ||
      !is.character(lineage$tag) ||
      length(lineage$tag) != 1L ||
      is.na(lineage$tag) ||
      !nzchar(lineage$tag)
  ) {
    cli::cli_abort(
      "GEPA checkpoint candidate lineage is malformed",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  for (id in names(component_specs)) {
    component <- value$components[[id]]
    spec <- component_specs[[id]]
    if (
      !is.list(component) ||
        !identical(names(component), c("id", "kind", "path", "value")) ||
        !identical(component$id, id) ||
        !identical(component$kind, "instructions") ||
        !identical(component$path, spec$path) ||
        !is.character(component$value) ||
        length(component$value) != 1L ||
        is.na(component$value)
    ) {
      cli::cli_abort(
        "GEPA checkpoint candidate component is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
  }
  candidate <- structure(
    value[c("components", "valid", "failure", "history")],
    class = c("dsprrr_gepa_component_candidate", "list")
  )
  candidate <- gepa_set_component_candidate_lineage(
    candidate,
    parents = lineage$parents,
    ancestors = lineage$ancestors,
    ancestor = lineage$ancestor,
    tag = lineage$tag
  )
  tryCatch(
    gepa_validate_component_candidate(candidate, program = program),
    error = function(error) {
      cli::cli_abort(
        "GEPA checkpoint candidate does not match the original program",
        class = "dsprrr_optimizer_checkpoint_malformed",
        parent = error
      )
    }
  )
  candidate
}

gepa_checkpoint_project_rows <- function(records) {
  lapply(seq_along(records), function(index) {
    record <- records[[index]]
    if (!is.list(record) || is.null(record$row_index)) {
      cli::cli_abort(
        "GEPA evaluation returned a malformed checkpoint row",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    sanitized_prediction <- tryCatch(
      optimizer_checkpoint_sanitize(
        record$predicted,
        paste0("progress.evaluation_rows[[", index, "]].predicted")
      ),
      error = function(error) {
        cli::cli_abort(
          c(
            "GEPA checkpoint cannot store this prediction value",
            "x" = "The value for evaluation row {record$row_index} is not plain data.",
            "i" = "Use scalar or nested data values without runtime objects or custom classes."
          ),
          class = "dsprrr_optimizer_checkpoint_unsafe_value",
          parent = error
        )
      }
    )
    record["predicted"] <- list(sanitized_prediction)
    record$program_trace <- NULL
    record
  })
}

gepa_checkpoint_validate_rows <- function(rows, dataset, path) {
  if (!is.list(rows)) {
    cli::cli_abort(
      "GEPA checkpoint evaluation rows are malformed",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  if (length(rows) == 0L) {
    return(invisible(rows))
  }
  indices <- integer(length(rows))
  for (index in seq_along(rows)) {
    row <- rows[[index]]
    allowed <- c(
      "row_index",
      "score",
      "error",
      "predicted",
      "feedback",
      "input_tokens",
      "output_tokens",
      "total_tokens",
      "total_cost",
      "provider_calls",
      "metric_calls",
      "provider_usage_unknown",
      "tokens_unknown",
      "latency_ms",
      "error_class"
    )
    required <- setdiff(allowed, c("error_class"))
    if (
      !is.list(row) ||
        length(setdiff(names(row), allowed)) > 0L ||
        !all(required %in% names(row)) ||
        "program_trace" %in% names(row) ||
        !is.integer(row$row_index) ||
        length(row$row_index) != 1L ||
        is.na(row$row_index) ||
        row$row_index < 1L ||
        row$row_index > nrow(dataset) ||
        !is.numeric(row$score) ||
        length(row$score) != 1L ||
        !is.character(row$error) ||
        length(row$error) != 1L ||
        !is.character(row$feedback) ||
        length(row$feedback) != 1L ||
        !is.logical(row$provider_usage_unknown) ||
        length(row$provider_usage_unknown) != 1L ||
        is.na(row$provider_usage_unknown) ||
        !is.logical(row$tokens_unknown) ||
        length(row$tokens_unknown) != 1L ||
        is.na(row$tokens_unknown) ||
        !is.numeric(row$input_tokens) ||
        length(row$input_tokens) != 1L ||
        !is.numeric(row$output_tokens) ||
        length(row$output_tokens) != 1L ||
        !is.numeric(row$total_tokens) ||
        length(row$total_tokens) != 1L ||
        !is.numeric(row$total_cost) ||
        length(row$total_cost) != 1L ||
        !is.numeric(row$provider_calls) ||
        length(row$provider_calls) != 1L ||
        !is.numeric(row$metric_calls) ||
        length(row$metric_calls) != 1L ||
        !is.numeric(row$latency_ms) ||
        length(row$latency_ms) != 1L ||
        !is.null(row$error_class) &&
          (!is.character(row$error_class) ||
            length(row$error_class) != 1L)
    ) {
      cli::cli_abort(
        "GEPA checkpoint evaluation row is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    indices[[index]] <- row$row_index
    tryCatch(
      optimizer_checkpoint_sanitize(
        row$predicted,
        paste0(path, "[[", index, "]].predicted")
      ),
      error = function(error) {
        cli::cli_abort(
          "GEPA checkpoint prediction value is not plain data",
          class = "dsprrr_optimizer_checkpoint_malformed",
          parent = error
        )
      }
    )
  }
  if (anyDuplicated(indices) || is.unsorted(indices, strictly = TRUE)) {
    cli::cli_abort(
      "GEPA checkpoint evaluation row indices are duplicated or unordered",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  invisible(rows)
}

gepa_checkpoint_initial_state <- function(program, component_specs) {
  baseline <- gepa_component_candidate(program)
  list(
    schema_version = 1L,
    phase = "initializing",
    generation = 1L,
    generations_completed = 0L,
    population = list(gepa_checkpoint_encode_candidate(baseline)),
    next_population = list(),
    next_proposal_index = 2L,
    active_proposal = NULL,
    candidate_index = 1L,
    active_candidate = NULL,
    active_evaluation = NULL,
    generation_record_keys = character(),
    all_records = list(),
    best_record_key = NULL,
    merge_invocations = 0L,
    lineage_merges = 0L,
    all_generations = list(),
    termination = NULL,
    no_mutable_components = length(component_specs) == 0L
  )
}

gepa_checkpoint_state_fields <- function() {
  c(
    "schema_version",
    "phase",
    "generation",
    "generations_completed",
    "population",
    "next_population",
    "next_proposal_index",
    "active_proposal",
    "candidate_index",
    "active_candidate",
    "active_evaluation",
    "generation_record_keys",
    "all_records",
    "best_record_key",
    "merge_invocations",
    "lineage_merges",
    "all_generations",
    "termination",
    "no_mutable_components"
  )
}

gepa_checkpoint_state_validate <- function(
  state,
  program,
  component_specs,
  metric_names,
  validation_dataset,
  discovery_dataset,
  teleprompter,
  budget
) {
  required <- gepa_checkpoint_state_fields()
  if (
    !is.list(state) ||
      !identical(names(state), required) ||
      !identical(state$schema_version, 1L) ||
      !is.character(state$phase) ||
      length(state$phase) != 1L ||
      !state$phase %in%
        c(
          "initializing",
          "evaluating",
          "selecting",
          "proposing",
          "complete"
        ) ||
      !is.integer(state$generation) ||
      length(state$generation) != 1L ||
      state$generation < 1L ||
      state$generation > teleprompter@generations + 1L ||
      !is.integer(state$generations_completed) ||
      length(state$generations_completed) != 1L ||
      state$generations_completed < 0L ||
      state$generations_completed > teleprompter@generations ||
      !is.list(state$population) ||
      length(state$population) > teleprompter@population_size ||
      !is.list(state$next_population) ||
      length(state$next_population) > teleprompter@population_size ||
      !is.integer(state$next_proposal_index) ||
      length(state$next_proposal_index) != 1L ||
      state$next_proposal_index < 1L ||
      state$next_proposal_index > teleprompter@population_size + 1L ||
      !is.integer(state$candidate_index) ||
      length(state$candidate_index) != 1L ||
      state$candidate_index < 1L ||
      state$candidate_index > length(state$population) + 1L ||
      !is.character(state$generation_record_keys) ||
      anyNA(state$generation_record_keys) ||
      !all(nzchar(state$generation_record_keys)) ||
      anyDuplicated(state$generation_record_keys) ||
      !is.list(state$all_records) ||
      !is.list(state$all_generations) ||
      !is.integer(state$merge_invocations) ||
      length(state$merge_invocations) != 1L ||
      state$merge_invocations < 0L ||
      !is.integer(state$lineage_merges) ||
      length(state$lineage_merges) != 1L ||
      state$lineage_merges < 0L ||
      state$lineage_merges > state$merge_invocations ||
      !is.logical(state$no_mutable_components) ||
      length(state$no_mutable_components) != 1L ||
      is.na(state$no_mutable_components) ||
      !is.null(state$termination) &&
        (!is.character(state$termination) ||
          length(state$termination) != 1L ||
          is.na(state$termination) ||
          !nzchar(state$termination)) ||
      !is.null(state$best_record_key) &&
        (!is.character(state$best_record_key) ||
          length(state$best_record_key) != 1L ||
          is.na(state$best_record_key))
  ) {
    cli::cli_abort(
      "GEPA checkpoint search state is malformed",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  if (!identical(state$no_mutable_components, length(component_specs) == 0L)) {
    cli::cli_abort(
      "GEPA checkpoint mutable-component scope is incompatible",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  decode_candidates <- function(values, path) {
    lapply(seq_along(values), function(index) {
      tryCatch(
        gepa_checkpoint_decode_candidate(
          values[[index]],
          program,
          component_specs
        ),
        error = function(error) {
          if (inherits(error, "dsprrr_optimizer_checkpoint_malformed")) {
            stop(error)
          }
          cli::cli_abort(
            "GEPA checkpoint candidate state is malformed",
            class = "dsprrr_optimizer_checkpoint_malformed",
            path = paste0(path, "[[", index, "]"),
            parent = error
          )
        }
      )
    })
  }
  decode_candidates(state$population, "population")
  decode_candidates(state$next_population, "next_population")
  malformed <- function(message, record_key = NULL) {
    cli::cli_abort(
      message,
      class = "dsprrr_optimizer_checkpoint_malformed",
      record_key = record_key
    )
  }
  same_number <- function(actual, expected) {
    length(actual) == 1L &&
      length(expected) == 1L &&
      if (is.na(actual) || is.na(expected)) {
        is.na(actual) && is.na(expected)
      } else {
        identical(as.numeric(actual), as.numeric(expected))
      }
  }
  aggregate_score <- function(rows, dataset, path) {
    if (length(rows) != nrow(dataset)) {
      malformed(
        paste0(path, " does not contain every evaluation row.")
      )
    }
    as.numeric(optimizer_combine_eval_records(rows, dataset)@mean_score)
  }
  sum_metric_calls <- function(rows) {
    if (length(rows) == 0L) {
      return(0)
    }
    values <- vapply(rows, function(row) row$metric_calls, numeric(1))
    if (anyNA(values)) {
      NA_real_
    } else {
      sum(values)
    }
  }
  completed_row_units <- function(unit_id, rows) {
    if (length(rows) == 0L) {
      return(TRUE)
    }
    all(vapply(
      rows,
      function(row) {
        paste0(unit_id, ":row:", row$row_index) %in%
          budget$completed_units
      },
      logical(1)
    ))
  }
  record_keys <- names(state$all_records)
  if (length(state$all_records) == 0L) {
    record_keys <- character()
  } else if (
    is.null(record_keys) ||
      anyNA(record_keys) ||
      !all(nzchar(record_keys)) ||
      anyDuplicated(record_keys)
  ) {
    cli::cli_abort(
      "GEPA checkpoint record keys are malformed",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  for (key in record_keys) {
    record <- state$all_records[[key]]
    fields <- c(
      "record_key",
      "candidate",
      "candidate_id",
      "parents",
      "lineage",
      "candidate_valid",
      "scores",
      "metric_rows",
      "discovery_rows",
      "discovery_complete",
      "discovery_metric_calls",
      "completed_metrics",
      "complete",
      "generation",
      "index"
    )
    if (
      !is.list(record) ||
        !identical(names(record), fields) ||
        !identical(record$record_key, key) ||
        !is.character(record$candidate_id) ||
        length(record$candidate_id) != 1L ||
        is.na(record$candidate_id) ||
        !is.character(record$parents) ||
        !is.list(record$lineage) ||
        !is.logical(record$candidate_valid) ||
        length(record$candidate_valid) != 1L ||
        is.na(record$candidate_valid) ||
        !is.numeric(record$scores) ||
        length(record$scores) != length(metric_names) ||
        !is.list(record$metric_rows) ||
        length(record$metric_rows) != length(metric_names) ||
        !identical(names(record$scores), metric_names) ||
        !is.list(record$discovery_rows) ||
        !is.logical(record$discovery_complete) ||
        length(record$discovery_complete) != 1L ||
        is.na(record$discovery_complete) ||
        !isTRUE(record$discovery_complete) ||
        !is.numeric(record$discovery_metric_calls) ||
        length(record$discovery_metric_calls) != 1L ||
        !isTRUE(record$complete) ||
        !is.integer(record$completed_metrics) ||
        length(record$completed_metrics) != 1L ||
        record$completed_metrics != length(metric_names) ||
        !is.logical(record$complete) ||
        length(record$complete) != 1L ||
        is.na(record$complete) ||
        !is.integer(record$generation) ||
        length(record$generation) != 1L ||
        record$generation < 1L ||
        record$generation > teleprompter@generations ||
        !is.integer(record$index) ||
        length(record$index) != 1L ||
        record$index < 1L ||
        record$index > teleprompter@population_size
    ) {
      cli::cli_abort(
        "GEPA checkpoint candidate record is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed",
        record_key = key
      )
    }
    candidate <- gepa_checkpoint_decode_candidate(
      record$candidate,
      program,
      component_specs
    )
    if (
      !identical(gepa_component_candidate_id(candidate), record$candidate_id) ||
        !identical(isTRUE(candidate$valid), isTRUE(record$candidate_valid)) ||
        !identical(
          gepa_component_candidate_lineage(candidate),
          record$lineage
        ) ||
        !identical(
          gepa_component_candidate_lineage(candidate)$parents,
          record$parents
        ) ||
        !identical(
          key,
          gepa_checkpoint_record_key(record$generation, record$index)
        )
    ) {
      cli::cli_abort(
        "GEPA checkpoint candidate lineage or identity does not match",
        class = "dsprrr_optimizer_checkpoint_malformed",
        record_key = key
      )
    }
    for (metric_index in seq_along(metric_names)) {
      gepa_checkpoint_validate_rows(
        record$metric_rows[[metric_index]],
        validation_dataset,
        paste0("all_records.", key, ".metric_rows[[", metric_index, "]]")
      )
      metric_rows <- record$metric_rows[[metric_index]]
      metric_unit <- paste0(
        "gepa:generation:",
        record$generation,
        ":candidate:",
        record$index,
        ":metric:",
        metric_index
      )
      if (
        !metric_unit %in% budget$completed_units ||
          isTRUE(record$candidate_valid) &&
            !completed_row_units(metric_unit, metric_rows)
      ) {
        malformed(
          "GEPA checkpoint candidate metric is not committed in the budget ledger.",
          record_key = key
        )
      }
      if (
        !same_number(
          record$scores[[metric_index]],
          aggregate_score(
            metric_rows,
            validation_dataset,
            paste0("all_records.", key, ".metric_rows[[", metric_index, "]]")
          )
        )
      ) {
        malformed(
          "GEPA checkpoint metric score does not match its evaluation rows.",
          record_key = key
        )
      }
    }
    gepa_checkpoint_validate_rows(
      record$discovery_rows,
      discovery_dataset,
      paste0("all_records.", key, ".discovery_rows")
    )
    expected_discovery_calls <- 0
    if (isTRUE(record$candidate_valid)) {
      if (identical(validation_dataset, discovery_dataset)) {
        if (!identical(record$discovery_rows, record$metric_rows[[1L]])) {
          malformed(
            "GEPA checkpoint discovery rows do not match validation evidence.",
            record_key = key
          )
        }
        expected_discovery_calls <- sum_metric_calls(record$metric_rows[[1L]])
      } else {
        if (length(record$discovery_rows) != nrow(discovery_dataset)) {
          malformed(
            "GEPA checkpoint discovery rows are incomplete.",
            record_key = key
          )
        }
        discovery_unit <- paste0(
          "gepa:generation:",
          record$generation,
          ":candidate:",
          record$index,
          ":discovery"
        )
        if (
          !discovery_unit %in% budget$completed_units ||
            !completed_row_units(discovery_unit, record$discovery_rows)
        ) {
          malformed(
            "GEPA checkpoint discovery evaluation is not committed in the budget ledger.",
            record_key = key
          )
        }
        expected_discovery_calls <- sum_metric_calls(record$discovery_rows)
      }
    } else if (
      identical(validation_dataset, discovery_dataset) &&
        !identical(record$discovery_rows, record$metric_rows[[1L]])
    ) {
      malformed(
        "GEPA checkpoint invalid-candidate discovery evidence is inconsistent.",
        record_key = key
      )
    }
    if (!same_number(record$discovery_metric_calls, expected_discovery_calls)) {
      malformed(
        "GEPA checkpoint discovery usage does not match its evaluation rows.",
        record_key = key
      )
    }
  }
  expected_best_record_key <- gepa_checkpoint_best_record_key(
    state$all_records,
    teleprompter@selection,
    metric_names
  )
  if (!identical(state$best_record_key, expected_best_record_key)) {
    malformed(
      "GEPA checkpoint best-record key does not match committed evaluation scores."
    )
  }
  if (!all(state$generation_record_keys %in% record_keys)) {
    cli::cli_abort(
      "GEPA checkpoint state references a missing candidate record",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  if (length(state$generation_record_keys) > 0L) {
    generation_records <- state$all_records[state$generation_record_keys]
    if (
      any(vapply(
        generation_records,
        function(record) !identical(record$generation, state$generation),
        logical(1)
      ))
    ) {
      cli::cli_abort(
        "GEPA checkpoint current-generation records have invalid linkage",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
  }
  expected_current_keys <- record_keys[vapply(
    state$all_records,
    function(record) identical(record$generation, state$generation),
    logical(1)
  )]
  if (!identical(state$generation_record_keys, expected_current_keys)) {
    malformed(
      "GEPA checkpoint current-generation record order is inconsistent."
    )
  }
  if (length(state$all_generations) > 0L) {
    if (
      is.null(names(state$all_generations)) ||
        anyDuplicated(names(state$all_generations))
    ) {
      cli::cli_abort(
        "GEPA checkpoint generation summaries are malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    for (generation_name in names(state$all_generations)) {
      summary <- state$all_generations[[generation_name]]
      if (
        !is.list(summary) ||
          !identical(
            names(summary),
            c("generation", "record_keys", "n_selectable", "n_invalid")
          ) ||
          !is.integer(summary$generation) ||
          length(summary$generation) != 1L ||
          !is.character(summary$record_keys) ||
          anyNA(summary$record_keys) ||
          !all(summary$record_keys %in% record_keys) ||
          !is.integer(summary$n_selectable) ||
          length(summary$n_selectable) != 1L ||
          summary$n_selectable < 0L ||
          !is.integer(summary$n_invalid) ||
          length(summary$n_invalid) != 1L ||
          summary$n_invalid < 0L
      ) {
        malformed("GEPA checkpoint generation summary is malformed.")
      }
      expected_generation_keys <- record_keys[vapply(
        state$all_records,
        function(record) {
          identical(record$generation, summary$generation) &&
            isTRUE(record$complete)
        },
        logical(1)
      )]
      expected_selectable <- sum(vapply(
        state$all_records[expected_generation_keys],
        function(record) isTRUE(record$candidate_valid),
        logical(1)
      ))
      expected_invalid <- sum(vapply(
        state$all_records[expected_generation_keys],
        function(record) !isTRUE(record$candidate_valid),
        logical(1)
      ))
      if (
        !identical(generation_name, as.character(summary$generation)) ||
          !identical(summary$record_keys, expected_generation_keys) ||
          !identical(summary$n_selectable, as.integer(expected_selectable)) ||
          !identical(summary$n_invalid, as.integer(expected_invalid))
      ) {
        malformed(
          "GEPA checkpoint generation summary does not match committed records."
        )
      }
    }
  }
  track_stats <- isTRUE(gepa_teleprompter_property(
    teleprompter,
    "track_stats",
    TRUE
  ))
  if (!track_stats && length(state$all_generations) > 0L) {
    malformed(
      "GEPA checkpoint contains generation summaries while tracking is disabled."
    )
  }
  if (track_stats) {
    completed_summary_names <- if (state$generations_completed > 0L) {
      as.character(seq_len(state$generations_completed))
    } else {
      character()
    }
    if (!all(completed_summary_names %in% names(state$all_generations))) {
      malformed(
        "GEPA checkpoint is missing a completed generation summary."
      )
    }
    allowed_summary_names <- unique(c(
      completed_summary_names,
      as.character(state$generation)
    ))
    if (!all(names(state$all_generations) %in% allowed_summary_names)) {
      malformed(
        "GEPA checkpoint has a summary for an uncommitted generation."
      )
    }
    if (identical(state$phase, "complete") && !state$no_mutable_components) {
      if (
        length(state$all_records) == 0L ||
          state$generations_completed !=
            max(vapply(
              state$all_records,
              `[[`,
              integer(1),
              "generation"
            )) ||
          !identical(names(state$all_generations), completed_summary_names)
      ) {
        malformed(
          "GEPA checkpoint completed generation history is incomplete."
        )
      }
    }
  }
  if (
    !is.null(state$best_record_key) && !state$best_record_key %in% record_keys
  ) {
    cli::cli_abort(
      "GEPA checkpoint best-record key is missing",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  if (!is.null(state$active_candidate)) {
    active <- state$active_candidate
    expected_active <- c(
      "candidate",
      "generation",
      "index",
      "scores",
      "metric_rows",
      "discovery_rows",
      "discovery_complete",
      "discovery_metric_calls",
      "completed_metrics",
      "candidate_valid"
    )
    if (
      !is.list(active) ||
        !identical(names(active), expected_active) ||
        !identical(active$generation, state$generation) ||
        !identical(active$index, state$candidate_index) ||
        !is.numeric(active$scores) ||
        length(active$scores) != length(metric_names) ||
        !is.list(active$metric_rows) ||
        length(active$metric_rows) != length(metric_names) ||
        !is.list(active$discovery_rows) ||
        !is.logical(active$discovery_complete) ||
        length(active$discovery_complete) != 1L ||
        is.na(active$discovery_complete) ||
        !is.numeric(active$discovery_metric_calls) ||
        length(active$discovery_metric_calls) != 1L ||
        !is.integer(active$completed_metrics) ||
        length(active$completed_metrics) != 1L ||
        active$completed_metrics < 0L ||
        active$completed_metrics > length(metric_names) ||
        !is.logical(active$candidate_valid) ||
        length(active$candidate_valid) != 1L ||
        is.na(active$candidate_valid)
    ) {
      cli::cli_abort(
        "GEPA checkpoint active candidate is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    gepa_checkpoint_decode_candidate(
      active$candidate,
      program,
      component_specs
    )
    for (metric_index in seq_along(metric_names)) {
      gepa_checkpoint_validate_rows(
        active$metric_rows[[metric_index]],
        validation_dataset,
        paste0("active_candidate.metric_rows[[", metric_index, "]]")
      )
    }
    gepa_checkpoint_validate_rows(
      active$discovery_rows,
      discovery_dataset,
      "active_candidate.discovery_rows"
    )
    candidate <- gepa_checkpoint_decode_candidate(
      active$candidate,
      program,
      component_specs
    )
    if (
      !identical(isTRUE(candidate$valid), isTRUE(active$candidate_valid)) ||
        state$candidate_index > length(state$population) ||
        !identical(
          gepa_component_candidate_id(candidate),
          gepa_component_candidate_id(gepa_checkpoint_decode_candidate(
            state$population[[state$candidate_index]],
            program,
            component_specs
          ))
        )
    ) {
      cli::cli_abort(
        "GEPA checkpoint active candidate does not match the population cursor",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    if (!identical(names(active$scores), metric_names)) {
      malformed("GEPA checkpoint active metric names are malformed.")
    }
    for (metric_index in seq_along(metric_names)) {
      rows <- active$metric_rows[[metric_index]]
      metric_unit <- paste0(
        "gepa:generation:",
        state$generation,
        ":candidate:",
        state$candidate_index,
        ":metric:",
        metric_index
      )
      if (metric_index <= active$completed_metrics) {
        if (
          !metric_unit %in% budget$completed_units ||
            !same_number(
              active$scores[[metric_index]],
              aggregate_score(
                rows,
                validation_dataset,
                paste0("active_candidate.metric_rows[[", metric_index, "]]")
              )
            ) ||
            isTRUE(active$candidate_valid) &&
              !completed_row_units(metric_unit, rows)
        ) {
          malformed(
            "GEPA checkpoint active metric score or ledger does not match its rows."
          )
        }
      } else if (length(rows) != 0L || !is.na(active$scores[[metric_index]])) {
        malformed(
          "GEPA checkpoint contains uncommitted metric evidence."
        )
      }
    }
    if (isTRUE(active$discovery_complete)) {
      if (
        identical(validation_dataset, discovery_dataset) &&
          active$completed_metrics >= 1L
      ) {
        if (
          !identical(active$discovery_rows, active$metric_rows[[1L]]) ||
            !same_number(
              active$discovery_metric_calls,
              sum_metric_calls(active$metric_rows[[1L]])
            )
        ) {
          malformed(
            "GEPA checkpoint active discovery evidence is inconsistent."
          )
        }
      } else if (isTRUE(active$candidate_valid)) {
        if (
          active$completed_metrics != length(metric_names) ||
            length(active$discovery_rows) != nrow(discovery_dataset) ||
            !same_number(
              active$discovery_metric_calls,
              sum_metric_calls(active$discovery_rows)
            )
        ) {
          malformed(
            "GEPA checkpoint active discovery evaluation is incomplete."
          )
        }
      } else if (
        length(active$discovery_rows) != 0L ||
          !same_number(active$discovery_metric_calls, 0)
      ) {
        malformed(
          "GEPA checkpoint invalid-candidate discovery evidence is inconsistent."
        )
      }
    } else if (
      length(active$discovery_rows) != 0L ||
        !same_number(active$discovery_metric_calls, 0)
    ) {
      malformed(
        "GEPA checkpoint has uncommitted discovery evidence."
      )
    }
  }
  if (!is.null(state$active_evaluation)) {
    evaluation <- state$active_evaluation
    expected_eval <- c("role", "metric_index", "unit_id", "rows")
    dataset <- if (identical(evaluation$role, "discovery")) {
      discovery_dataset
    } else {
      validation_dataset
    }
    if (
      !is.list(evaluation) ||
        !identical(names(evaluation), expected_eval) ||
        !is.character(evaluation$role) ||
        length(evaluation$role) != 1L ||
        !evaluation$role %in% c("metric", "discovery") ||
        !is.integer(evaluation$metric_index) ||
        length(evaluation$metric_index) != 1L ||
        evaluation$metric_index < 1L ||
        evaluation$metric_index > length(metric_names) ||
        identical(evaluation$role, "discovery") &&
          evaluation$metric_index != 1L ||
        !is.character(evaluation$unit_id) ||
        length(evaluation$unit_id) != 1L ||
        is.na(evaluation$unit_id) ||
        !is.list(evaluation$rows)
    ) {
      cli::cli_abort(
        "GEPA checkpoint active evaluation is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    gepa_checkpoint_validate_rows(
      evaluation$rows,
      dataset,
      "active_evaluation.rows"
    )
    active_candidate <- state$active_candidate
    expected_unit <- if (identical(evaluation$role, "discovery")) {
      paste0(
        "gepa:generation:",
        state$generation,
        ":candidate:",
        state$candidate_index,
        ":discovery"
      )
    } else {
      paste0(
        "gepa:generation:",
        state$generation,
        ":candidate:",
        state$candidate_index,
        ":metric:",
        evaluation$metric_index
      )
    }
    if (
      is.null(active_candidate) ||
        !identical(evaluation$unit_id, expected_unit) ||
        !identical(evaluation$role, "discovery") &&
          evaluation$metric_index != active_candidate$completed_metrics + 1L ||
        !if (isTRUE(active_candidate$candidate_valid)) {
          completed_row_units(evaluation$unit_id, evaluation$rows)
        } else {
          length(evaluation$rows) == 0L ||
            length(evaluation$rows) == nrow(dataset) &&
              evaluation$unit_id %in% budget$completed_units
        }
    ) {
      cli::cli_abort(
        "GEPA checkpoint active evaluation cursor is inconsistent",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    if (
      identical(evaluation$role, "discovery") &&
        isTRUE(active_candidate$discovery_complete)
    ) {
      cli::cli_abort(
        "GEPA checkpoint discovery evaluation was already committed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
  }
  if (!is.null(state$active_proposal)) {
    proposal <- state$active_proposal
    expected_proposal <- c(
      "kind",
      "candidate",
      "parent1_record_key",
      "parent1_id",
      "parent2_id",
      "component_id",
      "unit_id",
      "selector_context",
      "mutation",
      "merge_invocation_delta",
      "lineage_merge_delta"
    )
    if (
      !is.list(proposal) ||
        !identical(names(proposal), expected_proposal) ||
        !is.character(proposal$kind) ||
        length(proposal$kind) != 1L ||
        !proposal$kind %in% c("initial", "generation") ||
        !is.null(proposal$parent1_record_key) &&
          (!is.character(proposal$parent1_record_key) ||
            length(proposal$parent1_record_key) != 1L ||
            !proposal$parent1_record_key %in% record_keys) ||
        !is.character(proposal$parent1_id) ||
        length(proposal$parent1_id) != 1L ||
        !is.character(proposal$parent2_id) ||
        length(proposal$parent2_id) != 1L ||
        !is.logical(proposal$mutation) ||
        length(proposal$mutation) != 1L ||
        is.na(proposal$mutation) ||
        !is.null(proposal$component_id) &&
          (!is.character(proposal$component_id) ||
            length(proposal$component_id) != 1L ||
            !proposal$component_id %in% names(component_specs)) ||
        isTRUE(proposal$mutation) && is.null(proposal$component_id) ||
        !isTRUE(proposal$mutation) && !is.null(proposal$component_id) ||
        !is.character(proposal$unit_id) ||
        length(proposal$unit_id) != 1L ||
        is.na(proposal$unit_id) ||
        isTRUE(proposal$mutation) && !nzchar(proposal$unit_id) ||
        !isTRUE(proposal$mutation) && nzchar(proposal$unit_id) ||
        !is.list(proposal$selector_context) ||
        !is.integer(proposal$merge_invocation_delta) ||
        length(proposal$merge_invocation_delta) != 1L ||
        proposal$merge_invocation_delta < 0L ||
        !is.integer(proposal$lineage_merge_delta) ||
        length(proposal$lineage_merge_delta) != 1L ||
        proposal$lineage_merge_delta < 0L ||
        proposal$lineage_merge_delta > proposal$merge_invocation_delta
    ) {
      cli::cli_abort(
        "GEPA checkpoint active proposal is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    candidate <- gepa_checkpoint_decode_candidate(
      proposal$candidate,
      program,
      component_specs
    )
    if (
      !identical(gepa_component_candidate_id(candidate), proposal$parent1_id) &&
        identical(proposal$kind, "initial")
    ) {
      cli::cli_abort(
        "GEPA checkpoint initialization proposal parent is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    if (
      identical(proposal$kind, "generation") &&
        (is.null(proposal$parent1_record_key) ||
          !identical(
            state$all_records[[proposal$parent1_record_key]]$candidate_id,
            proposal$parent1_id
          ) ||
          !proposal$parent2_id %in%
            vapply(state$all_records, `[[`, character(1), "candidate_id"))
    ) {
      cli::cli_abort(
        "GEPA checkpoint proposal parent linkage is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
  }
  if (identical(state$phase, "initializing")) {
    if (
      length(state$population) != state$next_proposal_index - 1L ||
        state$next_proposal_index > teleprompter@population_size ||
        length(state$all_records) != 0L ||
        length(state$generation_record_keys) != 0L
    ) {
      malformed("GEPA checkpoint initialization cursor is inconsistent.")
    }
  } else if (identical(state$phase, "proposing")) {
    if (
      length(state$population) != teleprompter@population_size ||
        length(state$next_population) != state$next_proposal_index - 1L ||
        state$next_proposal_index > teleprompter@population_size ||
        state$candidate_index != 1L ||
        length(state$generation_record_keys) != 0L
    ) {
      malformed("GEPA checkpoint proposal cursor is inconsistent.")
    }
  } else if (state$phase %in% c("evaluating", "selecting")) {
    if (
      length(state$population) != teleprompter@population_size ||
        length(state$generation_record_keys) != state$candidate_index - 1L ||
        state$phase == "selecting" &&
          state$candidate_index != length(state$population) + 1L ||
        state$candidate_index <= length(state$population) &&
          is.null(state$active_candidate) &&
          !is.null(state$active_evaluation) ||
        state$candidate_index > length(state$population) &&
          (!is.null(state$active_candidate) ||
            !is.null(state$active_evaluation))
    ) {
      malformed("GEPA checkpoint evaluation cursor is inconsistent.")
    }
  }
  if (
    identical(state$phase, "selecting") &&
      (state$candidate_index <= length(state$population) ||
        !is.null(state$active_candidate) ||
        !is.null(state$active_evaluation)) ||
      state$phase %in%
        c("initializing", "proposing") &&
        (!is.null(state$active_candidate) ||
          !is.null(state$active_evaluation)) ||
      identical(state$phase, "complete") &&
        (is.null(state$termination) ||
          !is.null(state$active_proposal) ||
          !is.null(state$active_candidate) ||
          !is.null(state$active_evaluation)) ||
      identical(state$phase, "proposing") && state$generation < 2L ||
      identical(state$phase, "initializing") && state$generation != 1L
  ) {
    cli::cli_abort(
      "GEPA checkpoint phase cursors are inconsistent",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  invisible(state)
}

gepa_checkpoint_record_key <- function(generation, index) {
  paste0("generation:", generation, ":candidate:", index)
}

gepa_checkpoint_new_active_candidate <- function(
  candidate,
  generation,
  index,
  metric_names
) {
  list(
    candidate = gepa_checkpoint_encode_candidate(candidate),
    generation = as.integer(generation),
    index = as.integer(index),
    scores = stats::setNames(rep(NA_real_, length(metric_names)), metric_names),
    metric_rows = rep(list(list()), length(metric_names)),
    discovery_rows = list(),
    discovery_complete = FALSE,
    discovery_metric_calls = 0L,
    completed_metrics = 0L,
    candidate_valid = isTRUE(candidate$valid)
  )
}

gepa_checkpoint_eval_from_rows <- function(rows, dataset) {
  optimizer_combine_eval_records(rows, dataset)
}

gepa_checkpoint_eval_rows_from_invalid <- function(eval_result) {
  examples <- eval_result@examples
  lapply(seq_len(nrow(examples)), function(index) {
    row <- list(
      row_index = as.integer(index),
      score = as.numeric(examples$score[[index]]),
      error = as.character(examples$error[[index]] %||% NA_character_),
      predicted = if ("predicted" %in% names(examples)) {
        examples$predicted[[index]]
      } else {
        NA
      },
      feedback = as.character(examples$feedback[[index]] %||% NA_character_),
      input_tokens = 0L,
      output_tokens = 0L,
      total_tokens = 0L,
      total_cost = 0,
      provider_calls = 0L,
      metric_calls = 0L,
      provider_usage_unknown = FALSE,
      tokens_unknown = FALSE,
      latency_ms = 0
    )
    if ("error_class" %in% names(examples)) {
      row$error_class <- as.character(
        examples$error_class[[index]] %||% NA_character_
      )
    }
    row
  })
}

gepa_checkpoint_hydrate_record <- function(
  record,
  program,
  validation_dataset,
  discovery_dataset,
  metrics,
  component_specs,
  metric_threshold = 1
) {
  candidate <- gepa_checkpoint_decode_candidate(
    record$candidate,
    program,
    component_specs
  )
  metric_results <- lapply(record$metric_rows, function(rows) {
    gepa_checkpoint_eval_from_rows(rows, validation_dataset)
  })
  primary_eval <- if (length(metric_results) > 0L) {
    metric_results[[1L]]
  } else {
    NULL
  }
  discovery_eval <- if (isTRUE(record$discovery_complete)) {
    if (identical(validation_dataset, discovery_dataset)) {
      primary_eval
    } else {
      gepa_checkpoint_eval_from_rows(record$discovery_rows, discovery_dataset)
    }
  } else {
    NULL
  }
  failed_examples <- if (
    isTRUE(record$candidate_valid) &&
      S7::S7_inherits(discovery_eval, EvalResult)
  ) {
    gepa_failed_examples(
      discovery_eval,
      discovery_dataset,
      program$signature,
      threshold = metric_threshold,
      output_col = get_metric_field(metrics[[1L]])
    )
  } else {
    list()
  }
  hydrated <- list(
    candidate_id = record$candidate_id,
    candidate = candidate,
    parents = record$parents,
    lineage = record$lineage,
    candidate_valid = record$candidate_valid,
    scores = stats::setNames(as.numeric(record$scores), names(record$scores)),
    primary_eval = primary_eval,
    discovery_eval = discovery_eval,
    failed_examples = failed_examples,
    generation = record$generation,
    index = record$index,
    discovery_metric_calls = record$discovery_metric_calls,
    completed_metrics = record$completed_metrics,
    complete = record$complete
  )
  hydrated$selectable <- gepa_component_record_selectable(hydrated)
  hydrated
}

gepa_checkpoint_record_from_active <- function(
  active,
  program,
  metric_names,
  budget,
  discovery_dataset,
  validation_dataset
) {
  candidate <- gepa_checkpoint_decode_candidate(
    active$candidate,
    program,
    gepa_program_component_specs(program)
  )
  metric_unit_ids <- paste0(
    "gepa:generation:",
    active$generation,
    ":candidate:",
    active$index,
    ":metric:",
    seq_along(metric_names)
  )
  complete <- all(vapply(
    metric_unit_ids,
    function(unit_id) optimizer_budget_unit_completed(budget, unit_id),
    logical(1)
  )) &&
    !anyNA(active$scores)
  candidate_id <- gepa_component_candidate_id(candidate)
  lineage <- gepa_component_candidate_lineage(candidate)
  list(
    record_key = gepa_checkpoint_record_key(active$generation, active$index),
    candidate = gepa_checkpoint_encode_candidate(candidate),
    candidate_id = candidate_id,
    parents = lineage$parents,
    lineage = lineage,
    candidate_valid = isTRUE(active$candidate_valid),
    scores = stats::setNames(as.numeric(active$scores), metric_names),
    metric_rows = active$metric_rows,
    discovery_rows = active$discovery_rows,
    discovery_complete = isTRUE(active$discovery_complete),
    discovery_metric_calls = as.numeric(active$discovery_metric_calls),
    completed_metrics = as.integer(active$completed_metrics),
    complete = isTRUE(complete),
    generation = as.integer(active$generation),
    index = as.integer(active$index)
  )
}

gepa_checkpoint_best_record_key <- function(records, selection, metric_names) {
  keys <- names(records)
  if (is.null(keys)) {
    keys <- character()
  }
  selectable <- vapply(
    records,
    function(record) {
      isTRUE(record$complete) && isTRUE(record$candidate_valid)
    },
    logical(1)
  )
  keys <- keys[selectable]
  if (length(keys) == 0L) {
    return(NULL)
  }
  scores <- do.call(rbind, lapply(records[keys], `[[`, "scores"))
  if (selection == "pareto" && length(metric_names) > 1L) {
    ranks <- pareto_ranks(scores)
    crowding <- pareto_crowding_distance(scores, ranks)
    best <- select_pareto_best(scores, ranks, crowding)
  } else if (!all(is.na(scores[, 1L]))) {
    best <- which.max(scores[, 1L])
  } else {
    return(NULL)
  }
  keys[[best]]
}

gepa_checkpoint_update_selection <- function(
  state,
  teleprompter,
  program,
  validation_dataset,
  discovery_dataset,
  metrics,
  component_specs,
  metric_names
) {
  state["best_record_key"] <- list(gepa_checkpoint_best_record_key(
    state$all_records,
    teleprompter@selection,
    metric_names
  ))
  all_records <- lapply(state$all_records, function(record) {
    gepa_checkpoint_hydrate_record(
      record,
      program,
      validation_dataset,
      discovery_dataset,
      metrics,
      component_specs,
      metric_threshold = teleprompter@metric_threshold
    )
  })
  generation_records <- state$all_records[state$generation_record_keys]
  complete_keys <- names(generation_records)[vapply(
    generation_records,
    function(record) isTRUE(record$complete),
    logical(1)
  )]
  complete_keys <- complete_keys %||% character()
  complete_records <- lapply(
    generation_records[complete_keys],
    function(record) {
      gepa_checkpoint_hydrate_record(
        record,
        program,
        validation_dataset,
        discovery_dataset,
        metrics,
        component_specs,
        metric_threshold = teleprompter@metric_threshold
      )
    }
  )
  selectable_current <- Filter(
    gepa_component_record_selectable,
    complete_records
  )
  if (
    state$candidate_index > length(state$population) &&
      is.null(state$active_candidate)
  ) {
    state$generations_completed <- max(
      state$generations_completed,
      as.integer(state$generation)
    )
  }
  if (isTRUE(gepa_teleprompter_property(teleprompter, "track_stats", TRUE))) {
    state$all_generations[[as.character(state$generation)]] <- list(
      generation = as.integer(state$generation),
      record_keys = complete_keys,
      n_selectable = as.integer(length(selectable_current)),
      n_invalid = as.integer(sum(
        !vapply(
          complete_records,
          function(record) isTRUE(record$candidate_valid),
          logical(1)
        )
      ))
    )
  }
  state
}

gepa_checkpoint_write <- function(
  context,
  state,
  best_program = context$best_program
) {
  if (is.null(context)) {
    return(invisible(NULL))
  }
  if (
    !is.list(state) || !identical(names(state), gepa_checkpoint_state_fields())
  ) {
    cli::cli_abort(
      "GEPA checkpoint transition removed or reordered a required state field",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  optimizer_checkpoint_write(
    context,
    phase = state$phase,
    search_state = state,
    lineage = list(
      best_record_key = state$best_record_key,
      generation_record_keys = state$generation_record_keys
    ),
    best_program = best_program
  )
  invisible(NULL)
}

gepa_checkpoint_proposal_plan <- function(
  candidate,
  kind,
  parent1_record_key = NULL,
  parent1_id,
  parent2_id = "",
  component_id = NULL,
  unit_id = "",
  selector_context = list(),
  mutation = !is.null(component_id),
  merge_invocation_delta = 0L,
  lineage_merge_delta = 0L
) {
  list(
    kind = kind,
    candidate = gepa_checkpoint_encode_candidate(candidate),
    parent1_record_key = parent1_record_key,
    parent1_id = parent1_id,
    parent2_id = parent2_id,
    component_id = component_id,
    unit_id = unit_id,
    selector_context = selector_context,
    mutation = isTRUE(mutation),
    merge_invocation_delta = as.integer(merge_invocation_delta),
    lineage_merge_delta = as.integer(lineage_merge_delta)
  )
}

gepa_checkpoint_plan_generation_child <- function(
  state,
  program,
  teleprompter,
  validation_dataset,
  discovery_dataset,
  metrics,
  component_specs,
  metric_names,
  index,
  component_selector,
  use_merge,
  max_merges_remaining
) {
  all_records <- lapply(state$all_records, function(record) {
    gepa_checkpoint_hydrate_record(
      record,
      program,
      validation_dataset,
      discovery_dataset,
      metrics,
      component_specs,
      metric_threshold = teleprompter@metric_threshold
    )
  })
  parent_keys <- names(state$all_records)[vapply(
    state$all_records,
    function(record) isTRUE(record$complete) && isTRUE(record$candidate_valid),
    logical(1)
  )]
  parent_records <- lapply(parent_keys, function(key) {
    gepa_checkpoint_hydrate_record(
      state$all_records[[key]],
      program,
      validation_dataset,
      discovery_dataset,
      metrics,
      component_specs,
      metric_threshold = teleprompter@metric_threshold
    )
  })
  if (length(parent_records) == 0L) {
    return(NULL)
  }
  keys <- parent_keys
  names(parent_records) <- keys
  parent_records <- lapply(seq_along(parent_records), function(position) {
    record <- parent_records[[position]]
    record$checkpoint_record_key <- keys[[position]]
    record
  })
  parent_records <- gepa_component_parent_records(
    parent_records,
    teleprompter@selection
  )
  if (length(parent_records) == 0L) {
    return(NULL)
  }
  scores <- do.call(rbind, lapply(parent_records, `[[`, "scores"))
  if (teleprompter@selection == "pareto" && ncol(scores) > 1L) {
    ranks <- pareto_ranks(scores)
    crowding <- pareto_crowding_distance(scores, ranks)
  } else {
    primary <- scores[, 1L]
    primary[is.na(primary)] <- -Inf
    ranks <- rank(-primary, ties.method = "min")
    crowding <- primary
    crowding[is.infinite(crowding)] <- 0
  }

  parent1 <- gepa_component_select_parent(parent_records, ranks, crowding)
  parent2 <- gepa_component_select_parent(parent_records, ranks, crowding)
  child <- parent1$candidate
  merge_invocations <- 0L
  lineage_merges <- 0L
  if (stats::runif(1L) < teleprompter@crossover_rate) {
    merged <- NULL
    if (isTRUE(use_merge) && merge_invocations < max_merges_remaining) {
      merge_invocations <- merge_invocations + 1L
      merged <- gepa_merge_component_candidates(
        parent1$candidate,
        parent2$candidate,
        registry = gepa_component_candidate_registry(Filter(
          gepa_component_record_selectable,
          all_records
        )),
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
  mutate <- stats::runif(1L) < teleprompter@mutation_rate
  component_id <- NULL
  unit_id <- ""
  selector_context <- list(
    phase = "search",
    generation = as.integer(state$generation),
    index = as.integer(index),
    parent_ids = c(
      gepa_component_candidate_id(parent1$candidate),
      gepa_component_candidate_id(parent2$candidate)
    )
  )
  if (mutate) {
    selected <- gepa_component_selector_ids(
      component_selector,
      child,
      failed_examples = parent1$failed_examples,
      context = selector_context
    )
    if (length(selected) != 1L) {
      gepa_checkpoint_abort_unsupported(
        "Checkpointed GEPA requires the round_robin selector to choose one component."
      )
    }
    component_id <- selected[[1L]]
    unit_id <- paste0(
      "gepa:generation:",
      state$generation,
      ":component_mutation:",
      index
    )
  }
  gepa_checkpoint_proposal_plan(
    child,
    kind = "generation",
    parent1_record_key = parent1$checkpoint_record_key,
    parent1_id = gepa_component_candidate_id(parent1$candidate),
    parent2_id = gepa_component_candidate_id(parent2$candidate),
    component_id = component_id,
    unit_id = unit_id,
    selector_context = selector_context,
    mutation = mutate,
    merge_invocation_delta = merge_invocations,
    lineage_merge_delta = lineage_merges
  )
}

gepa_checkpoint_eval_one <- function(
  program,
  candidate,
  dataset,
  metric,
  .llm,
  control,
  budget,
  stage,
  unit_id,
  active_evaluation,
  on_progress,
  failure_score = 0
) {
  materialization <- gepa_materialize_component_candidate(
    program,
    candidate,
    failure_score = failure_score
  )
  if (!materialization$ok) {
    if (optimizer_budget_unit_completed(budget, unit_id)) {
      eval_result <- optimizer_combine_eval_records(
        active_evaluation$rows,
        dataset
      )
      return(list(
        eval_result = eval_result,
        materialization = materialization,
        completed = TRUE,
        rows = active_evaluation$rows,
        failure_kind = "candidate"
      ))
    }
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
    rows <- if (completed) {
      gepa_checkpoint_project_rows(
        gepa_checkpoint_eval_rows_from_invalid(eval_result)
      )
    } else {
      active_evaluation$rows
    }
    if (completed) {
      on_progress(rows)
    }
    return(list(
      eval_result = eval_result,
      materialization = materialization,
      completed = completed,
      rows = rows,
      failure_kind = "candidate"
    ))
  }

  if (nrow(dataset) == 0L) {
    if (optimizer_budget_unit_completed(budget, unit_id)) {
      return(list(
        eval_result = optimizer_combine_eval_records(list(), dataset),
        materialization = materialization,
        completed = TRUE,
        rows = list(),
        failure_kind = NULL
      ))
    }
    can_start <- optimizer_budget_preflight(
      budget,
      stage,
      planned = list(trials = 1L),
      unit_id = unit_id,
      work_unit = "optimizer_trial",
      max_started = 0L
    )
    if (!can_start) {
      return(list(
        eval_result = optimizer_combine_eval_records(list(), dataset),
        materialization = materialization,
        completed = FALSE,
        rows = list(),
        failure_kind = NULL
      ))
    }
    eval_result <- eval_program(
      materialization$program,
      dataset,
      metric,
      .llm = .llm,
      control = control
    )
    record_optimizer_usage(
      budget,
      optimizer_eval_usage(eval_result),
      stage = stage,
      unit_id = unit_id,
      work_unit = "evaluation_dataset",
      max_started = 1L
    )
    record_eval_result_outcomes(budget, eval_result, stage)
    optimizer_budget_count_trial(budget, stage, unit_id)
    optimizer_budget_complete_unit(budget, unit_id)
    on_progress(list())
    return(list(
      eval_result = eval_result,
      materialization = materialization,
      completed = TRUE,
      rows = list(),
      failure_kind = NULL
    ))
  }

  evaluation_program <- materialization$program
  if (is.null(.llm)) {
    evaluation_program <- gepa_checkpoint_attach_default_chats(
      evaluation_program
    )
  }
  latest_rows <- active_evaluation$rows
  eval_result <- optimizer_eval_program(
    evaluation_program,
    dataset,
    metric,
    .llm = .llm,
    control = control,
    budget = budget,
    stage = stage,
    unit_id = unit_id,
    partial_records = active_evaluation$rows,
    on_progress = function(rows, row_index, current_budget) {
      latest_rows <<- rows
      on_progress(rows)
    },
    checkpoint_projection = gepa_checkpoint_project_rows,
    clone_program_per_row = TRUE,
    .propagate_provider_errors = TRUE
  )
  list(
    eval_result = eval_result,
    materialization = materialization,
    completed = optimizer_budget_unit_completed(budget, unit_id),
    rows = latest_rows,
    failure_kind = NULL
  )
}

gepa_effective_task_models <- function(program, .llm) {
  if (!is.null(.llm)) {
    return(gepa_chat_model_name(.llm))
  }
  graph <- module_graph(program, boundaries = "cross", cycles = "error")
  leaf <- vapply(
    graph$path,
    function(path) {
      prefix <- if (identical(path, "$")) "$/" else paste0(path, "/")
      !any(startsWith(graph$path, prefix))
    },
    logical(1)
  )
  graph <- graph[leaf, , drop = FALSE]
  default_chat <- get_default_chat(create = FALSE)
  models <- lapply(seq_len(nrow(graph)), function(index) {
    module <- graph$module[[index]]
    chat <- tryCatch(module$chat, error = function(e) NULL) %||% default_chat
    gepa_chat_model_name(chat)
  })
  names(models) <- graph$path
  if (all(vapply(models, is.null, logical(1)))) {
    return(NULL)
  }
  if (length(models) == 1L) {
    models[[1L]]
  } else {
    vapply(models, function(model) model %||% NA_character_, character(1))
  }
}

gepa_checkpoint_best_program <- function(
  state,
  program,
  component_specs
) {
  key <- state$best_record_key
  if (is.null(key) || is.null(state$all_records[[key]])) {
    return(gepa_clone_component_program(program))
  }
  candidate <- gepa_checkpoint_decode_candidate(
    state$all_records[[key]]$candidate,
    program,
    component_specs
  )
  materialized <- gepa_materialize_component_candidate(
    program,
    candidate,
    failure_score = 0
  )
  if (!materialized$ok || !materialized$selectable) {
    return(gepa_clone_component_program(program))
  }
  materialized$program
}

gepa_checkpoint_no_mutable_result <- function(
  teleprompter,
  program,
  budget,
  control,
  trial_log,
  task_chat_source,
  reflection_chat_source,
  .llm,
  .reflection_llm,
  metric_names
) {
  optimized <- gepa_clone_component_program(program)
  budget_summary <- optimizer_budget_summary(budget)
  details <- list(
    selection = teleprompter@selection,
    population_size = teleprompter@population_size,
    generations = teleprompter@generations,
    optimization_mode = "component_candidates",
    component_selector = gepa_component_selector_label(
      gepa_teleprompter_property(
        teleprompter,
        "component_selector",
        "round_robin"
      )
    ),
    use_merge = isTRUE(gepa_teleprompter_property(
      teleprompter,
      "use_merge",
      TRUE
    )),
    max_merge_invocations = gepa_teleprompter_property(
      teleprompter,
      "max_merge_invocations",
      5L
    ),
    merge_invocations = 0L,
    successful_merge_count = 0L,
    track_best_outputs = isTRUE(gepa_teleprompter_property(
      teleprompter,
      "track_best_outputs",
      TRUE
    )),
    optimization_skipped = TRUE,
    skip_reason = "no_mutable_components",
    model_routing = list(
      task_model = gepa_effective_task_models(program, .llm),
      task_chat_source = task_chat_source,
      reflection_model = gepa_chat_model_name(.reflection_llm),
      reflection_chat_source = reflection_chat_source,
      reflection_budget_stages = c("gepa_reflection", "gepa_flex_reflection")
    ),
    flex_paths = gepa_flex_paths(program, mutable_only = FALSE),
    mutable_flex_paths = character(),
    component_ids = character(),
    component_semantics = utils::modifyList(
      gepa_component_semantics(isTRUE(gepa_teleprompter_property(
        teleprompter,
        "track_best_outputs",
        TRUE
      ))),
      list(
        fine_grained_resume = TRUE,
        note = "No mutable components required provider work; the GEPA checkpoint is terminal."
      )
    ),
    best_candidate = NULL,
    best_candidate_id = NULL,
    best_scores = stats::setNames(
      rep(NA_real_, length(metric_names)),
      metric_names
    ),
    candidates = list(),
    parents = list(),
    val_aggregate_scores = numeric(),
    val_subscores = list(),
    per_val_instance_best_candidates = list(),
    validation_frontier_scores = numeric(),
    best_outputs_valset = if (
      isTRUE(gepa_teleprompter_property(
        teleprompter,
        "track_best_outputs",
        TRUE
      ))
    ) {
      list()
    } else {
      NULL
    },
    discovery_eval_counts = integer(),
    objective_pareto_front = list(),
    resume_supported = TRUE,
    search_state = list(
      schema_version = 1L,
      phase = "complete",
      generations_run = 0L,
      candidate_ids = character(),
      next_generation = 1L
    ),
    all_generations = list(),
    invalid_candidate_count = 0L
  )
  record_optimization_result(
    optimized,
    optimizer = "GEPA",
    status = "completed",
    best_params = list(),
    trials = tibble::tibble(),
    budget = budget_summary,
    stop_reason = "no_mutable_components",
    extensions = details
  )
  if (!is.null(trial_log)) {
    trial_log$save()
  }
  optimized
}

gepa_checkpoint_finalize <- function(
  teleprompter,
  program,
  state,
  budget,
  metrics,
  metric_names,
  component_specs,
  validation_dataset,
  discovery_dataset,
  task_chat_source,
  reflection_chat_source,
  .llm,
  .reflection_llm,
  trial_log
) {
  if (isTRUE(state$no_mutable_components)) {
    return(gepa_checkpoint_no_mutable_result(
      teleprompter,
      program,
      budget,
      NULL,
      trial_log,
      task_chat_source,
      reflection_chat_source,
      .llm,
      .reflection_llm,
      metric_names
    ))
  }

  all_records <- lapply(state$all_records, function(record) {
    gepa_checkpoint_hydrate_record(
      record,
      program,
      validation_dataset,
      discovery_dataset,
      metrics,
      component_specs,
      metric_threshold = teleprompter@metric_threshold
    )
  })
  selectable_records <- Filter(gepa_component_record_selectable, all_records)
  best_record <- if (
    is.null(state$best_record_key) ||
      is.null(state$all_records[[state$best_record_key]])
  ) {
    NULL
  } else {
    gepa_checkpoint_hydrate_record(
      state$all_records[[state$best_record_key]],
      program,
      validation_dataset,
      discovery_dataset,
      metrics,
      component_specs,
      metric_threshold = teleprompter@metric_threshold
    )
  }
  optimized <- if (is.null(best_record)) {
    gepa_clone_component_program(program)
  } else {
    materialized <- gepa_materialize_component_candidate(
      program,
      best_record$candidate,
      failure_score = 0
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
      "!" = "Returning unmodified program"
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
  track_best_outputs <- isTRUE(gepa_teleprompter_property(
    teleprompter,
    "track_best_outputs",
    TRUE
  ))
  validation_result <- gepa_component_validation_result(
    selectable_records,
    track_best_outputs = track_best_outputs
  )
  semantics <- gepa_component_semantics(track_best_outputs)
  semantics$fine_grained_resume <- TRUE
  semantics$note <- paste(
    "Checkpoint resume is supported for sequential round_robin Predict",
    "component searches with plain data predictions."
  )
  all_generations <- if (
    isTRUE(gepa_teleprompter_property(
      teleprompter,
      "track_stats",
      TRUE
    ))
  ) {
    lapply(state$all_generations, function(summary) {
      list(
        generation = summary$generation,
        population = lapply(summary$record_keys, function(key) {
          all_records[[key]]
        }),
        n_selectable = summary$n_selectable,
        n_invalid = summary$n_invalid
      )
    })
  } else {
    list()
  }
  budget_summary <- optimizer_budget_summary(budget)
  generations_run <- if (length(state$all_generations) > 0L) {
    max(vapply(state$all_generations, `[[`, integer(1), "generation"))
  } else if (length(state$all_records) > 0L) {
    max(vapply(state$all_records, `[[`, integer(1), "generation"))
  } else {
    as.integer(state$generation)
  }
  next_generation <- if (identical(state$phase, "complete")) {
    min(teleprompter@generations + 1L, generations_run + 1L)
  } else {
    as.integer(state$generation)
  }
  details <- list(
    selection = teleprompter@selection,
    population_size = teleprompter@population_size,
    generations = teleprompter@generations,
    optimization_mode = "component_candidates",
    component_selector = gepa_component_selector_label(
      gepa_teleprompter_property(
        teleprompter,
        "component_selector",
        "round_robin"
      )
    ),
    use_merge = isTRUE(gepa_teleprompter_property(
      teleprompter,
      "use_merge",
      TRUE
    )),
    max_merge_invocations = gepa_teleprompter_property(
      teleprompter,
      "max_merge_invocations",
      5L
    ),
    merge_invocations = state$merge_invocations,
    successful_merge_count = state$lineage_merges,
    track_best_outputs = track_best_outputs,
    model_routing = list(
      task_model = gepa_effective_task_models(program, .llm),
      task_chat_source = task_chat_source,
      reflection_model = gepa_chat_model_name(.reflection_llm),
      reflection_chat_source = reflection_chat_source,
      reflection_budget_stages = c("gepa_reflection", "gepa_flex_reflection")
    ),
    flex_paths = gepa_flex_paths(program, mutable_only = FALSE),
    mutable_flex_paths = character(),
    component_ids = names(component_specs),
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
    resume_supported = TRUE,
    search_state = list(
      schema_version = 1L,
      phase = state$phase,
      generations_run = generations_run,
      candidate_ids = names(validation_result$candidates),
      next_generation = next_generation,
      candidate_index = state$candidate_index,
      next_proposal_index = state$next_proposal_index
    ),
    all_generations = all_generations,
    invalid_candidate_count = sum(
      !vapply(
        state$all_records,
        function(record) isTRUE(record$candidate_valid),
        logical(1)
      )
    )
  )
  aggregate_scores <- validation_result$val_aggregate_scores
  candidate_ids <- names(aggregate_scores)
  if (is.null(candidate_ids)) {
    candidate_ids <- names(validation_result$candidates)
  }
  if (
    is.null(candidate_ids) || length(candidate_ids) != length(aggregate_scores)
  ) {
    candidate_ids <- as.character(seq_along(aggregate_scores))
  }
  trials <- tibble::tibble(
    trial_id = seq_along(aggregate_scores),
    candidate_id = candidate_ids,
    score = as.numeric(aggregate_scores)
  )
  best_candidate_id <- best_record$candidate_id %||% NULL
  best_trial <- match(best_candidate_id, trials$candidate_id)
  best_score <- if (length(final_scores) == 0L || all(is.na(final_scores))) {
    NULL
  } else {
    mean(final_scores, na.rm = TRUE)
  }
  result <- record_optimization_result(
    optimized,
    optimizer = "GEPA",
    status = if (identical(state$phase, "complete")) "completed" else "partial",
    best_score = best_score,
    best_trial = if (length(best_trial) == 0L || is.na(best_trial)) {
      NULL
    } else {
      best_trial
    },
    best_params = details$best_candidate %||% list(),
    trials = trials,
    lineage = list(
      best_candidate_id = best_candidate_id,
      parents = validation_result$parents
    ),
    budget = budget_summary,
    stop_reason = optimization_stop_reason(budget_summary),
    extensions = details
  )

  if (!is.null(trial_log)) {
    for (key in names(state$all_records)) {
      record <- state$all_records[[key]]
      if (!isTRUE(record$complete)) {
        next
      }
      trial_id <- paste0(
        "gepa-",
        substr(digest::digest(key, algo = "sha256", serialize = FALSE), 1L, 24L)
      )
      existing <- vapply(
        trial_log$trials,
        function(trial) trial@trial_id,
        character(1)
      )
      if (trial_id %in% existing) {
        next
      }
      hydrated <- all_records[[key]]
      params <- c(
        list(
          generation = hydrated$generation,
          index = hydrated$index,
          optimization_mode = "component_candidates"
        ),
        gepa_component_candidate_params(hydrated$candidate)
      )
      trial <- create_trial(
        optimizer_name = "GEPA",
        trial_id = trial_id,
        params = params
      )
      trial <- start_trial(trial)
      trial <- complete_trial(trial, hydrated$primary_eval)
      trial_log$add_trial(trial)
    }
    trial_log$save()
  }
  result
}

compile_gepa_checkpointed <- function(
  teleprompter,
  program,
  discovery_dataset,
  validation_dataset,
  metrics,
  metric_names,
  .llm,
  .reflection_llm,
  task_chat_source,
  reflection_chat_source,
  control,
  trial_log = NULL
) {
  outer_rng <- optimizer_checkpoint_capture_rng()
  on.exit(optimizer_checkpoint_restore_rng(outer_rng), add = TRUE)
  component_selector <- gepa_teleprompter_property(
    teleprompter,
    "component_selector",
    "round_robin"
  )
  if (!identical(component_selector, "round_robin")) {
    gepa_checkpoint_abort_unsupported(
      "Checkpointed GEPA currently supports component_selector = 'round_robin'."
    )
  }
  if (!identical(control@num_threads, 1L)) {
    gepa_checkpoint_abort_unsupported(
      "Checkpointed GEPA requires sequential row evaluation with num_threads = 1."
    )
  }
  component_specs <- gepa_checkpoint_supported_program(program)
  if (!is.data.frame(discovery_dataset) || !is.data.frame(validation_dataset)) {
    cli::cli_abort(
      "GEPA checkpoint datasets must be data frames",
      class = "dsprrr_gepa_config_error"
    )
  }

  runtime_registry <- artifact_validate_registry(control@checkpoint_registry)
  if (length(component_specs) == 0L) {
    task_runtime <- list(kind = "unused_no_mutable_components")
    reflection_runtime <- list(kind = "unused_no_mutable_components")
  } else {
    task_runtime <- gepa_checkpoint_task_runtime_identity(program, .llm)
    reflection_runtime <- gepa_checkpoint_chat_identity(
      .reflection_llm,
      "reflection_runtime"
    )
  }
  metric_fingerprints <- vapply(
    metrics,
    function(metric) {
      optimizer_checkpoint_metric_fingerprint(metric, runtime_registry)
    },
    character(1)
  )
  names(metric_fingerprints) <- metric_names
  track_best_outputs <- isTRUE(gepa_teleprompter_property(
    teleprompter,
    "track_best_outputs",
    TRUE
  ))
  track_stats <- isTRUE(gepa_teleprompter_property(
    teleprompter,
    "track_stats",
    TRUE
  ))
  use_merge <- isTRUE(gepa_teleprompter_property(
    teleprompter,
    "use_merge",
    TRUE
  ))
  max_merge_setting <- gepa_teleprompter_property(
    teleprompter,
    "max_merge_invocations",
    5L
  )
  max_merge_invocations <- if (is.null(max_merge_setting)) {
    Inf
  } else {
    as.integer(max_merge_setting)
  }
  config <- list(
    schema_version = 1L,
    candidate_scope = "predictor_instruction_components",
    component_selector = component_selector,
    population_size = as.integer(teleprompter@population_size),
    generations = as.integer(teleprompter@generations),
    mutation_rate = as.numeric(teleprompter@mutation_rate),
    crossover_rate = as.numeric(teleprompter@crossover_rate),
    selection = as.character(teleprompter@selection),
    metric_threshold = as.numeric(teleprompter@metric_threshold %||% 1),
    metric_names = metric_names,
    metric_fingerprints = metric_fingerprints,
    seed = teleprompter@seed,
    track_stats = track_stats,
    track_best_outputs = track_best_outputs,
    use_merge = use_merge,
    max_merge_invocations = if (is.null(max_merge_setting)) {
      NULL
    } else {
      as.integer(max_merge_setting)
    },
    execution_policy = "sequential_rows_with_fresh_task_chat_v1",
    task_chat_source = task_chat_source,
    reflection_chat_source = reflection_chat_source,
    task_runtime = task_runtime,
    reflection_runtime = reflection_runtime
  )
  if (is.null(trial_log) && !is.null(control@log_dir)) {
    trial_log <- TrialLog$new(
      optimizer_name = "GEPA",
      log_dir = control@log_dir
    )
  }
  initial_state <- gepa_checkpoint_initial_state(program, component_specs)
  if (length(component_specs) == 0L) {
    initial_state$phase <- "complete"
    initial_state$termination <- "no_mutable_components"
  }
  context <- optimizer_checkpoint_begin(
    optimizer_name = "GEPA",
    optimizer_version = 1L,
    program = program,
    data = list(
      trainset = discovery_dataset,
      valset = validation_dataset
    ),
    metric = metrics[[1L]],
    config = config,
    control = control,
    initial_state = initial_state,
    initial_phase = initial_state$phase
  )
  state <- context$search_state
  budget <- context$budget
  gepa_checkpoint_state_validate(
    state,
    program,
    component_specs,
    metric_names,
    validation_dataset,
    discovery_dataset,
    teleprompter,
    budget
  )
  if (!identical(context$phase, state$phase)) {
    cli::cli_abort(
      "GEPA checkpoint phase does not match its search state",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }

  if (isTRUE(context$resumed)) {
    if (!identical(state$phase, "complete")) {
      optimizer_checkpoint_restore_rng(context$rng)
    }
  } else if (!is.null(teleprompter@seed)) {
    set.seed(teleprompter@seed)
  }

  write_checkpoint <- function(best_program = context$best_program) {
    gepa_checkpoint_write(context, state, best_program = best_program)
  }
  if (!isTRUE(context$resumed)) {
    write_checkpoint(gepa_clone_component_program(program))
  }

  if (!identical(state$phase, "complete")) {
    failure_score <- 0
    while (!identical(state$phase, "complete")) {
      if (optimizer_budget_stopped(budget)) {
        break
      }

      if (identical(state$phase, "initializing")) {
        if (state$next_proposal_index > teleprompter@population_size) {
          state$next_proposal_index <- 1L
          state$phase <- "evaluating"
          write_checkpoint(gepa_checkpoint_best_program(
            state,
            program,
            component_specs
          ))
          next
        }
        index <- state$next_proposal_index
        if (is.null(state$active_proposal)) {
          baseline <- gepa_checkpoint_decode_candidate(
            state$population[[1L]],
            program,
            component_specs
          )
          ids <- names(baseline$components)
          component_id <- ids[[((index - 2L) %% length(ids)) + 1L]]
          context_text <- list(
            phase = "initialization",
            generation = 0L,
            index = as.integer(index)
          )
          state$active_proposal <- gepa_checkpoint_proposal_plan(
            baseline,
            kind = "initial",
            parent1_id = gepa_component_candidate_id(baseline),
            component_id = component_id,
            unit_id = paste0("gepa:initial_component_mutation:", index),
            selector_context = context_text
          )
          write_checkpoint(gepa_checkpoint_best_program(
            state,
            program,
            component_specs
          ))
        }
        plan <- state$active_proposal
      } else if (identical(state$phase, "proposing")) {
        if (state$next_proposal_index > teleprompter@population_size) {
          state$population <- state$next_population
          state$next_population <- list()
          state$next_proposal_index <- 1L
          state$candidate_index <- 1L
          state$generation_record_keys <- character()
          state["active_candidate"] <- list(NULL)
          state["active_evaluation"] <- list(NULL)
          state$phase <- "evaluating"
          write_checkpoint(gepa_checkpoint_best_program(
            state,
            program,
            component_specs
          ))
          next
        }
        index <- state$next_proposal_index
        if (is.null(state$active_proposal)) {
          remaining_merges <- if (is.infinite(max_merge_invocations)) {
            Inf
          } else {
            max(0, max_merge_invocations - state$merge_invocations)
          }
          plan <- gepa_checkpoint_plan_generation_child(
            state,
            program,
            teleprompter,
            validation_dataset,
            discovery_dataset,
            metrics,
            component_specs,
            metric_names,
            index,
            component_selector,
            use_merge,
            remaining_merges
          )
          if (is.null(plan)) {
            state$phase <- "complete"
            state$termination <- "no_selectable_candidates"
            write_checkpoint(gepa_checkpoint_best_program(
              state,
              program,
              component_specs
            ))
            next
          }
          state$merge_invocations <- as.integer(
            state$merge_invocations + plan$merge_invocation_delta
          )
          state$lineage_merges <- as.integer(
            state$lineage_merges + plan$lineage_merge_delta
          )
          state$active_proposal <- plan
          write_checkpoint(gepa_checkpoint_best_program(
            state,
            program,
            component_specs
          ))
        }
        plan <- state$active_proposal
      } else if (identical(state$phase, "evaluating")) {
        if (state$candidate_index > length(state$population)) {
          state$phase <- "selecting"
          write_checkpoint(gepa_checkpoint_best_program(
            state,
            program,
            component_specs
          ))
          next
        }
        if (is.null(state$active_candidate)) {
          candidate <- gepa_checkpoint_decode_candidate(
            state$population[[state$candidate_index]],
            program,
            component_specs
          )
          state$active_candidate <- gepa_checkpoint_new_active_candidate(
            candidate,
            state$generation,
            state$candidate_index,
            metric_names
          )
          if (!isTRUE(candidate$valid)) {
            state$active_candidate$discovery_complete <- TRUE
          }
          write_checkpoint(gepa_checkpoint_best_program(
            state,
            program,
            component_specs
          ))
        }

        while (state$active_candidate$completed_metrics < length(metrics)) {
          if (optimizer_budget_stopped(budget)) {
            break
          }
          metric_index <- state$active_candidate$completed_metrics + 1L
          unit_id <- paste0(
            "gepa:generation:",
            state$generation,
            ":candidate:",
            state$candidate_index,
            ":metric:",
            metric_index
          )
          if (is.null(state$active_evaluation)) {
            state$active_evaluation <- list(
              role = "metric",
              metric_index = as.integer(metric_index),
              unit_id = unit_id,
              rows = list()
            )
            write_checkpoint(gepa_checkpoint_best_program(
              state,
              program,
              component_specs
            ))
          }
          candidate <- gepa_checkpoint_decode_candidate(
            state$active_candidate$candidate,
            program,
            component_specs
          )
          evaluated <- gepa_checkpoint_eval_one(
            program,
            candidate,
            validation_dataset,
            metric = metrics[[metric_index]],
            .llm = .llm,
            control = control,
            budget = budget,
            stage = paste0("gepa_metric_", metric_index),
            unit_id = unit_id,
            active_evaluation = state$active_evaluation,
            on_progress = function(rows) {
              state$active_evaluation$rows <<- rows
              write_checkpoint(gepa_checkpoint_best_program(
                state,
                program,
                component_specs
              ))
            },
            failure_score = failure_score
          )
          if (!isTRUE(evaluated$completed)) {
            state$active_evaluation$rows <- evaluated$rows
            write_checkpoint(gepa_checkpoint_best_program(
              state,
              program,
              component_specs
            ))
            break
          }
          state$active_candidate$metric_rows[[metric_index]] <- evaluated$rows
          state$active_candidate$scores[[
            metric_index
          ]] <- evaluated$eval_result@mean_score
          state$active_candidate$completed_metrics <- as.integer(metric_index)
          if (identical(evaluated$failure_kind, "candidate")) {
            state$active_candidate$candidate_valid <- FALSE
            candidate$valid <- FALSE
            candidate["failure"] <- list(list(
              error_class = evaluated$materialization$error_class,
              error_message = evaluated$materialization$error_message
            ))
            state$active_candidate$candidate <- gepa_checkpoint_encode_candidate(
              candidate
            )
          }
          if (
            metric_index == 1L &&
              identical(discovery_dataset, validation_dataset)
          ) {
            state$active_candidate$discovery_rows <- evaluated$rows
            state$active_candidate$discovery_complete <- TRUE
            state$active_candidate$discovery_metric_calls <- as.integer(
              evaluated$eval_result@metric_calls
            )
          }
          state["active_evaluation"] <- list(NULL)
          write_checkpoint(gepa_checkpoint_best_program(
            state,
            program,
            component_specs
          ))
        }

        if (
          optimizer_budget_stopped(budget) ||
            state$active_candidate$completed_metrics < length(metrics)
        ) {
          break
        }
        if (!isTRUE(state$active_candidate$candidate_valid)) {
          state$active_candidate$discovery_complete <- TRUE
        }
        if (
          isTRUE(state$active_candidate$candidate_valid) &&
            !isTRUE(state$active_candidate$discovery_complete)
        ) {
          if (optimizer_budget_stopped(budget)) {
            break
          }
          unit_id <- paste0(
            "gepa:generation:",
            state$generation,
            ":candidate:",
            state$candidate_index,
            ":discovery"
          )
          if (is.null(state$active_evaluation)) {
            state$active_evaluation <- list(
              role = "discovery",
              metric_index = 1L,
              unit_id = unit_id,
              rows = list()
            )
            write_checkpoint(gepa_checkpoint_best_program(
              state,
              program,
              component_specs
            ))
          }
          candidate <- gepa_checkpoint_decode_candidate(
            state$active_candidate$candidate,
            program,
            component_specs
          )
          evaluated <- gepa_checkpoint_eval_one(
            program,
            candidate,
            discovery_dataset,
            metric = metrics[[1L]],
            .llm = .llm,
            control = control,
            budget = budget,
            stage = "gepa_discovery_metric",
            unit_id = unit_id,
            active_evaluation = state$active_evaluation,
            on_progress = function(rows) {
              state$active_evaluation$rows <<- rows
              write_checkpoint(gepa_checkpoint_best_program(
                state,
                program,
                component_specs
              ))
            },
            failure_score = failure_score
          )
          if (!isTRUE(evaluated$completed)) {
            state$active_evaluation$rows <- evaluated$rows
            write_checkpoint(gepa_checkpoint_best_program(
              state,
              program,
              component_specs
            ))
            break
          }
          state$active_candidate$discovery_rows <- evaluated$rows
          state$active_candidate$discovery_complete <- TRUE
          state$active_candidate$discovery_metric_calls <- as.integer(
            evaluated$eval_result@metric_calls
          )
          state["active_evaluation"] <- list(NULL)
          write_checkpoint(gepa_checkpoint_best_program(
            state,
            program,
            component_specs
          ))
        }

        if (!isTRUE(state$active_candidate$discovery_complete)) {
          break
        }
        record <- gepa_checkpoint_record_from_active(
          state$active_candidate,
          program,
          metric_names,
          budget,
          discovery_dataset,
          validation_dataset
        )
        key <- record$record_key
        if (key %in% names(state$all_records)) {
          cli::cli_abort(
            "GEPA checkpoint candidate record would be committed twice",
            class = "dsprrr_optimizer_checkpoint_malformed"
          )
        }
        state$all_records[[key]] <- record
        state$generation_record_keys <- c(state$generation_record_keys, key)
        state["best_record_key"] <- list(gepa_checkpoint_best_record_key(
          state$all_records,
          teleprompter@selection,
          metric_names
        ))
        state["active_candidate"] <- list(NULL)
        state["active_evaluation"] <- list(NULL)
        state$candidate_index <- as.integer(state$candidate_index + 1L)
        write_checkpoint(gepa_checkpoint_best_program(
          state,
          program,
          component_specs
        ))
      } else if (identical(state$phase, "selecting")) {
        state <- gepa_checkpoint_update_selection(
          state,
          teleprompter,
          program,
          validation_dataset,
          discovery_dataset,
          metrics,
          component_specs,
          metric_names
        )
        write_checkpoint(gepa_checkpoint_best_program(
          state,
          program,
          component_specs
        ))
        if (optimizer_budget_stopped(budget)) {
          break
        }
        current_summary <- state$all_generations[[as.character(
          state$generation
        )]]
        n_selectable <- if (is.null(current_summary)) {
          sum(vapply(
            state$all_records[state$generation_record_keys],
            function(record) {
              isTRUE(record$complete) && isTRUE(record$candidate_valid)
            },
            logical(1)
          ))
        } else {
          current_summary$n_selectable
        }
        if (state$generation >= teleprompter@generations) {
          state$phase <- "complete"
          state$termination <- "generation_limit"
          write_checkpoint(gepa_checkpoint_best_program(
            state,
            program,
            component_specs
          ))
          next
        }
        if (n_selectable == 0L) {
          state$phase <- "complete"
          state$termination <- "no_selectable_candidates"
          write_checkpoint(gepa_checkpoint_best_program(
            state,
            program,
            component_specs
          ))
          next
        }
        state$generation <- as.integer(state$generation + 1L)
        state$phase <- "proposing"
        state$next_population <- list()
        state$next_proposal_index <- 1L
        state$candidate_index <- 1L
        state$generation_record_keys <- character()
        state["active_candidate"] <- list(NULL)
        state["active_evaluation"] <- list(NULL)
        write_checkpoint(gepa_checkpoint_best_program(
          state,
          program,
          component_specs
        ))
        next
      }

      if (
        state$phase %in%
          c("initializing", "proposing") &&
          !is.null(state$active_proposal)
      ) {
        plan <- state$active_proposal
        candidate <- gepa_checkpoint_decode_candidate(
          plan$candidate,
          program,
          component_specs
        )
        if (isTRUE(plan$mutation)) {
          failed_examples <- if (is.null(plan$parent1_record_key)) {
            list()
          } else {
            parent <- gepa_checkpoint_hydrate_record(
              state$all_records[[plan$parent1_record_key]],
              program,
              validation_dataset,
              discovery_dataset,
              metrics,
              component_specs,
              metric_threshold = teleprompter@metric_threshold
            )
            parent$failed_examples
          }
          proposed <- gepa_mutate_component_candidate(
            candidate,
            program = program,
            failed_examples = failed_examples,
            .llm = .reflection_llm,
            verbose = teleprompter@verbose,
            budget = budget,
            unit_id = plan$unit_id,
            component_id = plan$component_id,
            component_selector = component_selector,
            selector_context = plan$selector_context,
            failure_score = failure_score
          )
          if (
            !optimizer_budget_unit_completed(budget, plan$unit_id) &&
              optimizer_budget_stopped(budget)
          ) {
            write_checkpoint(gepa_checkpoint_best_program(
              state,
              program,
              component_specs
            ))
            break
          }
          if (!optimizer_budget_unit_completed(budget, plan$unit_id)) {
            cli::cli_abort(
              "GEPA reflection proposal returned without a completed budget unit",
              class = "dsprrr_optimizer_invariant_error"
            )
          }
        } else {
          proposed <- candidate
        }
        if (identical(plan$kind, "initial")) {
          state$population[[state$next_proposal_index]] <-
            gepa_checkpoint_encode_candidate(proposed)
          state$next_proposal_index <- as.integer(
            state$next_proposal_index + 1L
          )
          state["active_proposal"] <- list(NULL)
          if (state$next_proposal_index > teleprompter@population_size) {
            state$next_proposal_index <- 1L
            state$phase <- "evaluating"
          }
        } else {
          state$next_population[[state$next_proposal_index]] <-
            gepa_checkpoint_encode_candidate(proposed)
          state$next_proposal_index <- as.integer(
            state$next_proposal_index + 1L
          )
          state["active_proposal"] <- list(NULL)
          if (state$next_proposal_index > teleprompter@population_size) {
            state$population <- state$next_population
            state$next_population <- list()
            state$next_proposal_index <- 1L
            state$candidate_index <- 1L
            state$generation_record_keys <- character()
            state["active_candidate"] <- list(NULL)
            state["active_evaluation"] <- list(NULL)
            state$phase <- "evaluating"
          }
        }
        write_checkpoint(gepa_checkpoint_best_program(
          state,
          program,
          component_specs
        ))
        next
      }

      if (optimizer_budget_stopped(budget)) {
        break
      }
    }
  }

  if (!identical(state$phase, "complete")) {
    state <- gepa_checkpoint_update_selection(
      state,
      teleprompter,
      program,
      validation_dataset,
      discovery_dataset,
      metrics,
      component_specs,
      metric_names
    )
  }
  result <- gepa_checkpoint_finalize(
    teleprompter,
    program,
    state,
    budget,
    metrics,
    metric_names,
    component_specs,
    validation_dataset,
    discovery_dataset,
    task_chat_source,
    reflection_chat_source,
    .llm,
    .reflection_llm,
    trial_log
  )
  checkpoint_best <- gepa_checkpoint_best_program(
    state,
    program,
    component_specs
  )
  if (!isTRUE(context$resumed)) {
    write_checkpoint(checkpoint_best)
  } else if (!identical(state$phase, context$phase)) {
    write_checkpoint(checkpoint_best)
  } else if (!identical(state$phase, "complete")) {
    write_checkpoint(checkpoint_best)
  }
  result
}
