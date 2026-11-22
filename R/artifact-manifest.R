# Validate the versioned manifest schema and graph before reconstruction.

artifact_is_plain_list <- function(value) {
  is.list(value) &&
    is.null(attr(value, "class")) &&
    length(setdiff(names(attributes(value)), "names")) == 0L
}

artifact_names_match <- function(names, expected) {
  !is.null(names) &&
    length(names) == length(expected) &&
    !anyDuplicated(names) &&
    setequal(names, expected)
}

artifact_validate_manifest <- function(artifact, dependencies = TRUE) {
  malformed <- function(message) {
    cli::cli_abort(
      c("Malformed dsprrr program artifact", "x" = message),
      class = "dsprrr_artifact_malformed"
    )
  }

  if (
    !is.list(artifact) ||
      !identical(class(artifact), c("dsprrr_program_artifact", "list")) ||
      !artifact_names_match(
        names(attributes(artifact)),
        c("names", "class")
      )
  ) {
    malformed("Artifact must be a list.")
  }
  top_level_names <- c(
    "format",
    "format_version",
    "root",
    "graph",
    "metadata",
    "exclusions",
    "integrity"
  )
  if (!artifact_names_match(names(artifact), top_level_names)) {
    malformed("Artifact has unknown or missing top-level fields.")
  }
  if (!identical(artifact$format, "dsprrr-program")) {
    malformed("Missing or invalid format marker.")
  }
  version <- artifact$format_version
  if (
    !is.numeric(version) ||
      length(version) != 1L ||
      is.na(version) ||
      !is.finite(version) ||
      version != floor(version)
  ) {
    malformed("format_version must be one integer.")
  }
  if (version != artifact_format_version()) {
    cli::cli_abort(
      c(
        "Unsupported dsprrr program artifact version",
        "x" = "Got version {.val {version}}; this package supports version {.val {artifact_format_version()}}."
      ),
      class = "dsprrr_artifact_unsupported_version"
    )
  }
  if (!artifact_is_plain_list(artifact$graph)) {
    malformed("graph must be a list.")
  }
  if (
    !artifact_names_match(
      names(artifact$graph),
      c("cycle_policy", "shared_identity", "nodes", "edges")
    )
  ) {
    malformed("graph has unknown or missing fields.")
  }
  if (!identical(artifact$graph$cycle_policy, "reject")) {
    malformed("cycle_policy must be 'reject'.")
  }
  if (!identical(artifact$graph$shared_identity, "preserve")) {
    malformed("shared_identity must be 'preserve'.")
  }
  nodes <- artifact$graph$nodes
  valid_nodes <- artifact_is_plain_list(nodes) &&
    length(nodes) > 0L &&
    !is.null(names(nodes)) &&
    all(!is.na(names(nodes)) & nzchar(names(nodes))) &&
    !anyDuplicated(names(nodes))
  if (!valid_nodes) {
    malformed("graph.nodes must be a non-empty, uniquely named list.")
  }
  if (
    !is.character(artifact$root) ||
      length(artifact$root) != 1L ||
      !artifact$root %in% names(nodes)
  ) {
    malformed("root must name one graph node.")
  }
  for (id in names(nodes)) {
    node <- nodes[[id]]
    if (!artifact_is_plain_list(node) || !identical(node$id, id)) {
      malformed(paste0("Node ", id, " has an invalid id."))
    }
    required <- c(
      "path",
      "class",
      "signature",
      "config",
      "state",
      "fields",
      "children"
    )
    missing <- setdiff(required, names(node))
    if (length(missing) > 0L) {
      malformed(paste0(
        "Node ",
        id,
        " is missing: ",
        paste(missing, collapse = ", "),
        "."
      ))
    }
    artifact_validate_child_refs(node$children, names(nodes), malformed)
    artifact_validate_node_payload(node, malformed)
  }
  edges <- artifact$graph$edges
  if (!artifact_is_plain_list(edges)) {
    malformed("graph.edges must be a list.")
  }
  for (i in seq_along(edges)) {
    edge <- edges[[i]]
    valid <- artifact_is_plain_list(edge) &&
      artifact_names_match(names(edge), c("from", "to", "path")) &&
      is.character(edge$from) &&
      length(edge$from) == 1L &&
      is.character(edge$to) &&
      length(edge$to) == 1L &&
      is.character(edge$path) &&
      length(edge$path) == 1L &&
      edge$from %in% names(nodes) &&
      edge$to %in% names(nodes)
    if (!valid) {
      malformed(paste0("Edge ", i, " is invalid."))
    }
  }
  artifact_validate_graph_manifest(artifact, malformed)
  artifact_validate_exclusions(artifact$exclusions, malformed)
  artifact_validate_metadata(artifact$metadata)
  artifact_validate_integrity(artifact)
  if (isTRUE(dependencies)) {
    artifact_validate_dependencies(artifact$metadata)
  }
  invisible(artifact)
}

artifact_validate_restored_graph <- function(program, artifact) {
  graph <- module_graph(program, boundaries = "cross", cycles = "record")
  expected_paths <- c(
    artifact$root,
    vapply(artifact$graph$edges, `[[`, character(1), "path")
  )
  expected_canonical <- c(
    artifact$root,
    vapply(artifact$graph$edges, `[[`, character(1), "to")
  )
  if (
    !identical(graph$path, expected_paths) ||
      !identical(graph$canonical_path, expected_canonical)
  ) {
    cli::cli_abort(
      "Restored program graph does not match its artifact manifest",
      class = "dsprrr_artifact_graph_error"
    )
  }
  canonical <- graph[!graph$shared & !graph$cycle, , drop = FALSE]
  for (i in seq_len(nrow(canonical))) {
    node <- artifact$graph$nodes[[canonical$path[[i]]]]
    expected_compiled <- isTRUE(node$state$compiled)
    expected_frozen <- isTRUE(node$config$.module_graph$frozen)
    if (
      !identical(canonical$compiled[[i]], expected_compiled) ||
        !identical(canonical$frozen[[i]], expected_frozen)
    ) {
      cli::cli_abort(
        "Restored program flags do not match its artifact manifest",
        class = "dsprrr_artifact_graph_error"
      )
    }
  }
  invisible(program)
}

artifact_validate_child_refs <- function(value, node_ids, malformed) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (artifact_is_node_ref(value)) {
    if (!value$.node %in% node_ids) {
      malformed(paste0(
        "Child reference targets unknown node ",
        value$.node,
        "."
      ))
    }
    return(invisible(NULL))
  }
  if (!artifact_is_plain_list(value)) {
    malformed("Child structures may contain only lists and node references.")
  }
  for (item in value) {
    artifact_validate_child_refs(item, node_ids, malformed)
  }
  invisible(NULL)
}

artifact_is_character_scalar <- function(value, nonempty = FALSE) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    (!nonempty || nzchar(value))
}

artifact_is_logical_scalar <- function(value) {
  is.logical(value) && length(value) == 1L && !is.na(value)
}

artifact_is_number_scalar <- function(
  value,
  whole = FALSE,
  minimum = -Inf
) {
  is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    (!whole || value == floor(value)) &&
    value >= minimum
}

artifact_is_optional_number_scalar <- function(
  value,
  whole = FALSE,
  minimum = -Inf
) {
  is.null(value) || artifact_is_number_scalar(value, whole, minimum)
}

artifact_validate_node_payload <- function(node, malformed) {
  expected_names <- c(
    "id",
    "path",
    "class",
    "kind",
    "signature",
    "config",
    "state",
    "optimization",
    "provider_model",
    "fields",
    "children"
  )
  if (!artifact_names_match(names(node), expected_names)) {
    malformed(paste0("Node ", node$id, " has unknown or missing fields."))
  }
  if (
    !is.character(node$class) ||
      length(node$class) != 1L ||
      !node$class %in% artifact_supported_module_classes()
  ) {
    malformed(paste0("Node ", node$id, " has an unsupported class."))
  }
  if (
    !is.character(node$kind) ||
      length(node$kind) != 1L ||
      is.na(node$kind) ||
      !node$kind %in% artifact_expected_kinds(node$class)
  ) {
    malformed(paste0("Node ", node$id, " has an invalid module kind."))
  }

  artifact_validate_signature_record(node$signature, node$id, malformed)
  artifact_validate_persisted_value(
    node$config,
    paste0("graph.nodes.", node$id, ".config"),
    drop_runtime_names = TRUE
  )
  configured_kind <- node$config$.module_kind
  if (!is.null(configured_kind) && !identical(configured_kind, node$kind)) {
    malformed(paste0(
      "Node ",
      node$id,
      " has inconsistent class and config kinds."
    ))
  }
  artifact_validate_state_and_optimization(node, malformed)
  artifact_validate_provider_model(node$provider_model, node$id, malformed)
  artifact_validate_fields(node, malformed)
  artifact_validate_children_schema(node, malformed)
  invisible(node)
}

artifact_validate_state_and_optimization <- function(node, malformed) {
  state_names <- c("best_score", "best_trial", "best_params", "compiled")
  if (
    !is.list(node$state) ||
      !artifact_names_match(names(node$state), state_names)
  ) {
    malformed(paste0("Node ", node$id, " has invalid persisted state fields."))
  }
  artifact_validate_persisted_value(
    node$state,
    paste0("graph.nodes.", node$id, ".state"),
    drop_runtime_names = TRUE
  )
  if (!artifact_is_logical_scalar(node$state$compiled)) {
    malformed(paste0("Node ", node$id, " has an invalid compiled flag."))
  }
  if (!artifact_is_optional_number_scalar(node$state$best_score)) {
    malformed(paste0("Node ", node$id, " has an invalid best score."))
  }
  if (
    !artifact_is_optional_number_scalar(
      node$state$best_trial,
      whole = TRUE,
      minimum = 1
    )
  ) {
    malformed(paste0("Node ", node$id, " has an invalid best trial."))
  }

  optimization_names <- c(
    "compiled",
    "teleprompter",
    "provenance",
    "best_score",
    "best_trial",
    "best_params",
    "n_trials"
  )
  if (
    !is.list(node$optimization) ||
      !artifact_names_match(names(node$optimization), optimization_names)
  ) {
    malformed(paste0("Node ", node$id, " has invalid optimizer provenance."))
  }
  artifact_validate_persisted_value(
    node$optimization,
    paste0("graph.nodes.", node$id, ".optimization"),
    drop_runtime_names = TRUE
  )
  optimization <- node$optimization
  if (
    !artifact_is_logical_scalar(optimization$compiled) ||
      !artifact_is_number_scalar(
        optimization$n_trials,
        whole = TRUE,
        minimum = 0
      )
  ) {
    malformed(paste0("Node ", node$id, " has invalid optimizer metadata."))
  }
  mirrored <- c("compiled", "best_score", "best_trial", "best_params")
  if (
    !all(vapply(
      mirrored,
      function(name) identical(optimization[[name]], node$state[[name]]),
      logical(1)
    ))
  ) {
    malformed(paste0(
      "Node ",
      node$id,
      " has optimizer metadata inconsistent with persisted state."
    ))
  }
  if (!identical(optimization$teleprompter, node$config$teleprompter)) {
    malformed(paste0(
      "Node ",
      node$id,
      " has optimizer metadata inconsistent with persisted config."
    ))
  }
  provenance <- optimization$provenance
  if (!is.null(provenance)) {
    if (!optimization_result_record_valid(provenance)) {
      malformed(paste0(
        "Node ",
        node$id,
        " has malformed optimization result provenance."
      ))
    }
    provenance_score <- if (is.na(provenance$best_score)) {
      NULL
    } else {
      provenance$best_score
    }
    provenance_trial <- if (is.na(provenance$best_trial)) {
      NULL
    } else {
      provenance$best_trial
    }
    if (
      !identical(provenance$optimizer, optimization$teleprompter) ||
        !identical(provenance_score, optimization$best_score) ||
        !identical(provenance_trial, optimization$best_trial) ||
        !identical(provenance$best_params, optimization$best_params) ||
        nrow(provenance$trials) != optimization$n_trials
    ) {
      malformed(paste0(
        "Node ",
        node$id,
        " has optimization result inconsistent with persisted metadata."
      ))
    }
  }
  invisible(node$state)
}

artifact_validate_signature_record <- function(signature, id, malformed) {
  if (
    !artifact_is_plain_list(signature) ||
      !artifact_names_match(
        names(signature),
        c("inputs", "output_type", "instructions")
      ) ||
      !artifact_is_plain_list(signature$inputs) ||
      !is.character(signature$instructions) ||
      length(signature$instructions) != 1L ||
      is.na(signature$instructions)
  ) {
    malformed(paste0("Node ", id, " has an invalid signature record."))
  }
  for (i in seq_along(signature$inputs)) {
    input <- signature$inputs[[i]]
    valid <- artifact_is_plain_list(input) &&
      artifact_names_match(
        names(input),
        c("name", "description", "type", "extra")
      ) &&
      is.character(input$name) &&
      length(input$name) == 1L &&
      !is.na(input$name) &&
      (is.null(input$description) ||
        (is.character(input$description) &&
          length(input$description) == 1L &&
          !is.na(input$description)))
    if (!valid) {
      malformed(paste0("Node ", id, " has an invalid signature input."))
    }
    artifact_validate_type_record(input$type, malformed)
    artifact_validate_persisted_value(
      input$extra,
      paste0("graph.nodes.", id, ".signature.inputs[[", i, "]].extra"),
      drop_runtime_names = TRUE
    )
  }
  artifact_validate_type_record(signature$output_type, malformed)
  invisible(signature)
}

artifact_validate_type_record <- function(type, malformed) {
  if (
    !artifact_is_plain_list(type) ||
      !artifact_is_character_scalar(type$kind, nonempty = TRUE)
  ) {
    malformed("Signature contains an invalid type record.")
  }
  allowed <- switch(
    type$kind,
    ignore = "kind",
    basic = c("kind", "type", "description", "required"),
    enum = c("kind", "values", "description", "required"),
    array = c("kind", "items", "description", "required"),
    object = c(
      "kind",
      "properties",
      "description",
      "required",
      "additional_properties"
    ),
    json_schema = c("kind", "json", "description", "required"),
    NULL
  )
  if (is.null(allowed) || !artifact_names_match(names(type), allowed)) {
    malformed("Signature contains an unsupported type record.")
  }
  if (type$kind %in% c("basic", "enum", "array", "object", "json_schema")) {
    valid_description <- is.null(type$description) ||
      artifact_is_character_scalar(type$description)
    if (
      !valid_description ||
        !artifact_is_logical_scalar(type$required)
    ) {
      malformed("Signature type has an invalid required flag.")
    }
  }
  if (
    identical(type$kind, "basic") &&
      (!artifact_is_character_scalar(type$type, nonempty = TRUE) ||
        !type$type %in% c("string", "number", "integer", "boolean"))
  ) {
    malformed("Basic signature type has an invalid type name.")
  }
  if (
    identical(type$kind, "enum") &&
      (!is.character(type$values) ||
        length(type$values) == 0L ||
        anyNA(type$values))
  ) {
    malformed("Enum signature type has invalid values.")
  }
  if (identical(type$kind, "array")) {
    artifact_validate_type_record(type$items, malformed)
  }
  if (identical(type$kind, "object")) {
    property_names <- names(type$properties)
    if (
      !artifact_is_plain_list(type$properties) ||
        (length(type$properties) > 0L &&
          (is.null(property_names) ||
            anyNA(property_names) ||
            !all(nzchar(property_names)) ||
            anyDuplicated(property_names))) ||
        !artifact_is_logical_scalar(type$additional_properties)
    ) {
      malformed("Object signature type has invalid properties.")
    }
    for (property in type$properties) {
      artifact_validate_type_record(property, malformed)
    }
  }
  if (identical(type$kind, "json_schema")) {
    artifact_validate_json_schema_value(type$json)
  }
  invisible(type)
}

artifact_validate_persisted_value <- function(
  value,
  path,
  drop_runtime_names
) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (is.atomic(value)) {
    exclusions <- new.env(parent = emptyenv())
    exclusions$records <- list()
    artifact_sanitize_atomic(value, path, exclusions, drop_runtime_names)
    if (length(exclusions$records) > 0L) {
      cli::cli_abort(
        "Persisted artifact value violates the secret or runtime-data policy at {.field {path}}",
        class = "dsprrr_artifact_unsafe_value"
      )
    }
    return(invisible(NULL))
  }
  if (!is.list(value)) {
    cli::cli_abort(
      "Persisted artifact contains an unsafe value at {.field {path}}",
      class = "dsprrr_artifact_unsafe_value"
    )
  }
  if (artifact_is_envelope_candidate(value)) {
    artifact_validate_envelope(value, path, drop_runtime_names)
    return(invisible(NULL))
  }
  attributes <- attributes(value)
  if (is.data.frame(value) && !artifact_is_supported_data_frame(value)) {
    cli::cli_abort(
      "Persisted artifact contains an unsupported data-frame class at {.field {path}}",
      class = "dsprrr_artifact_unsafe_value"
    )
  }
  allowed_attributes <- if (is.data.frame(value)) {
    c("class", "names", "row.names")
  } else {
    "names"
  }
  if (
    length(setdiff(names(attributes), allowed_attributes)) > 0L ||
      (!is.data.frame(value) && !is.null(attr(value, "class")))
  ) {
    cli::cli_abort(
      "Persisted artifact contains an unsupported list value at {.field {path}}",
      class = "dsprrr_artifact_unsafe_value"
    )
  }
  item_names <- names(value)
  for (i in seq_along(value)) {
    name <- if (
      !is.null(item_names) && !is.na(item_names[[i]]) && nzchar(item_names[[i]])
    ) {
      item_names[[i]]
    } else {
      as.character(i)
    }
    if (
      artifact_is_secret_name(name) ||
        (drop_runtime_names && artifact_is_runtime_name(name))
    ) {
      cli::cli_abort(
        "Persisted artifact violates the secret or runtime-data policy at {.field {path}}",
        class = "dsprrr_artifact_unsafe_value"
      )
    }
    artifact_validate_persisted_value(
      value[[i]],
      paste0(path, ".", name),
      drop_runtime_names
    )
  }
  invisible(NULL)
}

artifact_validate_envelope <- function(
  value,
  path,
  drop_runtime_names = FALSE
) {
  envelope <- value$.dsprrr
  valid <- artifact_is_plain_list(envelope) &&
    artifact_names_match(
      names(envelope),
      c("version", "kind", "payload")
    ) &&
    identical(envelope$version, 1L) &&
    artifact_is_character_scalar(envelope$kind, nonempty = TRUE) &&
    envelope$kind %in% c("runtime", "content", "plain")
  if (!isTRUE(valid)) {
    cli::cli_abort(
      "Malformed dsprrr value envelope at {.field {path}}",
      class = "dsprrr_artifact_malformed"
    )
  }
  switch(
    envelope$kind,
    runtime = {
      if (is.null(envelope$payload)) {
        cli::cli_abort(
          "Runtime envelope has no payload at {.field {path}}",
          class = "dsprrr_artifact_malformed"
        )
      }
      artifact_validate_runtime_ref(envelope$payload, path)
    },
    content = artifact_validate_content_ref(envelope$payload, path),
    plain = {
      if (!artifact_is_plain_list(envelope$payload)) {
        cli::cli_abort(
          "Plain-list envelope has an invalid payload at {.field {path}}",
          class = "dsprrr_artifact_malformed"
        )
      }
      payload_names <- names(envelope$payload)
      for (i in seq_along(envelope$payload)) {
        name <- if (
          !is.null(payload_names) &&
            !is.na(payload_names[[i]]) &&
            nzchar(payload_names[[i]])
        ) {
          payload_names[[i]]
        } else {
          as.character(i)
        }
        if (
          artifact_is_secret_name(name) ||
            (drop_runtime_names && artifact_is_runtime_name(name))
        ) {
          cli::cli_abort(
            "Plain-list envelope violates persistence policy at {.field {path}}",
            class = "dsprrr_artifact_unsafe_value"
          )
        }
        artifact_validate_persisted_value(
          envelope$payload[[i]],
          paste0(path, ".", name),
          drop_runtime_names = drop_runtime_names
        )
      }
    }
  )
  invisible(value)
}

artifact_validate_runtime_ref <- function(ref, path) {
  if (is.null(ref)) {
    return(invisible(NULL))
  }
  if (
    !artifact_is_plain_list(ref) ||
      !artifact_is_character_scalar(ref$kind, nonempty = TRUE)
  ) {
    cli::cli_abort(
      "Malformed runtime reference at {.field {path}}",
      class = "dsprrr_artifact_malformed"
    )
  }
  valid <- switch(
    ref$kind,
    builtin = artifact_names_match(names(ref), c("kind", "id", "args")) &&
      artifact_is_character_scalar(ref$id, nonempty = TRUE) &&
      artifact_is_plain_list(ref$args),
    registry = artifact_names_match(
      names(ref),
      c("kind", "id", "interface_sha256")
    ) &&
      artifact_is_character_scalar(ref$id, nonempty = TRUE) &&
      artifact_is_character_scalar(ref$interface_sha256) &&
      isTRUE(grepl("^[0-9a-f]{64}$", ref$interface_sha256)),
    trusted = artifact_names_match(
      names(ref),
      c("kind", "serialized", "sha256")
    ) &&
      is.raw(ref$serialized) &&
      artifact_is_character_scalar(ref$sha256) &&
      isTRUE(grepl("^[0-9a-f]{64}$", ref$sha256)),
    FALSE
  )
  if (!valid) {
    cli::cli_abort(
      "Malformed runtime reference at {.field {path}}",
      class = "dsprrr_artifact_malformed"
    )
  }
  if (identical(ref$kind, "builtin")) {
    artifact_validate_builtin_ref(ref, path)
  }
  if (
    identical(ref$kind, "trusted") &&
      !identical(
        digest::digest(ref$serialized, algo = "sha256", serialize = FALSE),
        ref$sha256
      )
  ) {
    cli::cli_abort(
      "Embedded runtime value failed its integrity check",
      class = "dsprrr_artifact_integrity_error"
    )
  }
  invisible(ref)
}

artifact_validate_builtin_ref <- function(ref, path) {
  artifact_validate_persisted_value(
    ref$args,
    paste0(path, ".args"),
    drop_runtime_names = TRUE
  )
  valid <- switch(
    ref$id,
    default_reward = length(ref$args) == 0L,
    reduce_first = length(ref$args) == 0L,
    reduce_majority = artifact_names_match(
      names(ref$args),
      c("field", "tie_breaker")
    ) &&
      (is.null(ref$args$field) ||
        artifact_is_character_scalar(ref$args$field, nonempty = TRUE)) &&
      artifact_is_character_scalar(ref$args$tie_breaker, nonempty = TRUE) &&
      ref$args$tie_breaker %in% c("first", "random"),
    reduce_weighted_vote = artifact_names_match(
      names(ref$args),
      "field"
    ) &&
      (is.null(ref$args$field) ||
        artifact_is_character_scalar(ref$args$field, nonempty = TRUE)),
    FALSE
  )
  if (!isTRUE(valid)) {
    cli::cli_abort(
      "Malformed built-in runtime reference at {.field {path}}",
      class = "dsprrr_artifact_malformed"
    )
  }
  invisible(ref)
}

artifact_validate_provider_model <- function(value, id, malformed) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  valid <- artifact_is_plain_list(value) &&
    artifact_names_match(names(value), c("provider", "model")) &&
    artifact_is_character_scalar(value$provider, nonempty = TRUE) &&
    (is.null(value$model) ||
      artifact_is_character_scalar(value$model, nonempty = TRUE))
  if (!valid) {
    malformed(paste0("Node ", id, " has invalid provider/model metadata."))
  }
  invisible(value)
}

artifact_validate_fields <- function(node, malformed) {
  runtime_binding_fields <- c("runner", "interpreter_factory")
  allowed <- switch(
    node$class,
    ReactModule = c("template", "demos", "max_iterations", "tools"),
    PredictModule = c("template", "demos"),
    PipelineModule = "steps",
    EnsembleModule = c("reduce_fn", "weights"),
    MultiChainComparisonModule = c(
      "M",
      "temperature",
      "comparison_template"
    ),
    RefineModule = c(
      "N",
      "reward_fn",
      "threshold",
      "fail_count",
      "feedback_template",
      "feedback_field"
    ),
    BestOfNModule = c(
      "N",
      "reward_fn",
      "threshold",
      "fail_count"
    ),
    AssertModule = c(
      "assertions",
      "max_retries",
      "on_failure",
      "feedback_template"
    ),
    KNNFewShotModule = c(
      "k",
      "vectorizer",
      "input_text",
      "train_embeddings",
      "trainset_demos",
      "merge_demos",
      "original_demos"
    ),
    FnModule = "forward_fn",
    ProgramOfThoughtModule = c(
      runtime_binding_fields,
      "max_iters",
      "extract_answer"
    ),
    CodeActModule = c(runtime_binding_fields, "tools", "max_iterations"),
    RLMModule = c(
      runtime_binding_fields,
      "max_iterations",
      "max_llm_calls",
      "max_output_chars",
      "sub_lm",
      "verbose",
      "tools"
    ),
    RAGModule = c("store", "retriever", "k", "context_format"),
    FlexModule = c(
      "module_src",
      "max_predictor_calls",
      "max_tool_calls",
      "source_format",
      "tools",
      "interpreter_factory",
      "require_sandbox"
    )
  )
  if (
    !artifact_is_plain_list(node$fields) ||
      !artifact_names_match(names(node$fields), allowed)
  ) {
    malformed(paste0("Node ", node$id, " has invalid class-specific fields."))
  }
  artifact_validate_persisted_value(
    node$fields,
    paste0("graph.nodes.", node$id, ".fields"),
    drop_runtime_names = FALSE
  )
  runtime_paths <- artifact_validate_nested_fields(node, malformed)
  for (i in seq_along(runtime_paths)) {
    artifact_validate_runtime_field(
      runtime_paths[[i]],
      paste0("graph.nodes.", node$id, ".fields.runtime[[", i, "]]")
    )
  }
  if (identical(node$class, "RLMModule")) {
    artifact_validate_provider_model(node$fields$sub_lm, node$id, malformed)
  }
  artifact_validate_field_domains(node, malformed)
  invisible(node$fields)
}

artifact_validate_runtime_field <- function(value, path) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (!artifact_is_envelope_candidate(value)) {
    cli::cli_abort(
      "Runtime field does not use a dsprrr value envelope at {.field {path}}",
      class = "dsprrr_artifact_malformed"
    )
  }
  artifact_validate_envelope(value, path, drop_runtime_names = FALSE)
  if (!identical(value$.dsprrr$kind, "runtime")) {
    cli::cli_abort(
      "Runtime field has the wrong envelope kind at {.field {path}}",
      class = "dsprrr_artifact_malformed"
    )
  }
  invisible(value)
}

artifact_validate_field_domains <- function(node, malformed) {
  invalid <- function(label = "class-specific field values") {
    malformed(paste0("Node ", node$id, " has invalid ", label, "."))
  }
  fields <- node$fields
  react_finalization <- node$config$.react_finalization
  valid_react_finalization <- is.null(react_finalization) ||
    (artifact_is_character_scalar(react_finalization, nonempty = TRUE) &&
      react_finalization %in% c("structured", "submit"))
  valid_template <- function(value) artifact_is_character_scalar(value)
  valid_demos <- function(value) artifact_is_plain_list(value)
  positive_integer <- function(value) {
    artifact_is_number_scalar(value, whole = TRUE, minimum = 1)
  }
  nonnegative_integer <- function(value) {
    artifact_is_number_scalar(value, whole = TRUE, minimum = 0)
  }
  valid_runtime_binding <- function(fields) {
    has_runner <- !is.null(fields$runner)
    has_factory <- !is.null(fields$interpreter_factory)
    xor(has_runner, has_factory)
  }

  valid <- switch(
    node$class,
    ReactModule = valid_template(fields$template) &&
      valid_demos(fields$demos) &&
      positive_integer(fields$max_iterations) &&
      valid_react_finalization,
    PredictModule = valid_template(fields$template) &&
      valid_demos(fields$demos),
    PipelineModule = artifact_validate_pipeline_fields(node, malformed),
    EnsembleModule = is.numeric(fields$weights) &&
      length(fields$weights) > 0L &&
      !anyNA(fields$weights) &&
      all(is.finite(fields$weights)) &&
      !is.null(fields$reduce_fn),
    MultiChainComparisonModule = positive_integer(fields$M) &&
      artifact_is_number_scalar(fields$temperature) &&
      artifact_is_character_scalar(fields$comparison_template),
    RefineModule = positive_integer(fields$N) &&
      !is.null(fields$reward_fn) &&
      artifact_is_number_scalar(fields$threshold) &&
      positive_integer(fields$fail_count) &&
      artifact_is_character_scalar(fields$feedback_template) &&
      artifact_is_character_scalar(fields$feedback_field, nonempty = TRUE),
    BestOfNModule = positive_integer(fields$N) &&
      !is.null(fields$reward_fn) &&
      artifact_is_number_scalar(fields$threshold) &&
      positive_integer(fields$fail_count),
    AssertModule = artifact_validate_assertion_fields(node, malformed) &&
      nonnegative_integer(fields$max_retries) &&
      artifact_is_character_scalar(fields$on_failure, nonempty = TRUE) &&
      fields$on_failure %in% c("error", "warn") &&
      artifact_is_character_scalar(fields$feedback_template),
    KNNFewShotModule = artifact_validate_knn_fields(node, malformed),
    FnModule = !is.null(fields$forward_fn),
    ProgramOfThoughtModule = valid_runtime_binding(fields) &&
      positive_integer(fields$max_iters) &&
      artifact_is_logical_scalar(fields$extract_answer),
    CodeActModule = valid_runtime_binding(fields) &&
      positive_integer(fields$max_iterations),
    RLMModule = valid_runtime_binding(fields) &&
      positive_integer(fields$max_iterations) &&
      nonnegative_integer(fields$max_llm_calls) &&
      positive_integer(fields$max_output_chars) &&
      artifact_is_logical_scalar(fields$verbose),
    RAGModule = positive_integer(fields$k) &&
      artifact_is_character_scalar(fields$context_format, nonempty = TRUE),
    FlexModule = {
      source_format <- fields$source_format
      tools <- fields$tools
      factory <- fields$interpreter_factory
      max_tool_calls <- fields$max_tool_calls
      artifact_is_character_scalar(fields$module_src, nonempty = TRUE) &&
        (is.null(fields$max_predictor_calls) ||
          nonnegative_integer(fields$max_predictor_calls)) &&
        (is.null(max_tool_calls) || nonnegative_integer(max_tool_calls)) &&
        artifact_is_character_scalar(source_format, nonempty = TRUE) &&
        source_format %in% c("json", "r") &&
        artifact_is_plain_list(tools) &&
        flex_host_tool_names_valid(tools) &&
        artifact_is_logical_scalar(fields$require_sandbox) &&
        if (identical(source_format, "json")) {
          length(tools) == 0L && is.null(factory)
        } else {
          !is.null(factory)
        }
    },
    FALSE
  )
  if (!isTRUE(valid)) {
    invalid()
  }
  invisible(fields)
}

artifact_validate_pipeline_fields <- function(node, malformed) {
  steps <- node$fields$steps
  if (!artifact_is_plain_list(steps) || length(steps) == 0L) {
    malformed(paste0("Node ", node$id, " has invalid pipeline steps."))
  }
  for (step in steps) {
    input_map <- step$input_map
    input_names <- names(input_map)
    valid_input_map <- artifact_is_plain_list(input_map) &&
      (length(input_map) == 0L ||
        (!is.null(input_names) &&
          !anyNA(input_names) &&
          all(nzchar(input_names)) &&
          !anyDuplicated(input_names) &&
          all(vapply(
            input_map,
            artifact_is_character_scalar,
            logical(1),
            nonempty = TRUE
          ))))
    output_select <- step$output_select
    valid_output_select <- is.character(output_select) &&
      !anyNA(output_select) &&
      all(nzchar(output_select)) &&
      !anyDuplicated(output_select)
    if (
      !valid_input_map ||
        !valid_output_select ||
        !artifact_is_plain_list(step$static_inputs)
    ) {
      malformed(paste0("Node ", node$id, " has invalid pipeline step values."))
    }
  }
  TRUE
}

artifact_validate_assertion_fields <- function(node, malformed) {
  assertions <- node$fields$assertions
  for (assertion in assertions) {
    valid <- !is.null(assertion$condition) &&
      artifact_is_character_scalar(assertion$message) &&
      (is.null(assertion$field) ||
        artifact_is_character_scalar(assertion$field, nonempty = TRUE)) &&
      artifact_is_character_scalar(assertion$type, nonempty = TRUE) &&
      assertion$type %in% c("assert", "suggest")
    if (!isTRUE(valid)) {
      malformed(paste0("Node ", node$id, " has an invalid assertion record."))
    }
  }
  TRUE
}

artifact_validate_knn_fields <- function(node, malformed) {
  fields <- node$fields
  valid <- artifact_is_number_scalar(fields$k, whole = TRUE, minimum = 1) &&
    !is.null(fields$vectorizer) &&
    !is.null(fields$input_text) &&
    is.matrix(fields$train_embeddings) &&
    is.numeric(fields$train_embeddings) &&
    !anyNA(fields$train_embeddings) &&
    all(is.finite(fields$train_embeddings)) &&
    artifact_is_plain_list(fields$trainset_demos) &&
    nrow(fields$train_embeddings) == length(fields$trainset_demos) &&
    artifact_is_logical_scalar(fields$merge_demos) &&
    artifact_is_plain_list(fields$original_demos)
  if (!isTRUE(valid)) {
    malformed(paste0("Node ", node$id, " has invalid KNN fields."))
  }
  TRUE
}

artifact_validate_children_schema <- function(node, malformed) {
  children <- node$children
  invalid <- function() {
    malformed(paste0("Node ", node$id, " has invalid class-specific children."))
  }
  leaf_classes <- c(
    "ReactModule",
    "PredictModule",
    "FnModule",
    "ProgramOfThoughtModule",
    "CodeActModule",
    "RAGModule",
    "FlexModule"
  )
  if (node$class %in% leaf_classes) {
    if (!artifact_is_plain_list(children) || length(children) != 0L) {
      invalid()
    }
    return(invisible(children))
  }

  valid_ref_list <- function(value, nonempty = FALSE) {
    artifact_is_plain_list(value) &&
      (!nonempty || length(value) > 0L) &&
      all(vapply(value, artifact_is_node_ref, logical(1)))
  }
  if (identical(node$class, "RLMModule")) {
    valid <- artifact_is_plain_list(children) &&
      artifact_names_match(
        names(children),
        c("generate_action", "extract")
      ) &&
      artifact_is_node_ref(children$generate_action) &&
      artifact_is_node_ref(children$extract)
    if (!valid) {
      invalid()
    }
    return(invisible(children))
  }
  valid <- switch(
    node$class,
    PipelineModule = artifact_is_plain_list(children) &&
      artifact_names_match(names(children), "steps") &&
      valid_ref_list(children$steps, nonempty = TRUE) &&
      length(children$steps) == length(node$fields$steps),
    EnsembleModule = artifact_is_plain_list(children) &&
      artifact_names_match(names(children), "modules") &&
      valid_ref_list(children$modules, nonempty = TRUE) &&
      length(children$modules) == length(node$fields$weights),
    MultiChainComparisonModule = artifact_is_plain_list(children) &&
      artifact_names_match(names(children), "inner_module") &&
      artifact_is_node_ref(children$inner_module),
    RefineModule = artifact_is_plain_list(children) &&
      artifact_names_match(names(children), "module") &&
      artifact_is_node_ref(children$module),
    BestOfNModule = artifact_is_plain_list(children) &&
      artifact_names_match(names(children), "module") &&
      artifact_is_node_ref(children$module),
    AssertModule = artifact_is_plain_list(children) &&
      artifact_names_match(names(children), "module") &&
      artifact_is_node_ref(children$module),
    KNNFewShotModule = artifact_is_plain_list(children) &&
      artifact_names_match(names(children), "module") &&
      artifact_is_node_ref(children$module),
    FALSE
  )
  if (!isTRUE(valid)) {
    invalid()
  }
  invisible(children)
}

artifact_validate_nested_fields <- function(node, malformed) {
  runtime_collection <- function(values, label) {
    if (!artifact_is_plain_list(values)) {
      malformed(paste0("Node ", node$id, " has invalid ", label, "."))
    }
    values
  }
  switch(
    node$class,
    ReactModule = runtime_collection(node$fields$tools, "tools"),
    PipelineModule = {
      steps <- node$fields$steps
      if (!artifact_is_plain_list(steps)) {
        malformed(paste0("Node ", node$id, " has invalid pipeline steps."))
      }
      for (step in steps) {
        if (
          !artifact_is_plain_list(step) ||
            !artifact_names_match(
              names(step),
              c("input_map", "output_select", "static_inputs")
            )
        ) {
          malformed(paste0(
            "Node ",
            node$id,
            " has invalid pipeline step metadata."
          ))
        }
      }
      list()
    },
    EnsembleModule = list(node$fields$reduce_fn),
    RefineModule = list(node$fields$reward_fn),
    BestOfNModule = list(node$fields$reward_fn),
    AssertModule = {
      assertions <- node$fields$assertions
      if (!artifact_is_plain_list(assertions)) {
        malformed(paste0("Node ", node$id, " has invalid assertions."))
      }
      conditions <- list()
      for (i in seq_along(assertions)) {
        assertion <- assertions[[i]]
        valid <- artifact_is_plain_list(assertion) &&
          artifact_names_match(
            names(assertion),
            c("condition", "message", "field", "type")
          ) &&
          artifact_is_character_scalar(assertion$message) &&
          (is.null(assertion$field) ||
            artifact_is_character_scalar(assertion$field, nonempty = TRUE)) &&
          artifact_is_character_scalar(assertion$type, nonempty = TRUE) &&
          assertion$type %in% c("assert", "suggest")
        if (!isTRUE(valid)) {
          malformed(paste0(
            "Node ",
            node$id,
            " has an invalid assertion record."
          ))
        }
        conditions[[i]] <- assertion$condition
      }
      conditions
    },
    KNNFewShotModule = list(
      node$fields$vectorizer,
      node$fields$input_text
    ),
    FnModule = list(node$fields$forward_fn),
    ProgramOfThoughtModule = list(
      node$fields$runner,
      node$fields$interpreter_factory
    ),
    CodeActModule = c(
      list(
        node$fields$runner,
        node$fields$interpreter_factory
      ),
      runtime_collection(node$fields$tools, "tools")
    ),
    RLMModule = c(
      list(
        node$fields$runner,
        node$fields$interpreter_factory
      ),
      runtime_collection(node$fields$tools, "tools")
    ),
    RAGModule = list(node$fields$store, node$fields$retriever),
    FlexModule = c(
      list(node$fields$interpreter_factory),
      runtime_collection(node$fields$tools, "tools")
    ),
    list()
  )
}

artifact_validate_exclusions <- function(exclusions, malformed) {
  if (!artifact_is_plain_list(exclusions)) {
    malformed("exclusions must be a list.")
  }
  allowed_reasons <- c(
    "chat",
    "credential",
    "module-reference",
    "runtime-data"
  )
  for (record in exclusions) {
    valid <- artifact_is_plain_list(record) &&
      artifact_names_match(names(record), c("path", "reason")) &&
      is.character(record$path) &&
      length(record$path) == 1L &&
      is.character(record$reason) &&
      length(record$reason) == 1L &&
      record$reason %in% allowed_reasons
    if (!valid) {
      malformed("exclusions contains an invalid record.")
    }
  }
  invisible(exclusions)
}

artifact_validate_graph_manifest <- function(artifact, malformed) {
  nodes <- artifact$graph$nodes
  if (!identical(artifact$root, "$")) {
    malformed("The canonical root node ID must be '$'.")
  }
  for (id in names(nodes)) {
    if (!identical(nodes[[id]]$path, id)) {
      malformed(paste0(
        "Node ",
        id,
        " does not use its canonical path as its ID."
      ))
    }
  }

  edge_key <- function(edge) paste(edge$from, edge$to, edge$path, sep = "\r")
  canonical_nodes <- artifact$root
  canonical_edges <- list()
  walk_canonical <- function(id, active = character()) {
    child_edges <- artifact_child_edges(
      nodes[[id]]$children,
      from = id,
      path = id
    )
    for (edge in child_edges) {
      canonical_edges[[length(canonical_edges) + 1L]] <<- edge
      if (edge$to %in% c(active, id)) {
        cli::cli_abort(
          "Cyclic references are not supported by this artifact version",
          class = "dsprrr_artifact_cycle"
        )
      }
      if (!edge$to %in% canonical_nodes) {
        canonical_nodes <<- c(canonical_nodes, edge$to)
        walk_canonical(edge$to, c(active, id))
      }
    }
    invisible(NULL)
  }
  walk_canonical(artifact$root)

  expected_keys <- vapply(canonical_edges, edge_key, character(1))
  actual_keys <- vapply(artifact$graph$edges, edge_key, character(1))
  if (!identical(expected_keys, actual_keys) || anyDuplicated(actual_keys)) {
    malformed("graph.edges does not match the node child references.")
  }
  unreachable <- setdiff(names(nodes), canonical_nodes)
  if (length(unreachable) > 0L) {
    malformed(paste0(
      "Unreachable graph node",
      if (length(unreachable) > 1L) "s: " else ": ",
      paste(unreachable, collapse = ", "),
      "."
    ))
  }
  if (!identical(names(nodes), canonical_nodes)) {
    malformed("graph.nodes is not in canonical traversal order.")
  }
  artifact_validate_composite_signatures(artifact, malformed)
  invisible(artifact)
}

artifact_validate_composite_signatures <- function(artifact, malformed) {
  nodes <- artifact$graph$nodes
  child_node <- function(ref) nodes[[ref$.node]]
  wrapper_classes <- c(
    "RefineModule",
    "BestOfNModule",
    "AssertModule",
    "KNNFewShotModule"
  )
  for (node in nodes) {
    expected <- NULL
    if (node$class %in% wrapper_classes) {
      expected <- child_node(node$children$module)$signature
    } else if (identical(node$class, "EnsembleModule")) {
      child_signatures <- lapply(
        node$children$modules,
        function(ref) child_node(ref)$signature
      )
      input_names <- lapply(child_signatures, function(signature) {
        vapply(signature$inputs, `[[`, character(1), "name")
      })
      compatible <- all(vapply(
        input_names,
        identical,
        logical(1),
        y = input_names[[1]]
      ))
      if (!compatible) {
        malformed(paste0(
          "Node ",
          node$id,
          " has incompatible ensemble child signatures."
        ))
      }
      expected <- child_signatures[[1]]
    } else if (identical(node$class, "PipelineModule")) {
      expected <- artifact_derive_pipeline_signature(node, nodes)
    }
    if (!is.null(expected) && !identical(node$signature, expected)) {
      malformed(paste0(
        "Node ",
        node$id,
        " has a signature inconsistent with its children."
      ))
    }
  }
  invisible(artifact)
}

artifact_derive_pipeline_signature <- function(node, nodes) {
  child_nodes <- lapply(node$children$steps, function(ref) nodes[[ref$.node]])
  required_inputs <- list()
  satisfied <- character()
  for (i in seq_along(child_nodes)) {
    child <- child_nodes[[i]]
    step <- node$fields$steps[[i]]
    for (input in child$signature$inputs) {
      upstream_source <- NULL
      for (from in names(step$input_map)) {
        if (identical(step$input_map[[from]], input$name)) {
          upstream_source <- from
          break
        }
      }
      if (
        input$name %in%
          satisfied ||
          (!is.null(upstream_source) && upstream_source %in% satisfied) ||
          input$name %in% names(step$static_inputs)
      ) {
        next
      }
      required_inputs[[input$name]] <- input
    }
    output <- child$signature$output_type
    output_names <- if (identical(output$kind, "object")) {
      names(output$properties)
    } else {
      "output"
    }
    satisfied <- unique(c(satisfied, output_names))
  }
  instructions <- ""
  for (child in child_nodes) {
    if (nzchar(child$signature$instructions)) {
      instructions <- child$signature$instructions
      break
    }
  }
  list(
    inputs = unname(required_inputs),
    output_type = child_nodes[[length(child_nodes)]]$signature$output_type,
    instructions = instructions
  )
}

artifact_child_edges <- function(value, from, path) {
  if (is.null(value)) {
    return(list())
  }
  if (artifact_is_node_ref(value)) {
    return(list(list(from = from, to = value$.node, path = path)))
  }
  segments <- module_graph_list_segments(value)
  edges <- list()
  for (i in seq_along(value)) {
    edges <- c(
      edges,
      artifact_child_edges(
        value[[i]],
        from = from,
        path = module_graph_append_path(path, segments[[i]])
      )
    )
  }
  edges
}

artifact_is_node_ref <- function(value) {
  artifact_is_plain_list(value) &&
    artifact_names_match(names(value), ".node") &&
    is.character(value$.node) &&
    length(value$.node) == 1L &&
    !is.na(value$.node) &&
    nzchar(value$.node)
}
