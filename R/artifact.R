#' Persist Complete dsprrr Programs
#'
#' @description
#' Program artifacts are versioned, transport-independent manifests for complete
#' dsprrr module graphs. They preserve signatures, declarative configuration,
#' demos, optimization provenance, compiled state, pipeline mappings, wrappers,
#' ensembles, and shared module identity. Runtime chats, credentials, generated
#' prompts, caches, and execution history are excluded.
#'
#' Callables and runtime objects are never captured implicitly. Supply a named
#' `registry` to store stable IDs, or set `trusted = TRUE` to embed them. Embedded
#' values are restored only when `trusted = TRUE` is also supplied while loading.
#' Registry IDs are the recommended contract for tools, custom functions,
#' retrievers, stores, and code runners.
#'
#' Declarative ellmer text, JSON, inline/remote image, and PDF content is stored
#' through a closed codec. Remote content URLs must be stable and free of user
#' information or credential/signature query parameters. Thinking, tool-call,
#' uploaded, and other runtime content still requires a registry or trusted
#' embedding. The payload digest detects changes but is not an authenticity or
#' trust signal.
#'
#' Artifacts currently reject cyclic module graphs with a typed error. Shared
#' acyclic nodes are represented once and reconstructed with identical R6
#' identity at every edge.
#'
#' @param program A dsprrr `Module` program.
#' @param registry A named list of functions or runtime objects. Artifact records
#'   contain registry names, never the registered values themselves.
#' @param trusted Whether arbitrary runtime values may be embedded or restored.
#'   This is `FALSE` by default and should be enabled only for artifacts and code
#'   you trust.
#' @param path A local artifact file. `save_program()` stages and validates a
#'   temporary file in the same directory, then atomically creates or replaces
#'   the destination. If the final replacement fails, an existing destination
#'   is left unchanged.
#'
#' @return
#' * `program_artifact()` returns a `dsprrr_program_artifact` manifest.
#' * `save_program()` invisibly returns `path`.
#' * `load_program()` returns the reconstructed root module.
#'
#' @examples
#' mod <- module(signature("text -> answer"))
#' artifact <- program_artifact(mod)
#' restored <- restore_module_config(artifact)
#'
#' path <- tempfile(fileext = ".rds")
#' save_program(mod, path)
#' restored <- load_program(path)
#' unlink(path)
#'
#' @name program-artifact
NULL

artifact_format_version <- function() 3L

#' @rdname program-artifact
#' @export
program_artifact <- function(program, registry = list(), trusted = FALSE) {
  module_graph_check_program(program)
  registry <- artifact_validate_registry(registry)
  trusted <- artifact_validate_trusted(trusted)

  graph <- module_graph(program, boundaries = "cross", cycles = "record")
  if (any(graph$cycle)) {
    cycle <- graph[which(graph$cycle)[1], , drop = FALSE]
    cli::cli_abort(
      c(
        "Cyclic module graphs cannot be persisted",
        "x" = "Path {.val {cycle$path}} points back to {.val {cycle$canonical_path}}.",
        "i" = "Break the cycle before creating an artifact. Shared acyclic modules are supported."
      ),
      class = "dsprrr_artifact_cycle"
    )
  }

  canonical <- graph[!graph$shared, , drop = FALSE]
  node_ids <- stats::setNames(
    canonical$path,
    canonical$id
  )
  exclusions <- new.env(parent = emptyenv())
  exclusions$records <- list()

  nodes <- lapply(seq_len(nrow(canonical)), function(i) {
    module <- canonical$module[[i]]
    artifact_serialize_node(
      module = module,
      id = node_ids[[canonical$id[[i]]]],
      path = canonical$path[[i]],
      node_ids = node_ids,
      registry = registry,
      trusted = trusted,
      exclusions = exclusions
    )
  })
  names(nodes) <- vapply(nodes, `[[`, character(1), "id")

  edges <- lapply(seq_len(nrow(graph))[-1], function(i) {
    parent_row <- match(graph$parent_path[[i]], graph$path)
    list(
      from = node_ids[[graph$id[[parent_row]]]],
      to = node_ids[[graph$id[[i]]]],
      path = graph$path[[i]]
    )
  })

  artifact <- structure(
    list(
      format = "dsprrr-program",
      format_version = artifact_format_version(),
      root = node_ids[[canonical$id[[1]]]],
      graph = list(
        cycle_policy = "reject",
        shared_identity = "preserve",
        nodes = nodes,
        edges = edges
      ),
      metadata = artifact_metadata(),
      exclusions = exclusions$records
    ),
    class = c("dsprrr_program_artifact", "list")
  )
  artifact$integrity <- artifact_integrity(artifact)
  artifact_validate_manifest(artifact)
  artifact
}

#' @rdname program-artifact
#' @export
save_program <- function(program, path, registry = list(), trusted = FALSE) {
  artifact <- program_artifact(
    program,
    registry = registry,
    trusted = trusted
  )
  artifact_atomic_save_rds(artifact, path)
  invisible(path)
}

#' @rdname program-artifact
#' @export
load_program <- function(path, registry = list(), trusted = FALSE) {
  artifact <- artifact_read_rds(path)
  restore_program_artifact(
    artifact,
    registry = registry,
    trusted = trusted
  )
}

artifact_validate_registry <- function(registry) {
  if (is.null(registry)) {
    registry <- list()
  }
  valid_names <- is.list(registry) &&
    (length(registry) == 0L ||
      (!is.null(names(registry)) &&
        all(!is.na(names(registry)) & nzchar(names(registry))) &&
        !anyDuplicated(names(registry))))
  if (!valid_names) {
    cli::cli_abort(
      "{.arg registry} must be a named list with unique, non-empty names",
      class = "dsprrr_artifact_registry_error"
    )
  }
  registry
}

artifact_validate_trusted <- function(trusted) {
  if (!is.logical(trusted) || length(trusted) != 1L || is.na(trusted)) {
    cli::cli_abort("{.arg trusted} must be TRUE or FALSE")
  }
  trusted
}

artifact_metadata <- function() {
  packages <- c("dsprrr", "ellmer", "R6", "S7")
  versions <- lapply(packages, function(package) {
    tryCatch(
      as.character(utils::packageVersion(package)),
      error = function(e) NA_character_
    )
  })
  names(versions) <- packages

  list(
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    packages = versions
  )
}

artifact_integrity <- function(artifact) {
  payload <- artifact[c(
    "format",
    "format_version",
    "root",
    "graph",
    "metadata",
    "exclusions"
  )]
  payload$metadata$created_at <- NULL
  list(
    algorithm = "sha256",
    payload_sha256 = digest::digest(payload, algo = "sha256", serialize = TRUE)
  )
}

artifact_validate_integrity <- function(artifact) {
  integrity <- artifact$integrity
  valid <- artifact_is_plain_list(integrity) &&
    artifact_names_match(
      names(integrity),
      c("algorithm", "payload_sha256")
    ) &&
    identical(integrity$algorithm, "sha256") &&
    is.character(integrity$payload_sha256) &&
    length(integrity$payload_sha256) == 1L &&
    !is.na(integrity$payload_sha256)
  if (!valid) {
    cli::cli_abort(
      "Program artifact has invalid integrity metadata",
      class = "dsprrr_artifact_malformed"
    )
  }
  expected <- artifact_integrity(artifact)$payload_sha256
  if (!identical(integrity$payload_sha256, expected)) {
    cli::cli_abort(
      "Program artifact integrity check failed",
      class = "dsprrr_artifact_integrity_error"
    )
  }
  invisible(artifact)
}

artifact_validate_metadata <- function(metadata) {
  allowed <- c(
    "created_at",
    "r_version",
    "packages",
    "migrated_from",
    "source_dsprrr_version"
  )
  present <- c("created_at", "r_version", "packages")
  metadata_names <- names(metadata)
  valid <- artifact_is_plain_list(metadata) &&
    !is.null(metadata_names) &&
    !anyDuplicated(metadata_names) &&
    all(present %in% metadata_names) &&
    all(metadata_names %in% allowed) &&
    is.character(metadata$created_at) &&
    length(metadata$created_at) == 1L &&
    !is.na(metadata$created_at) &&
    grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
      metadata$created_at
    ) &&
    is.character(metadata$r_version) &&
    length(metadata$r_version) == 1L &&
    !is.na(metadata$r_version) &&
    grepl("^[0-9]+([.][0-9]+)+$", metadata$r_version) &&
    artifact_is_plain_list(metadata$packages) &&
    !is.null(names(metadata$packages)) &&
    artifact_names_match(
      names(metadata$packages),
      c("dsprrr", "ellmer", "R6", "S7")
    ) &&
    all(vapply(
      metadata$packages,
      function(version) {
        is.character(version) &&
          length(version) == 1L &&
          !is.na(version) &&
          grepl("^[0-9]+([.-][0-9A-Za-z]+)*$", version)
      },
      logical(1)
    ))
  if (!valid) {
    cli::cli_abort(
      "Program artifact has invalid producer metadata",
      class = "dsprrr_artifact_malformed"
    )
  }
  if (
    !is.null(metadata$migrated_from) &&
      (!is.numeric(metadata$migrated_from) ||
        length(metadata$migrated_from) != 1L)
  ) {
    cli::cli_abort(
      "Program artifact has invalid migration metadata",
      class = "dsprrr_artifact_malformed"
    )
  }
  if (
    !is.null(metadata$source_dsprrr_version) &&
      (!is.character(metadata$source_dsprrr_version) ||
        length(metadata$source_dsprrr_version) != 1L ||
        is.na(metadata$source_dsprrr_version) ||
        !grepl(
          "^[0-9]+([.-][0-9A-Za-z]+)*$",
          metadata$source_dsprrr_version
        ))
  ) {
    cli::cli_abort(
      "Program artifact has invalid source version metadata",
      class = "dsprrr_artifact_malformed"
    )
  }
  invisible(metadata)
}

artifact_validate_dependencies <- function(metadata) {
  required_r <- tryCatch(
    base::package_version(metadata$r_version),
    error = function(e) NULL
  )
  if (is.null(required_r) || getRversion() < required_r) {
    cli::cli_abort(
      c(
        "Program artifact requires a newer R version",
        "x" = "R {.val {metadata$r_version}} or newer is required."
      ),
      class = "dsprrr_artifact_dependency_error"
    )
  }
  for (package in names(metadata$packages)) {
    required <- metadata$packages[[package]]
    if (!requireNamespace(package, quietly = TRUE)) {
      cli::cli_abort(
        c(
          "Program artifact dependency is unavailable",
          "x" = "Package {.pkg {package}} is required."
        ),
        class = "dsprrr_artifact_dependency_error"
      )
    }
    installed <- tryCatch(
      utils::packageVersion(package),
      error = function(e) NULL
    )
    required_version <- tryCatch(
      base::package_version(required),
      error = function(e) NULL
    )
    if (
      is.null(installed) ||
        is.null(required_version) ||
        installed < required_version
    ) {
      cli::cli_abort(
        c(
          "Program artifact dependency version is unavailable",
          "x" = "Package {.pkg {package}} requires version {.val {required}} or newer."
        ),
        class = "dsprrr_artifact_dependency_error"
      )
    }
  }
  invisible(metadata)
}

artifact_serialize_node <- function(
  module,
  id,
  path,
  node_ids,
  registry,
  trusted,
  exclusions
) {
  node_path <- paste0("graph.nodes.", id)
  kind <- artifact_module_kind(module)
  module_config <- module$config %||% list()
  module_config$.module_kind <- kind
  config <- artifact_sanitize_value(
    module_config,
    paste0(node_path, ".config"),
    registry,
    trusted,
    exclusions,
    drop_runtime_names = TRUE
  )
  state <- artifact_serialize_state(
    module,
    node_path,
    registry,
    trusted,
    exclusions
  )
  fields <- artifact_serialize_fields(
    module,
    node_path,
    registry,
    trusted,
    exclusions
  )
  children <- artifact_children_to_refs(
    module_children(module),
    node_ids
  )

  list(
    id = id,
    path = path,
    class = artifact_module_class(module),
    kind = kind,
    signature = artifact_serialize_signature(
      module$signature,
      registry,
      trusted,
      exclusions,
      paste0(node_path, ".signature")
    ),
    config = config,
    state = state,
    optimization = list(
      compiled = is_module_compiled_internal(module),
      teleprompter = config$teleprompter,
      provenance = config$optimizer,
      best_score = state$best_score,
      best_trial = state$best_trial,
      best_params = state$best_params,
      n_trials = artifact_module_n_trials(module, config)
    ),
    provider_model = artifact_provider_model(module$chat) %||%
      artifact_detached_runtime(module)$chat,
    fields = fields,
    children = children
  )
}

artifact_module_n_trials <- function(module, config) {
  observed <- if (is.data.frame(module$state$trials)) {
    nrow(module$state$trials)
  } else {
    0L
  }
  recorded <- config$optimizer$n_trials
  if (
    observed == 0L &&
      artifact_is_number_scalar(recorded, whole = TRUE, minimum = 0)
  ) {
    return(as.integer(recorded))
  }
  as.integer(observed)
}

artifact_module_class <- function(module) {
  supported <- artifact_supported_module_classes()
  class <- class(module)[1]
  if (!class %in% supported) {
    cli::cli_abort(
      c(
        "Unsupported module class in program artifact",
        "x" = "Cannot reconstruct {.cls {class(module)[1]}}.",
        "i" = "This artifact version supports built-in module classes only; custom Module subclasses need an explicit artifact codec."
      ),
      class = "dsprrr_artifact_unsupported_module"
    )
  }
  class
}

artifact_supported_module_classes <- function() {
  c(
    "ReactModule",
    "PredictModule",
    "PipelineModule",
    "EnsembleModule",
    "MultiChainComparisonModule",
    "RefineModule",
    "BestOfNModule",
    "AssertModule",
    "KNNFewShotModule",
    "FnModule",
    "ProgramOfThoughtModule",
    "CodeActModule",
    "RLMModule",
    "RAGModule"
  )
}

artifact_module_kind <- function(module) {
  class <- artifact_module_class(module)
  kind <- switch(
    class,
    ReactModule = "react",
    PredictModule = {
      configured <- module$config$.module_kind %||% "predict"
      if (!configured %in% c("predict", "chain_of_thought")) {
        cli::cli_abort(
          c(
            "Predict module has an inconsistent module kind",
            "x" = "Got {.val {configured}}."
          ),
          class = "dsprrr_artifact_malformed"
        )
      }
      configured
    },
    PipelineModule = "pipeline",
    EnsembleModule = "ensemble",
    MultiChainComparisonModule = "multichain",
    RefineModule = "refine",
    BestOfNModule = "best_of_n",
    AssertModule = "assert",
    KNNFewShotModule = "knn_few_shot",
    FnModule = "fn",
    ProgramOfThoughtModule = "program_of_thought",
    CodeActModule = "codeact",
    RLMModule = "rlm",
    RAGModule = "rag"
  )
  kind
}

artifact_expected_kinds <- function(class) {
  switch(
    class,
    ReactModule = "react",
    PredictModule = c("predict", "chain_of_thought"),
    PipelineModule = "pipeline",
    EnsembleModule = "ensemble",
    MultiChainComparisonModule = "multichain",
    RefineModule = "refine",
    BestOfNModule = "best_of_n",
    AssertModule = "assert",
    KNNFewShotModule = "knn_few_shot",
    FnModule = "fn",
    ProgramOfThoughtModule = "program_of_thought",
    CodeActModule = "codeact",
    RLMModule = "rlm",
    RAGModule = "rag",
    character()
  )
}

artifact_serialize_state <- function(
  module,
  node_path,
  registry,
  trusted,
  exclusions
) {
  state <- module$state %||% list()
  names <- c(
    "best_score",
    "best_trial",
    "best_params"
  )
  values <- lapply(names, function(name) {
    artifact_sanitize_value(
      state[[name]],
      paste0(node_path, ".state.", name),
      registry,
      trusted,
      exclusions,
      drop_runtime_names = FALSE
    )
  })
  names(values) <- names
  runtime_state <- c(
    "traces",
    "cache",
    "trials",
    "last_grid",
    "optimization_history",
    "attempts",
    "assertion_results",
    "executions",
    "trajectories",
    "repl_history",
    "demo_selections",
    "individual_results"
  )
  for (name in runtime_state) {
    value <- state[[name]]
    if (!is.null(value) && length(value) > 0L) {
      artifact_record_exclusion(
        exclusions,
        paste0(node_path, ".state.", name),
        "runtime-data"
      )
    }
  }
  values$compiled <- is_module_compiled_internal(module)
  values
}

artifact_serialize_fields <- function(
  module,
  node_path,
  registry,
  trusted,
  exclusions
) {
  clean <- function(value, name, drop_runtime_names = FALSE) {
    artifact_sanitize_value(
      value,
      paste0(node_path, ".fields.", name),
      registry,
      trusted,
      exclusions,
      drop_runtime_names = drop_runtime_names
    )
  }
  runtime <- function(value, name) {
    if (is.null(value)) {
      return(NULL)
    }
    artifact_envelope(
      "runtime",
      artifact_encode_runtime(
        value,
        paste0(node_path, ".fields.", name),
        registry,
        trusted
      )
    )
  }
  runtime_list <- function(values, name) {
    if (is.null(values)) {
      return(list())
    }
    refs <- lapply(seq_along(values), function(i) {
      runtime(values[[i]], paste0(name, "[[", i, "]]"))
    })
    names(refs) <- names(values)
    refs
  }

  class <- artifact_module_class(module)
  switch(
    class,
    ReactModule = list(
      template = module$template,
      demos = clean(module$demos, "demos"),
      max_iterations = module$max_iterations,
      tools = runtime_list(module$tools, "tools")
    ),
    PredictModule = list(
      template = module$template,
      demos = clean(module$demos, "demos")
    ),
    PipelineModule = list(
      steps = lapply(seq_along(module$steps), function(i) {
        step <- module$steps[[i]]
        list(
          input_map = clean(
            step@input_map,
            paste0("steps[[", i, "]].input_map")
          ),
          output_select = clean(
            step@output_select,
            paste0("steps[[", i, "]].output_select")
          ),
          static_inputs = clean(
            step@static_inputs,
            paste0("steps[[", i, "]].static_inputs")
          )
        )
      })
    ),
    EnsembleModule = list(
      reduce_fn = runtime(module$reduce_fn, "reduce_fn"),
      weights = clean(module$weights, "weights")
    ),
    MultiChainComparisonModule = list(
      M = module$M,
      temperature = module$temperature,
      comparison_template = module$comparison_template
    ),
    RefineModule = list(
      N = module$N,
      reward_fn = runtime(module$reward_fn, "reward_fn"),
      threshold = module$threshold,
      fail_count = module$fail_count,
      feedback_template = module$feedback_template,
      feedback_field = module$feedback_field
    ),
    BestOfNModule = list(
      N = module$N,
      reward_fn = runtime(module$reward_fn, "reward_fn"),
      threshold = module$threshold,
      fail_count = module$fail_count
    ),
    AssertModule = list(
      assertions = lapply(
        seq_along(module$assertion_set@assertions),
        function(i) {
          assertion <- module$assertion_set@assertions[[i]]
          list(
            condition = runtime(
              assertion@condition,
              paste0("assertions[[", i, "]].condition")
            ),
            message = assertion@message,
            field = assertion@field,
            type = assertion@type
          )
        }
      ),
      max_retries = module$max_retries,
      on_failure = module$on_failure,
      feedback_template = module$feedback_template
    ),
    KNNFewShotModule = list(
      k = module$k,
      vectorizer = runtime(module$vectorizer, "vectorizer"),
      input_text = runtime(module$input_text, "input_text"),
      train_embeddings = clean(module$train_embeddings, "train_embeddings"),
      trainset_demos = clean(module$trainset_demos, "trainset_demos"),
      merge_demos = module$merge_demos,
      original_demos = clean(module$original_demos, "original_demos")
    ),
    FnModule = list(
      forward_fn = runtime(
        module$.__enclos_env__$private$.forward_fn,
        "forward_fn"
      )
    ),
    ProgramOfThoughtModule = list(
      runner = runtime(module$runner, "runner"),
      max_iters = module$max_iters,
      extract_answer = module$extract_answer
    ),
    CodeActModule = list(
      runner = runtime(module$runner, "runner"),
      tools = runtime_list(module$tools, "tools"),
      max_iterations = module$max_iterations
    ),
    RLMModule = list(
      runner = runtime(module$runner, "runner"),
      max_iterations = module$max_iterations,
      max_llm_calls = module$max_llm_calls,
      max_output_chars = module$max_output_chars,
      sub_lm = artifact_provider_model(module$sub_lm) %||%
        artifact_detached_runtime(module)$sub_lm,
      verbose = module$verbose,
      tools = runtime_list(module$tools, "tools")
    ),
    RAGModule = list(
      store = runtime(module$store, "store"),
      retriever = runtime(module$retriever, "retriever"),
      k = module$k,
      context_format = module$context_format
    )
  )
}

artifact_children_to_refs <- function(value, node_ids) {
  if (is.null(value)) {
    return(NULL)
  }
  if (inherits(value, "Module")) {
    address <- rlang::obj_address(value)
    id <- node_ids[[address]]
    if (is.null(id)) {
      cli::cli_abort(
        "Module graph changed while creating the artifact",
        class = "dsprrr_artifact_graph_changed"
      )
    }
    return(list(.node = id))
  }
  if (!is.list(value)) {
    cli::cli_abort(
      "Invalid child structure returned by module graph adapter",
      class = "dsprrr_artifact_malformed"
    )
  }
  result <- lapply(value, artifact_children_to_refs, node_ids = node_ids)
  names(result) <- names(value)
  result
}

artifact_provider_model <- function(chat) {
  if (is.null(chat)) {
    return(NULL)
  }
  model <- tryCatch(chat$get_model(), error = function(e) NULL)
  if (!is.null(model)) {
    model <- as.character(model)[1]
  }
  classes <- setdiff(class(chat), c("Chat", "R6"))
  list(
    provider = if (length(classes) > 0L) classes[[1]] else class(chat)[1],
    model = model
  )
}

artifact_detached_runtime <- function(module) {
  attr(module, "dsprrr_artifact_runtime", exact = TRUE) %||% list()
}

artifact_record_exclusion <- function(exclusions, path, reason) {
  exclusions$records[[length(exclusions$records) + 1L]] <- list(
    path = path,
    reason = reason
  )
  invisible(NULL)
}

artifact_omit <- function() structure(list(), class = "dsprrr_artifact_omit")

artifact_is_omit <- function(value) inherits(value, "dsprrr_artifact_omit")

artifact_envelope <- function(kind, payload) {
  list(.dsprrr = list(version = 1L, kind = kind, payload = payload))
}

artifact_is_envelope_candidate <- function(value) {
  artifact_is_plain_list(value) &&
    artifact_names_match(names(value), ".dsprrr")
}

artifact_sanitize_value <- function(
  value,
  path,
  registry,
  trusted,
  exclusions,
  drop_runtime_names
) {
  if (is.null(value)) {
    return(NULL)
  }
  if (inherits(value, "Module")) {
    artifact_record_exclusion(exclusions, path, "module-reference")
    return(artifact_omit())
  }
  if (inherits(value, "Chat")) {
    artifact_record_exclusion(exclusions, path, "chat")
    return(artifact_omit())
  }
  if (inherits(value, "ellmer::Content")) {
    content <- artifact_encode_content(value, path)
    if (!is.null(content)) {
      return(artifact_envelope("content", content))
    }
    return(artifact_envelope(
      "runtime",
      artifact_encode_runtime(
        value,
        path,
        registry,
        trusted
      )
    ))
  }
  if (
    is.function(value) ||
      inherits(value, "ToolDef") ||
      inherits(value, "ellmer::ToolDef") ||
      is.environment(value) ||
      is.language(value) ||
      is.expression(value)
  ) {
    return(artifact_envelope(
      "runtime",
      artifact_encode_runtime(
        value,
        path,
        registry,
        trusted
      )
    ))
  }
  if (is.atomic(value)) {
    sanitized <- tryCatch(
      artifact_sanitize_atomic(
        value,
        path,
        exclusions,
        drop_runtime_names
      ),
      error = function(e) e
    )
    if (!inherits(sanitized, "condition")) {
      return(sanitized)
    }
    return(artifact_envelope(
      "runtime",
      artifact_encode_runtime(
        value,
        path,
        registry,
        trusted
      )
    ))
  }
  if (!is.list(value)) {
    return(artifact_envelope(
      "runtime",
      artifact_encode_runtime(
        value,
        path,
        registry,
        trusted
      )
    ))
  }
  value_attributes <- attributes(value)
  if (is.data.frame(value) && !artifact_is_supported_data_frame(value)) {
    return(artifact_envelope(
      "runtime",
      artifact_encode_runtime(
        value,
        path,
        registry,
        trusted
      )
    ))
  }
  allowed_list_attributes <- if (is.data.frame(value)) {
    c("class", "names", "row.names")
  } else {
    "names"
  }
  unknown_attributes <- setdiff(
    names(value_attributes),
    allowed_list_attributes
  )
  if (
    length(unknown_attributes) > 0L ||
      (!is.data.frame(value) && !is.null(attr(value, "class")))
  ) {
    return(artifact_envelope(
      "runtime",
      artifact_encode_runtime(
        value,
        path,
        registry,
        trusted
      )
    ))
  }

  item_names <- names(value)
  result <- list()
  for (i in seq_along(value)) {
    name <- if (!is.null(item_names) && nzchar(item_names[[i]])) {
      item_names[[i]]
    } else {
      as.character(i)
    }
    item_path <- paste0(path, ".", name)
    if (artifact_is_secret_name(name)) {
      artifact_record_exclusion(exclusions, item_path, "credential")
      next
    }
    if (drop_runtime_names && artifact_is_runtime_name(name)) {
      artifact_record_exclusion(exclusions, item_path, "runtime-data")
      next
    }
    item <- artifact_sanitize_value(
      value[[i]],
      item_path,
      registry,
      trusted,
      exclusions,
      drop_runtime_names
    )
    if (!artifact_is_omit(item)) {
      # Single-bracket assignment preserves declarative NULL values. Using
      # `[[<- NULL` removes the element and can also corrupt the following
      # name assignment, making otherwise safe configs impossible to persist.
      position <- length(result) + 1L
      result[position] <- list(item)
      if (!is.null(item_names)) {
        names(result)[[position]] <- item_names[[i]]
      }
    }
  }

  if (is.data.frame(value)) {
    class(result) <- class(value)
    attr(result, "row.names") <- attr(value, "row.names")
  } else if (artifact_is_envelope_candidate(result)) {
    return(artifact_envelope("plain", result))
  }
  result
}

artifact_encode_content <- function(value, path) {
  if (inherits(value, "ellmer::ContentText")) {
    record <- list(kind = "text", text = value@text)
  } else if (inherits(value, "ellmer::ContentJson")) {
    if (!is.null(value@data)) {
      artifact_validate_json_schema_value(value@data)
      artifact_validate_content_policy(value@data, path)
    }
    if (!is.null(value@parsed)) {
      artifact_validate_content_policy(value@parsed, path)
    }
    record <- list(kind = "json", data = value@data, string = value@string)
  } else if (inherits(value, "ellmer::ContentImageInline")) {
    record <- list(kind = "image_inline", type = value@type, data = value@data)
  } else if (inherits(value, "ellmer::ContentImageRemote")) {
    if (!artifact_is_safe_remote_url(value@url)) {
      cli::cli_abort(
        c(
          "Remote content URL cannot be persisted safely",
          "x" = "{.field {path}} contains user information, credentials, or a signed URL.",
          "i" = "Use inline content or a stable credential-free HTTPS URL."
        ),
        class = "dsprrr_artifact_unsafe_value"
      )
    }
    record <- list(
      kind = "image_remote",
      url = value@url,
      detail = value@detail
    )
  } else if (inherits(value, "ellmer::ContentPDF")) {
    record <- list(
      kind = "pdf",
      type = value@type,
      data = value@data,
      filename = value@filename
    )
  } else {
    return(NULL)
  }
  artifact_validate_content_ref(record, path)
  record
}

artifact_is_safe_remote_url <- function(url) {
  if (
    !artifact_is_character_scalar(url, nonempty = TRUE) ||
      grepl("[[:cntrl:] ]", url) ||
      !grepl("^https://", url, ignore.case = TRUE)
  ) {
    return(FALSE)
  }
  without_scheme <- sub("^[^:]+://", "", url)
  authority <- sub("[/#?].*$", "", without_scheme)
  if (
    !nzchar(authority) ||
      grepl("@", utils::URLdecode(authority), fixed = TRUE)
  ) {
    return(FALSE)
  }
  if (!grepl("?", url, fixed = TRUE)) {
    return(TRUE)
  }
  query <- sub("^[^?]*[?]", "", url)
  query <- sub("#.*$", "", query)
  if (!nzchar(query)) {
    return(TRUE)
  }
  keys <- vapply(
    strsplit(query, "[&;]")[[1]],
    function(item) utils::URLdecode(sub("=.*$", "", item)),
    character(1)
  )
  sensitive <- vapply(keys, artifact_is_secret_name, logical(1)) |
    grepl(
      "(^|[-_])(sig(nature)?|auth|credential|expires?)($|[-_])|^x-(amz|goog)-",
      keys,
      ignore.case = TRUE
    )
  !any(sensitive)
}

artifact_validate_content_ref <- function(record, path) {
  malformed <- function() {
    cli::cli_abort(
      "Malformed declarative content at {.field {path}}",
      class = "dsprrr_artifact_malformed"
    )
  }
  if (
    !artifact_is_plain_list(record) ||
      !artifact_is_character_scalar(record$kind, nonempty = TRUE)
  ) {
    malformed()
  }
  valid <- switch(
    record$kind,
    text = artifact_names_match(names(record), c("kind", "text")) &&
      artifact_is_character_scalar(record$text),
    json = artifact_names_match(
      names(record),
      c("kind", "data", "string")
    ) &&
      (!is.null(record$data) || !is.null(record$string)) &&
      (is.null(record$string) || artifact_is_character_scalar(record$string)),
    image_inline = artifact_names_match(
      names(record),
      c("kind", "type", "data")
    ) &&
      artifact_is_character_scalar(record$type, nonempty = TRUE) &&
      startsWith(record$type, "image/") &&
      (is.null(record$data) || artifact_is_character_scalar(record$data)),
    image_remote = artifact_names_match(
      names(record),
      c("kind", "url", "detail")
    ) &&
      artifact_is_safe_remote_url(record$url) &&
      artifact_is_character_scalar(record$detail) &&
      record$detail %in% c("", "auto", "low", "high"),
    pdf = artifact_names_match(
      names(record),
      c("kind", "type", "data", "filename")
    ) &&
      identical(record$type, "application/pdf") &&
      artifact_is_character_scalar(record$data) &&
      artifact_is_character_scalar(record$filename, nonempty = TRUE),
    FALSE
  )
  if (!isTRUE(valid)) {
    malformed()
  }
  if (identical(record$kind, "json") && !is.null(record$data)) {
    artifact_validate_json_schema_value(record$data)
    artifact_validate_content_policy(record$data, path)
  }
  if (identical(record$kind, "json") && !is.null(record$string)) {
    parsed <- tryCatch(
      jsonlite::fromJSON(record$string, simplifyVector = FALSE),
      error = function(e) e
    )
    if (inherits(parsed, "condition")) {
      malformed()
    }
    artifact_validate_content_policy(parsed, path)
  }
  invisible(record)
}

artifact_validate_content_policy <- function(value, path) {
  if (!is.list(value)) {
    return(invisible(NULL))
  }
  value_names <- names(value)
  for (i in seq_along(value)) {
    name <- if (
      !is.null(value_names) &&
        !is.na(value_names[[i]]) &&
        nzchar(value_names[[i]])
    ) {
      value_names[[i]]
    } else {
      as.character(i)
    }
    if (artifact_is_secret_name(name) || artifact_is_runtime_name(name)) {
      cli::cli_abort(
        "Declarative content violates persistence policy at {.field {path}}",
        class = "dsprrr_artifact_unsafe_value"
      )
    }
    artifact_validate_content_policy(value[[i]], paste0(path, ".", name))
  }
  invisible(NULL)
}

artifact_restore_content <- function(record) {
  artifact_validate_content_ref(record, "content")
  switch(
    record$kind,
    text = ellmer::ContentText(record$text),
    json = do.call(
      get("ContentJson", envir = asNamespace("ellmer")),
      list(data = record$data, string = record$string)
    ),
    image_inline = ellmer::ContentImageInline(record$type, record$data),
    image_remote = ellmer::ContentImageRemote(record$url, record$detail),
    pdf = ellmer::ContentPDF(record$type, record$data, record$filename)
  )
}

artifact_sanitize_atomic <- function(
  value,
  path,
  exclusions,
  drop_runtime_names
) {
  item_names <- names(value)
  if (!is.null(item_names)) {
    secret <- !is.na(item_names) &
      vapply(
        item_names,
        artifact_is_secret_name,
        logical(1)
      )
    runtime <- if (drop_runtime_names) {
      !is.na(item_names) &
        vapply(
          item_names,
          artifact_is_runtime_name,
          logical(1)
        )
    } else {
      rep(FALSE, length(item_names))
    }
    drop <- secret | runtime
    if (any(drop)) {
      for (i in which(drop)) {
        artifact_record_exclusion(
          exclusions,
          paste0(path, ".", item_names[[i]]),
          if (secret[[i]]) "credential" else "runtime-data"
        )
      }
      value <- value[!drop]
    }
  }
  attributes <- attributes(value)
  if (is.null(attributes)) {
    return(value)
  }
  allowed_attributes <- c(
    "class",
    "dim",
    "dimnames",
    "levels",
    "names",
    "tzone",
    "units"
  )
  unknown <- setdiff(names(attributes), allowed_attributes)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "Unsupported attributed value in program artifact",
        "x" = "{.field {path}} has attribute{?s} {.field {unknown}}."
      ),
      class = "dsprrr_artifact_unsafe_value"
    )
  }
  class <- attributes$class %||% character()
  allowed_classes <- c(
    "AsIs",
    "Date",
    "difftime",
    "factor",
    "integer64",
    "ordered",
    "POSIXct",
    "POSIXlt",
    "POSIXt"
  )
  if (length(setdiff(class, allowed_classes)) > 0L) {
    cli::cli_abort(
      c(
        "Unsupported atomic class in program artifact",
        "x" = "{.field {path}} has class {.cls {class[[1]]}}."
      ),
      class = "dsprrr_artifact_unsafe_value"
    )
  }
  unsafe_attribute <- vapply(
    attributes,
    function(attribute) {
      is.function(attribute) ||
        is.environment(attribute) ||
        is.language(attribute) ||
        is.expression(attribute)
    },
    logical(1)
  )
  if (any(unsafe_attribute)) {
    cli::cli_abort(
      "{.field {path}} contains an unsafe attribute",
      class = "dsprrr_artifact_unsafe_value"
    )
  }
  value
}

artifact_is_supported_data_frame <- function(value) {
  identical(class(value), "data.frame") ||
    identical(class(value), c("tbl_df", "tbl", "data.frame"))
}

artifact_is_secret_name <- function(name) {
  normalized <- gsub("[^a-z0-9]+", "_", tolower(name))
  grepl(
    paste0(
      "(^|_)(api_?key|access_token|auth_token|authorization|",
      "client_secret|cookies?|credentials?|password|passwd|private_key|",
      "refresh_token|secret|session_token|token)(_|$)"
    ),
    normalized
  )
}

artifact_is_runtime_name <- function(name) {
  normalized <- gsub("[^a-z0-9]+", "_", tolower(name))
  exact <- normalized %in%
    c(
      "account_id",
      "api_args",
      "base_url",
      "cache",
      "history",
      "candidate_instructions",
      "candidate_programs",
      "demo_candidates",
      "demos_added",
      "evaluations",
      "generations",
      "all_generations",
      "instruction_candidates",
      "instruction",
      "instructions",
      "endpoint",
      "extra_headers",
      "headers",
      "http_headers",
      "messages",
      "predictions",
      "prompt",
      "prompts",
      "provider_args",
      "proposed_instructions",
      "responses",
      "request_args",
      "rule",
      "rules",
      "runtime_history",
      "trial_history",
      "traces",
      "turns"
    )
  generated <- normalized != "prompt_style" &&
    (grepl("(^|_)prompts?($|_)", normalized) ||
      grepl("(^|_)generations?($|_)", normalized) ||
      grepl("(^|_)history($|_)", normalized) ||
      grepl("candidate.*instruction|instruction.*candidate", normalized))
  exact || generated
}

artifact_encode_runtime <- function(value, path, registry, trusted) {
  if (is.null(value)) {
    return(NULL)
  }
  builtin <- if (is.function(value)) artifact_builtin_callable(value) else NULL
  if (!is.null(builtin)) {
    return(builtin)
  }

  registry_id <- artifact_registry_find(value, registry)
  if (!is.null(registry_id)) {
    return(list(
      kind = "registry",
      id = registry_id,
      interface_sha256 = artifact_runtime_interface(value)
    ))
  }
  if (trusted) {
    serialized <- serialize(value, connection = NULL, version = 3)
    return(list(
      kind = "trusted",
      serialized = serialized,
      sha256 = digest::digest(serialized, algo = "sha256", serialize = FALSE)
    ))
  }

  cli::cli_abort(
    c(
      "Runtime value is not registered for persistence",
      "x" = "{.field {path}} contains {.cls {class(value)[1]}}.",
      "i" = "Supply it in a named {.arg registry}, or explicitly use {.code trusted = TRUE}."
    ),
    class = "dsprrr_artifact_unsafe_value"
  )
}

artifact_registry_find <- function(value, registry) {
  if (length(registry) == 0L) {
    return(NULL)
  }
  matches <- vapply(registry, identical, logical(1), y = value)
  if (!any(matches)) NULL else names(registry)[which(matches)[1]]
}

artifact_runtime_interface <- function(value) {
  digest::digest(
    artifact_runtime_interface_descriptor(value),
    algo = "sha256",
    serialize = TRUE
  )
}

artifact_runtime_interface_descriptor <- function(value) {
  if (inherits(value, "ToolDef") || inherits(value, "ellmer::ToolDef")) {
    return(list(
      type = "tool",
      class = class(value),
      name = value@name,
      description = value@description,
      arguments = artifact_serialize_type(value@arguments),
      convert = value@convert,
      annotations = value@annotations,
      formals = formals(value)
    ))
  }
  if (is.function(value)) {
    return(list(
      type = "function",
      class = class(value),
      formals = formals(value)
    ))
  }
  if (is.list(value)) {
    fields <- lapply(value, function(field) {
      if (is.function(field)) {
        artifact_runtime_interface_descriptor(field)
      } else {
        list(type = typeof(field), class = class(field), names = names(field))
      }
    })
    names(fields) <- names(value)
    return(list(type = "list", class = class(value), fields = fields))
  }
  list(
    type = typeof(value),
    class = class(value),
    names = tryCatch(sort(names(value)), error = function(e) NULL)
  )
}

artifact_same_function <- function(value, prototype) {
  is.function(value) &&
    identical(formals(value), formals(prototype)) &&
    identical(body(value), body(prototype))
}

artifact_builtin_environment_matches <- function(fn, bindings) {
  environment <- environment(fn)
  !is.null(environment) &&
    identical(parent.env(environment), asNamespace("dsprrr")) &&
    identical(sort(ls(environment, all.names = TRUE)), sort(bindings))
}

artifact_closure_binding <- function(fn, name, default = NULL) {
  env <- environment(fn)
  if (is.null(env) || !exists(name, envir = env, inherits = FALSE)) {
    return(default)
  }
  get(name, envir = env, inherits = FALSE)
}

artifact_builtin_callable <- function(fn) {
  if (
    artifact_same_function(fn, default_reward_fn()) &&
      artifact_builtin_environment_matches(fn, character())
  ) {
    return(list(kind = "builtin", id = "default_reward", args = list()))
  }
  if (
    artifact_same_function(fn, reduce_majority()) &&
      artifact_builtin_environment_matches(fn, c("field", "tie_breaker"))
  ) {
    return(list(
      kind = "builtin",
      id = "reduce_majority",
      args = list(
        field = artifact_closure_binding(fn, "field"),
        tie_breaker = artifact_closure_binding(fn, "tie_breaker", "first")
      )
    ))
  }
  if (
    artifact_same_function(fn, reduce_weighted_vote()) &&
      artifact_builtin_environment_matches(fn, "field")
  ) {
    return(list(
      kind = "builtin",
      id = "reduce_weighted_vote",
      args = list(field = artifact_closure_binding(fn, "field"))
    ))
  }
  if (
    artifact_same_function(fn, reduce_first()) &&
      artifact_builtin_environment_matches(fn, character())
  ) {
    return(list(kind = "builtin", id = "reduce_first", args = list()))
  }
  NULL
}

artifact_serialize_signature <- function(
  signature,
  registry,
  trusted,
  exclusions,
  path
) {
  if (!inherits(signature, "dsprrr::Signature")) {
    cli::cli_abort(
      "Artifact module has an invalid signature",
      class = "dsprrr_artifact_malformed"
    )
  }
  inputs <- lapply(seq_along(signature@inputs), function(i) {
    input <- signature@inputs[[i]]
    extra <- input[setdiff(
      names(input),
      c("name", "type", "description", "class")
    )]
    list(
      name = input$name,
      description = input$description,
      type = artifact_serialize_type(input$type),
      extra = artifact_sanitize_value(
        extra,
        paste0(path, ".inputs[[", i, "]].extra"),
        registry,
        trusted,
        exclusions,
        drop_runtime_names = FALSE
      )
    )
  })
  list(
    inputs = inputs,
    output_type = artifact_serialize_type(signature@output_type),
    instructions = signature@instructions
  )
}

artifact_serialize_type <- function(type) {
  required <- function(value) {
    tryCatch(isTRUE(value@required), error = function(e) TRUE)
  }
  description <- function(value) {
    tryCatch(value@description, error = function(e) NULL)
  }
  if (inherits(type, "ellmer::TypeIgnore")) {
    return(list(kind = "ignore"))
  }
  if (inherits(type, "ellmer::TypeJsonSchema")) {
    artifact_validate_json_schema_value(type@json)
    return(list(
      kind = "json_schema",
      json = type@json,
      description = description(type),
      required = required(type)
    ))
  }
  if (inherits(type, "ellmer::TypeBasic")) {
    return(list(
      kind = "basic",
      type = type@type,
      description = description(type),
      required = required(type)
    ))
  }
  if (inherits(type, "ellmer::TypeEnum")) {
    return(list(
      kind = "enum",
      values = type@values,
      description = description(type),
      required = required(type)
    ))
  }
  if (inherits(type, "ellmer::TypeArray")) {
    return(list(
      kind = "array",
      items = artifact_serialize_type(type@items),
      description = description(type),
      required = required(type)
    ))
  }
  if (inherits(type, "ellmer::TypeObject")) {
    return(list(
      kind = "object",
      properties = lapply(type@properties, artifact_serialize_type),
      description = description(type),
      required = required(type),
      additional_properties = isTRUE(type@additional_properties)
    ))
  }
  cli::cli_abort(
    c(
      "Unsupported signature type in program artifact",
      "x" = "Cannot persist {.cls {class(type)[1]}}."
    ),
    class = "dsprrr_artifact_unsupported_type"
  )
}

artifact_validate_json_schema_value <- function(value) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (is.atomic(value)) {
    valid_type <- is.logical(value) ||
      is.integer(value) ||
      is.double(value) ||
      is.character(value)
    valid_number <- !is.numeric(value) || all(is.finite(value))
    if (
      !valid_type ||
        anyNA(value) ||
        !valid_number ||
        !is.null(attributes(value))
    ) {
      cli::cli_abort(
        "JSON-schema types must contain only plain JSON values",
        class = "dsprrr_artifact_unsupported_type"
      )
    }
    return(invisible(NULL))
  }
  if (
    !artifact_is_plain_list(value) ||
      anyDuplicated(names(value) %||% character())
  ) {
    cli::cli_abort(
      "JSON-schema types must contain only plain JSON values",
      class = "dsprrr_artifact_unsupported_type"
    )
  }
  keys <- names(value)
  if (
    !is.null(keys) &&
      (length(keys) != length(value) ||
        anyNA(keys) ||
        any(!nzchar(keys)))
  ) {
    cli::cli_abort(
      "JSON-schema lists must be either named objects or unnamed arrays",
      class = "dsprrr_artifact_unsupported_type"
    )
  }
  for (item in value) {
    artifact_validate_json_schema_value(item)
  }
  invisible(NULL)
}

artifact_atomic_save_rds <- function(value, path) {
  path <- artifact_validate_path(path)
  directory <- dirname(path)
  if (!dir.exists(directory)) {
    cli::cli_abort(
      "Artifact directory does not exist: {.path {directory}}",
      class = "dsprrr_artifact_io_error"
    )
  }
  temporary <- artifact_private_stage(path)
  on.exit(unlink(temporary), add = TRUE)

  tryCatch(
    artifact_write_rds(value, temporary),
    error = function(e) {
      cli::cli_abort(
        "Could not write program artifact",
        parent = e,
        class = "dsprrr_artifact_io_error"
      )
    }
  )
  staged <- tryCatch(
    readRDS(temporary),
    error = function(e) {
      cli::cli_abort(
        "Could not verify staged program artifact",
        parent = e,
        class = "dsprrr_artifact_io_error"
      )
    }
  )
  artifact_validate_manifest(staged)
  artifact_atomic_replace(temporary, path, what = "program artifact")
  invisible(path)
}

artifact_atomic_write_lines <- function(lines, path) {
  path <- artifact_validate_path(path)
  directory <- dirname(path)
  if (!dir.exists(directory)) {
    cli::cli_abort(
      "Output directory does not exist: {.path {directory}}",
      class = "dsprrr_artifact_io_error"
    )
  }
  temporary <- artifact_private_stage(path)
  on.exit(unlink(temporary), add = TRUE)
  tryCatch(
    artifact_write_lines(lines, temporary),
    error = function(e) {
      cli::cli_abort(
        "Could not write exported module code",
        parent = e,
        class = "dsprrr_artifact_io_error"
      )
    }
  )
  parsed <- tryCatch(
    parse(temporary),
    error = function(e) {
      cli::cli_abort(
        "Generated module code did not parse",
        parent = e,
        class = "dsprrr_artifact_io_error"
      )
    }
  )
  if (length(parsed) == 0L) {
    cli::cli_abort(
      "Generated module code was empty",
      class = "dsprrr_artifact_io_error"
    )
  }
  artifact_atomic_replace(temporary, path, what = "exported code")
  invisible(path)
}

artifact_private_stage <- function(path) {
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
  created <- file.create(temporary, showWarnings = FALSE)
  private <- created && isTRUE(Sys.chmod(temporary, mode = "0600"))
  if (private && .Platform$OS.type == "unix") {
    private <- identical(
      as.character(as.octmode(file.info(temporary)$mode)),
      "600"
    )
  }
  if (!private) {
    unlink(temporary)
    cli::cli_abort(
      "Could not create a private staging file for {.path {path}}",
      class = "dsprrr_artifact_io_error"
    )
  }
  temporary
}

artifact_write_rds <- function(value, path) {
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  saveRDS(value, connection, version = 3)
  invisible(path)
}

artifact_write_lines <- function(lines, path) {
  connection <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  writeLines(lines, connection, useBytes = TRUE)
  invisible(path)
}

artifact_atomic_replace <- function(source, destination, what) {
  destination_exists <- file.exists(destination)
  replaced <- if (!destination_exists || .Platform$OS.type != "windows") {
    file.rename(source, destination)
  } else {
    artifact_windows_atomic_replace(source, destination)
  }

  if (!isTRUE(replaced)) {
    cli::cli_abort(
      c(
        "Could not atomically publish {what} at {.path {destination}}",
        "i" = "An existing destination was left unchanged."
      ),
      class = "dsprrr_artifact_io_error"
    )
  }
  invisible(destination)
}

artifact_windows_atomic_replace <- function(source, destination) {
  powershell <- Sys.which(c("pwsh", "powershell"))
  powershell <- unname(powershell[nzchar(powershell)][1])
  if (length(powershell) == 0L || is.na(powershell)) {
    return(FALSE)
  }

  quote_path <- function(path) {
    path <- normalizePath(path, winslash = "\\", mustWork = TRUE)
    paste0("'", gsub("'", "''", path, fixed = TRUE), "'")
  }
  command <- sprintf(
    "[System.IO.File]::Replace(%s, %s, $null, $true)",
    quote_path(source),
    quote_path(destination)
  )
  status <- tryCatch(
    suppressWarnings(system2(
      powershell,
      c(
        "-NoLogo",
        "-NoProfile",
        "-NonInteractive",
        "-Command",
        shQuote(command)
      ),
      stdout = FALSE,
      stderr = FALSE
    )),
    error = function(e) 1L
  )
  identical(status, 0L)
}

artifact_validate_path <- function(path) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    cli::cli_abort("{.arg path} must be one non-empty file path")
  }
  path
}

artifact_read_rds <- function(path) {
  path <- artifact_validate_path(path)
  if (!file.exists(path)) {
    cli::cli_abort(
      "Program artifact does not exist: {.path {path}}",
      class = "dsprrr_artifact_io_error"
    )
  }
  tryCatch(
    readRDS(path),
    error = function(e) {
      cli::cli_abort(
        "Could not read program artifact",
        parent = e,
        class = "dsprrr_artifact_malformed"
      )
    }
  )
}

artifact_validate_v2_manifest <- function(config) {
  malformed <- function(message) {
    cli::cli_abort(
      c("Malformed v2 dsprrr module artifact", "x" = message),
      class = c(
        "dsprrr_artifact_migration_error",
        "dsprrr_artifact_malformed"
      )
    )
  }
  required <- c(
    "format_version",
    "module_kind",
    "signature",
    "config",
    "state",
    "fields",
    "metadata"
  )
  allowed <- c(required, "optimization")
  config_names <- names(config)
  if (
    !artifact_is_plain_list(config) ||
      is.null(config_names) ||
      anyDuplicated(config_names) ||
      !all(required %in% config_names) ||
      !all(config_names %in% allowed) ||
      !identical(config$format_version, 2L) ||
      !artifact_is_character_scalar(config$module_kind, nonempty = TRUE) ||
      !config$module_kind %in%
        c("predict", "react", "chain_of_thought", "multichain")
  ) {
    malformed("Artifact has an invalid top-level schema.")
  }

  artifact_validate_v2_signature(config$signature, malformed)
  if (!artifact_is_plain_list(config$config)) {
    malformed("config must be a plain list.")
  }
  artifact_validate_v2_safe_value(config$config, "config", malformed)
  artifact_validate_v2_state(config$state, malformed)
  artifact_validate_v2_fields(config, malformed)
  artifact_validate_v2_metadata(config, malformed)
  if (!is.null(config$optimization)) {
    artifact_validate_v2_optimization(config, malformed)
  }
  invisible(config)
}

artifact_validate_v2_signature <- function(signature, malformed) {
  valid <- artifact_is_plain_list(signature) &&
    artifact_names_match(
      names(signature),
      c("inputs", "output_type", "instructions")
    ) &&
    artifact_is_plain_list(signature$inputs) &&
    artifact_is_character_scalar(signature$instructions)
  if (!isTRUE(valid)) {
    malformed("Signature has an invalid schema.")
  }
  input_names <- character()
  for (input in signature$inputs) {
    valid_input <- artifact_is_plain_list(input) &&
      artifact_names_match(
        names(input),
        c("name", "description", "type", "extra")
      ) &&
      artifact_is_character_scalar(input$name, nonempty = TRUE) &&
      (is.null(input$description) ||
        artifact_is_character_scalar(input$description)) &&
      artifact_is_plain_list(input$extra)
    if (!isTRUE(valid_input)) {
      malformed("Signature contains an invalid input record.")
    }
    input_names <- c(input_names, input$name)
    artifact_validate_v2_type(input$type, malformed)
    artifact_validate_v2_safe_value(
      input$extra,
      "signature.input.extra",
      malformed
    )
  }
  if (anyDuplicated(input_names)) {
    malformed("Signature input names must be unique.")
  }
  artifact_validate_v2_type(signature$output_type, malformed)
  invisible(signature)
}

artifact_validate_v2_type <- function(type, malformed) {
  record <- tryCatch(artifact_serialize_type(type), error = function(e) e)
  if (inherits(record, "condition")) {
    malformed("Signature contains an unsupported ellmer type.")
  }
  artifact_validate_type_record(record, malformed)
  invisible(type)
}

artifact_validate_v2_safe_value <- function(value, path, malformed) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (inherits(value, "ellmer::Content")) {
    content <- tryCatch(
      artifact_encode_content(value, path),
      error = function(e) e
    )
    if (inherits(content, "condition") || is.null(content)) {
      malformed(paste0(path, " contains unsupported runtime content."))
    }
    return(invisible(NULL))
  }
  if (is.atomic(value)) {
    exclusions <- new.env(parent = emptyenv())
    exclusions$records <- list()
    valid <- tryCatch(
      {
        artifact_sanitize_atomic(
          value,
          path,
          exclusions,
          drop_runtime_names = FALSE
        )
        TRUE
      },
      error = function(e) FALSE
    )
    if (!valid) {
      malformed(paste0(path, " contains an unsafe atomic value."))
    }
    return(invisible(NULL))
  }
  if (!is.list(value)) {
    malformed(paste0(path, " contains an unsafe runtime value."))
  }
  attributes <- attributes(value)
  supported_data_frame <- is.data.frame(value) &&
    artifact_is_supported_data_frame(value)
  allowed_attributes <- if (supported_data_frame) {
    c("class", "names", "row.names")
  } else {
    "names"
  }
  if (
    (is.data.frame(value) && !supported_data_frame) ||
      length(setdiff(names(attributes), allowed_attributes)) > 0L ||
      (!supported_data_frame && !is.null(attr(value, "class")))
  ) {
    malformed(paste0(path, " contains an unsafe list value."))
  }
  for (i in seq_along(value)) {
    artifact_validate_v2_safe_value(
      value[[i]],
      paste0(path, "[[", i, "]]"),
      malformed
    )
  }
  invisible(NULL)
}

artifact_validate_v2_state <- function(state, malformed) {
  expected <- c(
    "compiled",
    "best_score",
    "best_trial",
    "best_params",
    "trials",
    "last_grid",
    "optimization_history"
  )
  valid <- artifact_is_plain_list(state) &&
    artifact_names_match(names(state), expected) &&
    artifact_is_logical_scalar(state$compiled) &&
    artifact_is_optional_number_scalar(state$best_score) &&
    artifact_is_optional_number_scalar(
      state$best_trial,
      whole = TRUE,
      minimum = 1
    ) &&
    is.data.frame(state$trials) &&
    artifact_is_supported_data_frame(state$trials) &&
    is.data.frame(state$last_grid) &&
    artifact_is_supported_data_frame(state$last_grid) &&
    is.list(state$optimization_history)
  if (!isTRUE(valid)) {
    malformed("Artifact has invalid persisted state.")
  }
  artifact_validate_v2_safe_value(
    state[c("compiled", "best_score", "best_trial", "best_params")],
    "state.curated",
    malformed
  )
  invisible(state)
}

artifact_validate_v2_fields <- function(config, malformed) {
  fields <- config$fields
  expected <- switch(
    config$module_kind,
    predict = c("template", "demos"),
    react = c("template", "demos", "max_iterations", "tools"),
    chain_of_thought = c("template", "demos"),
    multichain = c(
      "M",
      "temperature",
      "comparison_template",
      "inner_module"
    )
  )
  if (
    !artifact_is_plain_list(fields) ||
      !artifact_names_match(names(fields), expected)
  ) {
    malformed("Artifact has invalid class-specific fields.")
  }
  valid <- switch(
    config$module_kind,
    predict = artifact_is_character_scalar(fields$template) &&
      artifact_is_plain_list(fields$demos),
    chain_of_thought = artifact_is_character_scalar(fields$template) &&
      artifact_is_plain_list(fields$demos),
    react = artifact_is_character_scalar(fields$template) &&
      artifact_is_plain_list(fields$demos) &&
      artifact_is_number_scalar(
        fields$max_iterations,
        whole = TRUE,
        minimum = 1
      ) &&
      artifact_is_plain_list(fields$tools) &&
      length(fields$tools) == 0L,
    multichain = artifact_is_number_scalar(
      fields$M,
      whole = TRUE,
      minimum = 1
    ) &&
      artifact_is_number_scalar(fields$temperature) &&
      (is.null(fields$comparison_template) ||
        artifact_is_character_scalar(fields$comparison_template)) &&
      is.list(fields$inner_module),
    FALSE
  )
  if (!isTRUE(valid)) {
    malformed("Artifact has invalid class-specific field values.")
  }
  safe_fields <- fields[setdiff(names(fields), "inner_module")]
  artifact_validate_v2_safe_value(safe_fields, "fields", malformed)
  if (identical(config$module_kind, "multichain")) {
    artifact_validate_v2_manifest(fields$inner_module)
  }
  invisible(fields)
}

artifact_validate_v2_metadata <- function(config, malformed) {
  metadata <- config$metadata
  allowed <- c(
    "module_class",
    "created_at",
    "dsprrr_version",
    "r_version",
    "module_type"
  )
  metadata_names <- names(metadata)
  expected_class <- switch(
    config$module_kind,
    predict = "PredictModule",
    chain_of_thought = "PredictModule",
    react = "ReactModule",
    multichain = "MultiChainComparisonModule"
  )
  valid <- artifact_is_plain_list(metadata) &&
    !is.null(metadata_names) &&
    !anyDuplicated(metadata_names) &&
    all(metadata_names %in% allowed) &&
    "module_class" %in% metadata_names &&
    identical(metadata$module_class, expected_class) &&
    (is.null(metadata$dsprrr_version) ||
      artifact_is_character_scalar(metadata$dsprrr_version, nonempty = TRUE)) &&
    (is.null(metadata$r_version) ||
      artifact_is_character_scalar(metadata$r_version, nonempty = TRUE)) &&
    (is.null(metadata$module_type) ||
      identical(metadata$module_type, expected_class))
  if (!isTRUE(valid)) {
    malformed("Artifact has invalid producer metadata.")
  }
  artifact_validate_v2_safe_value(metadata, "metadata", malformed)
  invisible(metadata)
}

artifact_validate_v2_optimization <- function(config, malformed) {
  optimization <- config$optimization
  expected <- c(
    "compiled",
    "best_score",
    "best_trial",
    "best_params",
    "n_trials"
  )
  valid <- artifact_is_plain_list(optimization) &&
    artifact_names_match(names(optimization), expected) &&
    identical(optimization$compiled, config$state$compiled) &&
    identical(optimization$best_score, config$state$best_score) &&
    identical(optimization$best_trial, config$state$best_trial) &&
    identical(optimization$best_params, config$state$best_params) &&
    artifact_is_number_scalar(
      optimization$n_trials,
      whole = TRUE,
      minimum = 0
    )
  if (!isTRUE(valid)) {
    malformed("Artifact has invalid optimization metadata.")
  }
  invisible(optimization)
}

restore_program_artifact <- function(
  artifact,
  registry = list(),
  trusted = FALSE,
  signature = NULL
) {
  registry <- artifact_validate_registry(registry)
  trusted <- artifact_validate_trusted(trusted)

  artifact_version <- if (is.list(artifact)) {
    unclass(artifact)[["format_version"]]
  } else {
    NULL
  }
  if (identical(artifact_version, 2L)) {
    artifact_validate_v2_manifest(artifact)
    artifact <- migrate_program_artifact_v2(
      artifact,
      signature = signature,
      registry = registry,
      trusted = trusted
    )
    signature <- NULL
  }
  artifact_validate_manifest(artifact)

  if (!is.null(signature)) {
    if (!inherits(signature, "dsprrr::Signature")) {
      cli::cli_abort("{.arg signature} must be a Signature object")
    }
    root <- artifact$root
    if (
      length(artifact$graph$nodes) > 1L ||
        length(artifact$graph$nodes[[root]]$children) > 0L
    ) {
      cli::cli_abort(
        c(
          "A stored signature cannot override a composite program",
          "i" = "Update the source program's component signatures and create a new artifact."
        ),
        class = "dsprrr_artifact_signature_override_error"
      )
    }
    placeholder <- new.env(parent = emptyenv())
    placeholder$records <- list()
    artifact$graph$nodes[[root]]$signature <- artifact_serialize_signature(
      signature,
      registry,
      trusted,
      placeholder,
      paste0("graph.nodes.", root, ".signature")
    )
  }

  cache <- new.env(parent = emptyenv(), hash = TRUE)
  program <- artifact_build_node(
    artifact$root,
    artifact,
    cache,
    active = character(),
    registry = registry,
    trusted = trusted
  )
  artifact_validate_restored_graph(program, artifact)
  program
}

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

artifact_validate_manifest <- function(artifact) {
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
      is.na(version)
  ) {
    malformed("format_version must be one integer.")
  }
  if (
    version != artifact_format_version() ||
      version != as.integer(version)
  ) {
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
  artifact_validate_dependencies(artifact$metadata)
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
  if (
    !identical(optimization$teleprompter, node$config$teleprompter) ||
      !identical(optimization$provenance, node$config$optimizer)
  ) {
    malformed(paste0(
      "Node ",
      node$id,
      " has optimizer metadata inconsistent with persisted config."
    ))
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
            any(!nzchar(property_names)) ||
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
    ProgramOfThoughtModule = c("runner", "max_iters", "extract_answer"),
    CodeActModule = c("runner", "tools", "max_iterations"),
    RLMModule = c(
      "runner",
      "max_iterations",
      "max_llm_calls",
      "max_output_chars",
      "sub_lm",
      "verbose",
      "tools"
    ),
    RAGModule = c("store", "retriever", "k", "context_format")
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
  valid_template <- function(value) artifact_is_character_scalar(value)
  valid_demos <- function(value) artifact_is_plain_list(value)
  positive_integer <- function(value) {
    artifact_is_number_scalar(value, whole = TRUE, minimum = 1)
  }
  nonnegative_integer <- function(value) {
    artifact_is_number_scalar(value, whole = TRUE, minimum = 0)
  }

  valid <- switch(
    node$class,
    ReactModule = valid_template(fields$template) &&
      valid_demos(fields$demos) &&
      positive_integer(fields$max_iterations),
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
    ProgramOfThoughtModule = !is.null(fields$runner) &&
      positive_integer(fields$max_iters) &&
      artifact_is_logical_scalar(fields$extract_answer),
    CodeActModule = !is.null(fields$runner) &&
      positive_integer(fields$max_iterations),
    RLMModule = !is.null(fields$runner) &&
      positive_integer(fields$max_iterations) &&
      positive_integer(fields$max_llm_calls) &&
      positive_integer(fields$max_output_chars) &&
      artifact_is_logical_scalar(fields$verbose),
    RAGModule = positive_integer(fields$k) &&
      artifact_is_character_scalar(fields$context_format, nonempty = TRUE),
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
    "RLMModule",
    "RAGModule"
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
    ProgramOfThoughtModule = list(node$fields$runner),
    CodeActModule = c(
      list(node$fields$runner),
      runtime_collection(node$fields$tools, "tools")
    ),
    RLMModule = c(
      list(node$fields$runner),
      runtime_collection(node$fields$tools, "tools")
    ),
    RAGModule = list(node$fields$store, node$fields$retriever),
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

artifact_build_node <- function(
  id,
  artifact,
  cache,
  active,
  registry,
  trusted
) {
  if (exists(id, envir = cache, inherits = FALSE)) {
    return(get(id, envir = cache, inherits = FALSE))
  }
  if (id %in% active) {
    cli::cli_abort(
      "Cyclic references are not supported by this artifact version",
      class = "dsprrr_artifact_cycle"
    )
  }
  node <- artifact$graph$nodes[[id]]
  children <- artifact_children_from_refs(
    node$children,
    artifact,
    cache,
    c(active, id),
    registry,
    trusted
  )
  construction <- tryCatch(
    artifact_construct_module(
      node,
      children,
      registry,
      trusted
    ),
    error = function(e) e
  )
  if (inherits(construction, "condition")) {
    if (any(grepl("^dsprrr_artifact_", class(construction)))) {
      stop(construction)
    }
    cli::cli_abort(
      c(
        "Could not reconstruct module node",
        "x" = "Node {.val {node$id}} has invalid fields."
      ),
      parent = construction,
      class = "dsprrr_artifact_malformed"
    )
  }
  module <- construction
  assign(id, module, envir = cache)
  artifact_restore_common(module, node, registry, trusted)
  module
}

artifact_children_from_refs <- function(
  value,
  artifact,
  cache,
  active,
  registry,
  trusted
) {
  if (is.null(value)) {
    return(NULL)
  }
  if (artifact_is_node_ref(value)) {
    return(artifact_build_node(
      value$.node,
      artifact,
      cache,
      active,
      registry,
      trusted
    ))
  }
  result <- lapply(
    value,
    artifact_children_from_refs,
    artifact = artifact,
    cache = cache,
    active = active,
    registry = registry,
    trusted = trusted
  )
  names(result) <- names(value)
  result
}

artifact_construct_module <- function(node, children, registry, trusted) {
  signature <- artifact_deserialize_signature(node$signature, registry, trusted)
  config <- artifact_restore_value(node$config, registry, trusted)
  fields <- node$fields
  runtime <- function(ref) artifact_decode_runtime_field(ref, registry, trusted)
  runtime_list <- function(refs) {
    result <- lapply(refs %||% list(), runtime)
    names(result) <- names(refs)
    result
  }

  module <- switch(
    node$class,
    ReactModule = ReactModule$new(
      signature = signature,
      tools = runtime_list(fields$tools),
      max_iterations = fields$max_iterations,
      template = fields$template %||% "",
      demos = artifact_restore_value(
        fields$demos %||% list(),
        registry,
        trusted
      ),
      config = config,
      chat = NULL
    ),
    PredictModule = PredictModule$new(
      signature = signature,
      template = fields$template %||% "",
      demos = artifact_restore_value(
        fields$demos %||% list(),
        registry,
        trusted
      ),
      config = config,
      chat = NULL
    ),
    PipelineModule = {
      modules <- children$steps
      specs <- fields$steps %||% vector("list", length(modules))
      if (length(specs) != length(modules)) {
        artifact_abort_node(
          node,
          "Pipeline step metadata does not match its children."
        )
      }
      steps <- lapply(seq_along(modules), function(i) {
        spec <- specs[[i]] %||% list()
        PipelineStep(
          module = modules[[i]],
          input_map = artifact_restore_value(
            spec$input_map %||% list(),
            registry,
            trusted
          ),
          output_select = artifact_restore_value(
            spec$output_select %||% character(),
            registry,
            trusted
          ),
          static_inputs = artifact_restore_value(
            spec$static_inputs %||% list(),
            registry,
            trusted
          )
        )
      })
      names(steps) <- names(modules)
      PipelineModule$new(steps = steps, config = config, chat = NULL)
    },
    EnsembleModule = EnsembleModule$new(
      modules = children$modules,
      reduce_fn = runtime(fields$reduce_fn),
      weights = artifact_restore_value(fields$weights, registry, trusted),
      config = config,
      chat = NULL
    ),
    MultiChainComparisonModule = MultiChainComparisonModule$new(
      signature = signature,
      inner_module = children$inner_module,
      M = fields$M,
      temperature = fields$temperature,
      comparison_template = fields$comparison_template,
      config = config,
      chat = NULL
    ),
    RefineModule = RefineModule$new(
      module = children$module,
      N = fields$N,
      reward_fn = runtime(fields$reward_fn),
      threshold = fields$threshold,
      fail_count = fields$fail_count,
      feedback_template = fields$feedback_template,
      feedback_field = fields$feedback_field,
      config = config,
      chat = NULL
    ),
    BestOfNModule = BestOfNModule$new(
      module = children$module,
      N = fields$N,
      reward_fn = runtime(fields$reward_fn),
      threshold = fields$threshold,
      fail_count = fields$fail_count,
      config = config,
      chat = NULL
    ),
    AssertModule = {
      assertions <- lapply(fields$assertions %||% list(), function(assertion) {
        Assertion(
          condition = runtime(assertion$condition),
          message = assertion$message,
          field = assertion$field,
          type = assertion$type
        )
      })
      AssertModule$new(
        module = children$module,
        assertions = assertions,
        max_retries = fields$max_retries,
        on_failure = fields$on_failure,
        feedback_template = fields$feedback_template,
        config = config,
        chat = NULL
      )
    },
    KNNFewShotModule = {
      result <- KNNFewShotModule$new(
        module = children$module,
        k = fields$k,
        vectorizer = runtime(fields$vectorizer),
        input_text = runtime(fields$input_text),
        train_embeddings = artifact_restore_value(
          fields$train_embeddings,
          registry,
          trusted
        ),
        trainset_demos = artifact_restore_value(
          fields$trainset_demos,
          registry,
          trusted
        ),
        merge_demos = fields$merge_demos,
        config = config,
        chat = NULL
      )
      # The constructor isolates its child for normal compilation. Persistence
      # must instead restore the graph's explicit sharing contract.
      result$module <- children$module
      result$original_demos <- artifact_restore_value(
        fields$original_demos %||% list(),
        registry,
        trusted
      )
      result
    },
    FnModule = FnModule$new(
      signature = signature,
      forward_fn = runtime(fields$forward_fn),
      config = config,
      chat = NULL,
      name = config$name
    ),
    ProgramOfThoughtModule = ProgramOfThoughtModule$new(
      signature = signature,
      runner = runtime(fields$runner),
      max_iters = fields$max_iters,
      extract_answer = fields$extract_answer,
      config = config,
      chat = NULL
    ),
    CodeActModule = CodeActModule$new(
      signature = signature,
      tools = runtime_list(fields$tools),
      runner = runtime(fields$runner),
      max_iterations = fields$max_iterations,
      config = config,
      chat = NULL
    ),
    RLMModule = RLMModule$new(
      signature = signature,
      runner = runtime(fields$runner),
      max_iterations = fields$max_iterations,
      max_llm_calls = fields$max_llm_calls,
      max_output_chars = fields$max_output_chars,
      sub_lm = NULL,
      verbose = fields$verbose,
      tools = runtime_list(fields$tools),
      config = config,
      chat = NULL
    ),
    RAGModule = RAGModule$new(
      signature = signature,
      store = runtime(fields$store),
      retriever = runtime(fields$retriever),
      k = fields$k,
      context_format = fields$context_format,
      config = config,
      chat = NULL
    ),
    artifact_abort_node(
      node,
      paste0("Unsupported module class ", node$class, ".")
    )
  )

  if (length(children) > 0L) {
    exclusions <- new.env(parent = emptyenv())
    exclusions$records <- list()
    derived <- artifact_serialize_signature(
      module$signature,
      registry,
      trusted,
      exclusions,
      paste0("graph.nodes.", node$id, ".derived_signature")
    )
    if (!identical(derived, node$signature)) {
      artifact_abort_node(
        node,
        "Stored signature does not match the child-derived composite signature."
      )
    }
  }
  module$signature <- signature
  module
}

artifact_abort_node <- function(node, message) {
  cli::cli_abort(
    c(
      "Malformed module node in program artifact",
      "x" = "Node {.val {node$id}}: {message}"
    ),
    class = "dsprrr_artifact_malformed"
  )
}

artifact_restore_common <- function(module, node, registry, trusted) {
  module$config <- normalize_module_config(
    artifact_restore_value(node$config, registry, trusted)
  )
  state <- artifact_restore_value(node$state, registry, trusted)
  for (name in names(state)) {
    module$state[[name]] <- state[[name]]
  }
  module$state$compiled <- isTRUE(state$compiled)
  runtime_metadata <- list()
  if (!is.null(node$provider_model)) {
    runtime_metadata$chat <- node$provider_model
  }
  if (identical(node$class, "RLMModule") && !is.null(node$fields$sub_lm)) {
    runtime_metadata$sub_lm <- node$fields$sub_lm
  }
  if (length(runtime_metadata) > 0L) {
    attr(module, "dsprrr_artifact_runtime") <- runtime_metadata
  }
  module$chat <- NULL
  invisible(module)
}

artifact_restore_value <- function(value, registry, trusted) {
  if (is.null(value) || is.atomic(value)) {
    return(value)
  }
  if (is.list(value) && artifact_is_envelope_candidate(value)) {
    return(artifact_restore_envelope(value, registry, trusted))
  }
  if (!is.list(value)) {
    cli::cli_abort(
      "Malformed persisted value",
      class = "dsprrr_artifact_malformed"
    )
  }
  result <- lapply(
    value,
    artifact_restore_value,
    registry = registry,
    trusted = trusted
  )
  names(result) <- names(value)
  if (is.data.frame(value)) {
    class(result) <- class(value)
    attr(result, "row.names") <- attr(value, "row.names")
  }
  result
}

artifact_restore_envelope <- function(value, registry, trusted) {
  artifact_validate_envelope(value, "value")
  envelope <- value$.dsprrr
  switch(
    envelope$kind,
    runtime = artifact_decode_runtime(envelope$payload, registry, trusted),
    content = artifact_restore_content(envelope$payload),
    plain = {
      result <- lapply(
        envelope$payload,
        artifact_restore_value,
        registry = registry,
        trusted = trusted
      )
      names(result) <- names(envelope$payload)
      result
    }
  )
}

artifact_decode_runtime_field <- function(value, registry, trusted) {
  if (is.null(value)) {
    return(NULL)
  }
  artifact_validate_runtime_field(value, "runtime")
  artifact_decode_runtime(value$.dsprrr$payload, registry, trusted)
}

artifact_decode_runtime <- function(ref, registry, trusted) {
  if (is.null(ref)) {
    return(NULL)
  }
  if (!is.list(ref) || !is.character(ref$kind) || length(ref$kind) != 1L) {
    cli::cli_abort(
      "Malformed runtime reference in program artifact",
      class = "dsprrr_artifact_malformed"
    )
  }
  switch(
    ref$kind,
    builtin = artifact_restore_builtin(ref),
    registry = {
      if (
        !is.character(ref$id) ||
          length(ref$id) != 1L ||
          !ref$id %in% names(registry)
      ) {
        cli::cli_abort(
          c(
            "Program artifact requires an unavailable registry value",
            "x" = "Missing registry ID {.val {ref$id %||% '<invalid>'}}."
          ),
          class = "dsprrr_artifact_registry_error"
        )
      }
      value <- registry[[ref$id]]
      interface <- artifact_runtime_interface(value)
      if (
        !is.character(ref$interface_sha256) ||
          length(ref$interface_sha256) != 1L ||
          !identical(ref$interface_sha256, interface)
      ) {
        cli::cli_abort(
          c(
            "Program artifact registry interface does not match",
            "x" = "Registry ID {.val {ref$id}} has a different callable or resource interface."
          ),
          class = "dsprrr_artifact_registry_error"
        )
      }
      value
    },
    trusted = {
      if (!trusted) {
        cli::cli_abort(
          c(
            "Program artifact contains an embedded runtime value",
            "i" = "Restore only a trusted artifact with {.code trusted = TRUE}."
          ),
          class = "dsprrr_artifact_unsafe_value"
        )
      }
      valid <- is.raw(ref$serialized) &&
        is.character(ref$sha256) &&
        length(ref$sha256) == 1L &&
        identical(
          digest::digest(ref$serialized, algo = "sha256", serialize = FALSE),
          ref$sha256
        )
      if (!valid) {
        cli::cli_abort(
          "Embedded runtime value failed its integrity check",
          class = "dsprrr_artifact_integrity_error"
        )
      }
      tryCatch(
        unserialize(ref$serialized),
        error = function(e) {
          cli::cli_abort(
            "Embedded runtime value could not be restored",
            parent = e,
            class = "dsprrr_artifact_malformed"
          )
        }
      )
    },
    cli::cli_abort(
      "Unknown runtime reference kind {.val {ref$kind}}",
      class = "dsprrr_artifact_malformed"
    )
  )
}

artifact_restore_builtin <- function(ref) {
  args <- ref$args %||% list()
  switch(
    ref$id,
    default_reward = default_reward_fn(),
    reduce_majority = do.call(reduce_majority, args),
    reduce_weighted_vote = do.call(reduce_weighted_vote, args),
    reduce_first = reduce_first(),
    cli::cli_abort(
      "Unknown built-in callable {.val {ref$id}}",
      class = "dsprrr_artifact_malformed"
    )
  )
}

artifact_deserialize_signature <- function(signature, registry, trusted) {
  if (!is.list(signature) || !is.list(signature$inputs)) {
    cli::cli_abort(
      "Malformed signature in program artifact",
      class = "dsprrr_artifact_malformed"
    )
  }
  inputs <- lapply(signature$inputs, function(input_record) {
    extra <- artifact_restore_value(
      input_record$extra %||% list(),
      registry,
      trusted
    )
    structure(
      c(
        list(
          name = input_record$name,
          type = artifact_deserialize_type(input_record$type),
          description = input_record$description
        ),
        extra
      ),
      class = "dsprrr_input"
    )
  })
  Signature(
    inputs = inputs,
    output_type = artifact_deserialize_type(signature$output_type),
    instructions = signature$instructions %||% ""
  )
}

artifact_deserialize_type <- function(type) {
  if (!is.list(type) || !is.character(type$kind) || length(type$kind) != 1L) {
    cli::cli_abort(
      "Malformed signature type in program artifact",
      class = "dsprrr_artifact_malformed"
    )
  }
  description <- type$description
  required <- type$required %||% TRUE
  switch(
    type$kind,
    ignore = ellmer::type_ignore(),
    json_schema = do.call(
      get("TypeJsonSchema", envir = asNamespace("ellmer")),
      list(
        json = type$json,
        description = description,
        required = required
      )
    ),
    basic = switch(
      type$type,
      string = ellmer::type_string(description, required),
      number = ellmer::type_number(description, required),
      integer = ellmer::type_integer(description, required),
      boolean = ellmer::type_boolean(description, required),
      cli::cli_abort(
        "Unknown basic signature type {.val {type$type}}",
        class = "dsprrr_artifact_malformed"
      )
    ),
    enum = ellmer::type_enum(type$values, description, required),
    array = ellmer::type_array(
      artifact_deserialize_type(type$items),
      description,
      required
    ),
    object = {
      properties <- lapply(
        type$properties %||% list(),
        artifact_deserialize_type
      )
      do.call(
        get("TypeObject", envir = asNamespace("ellmer")),
        list(
          properties = properties,
          description = description,
          required = required,
          additional_properties = isTRUE(type$additional_properties)
        )
      )
    },
    cli::cli_abort(
      "Unknown signature type kind {.val {type$kind}}",
      class = "dsprrr_artifact_malformed"
    )
  )
}

migrate_program_artifact_v2 <- function(
  config,
  signature,
  registry,
  trusted
) {
  artifact_validate_v2_manifest(config)
  migrated <- tryCatch(
    {
      source <- config
      legacy_n_trials <- nrow(source$state$trials)
      source$state$trials <- tibble::tibble()
      source$state$last_grid <- tibble::tibble()
      source$state$optimization_history <- list()
      if (!is.null(signature)) {
        if (!inherits(signature, "dsprrr::Signature")) {
          cli::cli_abort("{.arg signature} must be a Signature object")
        }
        source$signature <- serialize_signature_v2(signature)
      }
      module <- restore_module_from_v2(source)
      if (legacy_n_trials > 0L) {
        optimizer <- module$config$optimizer %||% list()
        optimizer$n_trials <- legacy_n_trials
        module$config$optimizer <- optimizer
      }
      artifact <- program_artifact(
        module,
        registry = registry,
        trusted = trusted
      )
      artifact$metadata$migrated_from <- 2L
      source_version <- config$metadata$dsprrr_version
      if (
        is.character(source_version) &&
          length(source_version) == 1L &&
          !is.na(source_version)
      ) {
        artifact$metadata$source_dsprrr_version <- source_version
      }
      artifact$integrity <- artifact_integrity(artifact)
      artifact
    },
    error = function(e) e
  )
  if (inherits(migrated, "condition")) {
    cli::cli_abort(
      "Could not migrate v2 module configuration",
      parent = migrated,
      class = "dsprrr_artifact_migration_error"
    )
  }
  migrated
}

artifact_strip_demos <- function(artifact) {
  for (id in names(artifact$graph$nodes)) {
    fields <- artifact$graph$nodes[[id]]$fields
    if ("demos" %in% names(fields)) {
      artifact$graph$nodes[[id]]$fields$demos <- list()
    }
    if ("trainset_demos" %in% names(fields)) {
      artifact$graph$nodes[[id]]$fields$trainset_demos <- list()
    }
    if ("original_demos" %in% names(fields)) {
      artifact$graph$nodes[[id]]$fields$original_demos <- list()
    }
  }
  artifact$integrity <- artifact_integrity(artifact)
  artifact
}

artifact_runtime_kinds <- function(value) {
  kinds <- character()
  visit <- function(item) {
    if (!is.list(item)) {
      return(invisible(NULL))
    }
    if (artifact_is_envelope_candidate(item)) {
      envelope <- item$.dsprrr
      if (identical(envelope$kind, "runtime")) {
        kinds <<- c(kinds, envelope$payload$kind)
      } else if (identical(envelope$kind, "plain")) {
        for (child in envelope$payload) {
          visit(child)
        }
      }
      return(invisible(NULL))
    }
    for (child in item) {
      visit(child)
    }
    invisible(NULL)
  }
  visit(value)
  unique(kinds)
}
