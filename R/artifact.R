#' Save and load complete programs
#'
#' @description
#' `save_program()` writes a program to an `.rds` file and `load_program()`
#' rebuilds it, including nested modules, demonstrations, optimization
#' results and compiled state. `program_artifact()` returns the same content as
#' an R object, a versioned manifest, for example to pin with
#' [pin_module_config()] or to rebuild with [restore_module_config()].
#' `program_artifact_id()` returns a digest that identifies an artifact's
#' content.
#'
#' Chats, credentials, generated prompts, caches and execution history are
#' never saved: give the restored program a chat when you run it.
#'
#' @details
#' ## Functions and other runtime objects
#'
#' Tools, custom functions, retrievers, stores, code runners and interpreter
#' factories are never captured implicitly. List them in a named `registry`:
#' the artifact stores only their names, and loading with the same registry
#' puts them back. An entry is identified by its name and interface digest,
#' not by its function body, so keep registry names stable and versioned.
#' Alternatively, `trusted = TRUE` embeds the objects themselves; they are
#' restored only when `trusted = TRUE` is also passed when loading, so use it
#' only for artifacts and code you trust. Factories are never called while
#' saving or loading.
#'
#' ## Content rules
#'
#' Declarative ellmer content (text, JSON, inline or remote images, and PDFs)
#' is stored as data. Remote URLs must be stable HTTPS URLs without user
#' information, query strings, fragments or signed-path credentials.
#' Demonstration fields with credential-like names are rejected rather than
#' silently dropped. Thinking, tool-call, uploaded and other runtime content
#' needs a registry or trusted embedding.
#'
#' ## Format and identity
#'
#' Format version 6 is the only supported format; artifacts with another
#' version are rejected before any module is built. It records exactly one
#' runner or factory for each code-executing module, the complete Flex runtime
#' contract, and the action and extraction predictors of RLM modules. Cyclic
#' module graphs are rejected. A module shared by several parents is stored
#' once and restored as one object everywhere it is used.
#'
#' The digest detects changes; it is not a sign of authenticity or trust.
#' Registry entries count toward it through their names and interface digests,
#' and records of excluded runtime values count too, although the values do
#' not.
#'
#' @param program A dsprrr module or composed program.
#' @param x A module or a `dsprrr_program_artifact` manifest.
#' @param registry A named list of functions or runtime objects. Artifacts
#'   store the names, never the objects.
#' @param trusted Whether arbitrary runtime values may be embedded or restored
#'   (default `FALSE`). Enable it only for artifacts and code you trust.
#' @param path Path of the `.rds` file, on a local file system, in a directory
#'   that other processes do not change at the same time. `save_program()`
#'   writes a private temporary file in the same directory, checks it, and
#'   moves it into place, so a failed move leaves any existing file unchanged.
#'   If the check after the move fails, `save_program()` errors, but the new
#'   file may already be in place.
#'
#' @return
#' * `program_artifact()` returns a `dsprrr_program_artifact` manifest.
#' * `program_artifact_id()` returns the digest as a string starting with
#'   `"sha256:"`. Artifacts are checked for structure and integrity, without
#'   requiring the recorded dependency versions to be installed. A module
#'   rebuilt from an artifact keeps reporting that artifact's ID until it is
#'   changed, so execution traces point to the exact source; saving it again
#'   creates a new artifact whose ID can differ. Registry entries that a
#'   module's artifact uses stay attached to the module, so later calls and
#'   copies recover the same ID without the registry. A module rebuilt from
#'   trusted embedded values is different: execution metadata may omit its ID
#'   unless you create a new artifact with `program_artifact(module, trusted =
#'   TRUE)`.
#' * `save_program()` returns `path`, invisibly.
#' * `load_program()` returns the rebuilt program.
#'
#' @family persistence
#' @examples
#' mod <- module(signature("text -> answer"))
#'
#' path <- tempfile(fileext = ".rds")
#' save_program(mod, path)
#' restored <- load_program(path)
#' restored
#'
#' artifact <- program_artifact(mod)
#' program_artifact_id(artifact)
#' identical(program_artifact_id(restored), program_artifact_id(artifact))
#' unlink(path)
#'
#' @name program-artifact
NULL

artifact_format_version <- function() 6L

#' @rdname program-artifact
#' @export
program_artifact <- function(program, registry = list(), trusted = FALSE) {
  plan <- artifact_plan_program(
    program,
    registry = registry,
    trusted = trusted
  )
  artifact_finalize_program(artifact_serialize_program_plan(plan))
}

#' @rdname program-artifact
#' @export
program_artifact_id <- function(x, registry = list()) {
  restored_identity <- NULL
  artifact <- if (inherits(x, "dsprrr_program_artifact")) {
    artifact_validate_manifest(x, dependencies = FALSE)
    x
  } else if (inherits(x, "Module")) {
    registry <- artifact_merge_registries(
      artifact_bound_registry(x),
      artifact_validate_registry(registry)
    )
    artifact <- program_artifact(x, registry = registry)
    graph <- module_graph(x, boundaries = "cross", cycles = "record")
    artifact_bind_registry(
      graph$module[!graph$shared],
      artifact,
      registry
    )
    restored_identity <- artifact_restored_identity(x)
    artifact
  } else {
    cli::cli_abort(
      "{.arg x} must be a dsprrr Module or program artifact",
      class = "dsprrr_program_artifact_id_error"
    )
  }

  if (
    !is.null(restored_identity) &&
      identical(
        artifact_manifest_id(artifact),
        restored_identity$baseline_id
      )
  ) {
    return(restored_identity$source_id)
  }

  artifact_manifest_id(artifact)
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
  if (length(registry) > 1L) {
    duplicate <- NULL
    for (index in seq_len(length(registry) - 1L)) {
      matches <- vapply(
        registry[seq.int(index + 1L, length(registry))],
        identical,
        logical(1),
        y = registry[[index]]
      )
      if (any(matches)) {
        duplicate <- c(
          names(registry)[[index]],
          names(registry)[seq.int(index + 1L, length(registry))][which(matches)[
            1L
          ]]
        )
        break
      }
    }
    if (!is.null(duplicate)) {
      cli::cli_abort(
        c(
          "{.arg registry} assigns one runtime value to multiple IDs",
          "x" = "Conflicting aliases: {.val {duplicate}}.",
          "i" = "Use one immutable, versioned ID for each runtime value."
        ),
        class = "dsprrr_artifact_registry_error"
      )
    }
  }
  registry
}

artifact_merge_registries <- function(bound, supplied) {
  bound <- artifact_validate_registry(bound)
  supplied <- artifact_validate_registry(supplied)
  shared <- intersect(names(bound), names(supplied))
  conflicting <- shared[
    !vapply(
      shared,
      function(name) identical(bound[[name]], supplied[[name]]),
      logical(1)
    )
  ]
  if (length(conflicting) > 0L) {
    cli::cli_abort(
      c(
        "{.arg registry} conflicts with program-bound registry IDs",
        "x" = "Conflicting IDs: {.val {conflicting}}."
      ),
      class = "dsprrr_artifact_registry_error"
    )
  }
  artifact_validate_registry(c(
    supplied,
    bound[setdiff(names(bound), names(supplied))]
  ))
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

artifact_manifest_id <- function(artifact) {
  paste0(
    artifact$integrity$algorithm,
    ":",
    artifact$integrity$payload_sha256
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
  allowed <- c("created_at", "r_version", "packages")
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
      provenance = artifact_sanitize_value(
        module$state$optimization_result,
        paste0(node_path, ".optimization.provenance"),
        registry,
        trusted,
        exclusions,
        drop_runtime_names = TRUE
      ),
      best_score = state$best_score,
      best_trial = state$best_trial,
      best_params = state$best_params,
      n_trials = artifact_module_n_trials(module)
    ),
    provider_model = artifact_provider_model(module$chat) %||%
      artifact_detached_runtime(module)$chat,
    fields = fields,
    children = children
  )
}

artifact_module_n_trials <- function(module) {
  result <- module$state$optimization_result
  if (!is.null(result) && is.data.frame(result$trials)) {
    return(as.integer(nrow(result$trials)))
  }
  observed <- if (is.data.frame(module$state$trials)) {
    nrow(module$state$trials)
  } else {
    0L
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

.artifact_module_kinds <- c(
  ReactModule = "react",
  PredictModule = "predict",
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
  FlexModule = "flex"
)

artifact_supported_module_classes <- function() {
  names(.artifact_module_kinds)
}

artifact_module_kind <- function(module) {
  class <- artifact_module_class(module)
  if (identical(class, "PredictModule")) {
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
    return(configured)
  }
  unname(.artifact_module_kinds[[class]])
}

artifact_expected_kinds <- function(class) {
  if (identical(class, "PredictModule")) {
    return(c("predict", "chain_of_thought"))
  }
  if (class %in% names(.artifact_module_kinds)) {
    return(unname(.artifact_module_kinds[[class]]))
  }
  character()
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
  preserved <- artifact_detached_runtime(module)$state_exclusions %||%
    character()
  preserved <- intersect(preserved, artifact_runtime_state_fields())
  for (name in preserved) {
    artifact_record_exclusion(
      exclusions,
      paste0(node_path, ".state.", name),
      "runtime-data"
    )
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
  clean <- function(
    value,
    name,
    drop_runtime_names = FALSE,
    reject_secret_names = FALSE
  ) {
    artifact_sanitize_value(
      value,
      paste0(node_path, ".fields.", name),
      registry,
      trusted,
      exclusions,
      drop_runtime_names = drop_runtime_names,
      reject_secret_names = reject_secret_names
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
      demos = clean(
        module$demos,
        "demos",
        reject_secret_names = TRUE
      ),
      max_iterations = module$max_iterations,
      tools = runtime_list(module$tools, "tools")
    ),
    PredictModule = list(
      template = module$template,
      demos = clean(
        module$demos,
        "demos",
        reject_secret_names = TRUE
      )
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
      trainset_demos = clean(
        module$trainset_demos,
        "trainset_demos",
        reject_secret_names = TRUE
      ),
      merge_demos = module$merge_demos,
      original_demos = clean(
        module$original_demos,
        "original_demos",
        reject_secret_names = TRUE
      )
    ),
    FnModule = list(
      forward_fn = runtime(
        module$.artifact_forward_fn(),
        "forward_fn"
      )
    ),
    ProgramOfThoughtModule = list(
      runner = runtime(module$runner, "runner"),
      interpreter_factory = runtime(
        module$interpreter_factory,
        "interpreter_factory"
      ),
      max_iters = module$max_iters,
      extract_answer = module$extract_answer
    ),
    CodeActModule = list(
      runner = runtime(module$runner, "runner"),
      interpreter_factory = runtime(
        module$interpreter_factory,
        "interpreter_factory"
      ),
      tools = runtime_list(module$tools, "tools"),
      max_iterations = module$max_iterations
    ),
    RLMModule = list(
      runner = runtime(module$runner, "runner"),
      interpreter_factory = runtime(
        module$interpreter_factory,
        "interpreter_factory"
      ),
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
    ),
    FlexModule = list(
      module_src = module$module_src,
      max_predictor_calls = module$max_predictor_calls,
      max_tool_calls = module$max_tool_calls,
      source_format = module$source_format,
      tools = runtime_list(module$tools, "tools"),
      interpreter_factory = runtime(
        module$interpreter_factory,
        "interpreter_factory"
      ),
      require_sandbox = module$require_sandbox
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

  scalar_text <- function(value) {
    value <- tryCatch(as.character(value)[1L], error = function(e) NULL)
    if (
      is.null(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)
    ) {
      return(NULL)
    }
    value
  }

  get_model <- function() {
    scalar_text(tryCatch(chat$get_model(), error = function(e) NULL))
  }

  provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
  provider_props <- if (is.null(provider)) {
    NULL
  } else {
    tryCatch(ellmer_provider_props(provider), error = function(e) NULL)
  }

  if (is.list(provider_props)) {
    provider_class <- scalar_text(class(provider)[1L])
    provider_name <- scalar_text(provider_props$name)
    base_url <- scalar_text(provider_props$base_url)
    model <- get_model()

    # Provider properties also contain credential closures, headers, and
    # account-specific arguments. Persist only this closed, credential-free
    # identity. Unsafe URLs are omitted rather than risking embedded secrets.
    if (!is.null(base_url) && !artifact_is_safe_remote_url(base_url)) {
      base_url <- NULL
    }

    if (!is.null(provider_class) && !is.null(provider_name)) {
      descriptor <- as.character(jsonlite::toJSON(
        list(
          class = provider_class,
          name = provider_name,
          base_url = base_url
        ),
        auto_unbox = TRUE,
        null = "null",
        pretty = FALSE
      ))
      return(list(provider = descriptor, model = model))
    }
  }

  model <- get_model()
  classes <- setdiff(class(chat), c("Chat", "R6"))
  list(
    provider = if (length(classes) > 0L) classes[[1]] else class(chat)[1],
    model = model
  )
}

artifact_detached_runtime <- function(module) {
  attr(module, "dsprrr_artifact_runtime", exact = TRUE) %||% list()
}

artifact_restored_identity <- function(module) {
  identity <- artifact_detached_runtime(module)$restored_identity
  valid <- is.list(identity) &&
    identical(names(identity), c("source_id", "baseline_id")) &&
    is.character(identity$source_id) &&
    length(identity$source_id) == 1L &&
    !is.na(identity$source_id) &&
    grepl("^sha256:[0-9a-f]{64}$", identity$source_id) &&
    is.character(identity$baseline_id) &&
    length(identity$baseline_id) == 1L &&
    !is.na(identity$baseline_id) &&
    grepl("^sha256:[0-9a-f]{64}$", identity$baseline_id)
  if (!valid) {
    return(NULL)
  }
  identity
}

artifact_bind_restored_identity <- function(program, source_artifact) {
  # Trusted runtimes cannot be reidentified without a new explicit trust
  # decision. Manifest validation has already established the current format.
  if (artifact_has_trusted_runtime(source_artifact$graph$nodes)) {
    return(invisible(program))
  }

  baseline <- program_artifact(program)
  runtime <- artifact_detached_runtime(program)
  runtime$restored_identity <- list(
    source_id = artifact_manifest_id(source_artifact),
    baseline_id = artifact_manifest_id(baseline)
  )
  attr(program, "dsprrr_artifact_runtime") <- runtime
  invisible(program)
}

artifact_copy_runtime <- function(source, target) {
  source_graph <- module_graph(
    source,
    boundaries = "cross",
    cycles = "record"
  )
  source_modules <- stats::setNames(source_graph$module, source_graph$path)
  source_runtime <- lapply(source_modules, function(module) {
    attr(module, "dsprrr_artifact_runtime", exact = TRUE)
  })
  has_runtime <- !vapply(source_runtime, is.null, logical(1))
  if (!any(has_runtime)) {
    return(target)
  }

  target_graph <- module_graph(
    target,
    boundaries = "cross",
    cycles = "record"
  )
  target_modules <- stats::setNames(target_graph$module, target_graph$path)

  runtime_paths <- names(source_runtime)[has_runtime]
  for (path in intersect(runtime_paths, names(target_modules))) {
    attr(target_modules[[path]], "dsprrr_artifact_runtime") <-
      source_runtime[[path]]
  }
  target
}

artifact_bound_registry <- function(module) {
  graph <- module_graph(module, boundaries = "cross", cycles = "record")
  modules <- graph$module[!graph$shared]
  bound <- list()
  for (item in modules) {
    bound <- artifact_merge_registries(
      bound,
      artifact_detached_runtime(item)$registry %||% list()
    )
  }
  bound
}

artifact_runtime_state_fields <- function() {
  c(
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
}

artifact_walk_runtime_envelopes <- function(
  value,
  visitor,
  stop_when = NULL
) {
  visit <- function(item) {
    if ((is.function(stop_when) && isTRUE(stop_when())) || !is.list(item)) {
      return(invisible(NULL))
    }
    if (artifact_is_envelope_candidate(item)) {
      envelope <- item$.dsprrr
      if (identical(envelope$kind, "runtime")) {
        visitor(envelope)
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
  invisible(NULL)
}

artifact_registry_ids <- function(value) {
  ids <- character()
  artifact_walk_runtime_envelopes(value, function(envelope) {
    if (identical(envelope$payload$kind, "registry")) {
      ids <<- c(ids, envelope$payload$id)
    }
  })
  unique(ids)
}

artifact_has_trusted_runtime <- function(value) {
  found <- FALSE
  artifact_walk_runtime_envelopes(
    value,
    function(envelope) {
      if (identical(envelope$payload$kind, "trusted")) {
        found <<- TRUE
      }
    },
    stop_when = function() found
  )
  found
}

artifact_bind_registry <- function(modules, artifact, registry) {
  ids <- artifact_registry_ids(artifact$graph$nodes)
  bound <- registry[intersect(names(registry), ids)]
  if (length(bound) == 0L) {
    return(invisible(modules))
  }
  for (module in modules) {
    runtime <- artifact_detached_runtime(module)
    runtime$registry <- artifact_merge_registries(
      runtime$registry %||% list(),
      bound
    )
    attr(module, "dsprrr_artifact_runtime") <- runtime
  }
  invisible(modules)
}

restore_program_artifact <- function(
  artifact,
  registry = list(),
  trusted = FALSE
) {
  admission <- artifact_admit_restore(
    artifact,
    registry = registry,
    trusted = trusted
  )
  execution <- artifact_execute_restore(admission)
  artifact_finalize_restore(execution, admission)
}

artifact_bind_state_exclusions <- function(modules, artifact) {
  records <- artifact$exclusions %||% list()
  for (id in intersect(names(modules), names(artifact$graph$nodes))) {
    prefix <- paste0("graph.nodes.", id, ".state.")
    fields <- vapply(
      records,
      function(record) {
        path <- record$path %||% ""
        if (
          identical(record$reason %||% NULL, "runtime-data") &&
            startsWith(path, prefix)
        ) {
          substring(path, nchar(prefix) + 1L)
        } else {
          ""
        }
      },
      character(1)
    )
    fields <- intersect(fields[nzchar(fields)], artifact_runtime_state_fields())
    if (length(fields) > 0L) {
      runtime <- artifact_detached_runtime(modules[[id]])
      runtime$state_exclusions <- fields
      attr(modules[[id]], "dsprrr_artifact_runtime") <- runtime
    }
  }
  invisible(modules)
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
      rlang::cnd_signal(construction)
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
      interpreter_factory = runtime(fields$interpreter_factory),
      max_iters = fields$max_iters,
      extract_answer = fields$extract_answer,
      config = config,
      chat = NULL
    ),
    CodeActModule = CodeActModule$new(
      signature = signature,
      tools = runtime_list(fields$tools),
      runner = runtime(fields$runner),
      interpreter_factory = runtime(fields$interpreter_factory),
      max_iterations = fields$max_iterations,
      config = config,
      chat = NULL
    ),
    RLMModule = RLMModule$new(
      signature = signature,
      reasoning_mode = config$reasoning_mode %||%
        c("rationale", "native", "plan"),
      runner = runtime(fields$runner),
      interpreter_factory = runtime(fields$interpreter_factory),
      max_iterations = fields$max_iterations,
      max_llm_calls = fields$max_llm_calls,
      max_output_chars = fields$max_output_chars,
      sub_lm = NULL,
      verbose = fields$verbose,
      tools = runtime_list(fields$tools),
      config = config,
      chat = NULL,
      generate_action = children$generate_action,
      extract = children$extract
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
    FlexModule = FlexModule$new(
      signature = signature,
      module_src = fields$module_src,
      tools = runtime_list(fields$tools),
      interpreter_factory = runtime(fields$interpreter_factory),
      source_format = fields$source_format,
      max_predictor_calls = fields$max_predictor_calls,
      max_tool_calls = fields$max_tool_calls,
      require_sandbox = fields$require_sandbox,
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
  result <- artifact_restore_value(
    node$optimization$provenance,
    registry,
    trusted
  )
  if (!is.null(result)) {
    set_optimization_result(module, result)
  }
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
      ellmer::TypeJsonSchema,
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
        ellmer::TypeObject,
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
  artifact_walk_runtime_envelopes(value, function(envelope) {
    kinds <<- c(kinds, envelope$payload$kind)
  })
  unique(kinds)
}
