# Optimizer checkpoint infrastructure
#
# Checkpoints are private, versioned, closed manifests. They contain only
# deterministic hashes for input identities and a safe program artifact for the
# best partial program. Runtime providers, credentials, and opaque state are
# never serialized implicitly.

optimizer_checkpoint_format_version <- function() 1L

optimizer_checkpoint_enabled <- function(control) {
  !is.null(control) && !is.null(control@checkpoint_path)
}

optimizer_checkpoint_package_version <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("dsprrr")),
    error = function(e) NULL
  )
  if (
    !is.character(version) ||
      length(version) != 1L ||
      is.na(version) ||
      !nzchar(version)
  ) {
    cli::cli_abort(
      "Could not determine the dsprrr package version for a checkpoint",
      class = "dsprrr_optimizer_checkpoint_fingerprint_error"
    )
  }
  version
}

optimizer_checkpoint_hash <- function(value) {
  digest::digest(value, algo = "sha256", serialize = TRUE)
}

optimizer_checkpoint_is_secret_name <- function(name) {
  artifact_is_secret_name(name)
}

optimizer_checkpoint_sanitize <- function(value, path = "value") {
  if (is.null(value)) {
    return(NULL)
  }
  if (inherits(value, "dsprrr_optimizer_stop_reason")) {
    value <- unclass(value)
  }
  if (
    is.function(value) ||
      is.environment(value) ||
      is.language(value) ||
      is.expression(value) ||
      typeof(value) %in% c("externalptr", "weakref")
  ) {
    cli::cli_abort(
      c(
        "Optimizer checkpoint contains opaque runtime state",
        "x" = "{.field {path}} has class {.cls {class(value)[1]}}.",
        "i" = "Store a deterministic ID or plain search state instead."
      ),
      class = "dsprrr_optimizer_checkpoint_unsafe_value"
    )
  }

  if (is.atomic(value)) {
    attributes <- attributes(value)
    allowed <- c("names", "class", "levels", "row.names", "tzone")
    unknown <- setdiff(names(attributes), allowed)
    if (length(unknown) > 0L) {
      cli::cli_abort(
        c(
          "Optimizer checkpoint value has unsupported attributes",
          "x" = "{.field {path}} has: {paste(unknown, collapse = ', ')}."
        ),
        class = "dsprrr_optimizer_checkpoint_unsafe_value"
      )
    }
    value_names <- names(value)
    if (
      !is.null(value_names) &&
        any(vapply(
          value_names,
          optimizer_checkpoint_is_secret_name,
          logical(1)
        ))
    ) {
      cli::cli_abort(
        "Optimizer checkpoint field {.field {path}} contains a credential-like name",
        class = "dsprrr_optimizer_checkpoint_unsafe_value"
      )
    }
    return(value)
  }

  if (!is.list(value)) {
    cli::cli_abort(
      "Optimizer checkpoint cannot store {.cls {class(value)[1]}} at {.field {path}}",
      class = "dsprrr_optimizer_checkpoint_unsafe_value"
    )
  }

  is_data_frame <- is.data.frame(value)
  allowed_classes <- c("data.frame", "tbl_df", "tbl")
  value_classes <- class(value)
  if (
    (!is_data_frame && !is.null(attr(value, "class"))) ||
      (is_data_frame && length(setdiff(value_classes, allowed_classes)) > 0L)
  ) {
    cli::cli_abort(
      "Optimizer checkpoint cannot store classed list {.field {path}}",
      class = "dsprrr_optimizer_checkpoint_unsafe_value"
    )
  }
  attribute_names <- names(attributes(value))
  allowed_attributes <- if (is_data_frame) {
    c("names", "class", "row.names")
  } else {
    "names"
  }
  unknown_attributes <- setdiff(attribute_names, allowed_attributes)
  if (length(unknown_attributes) > 0L) {
    cli::cli_abort(
      "Optimizer checkpoint list {.field {path}} has unsupported attributes",
      class = "dsprrr_optimizer_checkpoint_unsafe_value"
    )
  }

  item_names <- names(value)
  if (!is.null(item_names)) {
    secret <- vapply(
      item_names,
      optimizer_checkpoint_is_secret_name,
      logical(1)
    )
    if (any(secret)) {
      cli::cli_abort(
        c(
          "Optimizer checkpoint configuration contains a credential-like field",
          "x" = "{.field {paste0(path, '.', item_names[which(secret)[1L]])}} cannot be persisted."
        ),
        class = "dsprrr_optimizer_checkpoint_unsafe_value"
      )
    }
  }

  sanitized <- lapply(seq_along(value), function(i) {
    name <- if (!is.null(item_names) && nzchar(item_names[[i]])) {
      item_names[[i]]
    } else {
      as.character(i)
    }
    optimizer_checkpoint_sanitize(value[[i]], paste0(path, ".", name))
  })
  names(sanitized) <- item_names
  if (is_data_frame) {
    class(sanitized) <- value_classes
    attr(sanitized, "row.names") <- attr(value, "row.names")
  }
  sanitized
}

optimizer_checkpoint_fingerprint_value <- function(value, path) {
  sanitized <- optimizer_checkpoint_sanitize(value, path)
  optimizer_checkpoint_hash(sanitized)
}

optimizer_checkpoint_binding_environment <- function(name, environment) {
  current <- environment
  while (!identical(current, emptyenv())) {
    if (exists(name, envir = current, inherits = FALSE)) {
      return(current)
    }
    current <- parent.env(current)
  }
  NULL
}

optimizer_checkpoint_function_formals <- function(fn) {
  fmls <- formals(fn)
  result <- lapply(fmls, function(value) {
    paste(deparse(value, width.cutoff = 500L), collapse = "\n")
  })
  names(result) <- names(fmls)
  result
}

optimizer_checkpoint_metric_descriptor <- function(
  metric,
  registry = list(),
  path = "metric",
  seen = list()
) {
  if (!is.function(metric)) {
    cli::cli_abort(
      "{.arg metric} must be a function",
      class = "dsprrr_optimizer_checkpoint_fingerprint_error"
    )
  }
  registry_id <- artifact_registry_find(metric, registry)
  if (!is.null(registry_id)) {
    return(list(
      kind = "registry",
      id = registry_id,
      interface_sha256 = artifact_runtime_interface(metric)
    ))
  }
  if (length(seen) >= 20L || any(vapply(seen, identical, logical(1), metric))) {
    cli::cli_abort(
      c(
        "Metric dependency graph is cyclic or too deep",
        "i" = "Register the metric in {.arg checkpoint_registry} with a stable ID."
      ),
      class = "dsprrr_optimizer_checkpoint_fingerprint_error"
    )
  }
  seen <- c(seen, list(metric))

  environment <- environment(metric)
  if (is.null(environment)) {
    environment <- emptyenv()
  }
  globals <- tryCatch(
    codetools::findGlobals(metric, merge = FALSE),
    error = function(e) {
      cli::cli_abort(
        "Could not inspect metric dependencies",
        parent = e,
        class = "dsprrr_optimizer_checkpoint_fingerprint_error"
      )
    }
  )

  describe_binding <- function(name, kind) {
    binding_environment <- optimizer_checkpoint_binding_environment(
      name,
      environment
    )
    if (is.null(binding_environment)) {
      return(list(name = name, kind = "unresolved"))
    }
    if (bindingIsActive(name, binding_environment)) {
      cli::cli_abort(
        c(
          "Metric uses active or stateful binding {.field {name}}",
          "i" = "Register the metric in {.arg checkpoint_registry} with a stable ID."
        ),
        class = "dsprrr_optimizer_checkpoint_fingerprint_error"
      )
    }
    value <- get(name, envir = binding_environment, inherits = FALSE)
    namespace <- if (isNamespace(binding_environment)) {
      getNamespaceName(binding_environment)
    } else if (identical(binding_environment, baseenv())) {
      "base"
    } else {
      NULL
    }

    if (is.function(value)) {
      if (!is.null(namespace) && namespace == "base" && is.primitive(value)) {
        return(list(name = name, kind = "base_primitive"))
      }
      if (!is.null(namespace)) {
        return(list(
          name = name,
          kind = "namespace_function",
          namespace = namespace,
          package_version = tryCatch(
            as.character(utils::packageVersion(namespace)),
            error = function(e) NA_character_
          ),
          formals = optimizer_checkpoint_function_formals(value),
          body = paste(
            deparse(body(value), width.cutoff = 500L),
            collapse = "\n"
          )
        ))
      }
      return(list(
        name = name,
        kind = "function",
        descriptor = optimizer_checkpoint_metric_descriptor(
          value,
          registry = registry,
          path = paste0(path, ".", name),
          seen = seen
        )
      ))
    }

    if (
      identical(binding_environment, globalenv()) ||
        (!is.null(namespace) && kind == "variable")
    ) {
      cli::cli_abort(
        c(
          "Metric depends on opaque state {.field {name}}",
          "i" = "Capture a plain immutable value in the metric closure, or register the metric with a stable ID."
        ),
        class = "dsprrr_optimizer_checkpoint_fingerprint_error"
      )
    }

    list(
      name = name,
      kind = "captured_value",
      sha256 = optimizer_checkpoint_fingerprint_value(
        value,
        paste0(path, ".", name)
      )
    )
  }

  variables <- setdiff(
    globals$variables %||% character(),
    names(formals(metric))
  )
  functions <- setdiff(globals$functions %||% character(), c("{", "(", "<-"))
  captures <- lapply(variables, describe_binding, kind = "variable")
  dependencies <- lapply(functions, describe_binding, kind = "function")
  names(captures) <- variables
  names(dependencies) <- functions

  attributes <- attributes(metric)
  attributes$srcref <- NULL
  attributes <- optimizer_checkpoint_sanitize(
    attributes,
    paste0(path, ".attributes")
  )

  list(
    kind = "closure",
    class = unname(class(metric)),
    formals = optimizer_checkpoint_function_formals(metric),
    body = paste(deparse(body(metric), width.cutoff = 500L), collapse = "\n"),
    captures = captures,
    dependencies = dependencies,
    attributes = attributes
  )
}

optimizer_checkpoint_metric_fingerprint <- function(metric, registry = list()) {
  descriptor <- tryCatch(
    optimizer_checkpoint_metric_descriptor(metric, registry = registry),
    error = function(e) {
      if (inherits(e, "dsprrr_optimizer_checkpoint_fingerprint_error")) {
        stop(e)
      }
      cli::cli_abort(
        c(
          "Metric cannot be fingerprinted safely",
          "x" = conditionMessage(e),
          "i" = "Register the metric in {.arg checkpoint_registry} with a stable ID."
        ),
        parent = e,
        class = "dsprrr_optimizer_checkpoint_fingerprint_error"
      )
    }
  )
  optimizer_checkpoint_hash(descriptor)
}

optimizer_checkpoint_runtime_identity <- function(
  value,
  registry = list(),
  path = "runtime"
) {
  registry_id <- artifact_registry_find(value, registry)
  if (!is.null(registry_id)) {
    return(list(
      kind = "registry",
      id_sha256 = optimizer_checkpoint_hash(registry_id),
      interface_sha256 = artifact_runtime_interface(value)
    ))
  }
  if (inherits(value, "Chat")) {
    get_provider <- tryCatch(value$get_provider, error = function(e) NULL)
    provider <- if (is.function(get_provider)) {
      tryCatch(get_provider(), error = function(e) NULL)
    } else {
      NULL
    }
    provider_props <- if (!is.null(provider)) {
      tryCatch(S7::props(provider), error = function(e) NULL)
    } else {
      NULL
    }
    if (!is.null(provider_props)) {
      scalar_text <- function(value) {
        is.character(value) &&
          length(value) == 1L &&
          !is.na(value) &&
          nzchar(value)
      }
      provider_class <- class(provider)[[1L]] %||% NULL
      provider_name <- provider_props$name %||% NULL
      base_url <- provider_props$base_url %||% NULL
      model <- provider_props$model %||%
        tryCatch(value$get_model(), error = function(e) NULL)
      if (
        scalar_text(provider_class) &&
          scalar_text(provider_name) &&
          scalar_text(base_url) &&
          scalar_text(model)
      ) {
        return(list(
          kind = "provider_model",
          provider_class_sha256 = optimizer_checkpoint_hash(provider_class),
          provider_name_sha256 = optimizer_checkpoint_hash(provider_name),
          base_url_sha256 = optimizer_checkpoint_hash(base_url),
          model_sha256 = optimizer_checkpoint_hash(model)
        ))
      }
    }
  }
  cli::cli_abort(
    c(
      "Optimizer runtime identity is opaque",
      "x" = "{.field {path}} cannot be identified deterministically.",
      "i" = "Register it in {.arg checkpoint_registry} with a stable ID."
    ),
    class = "dsprrr_optimizer_checkpoint_fingerprint_error"
  )
}

optimizer_checkpoint_effective_runtime_identity <- function(
  program,
  .llm = NULL,
  registry = list(),
  path = "effective_runtime"
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort(
      "{.field {path}} requires a Module program",
      class = "dsprrr_optimizer_checkpoint_fingerprint_error"
    )
  }
  if (!is.null(.llm)) {
    return(list(
      kind = "override",
      runtime = optimizer_checkpoint_runtime_identity(
        .llm,
        registry,
        paste0(path, ".override")
      )
    ))
  }

  graph <- module_graph(program, boundaries = "cross", cycles = "record")
  graph <- graph[!graph$cycle, , drop = FALSE]
  leaf <- vapply(
    graph$path,
    function(node_path) {
      prefix <- if (identical(node_path, "$")) "$/" else paste0(node_path, "/")
      !any(startsWith(graph$path, prefix))
    },
    logical(1)
  )
  graph <- graph[leaf, , drop = FALSE]
  default_chat <- get_default_chat(create = FALSE)
  nodes <- lapply(seq_len(nrow(graph)), function(i) {
    module_chat <- tryCatch(graph$module[[i]]$chat, error = function(e) NULL)
    effective <- module_chat %||% default_chat
    if (is.null(effective)) {
      cli::cli_abort(
        c(
          "No stable effective Chat is available for optimizer checkpointing",
          "x" = "{.field {paste0(path, '.', graph$path[[i]])}} has no attached or configured default Chat.",
          "i" = "Attach a Chat, pass {.arg .llm}, configure a default Chat, or register the runtime in {.arg checkpoint_registry}."
        ),
        class = "dsprrr_optimizer_checkpoint_fingerprint_error"
      )
    }
    list(
      path = graph$path[[i]],
      runtime = optimizer_checkpoint_runtime_identity(
        effective,
        registry,
        paste0(path, ".", graph$path[[i]])
      )
    )
  })
  names(nodes) <- graph$path
  list(kind = "program", nodes = nodes)
}

optimizer_checkpoint_strip_stop_reasons <- function(value) {
  if (inherits(value, "dsprrr_optimizer_stop_reason")) {
    return(lapply(unclass(value), optimizer_checkpoint_strip_stop_reasons))
  }
  if (
    is.list(value) &&
      !inherits(value, "Module") &&
      (is.null(attr(value, "class")) || is.data.frame(value))
  ) {
    for (i in seq_along(value)) {
      value[i] <- list(optimizer_checkpoint_strip_stop_reasons(value[[i]]))
    }
  }
  value
}

optimizer_checkpoint_program_copy <- function(program) {
  copy <- copy_module(program)
  graph <- module_graph(copy, boundaries = "cross", cycles = "record")
  canonical <- graph[!graph$shared & !graph$cycle, , drop = FALSE]
  for (module in canonical$module) {
    module$config <- optimizer_checkpoint_strip_stop_reasons(module$config)
    module$state <- optimizer_checkpoint_strip_stop_reasons(module$state)
  }
  copy
}

optimizer_checkpoint_program_artifact <- function(program, registry = list()) {
  tryCatch(
    program_artifact(
      optimizer_checkpoint_program_copy(program),
      registry = registry,
      trusted = FALSE
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Program cannot be checkpointed safely",
          "x" = conditionMessage(e),
          "i" = "Register required runtime values in {.arg checkpoint_registry}; trusted embedding is intentionally disabled."
        ),
        parent = e,
        class = "dsprrr_optimizer_checkpoint_fingerprint_error"
      )
    }
  )
}

optimizer_checkpoint_fingerprints <- function(
  program,
  data,
  metric,
  registry = list()
) {
  artifact <- optimizer_checkpoint_program_artifact(program, registry)
  list(
    program = artifact$integrity$payload_sha256,
    data = optimizer_checkpoint_fingerprint_value(data, "data"),
    metric = optimizer_checkpoint_metric_fingerprint(metric, registry)
  )
}

optimizer_checkpoint_integrity <- function(checkpoint) {
  payload <- checkpoint[c(
    "format",
    "format_version",
    "metadata",
    "optimizer",
    "compatibility",
    "budget",
    "rng",
    "progress",
    "best_program"
  )]
  list(
    algorithm = "sha256",
    payload_sha256 = optimizer_checkpoint_hash(payload)
  )
}

optimizer_checkpoint_is_sha256 <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    grepl("^[0-9a-f]{64}$", value)
}

optimizer_checkpoint_validate_manifest <- function(checkpoint) {
  malformed <- function(message) {
    cli::cli_abort(
      c("Malformed dsprrr optimizer checkpoint", "x" = message),
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  require_closed_value <- function(value, path) {
    sanitized <- optimizer_checkpoint_sanitize(value, path)
    if (!identical(sanitized, value)) {
      malformed(paste0(path, " is not in canonical closed form."))
    }
    invisible(value)
  }
  if (
    !is.list(checkpoint) ||
      !identical(
        class(checkpoint),
        c("dsprrr_optimizer_checkpoint", "list")
      )
  ) {
    malformed("Checkpoint must be a closed checkpoint list.")
  }
  expected_names <- c(
    "format",
    "format_version",
    "metadata",
    "optimizer",
    "compatibility",
    "budget",
    "rng",
    "progress",
    "best_program",
    "integrity"
  )
  if (!artifact_names_match(names(checkpoint), expected_names)) {
    malformed("Checkpoint has unknown or missing top-level fields.")
  }
  if (!identical(checkpoint$format, "dsprrr-optimizer-checkpoint")) {
    malformed("Checkpoint format marker is invalid.")
  }
  if (
    !identical(checkpoint$format_version, optimizer_checkpoint_format_version())
  ) {
    cli::cli_abort(
      c(
        "Unsupported optimizer checkpoint version",
        "x" = "Got {.val {checkpoint$format_version}}; expected {.val {optimizer_checkpoint_format_version()}}."
      ),
      class = "dsprrr_optimizer_checkpoint_unsupported_version"
    )
  }

  metadata_names <- c(
    "created_at",
    "written_at",
    "dsprrr_version",
    "artifact_version",
    "r_version"
  )
  if (
    !artifact_is_plain_list(checkpoint$metadata) ||
      !artifact_names_match(names(checkpoint$metadata), metadata_names)
  ) {
    malformed("Checkpoint metadata is invalid.")
  }
  require_closed_value(checkpoint$metadata, "metadata")
  timestamp_is_canonical <- function(value) {
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !grepl(
          "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
          value
        )
    ) {
      return(FALSE)
    }
    parsed <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    !is.na(parsed) &&
      identical(format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), value)
  }
  version_is_canonical <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      grepl("^[0-9]+([.-][0-9A-Za-z]+)*$", value)
  }
  if (
    !timestamp_is_canonical(checkpoint$metadata$created_at) ||
      !timestamp_is_canonical(checkpoint$metadata$written_at) ||
      !version_is_canonical(checkpoint$metadata$dsprrr_version) ||
      !is.integer(checkpoint$metadata$artifact_version) ||
      length(checkpoint$metadata$artifact_version) != 1L ||
      is.na(checkpoint$metadata$artifact_version) ||
      checkpoint$metadata$artifact_version < 1L ||
      !version_is_canonical(checkpoint$metadata$r_version)
  ) {
    malformed("Checkpoint metadata fields are not canonical scalars.")
  }
  if (
    !artifact_is_plain_list(checkpoint$optimizer) ||
      !artifact_names_match(
        names(checkpoint$optimizer),
        c("name", "version")
      ) ||
      !is.character(checkpoint$optimizer$name) ||
      length(checkpoint$optimizer$name) != 1L ||
      !nzchar(checkpoint$optimizer$name) ||
      !is.integer(checkpoint$optimizer$version) ||
      length(checkpoint$optimizer$version) != 1L ||
      is.na(checkpoint$optimizer$version) ||
      checkpoint$optimizer$version < 1L
  ) {
    malformed("Checkpoint optimizer identity is invalid.")
  }
  if (
    !artifact_is_plain_list(checkpoint$compatibility) ||
      !artifact_names_match(
        names(checkpoint$compatibility),
        c("config", "fingerprints")
      )
  ) {
    malformed("Checkpoint compatibility record is invalid.")
  }
  fingerprints <- checkpoint$compatibility$fingerprints
  if (
    !artifact_is_plain_list(fingerprints) ||
      !artifact_names_match(
        names(fingerprints),
        c("program", "data", "metric")
      ) ||
      !all(vapply(fingerprints, optimizer_checkpoint_is_sha256, logical(1)))
  ) {
    malformed("Checkpoint fingerprints are invalid.")
  }

  require_closed_value(
    checkpoint$compatibility$config,
    "compatibility.config"
  )
  require_closed_value(checkpoint$budget, "budget")
  validator_budget <- new_optimizer_budget()
  optimizer_budget_restore_state(validator_budget, checkpoint$budget)

  if (
    !is.null(checkpoint$rng) &&
      (!is.integer(checkpoint$rng) ||
        length(checkpoint$rng) == 0L ||
        anyNA(checkpoint$rng))
  ) {
    malformed("Checkpoint RNG state is invalid.")
  }
  if (
    !artifact_is_plain_list(checkpoint$progress) ||
      !artifact_names_match(
        names(checkpoint$progress),
        c("phase", "search_state", "lineage")
      ) ||
      !is.character(checkpoint$progress$phase) ||
      length(checkpoint$progress$phase) != 1L ||
      !nzchar(checkpoint$progress$phase)
  ) {
    malformed("Checkpoint progress state is invalid.")
  }
  require_closed_value(
    checkpoint$progress$search_state,
    "progress.search_state"
  )
  require_closed_value(
    checkpoint$progress$lineage,
    "progress.lineage"
  )
  if (!is.null(checkpoint$best_program)) {
    artifact_validate_manifest(checkpoint$best_program)
  }
  integrity <- checkpoint$integrity
  if (
    !artifact_is_plain_list(integrity) ||
      !artifact_names_match(
        names(integrity),
        c("algorithm", "payload_sha256")
      ) ||
      !identical(integrity$algorithm, "sha256") ||
      !optimizer_checkpoint_is_sha256(integrity$payload_sha256)
  ) {
    malformed("Checkpoint integrity record is invalid.")
  }
  expected <- optimizer_checkpoint_integrity(checkpoint)$payload_sha256
  if (!identical(expected, integrity$payload_sha256)) {
    cli::cli_abort(
      "Optimizer checkpoint integrity check failed",
      class = "dsprrr_optimizer_checkpoint_integrity_error"
    )
  }
  invisible(checkpoint)
}

optimizer_checkpoint_trust_abort <- function(message, parent = NULL) {
  cli::cli_abort(
    c(
      "Optimizer checkpoint path trust verification failed",
      "x" = message
    ),
    parent = parent,
    class = c(
      "dsprrr_optimizer_checkpoint_trust_error",
      "dsprrr_optimizer_checkpoint_io_error"
    )
  )
}

optimizer_checkpoint_lock_path <- function(path) {
  path <- artifact_validate_path(path)
  absolute <- tryCatch(
    as.character(fs::path_abs(path.expand(path))),
    error = function(e) NULL
  )
  if (is.null(absolute) || !dir.exists(dirname(absolute))) {
    optimizer_checkpoint_trust_abort(
      "the checkpoint lock directory cannot be resolved"
    )
  }
  parent <- tryCatch(
    as.character(fs::path_real(dirname(absolute))),
    error = function(e) NULL
  )
  if (is.null(parent)) {
    optimizer_checkpoint_trust_abort(
      "the checkpoint lock directory cannot be resolved canonically"
    )
  }
  file.path(parent, paste0(".", basename(absolute), ".lock"))
}

optimizer_checkpoint_with_lock <- function(path, code) {
  lock_path <- optimizer_checkpoint_lock_path(path)
  timeout <- getOption("dsprrr.optimizer_checkpoint_lock_timeout", 10)
  if (
    !is.numeric(timeout) ||
      length(timeout) != 1L ||
      is.na(timeout) ||
      timeout < 0
  ) {
    timeout <- 10
  }

  old_umask <- Sys.umask("0077")
  on.exit(Sys.umask(old_umask), add = TRUE)
  lock <- tryCatch(
    filelock::lock(lock_path, timeout = timeout * 1000),
    error = function(e) e
  )
  Sys.umask(old_umask)
  if (inherits(lock, "condition")) {
    optimizer_checkpoint_trust_abort(
      paste0(
        "the checkpoint lock could not be acquired: ",
        conditionMessage(lock)
      ),
      parent = lock
    )
  }
  if (is.null(lock)) {
    cli::cli_abort(
      c(
        "Timed out waiting for the optimizer checkpoint lock",
        "x" = "Lock: {.path {lock_path}}"
      ),
      class = c(
        "dsprrr_optimizer_checkpoint_lock_error",
        "dsprrr_optimizer_checkpoint_io_error"
      )
    )
  }
  on.exit(filelock::unlock(lock), add = TRUE)

  guard <- optimizer_checkpoint_path_guard(path)
  if (!identical(dirname(guard$path), dirname(lock_path))) {
    optimizer_checkpoint_trust_abort(
      "the checkpoint directory changed while its lock was acquired"
    )
  }
  if (cache_private_modes_supported()) {
    if (
      cache_path_is_symlink(lock_path) ||
        !cache_path_is_regular(lock_path) ||
        !cache_paths_owned_by_effective_user(lock_path) ||
        !cache_set_private_mode(lock_path, "0600")
    ) {
      optimizer_checkpoint_trust_abort(
        "the checkpoint lock is not an effective-user-owned 0600 file"
      )
    }
    lock_identity <- cache_private_file_identity(lock_path, guard$parent)
    if (!isTRUE(lock_identity$ok)) {
      optimizer_checkpoint_trust_abort(lock_identity$reason)
    }
  } else if (
    cache_path_is_symlink(lock_path) ||
      !cache_path_is_regular(lock_path)
  ) {
    optimizer_checkpoint_trust_abort(
      "the checkpoint lock is not a regular file"
    )
  }

  hook <- getOption("dsprrr.optimizer_checkpoint_lock_hook")
  if (is.function(hook)) {
    hook()
  }
  code(guard)
}

optimizer_checkpoint_path_guard <- function(path, require_existing = FALSE) {
  path <- artifact_validate_path(path)
  absolute <- tryCatch(
    as.character(fs::path_abs(path.expand(path))),
    error = function(e) NULL
  )
  if (is.null(absolute)) {
    optimizer_checkpoint_trust_abort(
      "the checkpoint path cannot be resolved"
    )
  }
  if (cache_path_is_symlink(absolute)) {
    optimizer_checkpoint_trust_abort(
      "the checkpoint target is a symbolic link"
    )
  }
  parent <- dirname(absolute)
  if (!dir.exists(parent)) {
    optimizer_checkpoint_trust_abort(
      paste0("the checkpoint directory does not exist: ", parent)
    )
  }
  canonical_parent <- tryCatch(
    as.character(fs::path_real(parent)),
    error = function(e) NULL
  )
  if (is.null(canonical_parent)) {
    optimizer_checkpoint_trust_abort(
      "the checkpoint directory cannot be resolved canonically"
    )
  }
  canonical_path <- file.path(canonical_parent, basename(absolute))

  if (cache_private_modes_supported()) {
    capability <- audit_existing_cache_parent_capability(canonical_path)
    chain_audit <- audit_cache_parent_chain(canonical_parent)
    directory_audit <- audit_private_cache_directory(canonical_parent)
    trust <- cache_directory_identity(canonical_parent)
    if (
      !isTRUE(capability$ok) ||
        !isTRUE(chain_audit$ok) ||
        !isTRUE(directory_audit$ok) ||
        is.null(trust) ||
        trust$owner_id != cache_effective_owner_id()
    ) {
      optimizer_checkpoint_trust_abort(
        capability$reason %||%
          chain_audit$reason %||%
          directory_audit$reason %||%
          "the checkpoint directory identity is not trusted"
      )
    }
  } else {
    trust <- list(path = canonical_parent, windows_unverified_acl = TRUE)
  }

  guard <- list(path = canonical_path, parent = trust)
  if (require_existing && !file.exists(canonical_path)) {
    optimizer_checkpoint_trust_abort("the checkpoint file does not exist")
  }
  if (file.exists(canonical_path)) {
    identity <- optimizer_checkpoint_file_identity(canonical_path, guard)
    if (!isTRUE(identity$ok)) {
      optimizer_checkpoint_trust_abort(identity$reason)
    }
    guard$existing_identity <- identity$identity
  } else {
    guard$existing_identity <- NULL
  }
  guard
}

optimizer_checkpoint_assert_parent <- function(guard) {
  if (cache_private_modes_supported()) {
    reason <- cache_directory_trust_error(guard$parent)
    if (!is.null(reason)) {
      optimizer_checkpoint_trust_abort(reason)
    }
  } else {
    current <- tryCatch(
      as.character(fs::path_real(dirname(guard$path))),
      error = function(e) NULL
    )
    if (is.null(current) || !identical(current, guard$parent$path)) {
      optimizer_checkpoint_trust_abort(
        "the checkpoint directory identity changed"
      )
    }
  }
  invisible(TRUE)
}

optimizer_checkpoint_file_identity <- function(path, guard) {
  optimizer_checkpoint_assert_parent(guard)
  if (cache_private_modes_supported()) {
    return(cache_private_file_identity(path, guard$parent))
  }
  if (!file.exists(path)) {
    return(list(ok = FALSE, missing = TRUE, reason = "the file is missing"))
  }
  if (cache_path_is_symlink(path) || !cache_path_is_regular(path)) {
    return(list(ok = FALSE, reason = "the checkpoint is not a regular file"))
  }
  parent <- tryCatch(
    as.character(fs::path_real(dirname(path))),
    error = function(e) NULL
  )
  info <- tryCatch(
    suppressWarnings(fs::file_info(path, follow = FALSE, fail = FALSE)),
    error = function(e) NULL
  )
  if (
    is.null(parent) ||
      !identical(parent, guard$parent$path) ||
      is.null(info) ||
      nrow(info) != 1L
  ) {
    return(list(ok = FALSE, reason = "the checkpoint identity is unavailable"))
  }
  list(
    ok = TRUE,
    identity = list(
      device_id = as.numeric(info$device_id[[1L]]),
      inode = as.numeric(info$inode[[1L]]),
      size = as.numeric(info$size[[1L]]),
      modification_time = as.numeric(info$modification_time[[1L]]),
      change_time = as.numeric(info$change_time[[1L]])
    )
  )
}

optimizer_checkpoint_identity_core <- function(identity) {
  identity[intersect(
    c("device_id", "inode", "owner_id", "mode"),
    names(identity)
  )]
}

optimizer_checkpoint_identity_content <- function(
  identity,
  include_change_time = TRUE
) {
  fields <- c(
    "device_id",
    "inode",
    "owner_id",
    "mode",
    "size",
    "modification_time"
  )
  if (isTRUE(include_change_time)) {
    fields <- c(fields, "change_time")
  }
  identity[intersect(fields, names(identity))]
}

optimizer_checkpoint_assert_same_file <- function(
  before,
  after,
  message,
  include_content = FALSE,
  include_change_time = TRUE
) {
  before_value <- if (include_content) {
    optimizer_checkpoint_identity_content(before, include_change_time)
  } else {
    optimizer_checkpoint_identity_core(before)
  }
  after_value <- if (include_content) {
    optimizer_checkpoint_identity_content(after, include_change_time)
  } else {
    optimizer_checkpoint_identity_core(after)
  }
  if (!identical(before_value, after_value)) {
    optimizer_checkpoint_trust_abort(message)
  }
  invisible(TRUE)
}

optimizer_checkpoint_conflict_abort <- function(message) {
  cli::cli_abort(
    c(
      "Optimizer checkpoint publication conflict",
      "x" = message,
      "i" = "Reload the latest checkpoint before publishing more progress."
    ),
    class = c(
      "dsprrr_optimizer_checkpoint_conflict",
      "dsprrr_optimizer_checkpoint_io_error"
    )
  )
}

optimizer_checkpoint_current_identity <- function(guard) {
  if (!file.exists(guard$path)) {
    return(NULL)
  }
  current <- optimizer_checkpoint_file_identity(guard$path, guard)
  if (!isTRUE(current$ok)) {
    optimizer_checkpoint_trust_abort(current$reason)
  }
  current$identity
}

optimizer_checkpoint_assert_predecessor <- function(current, expected) {
  if (is.null(expected) && is.null(current)) {
    return(invisible(TRUE))
  }
  if (is.null(expected) && !is.null(current)) {
    optimizer_checkpoint_conflict_abort(
      "the checkpoint appeared after this optimizer context was created"
    )
  }
  if (!is.null(expected) && is.null(current)) {
    optimizer_checkpoint_conflict_abort(
      "the checkpoint was removed after this optimizer context was created"
    )
  }
  if (
    !identical(
      optimizer_checkpoint_identity_content(current),
      optimizer_checkpoint_identity_content(expected)
    )
  ) {
    optimizer_checkpoint_conflict_abort(
      "the checkpoint changed after this optimizer context was created"
    )
  }
  invisible(TRUE)
}

optimizer_checkpoint_read_rds <- function(path) {
  readRDS(path)
}

optimizer_checkpoint_atomic_save_locked <- function(
  checkpoint,
  guard,
  expected_predecessor = NULL,
  enforce_predecessor = FALSE
) {
  path <- guard$path
  optimizer_checkpoint_assert_parent(guard)
  current_predecessor <- optimizer_checkpoint_current_identity(guard)
  if (isTRUE(enforce_predecessor)) {
    optimizer_checkpoint_assert_predecessor(
      current_predecessor,
      expected_predecessor
    )
  }
  temporary <- artifact_private_stage(path)
  on.exit(unlink(temporary), add = TRUE)
  staging_before <- optimizer_checkpoint_file_identity(temporary, guard)
  if (!isTRUE(staging_before$ok)) {
    optimizer_checkpoint_trust_abort(staging_before$reason)
  }
  tryCatch(
    artifact_write_rds(checkpoint, temporary),
    error = function(e) {
      cli::cli_abort(
        "Could not write optimizer checkpoint",
        parent = e,
        class = "dsprrr_optimizer_checkpoint_io_error"
      )
    }
  )
  staging_written <- optimizer_checkpoint_file_identity(temporary, guard)
  if (!isTRUE(staging_written$ok)) {
    optimizer_checkpoint_trust_abort(staging_written$reason)
  }
  optimizer_checkpoint_assert_same_file(
    staging_before$identity,
    staging_written$identity,
    "the checkpoint staging file changed identity while being written"
  )
  before_read <- staging_written
  staged <- tryCatch(
    optimizer_checkpoint_read_rds(temporary),
    error = function(e) {
      cli::cli_abort(
        "Could not verify staged optimizer checkpoint",
        parent = e,
        class = "dsprrr_optimizer_checkpoint_io_error"
      )
    }
  )
  after_read <- optimizer_checkpoint_file_identity(temporary, guard)
  if (!isTRUE(after_read$ok)) {
    optimizer_checkpoint_trust_abort(after_read$reason)
  }
  optimizer_checkpoint_assert_same_file(
    before_read$identity,
    after_read$identity,
    "the checkpoint staging file changed while being verified",
    include_content = TRUE
  )
  optimizer_checkpoint_validate_manifest(staged)
  optimizer_checkpoint_assert_parent(guard)
  if (is.null(current_predecessor)) {
    if (file.exists(path)) {
      optimizer_checkpoint_trust_abort(
        "the checkpoint target appeared during publication"
      )
    }
  } else {
    current <- optimizer_checkpoint_file_identity(path, guard)
    if (!isTRUE(current$ok)) {
      optimizer_checkpoint_trust_abort(current$reason)
    }
    optimizer_checkpoint_assert_same_file(
      current_predecessor,
      current$identity,
      "the checkpoint target changed before publication",
      include_content = TRUE
    )
  }
  tryCatch(
    artifact_atomic_replace(temporary, path, what = "optimizer checkpoint"),
    error = function(e) {
      cli::cli_abort(
        "Could not atomically publish optimizer checkpoint",
        parent = e,
        class = "dsprrr_optimizer_checkpoint_io_error"
      )
    }
  )
  optimizer_checkpoint_assert_parent(guard)
  installed <- optimizer_checkpoint_file_identity(path, guard)
  if (!isTRUE(installed$ok)) {
    optimizer_checkpoint_trust_abort(installed$reason)
  }
  optimizer_checkpoint_assert_same_file(
    after_read$identity,
    installed$identity,
    "the published checkpoint is not the verified staging file",
    include_content = TRUE,
    include_change_time = FALSE
  )
  invisible(installed$identity)
}

optimizer_checkpoint_atomic_save <- function(
  checkpoint,
  path,
  expected_predecessor = NULL,
  enforce_predecessor = FALSE
) {
  optimizer_checkpoint_validate_manifest(checkpoint)
  optimizer_checkpoint_with_lock(path, function(guard) {
    optimizer_checkpoint_atomic_save_locked(
      checkpoint,
      guard,
      expected_predecessor = expected_predecessor,
      enforce_predecessor = enforce_predecessor
    )
  })
}

optimizer_checkpoint_read_locked <- function(guard) {
  path <- guard$path
  if (!file.exists(path)) {
    optimizer_checkpoint_trust_abort("the checkpoint file does not exist")
  }
  before <- optimizer_checkpoint_file_identity(path, guard)
  if (!isTRUE(before$ok)) {
    optimizer_checkpoint_trust_abort(before$reason)
  }
  checkpoint <- tryCatch(
    optimizer_checkpoint_read_rds(path),
    error = function(e) {
      cli::cli_abort(
        "Could not read optimizer checkpoint",
        parent = e,
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
  )
  after <- optimizer_checkpoint_file_identity(path, guard)
  if (!isTRUE(after$ok)) {
    optimizer_checkpoint_trust_abort(after$reason)
  }
  optimizer_checkpoint_assert_same_file(
    before$identity,
    after$identity,
    "the checkpoint changed identity while being read",
    include_content = TRUE
  )
  optimizer_checkpoint_validate_manifest(checkpoint)
  list(checkpoint = checkpoint, identity = after$identity)
}

optimizer_checkpoint_read_snapshot <- function(path) {
  optimizer_checkpoint_with_lock(path, function(guard) {
    optimizer_checkpoint_read_locked(guard)
  })
}

optimizer_checkpoint_snapshot <- function(path) {
  optimizer_checkpoint_with_lock(path, function(guard) {
    optimizer_checkpoint_current_identity(guard)
  })
}

optimizer_checkpoint_read <- function(path) {
  optimizer_checkpoint_read_snapshot(path)$checkpoint
}

optimizer_checkpoint_diff_values <- function(expected, actual, path) {
  if (identical(expected, actual)) {
    return(list())
  }
  if (
    is.list(expected) &&
      is.list(actual) &&
      is.null(attr(expected, "class")) &&
      is.null(attr(actual, "class")) &&
      !is.null(names(expected)) &&
      !is.null(names(actual))
  ) {
    keys <- union(names(expected), names(actual))
    differences <- list()
    for (key in keys) {
      child <- optimizer_checkpoint_diff_values(
        expected[[key]],
        actual[[key]],
        paste0(path, ".", key)
      )
      differences <- c(differences, child)
    }
    return(differences)
  }
  list(list(field = path, checkpoint = actual, current = expected))
}

optimizer_checkpoint_compatibility_diff <- function(checkpoint, expected) {
  differences <- list()
  differences <- c(
    differences,
    optimizer_checkpoint_diff_values(
      expected$dsprrr_version,
      checkpoint$metadata$dsprrr_version,
      "metadata.dsprrr_version"
    ),
    optimizer_checkpoint_diff_values(
      expected$artifact_version,
      checkpoint$metadata$artifact_version,
      "metadata.artifact_version"
    ),
    optimizer_checkpoint_diff_values(
      expected$optimizer,
      checkpoint$optimizer,
      "optimizer"
    ),
    optimizer_checkpoint_diff_values(
      expected$compatibility,
      checkpoint$compatibility,
      "compatibility"
    )
  )
  differences
}

optimizer_checkpoint_abort_incompatible <- function(differences) {
  fields <- vapply(differences, `[[`, character(1), "field")
  cli::cli_abort(
    c(
      "Optimizer checkpoint is incompatible with this run",
      "x" = "Changed fields: {paste(fields, collapse = ', ')}",
      "i" = "Use the original program, data, metric, and search configuration, or start a new checkpoint."
    ),
    class = "dsprrr_optimizer_checkpoint_incompatible",
    differences = differences
  )
}

optimizer_checkpoint_begin <- function(
  optimizer_name,
  optimizer_version,
  program,
  data,
  metric,
  config,
  control,
  initial_state = list(),
  initial_phase = "initialized"
) {
  if (is.null(control)) {
    control <- optimizer_control()
  }
  registry <- artifact_validate_registry(control@checkpoint_registry)
  sanitized_config <- optimizer_checkpoint_sanitize(config, "config")
  expected <- list(
    dsprrr_version = optimizer_checkpoint_package_version(),
    artifact_version = artifact_format_version(),
    optimizer = list(
      name = as.character(optimizer_name)[1L],
      version = as.integer(optimizer_version)
    ),
    compatibility = list(
      config = sanitized_config,
      fingerprints = optimizer_checkpoint_fingerprints(
        program,
        data,
        metric,
        registry
      )
    )
  )

  checkpoint <- NULL
  predecessor <- NULL
  if (isTRUE(control@resume)) {
    restored <- optimizer_checkpoint_read_snapshot(control@checkpoint_path)
    checkpoint <- restored$checkpoint
    predecessor <- restored$identity
    differences <- optimizer_checkpoint_compatibility_diff(
      checkpoint,
      expected
    )
    if (length(differences) > 0L) {
      optimizer_checkpoint_abort_incompatible(differences)
    }
  } else if (optimizer_checkpoint_enabled(control)) {
    predecessor <- optimizer_checkpoint_snapshot(control@checkpoint_path)
  }

  publication <- new.env(parent = emptyenv())
  publication$predecessor <- predecessor

  budget <- new_optimizer_budget(
    control,
    state = if (is.null(checkpoint)) NULL else checkpoint$budget
  )
  best_program <- if (is.null(checkpoint) || is.null(checkpoint$best_program)) {
    copy_module(program)
  } else {
    restore_program_artifact(
      checkpoint$best_program,
      registry = registry,
      trusted = FALSE
    )
  }

  list(
    path = control@checkpoint_path,
    registry = registry,
    expected = expected,
    created_at = if (is.null(checkpoint)) {
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    } else {
      checkpoint$metadata$created_at
    },
    budget = budget,
    phase = if (is.null(checkpoint)) {
      initial_phase
    } else {
      checkpoint$progress$phase
    },
    search_state = if (is.null(checkpoint)) {
      optimizer_checkpoint_sanitize(initial_state, "initial_state")
    } else {
      checkpoint$progress$search_state
    },
    lineage = if (is.null(checkpoint)) list() else checkpoint$progress$lineage,
    rng = if (is.null(checkpoint)) NULL else checkpoint$rng,
    best_program = best_program,
    resumed = !is.null(checkpoint),
    publication = publication
  )
}

optimizer_checkpoint_capture_rng <- function() {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    as.integer(get(".Random.seed", envir = globalenv(), inherits = FALSE))
  } else {
    NULL
  }
}

optimizer_checkpoint_restore_rng <- function(rng) {
  if (is.null(rng)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  } else {
    assign(".Random.seed", as.integer(rng), envir = globalenv())
  }
  invisible(rng)
}

optimizer_checkpoint_write <- function(
  context,
  phase,
  search_state,
  lineage = context$lineage,
  best_program = context$best_program,
  rng = optimizer_checkpoint_capture_rng()
) {
  if (is.null(context$path)) {
    return(invisible(NULL))
  }
  checkpoint <- structure(
    list(
      format = "dsprrr-optimizer-checkpoint",
      format_version = optimizer_checkpoint_format_version(),
      metadata = list(
        created_at = context$created_at,
        written_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        dsprrr_version = context$expected$dsprrr_version,
        artifact_version = context$expected$artifact_version,
        r_version = paste(R.version$major, R.version$minor, sep = ".")
      ),
      optimizer = context$expected$optimizer,
      compatibility = context$expected$compatibility,
      budget = optimizer_budget_state(context$budget),
      rng = if (is.null(rng)) NULL else as.integer(rng),
      progress = list(
        phase = as.character(phase)[1L],
        search_state = optimizer_checkpoint_sanitize(
          search_state,
          "progress.search_state"
        ),
        lineage = optimizer_checkpoint_sanitize(
          lineage,
          "progress.lineage"
        )
      ),
      best_program = if (is.null(best_program)) {
        NULL
      } else {
        optimizer_checkpoint_program_artifact(best_program, context$registry)
      }
    ),
    class = c("dsprrr_optimizer_checkpoint", "list")
  )
  checkpoint$integrity <- optimizer_checkpoint_integrity(checkpoint)
  installed <- optimizer_checkpoint_atomic_save(
    checkpoint,
    context$path,
    expected_predecessor = context$publication$predecessor,
    enforce_predecessor = TRUE
  )
  context$publication$predecessor <- installed
  invisible(checkpoint)
}
