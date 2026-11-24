# Encode portable values, typed content, and registered runtime references.

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
  drop_runtime_names,
  reject_secret_names = FALSE
) {
  if (is.null(value)) {
    return(NULL)
  }
  if (reject_secret_names) {
    artifact_reject_secret_named_fields(value, path)
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
      drop_runtime_names,
      reject_secret_names
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

artifact_reject_secret_named_fields <- function(value, path) {
  value_names <- names(value)
  if (is.null(value_names)) {
    return(invisible(NULL))
  }
  secret <- !is.na(value_names) &
    nzchar(value_names) &
    vapply(value_names, artifact_is_secret_name, logical(1))
  if (!any(secret)) {
    return(invisible(NULL))
  }
  field_path <- paste0(path, ".", value_names[[which(secret)[[1L]]]])
  cli::cli_abort(
    c(
      "Semantic program data cannot be persisted safely",
      "x" = "{.field {field_path}} has a credential-like field name and cannot be silently removed.",
      "i" = "Rename the semantic field or keep this program outside the safe artifact format."
    ),
    class = "dsprrr_artifact_unsafe_value"
  )
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
      !grepl("^https://", url, ignore.case = TRUE) ||
      grepl("[?#]", url)
  ) {
    return(FALSE)
  }
  without_scheme <- sub("^[^:]+://", "", url)
  authority <- sub("[/#?].*$", "", without_scheme)
  if (
    !nzchar(authority) ||
      grepl("@", utils::URLdecode(authority), fixed = TRUE) ||
      artifact_remote_url_has_signed_path(url)
  ) {
    return(FALSE)
  }
  TRUE
}

artifact_remote_url_has_signed_path <- function(url) {
  without_scheme <- sub("^[^:]+://", "", url)
  path <- sub("^[^/]*", "", without_scheme)
  decoded <- tryCatch(utils::URLdecode(path), error = function(e) path)
  patterns <- c(
    "(^|/)s--[^/]+--(/|$)",
    paste0(
      "(^|/)(x-amz-)?(signed|signature|sig|token|auth|key)",
      "[=:_-][^/]+(/|$)"
    ),
    "(^|/)(signed|signature|token|auth|key)/[^/]+(/|$)"
  )
  any(vapply(
    patterns,
    grepl,
    logical(1),
    x = decoded,
    ignore.case = TRUE,
    perl = TRUE
  ))
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
      ellmer_content_json,
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
  value_class <- class(value)
  identical(value_class, "data.frame") ||
    identical(value_class, c("tbl_df", "tbl", "data.frame"))
}

artifact_normalize_field_name <- function(name) {
  if (
    !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(name)
  ) {
    return("")
  }

  # Split acronym-to-word before lower-to-upper so both `openaiAPIKey` and
  # ordinary lower camel case retain semantic token boundaries.
  name <- gsub(
    "([A-Z]+)([A-Z][a-z])",
    "\\1_\\2",
    name,
    perl = TRUE
  )
  name <- gsub(
    "([a-z0-9])([A-Z])",
    "\\1_\\2",
    name,
    perl = TRUE
  )
  normalized <- gsub("[^a-z0-9]+", "_", tolower(name))
  gsub("^_+|_+$", "", normalized)
}

artifact_is_secret_name <- function(name) {
  normalized <- artifact_normalize_field_name(name)
  compact <- gsub("[^a-z0-9]+", "", tolower(name))
  boundary_match <- grepl(
    paste0(
      "(^|_)(api_?key|access_key|access_token|auth_token|authorization|",
      "client_secret|cookies?|credentials?|password|passwd|private_key|",
      "refresh_token|secret|session_id|session_token|token)(_|$)"
    ),
    normalized
  )
  compact_match <- grepl(
    paste0(
      "(apikey|accesskey|accesstoken|authtoken|clientsecret|privatekey|",
      "refreshtoken|sessionid|sessiontoken)(file|path|value)?$"
    ),
    compact
  )
  connection_match <- grepl(
    paste0(
      "(^|_)(connection_(string|uri|url)|data_source_name|dsn)",
      "(_(file|path|value))?$"
    ),
    normalized
  ) ||
    grepl(
      paste0(
        "(^|_)(database|db|datasource|data_source|jdbc|redis|mongo|",
        "mongodb|neo4j|cassandra|clickhouse|couchbase|influxdb|",
        "postgres|postgresql|",
        "mysql|mariadb|mssql|sqlserver|snowflake|amqp|rabbitmq|broker|",
        "smtp|ldap|elasticsearch|opensearch)_(url|uri)$"
      ),
      normalized
    )
  key_material_match <- grepl(
    paste0(
      "(^|_)(passphrase|pass_phrase|encryption_key|signing_key|ssh_key|",
      "hmac_key|tls_key|ssl_key|master_key|fernet_key|license_key|",
      "service_account_key|storage_account_key|totp_seed)",
      "(_(file|path|value|pem|data|bytes|hex|base32|base64|b64|json|",
      "jwk|der|pkcs8))?$|",
      "(^|_)service_account_json",
      "(_(file|path|value|data|bytes|base64))?$"
    ),
    normalized
  )
  boundary_match || compact_match || connection_match || key_material_match
}

artifact_is_runtime_name <- function(name) {
  normalized <- artifact_normalize_field_name(name)
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
  if (inherits(value, "ellmer::ToolDef")) {
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
        !all(nzchar(keys)))
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
