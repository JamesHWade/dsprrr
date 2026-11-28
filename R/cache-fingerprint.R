# Cache request, provider, and output-schema fingerprinting

# Cache keys are an internal persistence format. Increment this whenever the
# fingerprint contract changes so old disk entries become unreachable.
cache_request_schema_version <- function() 3L

cache_envelope_schema_version <- function() 1L

#' Hash a value without retaining its contents in fingerprint material
#' @noRd
cache_opaque_value <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  serialized <- tryCatch(
    serialize(x, connection = NULL, version = 2),
    error = function(e) {
      cli::cli_abort(
        "Cannot safely fingerprint a value of class {.cls {class(x)[1]}}",
        class = "dsprrr_cache_fingerprint_error",
        parent = e
      )
    }
  )

  list(
    type = typeof(x),
    class = unname(class(x)),
    length = length(x),
    sha256 = digest::digest(serialized, algo = "sha256", serialize = FALSE)
  )
}

#' Determine whether a field contains authentication material
#' @noRd
cache_is_secret_name <- function(name) {
  compact <- gsub("[^a-z0-9]", "", tolower(name))
  compact %in%
    c(
      "apikey",
      "accesskey",
      "accesstoken",
      "authtoken",
      "token",
      "refreshtoken",
      "session",
      "sessionid",
      "sessiontoken",
      "bearer",
      "bearertoken",
      "authorization",
      "credential",
      "credentials",
      "secret",
      "clientsecret",
      "password",
      "cookie",
      "cookies",
      "privatekey"
    ) ||
    grepl(
      "(apikey|accesstoken|authtoken|refreshtoken|sessiontoken|bearertoken|authorization|credentials|clientsecret|password|privatekey)$",
      compact
    )
}

#' Determine whether a field partitions provider/account identity
#' @noRd
cache_is_account_name <- function(name) {
  compact <- gsub("[^a-z0-9]", "", tolower(name))
  compact %in%
    c(
      "account",
      "accountid",
      "organization",
      "organizationid",
      "org",
      "orgid",
      "project",
      "projectid",
      "tenant",
      "tenantid",
      "subscription",
      "subscriptionid",
      "workspace",
      "workspaceid"
    )
}

#' Resolve and hash credential/account fields without retaining their values
#' @noRd
cache_partition_records <- function(x, path = character()) {
  if (is.null(x)) {
    return(list())
  }

  if (is.atomic(x) && !is.null(names(x))) {
    x <- as.list(x)
  }

  if (is.list(x) && !is.null(names(x))) {
    records <- list()
    for (i in seq_along(x)) {
      name <- names(x)[[i]]
      value <- x[[i]]
      value_path <- c(path, name)
      if (cache_is_secret_name(name) || cache_is_account_name(name)) {
        if (is.function(value)) {
          if (length(formals(value)) != 0) {
            cli::cli_abort(
              "Credential getter {.field {name}} requires arguments",
              class = "dsprrr_cache_fingerprint_error"
            )
          }
          value <- tryCatch(
            value(),
            error = function(e) {
              cli::cli_abort(
                "Failed to resolve credential/account identity",
                class = "dsprrr_cache_fingerprint_error",
                parent = e
              )
            }
          )
        }
        records[[length(records) + 1L]] <- list(
          path = paste(value_path, collapse = "/"),
          value = cache_opaque_value(value)$sha256
        )
      } else {
        records <- c(records, cache_partition_records(value, value_path))
      }
    }
    return(records)
  }

  if (is.list(x)) {
    records <- list()
    for (i in seq_along(x)) {
      records <- c(
        records,
        cache_partition_records(x[[i]], c(path, as.character(i)))
      )
    }
    return(records)
  }

  list()
}

#' Build one opaque provider/account partition digest
#' @noRd
cache_account_partition <- function(x) {
  records <- cache_partition_records(x)
  if (length(records) == 0) {
    return(NULL)
  }
  paths <- vapply(records, `[[`, character(1), "path")
  records <- records[order(paths, seq_along(paths))]
  digest::digest(
    cache_fingerprint_json(records),
    algo = "sha256",
    serialize = FALSE
  )
}

#' Convert configuration to deterministic, content-safe plain R data
#'
#' Atomic values are represented by hashes. Named configuration lists are
#' sorted by name, while request/schema lists can preserve their original order.
#' @noRd
cache_config_fingerprint <- function(
  x,
  sort_named = TRUE,
  omit_secrets = FALSE
) {
  if (is.null(x)) {
    return(list(kind = "null"))
  }

  if (is.atomic(x) && is.null(names(x))) {
    return(c(list(kind = "atomic"), cache_opaque_value(x)))
  }

  if (is.atomic(x)) {
    x <- as.list(x)
  }

  if (is.data.frame(x)) {
    return(list(
      kind = "data_frame",
      rows = nrow(x),
      columns = lapply(seq_along(x), function(i) {
        list(
          name = names(x)[[i]],
          value = cache_config_fingerprint(x[[i]], sort_named = FALSE)
        )
      })
    ))
  }

  if (!is.list(x)) {
    cli::cli_abort(
      "Cannot safely fingerprint a value of class {.cls {class(x)[1]}}",
      class = "dsprrr_cache_fingerprint_error"
    )
  }

  item_names <- names(x)
  if (is.null(item_names)) {
    return(list(
      kind = "list",
      entries = lapply(
        x,
        cache_config_fingerprint,
        sort_named = sort_named,
        omit_secrets = omit_secrets
      )
    ))
  }

  indices <- seq_along(x)
  if (omit_secrets) {
    keep <- !vapply(item_names, cache_is_secret_name, logical(1))
    indices <- indices[keep]
  }
  if (sort_named && length(indices) > 1) {
    indices <- indices[order(item_names[indices], indices)]
  }

  list(
    kind = "named_list",
    entries = lapply(indices, function(i) {
      list(
        name = item_names[[i]],
        value = cache_config_fingerprint(
          x[[i]],
          sort_named = sort_named,
          omit_secrets = omit_secrets
        )
      )
    })
  )
}

#' Convert an ellmer output type to an exact recursive schema
#' @noRd
cache_output_schema <- function(output_type) {
  if (!is_ellmer_type(output_type)) {
    return(list(
      class = unname(class(output_type)),
      value = cache_config_fingerprint(output_type, sort_named = FALSE)
    ))
  }

  common <- list(
    class = class(output_type)[[1]],
    required = tryCatch(output_type@required, error = function(e) NULL),
    description = cache_opaque_value(
      tryCatch(output_type@description, error = function(e) NULL)
    )
  )

  if (inherits(output_type, "ellmer::TypeBasic")) {
    return(c(common, list(type = output_type@type)))
  }

  if (inherits(output_type, "ellmer::TypeEnum")) {
    return(c(common, list(values = cache_opaque_value(output_type@values))))
  }

  if (inherits(output_type, "ellmer::TypeArray")) {
    return(c(common, list(items = cache_output_schema(output_type@items))))
  }

  if (inherits(output_type, "ellmer::TypeObject")) {
    properties <- output_type@properties
    return(c(
      common,
      list(
        properties = lapply(seq_along(properties), function(i) {
          list(
            name = names(properties)[[i]],
            schema = cache_output_schema(properties[[i]])
          )
        }),
        additional_properties = output_type@additional_properties
      )
    ))
  }

  if (inherits(output_type, "ellmer::TypeJsonSchema")) {
    return(c(
      common,
      list(
        json = cache_config_fingerprint(output_type@json, sort_named = FALSE)
      )
    ))
  }

  if (inherits(output_type, "ellmer::TypeIgnore")) {
    return(common)
  }

  cli::cli_abort(
    "Unsupported ellmer output type {.cls {class(output_type)[1]}}",
    class = "dsprrr_cache_fingerprint_error"
  )
}

#' Serialize an output type to deterministic JSON
#' @noRd
serialize_output_type <- function(output_type) {
  cache_fingerprint_json(cache_output_schema(output_type))
}

#' Determine whether a value is an ellmer content object
#' @noRd
is_ellmer_content <- function(x) {
  is_content_input(x)
}

#' Fingerprint one ellmer content object
#' @noRd
cache_content_fingerprint <- function(content) {
  if (is.character(content)) {
    return(list(class = "text", text = cache_opaque_value(content)))
  }

  if (!is_ellmer_content(content)) {
    cli::cli_abort(
      "Unsupported request content {.cls {class(content)[1]}}",
      class = "dsprrr_cache_fingerprint_error"
    )
  }

  content_class <- class(content)[[1]]

  if (inherits(content, "ellmer::ContentText")) {
    return(list(class = content_class, text = cache_opaque_value(content@text)))
  }

  if (inherits(content, "ellmer::ContentThinking")) {
    return(list(
      class = content_class,
      thinking = cache_opaque_value(content@thinking),
      extra = cache_config_fingerprint(content@extra)
    ))
  }

  if (inherits(content, "ellmer::ContentJson")) {
    return(list(
      class = content_class,
      data = cache_config_fingerprint(content@data, sort_named = FALSE),
      string = cache_opaque_value(content@string)
    ))
  }

  if (inherits(content, "ellmer::ContentImageInline")) {
    return(list(
      class = content_class,
      type = content@type,
      data = cache_opaque_value(content@data)
    ))
  }

  if (inherits(content, "ellmer::ContentImageRemote")) {
    return(list(
      class = content_class,
      url = cache_opaque_value(content@url),
      detail = content@detail
    ))
  }

  if (inherits(content, "ellmer::ContentPDF")) {
    return(list(
      class = content_class,
      type = content@type,
      data = cache_opaque_value(content@data),
      filename = cache_opaque_value(content@filename),
      url = if (S7::prop_exists(content, "url")) {
        cache_opaque_value(content@url)
      } else {
        NULL
      }
    ))
  }

  if (inherits(content, "ellmer::ContentToolRequest")) {
    return(list(
      class = content_class,
      id = cache_opaque_value(content@id),
      name = content@name,
      arguments = cache_config_fingerprint(
        content@arguments,
        sort_named = FALSE
      ),
      extra = cache_config_fingerprint(content@extra, sort_named = FALSE)
    ))
  }

  if (inherits(content, "ellmer::ContentToolResult")) {
    request <- content@request
    request_identity <- if (is.null(request)) {
      NULL
    } else {
      list(
        id = cache_opaque_value(request@id),
        name = request@name,
        arguments = cache_config_fingerprint(
          request@arguments,
          sort_named = FALSE
        )
      )
    }
    return(list(
      class = content_class,
      value = cache_config_fingerprint(content@value, sort_named = FALSE),
      error = cache_opaque_value(content@error),
      extra = cache_config_fingerprint(content@extra, sort_named = FALSE),
      request = request_identity
    ))
  }

  props <- tryCatch(
    S7::props(content),
    error = function(e) {
      cli::cli_abort(
        "Cannot inspect request content {.cls {content_class}}",
        class = "dsprrr_cache_fingerprint_error",
        parent = e
      )
    }
  )
  props$tool <- NULL
  props$request <- NULL

  list(
    class = content_class,
    properties = cache_config_fingerprint(props, sort_named = FALSE)
  )
}

#' Fingerprint a request payload while retaining content boundaries
#' @noRd
cache_payload_fingerprint <- function(payload) {
  if (is.character(payload) || is_ellmer_content(payload)) {
    return(list(cache_content_fingerprint(payload)))
  }

  if (
    is.list(payload) &&
      all(vapply(payload, is_ellmer_content, logical(1)))
  ) {
    return(lapply(payload, cache_content_fingerprint))
  }

  cli::cli_abort(
    "Request payload contains unsupported content",
    class = "dsprrr_cache_fingerprint_error"
  )
}

#' Fingerprint one conversation turn
#' @noRd
cache_turn_fingerprint <- function(turn) {
  contents <- tryCatch(
    turn@contents,
    error = function(e) {
      cli::cli_abort(
        "Cannot inspect conversation turn {.cls {class(turn)[1]}}",
        class = "dsprrr_cache_fingerprint_error",
        parent = e
      )
    }
  )

  result <- list(
    class = class(turn)[[1]],
    role = tryCatch(turn@role, error = function(e) class(turn)[[1]]),
    contents = lapply(contents, cache_content_fingerprint)
  )

  if (inherits(turn, "ellmer::AssistantTurn")) {
    result$json <- cache_config_fingerprint(turn@json, sort_named = FALSE)
    if (S7::prop_exists(turn, "finish_reason")) {
      result$finish_reason <- cache_opaque_value(
        S7::prop(turn, "finish_reason")
      )
    }
  }

  result
}

#' Verify the canonical mutable-state contract supplied by ellmer::Chat
#' @noRd
cache_is_trusted_ellmer_chat <- function(llm) {
  ellmer_chat_cache_supported(llm)
}

#' Require a Chat whose state can be inspected and replayed safely
#' @noRd
cache_require_trusted_ellmer_chat <- function(llm, error_class) {
  if (!cache_is_trusted_ellmer_chat(llm)) {
    cli::cli_abort(
      c(
        "Caching requires a compatible {.cls ellmer::Chat}",
        "x" = "The Chat layout cannot be fingerprinted or replayed safely.",
        "i" = "Use an unmodified ellmer::Chat with ellmer 0.5.0 or newer."
      ),
      class = c(
        "dsprrr_ellmer_compatibility_error",
        "dsprrr_cache_untrusted_chat",
        error_class
      ),
      operation = "caching"
    )
  }
  invisible(llm)
}

#' Call a Chat getter, failing closed when required state is unavailable
#' @noRd
cache_chat_get <- function(llm, name, default = NULL, required = FALSE) {
  getter <- tryCatch(llm[[name]], error = function(e) NULL)
  if (is.null(getter)) {
    if (isTRUE(required)) {
      cli::cli_abort(
        "Chat state inspection requires {.code {name}()}",
        class = "dsprrr_cache_fingerprint_error"
      )
    }
    return(default)
  }
  if (!is.function(getter)) {
    cli::cli_abort(
      "Chat member {.field {name}} is not callable",
      class = "dsprrr_cache_fingerprint_error"
    )
  }

  tryCatch(
    getter(),
    error = function(e) {
      cli::cli_abort(
        "Failed to inspect Chat state with {.code {name}()}",
        class = "dsprrr_cache_fingerprint_error",
        parent = e
      )
    }
  )
}

#' Fingerprint provider identity and output-affecting configuration
#' @noRd
cache_provider_fingerprint <- function(llm) {
  provider <- cache_chat_get(
    llm,
    "get_provider",
    default = NULL,
    required = TRUE
  )

  if (!is.null(provider)) {
    props <- tryCatch(
      ellmer_provider_props(provider),
      error = function(e) {
        cli::cli_abort(
          "Cannot inspect provider {.cls {class(provider)[1]}}",
          class = "dsprrr_cache_fingerprint_error",
          parent = e
        )
      }
    )
    provider_name <- props$name %||% class(provider)[[1]]
    model <- cache_chat_get(llm, "get_model", default = NULL)
    # Runtime params such as temperature live on the Model object.
    model_values <- tryCatch(
      ellmer_chat_model_values(llm, required = TRUE),
      error = function(e) {
        cli::cli_abort(
          "Cannot inspect Chat request parameters for cache identity",
          class = c(
            "dsprrr_ellmer_compatibility_error",
            "dsprrr_cache_fingerprint_error"
          ),
          parent = e
        )
      }
    )
    props$params <- model_values$params
    props$extra_args <- model_values$extra_args
    account_partition <- cache_account_partition(props)
    props <- props[!vapply(names(props), cache_is_secret_name, logical(1))]
    props$name <- NULL

    return(list(
      kind = "ellmer_provider",
      class = class(provider)[[1]],
      name = provider_name,
      model = model,
      account_partition = account_partition,
      config = cache_config_fingerprint(props, omit_secrets = TRUE)
    ))
  }

  cli::cli_abort(
    "Chat provider inspection returned no provider",
    class = "dsprrr_cache_fingerprint_error"
  )
}

#' Build a versioned fingerprint for an actual Chat request
#' @noRd
cache_request_fingerprint <- function(
  llm,
  payload,
  output_type,
  rollout_id = NULL
) {
  cache_require_trusted_ellmer_chat(
    llm,
    error_class = "dsprrr_cache_fingerprint_error"
  )
  turns <- cache_chat_get(
    llm,
    "get_turns",
    default = NULL,
    required = TRUE
  )
  if (!is.list(turns)) {
    cli::cli_abort(
      "Chat history inspection must return a list of turns",
      class = "dsprrr_cache_fingerprint_error"
    )
  }
  tools <- cache_chat_get(
    llm,
    "get_tools",
    default = NULL,
    required = TRUE
  )
  if (!is.list(tools)) {
    cli::cli_abort(
      "Chat tool inspection must return a list",
      class = "dsprrr_cache_fingerprint_error"
    )
  }
  if (length(tools) > 0) {
    cli::cli_abort(
      c(
        "Caching is disabled for Chats with registered tools",
        "i" = "Tool implementations and side effects are not safely cacheable."
      ),
      class = c(
        "dsprrr_cache_tools_error",
        "dsprrr_cache_fingerprint_error"
      )
    )
  }
  system_prompt <- cache_chat_get(
    llm,
    "get_system_prompt",
    default = NULL,
    required = TRUE
  )

  list(
    version = cache_request_schema_version(),
    ellmer_version = as.character(utils::packageVersion("ellmer")),
    provider = cache_provider_fingerprint(llm),
    system_prompt = cache_opaque_value(system_prompt),
    history = lapply(turns, cache_turn_fingerprint),
    tools = list(),
    request = cache_payload_fingerprint(payload),
    output_schema = cache_output_schema(output_type),
    rollout_id = cache_opaque_value(
      if (is.null(rollout_id)) NULL else as.character(rollout_id)
    )
  )
}

#' Serialize a cache fingerprint to deterministic JSON
#' @noRd
cache_fingerprint_json <- function(fingerprint) {
  as.character(jsonlite::toJSON(
    fingerprint,
    auto_unbox = TRUE,
    null = "null",
    na = "string",
    digits = NA,
    pretty = FALSE
  ))
}

#' Compute Cache Key
#'
#' Runtime requests provide the complete fingerprint built by
#' `cache_request_fingerprint()`.
#' @noRd
cache_key <- function(fingerprint) {
  digest::digest(
    cache_fingerprint_json(fingerprint),
    algo = "sha256",
    serialize = FALSE
  )
}
