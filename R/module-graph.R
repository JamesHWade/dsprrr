#' Traverse and Transform Module Graphs
#'
#' @description
#' Module graphs provide one cycle-safe protocol for inspecting and transforming
#' nested dsprrr programs. Built-in adapters understand pipelines, wrappers,
#' ensembles, and multi-chain modules. Custom `Module` subclasses can opt in by
#' implementing two public methods:
#'
#' * `graph_children()` returns a list whose leaves are child `Module` objects.
#'   Lists can be nested and can be named or unnamed.
#' * `set_graph_children(children)` replaces that complete child structure and
#'   returns the program invisibly. This method is required only for replacement
#'   and mapping operations; read-only traversal needs only `graph_children()`.
#'
#' A custom module can additionally implement `graph_is_parameter()` and return
#' `TRUE` or `FALSE` to control whether [named_parameters()] includes it.
#'
#' Paths use a JSON-pointer-like form. `"$"` is the root, named list elements
#' use their names, and unnamed or ambiguously named lists use one-based numeric
#' positions. `/` and `~` in names are escaped as `~1` and `~0`.
#'
#' With `boundaries = "respect"`, compiled and frozen modules are included as
#' boundary nodes but neither they nor their descendants are mutated. A shared
#' module reachable through any protected path is protected at every alias.
#' Mapping visits each R6 identity once in post-order and rewires all aliases to
#' the same replacement. [replace_module()] defaults to replacing one path and
#' can opt into identity-wide replacement with `shared = "all"`. Mapping can
#' safely traverse cycles, but replacing a module that participates in a cycle
#' is rejected; use path-specific [replace_module()] to break a cycle edge.
#'
#' @param program A dsprrr `Module` object.
#' @param boundaries Whether compiled and frozen modules are crossed. Inspection
#'   defaults to `"cross"`, graph rewrites default to `"respect"`, and recursive
#'   LM propagation defaults to `"cross"` so runtime configuration reaches the
#'   complete program.
#' @param cycles Whether cycles are recorded or raise an error.
#' @param include_root Whether to include or transform the root module.
#' @param aliases Whether [named_modules()] includes shared and cyclic aliases.
#' @param .fn A function called as `.fn(module, path, ...)`. It must return a
#'   `Module`, or `NULL` to leave that module unchanged.
#' @param ... Additional arguments passed to `.fn`.
#' @param path A stable path returned by [module_graph()] or [named_modules()].
#' @param replacement A replacement `Module`.
#' @param shared Whether replacement affects only `path` or every reference to
#'   the same R6 object.
#' @param paths One or more graph paths to freeze or unfreeze.
#' @param recursive Whether freezing applies to every descendant by identity.
#' @param frozen Logical; `TRUE` freezes and `FALSE` unfreezes.
#' @param module A `Module` object.
#' @param chat An ellmer `Chat` object, or `NULL` to clear stored Chats.
#' @param clone Whether to give each module an independent deep clone of
#'   `chat`. The default avoids sharing mutable conversation history.
#'
#' @return
#' * `module_graph()` returns a tibble with one row per graph occurrence.
#' * `named_modules()` and `named_parameters()` return named lists.
#' * `map_modules()` and `replace_module()` return the resulting root module.
#' * `freeze_modules()` and `set_module_lm()` return `program` invisibly.
#' * `is_module_frozen()` returns one logical value.
#' * `module_children()` returns a list whose leaves are child modules.
#'
#' @examples
#' first <- module(signature("text -> answer"))
#' second <- module(signature("answer -> summary"))
#' program <- pipeline(first = first, second = second)
#'
#' names(named_modules(program))
#' names(named_parameters(program))
#'
#' program <- map_modules(program, function(module, path) {
#'   module$config$graph_path <- path
#'   module
#' })
#'
#' freeze_modules(program, "$/steps/first")
#' is_module_frozen(first)
#'
#' @name module-graph
NULL

#' @rdname module-graph
#' @export
module_graph <- function(
  program,
  boundaries = c("cross", "respect"),
  cycles = c("record", "error")
) {
  boundaries <- match.arg(boundaries)
  cycles <- match.arg(cycles)
  walk <- module_graph_walk(program, boundaries = boundaries, cycles = cycles)
  module_graph_tibble(walk$rows)
}

#' @rdname module-graph
#' @export
named_modules <- function(
  program,
  include_root = TRUE,
  aliases = FALSE,
  boundaries = c("cross", "respect"),
  cycles = c("record", "error")
) {
  boundaries <- match.arg(boundaries)
  cycles <- match.arg(cycles)
  walk <- module_graph_walk(program, boundaries = boundaries, cycles = cycles)
  rows <- walk$rows

  keep <- vapply(
    rows,
    function(row) {
      root_ok <- include_root || !identical(row$path, "$")
      alias_ok <- aliases || (!row$shared && !row$cycle)
      root_ok && alias_ok
    },
    logical(1)
  )
  rows <- rows[keep]

  modules <- lapply(rows, `[[`, "module")
  names(modules) <- vapply(rows, `[[`, character(1), "path")
  modules
}

#' @rdname module-graph
#' @export
named_parameters <- function(
  program,
  include_root = TRUE,
  boundaries = c("respect", "cross")
) {
  boundaries <- match.arg(boundaries)
  walk <- module_graph_walk(program, boundaries = "cross", cycles = "record")
  rows <- walk$rows

  keep <- vapply(
    rows,
    function(row) {
      canonical <- !row$shared && !row$cycle
      root_ok <- include_root || !identical(row$path, "$")
      boundary_ok <- boundaries == "cross" || !row$protected
      canonical &&
        root_ok &&
        boundary_ok &&
        module_graph_is_parameter(row$module)
    },
    logical(1)
  )
  rows <- rows[keep]

  parameters <- lapply(rows, `[[`, "module")
  names(parameters) <- vapply(rows, `[[`, character(1), "path")
  parameters
}

#' @rdname module-graph
#' @export
map_modules <- function(
  program,
  .fn,
  ...,
  include_root = TRUE,
  boundaries = c("respect", "cross")
) {
  module_graph_check_program(program)
  if (!is.function(.fn)) {
    cli::cli_abort("{.arg .fn} must be a function")
  }
  boundaries <- match.arg(boundaries)

  walk <- module_graph_walk(program, boundaries = "cross", cycles = "record")
  rows <- walk$rows
  canonical <- rows[vapply(
    rows,
    function(row) !row$shared && !row$cycle,
    logical(1)
  )]
  cycle_ids <- unique(vapply(
    rows[vapply(rows, `[[`, logical(1), "cycle")],
    `[[`,
    character(1),
    "id"
  ))
  dots <- list(...)
  root <- program
  transactions <- list()

  map_error <- tryCatch(
    {
      for (row in rev(canonical)) {
        if (boundaries == "respect" && row$protected) {
          next
        }

        # Descendants have already been transformed, so refresh this composite
        # exactly once before exposing it to the callback.
        module_graph_refresh(row$module)
        if (!include_root && identical(row$path, "$")) {
          next
        }

        replacement <- do.call(
          .fn,
          c(list(module = row$module, path = row$path), dots)
        )
        if (is.null(replacement) || identical(replacement, row$module)) {
          next
        }
        if (!inherits(replacement, "Module")) {
          cli::cli_abort(c(
            "{.arg .fn} must return a Module or NULL",
            "x" = "Path {.val {row$path}} returned {.cls {class(replacement)[1]}}."
          ))
        }
        if (row$id %in% cycle_ids) {
          cli::cli_abort(
            c(
              "Cannot replace a module that participates in a graph cycle",
              "x" = "Path {.val {row$path}} is part of a cycle.",
              "i" = "Mutate the module in place or use {.fn replace_module} on one edge."
            ),
            class = "dsprrr_module_graph_cycle_replacement"
          )
        }

        occurrences <- rows[vapply(
          rows,
          function(candidate) identical(candidate$id, row$id),
          logical(1)
        )]
        transaction <- module_graph_apply_replacement(
          occurrences,
          replacement
        )
        transactions[[length(transactions) + 1L]] <- transaction
        if (identical(row$path, "$")) {
          root <- replacement
        }
      }
      NULL
    },
    error = function(e) e
  )

  if (inherits(map_error, "condition")) {
    module_graph_rollback_transactions(transactions)
    if (length(transactions) > 0L) {
      try(module_graph_refresh_all(program), silent = TRUE)
    }
    cli::cli_abort(
      "Module graph mapping failed; structural replacements were rolled back",
      parent = map_error,
      class = "dsprrr_module_graph_map_error"
    )
  }

  root
}

#' @rdname module-graph
#' @export
replace_module <- function(
  program,
  path,
  replacement,
  shared = c("path", "all"),
  boundaries = c("respect", "cross")
) {
  module_graph_check_program(program)
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    cli::cli_abort("{.arg path} must be one graph path")
  }
  if (!inherits(replacement, "Module")) {
    cli::cli_abort("{.arg replacement} must be a Module")
  }
  shared <- match.arg(shared)
  boundaries <- match.arg(boundaries)

  walk <- module_graph_walk(program, boundaries = "cross", cycles = "record")
  rows <- walk$rows
  matches <- rows[vapply(
    rows,
    function(row) identical(row$path, path),
    logical(1)
  )]
  if (length(matches) == 0L) {
    cli::cli_abort(
      c(
        "Unknown module graph path {.val {path}}",
        "i" = "Inspect valid paths with {.fn module_graph}."
      ),
      class = "dsprrr_module_graph_unknown_path"
    )
  }
  target <- matches[[1]]

  occurrences <- if (shared == "all") {
    rows[vapply(
      rows,
      function(row) identical(row$id, target$id),
      logical(1)
    )]
  } else {
    matches
  }

  if (
    boundaries == "respect" &&
      any(vapply(occurrences, `[[`, logical(1), "protected"))
  ) {
    cli::cli_abort(
      c(
        "Cannot replace a module across a frozen or compiled boundary",
        "x" = "Protected path: {.val {path}}",
        "i" = "Use {.code boundaries = \"cross\"} only when replacing that boundary is intentional."
      ),
      class = "dsprrr_module_graph_protected"
    )
  }

  transaction <- module_graph_apply_replacement(occurrences, replacement)

  result <- if (
    any(vapply(
      occurrences,
      function(occurrence) identical(occurrence$path, "$"),
      logical(1)
    ))
  ) {
    replacement
  } else {
    program
  }
  refresh_error <- tryCatch(
    {
      module_graph_refresh_all(result)
      NULL
    },
    error = function(e) e
  )
  if (inherits(refresh_error, "condition")) {
    module_graph_rollback_transactions(list(transaction))
    try(module_graph_refresh_all(program), silent = TRUE)
    cli::cli_abort(
      "Module replacement failed validation and was rolled back",
      parent = refresh_error,
      class = "dsprrr_module_graph_replace_error"
    )
  }
  result
}

#' @rdname module-graph
#' @export
freeze_modules <- function(
  program,
  paths = "$",
  recursive = TRUE,
  frozen = TRUE
) {
  module_graph_check_program(program)
  if (!is.character(paths) || anyNA(paths)) {
    cli::cli_abort("{.arg paths} must be a character vector of graph paths")
  }
  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    cli::cli_abort("{.arg recursive} must be TRUE or FALSE")
  }
  if (!is.logical(frozen) || length(frozen) != 1L || is.na(frozen)) {
    cli::cli_abort("{.arg frozen} must be TRUE or FALSE")
  }

  walk <- module_graph_walk(program, boundaries = "cross", cycles = "record")
  rows <- walk$rows
  selected <- list()
  for (path in paths) {
    matches <- rows[vapply(
      rows,
      function(row) identical(row$path, path),
      logical(1)
    )]
    if (length(matches) == 0L) {
      cli::cli_abort(
        c(
          "Unknown module graph path {.val {path}}",
          "i" = "Inspect valid paths with {.fn module_graph}."
        ),
        class = "dsprrr_module_graph_unknown_path"
      )
    }
    selected[[length(selected) + 1L]] <- matches[[1]]$module
  }

  modules <- list()
  for (module in selected) {
    descendants <- if (recursive) {
      module_graph_unique_modules(module)
    } else {
      list(module)
    }
    modules <- c(modules, descendants)
  }
  modules <- module_graph_dedupe_modules(modules)
  for (module in modules) {
    graph_config <- module$config$.module_graph %||% list()
    graph_config$frozen <- frozen
    module$config$.module_graph <- graph_config
  }

  invisible(program)
}

#' @rdname module-graph
#' @export
is_module_frozen <- function(module) {
  module_graph_check_program(module)
  isTRUE(module$config$.module_graph$frozen)
}

#' @rdname module-graph
#' @export
set_module_lm <- function(
  program,
  chat,
  include_root = TRUE,
  boundaries = c("cross", "respect"),
  clone = TRUE
) {
  module_graph_check_program(program)
  if (!is.null(chat) && !inherits(chat, "Chat")) {
    cli::cli_abort("{.arg chat} must be an ellmer Chat object or NULL")
  }
  if (!is.logical(clone) || length(clone) != 1L || is.na(clone)) {
    cli::cli_abort("{.arg clone} must be TRUE or FALSE")
  }
  boundaries <- match.arg(boundaries)
  walk <- module_graph_walk(program, boundaries = "cross", cycles = "record")

  for (row in walk$rows) {
    if (row$shared || row$cycle) {
      next
    }
    if (!include_root && identical(row$path, "$")) {
      next
    }
    if (boundaries == "respect" && row$protected) {
      next
    }
    row$module$chat <- if (clone) module_graph_clone_chat(chat) else chat
  }

  invisible(program)
}

module_graph_clone_chat <- function(chat) {
  if (is.null(chat)) {
    return(NULL)
  }
  clone <- tryCatch(chat$clone, error = function(e) NULL)
  if (!is.function(clone)) {
    cli::cli_abort(
      c(
        "Cannot propagate an independent Chat clone",
        "x" = "The Chat does not implement clone().",
        "i" = "Use {.code clone = FALSE} only when shared mutable state is intentional."
      ),
      class = "dsprrr_module_graph_chat_clone"
    )
  }
  result <- tryCatch(
    clone(deep = TRUE),
    error = function(e) {
      cli::cli_abort(
        "Failed to clone Chat during graph propagation",
        parent = e,
        class = "dsprrr_module_graph_chat_clone"
      )
    }
  )
  if (
    is.null(result) ||
      identical(rlang::obj_address(result), rlang::obj_address(chat))
  ) {
    cli::cli_abort(
      "Chat clone() did not return an independent object",
      class = "dsprrr_module_graph_chat_clone"
    )
  }
  result
}

#' Return the child structure declared by a module
#'
#' This is primarily useful when implementing or testing a custom module graph
#' adapter. Custom modules should normally implement `graph_children()` rather
#' than call this function from that method.
#'
#' @param module A dsprrr `Module` object.
#' @rdname module-graph
#' @export
module_children <- function(module) {
  module_graph_check_program(module)
  module_graph_child_adapter(module)$children
}

module_graph_check_program <- function(program) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("{.arg program} must be a dsprrr Module")
  }
  invisible(program)
}

module_graph_child_adapter <- function(module) {
  custom_getter <- tryCatch(module$graph_children, error = function(e) NULL)
  if (is.function(custom_getter)) {
    children <- tryCatch(
      custom_getter(),
      error = function(e) {
        cli::cli_abort(
          "graph_children() failed for {.cls {class(module)[1]}}",
          parent = e,
          class = "dsprrr_module_graph_hook_error"
        )
      }
    )
    module_graph_validate_children(children)
    custom_setter <- tryCatch(
      module$set_graph_children,
      error = function(e) NULL
    )
    custom_validator <- tryCatch(
      module$validate_graph_children,
      error = function(e) NULL
    )
    validator <- function(value) {
      module_graph_validate_children(value)
      if (is.function(custom_validator)) {
        custom_validator(value)
      }
      invisible(value)
    }
    setter <- if (is.function(custom_setter)) {
      function(value) {
        validator(value)
        custom_setter(value)
        invisible(module)
      }
    } else {
      NULL
    }
    return(list(
      children = children,
      set = setter,
      validate = validator,
      custom = TRUE
    ))
  }

  if (inherits(module, "PipelineModule")) {
    steps <- lapply(module$steps, function(step) step@module)
    names(steps) <- names(module$steps)
    prepare <- function(value) {
      modules <- value$steps
      if (!is.list(modules) || length(modules) != length(module$steps)) {
        cli::cli_abort("Pipeline graph replacement changed the step structure")
      }
      if (!all(vapply(modules, inherits, logical(1), "Module"))) {
        cli::cli_abort("Every pipeline graph child must be a Module")
      }
      updated_steps <- module$steps
      for (i in seq_along(module$steps)) {
        step <- updated_steps[[i]]
        step@module <- modules[[i]]
        updated_steps[[i]] <- step
      }
      refreshed <- PipelineModule$new(
        steps = updated_steps,
        config = module$config,
        chat = module$chat
      )
      list(steps = updated_steps, signature = refreshed$signature)
    }
    validator <- function(value) {
      prepare(value)
      invisible(value)
    }
    setter <- function(value) {
      prepared <- prepare(value)
      module$steps <- prepared$steps
      module$signature <- prepared$signature
      invisible(module)
    }
    return(list(
      children = list(steps = steps),
      set = setter,
      validate = validator,
      custom = FALSE
    ))
  }

  if (inherits(module, "EnsembleModule")) {
    validator <- function(value) {
      modules <- value$modules
      if (!is.list(modules) || length(modules) != length(module$modules)) {
        cli::cli_abort(
          "Ensemble graph replacement changed the module structure"
        )
      }
      if (!all(vapply(modules, inherits, logical(1), "Module"))) {
        cli::cli_abort("Every ensemble graph child must be a Module")
      }
      validate_signature_compatibility(modules)
      invisible(value)
    }
    setter <- function(value) {
      validator(value)
      module$modules <- value$modules
      module$signature <- module$modules[[1]]$signature
      invisible(module)
    }
    return(list(
      children = list(modules = module$modules),
      set = setter,
      validate = validator,
      custom = FALSE
    ))
  }

  if (inherits(module, "MultiChainComparisonModule")) {
    validator <- function(value) {
      if (!inherits(value$inner_module, "Module")) {
        cli::cli_abort("Multi-chain graph child must be a Module")
      }
      invisible(value)
    }
    setter <- function(value) {
      validator(value)
      module$inner_module <- value$inner_module
      module$signature <- module$inner_module$signature
      invisible(module)
    }
    return(list(
      children = list(inner_module = module$inner_module),
      set = setter,
      validate = validator,
      custom = FALSE
    ))
  }

  single_child_classes <- c(
    "AssertModule",
    "BestOfNModule",
    "RefineModule",
    "KNNFewShotModule"
  )
  if (
    any(vapply(
      single_child_classes,
      function(class) inherits(module, class),
      logical(1)
    ))
  ) {
    validator <- function(value) {
      if (!inherits(value$module, "Module")) {
        cli::cli_abort("Wrapper graph child must be a Module")
      }
      invisible(value)
    }
    setter <- function(value) {
      validator(value)
      module$module <- value$module
      module$signature <- module$module$signature
      invisible(module)
    }
    return(list(
      children = list(module = module$module),
      set = setter,
      validate = validator,
      custom = FALSE
    ))
  }

  list(
    children = list(),
    set = NULL,
    validate = module_graph_validate_children,
    custom = FALSE
  )
}

module_graph_validate_children <- function(children) {
  if (!is.list(children)) {
    cli::cli_abort(
      c(
        "graph_children() must return a list",
        "x" = "Returned {.cls {class(children)[1]}}."
      ),
      class = "dsprrr_module_graph_hook_error"
    )
  }

  validate <- function(value, path = "children") {
    if (is.null(value) || inherits(value, "Module")) {
      return(invisible(NULL))
    }
    if (!is.list(value)) {
      cli::cli_abort(
        c(
          "graph_children() may contain only Modules, lists, or NULL",
          "x" = "{path} contains {.cls {class(value)[1]}}."
        ),
        class = "dsprrr_module_graph_hook_error"
      )
    }
    for (i in seq_along(value)) {
      validate(value[[i]], paste0(path, "[[", i, "]]"))
    }
    invisible(NULL)
  }

  validate(children)
  invisible(children)
}

module_graph_child_edges <- function(module, parent_path) {
  adapter <- module_graph_child_adapter(module)
  edges <- list()

  visit <- function(value, path, indices) {
    if (is.null(value)) {
      return(invisible(NULL))
    }
    if (inherits(value, "Module")) {
      index_path <- indices
      parent <- module
      setter <- if (is.function(adapter$set)) {
        function(replacement) {
          current <- module_graph_child_adapter(parent)
          updated <- module_graph_set_at(
            current$children,
            index_path,
            replacement
          )
          if (!is.function(current$set)) {
            cli::cli_abort(
              "Module graph child became read-only during replacement",
              class = "dsprrr_module_graph_read_only"
            )
          }
          current$set(updated)
          invisible(parent)
        }
      } else {
        NULL
      }
      edges[[length(edges) + 1L]] <<- list(
        module = value,
        path = path,
        setter = setter,
        parent_module = parent,
        index_path = index_path
      )
      return(invisible(NULL))
    }

    segments <- module_graph_list_segments(value)
    for (i in seq_along(value)) {
      visit(
        value[[i]],
        module_graph_append_path(path, segments[[i]]),
        c(indices, i)
      )
    }
    invisible(NULL)
  }

  children <- adapter$children
  segments <- module_graph_list_segments(children)
  for (i in seq_along(children)) {
    visit(
      children[[i]],
      module_graph_append_path(parent_path, segments[[i]]),
      i
    )
  }
  edges
}

module_graph_set_at <- function(value, indices, replacement) {
  if (length(indices) == 0L) {
    return(replacement)
  }
  index <- indices[[1]]
  if (!is.list(value) || index > length(value)) {
    cli::cli_abort(
      "Module graph child structure changed during replacement",
      class = "dsprrr_module_graph_structure_changed"
    )
  }
  value[[index]] <- module_graph_set_at(
    value[[index]],
    indices[-1],
    replacement
  )
  value
}

module_graph_get_at <- function(value, indices) {
  for (index in indices) {
    if (!is.list(value) || index > length(value)) {
      cli::cli_abort(
        "Module graph child structure changed during replacement",
        class = "dsprrr_module_graph_structure_changed"
      )
    }
    value <- value[[index]]
  }
  value
}

module_graph_apply_replacement <- function(occurrences, replacement) {
  writable <- occurrences[vapply(
    occurrences,
    function(occurrence) !identical(occurrence$path, "$"),
    logical(1)
  )]
  if (length(writable) == 0L) {
    return(list(snapshots = list()))
  }

  read_only <- writable[
    !vapply(
      writable,
      function(occurrence) {
        !is.null(occurrence$parent_module) && is.function(occurrence$setter)
      },
      logical(1)
    )
  ]
  if (length(read_only) > 0L) {
    cli::cli_abort(
      c(
        "Module graph path {.val {read_only[[1]]$path}} is read-only",
        "i" = "Custom programs must implement set_graph_children(children)."
      ),
      class = "dsprrr_module_graph_read_only"
    )
  }

  parent_ids <- vapply(
    writable,
    function(occurrence) rlang::obj_address(occurrence$parent_module),
    character(1)
  )
  snapshots <- list()

  for (parent_id in unique(parent_ids)) {
    group <- writable[parent_ids == parent_id]
    parent <- group[[1]]$parent_module
    adapter <- module_graph_child_adapter(parent)
    if (!is.function(adapter$set)) {
      cli::cli_abort(
        "Module graph parent became read-only during replacement",
        class = "dsprrr_module_graph_read_only"
      )
    }

    original <- adapter$children
    updated <- original
    for (occurrence in group) {
      current <- module_graph_get_at(updated, occurrence$index_path)
      if (!inherits(current, "Module")) {
        cli::cli_abort(
          "Module graph child structure changed during replacement",
          class = "dsprrr_module_graph_structure_changed"
        )
      }
      current_id <- rlang::obj_address(current)
      replacement_id <- rlang::obj_address(replacement)
      if (!current_id %in% c(occurrence$id, replacement_id)) {
        cli::cli_abort(
          "Module graph child identity changed during replacement",
          class = "dsprrr_module_graph_structure_changed"
        )
      }
      updated <- module_graph_set_at(
        updated,
        occurrence$index_path,
        replacement
      )
    }

    adapter$validate(updated)
    snapshots[[length(snapshots) + 1L]] <- list(
      parent = parent,
      original = original,
      updated = updated
    )
  }

  commit_error <- tryCatch(
    {
      for (snapshot in snapshots) {
        adapter <- module_graph_child_adapter(snapshot$parent)
        adapter$set(snapshot$updated)
      }
      NULL
    },
    error = function(e) e
  )
  transaction <- list(snapshots = snapshots)
  if (inherits(commit_error, "condition")) {
    module_graph_rollback_transactions(list(transaction))
    cli::cli_abort(
      "Module graph replacement could not be applied and was rolled back",
      parent = commit_error,
      class = "dsprrr_module_graph_replace_error"
    )
  }

  transaction
}

module_graph_rollback_transactions <- function(transactions) {
  rollback_errors <- list()
  for (transaction in rev(transactions)) {
    for (snapshot in rev(transaction$snapshots)) {
      error <- tryCatch(
        {
          adapter <- module_graph_child_adapter(snapshot$parent)
          adapter$set(snapshot$original)
          NULL
        },
        error = function(e) e
      )
      if (inherits(error, "condition")) {
        rollback_errors[[length(rollback_errors) + 1L]] <- error
      }
    }
  }
  if (length(rollback_errors) > 0L) {
    cli::cli_abort(
      "Module graph rollback failed; the graph may be inconsistent",
      parent = rollback_errors[[1]],
      class = "dsprrr_module_graph_rollback_error"
    )
  }
  invisible(NULL)
}

module_graph_list_segments <- function(value) {
  if (length(value) == 0L) {
    return(character())
  }
  item_names <- names(value)
  use_names <- !is.null(item_names) &&
    all(!is.na(item_names) & nzchar(item_names)) &&
    !anyDuplicated(item_names)
  if (use_names) item_names else as.character(seq_along(value))
}

module_graph_append_path <- function(path, segment) {
  segment <- gsub("~", "~0", segment, fixed = TRUE)
  segment <- gsub("/", "~1", segment, fixed = TRUE)
  paste0(path, "/", segment)
}

module_graph_walk <- function(
  program,
  boundaries = c("cross", "respect"),
  cycles = c("record", "error")
) {
  module_graph_check_program(program)
  boundaries <- match.arg(boundaries)
  cycles <- match.arg(cycles)
  seen <- new.env(parent = emptyenv(), hash = TRUE)
  rows <- list()

  visit <- function(
    module,
    path,
    parent_path,
    setter,
    parent_module,
    index_path,
    active,
    ancestor_protected,
    depth
  ) {
    id <- rlang::obj_address(module)
    cycle <- id %in% active
    shared <- exists(id, envir = seen, inherits = FALSE)
    canonical_path <- if (shared) get(id, envir = seen) else path
    frozen <- is_module_frozen_internal(module)
    compiled <- is_module_compiled_internal(module)
    path_protected <- ancestor_protected || frozen || compiled

    rows[[length(rows) + 1L]] <<- list(
      path = path,
      parent_path = parent_path,
      depth = depth,
      module = module,
      class = class(module)[1],
      id = id,
      canonical_path = canonical_path,
      shared = shared,
      cycle = cycle,
      frozen = frozen,
      compiled = compiled,
      ancestor_protected = ancestor_protected,
      path_protected = path_protected,
      protected = path_protected,
      setter = setter,
      parent_module = parent_module,
      index_path = index_path
    )

    if (cycle) {
      if (cycles == "error") {
        cli::cli_abort(
          c(
            "Cycle detected in module graph",
            "x" = "Path {.val {path}} points back to {.val {canonical_path}}."
          ),
          class = "dsprrr_module_graph_cycle"
        )
      }
      return(invisible(NULL))
    }
    if (shared) {
      return(invisible(NULL))
    }

    assign(id, path, envir = seen)
    edges <- module_graph_child_edges(module, path)
    for (edge in edges) {
      visit(
        module = edge$module,
        path = edge$path,
        parent_path = path,
        setter = edge$setter,
        parent_module = edge$parent_module,
        index_path = edge$index_path,
        active = c(active, id),
        ancestor_protected = path_protected,
        depth = depth + 1L
      )
    }
    invisible(NULL)
  }

  visit(
    module = program,
    path = "$",
    parent_path = NA_character_,
    setter = NULL,
    parent_module = NULL,
    index_path = integer(),
    active = character(),
    ancestor_protected = FALSE,
    depth = 0L
  )

  protected_roots <- module_graph_dedupe_modules(lapply(
    rows[vapply(rows, `[[`, logical(1), "path_protected")],
    `[[`,
    "module"
  ))
  protected_ids <- unique(unlist(
    lapply(
      protected_roots,
      function(module) {
        vapply(
          module_graph_unique_modules(module),
          rlang::obj_address,
          character(1)
        )
      }
    ),
    use.names = FALSE
  ))
  if (length(protected_ids) > 0L) {
    rows <- lapply(rows, function(row) {
      row$protected <- row$id %in% protected_ids
      row
    })
  }

  if (boundaries == "respect") {
    rows <- rows[!vapply(rows, `[[`, logical(1), "ancestor_protected")]
  }

  list(rows = rows)
}

module_graph_tibble <- function(rows) {
  boundary <- vapply(
    rows,
    function(row) {
      states <- c(if (row$frozen) "frozen", if (row$compiled) "compiled")
      if (length(states) == 0L) NA_character_ else paste(states, collapse = "+")
    },
    character(1)
  )

  tibble::tibble(
    path = vapply(rows, `[[`, character(1), "path"),
    parent_path = vapply(rows, `[[`, character(1), "parent_path"),
    depth = vapply(rows, `[[`, integer(1), "depth"),
    class = vapply(rows, `[[`, character(1), "class"),
    id = vapply(rows, `[[`, character(1), "id"),
    canonical_path = vapply(rows, `[[`, character(1), "canonical_path"),
    shared = vapply(rows, `[[`, logical(1), "shared"),
    cycle = vapply(rows, `[[`, logical(1), "cycle"),
    frozen = vapply(rows, `[[`, logical(1), "frozen"),
    compiled = vapply(rows, `[[`, logical(1), "compiled"),
    protected = vapply(rows, `[[`, logical(1), "protected"),
    boundary = boundary,
    module = lapply(rows, `[[`, "module")
  )
}

module_graph_is_parameter <- function(module) {
  hook <- tryCatch(module$graph_is_parameter, error = function(e) NULL)
  if (is.function(hook)) {
    value <- hook()
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort(
        "graph_is_parameter() must return TRUE or FALSE",
        class = "dsprrr_module_graph_hook_error"
      )
    }
    return(value)
  }

  has_children <- length(module_graph_child_edges(module, "$")) > 0L
  apply_params <- tryCatch(
    module$apply_optimization_params,
    error = function(e) NULL
  )
  !has_children && is.function(apply_params)
}

is_module_frozen_internal <- function(module) {
  isTRUE(module$config$.module_graph$frozen)
}

is_module_compiled_internal <- function(module) {
  checker <- tryCatch(module$is_compiled, error = function(e) NULL)
  checked <- if (is.function(checker)) {
    tryCatch(checker(), error = function(e) FALSE)
  } else {
    FALSE
  }
  isTRUE(checked) ||
    isTRUE(module$state$compiled) ||
    isTRUE(module$config$compiled)
}

module_graph_unique_modules <- function(program) {
  module_graph_check_program(program)
  seen <- new.env(parent = emptyenv(), hash = TRUE)
  modules <- list()

  visit <- function(module) {
    id <- rlang::obj_address(module)
    if (exists(id, envir = seen, inherits = FALSE)) {
      return(invisible(NULL))
    }
    assign(id, TRUE, envir = seen)
    modules[[length(modules) + 1L]] <<- module
    for (edge in module_graph_child_edges(module, "$")) {
      visit(edge$module)
    }
    invisible(NULL)
  }

  visit(program)
  modules
}

module_graph_dedupe_modules <- function(modules) {
  if (length(modules) == 0L) {
    return(list())
  }
  ids <- vapply(modules, rlang::obj_address, character(1))
  modules[!duplicated(ids)]
}

module_graph_refresh <- function(module) {
  hook <- tryCatch(module$graph_children_changed, error = function(e) NULL)
  if (is.function(hook)) {
    hook()
    return(invisible(module))
  }

  if (inherits(module, "PipelineModule")) {
    refreshed <- PipelineModule$new(
      steps = module$steps,
      config = module$config,
      chat = module$chat
    )
    module$signature <- refreshed$signature
    return(invisible(module))
  }

  if (inherits(module, "EnsembleModule")) {
    validate_signature_compatibility(module$modules)
    module$signature <- module$modules[[1]]$signature
    return(invisible(module))
  }

  if (inherits(module, "MultiChainComparisonModule")) {
    module$signature <- module$inner_module$signature
    return(invisible(module))
  }

  single_child_classes <- c(
    "AssertModule",
    "BestOfNModule",
    "RefineModule",
    "KNNFewShotModule"
  )
  if (
    any(vapply(
      single_child_classes,
      function(class) inherits(module, class),
      logical(1)
    ))
  ) {
    module$signature <- module$module$signature
  }

  invisible(module)
}

module_graph_refresh_all <- function(program) {
  walk <- module_graph_walk(program, boundaries = "cross", cycles = "record")
  canonical <- walk$rows[vapply(
    walk$rows,
    function(row) !row$shared && !row$cycle,
    logical(1)
  )]
  for (row in rev(canonical)) {
    module_graph_refresh(row$module)
  }
  invisible(program)
}
