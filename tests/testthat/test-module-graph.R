graph_test_module <- function(input = "text", output = "answer") {
  module(signature(paste0(input, " -> ", output)))
}

GraphTestProgram <- R6::R6Class(
  "GraphTestProgram",
  inherit = dsprrr:::Module,
  public = list(
    children = NULL,
    parameter = NULL,
    refreshes = 0L,
    initialize = function(children = list(), parameter = FALSE) {
      super$initialize(signature("text -> answer"))
      self$children <- children
      self$parameter <- parameter
    },
    graph_children = function() self$children,
    set_graph_children = function(children) {
      self$children <- children
      invisible(self)
    },
    graph_children_changed = function() {
      self$refreshes <- self$refreshes + 1L
      invisible(self)
    },
    graph_is_parameter = function() self$parameter,
    forward = function(...) {
      cli::cli_abort("GraphTestProgram is traversal-only")
    }
  )
)

GraphReadOnlyProgram <- R6::R6Class(
  "GraphReadOnlyProgram",
  inherit = dsprrr:::Module,
  public = list(
    children = NULL,
    initialize = function(children = list()) {
      super$initialize(signature("text -> answer"))
      self$children <- children
    },
    graph_children = function() self$children,
    forward = function(...) {
      cli::cli_abort("GraphReadOnlyProgram is traversal-only")
    }
  )
)

test_that("built-in composites expose stable named module paths", {
  first <- graph_test_module("text", "answer")
  second <- graph_test_module("answer", "summary")
  fallback <- graph_test_module("text", "answer")
  program <- ensemble(list(
    primary = best_of_n(
      pipeline(first = first, second = second),
      N = 2
    ),
    fallback = fallback
  ))

  graph <- module_graph(program)
  expect_identical(
    graph$path,
    c(
      "$",
      "$/modules/primary",
      "$/modules/primary/module",
      "$/modules/primary/module/steps/first",
      "$/modules/primary/module/steps/second",
      "$/modules/fallback"
    )
  )
  expect_identical(
    names(named_parameters(program)),
    c(
      "$/modules/primary/module/steps/first",
      "$/modules/primary/module/steps/second",
      "$/modules/fallback"
    )
  )
})

test_that("shared modules and cycles are detected by identity", {
  shared <- graph_test_module()
  ensemble_program <- ensemble(list(left = shared, right = shared))
  graph <- module_graph(ensemble_program)

  expect_identical(graph$shared, c(FALSE, FALSE, TRUE))
  expect_identical(graph$canonical_path[[3]], "$/modules/left")
  expect_identical(length(named_modules(ensemble_program)), 2L)
  expect_identical(
    length(named_modules(ensemble_program, aliases = TRUE)),
    3L
  )

  cyclic <- GraphTestProgram$new()
  cyclic$children <- list(self = cyclic)
  cycle_graph <- module_graph(cyclic)
  expect_identical(cycle_graph$path, c("$", "$/self"))
  expect_identical(cycle_graph$cycle, c(FALSE, TRUE))
  expect_snapshot(
    module_graph(cyclic, cycles = "error"),
    error = TRUE
  )

  visited <- character()
  map_modules(cyclic, function(module, path) {
    visited <<- c(visited, path)
    module
  })
  expect_identical(visited, "$")
})

test_that("custom hooks support named and unnamed lists and replacement", {
  first <- graph_test_module()
  second <- graph_test_module()
  third <- graph_test_module()
  replacement <- graph_test_module()
  program <- GraphTestProgram$new(list(
    named = list(primary = first),
    unnamed = list(second, third)
  ))

  expect_identical(
    module_graph(program)$path,
    c("$", "$/named/primary", "$/unnamed/1", "$/unnamed/2")
  )

  result <- replace_module(program, "$/unnamed/2", replacement)
  expect_identical(result, program)
  expect_identical(program$children$unnamed[[2]], replacement)
  expect_identical(program$refreshes, 1L)

  custom_parameter <- GraphTestProgram$new(parameter = TRUE)
  expect_identical(names(named_parameters(custom_parameter)), "$")

  escaped <- GraphTestProgram$new(stats::setNames(
    list(graph_test_module(), graph_test_module()),
    c("a/b", "c~d")
  ))
  expect_identical(
    module_graph(escaped)$path,
    c("$", "$/a~1b", "$/c~0d")
  )
})

test_that("mapping transforms shared children once and preserves sharing", {
  shared <- graph_test_module()
  replacement <- graph_test_module()
  program <- GraphTestProgram$new(list(left = shared, right = shared))
  calls <- 0L

  result <- map_modules(
    program,
    function(module, path) {
      calls <<- calls + 1L
      replacement
    },
    include_root = FALSE
  )

  expect_identical(calls, 1L)
  expect_identical(result$children$left, replacement)
  expect_identical(result$children$right, replacement)
  expect_identical(result$refreshes, 1L)
})

test_that("shared replacement preflights every writable alias", {
  shared <- graph_test_module()
  replacement <- graph_test_module()
  writable <- ensemble(list(shared))
  read_only <- GraphReadOnlyProgram$new(list(alias = shared))
  program <- GraphTestProgram$new(list(
    writable = writable,
    read_only = read_only
  ))

  expect_snapshot(
    map_modules(program, function(module, path) {
      if (identical(module, shared)) replacement else module
    }),
    error = TRUE
  )
  expect_identical(writable$modules[[1]], shared)
  expect_identical(read_only$children$alias, shared)
  expect_identical(program$refreshes, 0L)

  expect_snapshot(
    replace_module(
      program,
      "$/writable/modules/1",
      replacement,
      shared = "all"
    ),
    error = TRUE
  )
  expect_identical(writable$modules[[1]], shared)
  expect_identical(read_only$children$alias, shared)
})

test_that("incompatible composite replacement is atomic", {
  left <- graph_test_module("text", "answer")
  right <- graph_test_module("text", "answer")
  incompatible <- graph_test_module("question", "answer")
  program <- ensemble(list(left = left, right = right))

  expect_snapshot(
    replace_module(program, "$/modules/left", incompatible),
    error = TRUE
  )
  expect_identical(program$modules$left, left)
  expect_identical(
    vapply(program$signature@inputs, function(input) input$name, character(1)),
    "text"
  )

  wrapped_left <- best_of_n(left, N = 2)
  wrapped_right <- best_of_n(right, N = 2)
  nested <- ensemble(list(left = wrapped_left, right = wrapped_right))
  expect_snapshot(
    replace_module(nested, "$/modules/left/module", incompatible),
    error = TRUE
  )
  expect_identical(wrapped_left$module, left)
  expect_identical(
    vapply(nested$signature@inputs, function(input) input$name, character(1)),
    "text"
  )
})

test_that("mapping rolls back earlier structural replacements on failure", {
  left <- graph_test_module("text", "answer")
  right <- graph_test_module("text", "answer")
  compatible <- graph_test_module("text", "answer")
  incompatible <- graph_test_module("question", "answer")
  program <- ensemble(list(left = left, right = right))

  expect_snapshot(
    map_modules(program, function(module, path) {
      if (identical(path, "$/modules/right")) {
        compatible
      } else if (identical(path, "$/modules/left")) {
        incompatible
      } else {
        module
      }
    }),
    error = TRUE
  )
  expect_identical(program$modules$left, left)
  expect_identical(program$modules$right, right)
})

test_that("path and identity replacement have deterministic shared semantics", {
  shared <- graph_test_module()
  path_replacement <- graph_test_module()
  all_replacement <- graph_test_module()
  program <- ensemble(list(left = shared, right = shared))

  replace_module(program, "$/modules/left", path_replacement)
  expect_identical(program$modules$left, path_replacement)
  expect_identical(program$modules$right, shared)

  program <- ensemble(list(left = shared, right = shared))
  replace_module(
    program,
    "$/modules/left",
    all_replacement,
    shared = "all"
  )
  expect_identical(program$modules$left, all_replacement)
  expect_identical(program$modules$right, all_replacement)
})

test_that("frozen and compiled modules form deterministic boundaries", {
  leaf <- graph_test_module()
  compiled <- best_of_n(leaf, N = 2)
  outer <- best_of_n(compiled, N = 2)
  compiled$config$compiled <- TRUE

  expect_identical(
    module_graph(outer, boundaries = "respect")$path,
    c("$", "$/module")
  )
  expect_identical(
    module_graph(outer, boundaries = "cross")$path,
    c("$", "$/module", "$/module/module")
  )
  expect_identical(names(named_parameters(outer)), character())
  expect_identical(
    names(named_parameters(outer, boundaries = "cross")),
    "$/module/module"
  )

  visited <- character()
  map_modules(
    outer,
    function(module, path) {
      visited <<- c(visited, path)
      module
    },
    include_root = FALSE
  )
  expect_identical(visited, character())

  map_modules(
    outer,
    function(module, path) {
      visited <<- c(visited, path)
      module
    },
    include_root = FALSE,
    boundaries = "cross"
  )
  expect_setequal(visited, c("$/module", "$/module/module"))

  freeze_modules(outer, "$/module", recursive = TRUE)
  expect_identical(is_module_frozen(compiled), TRUE)
  expect_identical(is_module_frozen(leaf), TRUE)
  expect_snapshot(
    replace_module(outer, "$/module/module", graph_test_module()),
    error = TRUE
  )

  freeze_modules(outer, "$/module", recursive = TRUE, frozen = FALSE)
  expect_identical(is_module_frozen(compiled), FALSE)
  expect_identical(is_module_frozen(leaf), FALSE)
})

test_that("a protected alias protects a shared identity everywhere", {
  shared <- graph_test_module()
  open <- best_of_n(shared, N = 2)
  locked <- best_of_n(shared, N = 2)
  program <- ensemble(list(open = open, locked = locked))
  freeze_modules(program, "$/modules/locked", recursive = FALSE)

  graph <- module_graph(program, boundaries = "cross")
  shared_rows <- graph[graph$id == rlang::obj_address(shared), ]
  expect_identical(shared_rows$protected, c(TRUE, TRUE))
  respected <- module_graph(program, boundaries = "respect")
  respected_shared <- respected[
    respected$id == rlang::obj_address(shared),
  ]
  expect_identical(respected_shared$protected, TRUE)
  expect_identical(names(named_parameters(program)), character())
  expect_identical(
    names(named_parameters(program, boundaries = "cross")),
    "$/modules/open/module"
  )
})

test_that("runtime LM propagation can cross or respect graph boundaries", {
  leaf <- graph_test_module()
  compiled <- best_of_n(leaf, N = 2)
  outer <- best_of_n(compiled, N = 2)
  compiled$state$compiled <- TRUE
  chat <- suppressWarnings(ellmer::chat_openai(
    api_key = "dummy-key",
    model = "model-a"
  ))

  set_module_lm(outer, chat, boundaries = "cross")
  addresses <- vapply(
    list(outer$chat, compiled$chat, leaf$chat),
    rlang::obj_address,
    character(1)
  )
  expect_length(unique(addresses), 3L)
  expect_identical(
    vapply(
      list(outer$chat, compiled$chat, leaf$chat),
      function(value) value$get_model(),
      character(1)
    ),
    rep("model-a", 3)
  )

  set_module_lm(outer, NULL, boundaries = "cross")
  set_module_lm(outer, chat, boundaries = "respect")
  expect_identical(inherits(outer$chat, "Chat"), TRUE)
  expect_null(compiled$chat)
  expect_null(leaf$chat)

  set_module_lm(outer, chat, boundaries = "cross", clone = FALSE)
  expect_identical(outer$chat, chat)
  expect_identical(compiled$chat, chat)
  expect_identical(leaf$chat, chat)
})
