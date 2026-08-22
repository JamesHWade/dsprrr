TestChat <- R6::R6Class(
  "Chat",
  public = list(
    turns = NULL,
    model = NULL,
    parity_state = NULL,
    chat_impl = NULL,
    chat_structured_impl = NULL,
    stream_impl = NULL,

    initialize = function(
      chat = function(...) "mock response",
      chat_structured = function(...) list(answer = "mock response"),
      stream = function(...) character(),
      turns = list(),
      model = "test-model",
      provider = NULL
    ) {
      self$chat_impl <- chat
      self$chat_structured_impl <- chat_structured
      self$stream_impl <- stream
      self$turns <- turns
      self$model <- model
      private$provider <- if (is.null(provider)) {
        ellmer::Provider(
          name = "test",
          model = model,
          base_url = ""
        )
      } else {
        provider
      }
    },

    chat = function(...) {
      self$chat_impl(...)
    },

    chat_structured = function(...) {
      self$chat_structured_impl(...)
    },

    stream = function(...) {
      self$stream_impl(...)
    },

    get_turns = function() {
      self$turns
    },

    set_turns = function(turns) {
      self$turns <- turns
      invisible(self)
    },

    last_turn = function(role = NULL) {
      if (length(self$turns) == 0L) {
        return(NULL)
      }
      self$turns[[length(self$turns)]]
    },

    get_model = function() {
      self$model
    },

    get_provider = function() {
      private$provider
    }
  ),
  private = list(
    provider = NULL
  ),
  lock_objects = FALSE,
  parent_env = globalenv()
)

new_test_chat <- function(
  ...,
  clone = NULL,
  get_turns = NULL,
  set_turns = NULL,
  last_turn = NULL,
  get_model = NULL
) {
  chat <- TestChat$new(...)
  methods <- list(
    clone = clone,
    get_turns = get_turns,
    set_turns = set_turns,
    last_turn = last_turn,
    get_model = get_model
  )
  for (name in names(methods)) {
    if (is.function(methods[[name]])) {
      override_test_chat_method(chat, name, methods[[name]])
    }
  }
  chat
}

override_test_chat_method <- function(chat, name, method) {
  if (bindingIsLocked(name, chat)) {
    unlockBinding(name, chat)
  }
  assign(name, method, envir = chat)
  lockBinding(name, chat)
  invisible(chat)
}

as_test_chat <- function(chat) {
  if (inherits(chat, "Chat") && R6::is.R6(chat)) {
    return(chat)
  }
  method_or <- function(name, default) {
    method <- tryCatch(chat[[name]], error = function(e) NULL)
    if (is.function(method)) method else default
  }
  converted <- new_test_chat(
    chat = method_or("chat", function(...) "mock response"),
    chat_structured = method_or(
      "chat_structured",
      function(...) list(answer = "mock response")
    ),
    stream = method_or("stream", function(...) character()),
    get_turns = method_or("get_turns", NULL),
    set_turns = method_or("set_turns", NULL),
    last_turn = method_or("last_turn", NULL),
    get_model = method_or("get_model", NULL)
  )
  clone <- tryCatch(chat[["clone"]], error = function(e) NULL)
  if (is.function(clone)) {
    override_test_chat_method(converted, "clone", function(deep = TRUE) {
      args <- names(formals(clone))
      cloned <- if (any(c("deep", "...") %in% args)) {
        clone(deep = deep)
      } else {
        clone()
      }
      as_test_chat(cloned)
    })
  }
  converted
}
