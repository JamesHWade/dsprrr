#' KNNFewShot Wrapper Module
#'
#' @description
#' An R6 class that wraps any Module to provide dynamic k-nearest neighbor
#' demo selection at runtime. At each forward() call, it embeds the input,
#' finds the k most similar training examples, and uses those as demonstrations.
#'
#' @details
#' This module is typically created by compiling with [KNNFewShot] teleprompter,
#' not instantiated directly. The wrapper intercepts forward() calls to:
#' 1. Embed the input query
#' 2. Find k nearest neighbors from pre-computed training embeddings
#' 3. Set those neighbors as demos on the wrapped module
#' 4. Execute the wrapped module with the dynamic demos
#'
#' @keywords internal
#' @noRd
KNNFewShotModule <- R6::R6Class(
  "KNNFewShotModule",
  inherit = Module,
  public = list(
    #' @field module The wrapped module
    module = NULL,

    #' @field k Number of nearest neighbors to use
    k = NULL,

    #' @field vectorizer Function to compute embeddings
    vectorizer = NULL,

    #' @field input_text Function to convert inputs to text for embedding
    input_text = NULL,

    #' @field train_embeddings Pre-computed training set embeddings
    train_embeddings = NULL,

    #' @field trainset_demos List of training examples in demo format
    trainset_demos = NULL,

    #' @field merge_demos Whether to merge with existing demos
    merge_demos = NULL,

    #' @field original_demos Original demos from the wrapped module (if any)
    original_demos = NULL,

    #' @description
    #' Initialize a KNNFewShot wrapper module
    #'
    #' @param module The module to wrap (must inherit from Module)
    #' @param k Number of nearest neighbors
    #' @param vectorizer Function to compute embeddings
    #' @param input_text Function to convert inputs to text
    #' @param train_embeddings Pre-computed training embeddings matrix
    #' @param trainset_demos Training examples in demo format
    #' @param merge_demos Whether to merge with existing demos
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
      module,
      k = 3L,
      vectorizer,
      input_text,
      train_embeddings,
      trainset_demos,
      merge_demos = FALSE,
      config = list(),
      chat = NULL
    ) {
      # Validate module
      if (!inherits(module, "Module")) {
        cli::cli_abort(c(
          "module must be a Module object",
          "x" = "You provided: {.cls {class(module)[1]}}"
        ))
      }

      # Use module's signature
      super$initialize(
        signature = module$signature,
        config = config,
        chat = chat %||% module$chat
      )

      # Create deep copy of module to avoid modifying original
      self$module <- module$deepcopy()
      self$k <- as.integer(k)
      self$vectorizer <- vectorizer
      self$input_text <- input_text
      self$train_embeddings <- train_embeddings
      self$trainset_demos <- trainset_demos
      self$merge_demos <- merge_demos

      # Store original demos if merge_demos is TRUE
      if (merge_demos && !is.null(module$demos)) {
        self$original_demos <- module$demos
      } else {
        self$original_demos <- list()
      }

      # Initialize state for tracking
      self$state$knn_queries <- 0L
      self$state$demo_selections <- list()
    },

    #' @description
    #' Execute with dynamically selected demos
    #'
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param ... Additional arguments passed to wrapped module
    #' @return Tibble with output, chat, metadata columns
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      start_time <- Sys.time()

      # Handle both list and data frame inputs
      if (is.data.frame(batch)) {
        # Process each row
        results <- lapply(seq_len(nrow(batch)), function(i) {
          row_inputs <- as.list(batch[i, , drop = FALSE])
          private$forward_single(row_inputs, .llm = .llm, trace = trace, ...)
        })
        result <- do.call(rbind, results)
      } else {
        # Single input as list
        result <- private$forward_single(batch, .llm = .llm, trace = trace, ...)
      }

      # Update query count
      self$state$knn_queries <- self$state$knn_queries + 1L

      # Record trace if enabled
      if (trace) {
        execution_time <- as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
        trace_entry <- tibble::tibble(
          timestamp = Sys.time(),
          module = "KNNFewShotModule",
          operation = "forward",
          execution_time = execution_time
        )
        if (is.null(self$state$traces)) {
          self$state$traces <- trace_entry
        } else {
          self$state$traces <- rbind(self$state$traces, trace_entry)
        }
      }

      result
    },

    #' @description
    #' Create a deep copy of this module
    #' @return A new KNNFewShotModule with copied state
    deepcopy = function() {
      new_module <- KNNFewShotModule$new(
        module = self$module$deepcopy(),
        k = self$k,
        vectorizer = self$vectorizer,
        input_text = self$input_text,
        train_embeddings = self$train_embeddings,
        trainset_demos = self$trainset_demos,
        merge_demos = self$merge_demos,
        config = self$config,
        chat = self$chat
      )

      # Copy state
      new_module$state <- as.list(self$state)
      new_module$original_demos <- self$original_demos

      new_module
    },

    #' @description
    #' Get info about the last demo selection
    #' @return List with indices and demos selected for last query
    last_selection = function() {
      if (length(self$state$demo_selections) == 0) {
        return(NULL)
      }
      self$state$demo_selections[[length(self$state$demo_selections)]]
    },

    #' @description
    #' Reset state including demo selection history
    reset = function() {
      super$reset()
      self$state$knn_queries <- 0L
      self$state$demo_selections <- list()
      self$module$reset()
      invisible(self)
    }
  ),

  private = list(
    #' Process a single input
    forward_single = function(inputs, .llm = NULL, trace = TRUE, ...) {
      # Convert inputs to text for embedding
      # Create a pseudo data frame row for input_text function
      input_row <- as.data.frame(inputs)
      query_text <- self$input_text(input_row)

      # Compute query embedding
      query_embedding <- self$vectorizer(query_text)
      if (is.matrix(query_embedding)) {
        query_embedding <- query_embedding[1, ]
      }

      # Find k nearest neighbors
      neighbors <- find_knn_with_scores(
        query_embedding,
        self$train_embeddings,
        self$k
      )
      nn_indices <- neighbors$indices
      nn_scores <- neighbors$scores

      # Select demos from training set
      selected_demos <- self$trainset_demos[nn_indices]

      # Merge with original demos if requested
      if (self$merge_demos && length(self$original_demos) > 0) {
        all_demos <- c(self$original_demos, selected_demos)
      } else {
        all_demos <- selected_demos
      }

      # Set demos on the wrapped module
      self$module$demos <- all_demos

      # Record selection for debugging/inspection
      selection_info <- list(
        query_text = query_text,
        indices = nn_indices,
        scores = nn_scores,
        n_demos = length(all_demos)
      )
      self$state$demo_selections <- c(
        self$state$demo_selections,
        list(selection_info)
      )

      # Execute wrapped module
      result <- self$module$forward(inputs, .llm = .llm, trace = trace, ...)

      # Add KNN metadata to result
      if ("metadata" %in% names(result)) {
        result$metadata <- lapply(result$metadata, function(m) {
          m$knn_indices <- nn_indices
          m$knn_scores <- nn_scores
          m$knn_n_demos <- length(all_demos)
          m
        })
      }

      result
    }
  )
)
