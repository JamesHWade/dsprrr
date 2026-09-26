#' KNN few-shot: choose demonstrations by similarity at run time
#'
#' @description
#' `KNNFewShot()` gives every input its own demonstrations: the `k` training
#' rows whose embeddings are most similar to it. Compiling embeds the training
#' set once, with no model calls; each run then embeds the input and picks its
#' nearest rows. [LabeledFewShot()], by contrast, attaches one fixed set.
#'
#' @details
#' `vectorizer` turns a character vector into a numeric matrix with one row
#' per string. For real embeddings, wrap a provider, for example
#' `function(x) ragnar::embed_openai(x, model = "text-embedding-3-small")`.
#' Similarity is cosine similarity.
#'
#' [compile()] returns a wrapper module. Each run records the chosen rows and
#' their similarity scores in `compiled$state$demo_selections`. Demonstration
#' outputs come from the metric's `field` when there is one, and otherwise from
#' an automatically detected label column, as in [LabeledFewShot()].
#'
#' @param metric Optional. It is not used for scoring, but when it has a
#'   `field`, that column supplies the demonstrations' outputs.
#' @param metric_threshold,max_errors Accepted for consistency with the other
#'   optimizers (see [Teleprompter()]); `KNNFewShot()` does not use them.
#' @param k Integer number of neighbors used as demonstrations (default `3L`).
#' @param vectorizer Required. A function that takes a character vector and
#'   returns a numeric matrix of embeddings, one row per string.
#' @param input_text Optional function that turns a one-row data frame of
#'   inputs into the text to embed. The default pastes the signature's input
#'   columns together, separated by spaces.
#' @param cache_embeddings Currently has no effect: the training set is always
#'   embedded once, at compile time.
#' @param merge_demos If `TRUE`, keep the module's existing demonstrations and
#'   add the selected ones after them. If `FALSE` (the default), the selected
#'   rows replace them.
#'
#' @return A `KNNFewShot` object to pass to [compile()].
#' @family teleprompters
#' @examples
#' # A toy vectorizer that counts letters. Use real embeddings in practice.
#' letter_counts <- function(texts) {
#'   t(vapply(
#'     tolower(texts),
#'     function(x) tabulate(utf8ToInt(x) - 96L, nbins = 26L),
#'     integer(26)
#'   ))
#' }
#'
#' tp <- KNNFewShot(k = 2L, vectorizer = letter_counts)
#' tp
#'
#' qa <- module(signature("question -> answer"))
#' trainset <- data.frame(
#'   question = c(
#'     "What is 2 + 2?", "Capital of France?",
#'     "What is 10 / 5?", "Capital of Peru?"
#'   ),
#'   answer = c("4", "Paris", "2", "Lima")
#' )
#' # Compiling embeds the training rows; no model is called
#' compiled <- compile(qa, tp, trainset)
#'
#' \dontrun{
#' run(
#'   compiled,
#'   question = "Capital of Chile?",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' compiled$state$demo_selections
#' }
#'
#' @export
KNNFewShot <- S7::new_class(
  "KNNFewShot",
  parent = Teleprompter,
  properties = list(
    k = S7::new_property(
      S7::class_integer,
      default = 3L,
      validator = function(value) {
        if (value < 1) {
          return("k must be at least 1")
        }
        NULL
      }
    ),
    vectorizer = S7::new_property(
      S7::class_function,
      validator = function(value) {
        if (!is.function(value)) {
          return("vectorizer must be a function")
        }
        NULL
      }
    ),
    input_text = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !is.function(value)) {
          return("input_text must be a function or NULL")
        }
        NULL
      }
    ),
    cache_embeddings = S7::new_property(
      S7::class_logical,
      default = TRUE
    ),
    merge_demos = S7::new_property(
      S7::class_logical,
      default = FALSE
    )
  )
)

#' Compile method for KNNFewShot
#' @noRd
compile_knn <- function(teleprompter, program, trainset, .llm = NULL, ...) {
  # Validate inputs
  if (!inherits(program, "Module")) {
    cli::cli_abort("KNNFewShot currently only supports Module objects")
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("trainset must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }

  # Get input names from signature

  input_names <- vapply(
    program$signature@inputs,
    function(x) x$name,
    character(1)
  )

  # Build input_text function if not provided
  input_text_fn <- teleprompter@input_text
  if (is.null(input_text_fn)) {
    input_text_fn <- function(example) {
      # Concatenate all input columns
      values <- vapply(
        input_names,
        function(name) {
          if (name %in% names(example)) {
            as.character(example[[name]])
          } else {
            ""
          }
        },
        character(1)
      )
      paste(values, collapse = " ")
    }
  }

  # Convert trainset rows to text for embedding
  train_texts <- vapply(
    seq_len(nrow(trainset)),
    function(i) {
      row <- trainset[i, , drop = FALSE]
      input_text_fn(row)
    },
    character(1)
  )

  # Pre-compute embeddings for training set
  cli::cli_inform(
    "Computing embeddings for {nrow(trainset)} training examples..."
  )
  train_embeddings <- teleprompter@vectorizer(train_texts)

  # Validate embedding dimensions

  if (!is.matrix(train_embeddings)) {
    train_embeddings <- matrix(train_embeddings, nrow = length(train_texts))
  }
  if (nrow(train_embeddings) != nrow(trainset)) {
    cli::cli_abort(c(
      "Vectorizer returned wrong number of embeddings",
      "x" = "Expected {nrow(trainset)} rows, got {nrow(train_embeddings)}"
    ))
  }

  # Format trainset as demos (for reference structure)
  # Use the metric's field attribute if available to determine the output column
  output_col <- get_metric_field(teleprompter@metric)
  demos_data <- format_trainset_as_demos(
    trainset,
    program$signature,
    output_col = output_col
  )

  # Create the KNN wrapper module
  knn_module <- KNNFewShotModule$new(
    module = program,
    k = teleprompter@k,
    vectorizer = teleprompter@vectorizer,
    input_text = input_text_fn,
    train_embeddings = train_embeddings,
    trainset_demos = demos_data,
    merge_demos = teleprompter@merge_demos
  )

  knn_module$config$compilation_k <- teleprompter@k
  knn_module$config$n_train_examples <- nrow(trainset)
  record_optimization_result(
    knn_module,
    optimizer = "KNNFewShot",
    best_params = list(k = teleprompter@k),
    lineage = list(n_train_examples = nrow(trainset)),
    stop_reason = "completed",
    extensions = list(merge_demos = teleprompter@merge_demos)
  )

  knn_module
}

#' Compute cosine similarity between vectors
#' @noRd
cosine_similarity <- function(x, y) {
  # x is a vector, y is a matrix (each row is an embedding)
  if (is.vector(x)) {
    x <- matrix(x, nrow = 1)
  }

  # Normalize x
  x_norm <- sqrt(sum(x^2))
  if (x_norm == 0) {
    return(rep(0, nrow(y)))
  }
  x_normalized <- x / x_norm

  # Normalize y (row-wise)
  y_norms <- sqrt(rowSums(y^2))
  y_norms[y_norms == 0] <- 1 # Avoid division by zero

  y_normalized <- y / y_norms

  # Compute similarities
  as.vector(y_normalized %*% t(x_normalized))
}

#' Find k nearest neighbors by cosine similarity
#' @noRd
find_knn <- function(query_embedding, train_embeddings, k) {
  find_knn_with_scores(query_embedding, train_embeddings, k)$indices
}

#' Find k nearest neighbors and expose cosine similarity scores
#' @noRd
find_knn_with_scores <- function(query_embedding, train_embeddings, k) {
  similarities <- cosine_similarity(query_embedding, train_embeddings)
  # Get indices of top k similarities (highest first)
  indices <- order(similarities, decreasing = TRUE)[
    seq_len(min(k, length(similarities)))
  ]

  list(
    indices = indices,
    scores = similarities[indices]
  )
}
