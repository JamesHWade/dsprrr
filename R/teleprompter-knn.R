#' KNNFewShot Teleprompter
#'
#' @description
#' A teleprompter that uses k-nearest neighbor retrieval to dynamically select
#' demonstrations at runtime based on similarity to the input query. Unlike
#' [LabeledFewShot] which selects static demos at compile time, KNNFewShot
#' selects different demos for each query based on embedding similarity.
#'
#' @details
#' KNNFewShot works by:
#' 1. Pre-computing embeddings for all training examples at compile time
#' 2. At runtime, embedding each input query
#' 3. Finding the k most similar training examples using cosine similarity
#' 4. Using those examples as demonstrations for the current query
#'
#' This approach is particularly effective when:
#' - The training set is large and diverse
#' - Different types of queries benefit from different demonstrations
#' - Semantic similarity is a good proxy for task relevance
#'
#' @section Vectorizer Function:
#' The `vectorizer` parameter should be a function that takes a character vector
#' and returns a numeric matrix where each row is an embedding. Common options:
#' - `ragnar::embed_openai()` for OpenAI embeddings
#' - Custom embedding functions using other providers
#'
#' For testing, you can provide a deterministic vectorizer that returns
#' consistent embeddings for the same inputs.
#'
#' @param metric A metric function for evaluating predictions. If NULL,
#'   uses exact_match() by default.
#' @param metric_threshold Minimum score required to be considered successful.
#'   If NULL, uses the metric's default threshold.
#' @param max_errors Maximum number of errors allowed during optimization.
#'   Default is 5.
#' @param k Number of nearest neighbors to use as demonstrations. Default is 3.
#' @param vectorizer A function that takes a character vector and returns a
#'   numeric matrix of embeddings (one row per input). Required.
#' @param input_text A function that converts a training example (data frame row)
#'   to a character string for embedding. Default concatenates all input columns.
#' @param cache_embeddings Whether to cache embeddings for the training set.
#'   Default is TRUE.
#' @param merge_demos If TRUE, merge KNN-selected demos with any existing demos
#'   on the module. Default is FALSE (replace).
#'
#' @examples
#' \dontrun{
#' # Create a simple vectorizer (in practice, use ragnar::embed_openai or similar)
#' simple_vectorizer <- function(texts) {
#'   # Return random embeddings for demonstration
#'   matrix(runif(length(texts) * 10), nrow = length(texts))
#' }
#'
#' # Create the teleprompter
#' tp <- KNNFewShot(
#'   k = 3L,
#'   vectorizer = simple_vectorizer,
#'   input_text = function(example) example$question
#' )
#'
#' # Compile a module
#' compiled <- compile(tp, my_module, trainset, .llm = llm)
#'
#' # Now queries will get dynamically selected demos
#' run(compiled, question = "What is 2+2?", .llm = llm)
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

  # Mark as compiled
  knn_module$state$compiled <- TRUE
  knn_module$config$compiled <- TRUE
  knn_module$config$teleprompter <- "KNNFewShot"
  knn_module$config$compilation_k <- teleprompter@k
  knn_module$config$n_train_examples <- nrow(trainset)

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
