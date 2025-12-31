#' Create a ragnar Search Tool for ReAct Modules
#'
#' @description
#' Creates an ellmer-compatible tool that searches a ragnar document store.
#' This tool can be used with ReAct modules or registered with ellmer Chat
#' objects for agentic document retrieval.
#'
#' @param store A ragnar store created with `ragnar::ragnar_store_create()`.
#' @param k Number of documents to retrieve per search (default 5).
#' @param name Tool name (default "search_knowledge").
#' @param description Tool description for the LLM.
#'
#' @return A function suitable for use with ReAct modules or
#'   `ellmer::Chat$register_tool()`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a ragnar store from documents
#' library(ragnar)
#' store <- ragnar_store_create(
#'   documents = my_docs,
#'   embedding_fn = embed_openai()
#' )
#'
#' # Create a search tool
#' search_tool <- ragnar_tool(store, k = 3)
#'
#' # Use with ReAct module
#' react_mod <- module(
#'   signature("question -> answer"),
#'   type = "react",
#'   tools = list(search_tool)
#' )
#'
#' # Or register with ellmer Chat
#' chat <- ellmer::chat_openai()
#' chat$register_tool(search_tool)
#' }
ragnar_tool <- function(
  store,
  k = 5L,
  name = "search_knowledge",
  description = NULL
) {
  # Check ragnar availability
  rlang::check_installed("ragnar", reason = "for ragnar_tool()")

  if (is.null(description)) {
    description <- paste(
      "Search the knowledge base for relevant information.",
      "Returns the top",
      k,
      "most relevant documents.",
      "Use this tool to find facts, context, or supporting information."
    )
  }

  # Capture store in closure
  captured_store <- store
  captured_k <- as.integer(k)

  # Create the tool function
  tool_fn <- function(query) {
    if (is.null(query) || !nzchar(query)) {
      return("Error: Please provide a search query.")
    }

    results <- tryCatch(
      {
        ragnar::ragnar_retrieve(captured_store, query, k = captured_k)
      },
      error = function(e) {
        return(paste("Search error:", e$message))
      }
    )

    # Format results for LLM consumption
    if (is.data.frame(results)) {
      # Extract text content
      if ("text" %in% names(results)) {
        docs <- results$text
      } else if ("content" %in% names(results)) {
        docs <- results$content
      } else {
        docs <- apply(results, 1, function(row) {
          paste(names(row), ":", row, collapse = "; ")
        })
      }

      # Include metadata if available
      formatted <- vapply(
        seq_along(docs),
        function(i) {
          header <- paste0("[Result ", i, "]")

          # Add source if available
          if ("source" %in% names(results)) {
            header <- paste0(header, " (", results$source[i], ")")
          }

          paste0(header, "\n", docs[i])
        },
        character(1)
      )

      paste(formatted, collapse = "\n\n---\n\n")
    } else if (is.character(results)) {
      paste(results, collapse = "\n\n")
    } else {
      "No results found."
    }
  }

  # Attach metadata for tool registration
  attr(tool_fn, "name") <- name
  attr(tool_fn, "description") <- description
  attr(tool_fn, "arguments") <- list(
    query = list(
      type = "string",
      description = "The search query to find relevant documents"
    )
  )

  class(tool_fn) <- c("ragnar_tool", "dsprrr_tool", "function")

  tool_fn
}

#' Create a Semantic Search Tool from Documents
#'
#' @description
#' Convenience function that creates a ragnar store from documents and wraps
#' it in a search tool in one step.
#'
#' @param documents Character vector of documents, or a data frame with a
#'   'text' or 'content' column.
#' @param embedding_fn Embedding function from ragnar (e.g.,
#'   `ragnar::embed_openai()`).
#' @param k Number of documents to retrieve per search (default 5).
#' @param name Tool name (default "search_documents").
#' @param description Optional tool description.
#' @param ... Additional arguments passed to `ragnar::ragnar_store_create()`.
#'
#' @return A search tool function.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create tool directly from documents
#' docs <- c(
#'   "R is a programming language for statistical computing.",
#'   "Python is a general-purpose programming language.",
#'   "Julia is designed for high-performance numerical computing."
#' )
#'
#' search_tool <- create_search_tool(
#'   documents = docs,
#'   embedding_fn = ragnar::embed_openai(),
#'   k = 2
#' )
#'
#' # Use the tool
#' search_tool("What language is best for statistics?")
#' }
create_search_tool <- function(
  documents,
  embedding_fn,
  k = 5L,
  name = "search_documents",
  description = NULL,
  ...
) {
  rlang::check_installed("ragnar", reason = "for create_search_tool()")

  # Create ragnar store
  store <- tryCatch(
    {
      ragnar::ragnar_store_create(
        documents = documents,
        embedding_fn = embedding_fn,
        ...
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to create ragnar store",
          "x" = e$message
        ),
        parent = e
      )
    }
  )

  ragnar_tool(
    store = store,
    k = k,
    name = name,
    description = description
  )
}

#' Print method for ragnar_tool
#' @param x A ragnar_tool object
#' @param ... Additional arguments (unused)
#' @export
print.ragnar_tool <- function(x, ...) {
  cli::cli_h3("Ragnar Search Tool")
  cli::cli_text("{.field Name}: {attr(x, 'name')}")
  cli::cli_text("{.field Description}: {attr(x, 'description')}")

  invisible(x)
}
