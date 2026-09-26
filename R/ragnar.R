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
#' @return An ellmer tool definition (see [ellmer::tool()]) for `react()`
#'   modules or `Chat$register_tool()`. It can also be called directly with a
#'   query string.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a ragnar store from documents
#' library(ragnar)
#' store <- ragnar_store_create(
#'   embed = \\(x) embed_openai(x, model = "text-embedding-3-small")
#' )
#' for (i in seq_along(my_docs)) {
#'   doc <- MarkdownDocument(my_docs[[i]], origin = paste0("doc-", i))
#'   ragnar_store_insert(store, markdown_chunk(doc))
#' }
#' ragnar_store_build_index(store)
#'
#' # Create a search tool
#' search_tool <- ragnar_tool(store, k = 3)
#'
#' # Use with ReAct module
#' react_mod <- react(
#'   signature("question -> answer"),
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
  rlang::check_installed("ragnar", reason = "for ragnar_tool()")

  k <- as.integer(k)
  description <- description %||%
    paste(
      "Search the knowledge base for relevant information.",
      "Returns the top",
      k,
      "most relevant documents.",
      "Use this tool to find facts, context, or supporting information."
    )

  search <- function(query) {
    if (is.null(query) || !nzchar(query)) {
      return("Error: Please provide a search query.")
    }
    results <- tryCatch(
      ragnar::ragnar_retrieve(store, query, top_k = k),
      error = function(e) paste("Search error:", conditionMessage(e))
    )
    format_search_results(results)
  }

  ellmer::tool(
    search,
    description = description,
    arguments = list(
      query = ellmer::type_string(
        "The search query to find relevant documents"
      )
    ),
    name = name
  )
}

#' Format retrieved documents for an LLM
#' @noRd
format_search_results <- function(results) {
  if (is.character(results)) {
    return(paste(results, collapse = "\n\n"))
  }
  if (!is.data.frame(results) || nrow(results) == 0) {
    return("No results found.")
  }

  docs <- if ("text" %in% names(results)) {
    results$text
  } else if ("content" %in% names(results)) {
    results$content
  } else {
    apply(results, 1, function(row) {
      paste(names(row), ":", row, collapse = "; ")
    })
  }

  formatted <- vapply(
    seq_along(docs),
    function(i) {
      header <- paste0("[Result ", i, "]")
      if ("source" %in% names(results)) {
        header <- paste0(header, " (", results$source[i], ")")
      }
      paste0(header, "\n", docs[i])
    },
    character(1)
  )

  paste(formatted, collapse = "\n\n---\n\n")
}

#' Create a Semantic Search Tool from Documents
#'
#' @description
#' Convenience function that creates a ragnar store from documents and wraps
#' it in a search tool in one step.
#'
#' @param documents Character vector of documents, or a data frame with a
#'   'text' or 'content' column.
#' @param embedding_fn Embedding function passed to
#'   `ragnar::ragnar_store_create()` as `embed`, e.g.
#'   `\(x) ragnar::embed_openai(x, model = "text-embedding-3-small")`.
#' @param k Number of documents to retrieve per search (default 5).
#' @param name Tool name (default "search_documents").
#' @param description Optional tool description.
#' @param ... Additional arguments passed to `ragnar::ragnar_store_create()`.
#'
#' @return An ellmer tool definition, as returned by [ragnar_tool()].
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
#'   embedding_fn = \\(x) ragnar::embed_openai(x, model = "text-embedding-3-small"),
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

  if (is.data.frame(documents)) {
    text_col <- intersect(c("text", "content"), names(documents))
    if (length(text_col) == 0) {
      cli::cli_abort(
        "{.arg documents} needs a {.field text} or {.field content} column."
      )
    }
    documents <- documents[[text_col[[1]]]]
  }

  # Build an in-memory store (or wherever `...` points ragnar), one document
  # per entry, then index it for retrieval.
  store <- tryCatch(
    {
      store <- ragnar::ragnar_store_create(embed = embedding_fn, ...)
      for (i in seq_along(documents)) {
        doc <- ragnar::MarkdownDocument(
          as.character(documents[[i]]),
          origin = paste0("document-", i)
        )
        ragnar::ragnar_store_insert(store, ragnar::markdown_chunk(doc))
      }
      ragnar::ragnar_store_build_index(store)
      store
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to create ragnar store",
          "x" = conditionMessage(e)
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
