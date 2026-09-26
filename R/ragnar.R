#' Search a ragnar store from a tool-using module
#'
#' @description
#' `ragnar_tool()` wraps a ragnar document store as an ellmer tool that takes
#' a search query and returns the `k` best-matching chunks as text. Give it to
#' [react()] so the agent can search your documents while it reasons, or
#' register it on an ellmer Chat. [rag_module()] is the alternative that
#' retrieves once, before every call.
#'
#' @param store A ragnar store, built with `ragnar::ragnar_store_create()`,
#'   `ragnar::ragnar_store_insert()` and `ragnar::ragnar_store_build_index()`.
#' @param k Number of chunks to return per search.
#' @param name Tool name shown to the model.
#' @param description Tool description shown to the model. The default says
#'   it searches the knowledge base and returns the top `k` documents.
#'
#' @details
#' Results are numbered (`[Result 1]`, `[Result 2]`, ...) and separated by
#' `---`. A search error is returned to the model as text instead of
#' stopping the conversation. Requires the ragnar package.
#'
#' @return An ellmer tool definition (see [ellmer::tool()]) for [react()]
#'   modules or `Chat$register_tool()`. It can also be called directly with a
#'   query string.
#'
#' @export
#' @family integrations
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
#' search_tool <- ragnar_tool(store, k = 3L)
#' search_tool("How do I configure the cache?")
#'
#' # Use with a ReAct module
#' agent <- react("question -> answer", tools = list(search_tool))
#' run(agent, question = "How do I turn caching off?", .llm = ellmer::chat_openai(model = "gpt-6-luna"))
#'
#' # Or register it on an ellmer Chat
#' chat <- ellmer::chat_openai(model = "gpt-6-luna")
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

#' Build a search tool from a set of documents
#'
#' @description
#' `create_search_tool()` builds a ragnar store from documents (one chunked
#' document per element, embedded with `embedding_fn`), indexes it and wraps
#' it with [ragnar_tool()], in one step. Embedding the documents calls the
#' embedding provider.
#'
#' @param documents A character vector of documents, or a data frame with a
#'   `text` or `content` column.
#' @param embedding_fn An embedding function, passed to
#'   `ragnar::ragnar_store_create()` as `embed`, for example
#'   `\(x) ragnar::embed_openai(x, model = "text-embedding-3-small")`.
#' @param k Number of chunks to return per search.
#' @param name Tool name shown to the model.
#' @param description Tool description shown to the model; see
#'   [ragnar_tool()].
#' @param ... Passed to `ragnar::ragnar_store_create()`, for example
#'   `location` to keep the store in a file instead of in memory.
#'
#' @return An ellmer tool definition, as returned by [ragnar_tool()].
#'
#' @export
#' @family integrations
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
