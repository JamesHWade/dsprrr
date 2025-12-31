# Phase 3: Deep Ecosystem Integration

## Executive Summary

Phase 3 represents dsprrr’s evolution from a standalone DSPy
implementation to a first-class citizen of the tidyverse/posit AI
ecosystem. This issue replaces the original Phase 3 plan with a more
ambitious, deeply researched integration strategy.

The current Phase 3 plan in Issue \#5 identifies the right targets
(tidymodels, shinychat, vetiver) but underestimates the depth of
integration possible. This revised plan creates **bidirectional
bridges** where dsprrr modules become interchangeable with other
ecosystem components.

------------------------------------------------------------------------

## Current Integration Audit

| Package        | Current State                                                                                                                                                                               | Gap Analysis                                                            |
|----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| **ellmer**     | Good - uses `chat_structured()`, types, providers                                                                                                                                           | Missing: batch/parallel chat, tool registration, embeddings passthrough |
| **ragnar**     | None                                                                                                                                                                                        | No RAG support; need retrieval-augmented modules                        |
| **vitals**     | Good - [`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md), [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md) | Limited scorer integration, no Task creation helpers                    |
| **shinychat**  | None                                                                                                                                                                                        | No chat UI integration                                                  |
| **tidymodels** | Basic - `dials` params, `yardstick` in [`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md)                                                                | Missing: parsnip spec, tune workflow, workflows bundle                  |
| **vetiver**    | None                                                                                                                                                                                        | No deployment path                                                      |
| **pins**       | Suggested dependency                                                                                                                                                                        | Missing: module serialization helpers                                   |

------------------------------------------------------------------------

## Critical: Reasoning Model Support (2025 Model Landscape)

The LLM landscape has fundamentally changed in 2025. GPT-5 class models
and o-series reasoning models have different parameter requirements than
traditional models. dsprrr must handle this correctly.

### Current Model Families

| Provider      | Traditional Models                  | Reasoning Models                                                                     |
|---------------|-------------------------------------|--------------------------------------------------------------------------------------|
| **OpenAI**    | gpt-4.1, gpt-4.1-mini, gpt-4.1-nano | gpt-5, gpt-5.1, gpt-5.2, gpt-5-mini, gpt-5-nano, o3, o4-mini                         |
| **Anthropic** | claude-3-haiku                      | claude-sonnet-4.5, claude-opus-4.5, claude-haiku-4.5 (all support extended thinking) |

### Parameter Differences

**Traditional models** (gpt-4.1, older Claude): - `temperature`: 0-2
(controls randomness) - `top_p`: 0-1 (nucleus sampling) - `max_tokens`:
Output limit

**Reasoning models** (gpt-5, o3, o4-mini): - ~~`temperature`~~: **NOT
SUPPORTED** - fixed at 1.0 - `reasoning_effort`: “low”, “medium”,
“high”, or “none” (gpt-5.2+) - `reasoning_tokens`: Integer budget for
thinking tokens

**Claude 4+ Extended Thinking**: - `temperature`: Supported but behaves
differently with thinking enabled - Extended thinking: Enabled via API
parameter

### ellmer Support Status

ellmer’s `params()` function **already supports** reasoning parameters:

``` r
params(
  temperature = NULL,      # Traditional models only
  reasoning_effort = NULL, # "low", "medium", "high" for reasoning models
  reasoning_tokens = NULL, # Token budget alternative
  # ... other params
)
```

ellmer also has **robust cost tracking** via `token_usage()` - dsprrr
should leverage this rather than reimplementing.

### Required dsprrr Changes

#### 3.0.1 Model-Aware Parameter Handling

**Problem**: Current
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
includes `temperature` as a tunable parameter, which will error on
reasoning models.

**Solution**:

``` r
#' Detect if a model is a reasoning model
#' @keywords internal
is_reasoning_model <- function(chat) {
  model <- tryCatch(chat$get_model(), error = function(e) "")

  # OpenAI reasoning models
  openai_reasoning <- grepl("^(gpt-5|o[0-9]|o[0-9]+-)", model, ignore.case = TRUE)

  # Claude with extended thinking enabled
  claude_thinking <- grepl("claude-(sonnet|opus|haiku)-4", model) &&
    isTRUE(chat$get_params()$extended_thinking)

  openai_reasoning || claude_thinking
}

#' Get tunable parameters for a model type
#' @keywords internal
get_tunable_params <- function(chat) {
  if (is_reasoning_model(chat)) {
    list(
      reasoning_effort = c("low", "medium", "high"),
      max_tokens = c(1024, 2048, 4096)
    )
  } else {
    list(
      temperature = c(0.3, 0.7, 1.0),
      top_p = c(0.9, 0.95, 1.0),
      max_tokens = c(1024, 2048, 4096)
    )
  }
}
```

#### 3.0.2 Update `module_parameters()` for Reasoning Models

``` r
#' @rdname module_parameters
module_parameters <- function(module, include = NULL, exclude = NULL) {
  # ... existing code ...

  # Detect model type from chat
  chat <- module$chat %||% get_default_chat(create = FALSE)

  if (!is.null(chat) && is_reasoning_model(chat)) {
    # Reasoning model defaults - exclude non-tunable params
    exclude <- c(exclude, "temperature", "top_p")
    known_defaults <- list(
      reasoning_effort = c("low", "medium", "high"),
      max_output_tokens = c(1024, 4096, 8192)
    )
  } else {
    # Traditional model defaults
    known_defaults <- list(
      temperature = c(0, 1),
      top_p = c(0, 1),
      max_output_tokens = c(32, 4096)
    )
  }

  # ... rest of function ...
}
```

#### 3.0.3 Provider Defaults Helper

``` r
#' Get provider-specific defaults based on model type
#' @keywords internal
provider_defaults <- function(chat) {
  model <- tryCatch(chat$get_model(), error = function(e) "unknown")

  # GPT-5 series (reasoning)
  if (grepl("^gpt-5", model)) {
    return(list(
      reasoning_effort = "medium",
      max_tokens = 4096,
      supports_structured = TRUE,
      supports_temperature = FALSE,
      model_type = "reasoning"
    ))
  }

  # o-series (reasoning)
  if (grepl("^o[0-9]", model)) {
    return(list(
      reasoning_effort = "medium",
      max_tokens = 4096,
      supports_structured = TRUE,
      supports_temperature = FALSE,
      model_type = "reasoning"
    ))
  }

  # Claude 4+ (supports extended thinking)
  if (grepl("claude-(sonnet|opus|haiku)-(4|4\\.[0-9])", model)) {
    return(list(
      temperature = 1.0,
      max_tokens = 8192,
      supports_structured = TRUE,
      supports_temperature = TRUE,
      supports_extended_thinking = TRUE,
      model_type = "hybrid"
    ))
  }

  # GPT-4.1 series (traditional)
  if (grepl("^gpt-4\\.1", model)) {
    return(list(
      temperature = 0.7,
      max_tokens = 4096,
      supports_structured = TRUE,
      supports_temperature = TRUE,
      model_type = "traditional"
    ))
  }

  # Default fallback
  list(
    temperature = 0.7,
    max_tokens = 2048,
    supports_structured = TRUE,
    supports_temperature = TRUE,
    model_type = "traditional"
  )
}
```

#### 3.0.4 Leverage ellmer’s Cost Tracking

Instead of reimplementing cost tracking, dsprrr should provide a thin
wrapper around ellmer’s `token_usage()`:

``` r
#' Get usage summary from ellmer
#'
#' @description
#' Provides a dsprrr-focused view of LLM usage by wrapping ellmer's
#' token_usage() function.
#'
#' @export
dsp_usage_summary <- function() {
  usage <- ellmer::token_usage()

  if (nrow(usage) == 0) {
    cli::cli_alert_info("No usage data available")
    return(invisible(NULL))
  }

  cli::cli_h2("dsprrr Usage Summary")
  cli::cli_text("Total calls: {nrow(usage)}")
  cli::cli_text("Total tokens: {format(sum(usage$tokens, na.rm = TRUE), big.mark = ',')}")
  cli::cli_text("Total cost: ${format(sum(usage$cost, na.rm = TRUE), digits = 2)}")

  invisible(usage)
}
```

------------------------------------------------------------------------

## Integration Architecture

                        ┌─────────────────────────────────────────┐
                        │              dsprrr                      │
                        │  ┌─────────┐  ┌─────────┐  ┌─────────┐  │
                        │  │ Modules │  │ Compile │  │ Evaluate│  │
                        │  └────┬────┘  └────┬────┘  └────┬────┘  │
                        │       │            │            │        │
                        └───────┼────────────┼────────────┼────────┘
                                │            │            │
        ┌───────────────────────┼────────────┼────────────┼────────────────────┐
        │                       ▼            ▼            ▼                    │
        │  ┌─────────┐    ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  │
        │  │ ellmer  │◄──►│ ragnar  │  │ vitals  │  │tidymodels│ │shinychat│  │
        │  └────┬────┘    └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘  │
        │       │              │            │            │            │        │
        │       ▼              ▼            ▼            ▼            ▼        │
        │  ┌─────────────────────────────────────────────────────────────┐    │
        │  │                       vetiver                                │    │
        │  │                   (Deployment Layer)                         │    │
        │  └─────────────────────────────────────────────────────────────┘    │
        │                         Posit AI Ecosystem                           │
        └─────────────────────────────────────────────────────────────────────┘

------------------------------------------------------------------------

## 3.1 Deep ellmer Integration

### Current Usage

``` r
# dsprrr currently uses:
llm$chat_structured(prompt, type = signature@output_type)
llm$stream(prompt)
llm$chat_structured_async(prompt, type)
```

### Enhanced Integration

#### 3.1.1 Batch Processing via `parallel_chat()`

**Problem**:
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
processes rows sequentially or via mirai. ellmer’s
`parallel_chat_structured()` is more efficient for concurrent API calls.

**Solution**:

``` r
# New internal helper
run_batch_ellmer <- function(module, data, .llm, .parallel = TRUE) {
  prompts <- build_prompts(module, data)

  if (.parallel) {
    # Use ellmer's native parallelism
    results <- ellmer::parallel_chat_structured(
      .llm,
      prompts,
      type = module$signature@output_type
    )
  } else {
    # Sequential fallback
    results <- lapply(prompts, function(p) {
      .llm$chat_structured(p, type = module$signature@output_type)
    })
  }

  process_batch_results(results, data)
}
```

**Files**: `R/run.R` (modify
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
internals)

#### 3.1.2 Tool Registration for Modules

**Problem**: Modules can’t be used as tools for other modules (e.g.,
ReactModule calling a PredictModule).

**Solution**:

``` r
#' Register a module as an ellmer tool
#'
#' @param module A dsprrr Module
#' @param name Tool name (defaults to signature's output name)
#' @param description Tool description
#' @return An ellmer ToolDef object
#' @export
as_ellmer_tool <- function(module, name = NULL, description = NULL) {
  sig <- module$signature

  name <- name %||% paste0("dsprrr_", sig@output_name)
  description <- description %||% sig@instructions

  # Build parameter schema from signature inputs
  params <- lapply(sig@inputs, function(inp) {
    list(
      name = inp$name,
      type = "string",
      description = inp$description %||% inp$name
    )
  })

  ellmer::tool(
    name = name,
    description = description,
    func = function(...) {
      inputs <- list(...)
      result <- module$run(..., .return_format = "simple")
      as.character(result)
    },
    annotations = ellmer::tool_annotations(
      params = params
    )
  )
}
```

**Usage**:

``` r
# Create a classifier module
classifier <- module("text -> category: enum('spam', 'ham')")

# Register as tool for a React agent
agent <- module("task -> result", type = "react")
agent$add_tool(as_ellmer_tool(classifier, name = "classify_text"))
```

**Files**: New `R/ellmer-tools.R`

#### 3.1.3 Provider-Aware Defaults

**Problem**: Different providers have different optimal settings
(temperature ranges, token limits).

**Solution**:

``` r
#' Get provider-specific defaults
#' @keywords internal
provider_defaults <- function(chat) {
  provider <- class(chat)[1]

  switch(provider,
    "ChatOpenAI" = list(
      temperature = 0.7,
      max_tokens = 4096,
      supports_structured = TRUE
    ),
    "ChatClaude" = list(
      temperature = 1.0,
      max_tokens = 4096,
      supports_structured = TRUE,
      supports_thinking = TRUE
    ),
    "ChatOllama" = list(
      temperature = 0.8,
      max_tokens = 2048,
      supports_structured = FALSE  # Many local models don't
    ),
    # Default fallback
    list(temperature = 0.7, max_tokens = 2048, supports_structured = TRUE)
  )
}
```

**Files**: `R/utils.R` or new `R/providers.R`

------------------------------------------------------------------------

## 3.2 ragnar Integration (RAG Workflows)

### Overview

ragnar provides everything needed for RAG: document processing,
chunking, embeddings, storage, and retrieval. dsprrr should integrate at
multiple levels.

### 3.2.1 RAG-Enabled Modules

**Problem**: No way to ground module responses in retrieved documents.

**Solution** - New module type:

``` r
#' Create a retrieval-augmented module
#'
#' @param signature Module signature (should include 'context' input)
#' @param store A ragnar store created via `ragnar_store_create()`
#' @param k Number of chunks to retrieve (default 5)
#' @param retrieval_method One of "hybrid", "vss", or "bm25"
#' @param ... Additional arguments passed to module()
#' @export
rag_module <- function(
  signature,
  store,
  k = 5L,
  retrieval_method = c("hybrid", "vss", "bm25"),
  ...
) {
  if (!requireNamespace("ragnar", quietly = TRUE)) {
    cli::cli_abort("ragnar package required for RAG modules")
  }

  retrieval_method <- match.arg(retrieval_method)

  # Ensure signature has context input
  sig <- if (is.character(signature)) {
    signature(signature)
  } else {
    signature
  }

  if (!"context" %in% vapply(sig@inputs, `[[`, character(1), "name")) {
    cli::cli_warn("RAG modules typically expect a 'context' input")
  }

  # Create base module
  base_mod <- module(sig, ...)

  # Wrap in RAGModule
  RAGModule$new(
    base_module = base_mod,
    store = store,
    k = k,
    retrieval_method = retrieval_method
  )
}
```

**RAGModule R6 Class**:

``` r
RAGModule <- R6::R6Class(
  "RAGModule",
  inherit = Module,
  public = list(
    base_module = NULL,
    store = NULL,
    k = 5L,
    retrieval_method = "hybrid",

    initialize = function(base_module, store, k, retrieval_method) {
      self$base_module <- base_module
      self$store <- store
      self$k <- k
      self$retrieval_method <- retrieval_method

      # Copy signature from base module
      self$signature <- base_module$signature
      self$config <- base_module$config
      self$state <- base_module$state
    },

    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      # Extract query from inputs (use first text input)
      query <- private$extract_query(batch)

      # Retrieve relevant chunks
      chunks <- private$retrieve_chunks(query)

      # Format context
      context <- private$format_context(chunks)

      # Inject context into batch
      batch$context <- context

      # Forward to base module
      result <- self$base_module$forward(batch, .llm = .llm, trace = trace, ...)

      # Add retrieval metadata
      if (trace && nrow(result) > 0) {
        result$.retrieval <- list(list(
          chunks = chunks,
          query = query,
          k = self$k,
          method = self$retrieval_method
        ))
      }

      result
    },

    #' @description Get the underlying ragnar store
    get_store = function() {
      self$store
    },

    #' @description Inspect retrieved chunks for last query
    last_retrieval = function() {
      traces <- self$state$traces
      if (length(traces) == 0) return(NULL)

      last <- traces[[length(traces)]]
      last$retrieval
    }
  ),
  private = list(
    extract_query = function(batch) {
      # Use first text input as query
      for (inp in self$signature@inputs) {
        if (inp$name != "context" && inp$name %in% names(batch)) {
          return(batch[[inp$name]])
        }
      }
      cli::cli_abort("No query input found in batch")
    },

    retrieve_chunks = function(query) {
      switch(self$retrieval_method,
        "hybrid" = ragnar::ragnar_retrieve(self$store, query, k = self$k),
        "vss" = ragnar::ragnar_retrieve_vss(self$store, query, k = self$k),
        "bm25" = ragnar::ragnar_retrieve_bm25(self$store, query, k = self$k)
      )
    },

    format_context = function(chunks) {
      if (nrow(chunks) == 0) return("")

      paste(
        "Retrieved context:",
        paste(
          sprintf("[%d] %s", seq_len(nrow(chunks)), chunks$text),
          collapse = "\n\n"
        ),
        sep = "\n"
      )
    }
  )
)
```

**Files**: New `R/module-rag.R`

### 3.2.2 Retrieval as Tool

**Problem**: Agents need to decide when to retrieve.

**Solution**:

``` r
#' Create a retrieval tool for agents
#'
#' @param store A ragnar store
#' @param k Number of chunks (default 5)
#' @param name Tool name (default "search_documents")
#' @export
ragnar_tool <- function(store, k = 5L, name = "search_documents") {
  ellmer::tool(
    name = name,
    description = "Search the document store for relevant information",
    func = function(query) {
      chunks <- ragnar::ragnar_retrieve(store, query, k = k)
      if (nrow(chunks) == 0) return("No relevant documents found.")

      paste(
        sprintf("[%d] %s", seq_len(nrow(chunks)), chunks$text),
        collapse = "\n\n---\n\n"
      )
    }
  )
}
```

**Usage**:

``` r
# Create store
store <- ragnar::ragnar_store_create("my_docs.duckdb")
ragnar::ragnar_store_insert(store, my_chunks)

# Create agent with retrieval tool
agent <- module("question -> answer", type = "react")
agent$add_tool(ragnar_tool(store))

# Agent can now search documents
agent$run(question = "What is the company policy on remote work?")
```

**Files**: New `R/ragnar-tools.R`

### 3.2.3 Store Builder Helper

**Problem**: Building a ragnar store requires multiple steps.

**Solution**:

``` r
#' Build a ragnar store from documents
#'
#' @param paths Vector of file paths or URLs
#' @param store_path Path for DuckDB store (default in-memory)
#' @param embedding_fn Embedding function (default OpenAI)
#' @param chunk_size Target chunk size in tokens
#' @param ... Additional arguments to ragnar functions
#' @export
build_rag_store <- function(
  paths,
  store_path = ":memory:",
  embedding_fn = ragnar::embed_openai(),
  chunk_size = 512L,
  ...
) {
  # Read and convert to markdown
  docs <- lapply(paths, ragnar::read_as_markdown)

  # Chunk documents
  all_chunks <- do.call(rbind, lapply(docs, function(doc) {
    ragnar::markdown_chunk(doc, size = chunk_size)
  }))

  # Create store
  store <- ragnar::ragnar_store_create(store_path, embedding_fn = embedding_fn)

  # Insert chunks
  ragnar::ragnar_store_insert(store, all_chunks)

  # Build index
  ragnar::ragnar_store_build_index(store)

  store
}
```

**Files**: New `R/ragnar-tools.R`

------------------------------------------------------------------------

## 3.3 shinychat Integration

### Overview

shinychat provides the UI components; dsprrr provides the intelligence.
The integration should feel seamless.

### 3.3.1 Chat Handler Wrapper

**Problem**: No direct path from dsprrr module to shinychat handler.

**Solution**:

``` r
#' Create a shinychat-compatible handler from a module
#'
#' @param module A dsprrr Module
#' @param .llm Optional ellmer Chat (uses module's chat if not provided)
#' @param stream Whether to stream responses (default TRUE)
#' @param on_error Function to call on errors
#' @export
as_shinychat_handler <- function(
  module,
  .llm = NULL,
  stream = TRUE,
  on_error = NULL
) {
  if (!requireNamespace("shinychat", quietly = TRUE)) {
    cli::cli_abort("shinychat package required")
  }

  llm <- .llm %||% module$chat %||% get_default_chat(create = TRUE)

  function(user_message, chat_id = NULL) {
    # Map user message to module inputs
    # For simple modules, use first input
    input_name <- module$signature@inputs[[1]]$name
    inputs <- stats::setNames(list(user_message), input_name)

    tryCatch({
      if (stream && inherits(module, "PredictModule")) {
        # Stream response
        module$stream(
          !!!inputs,
          .llm = llm,
          callback = function(chunk) {
            shinychat::chat_append(chat_id, chunk, role = "assistant")
          }
        )
      } else {
        # Non-streaming response
        result <- module$run(!!!inputs, .llm = llm, .return_format = "simple")
        shinychat::chat_append(chat_id, as.character(result), role = "assistant")
      }
    }, error = function(e) {
      if (!is.null(on_error)) {
        on_error(e, chat_id)
      } else {
        shinychat::chat_append(
          chat_id,
          paste("Error:", e$message),
          role = "assistant"
        )
      }
    })
  }
}
```

**Files**: New `R/shinychat.R`

### 3.3.2 Ready-to-Use Chat App

**Problem**: Users want a quick way to test modules interactively.

**Solution**:

``` r
#' Launch an interactive chat app for a module
#'
#' @param module A dsprrr Module
#' @param .llm Optional ellmer Chat
#' @param title App title
#' @param ... Additional arguments to shinychat::chat_app()
#' @export
module_chat_app <- function(
  module,
  .llm = NULL,
  title = "dsprrr Chat",
  ...
) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cli::cli_abort("shiny package required")
  }
  if (!requireNamespace("shinychat", quietly = TRUE)) {
    cli::cli_abort("shinychat package required")
  }

  handler <- as_shinychat_handler(module, .llm = .llm)

  ui <- bslib::page_fluid(
    title = title,
    shinychat::chat_ui("chat")
  )

  server <- function(input, output, session) {
    shinychat::chat_server("chat", handler)
  }

  shiny::shinyApp(ui, server, ...)
}
```

**Usage**:

``` r
# Quick interactive testing
mod <- module("question -> answer", type = "predict")
module_chat_app(mod)
```

**Files**: New `R/shinychat.R`

### 3.3.3 Shiny Module for Embedding

**Problem**: Users building larger apps need a reusable Shiny module.

**Solution**:

``` r
#' UI for dsprrr chat module
#' @export
dsprrr_chat_ui <- function(id, ...) {
 ns <- shiny::NS(id)
 shinychat::chat_ui(ns("chat"), ...)
}

#' Server for dsprrr chat module
#' @export
dsprrr_chat_server <- function(id, module, .llm = NULL) {
 shiny::moduleServer(id, function(input, output, session) {
   handler <- as_shinychat_handler(module, .llm = .llm)
   shinychat::chat_server("chat", handler)
 })
}
```

**Files**: New `R/shinychat.R`

------------------------------------------------------------------------

## 3.4 Full tidymodels Integration

### Current State

dsprrr has basic integration: -
[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md)
returns a
[`dials::parameters`](https://dials.tidymodels.org/reference/parameters.html)
set -
[`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md)
can compute yardstick metrics -
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
accepts dials parameter sets

### Missing Pieces

1.  **No parsnip model specification** - Can’t use
    [`workflows::add_model()`](https://workflows.tidymodels.org/reference/add_model.html)
2.  **No tune compatibility** - Can’t use
    [`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
    or
    [`tune::tune_bayes()`](https://tune.tidymodels.org/reference/tune_bayes.html)
3.  **No recipe integration** - Can’t preprocess text
4.  **No rsample compatibility** - Can’t use cross-validation

### 3.4.1 Parsnip Model Specification

**Problem**: dsprrr modules aren’t parsnip models.

**Solution**:

``` r
#' Specify a dsprrr module for tidymodels
#'
#' @param mode Model mode (always "classification" or "regression")
#' @param engine LLM engine (e.g., "openai", "claude", "ollama")
#' @param signature Signature string or Signature object
#' @param temperature Temperature parameter (can be tuned)
#' @param ... Additional module configuration
#' @export
dsprrr_module <- function(
  mode = "classification",
  engine = NULL,
  signature = NULL,
  temperature = NULL,
  ...
) {
  args <- list(
    signature = rlang::enquo(signature),
    temperature = rlang::enquo(temperature),
    ...
  )

  parsnip::new_model_spec(
    "dsprrr_module",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}

# Register with parsnip
.onLoad <- function(libname, pkgname) {
  parsnip::set_new_model("dsprrr_module")

  parsnip::set_model_mode("dsprrr_module", "classification")
  parsnip::set_model_mode("dsprrr_module", "regression")

  parsnip::set_model_engine("dsprrr_module", mode = "classification", eng = "openai")
  parsnip::set_model_engine("dsprrr_module", mode = "classification", eng = "claude")
  parsnip::set_model_engine("dsprrr_module", mode = "classification", eng = "ollama")

  parsnip::set_dependency("dsprrr_module", "openai", "ellmer")
  parsnip::set_dependency("dsprrr_module", "claude", "ellmer")
  parsnip::set_dependency("dsprrr_module", "ollama", "ellmer")

  parsnip::set_fit(
    model = "dsprrr_module",
    eng = "openai",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("formula", "data"),
      func = c(pkg = "dsprrr", fun = "dsprrr_fit"),
      defaults = list()
    )
  )

  parsnip::set_pred(
    model = "dsprrr_module",
    eng = "openai",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data)
      )
    )
  )
}
```

**Fit function**:

``` r
#' Fit a dsprrr module (parsnip interface)
#' @keywords internal
dsprrr_fit <- function(formula, data, signature, temperature = 0.7, ...) {
  # Extract predictors and outcome
  outcome <- all.vars(formula)[1]
  predictors <- all.vars(formula)[-1]

  # Build signature if needed
  if (is.null(signature)) {
    signature <- paste(
      paste(predictors, collapse = ", "),
      "->",
      outcome
    )
  }

  # Create module
  mod <- module(signature, type = "predict", config = list(temperature = temperature))

  # Optionally compile on training data
  # (For now, just return the module)

  structure(
    list(
      fit = mod,
      formula = formula,
      predictors = predictors,
      outcome = outcome
    ),
    class = "dsprrr_fit"
  )
}
```

**Files**: New `R/tidymodels.R`

### 3.4.2 tune Workflow Compatibility

**Problem**: Can’t use
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
with dsprrr modules.

**Solution**: Implement `tunable()` method:

``` r
#' @export
tunable.dsprrr_module <- function(x, ...) {
  tibble::tibble(
    name = c("temperature", "signature"),
    call_info = list(
      list(pkg = "dials", fun = "new_quant_param", range = c(0, 1)),
      list(pkg = "dsprrr", fun = "signature_param")
    ),
    source = "model_spec",
    component = "dsprrr_module",
    component_id = "main"
  )
}

#' Custom parameter for signature variations
#' @export
signature_param <- function(values = NULL) {
  dials::new_qual_param(
    type = "character",
    values = values %||% character(),
    label = c(signature = "Prompt Signature")
  )
}
```

**Full workflow example**:

``` r
library(tidymodels)
library(dsprrr)

# Define model with tunable parameters
mod_spec <- dsprrr_module(mode = "classification") %>%
  set_engine("openai") %>%
  set_args(
    signature = "review -> sentiment: enum('positive', 'negative')",
    temperature = tune()
  )

# Create workflow
wf <- workflow() %>%
  add_model(mod_spec) %>%
  add_formula(sentiment ~ review)

# Tune with cross-validation
cv_folds <- vfold_cv(train_data, v = 3)

tune_results <- tune_grid(
  wf,
  resamples = cv_folds,
  grid = 5,
  metrics = metric_set(accuracy)
)

# Select best
best_params <- select_best(tune_results, metric = "accuracy")
final_wf <- finalize_workflow(wf, best_params)
```

**Files**: New `R/tidymodels.R`

------------------------------------------------------------------------

## 3.5 vetiver Deployment

### Overview

vetiver provides the deployment infrastructure. dsprrr needs to provide
the model interface.

### 3.5.1 vetiver Model Wrapper

**Problem**: dsprrr modules aren’t vetiver-compatible.

**Solution**:

``` r
#' Create a vetiver model from a dsprrr module
#'
#' @param module A dsprrr Module (optionally compiled)
#' @param model_name Name for the deployed model
#' @param versioned Whether to version the model (default TRUE)
#' @export
vetiver_module <- function(module, model_name, versioned = TRUE) {
  if (!requireNamespace("vetiver", quietly = TRUE)) {
    cli::cli_abort("vetiver package required")
  }

  # Capture module state for reproducibility
  module_bundle <- list(
    signature = module$signature,
    config = module$config,
    demos = if (inherits(module, "PredictModule")) module$demos else NULL,
    compiled = module$is_compiled(),
    class = class(module)
  )

  vetiver::vetiver_model(
    model = module,
    model_name = model_name,
    versioned = versioned,
    metadata = list(
      dsprrr_version = packageVersion("dsprrr"),
      signature = as.character(module$signature),
      compiled = module$is_compiled()
    )
  )
}

#' Deploy a dsprrr module as a Plumber API
#'
#' @param module A dsprrr Module
#' @param board A pins board for versioning
#' @param model_name Model name
#' @param ... Additional arguments to vetiver functions
#' @export
deploy_module <- function(module, board, model_name, ...) {
  v <- vetiver_module(module, model_name)

  # Pin the model
  vetiver::vetiver_pin_write(board, v)

  # Create Plumber API
  pr <- vetiver::vetiver_api(v)

  # Add custom endpoints
  pr$handle("POST", "/run", function(req, res) {
    inputs <- jsonlite::fromJSON(req$body)
    result <- module$run(!!!inputs, .return_format = "simple")
    list(result = result)
  })

  pr
}
```

**Files**: New `R/vetiver.R`

### 3.5.2 Deployment Helpers

``` r
#' Write a ready-to-deploy Plumber file
#'
#' @param module A dsprrr Module
#' @param path Output path for plumber.R
#' @param board_type Type of pins board ("folder", "connect", "s3")
#' @export
write_deployment_plumber <- function(
  module,
  path = "plumber.R",
  board_type = "folder"
) {
  template <- glue::glue('
# Plumber API for dsprrr module
# Generated by dsprrr::write_deployment_plumber()

library(plumber)
library(dsprrr)
library(vetiver)
library(pins)

# Load versioned model
board <- pins::board_{board_type}()
v <- vetiver::vetiver_pin_read(board, "{module_name}")
mod <- v$model

#* @post /predict
#* @serializer json
function(req) {{
  inputs <- jsonlite::fromJSON(req$postBody)
  result <- mod$run(!!!inputs, .return_format = "simple")
  list(prediction = result)
}}

#* @post /run_batch
#* @serializer json
function(req) {{
  data <- jsonlite::fromJSON(req$postBody)
  results <- run_dataset(mod, as.data.frame(data))
  as.list(results)
}}

#* @get /health
function() {{
  list(status = "healthy", model = "{module_name}")
}}
')

  writeLines(template, path)
  cli::cli_alert_success("Wrote {.file {path}}")
  invisible(path)
}
```

**Files**: New `R/vetiver.R`

------------------------------------------------------------------------

## 3.6 Production Features

### 3.6.1 Caching Layer (pins-based)

**Problem**: Repeated calls with same inputs waste API costs.

**Solution**:

``` r
#' Enable caching for a module
#'
#' @param module A dsprrr Module
#' @param board A pins board for caching
#' @param ttl Cache time-to-live (default "1 day")
#' @export
enable_caching <- function(module, board = NULL, ttl = "1 day") {
  board <- board %||% pins::board_folder(
    fs::path(Sys.getenv("HOME"), ".dsprrr", "cache")
  )

  module$config$cache_board <- board
  module$config$cache_ttl <- ttl

  # Wrap forward method
  original_forward <- module$forward

  module$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    cache_key <- private$compute_cache_key(batch)

    # Check cache
    cached <- tryCatch(
      pins::pin_read(board, cache_key),
      error = function(e) NULL
    )

    if (!is.null(cached)) {
      cli::cli_alert_info("Cache hit for {cache_key}")
      return(cached)
    }

    # Cache miss - call original
    result <- original_forward(batch, .llm = .llm, trace = trace, ...)

    # Store in cache
    tryCatch(
      pins::pin_write(board, result, cache_key),
      error = function(e) cli::cli_warn("Failed to cache: {e$message}")
    )

    result
  }

  invisible(module)
}
```

**Files**: New `R/caching.R`

### 3.6.2 Cost Tracking Dashboard

**Problem**: No aggregate view of LLM costs.

**Solution**:

``` r
#' Get usage summary across all modules
#'
#' @param modules List of modules (optional)
#' @param since Only include calls after this time
#' @export
dsp_usage_summary <- function(modules = NULL, since = Sys.time() - 86400) {
  # Aggregate from ellmer's global token usage if available
  usage <- ellmer::token_usage()

  if (nrow(usage) == 0) {
    cli::cli_alert_info("No usage data available")
    return(invisible(NULL))
  }

  # Filter by time
  usage <- usage[usage$timestamp >= since, ]

  # Summarize
  summary <- tibble::tibble(
    total_calls = nrow(usage),
    input_tokens = sum(usage$input_tokens, na.rm = TRUE),
    output_tokens = sum(usage$output_tokens, na.rm = TRUE),
    total_tokens = sum(usage$input_tokens + usage$output_tokens, na.rm = TRUE),
    estimated_cost = sum(usage$cost, na.rm = TRUE)
  )

  # By model
  by_model <- usage %>%
    dplyr::group_by(model) %>%
    dplyr::summarize(
      calls = dplyr::n(),
      tokens = sum(input_tokens + output_tokens, na.rm = TRUE),
      cost = sum(cost, na.rm = TRUE)
    )

  # Print summary
  cli::cli_h2("dsprrr Usage Summary")
  cli::cli_text("Total calls: {summary$total_calls}")
  cli::cli_text("Total tokens: {format(summary$total_tokens, big.mark = ',')}")
  cli::cli_text("Estimated cost: ${format(summary$estimated_cost, digits = 2)}")

  cli::cli_h3("By Model")
  for (i in seq_len(nrow(by_model))) {
    cli::cli_text(
      "  {by_model$model[i]}: {by_model$calls[i]} calls (${format(by_model$cost[i], digits = 2)})"
    )
  }

  invisible(list(summary = summary, by_model = by_model))
}
```

**Files**: New `R/usage.R`

------------------------------------------------------------------------

## Implementation Checklist

### Phase 3.0: Reasoning Model Support (Priority - 2-3 days)

Implement
[`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md)
detection helper

Implement `get_tunable_params()` for model-aware optimization

Update
[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md)
to exclude temperature for reasoning models

Implement
[`provider_defaults()`](https://jameshwade.github.io/dsprrr/reference/provider_defaults.md)
helper

Add `dsp_usage_summary()` wrapper around ellmer’s `token_usage()`

Add tests for GPT-5 and o-series model detection

Update documentation to explain reasoning model differences

### Phase 3.1: ellmer Deep Integration (1 week)

Integrate `parallel_chat_structured()` into
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)

Implement
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md)
for module-as-tool pattern

Add provider-aware defaults helper

Update async functions to use ellmer’s native async

Add tests for batch processing

### Phase 3.2: ragnar Integration (1 week)

Create `RAGModule` R6 class

Implement
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md)
factory function

Create
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md)
for agent retrieval

Implement `build_rag_store()` helper

Add vignette: “Retrieval-Augmented Generation with dsprrr”

Add tests with mock store

### Phase 3.3: shinychat Integration (3-4 days)

Implement `as_shinychat_handler()`

Create `module_chat_app()` for quick testing

Add Shiny module pair (`dsprrr_chat_ui/server`)

Add vignette: “Building Chat UIs with dsprrr”

Add example apps in `inst/examples/`

### Phase 3.4: tidymodels Full Integration (1-2 weeks)

Create parsnip model specification (`dsprrr_module()`)

Implement `tunable.dsprrr_module()`

Register engines (openai, claude, ollama)

Implement fit and predict methods

Add rsample integration tests

Add vignette: “dsprrr + tidymodels: Complete Guide”

### Phase 3.5: vetiver Deployment (3-4 days)

Implement `vetiver_module()` wrapper

Create `deploy_module()` helper

Implement `write_deployment_plumber()`

Add vignette: “Deploying dsprrr with vetiver”

Add example deployment in `inst/deploy/`

### Phase 3.6: Production Features (1 week)

Implement pins-based caching layer

Create `dsp_usage_summary()` dashboard

Add retry/rate limiting configuration

Add observability hooks (OpenTelemetry optional)

Update troubleshooting vignette

------------------------------------------------------------------------

## Dependencies

### New Dependencies (Suggested)

``` r
Suggests:
  ragnar,
  shinychat,
  vetiver,
  bslib  # For shinychat apps
```

### Notes

- All new dependencies are suggested, not required
- Each integration gracefully degrades without its package
- Core dsprrr functionality remains independent

------------------------------------------------------------------------

## Success Criteria

1.  **ellmer**: Modules use batch/parallel chat when beneficial
2.  **ragnar**: Users can create RAG modules in 5 lines of code
3.  **shinychat**: Interactive testing via `module_chat_app()` works
4.  **tidymodels**: Full tune workflow example runs successfully
5.  **vetiver**: Deployment to Posit Connect documented and tested
6.  **Production**: Caching reduces costs measurably in benchmarks

------------------------------------------------------------------------

## References

- [ellmer documentation](https://ellmer.tidyverse.org/)
- [ragnar documentation](https://ragnar.tidyverse.org/)
- [shinychat documentation](https://posit-dev.github.io/shinychat/r/)
- [tidymodels: How to build a parsnip
  model](https://www.tidymodels.org/learn/develop/models/)
- [vetiver documentation](https://rstudio.github.io/vetiver-r/)
- [vitals documentation](https://github.com/tidyverse/vitals)

------------------------------------------------------------------------

## Labels

`enhancement`, `ecosystem`, `phase-3`, `integration`
