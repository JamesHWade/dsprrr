# _targets.R - dsprrr Workflow Pipeline
#
# This template demonstrates a production LLM workflow using targets + dsprrr.
# The pipeline:
#   1. Loads and prepares data
#   2. Defines and optimizes a dsprrr module
#   3. Evaluates the module on a test set
#   4. Optionally runs vitals evaluation
#   5. Persists results using pins
#   6. Renders a Quarto report
#
# Usage:
#   1. Copy this file to your project root
#   2. Customize the data loading and module definition
#   3. Run: targets::tar_make()
#   4. View: targets::tar_read(evaluation_results)
#
# Documentation: https://docs.ropensci.org/targets/

library(targets)
library(tarchetypes)

# Set target options
tar_option_set(
  packages = c("dsprrr", "ellmer", "tibble"),
  format = "rds"
)

# Source custom functions (if any)
# tar_source("R/")

# ============================================================================
# Configuration
# ============================================================================

# LLM provider configuration
# Set this environment variable or customize dsprrr_targets_llm() below.
LLM_MODEL <- Sys.getenv("DSPRRR_MODEL", "claude-sonnet-4-5-20250929")

# Keep provider construction outside the graph so deployments and tests can
# replace it without editing target definitions. A custom factory receives the
# configured model name and must return an ellmer Chat object.
dsprrr_targets_llm <- function() {
  factory <- getOption("dsprrr.targets.llm_factory")

  if (is.null(factory)) {
    return(ellmer::chat_anthropic(model = LLM_MODEL))
  }
  if (!is.function(factory)) {
    stop("Option 'dsprrr.targets.llm_factory' must be a function.")
  }

  factory(model = LLM_MODEL)
}

# Pins board for persisting results
PINS_BOARD_PATH <- "pins"

# ============================================================================
# Pipeline Definition
# ============================================================================

list(
  # --------------------------------------------------------------------------
  # Data Preparation
  # --------------------------------------------------------------------------

  # Load training data
  tar_target(
    train_data,
    {
      # CUSTOMIZE: Replace with your data loading logic
      # Example: readr::read_csv("data/train.csv")
      tibble::tibble(
        text = c(
          "This product is amazing! Best purchase ever.",
          "Terrible experience, would not recommend.",
          "It's okay, nothing special.",
          "Absolutely love it, exceeded expectations!",
          "Worst product I've bought, complete waste.",
          "Decent quality for the price."
        ),
        target = c(
          "positive",
          "negative",
          "neutral",
          "positive",
          "negative",
          "neutral"
        )
      )
    }
  ),

  # Load test data
  tar_target(
    test_data,
    {
      # CUSTOMIZE: Replace with your test data
      tibble::tibble(
        text = c(
          "Great value, highly recommend!",
          "Disappointing quality, not worth it.",
          "Works as expected, no complaints."
        ),
        target = c("positive", "negative", "neutral")
      )
    }
  ),

  # --------------------------------------------------------------------------
  # Module Definition
  # --------------------------------------------------------------------------

  # Define the module signature and structure
  tar_target(
    module_definition,
    {
      dsprrr::signature(
        "text -> sentiment: enum('positive', 'negative', 'neutral')",
        instructions = "Classify the sentiment of the given text."
      ) |>
        dsprrr::module(
          type = "predict",
          template = "Analyze the sentiment of this text:\n\n{text}"
        )
    }
  ),

  # Create LLM client
  # Note: This is created as a target so it's only instantiated when needed
  tar_target(
    llm_client,
    {
      # CUSTOMIZE: edit dsprrr_targets_llm() for chat_openai(),
      # chat_ollama(), or another ellmer provider.
      dsprrr_targets_llm()
    },
    # Recreate the stateful Chat for every pipeline run. targets still stores
    # this value during the run so downstream targets share the same client.
    cue = tar_cue(mode = "always")
  ),

  # --------------------------------------------------------------------------
  # Optimization (Optional)
  # --------------------------------------------------------------------------

  # Optimize the module on training data
  tar_target(
    optimized_module,
    {
      mod <- module_definition$clone(deep = TRUE)

      # Define the evaluation metric
      metric <- function(prediction, expected_row) {
        as.numeric(
          tolower(as.character(prediction)) ==
            tolower(as.character(expected_row$target))
        )
      }

      # Run optimization
      dsprrr::optimize_grid(
        mod,
        data = train_data,
        metric = metric,
        parameters = list(
          temperature = c(0.0, 0.3, 0.7)
        ),
        .llm = llm_client,
        control = list(
          progress = TRUE,
          parallel = FALSE
        )
      )

      mod
    }
  ),

  # --------------------------------------------------------------------------
  # Evaluation
  # --------------------------------------------------------------------------

  # Evaluate on test set
  tar_target(
    evaluation_results,
    {
      metric <- function(prediction, expected_row) {
        as.numeric(
          tolower(as.character(prediction)) ==
            tolower(as.character(expected_row$target))
        )
      }

      dsprrr::evaluate(
        optimized_module,
        data = test_data,
        metric = metric,
        .llm = llm_client,
        .progress = TRUE
      )
    }
  ),

  # --------------------------------------------------------------------------
  # Vitals Integration (Optional)
  # --------------------------------------------------------------------------

  # Uncomment to enable vitals evaluation
  # tar_target(
  #   vitals_solver,
  #   dsprrr::as_vitals_solver(optimized_module)
  # ),
  #
  # tar_target(
  #   vitals_task,
  #   {
  #     rlang::check_installed("vitals")
  #     vitals::Task$new(
  #       dataset = vitals::as_dataset(test_data, input = "text", target = "target"),
  #       solver = vitals_solver,
  #       scorer = vitals::scorer_exact_match()
  #     )
  #   }
  # ),
  #
  # tar_target(
  #   vitals_results,
  #   vitals_task$run()
  # ),

  # --------------------------------------------------------------------------
  # Persistence
  # --------------------------------------------------------------------------

  # Create pins board
  tar_target(
    pins_board,
    {
      rlang::check_installed("pins")
      pins::board_folder(PINS_BOARD_PATH, versioned = TRUE)
    }
  ),

  # Pin the optimized module configuration
  tar_target(
    pinned_config,
    {
      dsprrr::pin_module_config(
        board = pins_board,
        name = "sentiment-classifier",
        module = optimized_module,
        description = paste("Optimized at", Sys.time())
      )
    }
  ),

  # Pin traces from evaluation
  tar_target(
    pinned_traces,
    {
      dsprrr::pin_trace(
        board = pins_board,
        name = "sentiment-eval-traces",
        module = optimized_module,
        include_prompts = TRUE,
        description = paste("Evaluation traces from", Sys.time())
      )
    }
  ),

  # Pin evaluation results
  tar_target(
    pinned_evaluation,
    {
      dsprrr::pin_vitals_log(
        board = pins_board,
        name = "sentiment-eval-results",
        eval_result = evaluation_results,
        module = optimized_module,
        description = paste("Test evaluation from", Sys.time())
      )
    }
  ),

  # --------------------------------------------------------------------------
  # Reporting
  # --------------------------------------------------------------------------

  # Generate summary statistics
  tar_target(
    summary_stats,
    {
      list(
        model = LLM_MODEL,
        n_train = nrow(train_data),
        n_test = nrow(test_data),
        accuracy = evaluation_results$mean_score,
        n_evaluated = evaluation_results$n_evaluated,
        n_errors = evaluation_results$n_errors,
        is_compiled = optimized_module$is_compiled(),
        best_params = if (optimized_module$is_compiled()) {
          optimized_module$state$best_params
        } else {
          NULL
        },
        timestamp = Sys.time()
      )
    }
  ),

  # Render Quarto report (if report.qmd exists)
  # tar_quarto(
  #   report,
  #   path = "report.qmd",
  #   quiet = FALSE
  # )

  # Alternative: Just save summary as JSON for external reporting
  tar_target(
    summary_json,
    {
      path <- file.path("outputs", "summary.json")
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(
        summary_stats,
        path,
        auto_unbox = TRUE,
        pretty = TRUE
      )
      path
    },
    format = "file"
  )
)
