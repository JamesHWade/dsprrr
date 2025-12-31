# tidymodels Integration

``` r
library(dsprrr)
library(ellmer)
# library(parsnip)
# library(tune)
# library(dials)
```

## Introduction

dsprrr provides full integration with the tidymodels ecosystem, allowing
you to use LLM-based predictions alongside traditional machine learning
models. This enables:

- Consistent model specification with parsnip
- Hyperparameter tuning with tune
- Cross-validation with rsample
- Workflow composition with workflows

## The `llm_predict` Model Specification

dsprrr registers an `llm_predict` model type with parsnip:

``` r
# Create an LLM prediction model specification
llm_spec <- llm_predict(
  mode = "classification",
  signature = "text -> sentiment: enum('positive', 'negative', 'neutral')"
) |>
  set_engine("dsprrr", model = "gpt-4o-mini")

print(llm_spec)
```

## Fitting Models

Use standard parsnip fitting:

``` r
# Training data (for classification, y determines output levels)
train_data <- tibble::tibble(
  text = c(
    "I love this product!",
    "This is terrible",
    "It's okay I guess"
  ),
  sentiment = factor(c("positive", "negative", "neutral"))
)

# Fit the model
llm_fit <- llm_spec |>
  fit(sentiment ~ text, data = train_data)
```

## Making Predictions

``` r
# New data
new_data <- tibble::tibble(
  text = c("Amazing quality!", "Worst purchase ever")
)

# Predict
predictions <- predict(llm_fit, new_data)
predictions
```

## Hyperparameter Tuning

dsprrr provides dials-compatible parameter functions:

``` r
# Temperature parameter
temperature()
temperature(range = c(0.1, 0.9))

# Top-p parameter
top_p()
top_p(range = c(0.5, 1.0))

# Reasoning effort (for reasoning models)
reasoning_effort()
```

### Tuning with tune

``` r
# Create a tunable specification
llm_spec_tune <- llm_predict(
  mode = "classification",
  signature = "text -> sentiment",
  temperature = tune()
) |>
  set_engine("dsprrr", model = "gpt-4o-mini")

# Create resampling folds
# library(rsample)
# folds <- vfold_cv(train_data, v = 3)

# Define grid
# grid <- grid_regular(temperature(), levels = 3)

# Tune
# tune_results <- tune_grid(
#   llm_spec_tune,
#   sentiment ~ text,
#   resamples = folds,
#   grid = grid
# )
```

## Using with Workflows

Combine LLM predictions with preprocessing:

``` r
# library(workflows)
# library(recipes)

# Create a recipe (if needed for preprocessing)
# rec <- recipe(sentiment ~ text, data = train_data)

# Create workflow
# wf <- workflow() |>
#   add_recipe(rec) |>
#   add_model(llm_spec)

# Fit workflow
# wf_fit <- fit(wf, data = train_data)
```

## dsprrr’s Native Optimization

While tidymodels integration is powerful, dsprrr also provides its own
optimization:

``` r
# Create module directly
mod <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')"),
  type = "predict"
)

# Use module_parameters() to get dials-compatible parameters
params <- module_parameters(mod)
params

# For reasoning models, parameters auto-adjust
params_reasoning <- module_parameters(mod, model = "o3")
params_reasoning
```

### Grid Search with dsprrr

``` r
train_data <- tibble::tibble(
  text = c("Great!", "Terrible!", "Okay"),
  target = c("positive", "negative", "neutral")
)

# Native grid search
# optimize_grid(
#   mod,
#   data = train_data,
#   metric = metric_exact_match(),
#   parameters = list(temperature = c(0.3, 0.7, 1.0))
# )

# View results
# module_trials(mod)
# module_metrics(mod)
```

## Comparison: tidymodels vs dsprrr Native

| Feature          | tidymodels    | dsprrr Native      |
|------------------|---------------|--------------------|
| Cross-validation | Built-in      | Manual             |
| Preprocessing    | recipes       | Manual             |
| Grid search      | tune_grid()   | optimize_grid()    |
| Random search    | tune_bayes()  | Not yet            |
| Metrics          | yardstick     | Custom + yardstick |
| Reasoning models | Manual config | Auto-detect        |

**When to use tidymodels:** - Need cross-validation - Complex
preprocessing pipelines - Ensemble with traditional ML models - Familiar
with tidymodels API

**When to use dsprrr native:** - Quick optimization - Prompt/instruction
tuning - Teleprompter-based optimization - Reasoning model support

## Advanced: Custom Model Engines

Register custom engines for specific providers:

``` r
# dsprrr auto-registers on package load
# But you can manually register:
register_dsprrr_engine()

# Check registered engines
# parsnip::show_model_info("llm_predict")
```

## Integration with vitals

dsprrr also integrates with vitals for LLM evaluation:

``` r
# library(vitals)

# Convert module to vitals solver
# solver <- as_vitals_solver(mod)

# Use with vitals evaluation framework
# results <- vitals::evaluate(solver, test_data)

# Convert vitals scorer to dsprrr metric
# my_metric <- as_dsprrr_metric(vitals::exact_match)
```

## Summary

tidymodels integration provides:

1.  **[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md)**:
    parsnip model specification for LLM predictions
2.  **dials parameters**:
    [`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
    [`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md),
    [`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md)
3.  **Workflow compatibility**: Works with recipes, workflows, and
    rsample
4.  **tune integration**: Hyperparameter tuning with
    [`tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)

Choose between tidymodels for full ML ecosystem integration or dsprrr’s
native optimization for LLM-specific features like: - Prompt
optimization - Teleprompter compilation - Reasoning model
auto-detection - Module persistence with pins
