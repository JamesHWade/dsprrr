# Package index

## Core Functions

Main functions for building LLM applications

- [`Signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
  : Create a Signature for LLM Operations
- [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  : Create an LLM Module
- [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) :
  Execute an LLM Module
- [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
  : Execute Module on Dataset
- [`predict(`*`<Module>`*`)`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md)
  [`predict(`*`<PredictModule>`*`)`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md)
  : Predict Method for Modules (tidymodels-style)
- [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
  : Evaluate a DSPrrr module
- [`evaluate_dsp()`](https://jameshwade.github.io/dsprrr/reference/evaluate_dsp.md)
  : Evaluate a Compiled Module

## Chat-Centric API

ellmer-style pipe-friendly functions

- [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md) :
  Declarative Structured Prediction
- [`as_module()`](https://jameshwade.github.io/dsprrr/reference/as_module.md)
  : Create a Module from a Chat
- [`last_trace()`](https://jameshwade.github.io/dsprrr/reference/last_trace.md)
  : Get the Last DSP Trace

## Default Chat Management

Automatic LLM client configuration

- [`default-chat`](https://jameshwade.github.io/dsprrr/reference/default-chat.md)
  : Default Chat Configuration
- [`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md)
  : Get the Default Chat
- [`set_default_chat()`](https://jameshwade.github.io/dsprrr/reference/set_default_chat.md)
  : Set the Default Chat
- [`clear_default_chat()`](https://jameshwade.github.io/dsprrr/reference/clear_default_chat.md)
  : Clear Cached Default Chat

## Async and Streaming

Asynchronous and streaming operations

- [`async`](https://jameshwade.github.io/dsprrr/reference/async.md) :
  Asynchronous Module Operations
- [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
  : Run a module asynchronously
- [`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)
  : Stream module output asynchronously

## Signatures and Inputs

Define module interfaces

- [`input()`](https://jameshwade.github.io/dsprrr/reference/input.md) :
  Create an input specification for a Signature
- [`input_string()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md)
  [`input_number()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md)
  [`input_boolean()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md)
  [`input_integer()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md)
  [`input_enum()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md)
  [`input_array()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md)
  [`input_object()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md)
  : Create typed input helpers for common cases
- [`dsp_trainset()`](https://jameshwade.github.io/dsprrr/reference/dsp_trainset.md)
  : Create Training Data for DSPrrr

## Teleprompters

Optimization strategies

- [`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md)
  : Teleprompter Base Class
- [`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md)
  : LabeledFewShot Teleprompter
- [`GridSearchTeleprompter`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md)
  : GridSearchTeleprompter

## Optimization

Functions for optimizing module performance

- [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
  : Grid Search Optimisation
- [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  : Compile S7 Generic and Methods
- [`compile_module()`](https://jameshwade.github.io/dsprrr/reference/compile_module.md)
  : Compile a DSPrrr Program
- [`module_parameter_set()`](https://jameshwade.github.io/dsprrr/reference/module_parameter_set.md)
  : Suggest tidymodels parameters for a module
- [`module_trials_summary()`](https://jameshwade.github.io/dsprrr/reference/module_trials_summary.md)
  : Summarise optimisation trials for a module
- [`module_metric_summary()`](https://jameshwade.github.io/dsprrr/reference/module_metric_summary.md)
  : Summarise optimisation metrics per trial

## Metrics

Evaluation metrics

- [`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md)
  : Create a Contains Metric
- [`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md)
  : Create a Custom Metric
- [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md)
  : Create an Exact Match Metric
- [`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md)
  : Create an F1 Score Metric
- [`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md)
  : Create a Field Equality Metric
- [`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md)
  : Create a Threshold Metric

## Orchestration

Production workflow helpers

- [`orchestration`](https://jameshwade.github.io/dsprrr/reference/orchestration.md)
  : Orchestration Helpers for Production Workflows
- [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)
  : Pin a Module Configuration
- [`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)
  : Restore a Module from Pinned Configuration
- [`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md)
  : Pin Module Traces
- [`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md)
  : Pin Vitals Evaluation Log
- [`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md)
  : Use dsprrr Workflow Templates
- [`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md)
  : Validate Workflow Configuration

## Traces

Execution trace utilities

- [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md)
  : Export Module Traces
- [`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md)
  : Summarize Module Traces
- [`clear_traces()`](https://jameshwade.github.io/dsprrr/reference/clear_traces.md)
  : Clear Module Traces

## Vitals Integration

Integration with vitals package

- [`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md)
  : Convert a dsprrr module into a vitals solver
- [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)
  : Adapt a vitals scorer for use as a dsprrr metric

## Utilities

Helper functions

- [`eval_vignette()`](https://jameshwade.github.io/dsprrr/reference/eval_vignette.md)
  : Determine if vignettes should be evaluated
- [`has_ellmer_credentials()`](https://jameshwade.github.io/dsprrr/reference/has_ellmer_credentials.md)
  : Check for ellmer credentials
