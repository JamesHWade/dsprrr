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
  : Execute Module on Data
- [`predict(`*`<Module>`*`)`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md)
  [`predict(`*`<PredictModule>`*`)`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md)
  : Predict Method for Modules (tidymodels-style)
- [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
  : Evaluate a DSPrrr module
- [`evaluate_dsp()`](https://jameshwade.github.io/dsprrr/reference/evaluate_dsp.md)
  : Evaluate a Compiled Module

## Advanced Reasoning Modules

DSPy-inspired reasoning patterns for improved accuracy

- [`signature-transforms`](https://jameshwade.github.io/dsprrr/reference/signature-transforms.md)
  : Signature Transforms for Advanced Reasoning Modules
- [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md)
  : Add Chain-of-Thought Reasoning to a Signature
- [`without_reasoning()`](https://jameshwade.github.io/dsprrr/reference/without_reasoning.md)
  : Remove Chain-of-Thought from a Signature
- [`has_reasoning()`](https://jameshwade.github.io/dsprrr/reference/has_reasoning.md)
  : Check if a Signature has Chain-of-Thought
- [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
  : Create a Chain-of-Thought Module
- [`module-wrapper`](https://jameshwade.github.io/dsprrr/reference/module-wrapper.md)
  : Wrapper Modules for Advanced Reasoning Patterns
- [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md)
  : Create a BestOfN Wrapper Module
- [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md)
  : Create a Refine Wrapper Module
- [`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md)
  : Convert a Metric to a Reward Function
- [`module-multichain`](https://jameshwade.github.io/dsprrr/reference/module-multichain.md)
  : MultiChainComparison Module
- [`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md)
  : Create a MultiChainComparison Module
- [`module-ensemble`](https://jameshwade.github.io/dsprrr/reference/module-ensemble.md)
  : Ensemble Module for Combining Multiple Modules
- [`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble_module.md)
  : Create an Ensemble Module
- [`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md)
  : Majority Vote Reducer
- [`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md)
  : Weighted Vote Reducer
- [`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md)
  : First Successful Output Reducer
- [`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md)
  : Best by Metric Reducer

## Code Execution Modules

Execute LLM-generated R code safely

- [`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
  : Create an R Code Runner
- [`r-code-runner`](https://jameshwade.github.io/dsprrr/reference/r-code-runner.md)
  : R Code Execution Backend
- [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md)
  : Create a Program of Thought Module
- [`module-program-of-thought`](https://jameshwade.github.io/dsprrr/reference/module-program-of-thought.md)
  : Program of Thought Module
- [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md)
  : Create a CodeAct Module
- [`module-codeact`](https://jameshwade.github.io/dsprrr/reference/module-codeact.md)
  : CodeAct Module

## Chat-Centric API

ellmer-style pipe-friendly functions

- [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md) :
  Declarative Structured Prediction
- [`as_module()`](https://jameshwade.github.io/dsprrr/reference/as_module.md)
  : Create a Module from a Chat
- [`get_last_trace()`](https://jameshwade.github.io/dsprrr/reference/get_last_trace.md)
  : Get the Last DSP Trace

## Configuration

Setup and configure dsprrr

- [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md)
  : Configure dsprrr Default Settings
- [`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)
  : dsprrr Situation Report
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
- [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
  : BootstrapFewShot Teleprompter
- [`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md)
  : BootstrapFewShotWithRandomSearch Teleprompter
- [`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md)
  : KNNFewShot Teleprompter
- [`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md) :
  SIMBA Teleprompter
- [`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md) :
  GEPA Teleprompter
- [`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md)
  : MIPROv2 Teleprompter
- [`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md) :
  COPRO Teleprompter
- [`teleprompter-ensemble`](https://jameshwade.github.io/dsprrr/reference/teleprompter-ensemble.md)
  : Ensemble Teleprompter
- [`Ensemble()`](https://jameshwade.github.io/dsprrr/reference/Ensemble.md)
  : Ensemble Teleprompter
- [`ensemble_from_programs()`](https://jameshwade.github.io/dsprrr/reference/ensemble_from_programs.md)
  : Compile Programs into an Ensemble

## Optimization

Functions for optimizing module performance

- [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
  : Grid Search Optimisation
- [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  : Compile S7 Generic and Methods
- [`compile_module()`](https://jameshwade.github.io/dsprrr/reference/compile_module.md)
  : Compile a DSPrrr Program
- [`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md)
  : Suggest tidymodels parameters for a module
- [`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md)
  : Summarise optimisation trials for a module
- [`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md)
  : Summarise optimisation metrics per trial

## Optimizer Accessors

Convenience functions for working with optimization results

- [`optimizer-accessors`](https://jameshwade.github.io/dsprrr/reference/optimizer-accessors.md)
  : Optimizer Convenience Functions
- [`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md)
  : Extract Best Parameters from a Module
- [`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md)
  : Extract Best Demos from a Compiled Module
- [`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md)
  : Apply Best Configuration from One Module to Another
- [`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)
  : Get Top Performing Trials
- [`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md)
  : Compare Module Configuration Before and After Optimization
- [`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md)
  : Export Module Configuration as R Code
- [`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md)
  : Get Optimization Summary
- [`print(`*`<dsprrr_optimization_summary>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.dsprrr_optimization_summary.md)
  : Print method for optimization summary

## Optimizer Infrastructure

Low-level optimizer building blocks

- [`OptimizerControl()`](https://jameshwade.github.io/dsprrr/reference/OptimizerControl.md)
  : Optimizer Control Parameters
- [`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md)
  : Create Optimizer Control
- [`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md)
  : Evaluate a Program on a Dataset
- [`sample_dataset()`](https://jameshwade.github.io/dsprrr/reference/sample_dataset.md)
  : Sample from a Dataset Deterministically
- [`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md)
  : Split Dataset into Train and Validation Sets
- [`Trial()`](https://jameshwade.github.io/dsprrr/reference/Trial.md) :
  Trial Record
- [`TrialLog`](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)
  : Trial Log
- [`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md)
  : Create a Trial Record
- [`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md)
  : Complete a Trial
- [`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)
  : Write Trials to JSONL File
- [`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md)
  : Read Trials from JSONL File
- [`load_trial_log()`](https://jameshwade.github.io/dsprrr/reference/load_trial_log.md)
  : Load Trial Log from Directory

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
- [`metric_model_graded_qa()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
  [`metric_model_graded_fact()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
  [`metric_detect_match()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
  [`metric_detect_includes()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
  [`metric_detect_pattern()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
  : Pre-built Vitals-backed Metrics

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

## Debugging & Inspection

Prompt inspection and history

- [`prompt-visibility`](https://jameshwade.github.io/dsprrr/reference/prompt-visibility.md)
  : Prompt Visibility and Inspection
- [`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md)
  : Get the Last Prompt
- [`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md)
  : Inspect LLM Call History
- [`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md)
  : Clear Prompt History

## Result Accessors

Extract data from structured results

- [`accessors`](https://jameshwade.github.io/dsprrr/reference/accessors.md)
  : Accessor Functions for DSPrrr Results
- [`get_output()`](https://jameshwade.github.io/dsprrr/reference/get_output.md)
  : Get output from a result
- [`get_metadata()`](https://jameshwade.github.io/dsprrr/reference/get_metadata.md)
  : Get metadata from a result
- [`get_tokens()`](https://jameshwade.github.io/dsprrr/reference/get_tokens.md)
  : Get token counts from a result
- [`get_cost()`](https://jameshwade.github.io/dsprrr/reference/get_cost.md)
  : Get cost from a result
- [`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md)
  : Session Cost Summary
- [`print(`*`<dsprrr_evaluation>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.dsprrr_evaluation.md)
  : Print method for dsprrr_evaluation
- [`print(`*`<dsprrr_batch_result>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.dsprrr_batch_result.md)
  : Print method for dsprrr_batch_result
- [`print(`*`<dsprrr_cost_summary>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.dsprrr_cost_summary.md)
  : Print method for dsprrr_cost_summary
- [`print(`*`<EvalResult>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.EvalResult.md)
  : Print method for EvalResult
- [`print(`*`<Trial>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.Trial.md)
  : Print method for Trial
- [`print(`*`<BootstrapFewShot>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.BootstrapFewShot.md)
  : Print method for BootstrapFewShot
- [`print(`*`<BootstrapFewShotWithRandomSearch>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.BootstrapFewShotWithRandomSearch.md)
  : Print method for BootstrapFewShotWithRandomSearch
- [`print(`*`<GEPA>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.GEPA.md)
  : Print method for GEPA
- [`print(`*`<SIMBA>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.SIMBA.md)
  : Print method for SIMBA
- [`print(`*`<COPRO>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.COPRO.md)
  : Print method for COPRO

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
- [`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md)
  : Create a vitals Task from a dsprrr module
- [`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md)
  : Convert dsprrr cost data to vitals format
- [`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md)
  : Convert dsprrr traces to vitals samples format
- [`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md)
  : Convert vitals samples to dsprrr traces format
- [`summarize_traces_df()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces_df.md)
  : Summarize a traces data frame
- [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)
  : Adapt a vitals scorer for use as a dsprrr metric
- [`metric_model_graded_qa()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
  [`metric_model_graded_fact()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
  [`metric_detect_match()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
  [`metric_detect_includes()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
  [`metric_detect_pattern()`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)
  : Pre-built Vitals-backed Metrics

## ellmer Integration

Deep integration with ellmer

- [`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md)
  : Convert a DSPrrr Module to an ellmer Tool
- [`register_dsprrr_tool()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_tool.md)
  : Register a DSPrrr Module as a Tool in a Chat

## RAG Integration

Retrieval-Augmented Generation with ragnar

- [`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md)
  : Create a RAG Module
- [`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md)
  : Create a ragnar Search Tool for ReAct Modules
- [`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md)
  : Create a Semantic Search Tool from Documents
- [`print(`*`<ragnar_tool>`*`)`](https://jameshwade.github.io/dsprrr/reference/print.ragnar_tool.md)
  : Print method for ragnar_tool

## tidymodels Integration

parsnip model specification and dials parameters

- [`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md)
  : LLM Prediction Model Specification
- [`register_dsprrr_engine()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_engine.md)
  : Register dsprrr Engine with parsnip
- [`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md)
  : Temperature Parameter for dials
- [`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md) :
  Top-p Parameter for dials
- [`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md)
  : Reasoning Effort Parameter for dials

## Model Utilities

Model detection and provider defaults

- [`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md)
  : Check if a model is a reasoning model
- [`provider_defaults()`](https://jameshwade.github.io/dsprrr/reference/provider_defaults.md)
  : Get default parameters for a provider

## Utilities

Helper functions

- [`eval_vignette()`](https://jameshwade.github.io/dsprrr/reference/eval_vignette.md)
  : Determine if vignettes should be evaluated
- [`has_ellmer_credentials()`](https://jameshwade.github.io/dsprrr/reference/has_ellmer_credentials.md)
  : Check for ellmer credentials
