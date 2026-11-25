# Changelog

## dsprrr 0.1.0

Initial CRAN release. `dsprrr` is experimental and its API may still
evolve.

### Core programming model

- Declare LLM tasks with typed inputs, outputs, and instructions using
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  [`input()`](https://jameshwade.github.io/dsprrr/reference/input.md),
  [`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md),
  [`append_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md),
  and
  [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md).
- Turn signatures into runnable programs with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
  [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
  (supporting `"rationale"`, `"plan"`, and `"native"` reasoning modes),
  [`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
  [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md)
  (including tool-loop `finalization = "submit"`),
  [`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
  [`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
  and code-executing modules
  ([`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
  [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md)).
- Compose and harden programs with
  [`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
  / `%>>%`,
  [`step()`](https://jameshwade.github.io/dsprrr/reference/step.md),
  [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
  [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
  [`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
  and output assertions
  ([`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md),
  [`assert_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md),
  [`suggest_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)).
- Run programs on single inputs, vectors, or data frames with
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
  [`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md)
  (sequential, `ellmer` parallel, and `mirai` backends),
  [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
  [`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
  and
  [`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md).
- Manage chats, portable conversation histories, and two-tier response
  caching with
  [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
  [`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md),
  [`local_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md),
  [`conversation_history()`](https://jameshwade.github.io/dsprrr/reference/conversation_history.md),
  [`save_history()`](https://jameshwade.github.io/dsprrr/reference/save_history.md)
  /
  [`load_history()`](https://jameshwade.github.io/dsprrr/reference/load_history.md),
  [`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md),
  [`cache_stats()`](https://jameshwade.github.io/dsprrr/reference/cache_stats.md),
  and
  [`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md).

### Evaluation and optimization

- Measure program quality with
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
  built-in metrics
  ([`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
  [`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md),
  [`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md),
  [`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
  [`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md),
  [`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md),
  [`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md),
  [`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md)),
  and `vitals` bridges
  ([`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
  [`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md),
  [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)).
- Optimize demonstrations, instructions, and request settings with
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  and teleprompters
  ([`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
  [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
  [`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
  [`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
  [`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
  [`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
  [`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
  [`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
  [`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
  [`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md))
  or
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).
- Inspect results, traces, prompts, and spend with
  [`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md),
  [`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md),
  [`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
  [`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
  [`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md),
  [`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
  [`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md),
  [`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md),
  [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md),
  [`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md),
  and
  [`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md).

### Persistence and integrations

- Save and restore versioned program artifacts with
  [`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md),
  [`load_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md),
  [`program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md),
  [`program_artifact_id()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md),
  [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
  [`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md),
  and
  [`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md).
- Integrate with the wider R ecosystem via
  [`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
  [`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md),
  [`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md),
  [`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
  [`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md),
  [`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md)
  (`parsnip`), and `dials` parameters
  ([`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
  [`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md),
  [`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md)).

### Experimental features

- Recursive Language Models
  ([`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md),
  [`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
  [`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
  [`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md),
  [`run_demo()`](https://jameshwade.github.io/dsprrr/reference/run_demo.md)),
  whole-program optimization
  ([`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)),
  calibrated decision outputs
  ([`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md),
  [`decision_bool()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md),
  [`decision_score()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md),
  [`decision_choice()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md),
  [`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md)),
  and meta/agentic optimizers
  ([`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
  [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
  [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)).

### Requirements

- Requires R \>= 4.1.0 and `ellmer` \>= 0.5.0.
- Installing from source requires a C compiler (used for exact
  filesystem identity checks in local persistence guards).
