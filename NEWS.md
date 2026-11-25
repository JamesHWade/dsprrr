# dsprrr 0.1.0

Initial CRAN release. `dsprrr` is experimental and its API may still evolve.

* Keep examples and tests compatible with CRAN temporary directories without
  relaxing the persistent-storage guards.
* Run a focused set of core and storage checks on CRAN; developer CI still
  runs the full suite.
* Cite the upstream source for the Omni announcement through a pinned link.

## Core programming model

* Declare LLM tasks with typed inputs, outputs, and instructions using
  `signature()`, `input()`, `with_instructions()`, `append_instructions()`,
  and `with_reasoning()`.
* Turn signatures into runnable programs with `module()`, `chain_of_thought()`
  (supporting `"rationale"`, `"plan"`, and `"native"` reasoning modes),
  `multi_chain_comparison()`, `react()` (including tool-loop `finalization = "submit"`),
  `rag_module()`, `module_fn()`, and code-executing modules (`program_of_thought()`,
  `code_act()`).
* Compose and harden programs with `pipeline()` / `%>>%`, `step()`,
  `best_of_n()`, `refine()`, `ensemble()`, and output assertions
  (`with_assertions()`, `assert_output()`, `suggest_output()`).
* Run programs on single inputs, vectors, or data frames with `run()`,
  `run_dataset()`, `concurrency_control()` (sequential, `ellmer` parallel, and
  `mirai` backends), `run_async()`, `run_stream()`, and `stream_async()`.
* Manage chats, portable conversation histories, and two-tier response caching
  with `dsp_configure()`, `with_lm()`, `local_lm()`, `conversation_history()`,
  `save_history()` / `load_history()`, `configure_cache()`, `cache_stats()`,
  and `dsprrr_sitrep()`.

## Evaluation and optimization

* Measure program quality with `evaluate()`, built-in metrics
  (`metric_exact_match()`, `metric_f1()`, `metric_contains()`,
  `metric_field_match()`, `metric_custom()`, `metric_threshold()`,
  `metric_with_feedback()`, `metric_with_trace()`), and `vitals` bridges
  (`as_vitals_solver()`, `as_vitals_task()`, `as_dsprrr_metric()`).
* Optimize demonstrations, instructions, and request settings with `compile()`
  and teleprompters (`LabeledFewShot()`, `BootstrapFewShot()`,
  `BootstrapFewShotWithRandomSearch()`, `KNNFewShot()`, `COPRO()`, `MIPROv2()`,
  `GEPA()`, `SIMBA()`, `GridSearchTeleprompter()`, `BetterTogether()`) or
  `optimize_grid()`.
* Inspect results, traces, prompts, and spend with `optimization_result()`,
  `optimization_summary()`, `best_params()`, `best_demos()`, `top_trials()`,
  `config_diff()`, `get_last_prompt()`, `inspect_history()`, `export_traces()`,
  `summarize_traces()`, and `session_cost()`.

## Persistence and integrations

* Save and restore versioned program artifacts with `save_program()`,
  `load_program()`, `program_artifact()`, `program_artifact_id()`,
  `pin_module_config()`, `restore_module_config()`, and `export_module_code()`.
* Integrate with the wider R ecosystem via `as_ellmer_tool()`, `ragnar_tool()`,
  `create_search_tool()`, `use_dsprrr_template()`, `validate_workflow()`,
  `llm_predict()` (`parsnip`), and `dials` parameters (`temperature()`,
  `top_p()`, `reasoning_effort()`).

## Experimental features

* Recursive Language Models (`rlm_module()`, `rlm()`, `mcp_repl_runner()`,
  `r_code_runner()`, `run_demo()`), whole-program optimization (`flex()`),
  calibrated decision outputs (`with_decisions()`, `decision_bool()`,
  `decision_score()`, `decision_choice()`, `ReAnchor()`), and meta/agentic
  optimizers (`Omni()`, `AutoResearch()`, `MetaHarness()`).

## Requirements

* Requires R >= 4.1.0 and `ellmer` >= 0.5.0.
* Installing from source requires a C compiler (used for exact filesystem
  identity checks in local persistence guards).
