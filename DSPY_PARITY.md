# DSPy vs dsprrr: Feature Parity Report

> _Generated 2026-06-02 from a multi-agent audit comparing DSPy 3.x (Python) against the dsprrr R package. Scores are estimates; per-area sections cite `file:symbol` evidence from `R/`._

## Executive Summary

dsprrr is a faithful R port of DSPy's authoring surface and a partial port of its machinery. It ports the parts an R user touches first — string signatures, the core prediction modules (Predict, ChainOfThought, ProgramOfThought, ReAct, RAG, ensemble/refine wrappers), a two-tier cache, scoped LM configuration, and ten teleprompters sharing a clean S7 `compile()` architecture — and it ports them as real, tested implementations rather than stubs. The gaps are deeper in the stack and consistent across areas: there is no Adapter abstraction (formatting and parsing are delegated wholesale to ellmer's `chat_structured()`), no predictor/parameter introspection or composable `Module` subclassing, no trace-aware metric protocol, no `dspy.LM` wrapper, no weight-optimization family (BootstrapFinetune, GRPO), and no callback/MLflow/serving story. Where dsprrr diverges, it usually leans on the R ecosystem (ellmer, ragnar, vitals, pins, tidymodels-flavored grid search), which is a reasonable trade rather than a deficiency. Net: strong on what you write, weaker on what optimizes and observes it.

## Parity Matrix

| Area | Parity level | Score | Note |
|------|-------------|-------|------|
| Signatures & Adapters | partial | 42 | Strong string-signature parser; the entire Adapter half is absent. |
| Core Prediction Modules | strong | 68 | Full module family and tracing; no introspection, adapters, or composite Module. |
| Agentic / Tool Modules | partial | 52 | ReAct delegates the loop to ellmer; no MCP, ToolCalls, or native-calling toggle. |
| Ensemble / Refine / Robustness | strong | 78 | All wrappers real, plus legacy Assert/Suggest; missing the rollout_id diversity engine. |
| Optimizers / Teleprompters | partial | 58 | 10+ prompt optimizers; no weight/RL optimization, weaker MIPRO/GEPA internals. |
| Evaluation & Metrics | partial | 55 | Solid metric library and multi-epoch stats; no trace-based dual-mode metrics. |
| Retrieval / RAG / Embeddings | partial | 38 | KNNFewShot and RAGModule work; no Embedder or built-in vector retriever. |
| LM Config, Caching, Async/Parallel | partial | 52 | Genuine two-tier cache; thin async, no LM wrapper. |
| Persistence, Observability & Deployment | partial | 42 | inspect_history and cost tracking match; no callbacks, MLflow, or serving. |

---

## Signatures & Adapters (partial, 42)

**What DSPy has.** A string-signature surface (`"inputs -> outputs"` with inline typing), class-based signatures with docstring instructions, `InputField`/`OutputField` factories over Pydantic constraints, a signature-manipulation API (`with_instructions`, `with_updated_fields`, `append`/`prepend`/`delete`, `equals`, `dump_state`/`load_state`), and a full Adapter layer: ChatAdapter with `[[ ## field ## ]]` markers, JSONAdapter with native `response_format` tiering and `json_repair`, XMLAdapter, TwoStepAdapter, BAMLAdapter, plus process-wide and scoped adapter configuration and a `dspy.Type` hook system (Image/Audio/Document/Citations/Reasoning, `adapt_to_native_lm_feature`).

**dsprrr's coverage.** The string-signature half is well done. `parse_signature` (R/signature-parser.R) splits on `->` with nesting-aware comma/colon handling (`split_respecting_nesting`), and `parse_type_string` maps the full inline-type vocabulary — `string`/`int`/`float`/`bool`/`list[...]`/`enum(...)`/`Literal[...]` plus bounds like `number[0,100]`. Outputs are native ellmer types, so structured output uses provider-native JSON schema directly via `chat_structured` (R/run.R:call_llm_request). `signature_to_json_schema()` (R/signature-schema.R) exports the contract. Reasoning is handled by composable transforms `with_reasoning()`/`without_reasoning()`/`chain_of_thought()` (R/signature-transforms.R).

**Gaps.** No Adapter abstraction at all — no ChatAdapter field markers, no JSON/XML/TwoStep/BAML adapters, no ChatAdapter→JSONAdapter fallback, no `use_native_function_calling` toggle, no scoped adapter config. No signature-manipulation API beyond the narrow reasoning transforms (no `with_instructions`, `with_updated_fields`, `append`/`prepend`/`delete`, `equals`, `dump_state`/`load_state`); optimizers mutate `self$signature@instructions` in place (R/module-predict.R:apply_optimization_params) rather than returning an immutable copy. No class-based form, no `InputField`/`OutputField` constraint metadata, no `custom_types` resolution. Union handling is weak — `Union[...]` keeps only the first type with a warning, and `dict[...]` degrades to a generic object. Instructions are prepended to the user prompt rather than sent as a system prompt.

**dsprrr extras.** Levenshtein-based closest-type suggestions and arrow-mistake detection (`-->`, `=>`, `<-`) give friendlier errors than DSPy (R/signature-parser.R). Input value type-mismatch warnings at runtime with an opt-out (`warn_signature_type_mismatches`, R/utils.R) — DSPy coerces outputs but doesn't pre-warn on inputs. Legacy S7-class type acceptance in `input()` for R-idiomatic specs. Dual glue `{ }` / ellmer `{{ }}` template interpolation.

---

## Core Prediction Modules (strong, 68)

**What DSPy has.** `dspy.Predict` with per-call `config={}` override, `forward()` returning a `Prediction` with output-field attributes, mutable tunable demos, optimizable instructions, per-predictor `lm` with `set_lm`/`get_lm` propagation, the Adapter layer, `named_predictors()`/`named_parameters()`/`map_named_predictors()` introspection, a PyTorch-style composite `Module` with auto-registered sub-modules, `deepcopy`/`reset_copy`, whole-program and state save/load, `inspect_history`, thread-safe `batch()` with straggler resubmission, native async, opt-in usage tracking, and the full sibling module family.

**dsprrr's coverage.** The everyday surface is covered with real implementations. `module()` (R/module.R) dispatches seven kinds; `PredictModule$forward` (R/module-predict.R) returns a tidy `tibble(output, chat, metadata)` with ellmer token/cost/duration tracking built in. Demos are tunable (`module_demos_as_tibble`) and teleprompters populate them. Instructions and templates are optimizable via `apply_optimization_params()`. `copy(deep=TRUE)` and `reset_copy()` clone the ellmer Chat with fresh turns. `inspect_history()` (R/inspect.R) reads a ring buffer with `file=` transcript export. The sibling family is complete: ReactModule, CodeActModule, RefineModule/BestOfNModule, MultiChainComparisonModule, RAGModule, plus an RLM extra. Runtime params (temperature, max_tokens, reasoning_effort) are applied by cloning the ellmer provider and injecting `@extra_args` (`apply_chat_params`, R/run.R).

**Gaps.** No Adapter layer. No predictor/parameter introspection — `named_predictors`, `predictors`, `named_parameters`, `map_named_predictors` are all absent. No `set_lm`/`get_lm` propagation across a tree of sub-predictors. No PyTorch-style composite Module: users can't subclass and get auto-registered tunable sub-modules; dsprrr relies on fixed module types plus pipelines. No per-call `config={}` override (mutate `module$config` or pass a pre-configured `.llm` instead). Only config/state serialization via pins, not full-program save/load with version checks. CoT reasoning is `type_string`-only — no `rationale_field_type`. `Module.batch` lacks straggler resubmission, `return_failed_examples`, and per-batch timeout/`max_errors`. Usage tracking is always-on rather than an opt-in flag on a `Prediction`.

**dsprrr extras.** ProgramOfThought generates and runs **R** code via `RCodeRunner` (R/r-code-runner.R) with subprocess isolation. The RLM (Recursive Language Model) module has no DSPy equivalent (R/module-rlm.R). Tidy/tibble-first outputs integrate with `predict.Module(new_data)` and the R data-frame ecosystem. pins-based persistence rather than pickle.

---

## Agentic / Tool Modules (partial, 52)

**What DSPy has.** A `dspy.ReAct` that builds an augmented signature (trajectory, next_thought, next_tool_name, next_tool_args), runs an explicit reason→act→observe loop with an auto-injected `finish` tool and a `next_tool_name` Literal constraint, enforces `max_iters`, truncates the trajectory and retries on context overflow, feeds tool exceptions back as observations, and supports async per-tool calls. Plus `dspy.CodeAct` (LM writes Python calling tools in a sandboxed Deno interpreter with source injection), a `dspy.Tool` wrapper with `from_mcp_tool`/`from_langchain`/`format_as_litellm_function_call`, a `ToolCalls` native function-calling I/O type, a `use_native_function_calling` toggle, and MCP integration.

**dsprrr's coverage.** ReactModule (R/module-react.R) delegates the entire tool loop to ellmer's internal executor via `llm$chat(prompt)`, then makes one `chat_structured()` call for the final answer. It inspects new turns post-hoc for `ContentToolRequest` to count iterations and aggregate tokens/cost. CodeActModule (R/module-codeact.R) is a hybrid that adds an `execute_r_code` tool plus user tools and loops up to `max_iterations`. ProgramOfThoughtModule (R/module-program-of-thought.R) is a faithful PoT: generate→execute→repair with error context fed back (`repair_code`, lines 434-510). `as_ellmer_tool()`/`register_dsprrr_tool()` (R/ellmer.R) wrap any dsprrr module as an ellmer ToolDef with structured error modes and output serialization — the inverse of `dspy.Tool` and the cleanest part of the agentic surface.

**Gaps.** ReAct has no dsprrr-built trajectory, so the agent's reasoning is neither optimizable nor inspectable as a DSPy-style trace. `max_iterations` is advisory — it only emits a post-hoc `cli_warn` and cannot stop a runaway ellmer loop (contrast RLM and CodeAct, which enforce it in their own loops). No `finish` tool, no tool-name Literal constraint, no context-overflow truncation/retry, no async ReAct. The entire native-function-calling stack is absent: no `dspy.Tool` conversions, no MCP/LangChain interop, no `ToolCalls` type, no `use_native_function_calling`. CodeAct doesn't inject tool source into the interpreter, and `has_pending_tools()` always returns FALSE so its loop is effectively single-turn. ReAct tool definitions aren't serialized — pinning a react module with tools aborts (R/orchestration.R:172-177). The code sandbox is callr process isolation plus a regex blocklist, documented as not a security boundary — weaker than DSPy's Deno sandbox.

**dsprrr extras.** RLMModule is a REPL-driven agent that explores large contexts via `peek`/`search`/`slice` and makes budgeted recursive sub-LM calls (`llm_query`), intercepted in the parent process — dsprrr's closest analog to a real text-based trajectory loop, and it has no DSPy equivalent. Tools are plain ellmer ToolDefs, so agents inherit ellmer's provider-agnostic ecosystem without a LiteLLM dependency. `ragnar_tool()` gives first-class R retrieval tools.

---

## Ensemble / Refine / Robustness (strong, 78)

**What DSPy has.** `BestOfN` with `rollout_id` + forced `temperature=1.0` cache-bypass for diversity, `Refine` with an LM-generated `OfferFeedback` loop and per-module blame, `MultiChainComparison` over caller-supplied completions, a standalone `dspy.majority()`, and an `Ensemble` teleprompter that randomly samples `size` programs per call.

**dsprrr's coverage.** This is the strongest area. BestOfNModule (R/module-wrapper.R) implements the full N-loop with early stop at threshold, best-score fallback, `fail_count` tolerance, and token/cost accumulation. RefineModule injects feedback into a named input field. MultiChainComparisonModule (R/module-multichain.R) matches the `M=3, temperature=0.7` defaults. EnsembleModule (R/module-ensemble.R) ships four exported reducers — `reduce_majority`, `reduce_weighted_vote`, `reduce_first`, `reduce_best_by_metric` — wired into the `compile()` generic via the S7 `Ensemble` teleprompter (R/teleprompter-ensemble.R), with signature-compatibility validation and weights from optimization scores.

**Gaps.** BestOfN/Refine miss DSPy's core diversity mechanism: no per-attempt `rollout_id` + `temperature=1.0` cache-bypass. Repeated attempts on identical inputs hit dsprrr's own cache and return the same output unless caching is disabled manually — which undercuts the value of best-of-N. Refine's feedback is a static glue template (`feedback_template`), not an LM-generated `OfferFeedback` signature with per-module blame. MultiChainComparison generates the M chains itself rather than accepting caller completions, and doesn't expose `reasoning_attempt_1..M`. There's no standalone `majority()` aggregator (only the ensemble reducer, keyed on the first field rather than DSPy's last). Ensemble runs programs deterministically instead of random-sampling per call. `reward_fn` argument order is reversed (`prediction, inputs` vs DSPy's `args_dict, prediction`).

**dsprrr extras.** The full legacy Assert/Suggest subsystem that DSPy 3 removed: AssertModule with hard/soft assertions, backtracking, feedback injection, and `on_failure=error|warn` (R/module-assert.R), plus a helper library (`assert_length`/`contains`/`matches`/`one_of`/`range`, R/assertion-helpers.R). `reduce_best_by_metric` is an oracle-selection reducer DSPy's majority-only ensemble lacks. Tidy `get_attempts()`/`get_feedback_history()` introspection.

---

## Optimizers / Teleprompters (partial, 58)

**What DSPy has.** A universal `compile()` API, LabeledFewShot, BootstrapFewShot(+RandomSearch), KNNFewShot, COPRO, MIPROv2 (Optuna TPE Bayesian optimization, four proposers), SIMBA, GEPA (genetic-Pareto reflective evolution with a budget, `reflection_lm`, lineage merging, and a textual/predictor-level feedback metric), BetterTogether (prompt+weight chaining), BootstrapFinetune (weight optimization), GRPO/RL via Arbor, Ensemble, save/load with cross-version compatibility, adapters, and MLflow autologging.

**dsprrr's coverage.** The prompt-optimization breadth is impressive. `compile()` is an S7 generic (R/compile.R) dispatched in R/zzz.R for 10+ optimizers, all real implementations. LabeledFewShot, BootstrapFewShot (R/teleprompter-bootstrap.R), BootstrapFewShotWithRandomSearch, COPRO (coordinate ascent over instructions, R/teleprompter-copro.R), and BetterTogether (strategy-string parsing, R/teleprompter-better-together.R) match DSPy closely. Shared infrastructure is genuine: `OptimizerControl`/`eval_program`/`check_budget` (R/optimizer-core.R), Pareto utilities (`pareto_dominates`/`pareto_frontier`/`pareto_ranks`, R/pareto.R), a discrete UCB bandit (R/optimizer-discrete-bo.R), and JSONL trial logging (R/optimizer-logging.R).

**Gaps.** No weight optimization at all — BootstrapFinetune and GRPO/Arbor are absent (CLAUDE.md Milestone H), which also hollows out BetterTogether's prompt+weight chaining (it can only chain prompt optimizers). GEPA diverges fundamentally: it's a generic GA, not DSPy's genetic-Pareto reflective evolution with budget controls (`auto`/`max_metric_calls`), a dedicated `reflection_lm`, lineage merging, or — most importantly — the textual feedback-metric protocol (`Prediction(score=, feedback=)`). MIPROv2 uses a UCB bandit rather than Optuna TPE, and implements only 2 of 4 proposers (data-aware and tip-aware; program-aware and fewshot-aware are missing). SIMBA omits the temperature params and uses simpler reflection. `compile()` doesn't surface a `teacher=` argument. No DSPy-compatible save/load, no adapter abstraction, no MLflow.

**dsprrr extras.** GridSearchTeleprompter backed by `Module$optimize_grid()` — a tidymodels-flavored grid search with no DSPy equivalent. The metric `@field` attribute system (`get_metric_field`/`detect_output_source`) builds demos from data-frame trainsets including nested list-columns. Git-friendly JSONL trial history for every optimizer, independent of MLflow.

---

## Evaluation & Metrics (partial, 55)

**What DSPy has.** A reusable `dspy.Evaluate` harness, an `EvaluationResult` with `(example, prediction, score)` triples, the metric contract `metric(gold, pred, trace=None)` with trace-based dual-mode switching (continuous in eval, binary in optimization), `answer_exact_match`/`answer_passage_match`/F1, canonical `normalize_text` (article removal + NFD), `SemanticF1`/`CompleteAndGrounded` LLM judges, the `Example`/`Prediction` primitives, configurable `failure_score`/`max_errors`, and `save_as_csv`/`save_as_json`.

**dsprrr's coverage.** `evaluate.Module()` (R/evaluate.R) runs a module over a data frame, applies the metric per row, and returns a printable `dsprrr_evaluation` list with `mean_score`, `scores`, `predictions`, `n_errors`. The metric library is solid and R-idiomatic: `metric_exact_match` (case/whitespace normalization), `metric_f1` (token-overlap precision/recall/F1), `metric_contains`, `metric_field_match`, `metric_threshold`, `metric_custom` (R/metrics.R). LLM-as-judge comes through the vitals bridge — `metric_model_graded_qa()`/`metric_model_graded_fact()` (R/vitals.R).

**Gaps.** The defining DSPy contract is absent: metrics never receive a `trace`, so there's no continuous-vs-binary dual mode for bootstrapping. The metric signature is reversed (`function(prediction, expected)`). No `dspy.Example`/`dspy.Prediction` primitives — datasets are bare data frames. `normalize_text` (R/metrics.R:325) lowercases and strips punctuation but omits English-article removal and NFD, so EM/F1 normalization is weaker. No `answer_passage_match`, no native `SemanticF1`/`CompleteAndGrounded`. Failures become `NA` (excluded via `na.rm`) rather than a configurable `failure_score`, and there's no `max_errors` abort in the harness. No `save_as_csv`/`save_as_json`. GEPA can't consume a `feedback` field from metrics.

**dsprrr extras.** Multi-epoch evaluation with `score_std` and a t-distribution 95% CI (R/evaluate.R:298-372) — beyond DSPy's single-pass aggregate. Conservative parallel safety: it refuses to reuse a non-serializable custom `.llm` across workers and falls back to sequential with a warning. Deep vitals integration (`as_vitals_solver`, `as_dsprrr_metric`, `as_vitals_task`) and `as_vitals_cost()`/`summarize_traces_df()` cost reporting. `metric_field_match` with AND/OR semantics over structured outputs.

---

## Retrieval / RAG / Embeddings (partial, 38)

**What DSPy has.** A unified `dspy.Embedder` (hosted-via-LiteLLM or custom callable, `batch_size`, caching, async `acall`), a built-in in-memory `retrievers.Embeddings` (brute-force↔FAISS auto-switch at 20k, returns `Prediction{passages, indices, scores}`), `ColBERTv2`, a standalone `dspy.KNN`, the legacy `Retrieve`/global `rm` config, `KNNFewShot`, and the canonical RAG pattern (retrieve as a plain callable composed with a separately-optimizable generation module).

**dsprrr's coverage.** This is the weakest area, by design — embedding and vector search are delegated to ragnar. RAGModule (R/module-rag.R) implements retrieve-then-generate: `extract_query` → `retrieve_context` (via `ragnar::ragnar_retrieve` or a custom `retriever(query, k)` closure) → inject into the context field → `chat_structured`. KNNFewShot is fully implemented as an S7 teleprompter (R/teleprompter-knn.R) plus a runtime KNNFewShotModule (R/module-knn.R) that embeds each query, finds k neighbors via pure-R `cosine_similarity`, and injects them as demos. `ragnar_tool()` (R/ragnar.R) exposes a ragnar store as an ellmer search tool for ReAct.

**Gaps.** No `dspy.Embedder` — no unified hosted/custom interface, no `batch_size`, no embedding-level caching, no async. Embeddings are raw closures (`KNNFewShot@vectorizer`, `create_search_tool(embedding_fn=)`) delegated to `ragnar::embed_openai()`. No built-in in-memory `retrievers.Embeddings` (no corpus-list search, no brute-force↔FAISS switch, no `Prediction{passages, indices}`); the cosine-similarity brute force exists but is scoped to demo selection, not passage retrieval. No `ColBERTv2`, no standalone `dspy.KNN` (the nearest-neighbor helpers are `@noRd` internals), no legacy `Retrieve`. RAG is a monolithic module rather than a retrieval callable composed with an independently optimizable generation sub-Predict.

**dsprrr extras.** Deep ragnar integration leverages an existing R-native RAG ecosystem rather than reimplementing vendor stores — which aligns with DSPy 3.0's "use external code" guidance. Retrieval-as-a-tool (`ragnar_tool`) plugs into ellmer ToolDef registration. Configurable fail-soft vs fail-hard retrieval via `config$fail_on_retrieval_error`, finer-grained than DSPy.

---

## LM Config, Caching, Async/Parallel (partial, 52)

**What DSPy has.** A `dspy.LM` universal client (provider/model string over LiteLLM) with `temperature`/`max_tokens`/`cache`/`num_retries`/`rollout_id`, `model_type='responses'`, `use_developer_role`, `copy`/`dump_state`/`load_state`/`finetune`/`reinforce`, a structured error hierarchy, `dspy.configure`/`dspy.context`, per-call `config={}`, `track_usage`, `configure_cache` (two-tier memory+disk), `asyncify` with a worker-pool limiter, `dspy.Parallel` over `(module, input)` pairs with straggler handling, and `streamify`/`StreamListener`.

**dsprrr's coverage.** Caching is the standout — genuine parity. `configure_cache()` (R/cache.R:68) maps directly onto DSPy's signature; `get_cache()` builds a `cachem::cache_layered` memory-LRU + disk tier with mem-then-disk lookup and write-through; `cache_key()` is a SHA256 of prompt/model/temperature/output_type (+`rollout_id`/`llm_id`) that never includes API keys. Per-call `.cache=FALSE` bypass, `DSPRRR_CACHE_ENABLED` env override, and synthetic turn injection on hit keep `inspect_history` coherent. LM config is delegated to ellmer: `dsp_configure()` (R/chat-default.R) dispatches to provider constructors, and `with_lm()`/`local_lm()` give scoped, nestable overrides with a documented resolution order. Parallelism is real and arguably richer in process options — `.parallel_method='ellmer'|'mirai'` selects `parallel_chat_structured` or `mirai::mirai_map` with auto-provisioned daemons.

**Gaps.** No `dspy.LM` wrapper — no provider/model string, no `copy`/`dump_state`/`load_state`, no `model_type='responses'`, no `use_developer_role`, no `finetune`/`reinforce`. No LM-level `num_retries`/backoff and no structured error hierarchy (errors are classified by ad-hoc `grepl` in dsp.R). `rollout_id` lives in the cache key but isn't exposed through `run()`/`forward()`, so users can't intentionally get distinct cache entries. No `track_usage` flag (usage is always-on) and no central settings registry. Async is a single `chat_structured_async` passthrough that bypasses the cache/trace/demo pipeline; no `asyncify` limiter, no async tools. No `dspy.Parallel` over arbitrary `(module, input)` pairs — dsprrr only fans one module over many inputs, with no `return_failed_examples`/straggler handling. No `streamify`/`StreamListener`/`StatusMessageProvider`; streaming yields raw text and bypasses structured output. The cache lacks security hardening (`restrict_pickle`/`safe_types`). Provider coverage in `dsp_configure()` is limited to OpenAI/Anthropic/Google (any ellmer Chat can still be passed explicitly).

**dsprrr extras.** `session_cost()` and `dsprrr_sitrep()` give a tibble-friendly per-model token/cost rollup and a `git_sitrep`-style config report. `disk_max_age` adds TTL-style cache expiry beyond DSPy's size/entry limits. `.parallel_method='mirai'` enables true multi-process parallelism from the R ecosystem.

---

## Persistence, Observability & Deployment (partial, 42)

**What DSPy has.** State-only and whole-program save/load (`save_program=True` via cloudpickle, reconstruction-free `dspy.load`) with a pickle security gate and cross-version metadata, `inspect_history`, a shared `GLOBAL_HISTORY` plus per-LM history, a `BaseCallback` hook set (module/lm/adapter/tool/evaluate start/end) with `configure(callbacks=)`, `enable_logging`, `track_usage` per-LM aggregation, MLflow tracing/optimizer-autologging/model-flavor serving, `asyncify`/`streamify`, and a FastAPI/MLflow serving pattern with per-thread config isolation.

**dsprrr's coverage.** Two pillars match DSPy. `inspect_history()` (R/inspect.R) is full — it reads the global ring buffer, returns a tibble, and adds a transcript-to-file export. Usage/cost tracking is real: `get_tokens()`/`get_cost()` S3 generics (R/accessors.R) plus `session_cost()` (R/chat-default.R) with a per-model breakdown that rivals `get_lm_usage`. Persistence is a different-but-real pins approach: `pin_module_config()`/`restore_module_config()` (R/orchestration.R) round-trip signature/demos/config/state with `format_version==2L` and `dsprrr_version`/`r_version` metadata. Optimizer runs log to JSONL + `metadata.json` + `best_program.rds` (R/optimizer-logging.R).

**Gaps.** No `BaseCallback` lifecycle hooks and no `configure(callbacks=)` — the only callback is `Module$stream(callback=)` for streaming chunks. No reconstruction-free whole-program load; `restore_module_config` needs the pinned config, supports only four module kinds, and drops react tools. No MLflow integration of any kind (tracing, optimizer autologging, model flavor). No serving stack (plumber endpoint, `mlflow models serve`, build-docker). No `asyncify`/`streamify`/`StreamListener`. No `track_usage` toggle and no per-LM history object (only one global history). No pickle security gate and no stated cross-version compatibility guarantee. Global mutable history is process-wide, not per-thread isolated for concurrent serving.

**dsprrr extras.** Plain-text transcript export of history to a file or connection. Tibble-first observability (`inspect_history`, `get_tokens`, `get_cost` all return tidy data). pins-board persistence for configs, traces, and eval logs. `as_vitals_cost()` bridges. Workflow scaffolding via `use_dsprrr_template('targets'/'quarto')` and `validate_workflow()`.

---

## Gaps Worth Closing

Ranked by impact across areas.

1. **Per-attempt diversity for BestOfN/Refine (rollout_id + temperature override).** The mechanism already exists in the cache layer (`cache_key()` accepts `rollout_id`, R/cache.R:343) but the wrappers never pass it. Without it, resampling collapses to identical cached outputs, which quietly defeats the entire point of best-of-N and refine. This is the highest-leverage fix: small, localized, and it repairs the correctness of a whole robustness area. Exposing `rollout_id` through `run()`/`forward()` closes a related LM-config gap at the same time.

2. **Trace-aware metric protocol (continuous in eval, binary in optimization).** This is the central DSPy metric contract and its absence ripples into bootstrapping quality across BootstrapFewShot, MIPROv2, and GEPA. Adding an optional `trace` argument to the metric signature unlocks correct binarization during compilation.

3. **A feedback-metric channel and a real GEPA.** GEPA's value in DSPy 3.x comes from textual, predictor-level feedback (`Prediction(score=, feedback=)`) driving reflection. dsprrr's GEPA is a generic GA that can't consume feedback. Supporting a feedback return type benefits both GEPA and Refine's `OfferFeedback` gap.

4. **Adapter abstraction (at least ChatAdapter + JSONAdapter fallback).** Its absence shows up in three areas (Signatures, Core Modules, Agentic). A minimal Adapter layer with a parse-failure fallback would also enable a `use_native_function_calling` toggle and give optimizers something to vary.

5. **Enforce ReAct `max_iterations` and build an inspectable trajectory.** Today the cap is an advisory warning that can't stop a runaway ellmer loop, and the agent's reasoning isn't optimizable. RLM and CodeAct already enforce their own loops, so the pattern exists in-repo to follow.

6. **`dspy.Parallel` over `(module, input)` pairs with failure handling.** dsprrr fans one module over many inputs but can't run a heterogeneous batch; adding `return_failed_examples`/straggler handling matters for production batch jobs.

7. **Signature-manipulation API (`with_instructions`, append/prepend/delete, dump_state/load_state).** Needed for clean optimizer-driven rewriting; right now optimizers mutate `@instructions` in place, breaking the immutability the S7 design otherwise provides.

8. **A `dspy.Embedder` abstraction.** Even a thin wrapper over ragnar/ellmer embeddings with batching and caching would close the largest single gap in the retrieval area and standardize the raw-closure pattern KNNFewShot and `create_search_tool` use today.

Lower priority but worth tracking: MIPROv2's missing proposers and TPE search, a callback/hook system, MLflow integration, and the weight-optimization family (BootstrapFinetune/GRPO) — the last is large and already scoped as Milestone H.

## Where dsprrr Leads or Diverges

dsprrr's divergences are mostly deliberate bets on the R ecosystem, and several of them are net wins. Structured output is provider-native through **ellmer**'s `chat_structured()` rather than a hand-rolled adapter ladder, which removes a whole class of parsing bugs at the cost of adapter flexibility. Retrieval and embeddings lean on **ragnar** instead of reimplementing vendor stores and a FAISS switch — fewer moving parts, and it matches DSPy 3.0's own "use external code" direction. Evaluation integrates with **vitals** (solver/scorer/task bridges, cost reporting) and adds multi-epoch statistics with confidence intervals that DSPy's single-pass `Evaluate` doesn't have. Persistence uses **pins** and observability is **tibble-first**, so `inspect_history`, `get_tokens`, and `get_cost` drop straight into dplyr. The **S7/R6** split — immutable signatures, stateful modules — is idiomatic R and makes the optimizable surface explicit. And the **tidymodels-flavored** `GridSearchTeleprompter` plus `predict.Module(new_data)` give R users a grid-search and prediction idiom with no DSPy analog. The flip side of every one of these is the same: by delegating to ellmer/ragnar/vitals, dsprrr inherits their abstractions and skips the deeper machinery (adapters, embedder, callbacks, weight optimization) that DSPy builds itself. That's the right trade for an R package meant to feel native, but it's the reason parity is "strong on authoring, partial on the rest."