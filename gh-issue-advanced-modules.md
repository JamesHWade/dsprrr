# Advanced Module Types: DSPy-Inspired Reasoning Modules

## Summary

Implement advanced module types from DSPy to enable sophisticated reasoning patterns in dsprrr. This builds on the existing `PredictModule` and `ReactModule` architecture to add:

- **ChainOfThought**: Step-by-step reasoning before final output
- **BestOfN**: Run module N times, return best result
- **Refine**: BestOfN with feedback loop for iterative improvement
- **MultiChainComparison**: Run M reasoning chains, synthesize best answer

## Motivation

These modules are core to DSPy's success in improving LLM output quality through principled prompting strategies. They enable:

1. **Better reasoning**: ChainOfThought forces models to "show their work"
2. **Higher reliability**: BestOfN provides multiple attempts at correct answers
3. **Iterative improvement**: Refine uses feedback to improve across attempts
4. **Ensemble reasoning**: MultiChainComparison leverages multiple reasoning paths

## Scope

### In Scope
- [x] ChainOfThought module
- [x] BestOfN wrapper module
- [x] Refine wrapper module (extends BestOfN)
- [x] MultiChainComparison module

### Out of Scope (Future Work)
- [ ] ProgramOfThought (code generation + execution)
- [ ] CodeAct (code + tool execution)
- [ ] Advanced teleprompters (MIPRO, GEPA)

---

## Implementation Plan

### 1. ChainOfThought Module

**Approach**: Functional signature transform (lightweight, composable)

**API Design**:
```r
# Pipe-friendly API
mod <- signature("question -> answer") |>
  chain_of_thought()

# Or explicit with custom prefix
mod <- signature("question -> answer") |>
  with_reasoning(prefix = "Let's analyze this step by step:")

# Returns structured output with reasoning
result <- run(mod, question = "What is 15 * 24?", .llm = llm)
# list(reasoning = "First, I'll break this down...", answer = "360")
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/signature-transforms.R` (new) | `with_reasoning()`, `chain_of_thought()` |
| `tests/testthat/test-signature-transforms.R` (new) | Unit tests |

**How it works**:
1. `with_reasoning(sig)` transforms signature's `output_type` from `type_string()` to `type_object(reasoning = type_string(), answer = type_string())`
2. The reasoning field has description: "Reasoning: Let's think step by step in order to"
3. LLM returns both reasoning and answer via `chat_structured()`

**Output Type Transformation**:
```r
# Original: "question -> answer"
type_string()

# Extended: "question -> reasoning, answer"
type_object(
  reasoning = type_string(.description = "Reasoning: Let's think step by step..."),
  answer = type_string()
)
```

---

### 2. BestOfN Module

**Approach**: R6 wrapper class that composes any Module

**API Design**:
```r
# Create wrapper
wrapper <- best_of_n(
  module = qa_module,
  N = 3,
  reward_fn = one_word_reward,  # Custom: function(pred, inputs) -> [0, 1]
  threshold = 1.0,
  fail_count = 3
)

# Or use existing metrics
wrapper <- best_of_n(
  module = qa_module,
  N = 5,
  reward_fn = as_reward_fn(metric_exact_match(field = "answer")),
  threshold = 0.8
)

# Run like normal module
result <- run(wrapper, question = "Capital of Belgium?")
# Returns best attempt or first above threshold

# Inspect all attempts
wrapper$get_attempts()
# tibble: attempt_id, prediction, score
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/module-wrapper.R` (new) | `BestOfNModule` R6 class, `best_of_n()` factory |
| `tests/testthat/test-module-wrapper.R` (new) | Unit tests |

**BestOfNModule R6 Class**:
```r
BestOfNModule <- R6::R6Class(
  "BestOfNModule",
  inherit = Module,
  public = list(
    module = NULL,      # Wrapped module
    N = NULL,           # Max attempts (integer)
    reward_fn = NULL,   # function(prediction, inputs) -> [0, 1]
    threshold = NULL,   # Early stop threshold
    fail_count = NULL,  # Error tolerance

    initialize = function(module, N, reward_fn, threshold, fail_count) {...},
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {...},
    get_attempts = function() {...}  # Return tibble of all attempts
  )
)
```

**Reward Function System**:
```r
# Convert metric to reward function
as_reward_fn <- function(metric, expected_field = "expected") {
  function(prediction, inputs) {
    expected <- inputs[[expected_field]]
    score <- metric(prediction, expected)
    if (is.logical(score)) as.numeric(score) else score
  }
}

# Custom reward function
one_word_reward <- function(prediction, inputs) {
  words <- strsplit(as.character(prediction), "\\s+")[[1]]
  if (length(words) == 1) 1.0 else 0.0
}
```

**Execution Flow**:
```
for i in 1:N:
  prediction_i = module$forward(batch, .llm)
  score_i = reward_fn(prediction_i, inputs)
  if score_i >= threshold: RETURN prediction_i

RETURN prediction with max(scores)
```

---

### 3. Refine Module

**Approach**: Extends BestOfN with feedback generation

**API Design**:
```r
refined <- refine(
  module = qa_module,
  N = 3,
  reward_fn = one_word_reward,
  threshold = 1.0,
  feedback_template = "Previous answer ({score} score) was too long. Give a single word."
)

result <- run(refined, question = "Capital of Belgium?")

# Access feedback history
refined$state$feedback_history
# ["Previous answer (0.0 score) was too long...", ...]
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/module-wrapper.R` | Add `RefineModule` R6 class, `refine()` factory |
| `tests/testthat/test-module-wrapper.R` | Additional tests |

**RefineModule R6 Class**:
```r
RefineModule <- R6::R6Class(
  "RefineModule",
  inherit = BestOfNModule,
  public = list(
    feedback_template = NULL,

    initialize = function(..., feedback_template = NULL) {...}
  ),
  private = list(
    generate_feedback = function(inputs, prediction, score) {...},
    inject_feedback = function(batch, feedback) {...}
  )
)
```

**Execution Flow** (differs from BestOfN):
```
for i in 1:N:
  if i > 1:
    batch = inject_feedback(batch, feedback_{i-1})

  prediction_i = module$forward(batch, .llm)
  score_i = reward_fn(prediction_i, inputs)

  if score_i >= threshold: RETURN prediction_i

  feedback_i = generate_feedback(inputs, prediction_i, score_i)

RETURN prediction with max(scores)
```

---

### 4. MultiChainComparison Module

**Approach**: Composite module that runs inner module M times, then synthesizes

**API Design**:
```r
# Create MCC module
multi <- multi_chain_comparison(
  signature("question -> answer"),
  M = 3,
  temperature = 0.7
)

# Or via module factory
multi <- module(sig, type = "multichain", M = 3)

# Works with any inner module
multi <- multi_chain_comparison(
  inner_module = chain_of_thought(sig),
  M = 5
)

result <- run(multi, question = "Complex reasoning task...", .llm = llm)
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/module-multichain.R` (new) | `MultiChainComparisonModule` R6 class |
| `R/module.R` | Add `type = "multichain"` to factory |
| `tests/testthat/test-module-multichain.R` (new) | Unit tests |

**MultiChainComparisonModule R6 Class**:
```r
MultiChainComparisonModule <- R6::R6Class(
  "MultiChainComparisonModule",
  inherit = Module,
  public = list(
    inner_module = NULL,
    M = 3L,
    temperature = 0.7,
    comparison_template = NULL,

    initialize = function(signature, inner_module = NULL, M = 3L, ...) {...},
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {...}
  ),
  private = list(
    build_comparison_signature = function() {...},
    build_comparison_prompt = function(attempts) {...},
    run_attempt = function(inputs, llm, attempt_num) {...}
  )
)
```

**Execution Flow**:
```
# Phase 1: Collect M attempts
for i in 1:M:
  attempt_i = inner_module$forward(inputs, .llm, temperature=temperature)
  attempts[i] = {rationale: ..., answer: ...}

# Phase 2: Synthesize best answer
comparison_prompt = format_attempts(attempts)
final = llm$chat_structured(comparison_prompt, comparison_type)

RETURN final$answer
```

**Default Comparison Template**:
```
You will evaluate {M} reasoning attempts for the same problem.

Student Attempt #1:
{reasoning_attempt_1}

Student Attempt #2:
{reasoning_attempt_2}

...

Analyze these attempts and provide:
1. Your refined reasoning
2. The best final answer
```

---

## Integration Points

### Optimization Support

All new modules integrate with existing optimization:

```r
# BestOfN with grid search
wrapper <- best_of_n(qa_module, N = 3)
optimize_grid(
  wrapper,
  data = dev_data,
  metric = metric_exact_match(),
  parameters = list(temperature = c(0.1, 0.5, 0.9))
)

# MultiChainComparison optimization
optimize_grid(
  multi,
  parameters = list(M = c(3, 5), temperature = c(0.5, 0.7))
)
```

### Teleprompter Compatibility

All modules work with existing teleprompters:

```r
# Compile ChainOfThought with few-shot examples
tp <- LabeledFewShot(k = 4)
compiled <- compile_module(cot_module, tp, trainset)

# BestOfN also compiles
compiled_wrapper <- compile_module(wrapper, tp, trainset)
```

### Tracing & Cost Tracking

Each module provides full observability:

```r
# ChainOfThought
mod$get_traces()  # Single call per run

# BestOfN
wrapper$get_traces()  # N calls per run
wrapper$get_attempts()  # Detailed per-attempt info

# MultiChainComparison
multi$get_traces()  # M+1 calls per run (M attempts + comparison)
multi$trace_summary()$total_cost  # Aggregated cost
```

---

## Implementation Checklist

### Phase 1: ChainOfThought (1-2 days)
- [ ] Create `R/signature-transforms.R`
- [ ] Implement `with_reasoning(signature, prefix)`
- [ ] Implement `chain_of_thought(signature, ...)` convenience wrapper
- [ ] Handle single-field and multi-field signatures
- [ ] Write unit tests
- [ ] Add to pkgdown reference

### Phase 2: BestOfN (2-3 days)
- [ ] Create `R/module-wrapper.R`
- [ ] Implement `BestOfNModule` R6 class
- [ ] Implement `best_of_n()` factory function
- [ ] Implement reward function conversion utilities
- [ ] Ensure compatibility with `run()`, `evaluate()`, `optimize_grid()`
- [ ] Write unit tests
- [ ] Record VCR cassette for integration test

### Phase 3: Refine (1 day)
- [ ] Add `RefineModule` to `R/module-wrapper.R`
- [ ] Implement feedback generation (template-based)
- [ ] Implement feedback injection into batch
- [ ] Add `refine()` factory function
- [ ] Write unit tests

### Phase 4: MultiChainComparison (2-3 days)
- [ ] Create `R/module-multichain.R`
- [ ] Implement `MultiChainComparisonModule` R6 class
- [ ] Implement dynamic comparison signature construction
- [ ] Update `module()` factory for `type = "multichain"`
- [ ] Implement trace aggregation across M+1 calls
- [ ] Write unit tests
- [ ] Record VCR cassette

### Phase 5: Documentation & Polish (1-2 days)
- [ ] Add examples to README.Rmd
- [ ] Create vignette: `advanced-modules.Rmd`
- [ ] Update CLAUDE.md with new implementation status
- [ ] Run `devtools::check()` and fix any issues
- [ ] Update pkgdown reference index

---

## Testing Strategy

### Unit Tests (mock LLM)
- Signature transformation produces correct output types
- BestOfN executes exactly N times (or stops early at threshold)
- Refine generates and injects feedback correctly
- MultiChainComparison aggregates traces and costs

### Integration Tests (VCR cassettes)
- ChainOfThought produces reasoning + answer
- BestOfN returns best scored attempt
- MultiChainComparison synthesizes from multiple attempts

### Example Cassettes to Record
- `tests/_vcr/cot-basic.yml` - ChainOfThought with simple QA
- `tests/_vcr/best-of-n-basic.yml` - BestOfN with 3 attempts
- `tests/_vcr/multichain-basic.yml` - MCC with M=3

---

## API Summary

| Function | Purpose | Returns |
|----------|---------|---------|
| `with_reasoning(sig, prefix)` | Transform signature for CoT | Modified Signature |
| `chain_of_thought(sig, ...)` | Create CoT module | PredictModule |
| `best_of_n(mod, N, reward_fn, ...)` | Create BestOfN wrapper | BestOfNModule |
| `refine(mod, N, reward_fn, ...)` | Create Refine wrapper | RefineModule |
| `multi_chain_comparison(sig, M, ...)` | Create MCC module | MultiChainComparisonModule |
| `as_reward_fn(metric)` | Convert metric to reward | function |

---

## References

- [DSPy Modules Documentation](https://dspy.ai/learn/programming/modules/)
- [ChainOfThought API](https://dspy.ai/api/modules/ChainOfThought/)
- [BestOfN API](https://dspy.ai/api/modules/BestOfN/)
- [Refine Tutorial](https://dspy.ai/tutorials/output_refinement/best-of-n-and-refine/)
- [MultiChainComparison API](https://dspy.ai/api/modules/MultiChainComparison/)

---

## Success Criteria

- [ ] All four modules implemented and tested
- [ ] Full integration with existing `run()`, `evaluate()`, `optimize_grid()`
- [ ] Comprehensive documentation with examples
- [ ] VCR cassettes for reproducible integration tests
- [ ] R CMD check passes with no errors/warnings

## Labels

`enhancement`, `module-types`, `phase-4`
