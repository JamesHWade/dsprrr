# Advanced Optimizers: DSPy-Inspired Teleprompters

## Summary

Implement advanced optimizer (teleprompter) types from DSPy to enable sophisticated automatic prompt optimization in dsprrr. This builds on the existing `LabeledFewShot` and `GridSearchTeleprompter` to add:

- **BootstrapFewShot**: Generate demonstrations via teacher model with metric validation
- **BootstrapFewShotWithRandomSearch**: Bootstrap + random search over hyperparameters
- **KNNFewShot**: Embedding-based dynamic demo selection at runtime
- **COPRO**: Coordinate ascent instruction optimization
- **MIPROv2**: Bayesian optimization over prompts with GPfit
- **SIMBA**: Self-improving hard example mining
- **GEPA**: Genetic/evolutionary Pareto frontier optimization
- **Ensemble**: Multi-module voting and selection

## Motivation

These optimizers are core to DSPy's power in automatically improving LLM prompts. They enable:

1. **Automatic demonstration generation**: BootstrapFewShot creates high-quality demos without manual labeling
2. **Adaptive example selection**: KNNFewShot selects the most relevant demos for each query
3. **Instruction optimization**: COPRO/MIPROv2 automatically improve system prompts
4. **Hard example focus**: SIMBA identifies and targets failure cases
5. **Multi-objective optimization**: GEPA balances quality vs. cost on Pareto frontier
6. **Ensemble robustness**: Combine multiple strategies for improved reliability

## Scope

### In Scope
- [x] BootstrapFewShot teleprompter
- [x] BootstrapFewShotWithRandomSearch teleprompter
- [x] KNNFewShot teleprompter (with ellmer embeddings)
- [x] COPRO (Coordinate Prompt Optimization)
- [x] MIPROv2 (Bayesian optimization with GPfit)
- [x] SIMBA (Self-Improving hard example mining)
- [x] GEPA (Genetic/Evolutionary Pareto optimization)
- [x] Ensemble teleprompter

### Out of Scope (Future Work)
- [ ] BootstrapFinetune (requires fine-tuning infrastructure)
- [ ] Distributed optimization across multiple machines

---

## Implementation Plan

### 1. BootstrapFewShot Teleprompter

**Approach**: Teacher-student bootstrapping with metric validation

**API Design**:
```r
# Create teleprompter
tp <- BootstrapFewShot(
  metric = metric_exact_match(field = "answer"),
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 16L,
  max_rounds = 1L,
  max_errors = 5L,
  teacher = NULL  # Optional teacher module (uses student by default)
)

# Compile module
compiled <- compile_module(student_module, tp, trainset, .llm = llm)

# Access bootstrapped demos
compiled$demos  # High-quality generated demos
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/teleprompter-bootstrap.R` (new) | `BootstrapFewShot` S7 class |
| `R/zzz.R` | Register compile method |
| `tests/testthat/test-teleprompter-bootstrap.R` (new) | Unit tests |

**BootstrapFewShot S7 Class**:
```r
BootstrapFewShot <- S7::new_class(

  "BootstrapFewShot",
  parent = Teleprompter,
  properties = list(
    max_bootstrapped_demos = S7::new_property(S7::class_integer, default = 4L),
    max_labeled_demos = S7::new_property(S7::class_integer, default = 16L),
    max_rounds = S7::new_property(S7::class_integer, default = 1L),
    teacher = S7::new_property(S7::class_any, default = NULL)
  )
)
```

**Execution Flow**:
```
1. Initialize teacher (use student if not provided)
2. Seed teacher with max_labeled_demos from trainset
3. For each round:
   a. For each training example:
      - Teacher generates prediction
      - Evaluate with metric
      - If passes threshold: add to bootstrapped demos
   b. Update teacher demos with bootstrapped demos
4. Select top max_bootstrapped_demos by metric score
5. Return student with selected demos
```

**Bootstrapped Demo Format**:
```r
list(
  inputs = list(question = "What is 2+2?"),
  output = list(answer = "4"),
  score = 1.0,
  round = 1L,
  source = "bootstrapped"
)
```

---

### 2. BootstrapFewShotWithRandomSearch Teleprompter

**Approach**: Extends BootstrapFewShot with parallel random search over hyperparameters

**API Design**:
```r
tp <- BootstrapFewShotWithRandomSearch(
  metric = metric_exact_match(),
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 16L,
  num_candidate_programs = 8L,  # Number of random configurations to try
  num_threads = 1L,  # Parallel execution threads
  teacher = NULL
)

# Compile with random search
compiled <- compile_module(module, tp, trainset, valset = valset, .llm = llm)

# Access search results
compiled$config$search_results  # tibble of all candidates with scores
compiled$config$best_candidate  # Best configuration details
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/teleprompter-bootstrap.R` | Add `BootstrapFewShotWithRandomSearch` S7 class |
| `tests/testthat/test-teleprompter-bootstrap.R` | Additional tests |

**Search Space**:
```r
# Default parameter ranges for random search
list(
  max_bootstrapped_demos = c(2L, 4L, 8L),
  max_labeled_demos = c(4L, 8L, 16L),
  temperature = c(0.1, 0.5, 0.7, 1.0)
)
```

**Execution Flow**:
```
1. Generate num_candidate_programs random configurations
2. For each configuration (optionally parallel via mirai):
   a. Run BootstrapFewShot with those parameters
   b. Evaluate compiled module on validation set
   c. Record score
3. Return module with best configuration
```

---

### 3. KNNFewShot Teleprompter

**Approach**: Dynamic demo selection based on embedding similarity at runtime

**API Design**:
```r
tp <- KNNFewShot(
  metric = metric_exact_match(),
  k = 3L,
  embedding_model = "text-embedding-3-small",  # ellmer embedding model
  trainset = trainset,  # Store for embedding lookup
  cache_embeddings = TRUE
)

# Compile embeds trainset and stores index
compiled <- compile_module(module, tp, trainset, .llm = llm)

# At runtime, demos are selected dynamically per-input
result <- run(compiled, question = "What is the capital of France?", .llm = llm)
# Automatically selects k most similar demos from trainset
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/teleprompter-knn.R` (new) | `KNNFewShot` S7 class, embedding utilities |
| `R/module-knn.R` (new) | `KNNModule` R6 wrapper for dynamic demo selection |
| `tests/testthat/test-teleprompter-knn.R` (new) | Unit tests |

**KNNFewShot S7 Class**:
```r
KNNFewShot <- S7::new_class(
  "KNNFewShot",
  parent = Teleprompter,
  properties = list(
    k = S7::new_property(S7::class_integer, default = 3L),
    embedding_model = S7::new_property(S7::class_character, default = "text-embedding-3-small"),
    cache_embeddings = S7::new_property(S7::class_logical, default = TRUE),
    trainset = S7::new_property(S7::class_any, default = NULL)
  )
)
```

**KNNModule R6 Class**:
```r
KNNModule <- R6::R6Class(
  "KNNModule",
  inherit = Module,
  public = list(
    inner_module = NULL,       # Wrapped module
    trainset = NULL,           # Training data for demo lookup
    trainset_embeddings = NULL, # Pre-computed embeddings matrix
    k = 3L,
    embedding_model = NULL,

    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      # 1. Embed the input
      input_embedding <- private$embed_input(batch)

      # 2. Find k nearest neighbors in trainset
      neighbors <- private$find_neighbors(input_embedding, self$k)

      # 3. Update inner module demos with selected neighbors
      self$inner_module$demos <- neighbors

      # 4. Forward to inner module
      self$inner_module$forward(batch, .llm = .llm, trace = trace, ...)
    }
  ),
  private = list(
    embed_input = function(batch) {...},
    find_neighbors = function(embedding, k) {...},
    cosine_similarity = function(a, b) {...}
  )
)
```

**Embedding Integration**:
```r
# Using ellmer for embeddings
embed_text <- function(text, model = "text-embedding-3-small") {
  ellmer::embed(text, model = model)
}

# Pre-compute trainset embeddings at compile time
compile_knn <- function(teleprompter, program, trainset, ...) {
  # Concatenate input fields for embedding

  input_texts <- apply(trainset[, input_cols], 1, paste, collapse = " ")

  # Batch embed all training examples
  embeddings <- ellmer::embed(input_texts, model = teleprompter@embedding_model)

  # Create KNNModule wrapper
  KNNModule$new(
    inner_module = program,
    trainset = trainset,
    trainset_embeddings = embeddings,
    k = teleprompter@k,
    embedding_model = teleprompter@embedding_model
  )
}
```

---

### 4. COPRO (Coordinate Prompt Optimization)

**Approach**: Coordinate ascent over instruction components

**API Design**:
```r
tp <- COPRO(
  metric = metric_exact_match(),
  breadth = 10L,        # Candidates per iteration
  depth = 3L,           # Number of optimization rounds
  init_temperature = 1.4,
  verbose = TRUE
)

# Compile optimizes instructions
compiled <- compile_module(module, tp, trainset, valset = valset, .llm = llm)

# Access optimization history
compiled$config$instruction_history  # All tried instructions with scores
compiled$config$best_instructions    # Final optimized instructions
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/teleprompter-copro.R` (new) | `COPRO` S7 class |
| `tests/testthat/test-teleprompter-copro.R` (new) | Unit tests |

**COPRO S7 Class**:
```r
COPRO <- S7::new_class(
  "COPRO",
  parent = Teleprompter,
  properties = list(
    breadth = S7::new_property(S7::class_integer, default = 10L),
    depth = S7::new_property(S7::class_integer, default = 3L),
    init_temperature = S7::new_property(S7::class_double, default = 1.4),
    verbose = S7::new_property(S7::class_logical, default = TRUE)
  )
)
```

**Execution Flow**:
```
1. Start with current module instructions as baseline
2. For each depth iteration:
   a. Generate breadth candidate instruction variants using LLM
   b. Evaluate each candidate on validation set
   c. Select best performing instruction
   d. Use best as baseline for next iteration
3. Return module with optimized instructions
```

**Instruction Generation Prompt**:
```
You are an expert prompt engineer. Given the current instruction and its performance,
generate an improved version.

Current instruction: {current_instruction}
Current score: {current_score}
Task: {task_description}

Failed examples:
{failed_examples}

Generate {breadth} improved instruction variants that might perform better.
Focus on clarity, specificity, and addressing the failure cases.
```

---

### 5. MIPROv2 (Bayesian Optimization)

**Approach**: Bayesian optimization over prompt space using GPfit (like tidymodels)

**API Design**:
```r
tp <- MIPROv2(
  metric = metric_exact_match(),
  num_candidates = 10L,    # Initial random candidates
  num_iterations = 20L,    # Bayesian optimization iterations
  init_temperature = 1.0,
  track_stats = TRUE,
  verbose = TRUE
)

# Compile with Bayesian optimization
compiled <- compile_module(module, tp, trainset, valset = valset, .llm = llm)

# Access optimization results
compiled$config$bo_history     # All evaluations with acquisition values
compiled$config$gp_model       # Final GP model for analysis
compiled$config$best_config    # Best found configuration
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/teleprompter-mipro.R` (new) | `MIPROv2` S7 class |
| `R/bayesian-opt.R` (new) | GPfit wrapper utilities |
| `tests/testthat/test-teleprompter-mipro.R` (new) | Unit tests |

**MIPROv2 S7 Class**:
```r
MIPROv2 <- S7::new_class(
  "MIPROv2",
  parent = Teleprompter,
  properties = list(
    num_candidates = S7::new_property(S7::class_integer, default = 10L),
    num_iterations = S7::new_property(S7::class_integer, default = 20L),
    init_temperature = S7::new_property(S7::class_double, default = 1.0),
    track_stats = S7::new_property(S7::class_logical, default = TRUE),
    verbose = S7::new_property(S7::class_logical, default = TRUE)
  )
)
```

**Search Space Encoding**:
```r
# Encode prompt configurations as numeric vectors for GP
encode_config <- function(config) {
  c(
    config$num_demos,           # Integer -> numeric
    config$temperature,         # Already numeric
    encode_instruction(config$instructions)  # Text -> embedding
  )
}

# Use sentence embedding for instruction encoding
encode_instruction <- function(text) {
  ellmer::embed(text, model = "text-embedding-3-small")
}
```

**Bayesian Optimization with GPfit**:
```r
# Similar to tidymodels tune_bayes() approach
bayesian_optimize <- function(objective_fn, bounds, n_iter, n_init) {
  # 1. Initial random sampling
  X_init <- random_sample(bounds, n_init)
  y_init <- sapply(X_init, objective_fn)

  # 2. Fit GP model using GPfit
  gp_model <- GPfit::GP_fit(X_init, y_init)

  # 3. Bayesian optimization loop
  for (i in seq_len(n_iter)) {
    # Expected Improvement acquisition function
    x_next <- optimize_acquisition(gp_model, bounds, type = "EI")

    # Evaluate objective
    y_next <- objective_fn(x_next)

    # Update GP model
    gp_model <- GPfit::GP_fit(rbind(X_init, x_next), c(y_init, y_next))
  }

  list(best_x = best_found, gp_model = gp_model, history = all_evaluations)
}
```

---

### 6. SIMBA (Self-Improving via Hard Example Mining)

**Approach**: Iteratively identify and focus on hard (failing) examples

**API Design**:
```r
tp <- SIMBA(
  metric = metric_exact_match(),
  max_iterations = 5L,
  hard_example_threshold = 0.5,  # Score below this = hard example
  improvement_threshold = 0.01,  # Stop if improvement < this
  verbose = TRUE
)

# Compile with hard example mining
compiled <- compile_module(module, tp, trainset, valset = valset, .llm = llm)

# Access mining results
compiled$config$hard_examples        # Identified hard examples
compiled$config$iteration_scores     # Score progression
compiled$config$improvements         # Per-iteration improvements
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/teleprompter-simba.R` (new) | `SIMBA` S7 class |
| `tests/testthat/test-teleprompter-simba.R` (new) | Unit tests |

**SIMBA S7 Class**:
```r
SIMBA <- S7::new_class(
  "SIMBA",
  parent = Teleprompter,
  properties = list(
    max_iterations = S7::new_property(S7::class_integer, default = 5L),
    hard_example_threshold = S7::new_property(S7::class_double, default = 0.5),
    improvement_threshold = S7::new_property(S7::class_double, default = 0.01),
    verbose = S7::new_property(S7::class_logical, default = TRUE)
  )
)
```

**Execution Flow**:
```
1. Initial evaluation on full dataset
2. For each iteration:
   a. Identify hard examples (score < threshold)
   b. Generate targeted demos/instructions for hard examples
   c. Re-evaluate on full dataset
   d. If improvement < threshold: stop early
3. Return module optimized for hard examples
```

**Hard Example Analysis Prompt**:
```
Analyze these examples where the model failed:

{failed_examples}

Common patterns in failures:
- [Identify patterns]

Generate improved instructions that specifically address these failure modes.
```

---

### 7. GEPA (Genetic/Evolutionary Pareto Optimization)

**Approach**: Multi-objective optimization balancing quality and cost on Pareto frontier

**API Design**:
```r
tp <- GEPA(
  metrics = list(
    quality = metric_exact_match(),
    cost = metric_cost()  # Track token/API costs
  ),
  population_size = 20L,
  generations = 10L,
  mutation_rate = 0.1,
  crossover_rate = 0.7,
  verbose = TRUE
)

# Compile with Pareto optimization
compiled <- compile_module(module, tp, trainset, valset = valset, .llm = llm)

# Access Pareto frontier
compiled$config$pareto_frontier  # tibble of non-dominated solutions
compiled$config$all_generations  # Full evolutionary history
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/teleprompter-gepa.R` (new) | `GEPA` S7 class |
| `R/pareto.R` (new) | Pareto dominance utilities |
| `tests/testthat/test-teleprompter-gepa.R` (new) | Unit tests |

**GEPA S7 Class**:
```r
GEPA <- S7::new_class(
  "GEPA",
  parent = Teleprompter,
  properties = list(
    metrics = S7::new_property(S7::class_list, default = list()),
    population_size = S7::new_property(S7::class_integer, default = 20L),
    generations = S7::new_property(S7::class_integer, default = 10L),
    mutation_rate = S7::new_property(S7::class_double, default = 0.1),
    crossover_rate = S7::new_property(S7::class_double, default = 0.7),
    verbose = S7::new_property(S7::class_logical, default = TRUE)
  )
)
```

**Genetic Operators**:
```r
# Crossover: blend instructions/demos from two parents
crossover <- function(parent1, parent2) {
  list(
    instructions = blend_instructions(parent1$instructions, parent2$instructions),
    demos = sample(c(parent1$demos, parent2$demos), k)
  )
}

# Mutation: LLM-guided instruction modification
mutate <- function(individual, llm) {
  if (runif(1) < mutation_rate) {
    individual$instructions <- llm$chat(
      paste("Slightly modify this instruction:", individual$instructions)
    )
  }
  individual
}

# Selection: NSGA-II style tournament on Pareto ranks
select <- function(population, fitness_matrix) {
  ranks <- compute_pareto_ranks(fitness_matrix)
  crowding <- compute_crowding_distance(fitness_matrix, ranks)
  tournament_select(population, ranks, crowding)
}
```

**Pareto Utilities**:
```r
# Check if solution a dominates solution b
dominates <- function(a, b) {
  all(a >= b) && any(a > b)
}

# Find non-dominated solutions (Pareto frontier)
pareto_frontier <- function(solutions, fitness_matrix) {
  is_dominated <- logical(nrow(fitness_matrix))
  for (i in seq_len(nrow(fitness_matrix))) {
    for (j in seq_len(nrow(fitness_matrix))) {
      if (i != j && dominates(fitness_matrix[j, ], fitness_matrix[i, ])) {
        is_dominated[i] <- TRUE
        break
      }
    }
  }
  solutions[!is_dominated]
}
```

---

### 8. Ensemble Teleprompter

**Approach**: Combine multiple optimized modules for improved robustness

**API Design**:
```r
tp <- Ensemble(
  teleprompters = list(
    BootstrapFewShot(k = 4),
    COPRO(depth = 2),
    KNNFewShot(k = 3)
  ),
  aggregation = "vote",  # "vote", "best", "weighted"
  metric = metric_exact_match(),
  weights = NULL  # Optional weights for "weighted" aggregation
)

# Compile creates ensemble of optimized modules
compiled <- compile_module(module, tp, trainset, valset = valset, .llm = llm)

# At runtime, all modules vote
result <- run(compiled, question = "What is 2+2?", .llm = llm)
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/teleprompter-ensemble.R` (new) | `Ensemble` S7 class |
| `R/module-ensemble.R` (new) | `EnsembleModule` R6 class |
| `tests/testthat/test-teleprompter-ensemble.R` (new) | Unit tests |

**Ensemble S7 Class**:
```r
Ensemble <- S7::new_class(
  "Ensemble",
  parent = Teleprompter,
  properties = list(
    teleprompters = S7::new_property(S7::class_list),
    aggregation = S7::new_property(
      S7::class_character,
      default = "vote",
      validator = function(value) {
        if (!value %in% c("vote", "best", "weighted")) {
          return("aggregation must be 'vote', 'best', or 'weighted'")
        }
        NULL
      }
    ),
    weights = S7::new_property(S7::class_any, default = NULL)
  )
)
```

**EnsembleModule R6 Class**:
```r
EnsembleModule <- R6::R6Class(
  "EnsembleModule",
  inherit = Module,
  public = list(
    modules = NULL,         # List of compiled modules
    aggregation = "vote",
    weights = NULL,

    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      # Run all modules
      results <- lapply(self$modules, function(mod) {
        mod$forward(batch, .llm = .llm, trace = FALSE, ...)
      })

      # Aggregate results
      final <- switch(self$aggregation,
        vote = private$aggregate_vote(results),
        best = private$aggregate_best(results),
        weighted = private$aggregate_weighted(results)
      )

      final
    }
  ),
  private = list(
    aggregate_vote = function(results) {
      # Majority voting for categorical outputs
      outputs <- lapply(results, function(r) r$output[[1]])
      table_outputs <- table(unlist(outputs))
      names(table_outputs)[which.max(table_outputs)]
    },
    aggregate_best = function(results) {
      # Return result with highest confidence (if available)
      # Otherwise return first result
      results[[1]]
    },
    aggregate_weighted = function(results) {
      # Weighted combination based on validation scores
      # For numeric outputs: weighted average
      # For categorical: weighted voting
    }
  )
)
```

---

## Integration Points

### Compile Generic Integration

All new teleprompters integrate via the existing `compile()` S7 generic:

```r
# In R/zzz.R, register all compile methods
S7::method(compile, list(BootstrapFewShot, class_any)) <- compile_bootstrap
S7::method(compile, list(BootstrapFewShotWithRandomSearch, class_any)) <- compile_bootstrap_random
S7::method(compile, list(KNNFewShot, class_any)) <- compile_knn
S7::method(compile, list(COPRO, class_any)) <- compile_copro
S7::method(compile, list(MIPROv2, class_any)) <- compile_mipro
S7::method(compile, list(SIMBA, class_any)) <- compile_simba
S7::method(compile, list(GEPA, class_any)) <- compile_gepa
S7::method(compile, list(Ensemble, class_any)) <- compile_ensemble
```

### Module Compatibility

All teleprompters work with existing module types:
- `PredictModule`: Primary target for optimization
- `ReactModule`: Optimize reasoning/tool use patterns
- `ChainOfThought`: Optimize with extended reasoning (Phase 4)
- `MultiChainComparison`: Optimize synthesis patterns (Phase 4)

### Tracing & Cost Tracking

Each teleprompter provides full observability:

```r
# Access optimization traces
compiled$config$optimization_traces  # All LLM calls during optimization
compiled$config$total_optimization_cost  # Accumulated cost
compiled$config$optimization_duration  # Time spent optimizing

# Runtime traces still work
compiled$trace_summary()
```

### Validation Set Handling

Consistent validation set pattern across all teleprompters:

```r
compile_module <- function(module, tp, trainset, valset = NULL, .llm = NULL, ...) {
  # If no valset provided, split trainset
  if (is.null(valset)) {
    split <- rsample::initial_split(trainset, prop = 0.8)
    trainset <- rsample::training(split)
    valset <- rsample::testing(split)
  }

  compile(tp, module, trainset, valset = valset, .llm = .llm, ...)
}
```

---

## Implementation Checklist

### Phase 1: BootstrapFewShot (2-3 days)
- [ ] Create `R/teleprompter-bootstrap.R`
- [ ] Implement `BootstrapFewShot` S7 class
- [ ] Implement `compile_bootstrap()` method
- [ ] Add bootstrapped demo format with metadata
- [ ] Implement teacher/student pattern
- [ ] Write unit tests
- [ ] Record VCR cassette

### Phase 2: BootstrapFewShotWithRandomSearch (1-2 days)
- [ ] Add `BootstrapFewShotWithRandomSearch` to `R/teleprompter-bootstrap.R`
- [ ] Implement random configuration generation
- [ ] Add parallel execution support via mirai
- [ ] Implement `compile_bootstrap_random()` method
- [ ] Write unit tests

### Phase 3: KNNFewShot (2-3 days)
- [ ] Create `R/teleprompter-knn.R`
- [ ] Implement `KNNFewShot` S7 class
- [ ] Create `R/module-knn.R` with `KNNModule` R6 class
- [ ] Integrate ellmer embeddings (`ellmer::embed()`)
- [ ] Implement cosine similarity and k-NN lookup
- [ ] Add embedding caching
- [ ] Write unit tests
- [ ] Record VCR cassette

### Phase 4: COPRO (2-3 days)
- [ ] Create `R/teleprompter-copro.R`
- [ ] Implement `COPRO` S7 class
- [ ] Implement coordinate ascent algorithm
- [ ] Create instruction generation prompts
- [ ] Implement `compile_copro()` method
- [ ] Write unit tests
- [ ] Record VCR cassette

### Phase 5: MIPROv2 (3-4 days)
- [ ] Create `R/teleprompter-mipro.R`
- [ ] Implement `MIPROv2` S7 class
- [ ] Create `R/bayesian-opt.R` with GPfit wrapper
- [ ] Implement search space encoding (including instruction embeddings)
- [ ] Implement Expected Improvement acquisition
- [ ] Implement `compile_mipro()` method
- [ ] Write unit tests
- [ ] Record VCR cassette

### Phase 6: SIMBA (2-3 days)
- [ ] Create `R/teleprompter-simba.R`
- [ ] Implement `SIMBA` S7 class
- [ ] Implement hard example identification
- [ ] Create targeted improvement prompts
- [ ] Implement early stopping
- [ ] Implement `compile_simba()` method
- [ ] Write unit tests
- [ ] Record VCR cassette

### Phase 7: GEPA (3-4 days)
- [ ] Create `R/teleprompter-gepa.R`
- [ ] Implement `GEPA` S7 class
- [ ] Create `R/pareto.R` with dominance utilities
- [ ] Implement genetic operators (crossover, mutation, selection)
- [ ] Implement NSGA-II style selection
- [ ] Implement `compile_gepa()` method
- [ ] Write unit tests

### Phase 8: Ensemble (2 days)
- [ ] Create `R/teleprompter-ensemble.R`
- [ ] Implement `Ensemble` S7 class
- [ ] Create `R/module-ensemble.R` with `EnsembleModule` R6 class
- [ ] Implement voting/aggregation strategies
- [ ] Implement `compile_ensemble()` method
- [ ] Write unit tests

### Phase 9: Documentation & Polish (2-3 days)
- [ ] Add examples to README.Rmd
- [ ] Create vignette: `advanced-optimization.Rmd`
- [ ] Update CLAUDE.md with new implementation status
- [ ] Run `devtools::check()` and fix any issues
- [ ] Update pkgdown reference index
- [ ] Add to NAMESPACE exports

---

## Testing Strategy

### Unit Tests (mock LLM)
- BootstrapFewShot generates valid demos with metric validation
- KNNFewShot selects correct k neighbors
- COPRO iterates and improves instructions
- MIPROv2 runs Bayesian optimization loop
- SIMBA identifies hard examples correctly
- GEPA maintains valid Pareto frontier
- Ensemble aggregates results correctly

### Integration Tests (VCR cassettes)
- BootstrapFewShot produces working demos
- KNNFewShot embedding lookup works end-to-end
- COPRO improves instruction quality
- MIPROv2 finds better configurations than random

### Example Cassettes to Record
- `tests/_vcr/bootstrap-basic.yml` - Basic BootstrapFewShot
- `tests/_vcr/knn-embeddings.yml` - KNNFewShot with embeddings
- `tests/_vcr/copro-optimize.yml` - COPRO instruction optimization
- `tests/_vcr/mipro-bayesian.yml` - MIPROv2 Bayesian optimization

---

## Dependencies

### Required Packages
- **GPfit**: Gaussian Process fitting for Bayesian optimization (CRAN)
- **ellmer**: Already a dependency, provides `embed()` for KNN

### Optional Packages
- **rsample**: Train/validation splitting (Suggested)
- **mirai**: Parallel execution for RandomSearch (Suggested)

---

## API Summary

| Function | Purpose | Returns |
|----------|---------|---------|
| `BootstrapFewShot(...)` | Create bootstrap teleprompter | S7 BootstrapFewShot |
| `BootstrapFewShotWithRandomSearch(...)` | Bootstrap + random search | S7 BootstrapFewShotWithRandomSearch |
| `KNNFewShot(...)` | K-NN demo selection | S7 KNNFewShot |
| `COPRO(...)` | Coordinate prompt optimization | S7 COPRO |
| `MIPROv2(...)` | Bayesian optimization | S7 MIPROv2 |
| `SIMBA(...)` | Hard example mining | S7 SIMBA |
| `GEPA(...)` | Pareto optimization | S7 GEPA |
| `Ensemble(...)` | Multi-teleprompter ensemble | S7 Ensemble |

---

## References

- [DSPy Optimizers Documentation](https://dspy.ai/learn/optimizers/)
- [BootstrapFewShot Tutorial](https://dspy.ai/tutorials/optimizers/bootstrap_fewshot/)
- [MIPROv2 Paper](https://arxiv.org/abs/2406.11695)
- [COPRO Tutorial](https://dspy.ai/tutorials/optimizers/copro/)
- [GPfit Package](https://cran.r-project.org/package=GPfit)
- [tidymodels tune_bayes()](https://tune.tidymodels.org/reference/tune_bayes.html)

---

## Success Criteria

- [ ] All eight teleprompters implemented and tested
- [ ] Full integration with existing `compile()` and `compile_module()` APIs
- [ ] Comprehensive documentation with examples
- [ ] VCR cassettes for reproducible integration tests
- [ ] R CMD check passes with no errors/warnings
- [ ] GPfit and ellmer embeddings working correctly

## Labels

`enhancement`, `teleprompter`, `optimization`, `phase-5`
