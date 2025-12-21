# vitals & dsprrr Integration Plan

## Executive Summary

This document outlines a strategic integration plan between `vitals`
(LLM evaluation framework) and `dsprrr` (LLM program optimization
framework), two complementary packages in the ellmer ecosystem. The
integration will create a unified workflow for developing, optimizing,
and evaluating LLM applications in R.

## Package Overview

### vitals

- **Purpose**: LLM evaluation framework (port of Inspect)
- **Architecture**: R6 class system
- **Core Concepts**: Tasks (datasets + solvers + scorers)
- **Strengths**: Production-ready evaluation, log viewer integration,
  model-graded scoring
- **Target Users**: LLM application developers needing rigorous
  evaluation

### dsprrr

- **Purpose**: LLM program optimization framework (port of DSPy)
- **Architecture**: S7 class system
- **Core Concepts**: Signatures, Modules (Predict), Teleprompters
  (optimizers)
- **Strengths**: Declarative schemas, systematic optimization,
  composable modules
- **Target Users**: Developers building complex, optimizable LLM
  workflows

### ellmer

- **Role**: Common foundation for LLM API interactions
- **Used by**: Both packages for structured LLM calls

## Complementary Nature

The packages are naturally complementary rather than competing:

1.  **dsprrr** focuses on *building and optimizing* LLM programs
2.  **vitals** focuses on *evaluating* LLM applications
3.  Together they form a complete lifecycle: Design → Build → Optimize →
    Evaluate

## Integration Benefits

### For Users

1.  **Unified Workflow**: Seamlessly move from program development
    (dsprrr) to evaluation (vitals)
2.  **Shared Data Formats**: Use the same datasets for optimization and
    evaluation
3.  **Cross-Package Metrics**: Use vitals’ sophisticated scorers in
    dsprrr optimization
4.  **Production Pipeline**: Clear path from prototype to
    production-ready LLM applications

### For the Ecosystem

1.  **Reduced Duplication**: Share common utilities and patterns
2.  **Consistent Design**: Harmonized APIs following tidyverse
    principles
3.  **Stronger Value Proposition**: Complete toolkit for LLM development
    in R
4.  **Cross-Package Innovation**: Features in one package benefit the
    other

## Integration Architecture

### Phase 1: Interoperability Layer (Immediate - 2 weeks)

Create adapters that allow the packages to work together without
changing their core architectures:

``` r
# Adapter: dsprrr Predict module → vitals solver
as_vitals_solver <- function(predict_module, chat = NULL) {
  function(inputs, ...) {
    results <- map(inputs, \(input) {
      result <- forward(predict_module, !!!parse_input(input), .chat = chat)
      list(
        result = as.character(result),
        solver_chat = attr(result, "chat")
      )
    })

    list(
      result = map_chr(results, "result"),
      solver_chat = map(results, "solver_chat")
    )
  }
}

# Adapter: vitals scorer → dsprrr metric
as_dsprrr_metric <- function(vitals_scorer) {
  function(predictions, targets) {
    samples <- tibble(
      input = names(predictions),
      target = targets,
      result = predictions
    )
    scores <- vitals_scorer(samples)
    mean(scores$score == "C", na.rm = TRUE)
  }
}
```

### Phase 2: Shared Components (Weeks 3-6)

Develop shared infrastructure in a common dependency or namespace:

1.  **Dataset Format Specification**
    - Standardize on tibble with `input`, `target`, optional `metadata`
    - Create validators for dataset consistency
    - Shared utilities for dataset splitting, sampling, bootstrapping
2.  **Metric Library**
    - Port vitals scorers to be usable in dsprrr optimization
    - Create unified metric interface accessible to both packages
    - Include: exact_match, model_graded, BLEU, ROUGE,
      semantic_similarity
3.  **Logging Infrastructure**
    - Extend vitals’ Inspect-compatible logging to capture dsprrr
      optimization traces
    - Create unified experiment tracking that shows both optimization
      and evaluation

### Phase 3: Deep Integration (Weeks 7-10)

Create first-class support for cross-package workflows:

1.  **dsprrr Teleprompter using vitals evaluation**

``` r
VitalsOptimizer <- S7::new_class("VitalsOptimizer",
  parent = Teleprompter,
  properties = list(
    task = vitals::Task,
    optimization_dataset = S7::class_data.frame,
    n_iterations = S7::class_integer
  )
)

compile.VitalsOptimizer <- function(program, teleprompter, ...) {
  # Use vitals Task evaluation to guide dsprrr optimization
  best_program <- NULL
  best_score <- -Inf

  for (i in seq_len(teleprompter@n_iterations)) {
    candidate <- mutate_program(program, i)

    # Evaluate using vitals
    task <- teleprompter@task$clone()
    task$solver <- as_vitals_solver(candidate)
    task$eval()

    score <- task$get_metrics()$accuracy
    if (score > best_score) {
      best_program <- candidate
      best_score <- score
    }
  }

  best_program
}
```

2.  **vitals Task with dsprrr modules**

``` r
# Direct support for dsprrr modules as solvers
task <- vitals::Task$new(
  dataset = evaluation_data,
  solver = dsprrr_solver(my_predict_module),
  scorer = model_graded_qa()
)
```

3.  **Unified CLI/UI**
    - Extend vitals log viewer to show dsprrr optimization history
    - Create unified dashboard for experiment tracking
    - Shared configuration management

### Phase 4: Advanced Features (Weeks 11-16)

1.  **AutoDSP**: Automatic program synthesis
    - Use vitals evaluation to automatically discover optimal dsprrr
      programs
    - Implement meta-learning across tasks
2.  **Continuous Optimization**
    - Production monitoring with vitals feeding back to dsprrr
      optimization
    - A/B testing framework for module variants
3.  **Multi-Stage Pipelines**
    - Complex workflows mixing dsprrr modules and vitals evaluation
    - Automatic checkpointing and recovery

## Technical Considerations

### Architecture Alignment

1.  **S7 vs R6**:
    - Keep existing architectures initially
    - Create S7 wrappers for vitals R6 classes where needed
    - Consider gradual migration of vitals to S7 in future
2.  **Data Structure Compatibility**:
    - Standardize on tibbles for datasets
    - Use lists for complex returns (maintaining existing patterns)
    - Ensure ellmer chat objects are consistently handled
3.  **Dependency Management**:
    - Keep packages loosely coupled initially
    - Consider a `dsprrr.vitals` extension package for deep integration
    - Shared utilities could live in ellmer or a new common package

### Potential Challenges

1.  **Different OOP Systems**: S7 (dsprrr) vs R6 (vitals)
    - Solution: Adapter pattern for interoperability
    - Long-term: Consider aligning on S7
2.  **Conceptual Overlap**: Both have evaluation concepts
    - Solution: Clear delineation - dsprrr for optimization metrics,
      vitals for comprehensive evaluation
    - Document best practices for when to use each
3.  **User Confusion**: When to use which package?
    - Solution: Clear documentation with workflow diagrams
    - Provide templates for common use cases
    - Create unified getting-started vignettes

## Implementation Roadmap

### Immediate Actions (Week 1-2)

1.  ✅ Create this integration plan
2.  Create adapter functions in dsprrr:
    [`as_vitals_solver()`](reference/as_vitals_solver.md),
    `from_vitals_metric()`
3.  Add vitals to dsprrr’s Suggests and create integration vignette
4.  Test basic interoperability with example workflows

### Short-term (Weeks 3-6)

1.  Implement shared dataset utilities
2.  Create bidirectional metric adapters
3.  Develop integration test suite
4.  Write comprehensive documentation

### Medium-term (Weeks 7-10)

1.  Implement VitalsOptimizer Teleprompter
2.  Add native dsprrr support in vitals
3.  Create unified logging infrastructure
4.  Develop showcase applications

### Long-term (Weeks 11+)

1.  Advanced features based on user feedback
2.  Performance optimizations
3.  Consider architectural alignment (S7 migration)
4.  Expand ecosystem with specialized packages

## Success Metrics

1.  **Technical Success**
    - Zero-friction interoperability between packages
    - \<5% performance overhead from integration
    - 100% backward compatibility maintained
2.  **User Success**
    - Reduced time from prototype to production
    - Increased adoption of both packages
    - Positive user feedback on integrated workflows
3.  **Ecosystem Success**
    - Clear position in tidyverse-adjacent ecosystem
    - Growing number of packages depending on integration
    - Conference talks and workshops featuring integrated workflow

## Recommendations for dsprrr API Changes (Pre-Release)

Since dsprrr has not been released yet, we have a unique opportunity to
design its API with vitals integration in mind from the start. After
careful analysis, here are strategic API changes that would dramatically
simplify integration:

### Critical Changes (High Impact, Low Effort)

1.  **Batch Processing Support in [`run()`](reference/run.md)**

    ``` r
    # Current: Single input only
    run(module, text = "one input", .llm = llm)

    # Proposed: Support vectorized inputs
    run(module, text = c("input1", "input2", "input3"), .llm = llm)
    # Returns list of results matching input length
    ```

    **Rationale**: vitals operates on datasets and expects solvers to
    handle vectors. This single change eliminates the need for complex
    adapters.

2.  **Structured Return Format**

    ``` r
    # Current: Returns just the parsed output
    result <- run(module, ...)  # Returns: list(sentiment = "positive")

    # Proposed: Return structured object with metadata
    result <- run(module, ...)
    # Returns: list(
    #   output = list(sentiment = "positive"),
    #   chat = <ellmer_chat_object>,
    #   metadata = list(tokens_used = 150, latency_ms = 234)
    # )

    # With convenience accessor
    result$output  # For backward compatibility
    ```

    **Rationale**: vitals needs chat objects for logging and metadata
    for evaluation. This aligns with vitals’ solver return format.

3.  **Module as Function Interface**

    ``` r
    # Current: Must use run() generic
    result <- run(module, text = "input", .llm = llm)

    # Proposed: Add function interface via S7
    # Make modules callable directly
    result <- module(text = "input", .llm = llm)

    # Implementation: Add S7 method
    S7::method(`(`, Predict) <- function(module, ...) {
      run(module, ...)
    }
    ```

    **Rationale**: Makes modules behave like functions, simplifying
    wrapping and composition.

### Strategic Changes (Medium Impact, Medium Effort)

4.  **Dataset-Aware Execution**

    ``` r
    # Proposed: Add run_dataset() method
    run_dataset <- S7::new_generic("run_dataset", "module")

    S7::method(run_dataset, Predict) <- function(module, dataset, .llm = NULL, .parallel = TRUE) {
      # dataset is a tibble with columns matching module inputs
      # Returns tibble with input columns + result column
      # Handles parallelization internally
    }
    ```

    **Rationale**: Direct support for tibble datasets eliminates
    conversion overhead.

5.  **Standardize on Tibble Datasets**

    ``` r
    # In compile() and evaluate() functions
    compile(module, teleprompter,
            train_data = tibble(text = ..., target = ...))  # Not just lists
    ```

    **Rationale**: Aligns with vitals’ dataset format, making data
    sharing seamless.

6.  **Unified Metric Interface**

    ``` r
    # Proposed: Metrics that work with both packages
    metric_accuracy <- function(predictions, targets, metadata = NULL) {
      # predictions: vector of outputs from module
      # targets: vector of expected outputs
      # metadata: optional list of chat objects, etc.

      # Returns single numeric score OR detailed tibble
      if (is.null(metadata)) {
        mean(predictions == targets)
      } else {
        tibble(
          score = predictions == targets,
          tokens = map_int(metadata$chats, ~ .x$token_count),
          latency = map_dbl(metadata$chats, ~ .x$latency)
        )
      }
    }
    ```

    **Rationale**: Metrics that return both simple scores and detailed
    analysis support both optimization and evaluation use cases.

### Future-Proofing Changes (Low Impact Now, High Value Later)

7.  **Execution Context Object**

    ``` r
    # Proposed: Separate execution context from module definition
    context <- ExecutionContext$new(
      llm = llm,
      cache = TRUE,
      parallel = TRUE,
      batch_size = 10
    )

    # Modules remain pure definitions
    result <- run(module, inputs, .context = context)
    ```

    **Rationale**: Cleanly separates module logic from execution
    concerns, making modules more portable between different execution
    environments.

8.  **Module Composition Primitives**

    ``` r
    # Proposed: Built-in composition support
    pipeline <- compose(
      module1,
      module2,
      adapter = function(x) list(text = x$output)
    )
    ```

    **Rationale**: First-class support for composition reduces need for
    custom adapters.

### Naming and Convention Alignment

9.  **Consider Alternative to [`run()`](reference/run.md)**

    ``` r
    # Options:
    # A. Keep run() but add aliases
    execute <- run  # More formal
    predict <- run  # Aligns with Predict class

    # B. Use generate() to align with vitals
    generate(module, ...)  # Matches vitals' generate() solver
    ```

    **Rationale**: Reduces cognitive overhead when switching between
    packages.

10. **Align Property Names**
    `r # Current: Various names # Proposed: Standardize on vitals conventions where sensible - demos -> examples # More intuitive - config -> settings # Clearer purpose`

### Implementation Priority

**Do Immediately (Before First Release):** - Changes 1, 2, 3 - Core
functionality alignment - Change 4 - Dataset support - Change 6 - Metric
interface

**Consider for v0.2:** - Changes 5, 7, 8 - Enhanced integration features

**Evaluate Based on User Feedback:** - Changes 9, 10 - Naming/convention
changes

## Original Recommendations

### Should We Do This?

**Strong Yes**, and with these API changes, integration becomes even
more compelling:

1.  **Start Right**: With the proposed API changes, integration is
    built-in from day one
2.  **User-Driven**: The changes make both packages easier to use
    independently AND together
3.  **Maintain Independence**: Each package remains valuable standalone
4.  **Document Extensively**: Clear guidance on when to use what
5.  **Community Engagement**: Involve users early through GitHub
    discussions

### Key Principles

1.  **Composability Over Coupling**: The API changes enable loose
    coupling with clear interfaces
2.  **Progressive Enhancement**: Basic features work independently,
    advanced features reward integration
3.  **Tidyverse Alignment**: Follow tidyverse design principles
    throughout
4.  **User-Centric Design**: Every integration feature must solve real
    user problems
5.  **Performance Awareness**: Batch processing and parallelization
    built in from the start

## Conclusion

The integration of vitals and dsprrr represents a significant
opportunity to create a comprehensive, best-in-class toolkit for LLM
application development in R. By combining dsprrr’s optimization
capabilities with vitals’ evaluation framework, we can offer users a
complete workflow from initial prototype through production deployment.

### Key Insights from API Analysis

Since dsprrr is pre-release, we have a unique opportunity to design for
integration from the start. The most impactful changes are:

1.  **Batch processing in [`run()`](reference/run.md)** - Eliminates the
    single biggest integration friction
2.  **Structured return format** - Enables seamless data flow between
    packages
3.  **Module-as-function interface** - Makes composition natural and
    intuitive

These changes benefit dsprrr users even without vitals, while making
integration nearly friction-free. The cost is minimal (a few days of
development), but the long-term value is substantial.

### Recommended Path Forward

1.  **Implement critical API changes** in dsprrr before release (Changes
    1-3, 6)
2.  **Create simple adapters** for immediate interoperability
3.  **Build shared dataset utilities** as a foundation
4.  **Iterate based on user feedback** for deeper integration

This positions the ellmer ecosystem as the definitive choice for serious
LLM development in R, comparable to or exceeding what’s available in
Python while maintaining R’s strengths in data analysis and statistical
computing. The proposed changes ensure both packages are stronger
together while remaining independently valuable.
