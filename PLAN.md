### 1\. Vision & Philosophy

`dsprrr` is a package for building **principled, test-driven, and optimizable** applications using Large Language Models in R. It moves beyond simple prompt engineering to a structured programming model where LLM workflows are treated as programs that can be systematically improved. This is an implementation of DSPy in R.

Our philosophy is to provide "tools for thinking" about LLM applications, deeply integrating with the tidyverse ethos. By representing prompts, examples, and evaluation results as data frames, and by designing composable, pipe-friendly APIs, `dsprrr` will make the complex task of developing robust AI systems feel like a natural extension of an R-based data analysis workflow. We are not just wrapping an API; we are creating a framework for rigorous, empirical LLM development.

-----

### 2\. Core Architecture (S7-based)

The architecture is built on S7, chosen for its modern, robust object system that aligns with R's functional heritage and ensures seamless tidyverse integration. The design emphasizes composability and extensibility through a system of formal classes and generics.

**2.1. Core S7 Classes**

  * **`Signature`**: A declarative, immutable schema for an LLM operation.

    ```r
    Signature <- S7::new_class("Signature",
      properties = list(
        inputs = S7::class_list,   # A list of S7::property objects
        output_type = S7::class_any, # An ellmer::type_* object
        instructions = S7::class_character
      ),
      validator = function(self) {
        # Validate that output_type is an ellmer type object
        # Validate that inputs is a list of correctly formed properties
      }
    )
    ```

  * **`Predict`**: The foundational, stateless execution module. It pairs a `Signature` with a `glue` template.

    ```r
    Predict <- S7::new_class("Predict",
      properties = list(
        signature = Signature,
        template = S7::class_character
      )
    )
    ```

    **Design Note:** The `Predict` module itself is stateless. The `ellmer` chat object, which holds conversation history, is managed by the execution context (e.g., a `Compiler` or a user's script), not within the module. This promotes a more functional, predictable, and reusable design.

  * **`Teleprompter`**: An S7 base class for optimization strategies. Different compilation techniques are implemented as subclasses.

    ```r
    Teleprompter <- S7::new_class("Teleprompter")

    GridSearchTeleprompter <- S7::new_class("GridSearchTeleprompter",
      parent = Teleprompter,
      properties = list(
        variants = S7::class_data.frame, # A tibble defining the search space
        k = S7::class_integer,           # Number of few-shot examples
        metric = S7::class_function
      )
    )
    ```

**2.2. Core S7 Generics**

The package's primary verbs will be S7 generics, allowing users to extend the system with their own classes.

  * `forward(module, ...)`: Executes a module. It takes a module object and named arguments corresponding to the module's `Signature`.
  * `compile(program, teleprompter, ...)`: Optimizes a program. It takes a module or pipeline, a `Teleprompter` object, a set of demos, and a development set.
  * `evaluate(program, dataset, metric)`: Evaluates a program's performance on a test set.

-----

### 3\. The End-to-End Workflow (User Experience)

The S7 architecture results in an exceptionally clean and intuitive user workflow that is fully pipe-friendly.

**Example: A Simple Sentiment Classifier**

```r
library(dsprrr)
library(tibble)

# 1. Define the program's INPUT/OUTPUT schema.
sentiment_sig <- Signature(
  inputs = list(
    S7::property(name = "text", class = class_character, description = "The text to classify.")
  ),
  output_type = ellmer::type_object(
    sentiment = ellmer::type_string(
      description = "The sentiment of the text.",
      choices = c("Positive", "Negative", "Neutral")
    )
  ),
  instructions = "Classify the sentiment of the provided text."
)

# 2. Define a stateless module that implements the signature.
sentiment_classifier <- Predict(
  signature = sentiment_sig,
  template = "Text: {text}\nSentiment:"
)

# 3. Define the optimization strategy (the "Teleprompter").
# We'll test two different instructional styles.
variants <- tibble(
  id = c("terse", "roleplay"),
  instructions_mod = c(
    "Be brief and accurate.",
    "You are a sentiment analysis expert. Provide one-word answers."
  )
)

teleprompter <- GridSearchTeleprompter(
  variants = variants,
  metric = metric_exact_match(field = "sentiment")
)

# 4. Compile the program using a dev set to find the best instructions.
compiled_classifier <- compile(
  program = sentiment_classifier,
  teleprompter = teleprompter,
  dev_set = sentiment_dev_data # A tibble with 'text' and 'sentiment' columns
)

# 5. Evaluate the optimized program on a held-out test set.
evaluation_results <- evaluate(
  program = compiled_classifier,
  dataset = sentiment_test_data,
  metric = metric_exact_match(field = "sentiment")
)

# 6. Use the final, optimized program.
forward(compiled_classifier, text = "I love using well-designed R packages!")
# > list(sentiment = "Positive")
```

-----

### 4\. Implementation Roadmap

**Milestone 1: S7 Foundation & Core Execution (Target: 3 Weeks)**

  * [ ] Implement the `Signature` and `Predict` S7 classes with robust validators.
  * [ ] Implement the `forward()` S7 generic and its method for the `Predict` class.
  * [ ] Establish the core `ellmer` integration for making API calls.
  * [ ] Develop comprehensive `testthat` tests for all class properties and `forward()` logic.

**Milestone 2: The Compilation Engine (Target: 5 Weeks)**

  * [ ] Implement the `Teleprompter` base class and the `GridSearchTeleprompter` subclass.
  * [ ] Implement the `compile()` S7 generic.
  * [ ] Create foundational metric helpers (e.g., `metric_exact_match`, `metric_f1`).
  * [ ] Write the introductory vignette (`vignettes/introduction.Rmd`), demonstrating the full compile-evaluate loop.

**Milestone 3: Ecosystem & Ergonomics (Target: 7 Weeks)**

  * [ ] Implement the `evaluate()` generic.
  * [ ] Develop robust error handling using `rlang` for both API failures and validation errors.
  * [ ] Add convenience wrappers for common LLM providers (e.g., `lm_openai()`).
  * [ ] Build out a comprehensive documentation website using `pkgdown`.

**Milestone 4: Future Vision & Extensibility**

  * [ ] **Advanced Teleprompters:** Implement more sophisticated optimizers, such as one that bootstraps few-shot examples from the development set.
  * [ ] **New Module Types:** Design `ChainOfThought` and `ReAct` modules to enable multi-step, tool-using programs.
  * [ ] **Caching & Deployment:** Explore integration with `pins` for caching expensive `compile()` results and `vetiver` for deploying final `dsprrr` programs as APIs.

-----

### 5\. API Design Principles

  * **Pipe-Friendly:** The API surface will be designed with the `|>` operator as a primary consideration.
  * **Type-Stable & Predictable:** Functions and generics will have reliable return types, making them easy to compose.
  * **Informative Errors:** Error messages, powered by `rlang`, will be designed to guide the user toward a solution.
  * **Extensible by Default:** The S7 generic-based architecture ensures that advanced users can easily create their own custom modules and teleprompters to extend the framework.
