# Generating llms.txt for R Packages

[llms.txt](https://llmstxt.org/) is a proposed standard for AI-friendly
documentation. This tutorial builds a dsprrr pipeline to automatically
generate `llms.txt` files for R packages.

*This tutorial is adapted from the [DSPy llms.txt
tutorial](https://dspy.ai/tutorials/llms_txt_generation/) by the DSPy
team.*

## Why This Matters Beyond llms.txt

The multi-stage pipeline you’ll build here is a blueprint for many
real-world AI workflows:

- **API documentation generation**: The same pattern—gather metadata,
  analyze structure, generate prose—applies to any documentation task.
  Replace R packages with REST APIs, GraphQL schemas, or database
  tables.

- **Code review automation**: Each stage (understand purpose → analyze
  structure → identify issues) maps directly to how a code review agent
  works. The typed S7 results ensure review findings don’t get lost
  between stages.

- **Migration guide creation**: When upgrading dependencies or
  refactoring APIs, you need the same capabilities: understand what
  exists, identify patterns, generate actionable guidance.

- **Knowledge base construction**: The pipeline extracts structured
  knowledge from unstructured sources. Swap packages for internal wikis,
  Slack channels, or support tickets.

The key insight is **staged analysis with typed handoffs**. Each LLM
call has a focused job. S7 classes ensure intermediate results are
validated before the next stage sees them. This is more reliable than
asking one prompt to do everything.

We’ll start with plain lists and functions, then show how S7 adds
structure for larger projects.

## What You’ll Build

A multi-stage analysis pipeline that:

1.  Gathers package metadata (DESCRIPTION, README, exports)
2.  Analyzes purpose and key concepts
3.  Analyzes code structure
4.  Generates usage examples
5.  Produces formatted `llms.txt`

``` r
library(dsprrr)
#> 
#> Attaching package: 'dsprrr'
#> The following object is masked from 'package:methods':
#> 
#>     signature
library(ellmer)
library(cli)
```

## Part 1: The Simple Approach

For a quick script or personal use, plain lists work fine.

### Gather Package Info

``` r
gather_package_info <- function(pkg_path = ".") {
  # Read DESCRIPTION
  desc_path <- file.path(pkg_path, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    cli_abort("No DESCRIPTION file found at {.path {pkg_path}}")
  }

  desc <- read.dcf(desc_path)

  # Parse dependencies
  imports <- desc[1, "Imports"] %||% ""
  deps <- if (nzchar(imports)) {
    trimws(strsplit(imports, ",")[[1]])
  } else {
    character()
  }

  # Read README
  readme_path <- file.path(pkg_path, "README.md")
  readme <- if (file.exists(readme_path)) {
    paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }

  # Get R files
  r_dir <- file.path(pkg_path, "R")
  r_files <- if (dir.exists(r_dir)) {
    list.files(r_dir, pattern = "\\.R$", ignore.case = TRUE)
  } else {
    character()
  }

  # Get exports from NAMESPACE
  ns_path <- file.path(pkg_path, "NAMESPACE")
  exports <- if (file.exists(ns_path)) {
    ns_lines <- readLines(ns_path, warn = FALSE)
    export_lines <- grep("^export\\(", ns_lines, value = TRUE)
    gsub("export\\((.+)\\)", "\\1", export_lines)
  } else {
    character()
  }

  # Check for vignettes
  vignette_dir <- file.path(pkg_path, "vignettes")
  has_vignettes <- dir.exists(vignette_dir) &&
    length(list.files(vignette_dir, pattern = "\\.(Rmd|qmd)$")) > 0

  # Return a simple list
 list(
    name = desc[1, "Package"],
    title = desc[1, "Title"] %||% "",
    description = desc[1, "Description"] %||% "",
    readme = readme,
    r_files = r_files,
    exports = exports,
    dependencies = deps,
    has_vignettes = has_vignettes
  )
}
```

### Define Signatures

Here’s where dsprrr comes in—each analysis stage has a clear contract.
Signatures define *what* each stage needs and *what* it produces. This
separation matters: when a stage fails or produces poor output, you know
exactly where to look.

Notice how we use
[`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
and
[`type_array()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
to define structured outputs. The LLM returns JSON matching this schema,
which we can then pass reliably to the next stage:

``` r
# Stage 1: Analyze purpose
analyze_purpose_sig <- signature(
  inputs = list(
    input("pkg_name", "Package name"),
    input("title", "Package title from DESCRIPTION"),
    input("description_text", "Description from DESCRIPTION"),
    input("readme_excerpt", "First 2000 chars of README"),
    input("exported_functions", "Comma-separated exports")
  ),
  output_type = type_object(
    purpose = type_string("One sentence: what problem does this solve?"),
    key_concepts = type_array(
      type_object(
        term = type_string("Concept name"),
        definition = type_string("One sentence definition")
      ),
      "3-5 core concepts"
    ),
    target_audience = type_string("Who should use this?"),
    prerequisites = type_array(type_string(), "Required knowledge")
  ),
  instructions = "Analyze this R package to extract its core purpose.
Be precise and technical. Focus on what makes it unique."
)
#> Warning: Unknown type 'package name', defaulting to string
#> Warning: Unknown type 'package title from description', defaulting to
#> string
#> Warning: Unknown type 'description from description', defaulting to
#> string
#> Warning: Unknown type 'first 2000 chars of readme', defaulting to string
#> Warning: Unknown type 'comma-separated exports', defaulting to string

# Stage 2: Analyze structure
analyze_structure_sig <- signature(
  inputs = list(
    input("pkg_name", "Package name"),
    input("r_files", "R files in R/ directory"),
    input("exports", "Exported function names"),
    input("has_vignettes", "Whether package has vignettes"),
    input("dependencies", "Package dependencies")
  ),
  output_type = type_object(
    organization = type_string("How is code organized? (1-2 sentences)"),
    main_files = type_array(
      type_object(
        file = type_string("Filename"),
        purpose = type_string("What it contains")
      ),
      "3-5 most important files"
    ),
    entry_points = type_array(type_string(), "Main functions to start with"),
    patterns = type_string("Notable patterns: S3/S4/R6/S7, tidyeval, etc.")
  ),
  instructions = "Analyze package structure to help developers navigate it.
Identify important files and entry points."
)
#> Warning: Unknown type 'package name', defaulting to string
#> Warning: Unknown type 'r files in r/ directory', defaulting to string
#> Warning: Unknown type 'exported function names', defaulting to string
#> Warning: Unknown type 'whether package has vignettes', defaulting to
#> string
#> Warning: Unknown type 'package dependencies', defaulting to string

# Stage 3: Generate examples
generate_examples_sig <- signature(
  inputs = list(
    input("pkg_name", "Package name"),
    input("purpose", "What the package does"),
    input("entry_points", "Main functions"),
    input("key_concepts", "Core concepts as JSON")
  ),
  output_type = type_object(
    basic = type_string("3-5 line minimal example"),
    intermediate = type_string("5-10 line common workflow"),
    gotchas = type_array(type_string(), "1-3 common mistakes")
  ),
  instructions = "Generate realistic R code examples.
Examples must be syntactically valid R."
)
#> Warning: Unknown type 'package name', defaulting to string
#> Warning: Unknown type 'what the package does', defaulting to string
#> Warning: Unknown type 'main functions', defaulting to string
#> Warning: Unknown type 'core concepts as json', defaulting to string

# Stage 4: Generate final llms.txt
generate_llmstxt_sig <- signature(
  inputs = list(
    input("pkg_name", "Package name"),
    input("purpose", "Package purpose"),
    input("target_audience", "Who uses this"),
    input("key_concepts_json", "JSON of term/definition pairs"),
    input("organization", "Code organization"),
    input("entry_points", "Main functions"),
    input("main_files_json", "JSON of file/purpose pairs"),
    input("basic_example", "Basic usage example"),
    input("intermediate_example", "Intermediate example"),
    input("gotchas", "Common mistakes")
  ),
  output_type = type_string(),
  instructions = "Generate llms.txt in markdown format with sections:
# {pkg_name}, Key Concepts, Quick Start, Common Workflow,
Code Organization, Entry Points, Watch Out For.
Keep it concise - this is reference documentation for AI systems."
)
#> Warning: Unknown type 'package name', defaulting to string
#> Warning: Unknown type 'package purpose', defaulting to string
#> Warning: Unknown type 'who uses this', defaulting to string
#> Warning: Unknown type 'json of term/definition pairs', defaulting to
#> string
#> Warning: Unknown type 'code organization', defaulting to string
#> Warning: Unknown type 'main functions', defaulting to string
#> Warning: Unknown type 'json of file/purpose pairs', defaulting to string
#> Warning: Unknown type 'basic usage example', defaulting to string
#> Warning: Unknown type 'intermediate example', defaulting to string
#> Warning: Unknown type 'common mistakes', defaulting to string
```

### Create Modules

Each module wraps a signature. We use `type = "chain_of_thought"` for
the analysis stages—this asks the LLM to show its work, which improves
accuracy for complex analysis tasks. The final `llmstxt` stage just
needs to synthesize; no reasoning required.

``` r
create_modules <- function(llm) {
  list(
    purpose = module(analyze_purpose_sig, type = "chain_of_thought"),
    structure = module(analyze_structure_sig, type = "chain_of_thought"),
    examples = module(generate_examples_sig, type = "chain_of_thought"),
    llmstxt = module(generate_llmstxt_sig),
    llm = llm
  )
}
```

### Run the Pipeline

The pipeline chains stages together. Each stage’s output feeds the next.
This is where the structured signatures pay off—we can confidently pass
`purpose$key_concepts` to the examples stage because we know its shape.

``` r
analyze_package <- function(pkg_path = ".", llm = chat_openai()) {
  cli_h1("Analyzing package")

  modules <- create_modules(llm)
  pkg_info <- gather_package_info(pkg_path)

  cli_alert_success("Gathered metadata for {.pkg {pkg_info$name}}")

  # Stage 1: Purpose
  cli_alert_info("Analyzing purpose...")
  purpose <- run(
    modules$purpose,
    pkg_name = pkg_info$name,
    title = pkg_info$title,
    description_text = pkg_info$description,
    readme_excerpt = substr(pkg_info$readme, 1, 2000),
    exported_functions = paste(pkg_info$exports, collapse = ", "),
    .llm = modules$llm
  )

  # Stage 2: Structure
  cli_alert_info("Analyzing structure...")
  structure <- run(
    modules$structure,
    pkg_name = pkg_info$name,
    r_files = paste(pkg_info$r_files, collapse = ", "),
    exports = paste(pkg_info$exports, collapse = ", "),
    has_vignettes = pkg_info$has_vignettes,
    dependencies = paste(pkg_info$dependencies, collapse = ", "),
    .llm = modules$llm
  )

  # Stage 3: Examples
  cli_alert_info("Generating examples...")
  examples <- run(
    modules$examples,
    pkg_name = pkg_info$name,
    purpose = purpose$purpose,
    entry_points = paste(structure$entry_points, collapse = ", "),
    key_concepts = jsonlite::toJSON(purpose$key_concepts, auto_unbox = TRUE),
    .llm = modules$llm
  )

  # Stage 4: Final output
  cli_alert_info("Generating llms.txt...")
  llmstxt <- run(
    modules$llmstxt,
    pkg_name = pkg_info$name,
    purpose = purpose$purpose,
    target_audience = purpose$target_audience,
    key_concepts_json = jsonlite::toJSON(purpose$key_concepts, auto_unbox = TRUE),
    organization = structure$organization,
    entry_points = paste(structure$entry_points, collapse = ", "),
    main_files_json = jsonlite::toJSON(structure$main_files, auto_unbox = TRUE),
    basic_example = examples$basic,
    intermediate_example = examples$intermediate,
    gotchas = paste(examples$gotchas, collapse = "; "),
    .llm = modules$llm
  )

  cli_alert_success("Done!")

  # Return everything as a simple list
  list(
    pkg_info = pkg_info,
    purpose = purpose,
    structure = structure,
    examples = examples,
    llmstxt = llmstxt
  )
}
```

### Use It

``` r
# Find package root (works from vignettes/ or project root)
pkg_root <- if (file.exists("DESCRIPTION")) "." else ".."

# Analyze and print
result <- analyze_package(pkg_root)
#> 
#> ── Analyzing package ───────────────────────────────────────────────────────────
#> Using model = "gpt-4.1".
#> ✔ Gathered metadata for dsprrr
#> 
#> ℹ Analyzing purpose...
#> 
#> ℹ Analyzing structure...
#> 
#> ℹ Generating examples...
#> 
#> ℹ Generating llms.txt...
#> 
#> ✔ Done!
cat(result$llmstxt)
```

    #> # dsprrr
    #> 
    #> A framework for principled, test-driven, and automatically optimized LLM workflows in R. Built for R programmers and data scientists who need systematic, composable, and data-driven improvement of prompt pipelines. Inspired by the DSPy framework.
    #> 
    #> ---
    #> 
    #> ## Key Concepts
    #> 
    #> - **Declarative Signatures**: Compact, type-safe notation for defining LLM inputs/outputs, enabling systematic validation and programming.
    #> - **Composable Modules**: Reusable, optimizable blocks that compose LLM programs; modules can be chained and managed independently.
    #> - **Automatic Prompt Optimization**: Data-driven searching and tuning of prompts, removing brittle prompt engineering.
    #> - **Test-driven LLM Programming**: Evaluate and improve workflows with labeled examples and metrics for reliable iteration.
    #> - **Tidyverse Integration**: Native compatibility with R data science pipelines (e.g., tibbles, purrr, dplyr).
    #> 
    #> ---
    #> 
    #> ## Quick Start
    #> 
    #> ```r
    #> library(dsprrr)
    #> 
    #> # Define a signature for a simple Q&A program (declarative I/O)
    #> sig <- signature("question -> answer")
    #> 
    #> # Inspect the signature
    #> print(sig)
    #> ```
    #> 
    #> ---
    #> 
    #> ## Common Workflow
    #> 
    #> ```r
    #> library(dsprrr)
    #> 
    #> # Labeled data for evaluation
    #> examples <- tibble::tibble(
    #>   question = c("What is the capital of France?", "2+2?"),
    #>   answer = c("Paris", "4")
    #> )
    #> 
    #> # Create a composable module
    #> mod <- module(
    #>   signature = signature("question -> answer"),
    #>   template = "Q: {question}\nA:"
    #> )
    #> 
    #> # (Optional) Compile for optimization
    #> dsp_mod <- compile(mod)
    #> 
    #> # Run on new input
    #> y_pred <- run(dsp_mod, tibble::tibble(question = "What is the capital of Germany?"))
    #> print(y_pred)
    #> 
    #> # Evaluate against ground truth
    #> evaluate(dsp_mod, examples)
    #> ```
    #> 
    #> ---
    #> 
    #> ## Code Organization
    #> 
    #> - **signature.R**: Core signature objects and I/O schema logic.
    #> - **module.R**: Composable modules—base class and interface.
    #> - **teleprompter.R**: Prompt optimization and dynamic tuning logic.
    #> - **optimize.R**: Optimization (grid search, systematic scoring, improvement).
    #> - **run.R**: High-level program/workflow execution coordination.
    #> 
    #> Other files: utilities, orchestration, integration, and tracing.
    #> 
    #> ---
    #> 
    #> ## Entry Points
    #> 
    #> - `signature`: Declare LLM input/output schemas.
    #> - `module`: Construct composable program modules.
    #> - `teleprompter`: Optimize prompt templates.
    #> - `compile`: Prepare and optimize modules/programs for execution.
    #> - `run`: Execute on new data.
    #> - `evaluate`: Test predictions against labeled ground truth.
    #> - `dsp`: Experimental pipeline orchestration (DSPy-style).
    #> 
    #> ---
    #> 
    #> ## Watch Out For
    #> 
    #> - **Signature strictness**: Inputs/outputs must match declared signature exactly; errors otherwise.
    #> - **Object model**: Modules, teleprompters, etc., are S7 objects (not lists/functions)—interact via their methods.
    #> - **Template variables**: Template fields must match those declared in signatures (e.g., `{question}`).
    #> 
    #> ---

**This works.** For a one-off script, you’re done.

------------------------------------------------------------------------

## Part 2: Adding Structure with S7

The simple approach works for scripts. But as pipelines grow—more
stages, more developers, production use—plain lists show their limits:

- **No validation**: What if a stage returns `NULL` for a required
  field? You won’t find out until three stages later when something
  breaks mysteriously.
- **No documentation**: What fields does each result have? You’ll need
  to trace through the code.
- **Hard to compose**: Passing results between stages is error-prone.
  Typos in field names silently return `NULL`.

S7 gives you typed containers that catch these problems at construction
time:

``` r
library(S7)
```

### S7 Classes for Results

``` r
# Package metadata
PackageInfo <- new_class("PackageInfo",
  properties = list(
    name = class_character,
    title = class_character,
    description = class_character,
    readme = new_property(class_character, default = ""),
    r_files = new_property(class_character, default = character()),
    exports = new_property(class_character, default = character()),
    dependencies = new_property(class_character, default = character()),
    has_vignettes = new_property(class_logical, default = FALSE)
  )
)

# Purpose analysis result
PurposeAnalysis <- new_class("PurposeAnalysis",
  properties = list(
    purpose = class_character,
    key_concepts = class_list,
    target_audience = class_character,
    prerequisites = new_property(class_character, default = character())
  )
)

# Structure analysis result
StructureAnalysis <- new_class("StructureAnalysis",
  properties = list(
    organization = class_character,
    main_files = class_list,
    entry_points = class_character,
    patterns = class_character
  )
)

# Generated examples
Examples <- new_class("Examples",
  properties = list(
    basic = class_character,
    intermediate = class_character,
    gotchas = new_property(class_character, default = character())
  )
)

# Complete analysis
AnalysisResult <- new_class("AnalysisResult",
  properties = list(
    pkg_info = PackageInfo,
    purpose = PurposeAnalysis,
    structure = StructureAnalysis,
    examples = Examples,
    llmstxt = new_property(class_character, default = "")
  )
)
```

Now you get: - **Type checking**: Can’t create a `PurposeAnalysis`
without a `purpose` - **Documentation**: Class definitions show what
fields exist - **IDE support**: Autocomplete works with `@` slots

### Print Methods

``` r
method(print, PackageInfo) <- function(x, ...) {
  cli_h3("Package: {x@name}")
  cli_text("{x@title}")
  cli_text("{length(x@exports)} exports, {length(x@r_files)} R files")
  invisible(x)
}

method(print, AnalysisResult) <- function(x, ...) {
  cli_h2("Analysis: {x@pkg_info@name}")
  cli_text("{.strong Purpose:} {x@purpose@purpose}")
  cli_text("{.strong Audience:} {x@purpose@target_audience}")
  cli_text("{.strong Entry points:} {.val {x@structure@entry_points}}")
  invisible(x)
}
```

### Updated Gather Function

``` r
gather_package_info <- function(pkg_path = ".") {
  desc_path <- file.path(pkg_path, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    cli_abort("No DESCRIPTION file found at {.path {pkg_path}}")
  }

  desc <- read.dcf(desc_path)

  imports <- desc[1, "Imports"] %||% ""
  deps <- if (nzchar(imports)) {
    trimws(strsplit(imports, ",")[[1]])
  } else {
    character()
  }

  readme_path <- file.path(pkg_path, "README.md")
  readme <- if (file.exists(readme_path)) {
    paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }

  r_dir <- file.path(pkg_path, "R")
  r_files <- if (dir.exists(r_dir)) {
    list.files(r_dir, pattern = "\\.R$", ignore.case = TRUE)
  } else {
    character()
  }

  ns_path <- file.path(pkg_path, "NAMESPACE")
  exports <- if (file.exists(ns_path)) {
    ns_lines <- readLines(ns_path, warn = FALSE)
    export_lines <- grep("^export\\(", ns_lines, value = TRUE)
    gsub("export\\((.+)\\)", "\\1", export_lines)
  } else {
    character()
  }

  vignette_dir <- file.path(pkg_path, "vignettes")
  has_vignettes <- dir.exists(vignette_dir) &&
    length(list.files(vignette_dir, pattern = "\\.(Rmd|qmd)$")) > 0

  # Return S7 object instead of list
  PackageInfo(
    name = desc[1, "Package"],
    title = desc[1, "Title"] %||% "",
    description = desc[1, "Description"] %||% "",
    readme = readme,
    r_files = r_files,
    exports = exports,
    dependencies = deps,
    has_vignettes = has_vignettes
  )
}
```

### Stage Functions

Each stage returns a typed S7 object. This is the key improvement over
the simple approach: if the LLM returns incomplete data (missing
`purpose`, for example), the `PurposeAnalysis()` constructor fails
immediately with a clear error. No silent `NULL` propagation:

``` r
analyze_purpose <- function(pkg_info, modules) {
  cli_alert_info("Analyzing purpose and concepts...")

  result <- run(
    modules$purpose,
    pkg_name = pkg_info@name,
    title = pkg_info@title,
    description_text = pkg_info@description,
    readme_excerpt = substr(pkg_info@readme, 1, 2000),
    exported_functions = paste(pkg_info@exports, collapse = ", "),
    .llm = modules$llm
  )

  PurposeAnalysis(
    purpose = result$purpose,
    key_concepts = result$key_concepts,
    target_audience = result$target_audience,
    prerequisites = result$prerequisites %||% character()
  )
}

analyze_structure <- function(pkg_info, modules) {
  cli_alert_info("Analyzing code structure...")

  result <- run(
    modules$structure,
    pkg_name = pkg_info@name,
    r_files = paste(pkg_info@r_files, collapse = ", "),
    exports = paste(pkg_info@exports, collapse = ", "),
    has_vignettes = pkg_info@has_vignettes,
    dependencies = paste(pkg_info@dependencies, collapse = ", "),
    .llm = modules$llm
  )

  StructureAnalysis(
    organization = result$organization,
    main_files = result$main_files,
    entry_points = result$entry_points,
    patterns = result$patterns
  )
}

generate_examples <- function(pkg_info, purpose, structure, modules) {
  cli_alert_info("Generating usage examples...")

  result <- run(
    modules$examples,
    pkg_name = pkg_info@name,
    purpose = purpose@purpose,
    entry_points = paste(structure@entry_points, collapse = ", "),
    key_concepts = jsonlite::toJSON(purpose@key_concepts, auto_unbox = TRUE),
    .llm = modules$llm
  )

  Examples(
    basic = result$basic,
    intermediate = result$intermediate,
    gotchas = result$gotchas %||% character()
  )
}

generate_llmstxt <- function(pkg_info, purpose, structure, examples, modules) {
  cli_alert_info("Generating llms.txt...")

  run(
    modules$llmstxt,
    pkg_name = pkg_info@name,
    purpose = purpose@purpose,
    target_audience = purpose@target_audience,
    key_concepts_json = jsonlite::toJSON(purpose@key_concepts, auto_unbox = TRUE),
    organization = structure@organization,
    entry_points = paste(structure@entry_points, collapse = ", "),
    main_files_json = jsonlite::toJSON(structure@main_files, auto_unbox = TRUE),
    basic_example = examples@basic,
    intermediate_example = examples@intermediate,
    gotchas = paste(examples@gotchas, collapse = "; "),
    .llm = modules$llm
  )
}
```

### Main Function

``` r
analyze_package <- function(pkg_path = ".", llm = chat_openai()) {
  cli_h1("Analyzing package")

  modules <- create_modules(llm)

  # Gather info (returns PackageInfo)
  pkg_info <- gather_package_info(pkg_path)
  cli_alert_success("Gathered metadata for {.pkg {pkg_info@name}}")
  print(pkg_info)

  # Run pipeline stages (each returns typed result)
  purpose <- analyze_purpose(pkg_info, modules)
  structure <- analyze_structure(pkg_info, modules)
  examples <- generate_examples(pkg_info, purpose, structure, modules)
  llmstxt <- generate_llmstxt(pkg_info, purpose, structure, examples, modules)

  cli_alert_success("Done!")

  # Return typed result
  AnalysisResult(
    pkg_info = pkg_info,
    purpose = purpose,
    structure = structure,
    examples = examples,
    llmstxt = llmstxt
  )
}
```

### Convenience Functions

Finally, we wrap everything in user-friendly functions. These hide the
complexity while preserving access to the full `AnalysisResult` for
users who need it:

``` r
generate_llmstxt_file <- function(pkg_path = ".", output = NULL, llm = chat_openai()) {
  result <- analyze_package(pkg_path, llm)

  output <- output %||% file.path(pkg_path, "llms.txt")
  writeLines(result@llmstxt, output)
  cli_alert_success("Wrote {.file {output}}")

  invisible(result)
}

preview_llmstxt <- function(pkg_path = ".", llm = chat_openai()) {
  result <- analyze_package(pkg_path, llm)
  cat(result@llmstxt)
  invisible(result)
}
```

### Running It

``` r
# Find package root (works from vignettes/ or project root)
pkg_root <- if (file.exists("DESCRIPTION")) "." else ".."

# Analyze current package
result <- analyze_package(pkg_root)
#> 
#> ── Analyzing package ───────────────────────────────────────────────────────────
#> Using model = "gpt-4.1".
#> ✔ Gathered metadata for dsprrr
#> 
#> 
#> 
#> ── Package: dsprrr 
#> 
#> Declarative Self-Improving Language Programs for R
#> 
#> 119 exports, 49 R files
#> 
#> ℹ Analyzing purpose and concepts...
#> 
#> ℹ Analyzing code structure...
#> 
#> ℹ Generating usage examples...
#> 
#> ℹ Generating llms.txt...
#> 
#> ✔ Done!

# View the analysis (uses print method)
print(result)
#> 
#> ── Analysis: dsprrr ──
#> 
#> Purpose: Provides a principled, declarative, and optimizable framework for
#> building and systematically improving LLM-powered applications in R, using
#> programmatic workflows that integrate deeply with tidyverse.
#> Audience: R developers and data scientists building LLM-driven applications who
#> require structured, scalable, and optimizable workflows—especially those using
#> the tidyverse.
#> Entry points: "Teleprompter", "module", "signature", "run", "evaluate", and
#> "optimize_grid"

# See the generated llms.txt
cat(result@llmstxt)
```

    #> # dsprrr
    #> 
    #> **Purpose:**
    #> Provides a principled, declarative, and optimizable framework for building and systematically improving LLM-powered applications in R, using programmatic workflows that integrate deeply with tidyverse.
    #> 
    #> **Target Audience:**
    #> R developers and data scientists building LLM-driven applications who require structured, scalable, and optimizable workflows—especially those using the tidyverse.
    #> 
    #> ---
    #> 
    #> # Key Concepts
    #> - **Declarative Signatures:** Compact, structured notation for specifying LLM input/output, used for workflow definition and validation.
    #> - **Optimizable Modules:** Composable workflow units for LLM processing, supporting evaluation and data-driven improvement.
    #> - **Automatic Prompt Optimization:** Empirical optimization of prompts and workflows using built-in algorithms and strategies.
    #> - **Tracing and Debugging:** Native tools for recording, inspecting, and analyzing LLM process steps and errors.
    #> - **Tidyverse Integration:** Designed for seamless compatibility with tidyverse idioms (e.g., tibble, pipes, functional programming).
    #> 
    #> ---
    #> 
    #> # Quick Start
    #> 
    #> ```r
    #> library(dsprrr)
    #> 
    #> # Create a simple declarative signature for a QA task
    #> sig <- signature("question -> answer")
    #> 
    #> # Create a module using the signature
    #> qa_mod <- module(signature = sig, description = "Simple QA module")
    #> ```
    #> 
    #> ---
    #> 
    #> # Common Workflow
    #> 
    #> ```r
    #> library(dsprrr)
    #> library(tibble)
    #> 
    #> # Sample QA dataset
    #> data <- tibble(
    #>   question = c("What is the capital of France?", "What is 2+2?"),
    #>   answer = c("Paris", "4")
    #> )
    #> 
    #> # Build a teleprompter for few-shot prompting
    #> tp <- Teleprompter(signature = signature("question -> answer"),
    #>                    examples = data)
    #> 
    #> # Run the teleprompter on a new question
    #> result <- run(tp, list(question = "What is the largest planet?"))
    #> print(result)
    #> ```
    #> 
    #> ---
    #> 
    #> # Code Organization
    #> 
    #> Code is organized into modular components:
    #> - **Signatures:** Definition and parsing (`signature.R`)
    #> - **Modules:** Composable workflow units (`module.R`)
    #> - **Teleprompters:** Prompting strategies (`teleprompter.R`, `teleprompter-*.R`)
    #> - **Optimizers:** Logic for workflow/prompt improvement (`optimize.R`)
    #> - **Evaluation:** Metrics and scoring (`evaluate.R`)
    #> - **Tracing:** Logging and debugging utilities (`traces.R`)
    #> 
    #> Each major area is isolated for easy extension and navigation.
    #> 
    #> ---
    #> 
    #> # Entry Points
    #> - `Teleprompter`: Build advanced and few-shot prompting workflows
    #> - `module`: Create and compose workflow modules
    #> - `signature`: Define structured I/O contracts for workflows
    #> - `run`: Execute a module or teleprompter on data
    #> - `evaluate`: Quantitatively score workflow outputs
    #> - `optimize_grid`: Systematically optimize modules/workflows
    #> 
    #> ---
    #> 
    #> # Watch Out For
    #> - **Signature Matching:** Signatures must exactly match your data fields; mismatches cause errors.
    #> - **Proper Signature Objects:** Modules and teleprompters require valid signature objects, not just character strings.
    #> - **Data Format:** Example data must follow the signature's format and match expected input/output columns.

## Example Output

Running on dsprrr produces:

``` markdown
# dsprrr

> DSPy-style LLM programming for R: signatures define I/O, modules
> encapsulate prompts, optimizers improve them automatically.

Data scientists and ML engineers building production LLM applications
who want systematic prompt optimization rather than manual tuning.

## Key Concepts

- **Signature**: Declarative specification of module inputs and outputs
  using arrow notation (`question -> answer`) or explicit types.
- **Module**: Reusable, stateful wrapper around an LLM call with
  configuration and optimization state.
- **Teleprompter**: Optimization strategy that compiles modules by
  adding few-shot examples or refining instructions.
- **Trace**: Record of module execution for debugging and analysis.

## Quick Start

```r
library(dsprrr)
library(ellmer)

mod <- signature("question -> answer") |> module(type = "predict")
run(mod, question = "What is R?", .llm = chat_openai())
```

## Common Workflow

``` r
sig <- signature("context, question -> answer",
                 instructions = "Answer based only on context.")
mod <- module(sig, type = "predict")

trainset <- dsp_trainset(
  context = c("R is for statistics.", "Python is general-purpose."),
  question = c("What is R for?", "Describe Python."),
  answer = c("Statistics", "General-purpose programming")
)

optimized <- compile(LabeledFewShot(k = 2), mod, trainset = trainset)
evaluate(optimized, testset, metric = metric_exact_match())
```

## Code Organization

Core abstractions in signature.R (S7) and module-base.R (R6). Module
variants in separate files. Optimization in teleprompter.R.

### Important Files

- **signature.R**: S7 Signature class with string parser
- **module-base.R**: R6 Module base class
- **module-predict.R**: PredictModule for text generation
- **teleprompter.R**: LabeledFewShot, GEPA, MIPROv2
- **run.R**: run() and run_dataset() generics

## Entry Points

- [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md):
  Define input/output contract
- [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md):
  Create module from signature
- [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md):
  Execute module with inputs
- [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md):
  Optimize with teleprompter
- [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md): Quick
  one-liner for Chat objects

## Watch Out For

- Modules are stateful—clone before modifying shared instances
- [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
  requires `.llm` unless you’ve called
  [`set_default_chat()`](https://jameshwade.github.io/dsprrr/reference/set_default_chat.md)
- Complex outputs need
  [`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
  from ellmer \`\`\`

## When to Use Which

| Approach        | Use When                                      |
|-----------------|-----------------------------------------------|
| **Plain lists** | One-off scripts, quick prototypes             |
| **S7 classes**  | Reusable pipelines, packages, need validation |

## Adapting This Pattern

The staged pipeline pattern adapts to many documentation and analysis
tasks:

| Application          | Gather Stage              | Analysis Stages                           | Output Stage                |
|----------------------|---------------------------|-------------------------------------------|-----------------------------|
| **API docs**         | Parse OpenAPI spec        | Analyze endpoints, group by resource      | Generate markdown reference |
| **Changelogs**       | Parse git commits, issues | Categorize changes, identify breaking     | Generate release notes      |
| **Code review**      | Diff files, parse AST     | Check style, find bugs, assess complexity | Generate review comments    |
| **Test generation**  | Parse function signatures | Identify edge cases, dependencies         | Generate test cases         |
| **Migration guides** | Diff API versions         | Identify breaking changes, patterns       | Generate upgrade steps      |

The key is the same: define clear signatures for each stage, use
structured outputs to pass data between stages, and let S7 classes
enforce the contracts.
