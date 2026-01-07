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
    #> Enables robust, test-driven, and optimizable LLM workflows in R via declarative programming, automatic prompt optimization, and modular program structure, leveraging user data for systematic improvement.
    #> 
    #> ---
    #> 
    #> ## Key Concepts
    #> 
    #> - **Declarative Signatures:** Compact notations specifying the structure of LLM inputs and outputs for programmatic workflows.
    #> - **Self-Improving LLM Workflows:** Treat LLM prompts and workflows as structured programs that can be automatically optimized and improved using test data.
    #> - **Composable Modules:** Reusable components that define parts of LLM workflows, enabling modular and flexible applications.
    #> - **Prompt Optimization:** Automated search and adaptation of prompts and workflow parameters, improving performance against evaluation data.
    #> - **Integration with tidyverse:** Seamless compatibility with R's tidyverse toolkit, facilitating data manipulation and workflow integration.
    #> 
    #> ---
    #> 
    #> ## Quick Start
    #> 
    #> Basic example:
    #> 
    #> ```r
    #> library(dsprrr)
    #> 
    #> # Declare a simple LLM workflow signature:
    #> qa_sig <- signature("question -> answer")
    #> ```
    #> 
    #> ---
    #> 
    #> ## Common Workflow
    #> 
    #> ```r
    #> library(dsprrr)
    #> 
    #> # 1. Define signature for a Q&A task
    #> qa_sig <- signature("question -> answer")
    #> 
    #> # 2. Create a simple module for the task
    #> qa_module <- module(
    #>   signature = qa_sig,
    #>   prompt = "Answer the following question: {question}"
    #> )
    #> 
    #> # 3. Compile the module to prep for execution
    #> qa_prog <- compile(qa_module)
    #> 
    #> # 4. Run with an example input
    #> a_result <- run(qa_prog, list(question = "What is R?") )
    #> 
    #> # 5. Print result
    #> print(a_result)
    #> ```
    #> 
    #> ---
    #> 
    #> ## Code Organization
    #> 
    #> Code is organized by functional domains: signatures, modules, optimization, orchestration, evaluation, and utilities. Each domain typically has a core file and specialized variants, reflecting a modular and extensible architecture.
    #> 
    #> - **signature.R:** Handles declarative signatures for LLM I/O.
    #> - **module.R and module-base.R:** Abstractions for 'modules'; specialized module-*.R files extend functionality.
    #> - **compile.R:** Compilation of modules/workflows.
    #> - **optimize.R, optimizer-*.R:** Optimization/search logic for self-improvement.
    #> - **run.R, orchestration.R:** Orchestrate, run, and manage LLM workflows.
    #> 
    #> ---
    #> 
    #> ## Entry Points
    #> 
    #> - `signature()`
    #> - `module()`
    #> - `compile()`
    #> - `run()`
    #> - `evaluate()`
    #> - `Teleprompter`
    #> 
    #> ---
    #> 
    #> ## Watch Out For
    #> 
    #> - Not loading or attaching the dsprrr package before using `signature()` or `module()`.
    #> - Mis-specifying signature notation (e.g., missing `->` or typo in variable names).
    #> - Trying to `run()` a module before compiling it with `compile()`.

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

Now you get:

- **Type checking**: Can’t create a `PurposeAnalysis` without a
  `purpose`
- **Documentation**: Class definitions show what fields exist
- **IDE support**: Autocomplete works with `@` slots

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
#> 130 exports, 50 R files
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
#> Purpose: dsprrr provides a declarative, test-driven, and optimizable
#> programming framework to systematically build, evaluate, and improve LLM
#> workflows in R, bridging prompt engineering and formal programmatic
#> optimization.
#> Audience: Advanced R developers and data scientists building complex,
#> data-driven LLM workflows who need systematic optimization and robust
#> debugging, especially in tidyverse-centric environments.
#> Entry points: "signature", "module", "Teleprompter", "run", "optimize_grid",
#> "evaluate", and "compile_module"

# See the generated llms.txt
cat(result@llmstxt)
```

    #> # dsprrr
    #> 
    #> Declarative, test-driven, and optimizable LLM program framework for R. Build, evaluate, and systematically improve LLM workflows using principled abstractions, composable modules, and data-driven optimization.
    #> 
    #> ## Key Concepts
    #> 
    #> - **Declarative Signatures**: Compact, type-safe notation for specifying LLM input/output structures.
    #> - **Composable Modules**: Reusable units for encapsulating and composing complex LLM workflows, integrated with tidyverse/ellmer.
    #> - **Test-driven LLM Optimization**: Guide prompt and workflow tuning with labeled data and metrics for systematic improvement.
    #> - **Program Tracing and Debugging**: Capture execution traces and diagnose multi-step or agentic pipeline errors.
    #> - **Integration with tidyverse and ellmer**: Seamless embedding in R data pipelines and LLM dialogue frameworks.
    #> 
    #> ## Quick Start
    #> 
    #> ### Basic Example
    #> ```r
    #> library(dsprrr)
    #> 
    #> # Define a signature for input/output structure
    #> sig <- signature("question -> answer")
    #> 
    #> # Create a trivial module (no LLM call, just structure)
    #> mod <- module(sig)
    #> 
    #> print(mod)
    #> ```
    #> 
    #> ### Intermediate (LLM-powered classification)
    #> ```r
    #> library(dsprrr)
    #> 
    #> sig <- signature("complaint -> category:string")
    #> complaint_classifier <- module(sig, 
    #>   prompt = "Classify the customer complaint into one of: billing, technical, service, other.")
    #> 
    #> dat <- data.frame(complaint = c(
    #>   "My internet has been down for days!",
    #>   "Why was I charged twice this month?",
    #>   "Can you make the agent more polite?"
    #> ))
    #> 
    #> results <- run(complaint_classifier, dat)
    #> print(results)
    #> 
    #> y_true <- c("technical", "billing", "service")
    #> eval <- evaluate(results$category, y_true, metric = metric_exact_match)
    #> print(eval)
    #> 
    #> best_mod <- optimize_grid(
    #>   complaint_classifier, dat, y_true, 
    #>   grid = list(temperature = c(0, 0.5, 1)),
    #>   metric = metric_exact_match
    #> )
    #> print(best_mod)
    #> ```
    #> 
    #> ## Common Workflow
    #> 1. **Declare signatures** to specify expected structure.
    #> 2. **Build modules** for each logical unit or LLM-powered task.
    #> 3. **Compose modules** for end-to-end workflows.
    #> 4. **Run** on datasets via `run()` or `run_dataset()`.
    #> 5. **Evaluate** and optimize with real data/metrics.
    #> 6. **Debug**/trace execution; iterate for improvement.
    #> 
    #> ## Code Organization
    #> - **signature.R**: Signature parsing/creation utilities.
    #> - **module.R**: Module definition/composition logic.
    #> - **teleprompter.R**: Prompt engineering & modular LLM workflow strategies.
    #> - **optimize.R**: Grid/random search optimization primitives.
    #> - **run.R**: Orchestration of LLM workflows, batch execution.
    #> 
    #> ## Entry Points
    #> - `signature` – Define workflow I/O structure.
    #> - `module` – Create LLM workflow components.
    #> - `Teleprompter` – Advanced prompt/module interface.
    #> - `run` – Execute modules over data.
    #> - `optimize_grid` – Tune workflow parameters systematically.
    #> - `evaluate` – Score outputs with custom metrics.
    #> - `compile_module` – Finalize/lock module configurations.
    #> 
    #> ## Watch Out For
    #> - **No prompt defined**: Modules need explicit prompt content to invoke LLMs.
    #> - **Signature/data mismatch**: Inputs/outputs in signature must match dataset column names/types.
    #> - **Metric omission**: Always supply a metric to `optimize_grid()` and `evaluate()` for meaningful results.

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
