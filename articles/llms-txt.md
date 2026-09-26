# Generate an llms.txt file

[llms.txt](https://llmstxt.org/) is a proposed Markdown file at the root
of a website that tells language models what the site is for and where
to look. This project builds a dsprrr program that writes one for an R
package, runs the code examples it produces, and turns that check into a
metric you can optimize the program against. It is adapted from the
[DSPy llms.txt
tutorial](https://dspy.ai/tutorials/llms_txt_generation/).

If your package has a pkgdown site, you already have an llms.txt. Since
pkgdown 2.2.0,
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
writes one that joins your home page with the reference and article
indexes, and adds a Markdown copy of every page. That file lists
everything on your site. It can’t tell a model which functions a
newcomer needs or which mistakes people make. This project writes that
shorter, curated summary, and because a program writes it, you can
measure it and improve it.

``` r

library(dsprrr)
library(ellmer)
```

## Gather the facts

Everything the model sees comes from files R can read without a model:
the DESCRIPTION, the README, the file names in `R/`, and the exports in
`NAMESPACE`.

``` r

gather_package_info <- function(pkg_path = ".") {
  path <- function(...) file.path(pkg_path, ...)
  file_lines <- function(file) {
    if (!file.exists(path(file))) return(character())
    readLines(path(file), warn = FALSE)
  }
  desc <- read.dcf(path("DESCRIPTION"))
  # read.dcf() only returns the fields a package actually has
  field <- function(name) {
    if (name %in% colnames(desc)) unname(desc[1, name]) else ""
  }
  exports <- grep("^export\\(", file_lines("NAMESPACE"), value = TRUE)

  list(
    name = field("Package"),
    title = field("Title"),
    description = field("Description"),
    readme = paste(file_lines("README.md"), collapse = "\n"),
    r_files = list.files(path("R"), pattern = "\\.R$", ignore.case = TRUE),
    exports = gsub("export\\((.+)\\)", "\\1", exports),
    dependencies = trimws(strsplit(field("Imports"), ",")[[1]]),
    has_vignettes = length(list.files(path("vignettes"), "\\.(Rmd|qmd)$")) > 0
  )
}

# "." from the package root, ".." from the vignettes folder
pkg_root <- if (file.exists("DESCRIPTION")) "." else ".."
info <- gather_package_info(pkg_root)
info$title
#> [1] "Declarative Self-Improving Language Programs"
lengths(info[c("r_files", "exports", "dependencies")])
#>      r_files      exports dependencies 
#>           72          169           18
```

## Four stages

The generator makes four calls, each with its own signature:

| Stage | Module | Reads | Writes |
|----|----|----|----|
| Purpose | [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md) | title, description, README excerpt, exports | purpose, key concepts, audience, prerequisites |
| Structure | [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md) | file names, exports, dependencies | organization, main files, entry points, patterns |
| Examples | [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md) | purpose, entry points, key concepts | a basic and an intermediate example, gotchas |
| Write | [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) | all of the above | the llms.txt text |

The output types are ellmer types, so each stage returns fields the next
one can use without parsing prose:

``` r

analyze_purpose_sig <- signature(
  inputs = list(
    input("pkg_name", description = "Package name"),
    input("title", description = "Package title from DESCRIPTION"),
    input("description_text", description = "Description from DESCRIPTION"),
    input("readme_excerpt", description = "First 2000 chars of README"),
    input("exported_functions", description = "Comma-separated exports")
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

analyze_structure_sig <- signature(
  inputs = list(
    input("pkg_name", description = "Package name"),
    input("r_files", description = "R files in R/ directory"),
    input("exports", description = "Exported function names"),
    input("has_vignettes", description = "Whether package has vignettes"),
    input("dependencies", description = "Package dependencies")
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

generate_examples_sig <- signature(
  inputs = list(
    input("pkg_name", description = "Package name"),
    input("purpose", description = "What the package does"),
    input("entry_points", description = "Main functions"),
    input("key_concepts", description = "Core concepts as JSON")
  ),
  output_type = type_object(
    basic = type_string("3-5 line minimal example"),
    intermediate = type_string("5-10 line common workflow"),
    gotchas = type_array(type_string(), "1-3 common mistakes")
  ),
  instructions = "Generate realistic R code examples.
Examples must be syntactically valid R."
)

generate_llmstxt_sig <- signature(
  inputs = list(
    input("pkg_name", description = "Package name"),
    input("purpose", description = "Package purpose"),
    input("target_audience", description = "Who uses this"),
    input("key_concepts_json", description = "JSON of term/definition pairs"),
    input("organization", description = "Code organization"),
    input("entry_points", description = "Main functions"),
    input("main_files_json", description = "JSON of file/purpose pairs"),
    input("basic_example", description = "Basic usage example"),
    input("intermediate_example", description = "Intermediate example"),
    input("gotchas", description = "Common mistakes")
  ),
  output_type = type_string(),
  instructions = "Generate llms.txt in markdown format with sections:
# {pkg_name}, Key Concepts, Quick Start, Common Workflow,
Code Organization, Entry Points, Watch Out For.
Keep it concise - this is reference documentation for AI systems."
)
```

The three analysis stages use
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
which adds a `reasoning` output and asks the model to think before it
answers. The writer only formats, so it is a plain
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).
Printing a module shows exactly what it will ask for:

``` r

purpose_mod <- chain_of_thought(analyze_purpose_sig)
structure_mod <- chain_of_thought(analyze_structure_sig)
examples_mod <- chain_of_thought(generate_examples_sig)
writer_mod <- module(generate_llmstxt_sig)

examples_mod
#> 
#> ── PredictModule ──
#> 
#> ── Signature
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • pkg_name: "string" - Package name
#> • purpose: "string" - What the package does
#> • entry_points: "string" - Main functions
#> • key_concepts: "string" - Core concepts as JSON
#> 
#> ── Output
#> Type: "object(reasoning: string, basic: string, intermediate: string, gotchas:
#> array(string))"
#> 
#> ── Instructions
#> Generate realistic R code examples. Examples must be syntactically valid R.
#> Think through your reasoning step by step before providing the answer.
```

## Run it

`analyze_package()` passes each stage’s fields to the next.
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) treats
an input longer than one as a batch, one call per element, so vectors
and tables are collapsed to single strings on the way:

``` r

analyze_package <- function(info, llm = chat_openai(model = "gpt-6-luna")) {
  # One chat for all four stages, so later stages also see earlier turns.
  purpose <- run(
    purpose_mod,
    pkg_name = info$name,
    title = info$title,
    description_text = info$description,
    readme_excerpt = substr(info$readme, 1, 2000),
    exported_functions = paste(info$exports, collapse = ", "),
    .llm = llm
  )
  structure <- run(
    structure_mod,
    pkg_name = info$name,
    r_files = paste(info$r_files, collapse = ", "),
    exports = paste(info$exports, collapse = ", "),
    has_vignettes = info$has_vignettes,
    dependencies = paste(info$dependencies, collapse = ", "),
    .llm = llm
  )
  key_concepts <- jsonlite::toJSON(purpose$key_concepts, auto_unbox = TRUE)
  entry_points <- paste(structure$entry_points, collapse = ", ")

  examples <- run(
    examples_mod,
    pkg_name = info$name,
    purpose = purpose$purpose,
    entry_points = entry_points,
    key_concepts = key_concepts,
    .llm = llm
  )
  run(
    writer_mod,
    pkg_name = info$name,
    purpose = purpose$purpose,
    target_audience = purpose$target_audience,
    key_concepts_json = key_concepts,
    organization = structure$organization,
    entry_points = entry_points,
    main_files_json = jsonlite::toJSON(structure$main_files, auto_unbox = TRUE),
    basic_example = examples$basic,
    intermediate_example = examples$intermediate,
    gotchas = paste(examples$gotchas, collapse = "; "),
    .llm = llm
  )
}
```

The writer’s output type is a bare
[`type_string()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
so [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
returns the text itself. Here is the file gpt-4.1 wrote for dsprrr when
this article was recorded, in January 2026; the code now uses
gpt-6-luna, whose file will read differently:

``` r

llms_txt <- analyze_package(info)
cat(llms_txt)
```

    #> # dsprrr
    #> 
    #> Declarative, test-driven, and data-optimizable workflows for LLM-based applications in R, based on the DSPy framework.
    #> 
    #> ## Key Concepts
    #> 
    #> - **Declarative Signatures:** Compact notation specifying LLM inputs/outputs for clarity and validation.
    #> - **Automated Prompt Optimization:** Systematically improve prompts and modules based on empirical data.
    #> - **Composable LLM Modules:** Modular workflow components for assembly and reuse.
    #> - **Integrated Tracing and Debugging:** Logging, analysis, and error tracing of LLM workflows.
    #> - **Tidyverse Integration:** Seamless use with tidyverse data science pipelines.
    #> 
    #> ## Quick Start
    #> 
    #> ```r
    #> library(dsprrr)
    #> 
    #> # Define a simple signature: question -> answer
    #> sig <- signature("question -> answer")
    #> 
    #> # Create a module (LLM-driven Q&A)
    #> qa_mod <- module(signature = sig)
    #> 
    #> # Run a simple LLM prediction (mock input)
    #> result <- run(qa_mod, question = "What is the capital of France?")
    #> print(result)
    #> ```
    #> 
    #> ## Common Workflow
    #> 
    #> ```r
    #> library(dsprrr)
    #> library(tibble)
    #> 
    #> # Define signature
    #> dir_sig <- signature("document -> sentiment")
    #> 
    #> # Create a classification module
    #> sentiment_mod <- module(signature = dir_sig, task = "Classify sentiment of the document.")
    #> 
    #> # Training data (labeled examples)
    #> labeled_data <- tibble(
    #>   document = c("I love R!", "This is terrible...", "It's fine, I guess."),
    #>   sentiment = c("positive", "negative", "neutral")
    #> )
    #> 
    #> # Compile the module (optional for optimization)
    #> compiled_mod <- compile(sentiment_mod)
    #> 
    #> # Optimize LLM module using examples
    #> grid_search <- optimize_grid(compiled_mod, trainset = labeled_data)
    #> 
    #> # Evaluate on new data
    #> test_data <- tibble(document = c("Amazing!", "Awful!"), sentiment = c("positive", "negative"))
    #> results <- evaluate(grid_search$best_config, dataset = test_data)
    #> print(results)
    #> ```
    #> 
    #> ## Code Organization
    #> 
    #> The codebase is organized in modular units:
    #> 
    #> - **signature.R:** Handles declarative signature specification and validation.
    #> - **module-base.R:** Implements composable LLM workflow logic.
    #> - **optimize.R:** Contains routines for data-driven LLM prompt/program optimization.
    #> - **run.R:** Entry points for workflow orchestration and execution.
    #> - **compile.R:** Transforms module workflows into optimized executables.
    #> 
    #> ## Entry Points
    #> 
    #> - `signature`: Define LLM input/output contracts.
    #> - `module`: Construct modular LLM operators.
    #> - `compile`: Prepare modules/workflows for execution or optimization.
    #> - `run`: Execute LLM workflow on input data.
    #> - `optimize_grid`: Automate configuration search/optimization with labeled data.
    #> - `evaluate`: Assess LLM/module performance against ground truth.
    #> 
    #> ## Watch Out For
    #> 
    #> - Always define signatures first; modules depend on them.
    #> - Optimization/evaluation requires labeled datasets; without them, data-driven features are disabled.
    #> - Use dsprrr (not ellmer) for systematic optimization, test-driven workflows, and trace/debug capabilities.

## Check the examples

The prose is plausible. The Common Workflow code is not:
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
has no `task` argument,
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
needs a teleprompter,
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
has no `trainset` argument, and
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
takes `data`, not `dataset`. Look back at the printed `examples_mod`:
the examples stage gets function names and a purpose statement, but no
argument lists, so the model guesses them.

Reading every generated example for mistakes like these doesn’t scale.
Running them does. `run_block()` evaluates one code block in a fresh R
process, in a temporary directory and without API keys, so model-written
code stays away from your session and your credentials:

``` r

code_blocks <- function(md) {
  blocks <- regmatches(md, gregexpr("(?s)```r\\n.*?\\n```", md, perl = TRUE))
  gsub("^```r\\n|\\n```$", "", blocks[[1]])
}

run_block <- function(code) {
  callr::r(
    function(code) {
      setwd(tempdir())
      tryCatch(
        {
          eval(parse(text = code), envir = new.env())
          NA_character_
        },
        error = function(e) gsub("\\s+", " ", rlang::cnd_header(e))
      )
    },
    args = list(code = code),
    env = c(OPENAI_API_KEY = "", ANTHROPIC_API_KEY = "", GOOGLE_API_KEY = "")
  )
}

# One entry per code block: NA if it ran, otherwise the error
example_problems <- function(md) {
  problems <- vapply(code_blocks(md), run_block, "", USE.NAMES = FALSE)
  # Stopping at the model call counts as running: there is no chat here.
  problems[grepl("^No default Chat", problems)] <- NA
  problems
}

example_problems(llms_txt)
```

    #> [1] NA                                                                          
    #> [2] "`module()` creates standard prediction modules and does not accept `task`."

The Quick Start runs as far as
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md), which
is as far as it can get without a model. The Common Workflow stops at
its first mistake.

One sample says little, so here is a second run of the same program:

``` r

second_run <- analyze_package(info)
example_problems(second_run)
#> [1] NA                                                 
#> [2] "Missing required inputs: \033[32mquestion\033[39m"
```

Different text, same kind of failure: this time the intermediate example
passes a whole tibble to
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
(`run(qa_mod, test_data)`), which expects named inputs. Rerunning until
an example happens to work is not a fix. The program needs to show the
model how the functions are called, and you need a score that tells you
whether the change worked.

## Make it one program

`analyze_package()` is ordinary R wrapped around four modules, so dsprrr
can’t evaluate or compile it as a whole. A
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
can be: it runs like a module,
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
scores it on a dataset, and
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
compiles all of its steps together. Pipelines are linear, though. Each
step receives only the previous step’s outputs, plus fixed values passed
to [`step()`](https://jameshwade.github.io/dsprrr/reference/step.md)
(see [Chain modules into
pipelines](https://jameshwade.github.io/dsprrr/articles/chaining-modules.md)).

So the redesign has two steps. The first reads the facts and writes
notes, including a quick start, and it now also gets every exported
function with its arguments. The second turns the notes into Markdown:

``` r

usage_lines <- function(pkg) {
  ns <- asNamespace(pkg)
  fns <- Filter(\(f) is.function(ns[[f]]), sort(getNamespaceExports(pkg)))
  vapply(fns, \(f) paste0(f, "(", toString(names(formals(ns[[f]]))), ")"), "")
}

notes <- chain_of_thought(signature(
  inputs = list(
    input("pkg_name", description = "Package name"),
    input("description_text", description = "Description from DESCRIPTION"),
    input("usage", description = "Exported functions and their arguments")
  ),
  output_type = type_object(
    purpose = type_string("One sentence: what problem does this solve?"),
    key_concepts = type_array(type_string(), "3-5 core concepts"),
    quick_start = type_string("5-10 lines of R that run as written"),
    gotchas = type_array(type_string(), "1-3 common mistakes")
  ),
  instructions = "Summarize this R package for other language models.
Call only functions listed in `usage`, with the arguments listed there."
))

write_up <- module(signature(
  "purpose, key_concepts, quick_start, gotchas -> llms_txt",
  instructions = "Write the body of an llms.txt file in Markdown:
a one-paragraph summary, then Key Concepts, Quick Start (the code unchanged,
in an R code block) and Watch Out For."
))

# select drops the reasoning field before it reaches the writer
llms_program <- pipeline(
  step(notes, select = c("purpose", "key_concepts", "quick_start", "gotchas")),
  write_up
)
```

The package name stops at the first step, so R adds the heading:

``` r

llm <- chat_openai(model = "gpt-6-luna")

out <- run(
  llms_program,
  pkg_name = "dsprrr",
  description_text = info$description,
  usage = paste(usage_lines("dsprrr"), collapse = "\n"),
  .llm = llm
)
writeLines(c("# dsprrr", "", out$llms_txt), "llms-summary.txt")
```

To publish the result with a pkgdown site, put it in `pkgdown/assets/`,
whose files are copied to the site root. Keep a name other than
`llms.txt`, or pkgdown’s own file will replace it.

## Measure it and improve it

The share of code blocks that run is a metric. It needs no reference
answer, so a training set is just a few packages described the same way:

``` r

package_facts <- function(pkg) {
  tibble::tibble(
    pkg_name = pkg,
    description_text = utils::packageDescription(pkg)$Description,
    usage = paste(usage_lines(pkg), collapse = "\n")
  )
}
packages <- do.call(rbind, lapply(c("glue", "jsonlite", "withr"), package_facts))

examples_run <- function(prediction, expected) {
  problems <- example_problems(prediction$llms_txt)
  if (length(problems) == 0) 0 else mean(is.na(problems))
}

evaluate(llms_program, packages, metric = examples_run, .llm = llm)$mean_score
```

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
can then improve the program without anyone editing a prompt.
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
runs the pipeline on each package, keeps the runs whose examples all ran
(`metric_threshold = 1`), and adds their notes and write-ups to both
steps as demonstrations. It warns that `packages` has no output column,
which is expected here because the metric doesn’t compare against one:

``` r

tuned <- compile(
  llms_program,
  BootstrapFewShot(
    metric = examples_run,
    metric_threshold = 1,
    max_bootstrapped_demos = 2L
  ),
  packages,
  .llm = llm
)

dsprrr_facts <- package_facts("dsprrr")
evaluate(tuned, dsprrr_facts, metric = examples_run, .llm = llm)$mean_score
```

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
returns a new program and leaves `llms_program` unchanged, so you can
score both on packages that were not in the training set before
choosing. [Compile and
optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)
covers the other optimizers and how to choose between them.
