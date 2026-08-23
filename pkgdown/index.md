# dsprrr <img src="man/figures/logo.png" align="right" width="120" alt="dsprrr hex sticker" />

## Programming—not prompting—LLMs in R

dsprrr brings the power of [DSPy](https://dspy.ai) to R. Instead of wrestling
with prompt strings, **declare** what you want, **compose** modules into
pipelines, and use **optimization** to improve prompts, examples, or the program
that executes them.

```r
# Install
pak::pak("JamesHWade/dsprrr")

# Configure credentials for an ellmer-supported provider, then:
library(dsprrr)
answerer <- module(signature("question -> answer"))
run(answerer, question = "What is the capital of France?")
#> $answer
#> [1] "Paris"
```

## Compose programs with reusable primitives

Every dsprrr program is built from the same three pieces. Learn these and the rest of the package falls into place.

<div class="row align-items-center g-4 my-4 primitive-row">
<div class="col-md-5">

### Signatures

**Declare your task.** Define typed inputs and outputs instead of wrestling with prompt strings. Portable, maintainable, and easy to iterate on.

[Learn about signatures &rarr;](articles/concepts-signatures-modules.html)

</div>
<div class="col-md-7">

```r
# Route a support ticket
sig <- signature(
  "ticket -> urgency: enum('low', 'high'), team: string"
)
```

</div>
</div>

<div class="row align-items-center g-4 my-4 primitive-row">
<div class="col-md-5">

### Modules

**Same interface, different strategy.** Modules control how a signature executes—reason step by step, use tools, or run ensembles—without rewriting the task.

[Explore modules &rarr;](articles/advanced-modules.html)

</div>
<div class="col-md-7">

```r
sig <- signature(
  "ticket -> urgency: enum('low', 'high'), team: string"
)

# Direct completion
classify <- module(sig)

# Add step-by-step reasoning
classify <- chain_of_thought(sig)

# Add a tool-use loop
lookup_tool <- ellmer::tool(
  function(query) paste("Found:", query),
  description = "Look up support policy details",
  arguments = list(query = ellmer::type_string())
)
classify <- react(sig, tools = list(lookup_tool))
```

</div>
</div>

<div class="row align-items-center g-4 my-4 primitive-row">
<div class="col-md-5">

### Optimizers

**Compile your program against a metric.** Give dsprrr examples and a scoring
function; it selects the best candidate observed within the configured budget.

[Try optimizers &rarr;](articles/compilation-optimization.html)

</div>
<div class="col-md-7">

```r
route_sig <- signature("ticket -> urgency: enum('low', 'high')")
router <- module(route_sig)
trainset <- tibble::tibble(
  ticket  = c("Package lost", "Need a receipt"),
  urgency = c("high", "low")
)

tp <- GEPA(metric = metric_exact_match(field = "urgency"))
optimized <- compile(router, tp, trainset)

board <- pins::board_temp()
pin_module_config(board, "ticket-router-v2", optimized)
```

</div>
</div>

## Find the release cohort that broke

A release-level average can reveal a regression without explaining it. If the
useful grouping and calculation are not known in advance, an RLM keeps the
source object in an R environment, lets the model inspect it with R code, and
returns only bounded observations between steps.

The deterministic tutorial asks an RLM to explore 40,000 checkout sessions and
200 change records. The answer is fixed and independently validated:

```text
release:      2.4.0
cohort:       platform=mobile / plan=pro
before_rate:  0.92
after_rate:   0.61
drop_pp:      31
change_id:    CHG-1842
```

```r
investigator <- rlm_module(
  paste(
    "sessions, changes, question ->",
    "release: string, cohort: string, before_rate: number,",
    "after_rate: number, drop_pp: number, change_id: string, evidence: string"
  ),
  interpreter_factory = function() {
    r_code_runner(timeout = 30, persistent = TRUE)
  },
  max_iterations = 8,
  max_llm_calls = 0L
)
```

This persistent callr runner stages rich R objects once, but it is explicitly
trusted-input-only. For compact JSON-compatible context, the one-call `rlm()`
helper creates a fresh managed `mcp-repl` OS sandbox by default. It disables
network access but allows writes inside the workspace. Install `mcptools` and
the external `mcp-repl` executable first. The final JSON-RPC request has a 7 KB
wire bound, with gzip/base64 attempted when the raw request is too large, and
RLM control frames have a 3,000-byte encoded bound.

[Investigate the deterministic regression &rarr;](articles/tutorial-rlm-dsprrr.html)
&nbsp;&middot;&nbsp;
[Understand the RLM execution contract &rarr;](articles/how-rlm-works.html)

### ProgramOfThought, RLM, or Flex?

| If the task requires... | Use... |
|---|---|
| A calculation whose steps are already known | `program_of_thought()` |
| A new exploration path for this input at inference time | `rlm_module()` |
| A reusable implementation discovered from labeled examples | `flex()` with GEPA |

RLM is experimental inference-time exploration; one invocation does not learn
an exploration program. Supported optimizers can still tune its action and
fallback instructions. Flex searches during compilation for an implementation
to reuse.

## Optimize the program, not only the prompt

Most optimizers improve instructions inside a workflow you designed. `flex()`
lets GEPA change the workflow itself: which predictors run, where deterministic
R is enough, and when to call a tool you supplied.

The worked example starts with a support router that asks a model about every
ticket. In a deterministic GEPA replay, the selected program checks known
incident codes directly and reserves the model for ambiguous prose. It keeps
all six held-out routes correct while cutting predictor calls from six to three.

> Flex did not find a better prompt. It found that half the tickets did not
> need one.

[See the complete router demo and decide when Flex fits &rarr;](articles/flex-optimization.html)

## Getting Started: Configure Your LLM

<ul class="nav nav-tabs" id="providerTabs" role="tablist">
  <li class="nav-item" role="presentation">
    <button class="nav-link active" id="openai-tab" data-bs-toggle="tab" data-bs-target="#openai" type="button" role="tab">OpenAI</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="anthropic-tab" data-bs-toggle="tab" data-bs-target="#anthropic" type="button" role="tab">Anthropic</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="gemini-tab" data-bs-toggle="tab" data-bs-target="#gemini" type="button" role="tab">Gemini</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="ollama-tab" data-bs-toggle="tab" data-bs-target="#ollama" type="button" role="tab">Ollama</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="auto-tab" data-bs-toggle="tab" data-bs-target="#auto" type="button" role="tab">Auto-detect</button>
  </li>
</ul>
<div class="tab-content" id="providerTabsContent">
  <div class="tab-pane fade show active" id="openai" role="tabpanel">

```r
library(dsprrr)
library(ellmer)

chat <- chat_openai(model = "gpt-4o-mini")
answerer <- module(signature("question -> answer"))
run(answerer, question = "What is 2+2?", .llm = chat)
#> $answer
#> [1] "4"
```

  </div>
  <div class="tab-pane fade" id="anthropic" role="tabpanel">

```r
library(dsprrr)
library(ellmer)

chat <- chat_claude(model = "claude-sonnet-4-20250514")
answerer <- module(signature("question -> answer"))
run(answerer, question = "What is 2+2?", .llm = chat)
#> $answer
#> [1] "4"
```

  </div>
  <div class="tab-pane fade" id="gemini" role="tabpanel">

```r
library(dsprrr)
library(ellmer)

chat <- chat_google_gemini(model = "gemini-2.0-flash")
answerer <- module(signature("question -> answer"))
run(answerer, question = "What is 2+2?", .llm = chat)
#> $answer
#> [1] "4"
```

  </div>
  <div class="tab-pane fade" id="ollama" role="tabpanel">

```r
library(dsprrr)
library(ellmer)

chat <- chat_ollama(model = "llama3.2")
answerer <- module(signature("question -> answer"))
run(answerer, question = "What is 2+2?", .llm = chat)
#> $answer
#> [1] "4"
```

  </div>
  <div class="tab-pane fade" id="auto" role="tabpanel">

```r
# dsprrr auto-detects from environment variables
library(dsprrr)

# Uses OPENAI_API_KEY, ANTHROPIC_API_KEY, or GOOGLE_API_KEY
answerer <- module(signature("question -> answer"))
run(answerer, question = "What is 2+2?")
#> $answer
#> [1] "4"
```

  </div>
</div>

## Define a task. Grow it into a system.

Start with a single signature and grow it into a multi-step program—the
same building blocks scale from a one-line extractor to a full pipeline.

<ul class="nav nav-tabs" id="moduleTabs" role="tablist">
  <li class="nav-item" role="presentation">
    <button class="nav-link active" id="extract-tab" data-bs-toggle="tab" data-bs-target="#extract" type="button" role="tab">Extract</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="agent-tab" data-bs-toggle="tab" data-bs-target="#agent" type="button" role="tab">Agent</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="pipeline-tab" data-bs-toggle="tab" data-bs-target="#pipeline" type="button" role="tab">Pipeline</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="multimodal-tab" data-bs-toggle="tab" data-bs-target="#multimodal" type="button" role="tab">Multimodal</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="optimize-tab" data-bs-toggle="tab" data-bs-target="#optimize" type="button" role="tab">Optimize</button>
  </li>
</ul>
<div class="tab-content example-gallery" id="moduleTabsContent">
  <div class="tab-pane fade show active" id="extract" role="tabpanel">

Signatures define a task and enforce typed outputs.

```r
# Extract several typed fields in one call
extract <- signature(
  "message -> name: string, email: string,
   intent: enum('meeting', 'intro', 'follow-up')"
) |> module()

result <- run(
  extract,
  message = "I'm Sarah (sarah@acme.co). Meet Thursday?",
  .llm = chat_openai()
)

# In simple mode (the default), run() returns the parsed output directly
result$name    #> "Sarah"
result$email   #> "sarah@acme.co"
result$intent  #> "meeting"
```

[Extract structured data &rarr;](articles/tutorial-structured-outputs.html)

  </div>
  <div class="tab-pane fade" id="agent" role="tabpanel">

Define tools as functions and hand them to a ReAct module.

```r
kb_search <- function(query) {
  paste(
    "Evaluators compare module outputs with labeled examples.",
    "Optimizers use those scores to select better prompts and demos."
  )
}

search <- ellmer::tool(
  function(query) kb_search(query),
  description = "Search a knowledge base",
  arguments = list(query = ellmer::type_string())
)

agent <- signature("question -> answer") |>
  react(tools = list(search))

answer <- run(
  agent,
  question = "How do dsprrr optimizers improve a module?",
  .llm = chat_openai()
)
answer$answer
#> "They score outputs against examples, then keep better prompts and demos."
```

[Build a tool-using agent &rarr;](articles/advanced-modules.html)

  </div>
  <div class="tab-pane fade" id="pipeline" role="tabpanel">

Compose modules into a pipeline with `%>>%`—outputs flow to inputs.

```r
# Pull a claim, then verify it against the source
find <- signature("article -> claim: string, source: string") |>
  chain_of_thought()

verify <- signature("claim, source -> verdict") |>
  chain_of_thought()

factcheck <- find %>>% verify

news_article <- "Acme reported that revenue grew 12% in Q4."
verdict <- run(factcheck, article = news_article, .llm = chat_openai())
verdict$verdict
#> "supported"
```

[Chain modules into pipelines &rarr;](articles/chaining-modules.html)

  </div>
  <div class="tab-pane fade" id="multimodal" role="tabpanel">

Name an image input in the signature and pass an ellmer content object.

```r
analyze <- signature("image, question -> answer") |>
  module()

run(
  analyze,
  image    = ellmer::ContentImageRemote(
    "https://www.r-project.org/logo/Rlogo.png"
  ),
  question = "What logo is shown?",
  .llm     = chat_openai()
)
#> "The image shows the R project logo."
```

[Work with multimodal inputs &rarr;](articles/advanced-ellmer.html)

  </div>
  <div class="tab-pane fade" id="optimize" role="tabpanel">

Optimizers improve a program against a metric—no prompt rewriting.

```r
extract <- signature(
  "message -> intent: enum('meeting', 'intro')"
) |>
  module()

trainset <- tibble::tibble(
  message = c("I'm Sarah (sarah@acme.co). Meet Thursday?",
              "Hi, this is Dev—just saying hello!"),
  intent  = c("meeting", "intro")
)

optimized <- compile(
  extract,
  GEPA(metric = metric_exact_match(field = "intent")),
  trainset
)

board <- pins::board_temp()
pin_module_config(board, "extract-v2", optimized)
```

[Optimize with your data &rarr;](articles/compilation-optimization.html)

  </div>
</div>

## Automatic Optimization

dsprrr can automatically optimize your prompts using your data.

<ul class="nav nav-tabs" id="optimizeTabs" role="tablist">
  <li class="nav-item" role="presentation">
    <button class="nav-link active" id="fewshot-tab" data-bs-toggle="tab" data-bs-target="#fewshot" type="button" role="tab">Few-Shot</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="grid-tab" data-bs-toggle="tab" data-bs-target="#grid" type="button" role="tab">Grid Search</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="agentic-opt-tab" data-bs-toggle="tab" data-bs-target="#agentic-opt" type="button" role="tab">Agentic</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="eval-tab" data-bs-toggle="tab" data-bs-target="#eval" type="button" role="tab">Evaluation</button>
  </li>
</ul>
<div class="tab-content" id="optimizeTabsContent">
  <div class="tab-pane fade show active" id="fewshot" role="tabpanel">

```r
# Add examples automatically
trainset <- tibble::tibble(
  text = c("Great product!", "Awful experience", "It works"),
  sentiment = c("positive", "negative", "neutral")
)

optimized <- compile(
  classifier,
  LabeledFewShot(k = 3),
  trainset
)

# Now includes 3 examples in every prompt
run(optimized, text = "Amazing service!", .llm = chat_openai())
#> "positive"
```

**Result**: Few-shot examples improve accuracy on edge cases.

  </div>
  <div class="tab-pane fade" id="grid" role="tabpanel">

```r
# Search over configurations
classifier$optimize_grid(
  data = validation_data,
  metric = metric_exact_match(),
  parameters = list(
    temperature = c(0.1, 0.5, 1.0),
    prompt_style = c("concise", "detailed")
  )
)

# View results
module_trials(classifier)
#> # A tibble: 6 × 4
#>   temperature prompt_style score    n
#>         <dbl> <chr>        <dbl> <int>
#> 1         0.1 concise      0.92    100
#> 2         0.1 detailed     0.88    100
#> ...
```

**Result**: Find the best configuration for your task.

  </div>
  <div class="tab-pane fade" id="agentic-opt" role="tabpanel">

```r
# Keep agent-proposed analysis inside an OS sandbox
runner <- mcp_repl_runner()

harness <- MetaHarness(
  metric = metric_exact_match(field = "sentiment"),
  max_iterations = 6L,
  max_candidates_per_iteration = 3L
)

optimized <- compile(
  classifier,
  harness,
  trainset,
  valset = validation_data,
  .llm = task_chat,
  .agent_llm = proposer_chat,
  runner = runner
)
```

**Result**: Search coordinated instructions and templates across an entire
module graph while dsprrr retains control of evaluation, budgets, lineage, and
accepted state.

[Run agentic optimization &rarr;](articles/agentic-optimization-harnesses.html)

  </div>
  <div class="tab-pane fade" id="eval" role="tabpanel">

```r
# Rigorous evaluation with metrics
results <- evaluate(
  classifier,
  test_data,
  metric = metric_exact_match()
)

results$mean_score
#> 0.94

# Integrate with vitals for advanced evaluation
library(vitals)
solver <- as_vitals_solver(classifier)
```

**Result**: Measure and track performance systematically.

  </div>
</div>

## Why dsprrr?

<div class="row row-cols-1 row-cols-md-3 g-4 my-4">
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Declarative</h5>
<p class="card-text">Define <strong>what</strong> you want, not <strong>how</strong> to prompt. Signatures like <code>"text -> sentiment"</code> describe your task clearly.</p>
</div>
</div>
</div>
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Composable</h5>
<p class="card-text">Build complex pipelines from simple modules. Each module is testable, optimizable, and reusable.</p>
</div>
</div>
</div>
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Optimizable</h5>
<p class="card-text">Automatically improve prompts with your data. Few-shot learning, grid search, and advanced teleprompters.</p>
</div>
</div>
</div>
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Integrated</h5>
<p class="card-text">Built on <a href="https://ellmer.tidyverse.org">ellmer</a> for LLM access and <a href="https://vitals.tidyverse.org">vitals</a> for evaluation. Works with tidyverse.</p>
</div>
</div>
</div>
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Observable</h5>
<p class="card-text">Inspect bounded module traces, debug failures, and track usage when providers report it.</p>
</div>
</div>
</div>
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Deployment building blocks</h5>
<p class="card-text">Persistence with pins, orchestration with targets, deployment with vetiver.</p>
</div>
</div>
</div>
</div>

## Learn More

### Tutorials

- [Getting Started](articles/getting-started.html) — Your first dsprrr module
- [Compilation & Optimization](articles/compilation-optimization.html) — Improve with data
- [Release Regression with an RLM](articles/tutorial-rlm-dsprrr.html) — Investigate a large R object adaptively
- [Agentic Optimization Harnesses](articles/agentic-optimization-harnesses.html) — Run sandboxed AutoResearch and Meta-Harness loops
- [Vitals Integration](articles/vitals-integration.html) — Advanced evaluation
- [Production Orchestration](articles/orchestration.html) — Deploy to production

### Reference

- [Function Reference](reference/index.html) — All functions documented
- [How the RLM Works](articles/how-rlm-works.html) — Execution, replay, typed submission, and runner boundaries

## Ecosystem

dsprrr integrates with much of Posit's LLM ecosystem:

| Package | Purpose |
|---------|---------|
| [ellmer](https://ellmer.tidyverse.org) | Chat with LLMs from R |
| [vitals](https://vitals.tidyverse.org) | LLM evaluation framework |
| [shinychat](https://posit-dev.github.io/shinychat/) | Chat UIs for Shiny |
| [mcptools](https://posit-dev.github.io/mcptools/) | Connect R programs to MCP tools |
| [mcp-repl](https://github.com/posit-dev/mcp-repl) | Persistent OS-sandboxed R execution |

Inspired by [DSPy](https://dspy.ai) from Stanford NLP.
