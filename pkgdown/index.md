# dsprrr <img src="man/figures/logo.png" align="right" width="120" alt="dsprrr hex sticker" />

<p class="hero-tagline">Program LLMs in R instead of hand-tuning prompt strings.</p>

Declare a task's inputs and typed outputs once, swap reasoning and tool-use
strategies without rewriting prompts, measure quality on labeled data, and let
an optimizer fit instructions and examples to your metric. Built on
[ellmer](https://ellmer.tidyverse.org) and inspired by
[DSPy](https://dspy.ai).

```{=html}
<div class="hero-actions d-flex flex-wrap align-items-center gap-2 my-4">
  <a class="btn btn-primary" href="articles/tutorial-hello-world.html">Get started</a>
  <a class="btn btn-outline-secondary" href="articles/concepts-signatures-modules.html">How dsprrr works</a>
  <a class="hero-link ms-sm-2" href="articles/dspy-comparison.html">Coming from DSPy?&nbsp;&rarr;</a>
</div>
```

## Why program instead of prompt?

A hand-written prompt mixes three fragile things into one string: *what* you
want, *how* to coax a particular model, and *which examples* to show. `dsprrr`
keeps them separate so you can test, compare, and optimize LLM features like
any other R code.

<div class="row g-3 my-3 compare-grid">
<div class="col-lg-6">
<div class="compare-card compare-before h-100">
<p class="compare-badge">Hand-tuned prompt</p>

```r
# Task and wording tangled in one string
chat$chat(paste(
  "Route this ticket. Return urgency",
  "(low/high) and team. Ticket:", ticket
))
```

- Free-text output you have to parse by hand
- Hard to test across rows or compare versions
- Breaks quietly when you change models

</div>
</div>
<div class="col-lg-6">
<div class="compare-card compare-after h-100">
<p class="compare-badge">Declarative dsprrr program</p>

```r
sig <- signature(
  "ticket -> urgency: enum('low', 'high')"
)
router <- chain_of_thought(sig) |>
  compile(
    BootstrapFewShot(metric = metric),
    trainset = train
  )
```

- Typed R list returned via structured schemas
- Swap strategies or chain steps in one line
- Measure with `evaluate()`, re-compile for any model

</div>
</div>
</div>

If you have a one-off prompt that already works and no data to measure it
against, plain [ellmer](https://ellmer.tidyverse.org) is enough. `dsprrr`
earns its keep once an LLM call becomes a repeatable workflow you need to
measure, compose, or port across models.

## The R modeling mental model

If you have ever fit a model in R, you already know the loop:

```{=html}
<div class="row row-cols-1 row-cols-sm-2 row-cols-xl-4 g-3 my-3 loop-grid">
  <div class="col">
    <div class="loop-card h-100">
      <p class="loop-step">1. Declare</p>
      <p class="loop-fn"><code>signature()</code></p>
      <p class="loop-analogy">Like <code>y ~ x</code></p>
      <p class="loop-desc">State inputs, typed outputs, and task intent without writing prompt plumbing.</p>
    </div>
  </div>
  <div class="col">
    <div class="loop-card h-100">
      <p class="loop-step">2. Build</p>
      <p class="loop-fn"><code>module()</code></p>
      <p class="loop-analogy">Like a model spec</p>
      <p class="loop-desc">Choose direct prediction, chain-of-thought, ReAct tools, or <code>%&gt;&gt;%</code> pipelines.</p>
    </div>
  </div>
  <div class="col">
    <div class="loop-card h-100">
      <p class="loop-step">3. Measure</p>
      <p class="loop-fn"><code>evaluate()</code></p>
      <p class="loop-analogy">Like <code>yardstick</code></p>
      <p class="loop-desc">Score predictions across a data frame and inspect prompts, traces, and cost.</p>
    </div>
  </div>
  <div class="col">
    <div class="loop-card h-100">
      <p class="loop-step">4. Optimize</p>
      <p class="loop-fn"><code>compile()</code></p>
      <p class="loop-analogy">Like <code>fit()</code></p>
      <p class="loop-desc">Search instructions and few-shot demos on training data to raise your metric.</p>
    </div>
  </div>
</div>
```

## How the pieces fit

One support-ticket workflow from declaration to a saved, compiled program:

<div class="row align-items-center g-4 my-3 primitive-row">
<div class="col-lg-5">

<span class="step-pill">01</span>

### Signatures

**Declare the contract.** Put inputs on the left of `->`, typed outputs on the
right, and optional instructions. `dsprrr` builds the prompt and JSON schema
from this, so results arrive as typed R values ready for a data frame.

[How dsprrr works&nbsp;&rarr;](articles/concepts-signatures-modules.html)

</div>
<div class="col-lg-7">

```r
library(dsprrr)

sig <- signature(
  "ticket -> urgency: enum('low', 'high'), team: string",
  instructions = "Route a support ticket."
)
```

</div>
</div>

<div class="row align-items-center g-4 my-3 primitive-row">
<div class="col-lg-5">

<span class="step-pill">02</span>

### Modules

**Swap how it thinks.** The same signature can run as one structured call,
with step-by-step reasoning first, or in a tool-calling loop. Run any module on
one input with `run()` or over a data frame with `run_dataset()`.

[Choose a module type&nbsp;&rarr;](articles/advanced-modules.html)

</div>
<div class="col-lg-7">

```r
chat <- ellmer::chat_openai(model = "gpt-6-luna")

router <- module(sig)                # one call
router_cot <- chain_of_thought(sig)  # reason first

lookup <- ellmer::tool(
  function(q) "Duplicate charges go to billing.",
  description = "Look up routing policy",
  arguments = list(q = ellmer::type_string())
)
router_agent <- react(sig, tools = list(lookup))

run(router, ticket = "Charged twice!", .llm = chat)
```

</div>
</div>

<div class="row align-items-center g-4 my-3 primitive-row">
<div class="col-lg-5">

<span class="step-pill">03</span>

### Pipelines

**Compose multi-step programs.** Chain modules with `%>>%` or wire inputs
explicitly with `pipeline()` and `step()`. A pipeline is itself a module, so
`run()`, `evaluate()`, and `compile()` work across all steps together.

[Chain modules into pipelines&nbsp;&rarr;](articles/chaining-modules.html)

</div>
<div class="col-lg-7">

```r
draft <- module(signature(
  "urgency, team -> reply",
  instructions = "Write a brief acknowledgment."
))

# Outputs connect to matching inputs by name
triage <- module(sig) %>>% draft

run(triage, ticket = "Charged twice!", .llm = chat)
```

</div>
</div>

<div class="row align-items-center g-4 my-3 primitive-row">
<div class="col-lg-5">

<span class="step-pill">04</span>

### Metrics & evaluation

**Measure instead of eyeballing.** Split your labeled rows, run the module over
the validation set, and score every prediction. Inspect misses, prompts
(`get_last_prompt()`), traces (`summarize_traces()`), and spend
(`session_cost()`).

[Metrics and evaluation&nbsp;&rarr;](articles/concepts-why-metrics-matter.html)

</div>
<div class="col-lg-7">

```r
tickets <- tibble::tribble(
  ~ticket,                       ~urgency, ~team,
  "Charged twice for my order",  "high",   "billing",
  "Change my profile photo?",    "low",    "account",
  "App deletes files on sync",   "high",   "technical",
  "Need last year's invoice",    "low",    "billing",
  "Unknown login on my account", "high",   "account",
  "Export fails in Safari",      "low",    "technical"
)

splits <- split_dataset(tickets, prop = 0.67, seed = 1)
metric <- metric_exact_match(field = "urgency")

baseline <- evaluate(
  router, splits$val, metric = metric, .llm = chat
)
baseline$mean_score
```

</div>
</div>

<div class="row align-items-center g-4 my-3 primitive-row">
<div class="col-lg-5">

<span class="step-pill">05</span>

### Optimizers & persistence

**Fit prompts to your data.** `compile()` returns a new module whose demos or
instructions were chosen to raise your metric on training rows (`BootstrapFewShot`,
`MIPROv2`, `GEPA`, and more). Verify on held-out rows, then save with
`save_program()`. Switch models? Swap `chat` and re-compile.

[Compile and optimize&nbsp;&rarr;](articles/compilation-optimization.html)

</div>
<div class="col-lg-7">

```r
optimizer <- BootstrapFewShot(
  metric = metric,
  max_bootstrapped_demos = 2L,
  max_labeled_demos = 0L
)

optimized <- router |>
  compile(optimizer, splits$train, .llm = chat)

evaluate(
  optimized, splits$val, metric, .llm = chat
)
save_program(optimized, "ticket_router.rds")
```

</div>
</div>

## Works with your R stack

```{=html}
<div class="row row-cols-1 row-cols-sm-2 row-cols-lg-3 g-3 my-3 eco-grid">
  <div class="col">
    <div class="eco-card h-100">
      <p class="eco-title"><a href="articles/models-and-providers.html">ellmer providers</a></p>
      <p class="eco-desc">Runs on any chat ellmer supports: OpenAI, Anthropic, Google, Ollama, Bedrock, and more.</p>
    </div>
  </div>
  <div class="col">
    <div class="eco-card h-100">
      <p class="eco-title"><a href="articles/vitals-recipes.html">vitals evaluation</a></p>
      <p class="eco-desc">Expose modules as vitals solvers and reuse vitals scorers directly as dsprrr metrics.</p>
    </div>
  </div>
  <div class="col">
    <div class="eco-card h-100">
      <p class="eco-title"><a href="articles/rag-workflows.html">ragnar retrieval</a></p>
      <p class="eco-desc">Connect document stores to RAG modules and ReAct agents with <code>ragnar_tool()</code>.</p>
    </div>
  </div>
  <div class="col">
    <div class="eco-card h-100">
      <p class="eco-title"><a href="articles/tidymodels-integration.html">tidymodels</a></p>
      <p class="eco-desc">Tune LLM parameters alongside workflows with the <code>llm_predict()</code> parsnip spec.</p>
    </div>
  </div>
  <div class="col">
    <div class="eco-card h-100">
      <p class="eco-title"><a href="articles/orchestration.html">pins &amp; targets</a></p>
      <p class="eco-desc">Version compiled configs with <code>pin_module_config()</code> and orchestrate batch runs.</p>
    </div>
  </div>
  <div class="col">
    <div class="eco-card h-100">
      <p class="eco-title"><a href="reference/index.html#run-programs">Caching, async &amp; traces</a></p>
      <p class="eco-desc">Built-in memory and disk cache, <code>run_async()</code>, <code>run_stream()</code>, and token/cost logs.</p>
    </div>
  </div>
</div>
```

## Where to go next

```{=html}
<div class="row row-cols-1 row-cols-sm-2 g-3 my-3 path-grid">
  <div class="col">
    <div class="path-card h-100">
      <p class="path-kicker">New to dsprrr</p>
      <p class="path-title"><a href="articles/tutorial-hello-world.html">Follow the 6-step tutorials&nbsp;&rarr;</a></p>
      <p class="path-desc">Go from your first typed call to structured extraction, demos, optimization, and saving a compiled module.</p>
    </div>
  </div>
  <div class="col">
    <div class="path-card h-100">
      <p class="path-kicker">Already using ellmer</p>
      <p class="path-title"><a href="articles/concepts-signatures-modules.html">Read how dsprrr works&nbsp;&rarr;</a></p>
      <p class="path-desc">Understand signatures, R6 module state, traces, and when a plain ellmer prompt is still the right tool.</p>
    </div>
  </div>
  <div class="col">
    <div class="path-card h-100">
      <p class="path-kicker">Coming from Python DSPy</p>
      <p class="path-title"><a href="articles/dspy-comparison.html">See the DSPy comparison&nbsp;&rarr;</a></p>
      <p class="path-desc">Translate signatures, modules, teleprompters, and pipelines between Python DSPy and idiomatic R.</p>
    </div>
  </div>
  <div class="col">
    <div class="path-card h-100">
      <p class="path-kicker">Ready to optimize</p>
      <p class="path-title"><a href="articles/advanced-optimization.html">Choose an optimizer&nbsp;&rarr;</a></p>
      <p class="path-desc">Compare all eleven teleprompters — from <code>LabeledFewShot</code> and <code>BootstrapFewShot</code> to <code>MIPROv2</code> and <code>GEPA</code>.</p>
    </div>
  </div>
</div>
```

## Installation {#installation}

Install the released version from CRAN:

```r
install.packages("dsprrr")
```

Or install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("JamesHWade/dsprrr")
```

Set a provider API key in your `.Renviron` (for example `OPENAI_API_KEY` or
`ANTHROPIC_API_KEY`), then run `dsprrr_sitrep()` to verify your setup.
Installing from source requires a C compiler (Rtools on Windows, Xcode Command
Line Tools on macOS, or your Linux distribution's build tools); the compiled
helper preserves exact filesystem identities for safe persistence.

**Experimental features:** `rlm_module()` lets a model explore a large R object
by writing code ([How RLM works](articles/how-rlm-works.html)), `flex()` lets
GEPA rewrite a whole program ([Flex](articles/flex-optimization.html)),
`with_decisions()` and `ReAnchor()` calibrate decision thresholds
([Calibrated decisions](articles/calibrated-decisions.html)), and `Omni()` and
the agentic harnesses compose optimizers
([Omni](articles/omni-meta-optimization.html),
[agentic harnesses](articles/agentic-optimization-harnesses.html)).

`dsprrr` is experimental; the API may change. It is inspired by
[DSPy](https://dspy.ai) from Stanford NLP, builds on
[ellmer](https://ellmer.tidyverse.org) for model access, and bridges to
[vitals](https://vitals.tidyverse.org) for evaluation.
