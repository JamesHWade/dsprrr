# The DSPy Philosophy: Programs, Not Prompts

This article explains the philosophy behind dsprrr and why treating
prompts as programs leads to better LLM applications.

## The Prompt Engineering Problem

Traditional LLM development looks like this:

1.  Write a prompt
2.  Test it manually
3.  Tweak the wording
4.  Test again
5.  Repeat until it “seems to work”

This approach has fundamental problems:

**Fragility**: A prompt that works perfectly with GPT-4 may fail with
Claude. A prompt optimized for one model version breaks when the
provider updates their model. Your carefully crafted prompt is tied to a
specific moment in time.

**Subjectivity**: “Seems to work” isn’t a metric. Without systematic
evaluation, you’re cherry-picking examples that confirm your prompt
works while ignoring failures. Confirmation bias runs rampant.

**Maintenance burden**: As requirements change, you accumulate prompt
variants. Which version is production? Which was tested? Your prompt
library becomes technical debt.

**No composability**: Prompts are strings. You can’t easily combine two
prompts, pass one to another, or build pipelines. Every integration is
ad-hoc string manipulation.

## The Insight: Prompts Are Programs

DSPy (Declarative Self-improving Language Programs), developed at
Stanford NLP, introduced a paradigm shift: **treat prompts as programs
that can be optimized**.

Instead of: - Writing prompt strings - Manually testing and tweaking

You: - Declare input/output specifications - Let the framework generate
prompts - Optimize automatically with your data

This is the same shift that happened in machine learning: from
hand-tuned features to learned representations.

## Three Pillars of DSPy Thinking

### Pillar 1: Signatures Define Contracts

A signature declares *what* you want, not *how* to ask for it:

``` r

# This is a contract: given a question, return an answer
signature("question -> answer")

# This is also a contract: given text, return a sentiment classification
signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
```

The signature is an interface. It says nothing about: - What prompt
template to use - How to format the examples - What instructions to
include

Those details are implementation—handled by the framework, not you.

**Why this matters**: When you separate interface from implementation,
you can change the implementation without changing the interface.
Optimize the prompt? The signature stays the same. Switch models? The
signature stays the same. Add few-shot examples? The signature stays the
same.

### Pillar 2: Modules Encapsulate Behavior

A module wraps a signature with executable logic:

``` r

mod <- module(signature("text -> sentiment"), type = "predict")
```

Modules are:

- **Stateful**: They remember their configuration, demos, and traces
- **Composable**: You can chain modules together
- **Optimizable**: Their parameters can be tuned automatically

Think of modules like functions in programming. A function has: - A
signature (input types → output types) - Implementation (the code) -
State (closure variables)

A dsprrr module has: - A signature (inputs → outputs) - Implementation
(LLM calls with a prompt template) - State (configuration, demos,
traces)

**Why this matters**: Modules are the unit of reuse. You build a module
once, optimize it, and deploy it. The optimization results travel with
the module.

### Pillar 3: Optimizers Find What Works

Here’s the radical part: instead of hand-tuning prompts, you define a
metric and let the optimizer search:

``` r

mod$optimize_grid(
  data = training_data,
  metric = metric_exact_match(),
  parameters = list(temperature = c(0, 0.3, 0.7))
)
```

The optimizer: - Tries each configuration - Measures performance against
your metric - Keeps the best one

This works because:

1.  LLMs are sensitive to prompt wording (small changes → big effects)
2.  The search space is navigable (parameters are mostly continuous)
3.  Evaluation is cheap compared to training

**Why this matters**: Human intuition about prompts is unreliable.
Systematic search finds configurations humans wouldn’t try. And unlike
manual tuning, optimization is reproducible.

## Mental Model: LLM Applications as Statistical Programs

Here’s a useful way to think about it:

**Traditional ML**: Data → Model → Predictions - The model has learned
parameters - Training optimizes those parameters - Inference uses the
trained parameters

**LLM Applications**: Prompt → LLM → Response - The prompt configures
behavior - The prompt is the “learned” artifact - Inference uses the
optimized prompt

In both cases, you have: - A specification (model architecture /
signature) - Learned parameters (weights / prompt configuration) - An
optimization process (gradient descent / grid search + teleprompters)

dsprrr makes this analogy explicit. A “compiled” module is like a
trained model—it has optimized parameters (prompt configuration, demos)
found through optimization.

## Why This Matters for R Users

R users already think statistically. You’re comfortable with: - Defining
models declaratively (`y ~ x1 + x2`) - Fitting models to data
([`lm()`](https://rdrr.io/r/stats/lm.html),
[`glm()`](https://rdrr.io/r/stats/glm.html)) - Evaluating with held-out
data - Comparing models with metrics

dsprrr brings this workflow to LLMs: - Define the task declaratively
(signatures) - Fit to data (teleprompters) - Evaluate with held-out data
([`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)) -
Compare with metrics

The tidyverse integration makes this natural:

``` r

# This feels like R
trainset |>
  run_dataset(classifier) |>
  mutate(correct = sentiment == expected_sentiment) |>
  summarize(accuracy = mean(correct))
```

## The dsprrr Adaptation

dsprrr brings DSPy’s ideas to R with some adaptations:

**S7 for Signatures**: Immutable, validated type objects. Once you
define a signature, it doesn’t change. This enables optimization—you can
swap prompts while keeping the interface stable.

**R6 for Modules**: Stateful execution context. Modules accumulate
traces, store optimized configurations, and provide an object-oriented
API (`$predict()`, `$optimize_grid()`).

**ellmer Integration**: dsprrr builds on ellmer’s chat infrastructure.
You get the same
[`chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.html),
[`chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html)
objects, with structured outputs handled via ellmer’s type system.

**tidyverse Conventions**: Tibbles everywhere. Pipe-friendly APIs.
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
returns a tibble. Traces are tibbles. Everything composes with dplyr.

## When Traditional Prompting Still Works

Not everything needs dsprrr. Use plain prompts when:

**The task is simple and stable**: If you need to summarize text and the
requirements won’t change, a direct ellmer call is fine.

**You’re exploring**: In early development, before you have training
data or clear requirements, manual prompting helps you understand the
problem.

**The prompt is truly one-off**: A single query to answer a single
question doesn’t benefit from the optimization infrastructure.

**You don’t have labeled data**: Optimization needs a metric, which
needs expected outputs. Without labels, you can’t optimize.

## The Paradigm Shift

The move from prompt engineering to prompt programming mirrors other
shifts in software development:

| From                     | To                   |
|--------------------------|----------------------|
| Assembly                 | High-level languages |
| Manual memory management | Garbage collection   |
| Hand-tuned SQL           | Query optimizers     |
| Feature engineering      | Deep learning        |
| Prompt engineering       | Prompt programming   |

Each shift: - Raises the abstraction level - Lets humans focus on
*what*, not *how* - Enables optimization beyond human intuition -
Improves reproducibility

dsprrr is this shift for LLM applications in R.

## Practical Implications

If you accept this philosophy, your workflow changes:

**Before (prompt engineering)**:

1.  Write prompt
2.  Try examples manually
3.  Tweak wording
4.  Deploy when it “feels right”
5.  Hope it keeps working

**After (prompt programming)**:

1.  Define signature
2.  Create labeled dataset
3.  Define metric
4.  Optimize
5.  Evaluate on held-out data
6.  Deploy with confidence
7.  Monitor with traces

The second workflow is: - Reproducible (anyone can re-run the
optimization) - Measurable (you have metrics, not vibes) - Maintainable
(change the data, re-optimize) - Debuggable (traces show what happened)

## Further Reading

- **[Tutorial 1: Your First LLM
  Call](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md)**
  — See signatures in action
- **[Understanding Signatures &
  Modules](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)**
  — Why S7 and R6
- **[How Optimization
  Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md)**
  — The theory behind teleprompters
- **[DSPy Paper](https://arxiv.org/abs/2310.03714)** — The academic
  foundations
- **[DSPy Documentation](https://dspy.ai)** — The Python original
