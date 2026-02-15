# Getting Started with dsprrr

## What is dsprrr?

dsprrr treats LLM prompts as **programs that can be optimized**, not
strings to be tweaked by hand. You declare what you want with
*signatures*, wrap them in *modules* for reuse, and let *teleprompters*
find the best prompts automatically.

``` r
library(dsprrr)
library(ellmer)

# Declare what you want
chat_openai() |> dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

That one-liner handles prompt construction, structured output parsing,
and type validation. No prompt engineering required.

## Choose Your Path

### New to dsprrr?

Start with the **tutorial sequence**—hands-on lessons that build on each
other:

1.  [Your First LLM
    Call](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md)
    — Make structured calls with
    [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md) (10
    min)
2.  [Building a
    Classifier](https://jameshwade.github.io/dsprrr/articles/tutorial-build-classifier.md)
    — Create reusable modules (20 min)
3.  [Extracting Structured
    Data](https://jameshwade.github.io/dsprrr/articles/tutorial-structured-outputs.md)
    — Multi-field outputs (25 min)
4.  [Improving with
    Examples](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md)
    — Few-shot learning (25 min)
5.  [Finding Best
    Configuration](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md)
    — Grid search (30 min)
6.  [Taking to
    Production](https://jameshwade.github.io/dsprrr/articles/tutorial-deploy-to-production.md)
    — Save and deploy (30 min)

### Already know the basics?

Jump to what you need:

- **[Quick
  Reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.md)**
  — Signature syntax, module types, metrics
- **[Troubleshooting](https://jameshwade.github.io/dsprrr/articles/troubleshooting.md)**
  — Fix common issues

### Want to understand the “why”?

Read the conceptual guides:

- **[The DSPy
  Philosophy](https://jameshwade.github.io/dsprrr/articles/concepts-dspy-philosophy.md)**
  — Programs, not prompts
- **[Understanding Signatures &
  Modules](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)**
  — Why S7 and R6
- **[How Optimization
  Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md)**
  — Teleprompter theory

### Building something specific?

Check the how-to guides:

- **[Compile &
  Optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)**
  — Full optimization workflow
- **[Build RAG
  Pipelines](https://jameshwade.github.io/dsprrr/articles/rag-workflows.md)**
  — Retrieval-augmented generation
- **[Deploy to
  Production](https://jameshwade.github.io/dsprrr/articles/orchestration.md)**
  — pins, targets, and Quarto

## 5-Minute Taste

Here’s dsprrr in action—from simple call to optimized module:

``` r
library(dsprrr)
library(ellmer)

# 1. Quick call
chat <- chat_openai()
chat |> dsp("text -> sentiment: enum('positive', 'negative', 'neutral')",
            text = "Love this product!")
#> "positive"

# 2. Reusable module
classifier <- chat |> as_module("text -> sentiment: enum('positive', 'negative', 'neutral')")
classifier$predict(text = c("Great!", "Awful", "Meh"))
#> c("positive", "negative", "neutral")

# 3. Optimized module
trainset <- dsp_trainset(
  text = c("Amazing!", "Terrible!", "It's okay"),
  sentiment = c("positive", "negative", "neutral")
)

optimized <- compile_module(
  program = classifier,
  teleprompter = LabeledFewShot(k = 2),
  trainset = trainset
)
```

## Prerequisites

Before you begin:

2.  **Install the packages**:

``` r
# install.packages("pak")
pak::pak("JamesHWade/dsprrr")
```

3.  **Set your API key**:

``` r
# In your .Renviron file
OPENAI_API_KEY=sk-your-key-here
```

## Learning Path

``` mermaid
flowchart TB
  subgraph Tutorials
    T1["1. Hello World"] --> T2["2. Build Classifier"] --> T3["3. Structured Outputs"]
    T3 --> T4["4. Demos"] --> T5["5. Optimize"] --> T6["6. Production"]
  end

  T6 --> Adv["Advanced Tutorials"]
  Adv --> TA["Text Adventure"]
  Adv --> LL["llms.txt Gen"]

  TA --> Split["Next Steps"]
  LL --> Split

  Split --> HowTo["How-To Guides"]
  Split --> Concepts["Concepts"]

  HowTo --> Reference["Reference"]
  Concepts --> Cheatsheet["Cheatsheet"]
  Cheatsheet --> Reference
```

Start at the top and work your way down. Each tutorial builds on the
previous one.

## What’s Next?

Ready to begin? Start with **[Tutorial 1: Your First LLM
Call](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md)**.
