# Tutorial 1: Your First LLM Call

In this tutorial, you’ll make your first structured LLM call with
dsprrr. By the end, you’ll be able to ask questions, get typed
responses, and understand why signatures are powerful.

**Time**: 10-15 minutes

## What You’ll Build

A working question-answering system that returns structured data—not
just raw text.

## Prerequisites

- R installed
- An OpenAI API key (set as `OPENAI_API_KEY` environment variable)
- Install the packages:

``` r

install.packages("pak")
pak::pak("JamesHWade/dsprrr")
pak::pak("tidyverse/ellmer")
```

## Step 1: Load the Packages

``` r

library(dsprrr)
library(ellmer)
```

You should see no errors. If you do, check that your API key is set
correctly.

## Step 2: Create a Chat Connection

Connect to OpenAI:

``` r

chat <- chat_openai()
```

This creates a chat object you’ll use for all your LLM calls.

## Step 3: Your First Structured Call

Let’s ask a simple question using
[`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md):

``` r

chat |> dsp("question -> answer", question = "What is the capital of France?")
```

You should see `"Paris"` returned. Let’s break down what happened:

- `"question -> answer"` is a **signature**—it declares one input
  (`question`) and one output (`answer`)
- dsprrr handled all the prompt engineering for you
- The result came back as structured data, not raw text

## Step 4: Try Different Questions

The same signature works for any question:

``` r

chat |> dsp("question -> answer", question = "What is 7 * 8?")

chat |> dsp("question -> answer", question = "Who wrote Romeo and Juliet?")
```

Notice how you get clean, direct answers—no extra prose.

## Step 5: Add Output Types

So far, answers have been strings. But what if you want a number or a
specific choice?

### Getting a Number

``` r

chat |> dsp(
  "math_problem -> result: number",
  math_problem = "What is 15% of 200?"
)
```

The `: number` after `result` tells dsprrr you want a numeric value, not
a string.

### Getting a Choice (Enum)

``` r

chat |> dsp(
  "text -> sentiment: enum('positive', 'negative', 'neutral')",
  text = "I absolutely loved this movie!"
)
```

The LLM must pick from exactly those three options. Try changing the
text to see different results:

``` r

chat |> dsp(
  "text -> sentiment: enum('positive', 'negative', 'neutral')",
  text = "This was a complete waste of time."
)

chat |> dsp(
  "text -> sentiment: enum('positive', 'negative', 'neutral')",
  text = "It was okay, I guess."
)
```

### Getting True/False

``` r

chat |> dsp(
  "statement -> is_true: bool",
  statement = "The Earth orbits the Sun."
)

chat |> dsp(
  "statement -> is_true: bool",
  statement = "Cats are larger than elephants."
)
```

## Step 6: Multiple Inputs

Signatures can have multiple inputs. Separate them with commas:

``` r

chat |> dsp(
  "context, question -> answer",
  context = "R was created in 1993 by Ross Ihaka and Robert Gentleman at the University of Auckland.",
  question = "When was R created?"
)
```

Now the LLM uses your context to answer the question:

``` r

chat |> dsp(
  "context, question -> answer",
  context = "The bakery opens at 7am and closes at 6pm. They sell croissants for $3 each.",
  question = "How much do croissants cost?"
)
```

## Step 7: Adding Instructions

You can guide the LLM’s behavior with instructions:

``` r

chat |> dsp(
  signature("question -> answer", instructions = "Answer in exactly one word."),
  question = "What color is the sky on a clear day?"
)

chat |> dsp(
  signature("question -> answer", instructions = "Answer like a pirate."),
  question = "What is the capital of France?"
)
```

## What You Learned

In this tutorial, you:

1.  Made your first structured LLM call with
    [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md)
2.  Used signatures to declare inputs and outputs
3.  Added output types: `string`, `number`, `bool`, `enum()`
4.  Combined multiple inputs
5.  Added instructions to guide behavior

## What’s Different from Raw LLM Calls?

Without dsprrr, you’d write prompts like:

    You are a helpful assistant. The user will ask a question.
    Respond with just the answer, nothing else.
    User: What is the capital of France?

With dsprrr, you just declare `"question -> answer"` and the framework
handles the rest. This becomes powerful when you need to:

- Optimize prompts automatically
- Chain multiple LLM calls together
- Get consistent, typed outputs

## Next Steps

Ready to build something reusable? Continue to:

- **[Tutorial 2: Building a
  Classifier](https://jameshwade.github.io/dsprrr/articles/tutorial-build-classifier.md)**
  — Create a module you can use repeatedly
- **[Quick
  Reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.md)**
  — Look up signature syntax
- **[The DSPy
  Philosophy](https://jameshwade.github.io/dsprrr/articles/concepts-dspy-philosophy.md)**
  — Understand *why* signatures work this way
