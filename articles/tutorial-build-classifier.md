# Tutorial 2: Building a Reusable Classifier

In [Tutorial
1](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md),
you built and ran a typed module. Now you will use that same contract
across hundreds of texts.

In this tutorial, you’ll build a **reusable module**—a classifier you
can use over and over.

**Time**: 20-25 minutes

## What You’ll Build

A sentiment classifier that: - Processes single texts or batches -
Remembers its configuration - Can be saved and reused

## Prerequisites

- Completed [Tutorial
  1](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md)
- `OPENAI_API_KEY` set in your environment

``` r

library(dsprrr)
library(ellmer)
```

## Step 1: Declare the Classifier

Declare the task once as a signature:

``` r

sentiment_sig <- signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')"
)
```

The signature is the reusable typed contract for every call.

## Step 2: Create a Reusable Module

Wrap the signature in a reusable module:

``` r

chat <- chat_openai()
classifier <- module(sentiment_sig)

classifier
```

Now `classifier` is an object you can use repeatedly.

## Step 3: Classify Single Texts

Use [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) to
classify:

``` r

run(classifier, text = "I absolutely loved this movie!", .llm = chat)
```

Try a few more:

``` r

run(classifier, text = "This was a complete waste of time.", .llm = chat)

run(classifier, text = "It was okay, I guess.", .llm = chat)

run(
  classifier,
  text = "The service was terrible but the food was amazing.",
  .llm = chat
)
```

## Step 4: Batch Processing

Here’s where modules shine. Process multiple texts with
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md):

``` r

reviews <- tibble::tibble(
  text = c(
    "Best purchase I've ever made!",
    "Broke after one day. Total garbage.",
    "Does what it says. Nothing special.",
    "Exceeded all my expectations!",
    "Would not recommend to anyone."
  )
)

run_dataset(classifier, reviews, .llm = chat)
```

All five classifications came back from one dataset operation, while
dsprrr retained one observable provider attempt per review.

## Step 5: Add Instructions

Add task-specific guidance to the signature:

``` r

# Define the signature separately
sig <- signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the overall sentiment. If mixed, choose the dominant emotion."
)

sig
```

Now create a module from the signature:

``` r

classifier2 <- module(sig)

classifier2
```

## Step 6: Running with `run()`

With the full control approach, use
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) to
execute:

``` r

run(classifier2, text = "This is fantastic!", .llm = chat)
```

Notice you pass the chat object via `.llm`. This gives you
flexibility—you can use different LLMs for different calls.

Batch processing works the same way:

``` r

run(
  classifier2,
  text = c("Love it!", "Hate it!", "It's fine"),
  .llm = chat
)
```

## Step 7: Working with Data Frames

Real data often comes in data frames. Use
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md):

``` r

library(tibble)

reviews_df <- tibble(
  id = 1:4,
  text = c(
    "Absolutely wonderful experience!",
    "Never buying from them again.",
    "Solid product, fair price.",
    "Changed my life for the better."
  )
)

results <- run_dataset(classifier2, reviews_df, .llm = chat)
results
```

The results include your original columns plus the classification.

## Step 8: Adding Descriptions

Make your inputs more informative with descriptions:

``` r

sig <- signature(
  inputs = list(
    input("review_text", description = "Customer review to classify")
  ),
  output_type = type_enum(values = c("positive", "negative", "neutral")),
  instructions = "Classify the customer sentiment."
)

detailed_classifier <- module(sig)

run(
  detailed_classifier,
  review_text = "Five stars! Would buy again!",
  .llm = chat
)
```

Descriptions help the LLM understand what it’s working with.

## Step 9: Checking Your Work

Modules track their calls. See what happened:

``` r

classifier2$trace_summary()
```

This shows you how many calls were made and the token costs.

## What You Learned

In this tutorial, you:

1.  Declared a reusable signature and module
2.  Used [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
    for individual inputs
3.  Used
    [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
    for batch processing
4.  Processed data frames with
    [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
5.  Added input descriptions for clarity
6.  Checked your work with `trace_summary()`

## The Module Advantage

The same module contract scales from exploration to optimization:

1.  **Reusability**: Define once, use everywhere
2.  **Efficiency**: Batch processing reduces API calls
3.  **Configuration**: Change settings in one place
4.  **Optimization**: Modules can be improved with training data
    (covered in [Tutorial
    4](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md))
5.  **Tracing**: Track what happened for debugging

## Next Steps

Your classifier works, but can it handle more complex outputs? Continue
to:

- **[Tutorial 3: Extracting Structured
  Data](https://jameshwade.github.io/dsprrr/articles/tutorial-structured-outputs.md)**
  — Get multiple fields and nested structures
- **[Quick
  Reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.md)**
  — Module types and methods
- **[Understanding Signatures &
  Modules](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)**
  — Why S7 for signatures, R6 for modules
