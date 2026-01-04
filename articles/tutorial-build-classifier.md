# Tutorial 2: Building a Reusable Classifier

In [Tutorial
1](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md),
you made one-off LLM calls with
[`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md). But
what if you need to classify hundreds of texts? Creating a new call each
time is tedious and slow.

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
#> 
#> Attaching package: 'dsprrr'
#> The following object is masked from 'package:methods':
#> 
#>     signature
library(ellmer)
```

## Step 1: The Problem with `dsp()`

In Tutorial 1, you classified sentiment like this:

``` r
chat <- chat_openai()
chat |> dsp("text -> sentiment: enum('positive', 'negative', 'neutral')", text = "Great!")
chat |> dsp("text -> sentiment: enum('positive', 'negative', 'neutral')", text = "Awful")
chat |> dsp("text -> sentiment: enum('positive', 'negative', 'neutral')", text = "Meh")
```

This works, but you’re repeating the signature every time. If you want
to change the signature, you have to change it everywhere.

## Step 2: Create a Reusable Module

The
[`as_module()`](https://jameshwade.github.io/dsprrr/reference/as_module.md)
function wraps a signature into a reusable object:

``` r
chat <- chat_openai()
#> Using model = "gpt-4.1".

classifier <- chat |>
  as_module("text -> sentiment: enum('positive', 'negative', 'neutral')")

classifier
#> 
#> ── PredictModule ──
#> 
#> ── Signature 
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • text: Input: text
#> 
#> ── Output 
#> Type: <ellmer::TypeObject>
#> 
#> ── Instructions 
#> Given the fields `text`, produce the fields `sentiment`.
```

Now `classifier` is an object you can use repeatedly.

## Step 3: Classify Single Texts

Use the `$predict()` method to classify:

``` r
classifier$predict(text = "I absolutely loved this movie!")
#> $sentiment
#> [1] "positive"
```

Try a few more:

``` r
classifier$predict(text = "This was a complete waste of time.")
#> $sentiment
#> [1] "negative"

classifier$predict(text = "It was okay, I guess.")
#> $sentiment
#> [1] "neutral"

classifier$predict(text = "The service was terrible but the food was amazing.")
#> $sentiment
#> [1] "neutral"
```

## Step 4: Batch Processing

Here’s where modules shine. Process multiple texts at once by passing a
vector:

``` r
reviews <- c(
  "Best purchase I've ever made!",
  "Broke after one day. Total garbage.",
  "Does what it says. Nothing special.",
  "Exceeded all my expectations!",
  "Would not recommend to anyone."
)

classifier$predict(text = reviews)
#> [[1]]
#> [[1]]$sentiment
#> [1] "positive"
#> 
#> 
#> [[2]]
#> [[2]]$sentiment
#> [1] "negative"
#> 
#> 
#> [[3]]
#> [[3]]$sentiment
#> [1] "neutral"
#> 
#> 
#> [[4]]
#> [[4]]$sentiment
#> [1] "positive"
#> 
#> 
#> [[5]]
#> [[5]]$sentiment
#> [1] "negative"
```

All five classifications came back in a single call. Much more efficient
than five separate calls.

## Step 5: The Full Control Approach

[`as_module()`](https://jameshwade.github.io/dsprrr/reference/as_module.md)
is convenient, but sometimes you need more control. The
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md) +
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
approach gives you that:

``` r
# Define the signature separately
sig <- signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the overall sentiment. If mixed, choose the dominant emotion."
)

sig
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • text: Input: text
#> 
#> ── Output
#> Type: <ellmer::TypeObject>
#> 
#> ── Instructions
#> Classify the overall sentiment. If mixed, choose the dominant emotion.
```

Now create a module from the signature:

``` r
classifier2 <- module(sig, type = "predict")

classifier2
#> 
#> ── PredictModule ──
#> 
#> ── Signature
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • text: Input: text
#> 
#> ── Output
#> Type: <ellmer::TypeObject>
#> 
#> ── Instructions
#> Classify the overall sentiment. If mixed, choose the dominant emotion.
```

## Step 6: Running with `run()`

With the full control approach, use
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) to
execute:

``` r
run(classifier2, text = "This is fantastic!", .llm = chat)
#> $sentiment
#> [1] "positive"
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
#> [[1]]
#> [1] "positive"
#> 
#> [[2]]
#> [1] "negative"
#> 
#> [[3]]
#> [1] "neutral"
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
#> # A tibble: 4 × 3
#>      id text                             result   
#>   <int> <chr>                            <list>   
#> 1     1 Absolutely wonderful experience! <chr [1]>
#> 2     2 Never buying from them again.    <chr [1]>
#> 3     3 Solid product, fair price.       <chr [1]>
#> 4     4 Changed my life for the better.  <chr [1]>
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

detailed_classifier <- module(sig, type = "predict")

run(
  detailed_classifier,
  review_text = "Five stars! Would buy again!",
  .llm = chat
)
#> [1] "positive"
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

1.  Created reusable modules with
    [`as_module()`](https://jameshwade.github.io/dsprrr/reference/as_module.md)
2.  Used `$predict()` for single and batch processing
3.  Built modules with full control using
    [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md) +
    [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
4.  Processed data frames with
    [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
5.  Added input descriptions for clarity
6.  Checked your work with `trace_summary()`

## When to Use Each Approach

| Approach                                                                                                                                            | Best For                                |
|-----------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------|
| [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md)                                                                                     | Quick one-off calls, exploration        |
| [`as_module()`](https://jameshwade.github.io/dsprrr/reference/as_module.md)                                                                         | Simple reusable modules, prototyping    |
| [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md) + [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) | Production code, optimization workflows |

## The Module Advantage

Why bother with modules when
[`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md) works?

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
