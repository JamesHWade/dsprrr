# Tutorial 2: Build a classifier

You will build a sentiment classifier and run it three ways: on one
text, on a vector of texts, and on a data frame with
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).
It uses the signature syntax from [Tutorial
1](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md).

``` r

library(dsprrr)
library(ellmer)

chat <- chat_openai(model = "gpt-6-luna")
```

The answers on this page were recorded with `gpt-4.1` and are replayed
when the site is built, so `gpt-6-luna` may word them differently.

## Define the classifier

Start from the sentiment signature in Tutorial 1 and print the module:

``` r

classifier <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
)

classifier
#> 
#> ── PredictModule ──
#> 
#> ── Signature
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • text: "string" - Input: text
#> 
#> ── Output
#> Type: "object(sentiment: enum(positive, negative, neutral))"
#> 
#> ── Instructions
#> Given the fields `text`, produce the fields `sentiment`.
```

The printout shows the inputs, the output type and the instructions that
start every prompt. This signature has no instructions of its own, so
dsprrr wrote a default from the field names.

## Classify one text

``` r

run(classifier, text = "I absolutely loved this movie!", .llm = chat)
#> $sentiment
#> [1] "positive"
```

``` r

run(classifier, text = "This was a complete waste of time.", .llm = chat)
#> $sentiment
#> [1] "negative"

run(classifier, text = "It was okay, I guess.", .llm = chat)
#> $sentiment
#> [1] "neutral"

run(
  classifier,
  text = "The service was terrible but the food was amazing.",
  .llm = chat
)
#> $sentiment
#> [1] "neutral"
```

The mixed review came back `neutral`. Whether that is right depends on
what you need the labels for; instructions, below, let you decide.

## Classify a vector of texts

Pass a vector and
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) returns
a list with one result per element, in the same order. Each result is
the named list you saw above, so
[`purrr::map_chr()`](https://purrr.tidyverse.org/reference/map.html) can
pull out the labels:

``` r

reviews <- c(
  "Best purchase I've ever made!",
  "Broke after one day. Total garbage.",
  "Does what it says. Nothing special.",
  "Exceeded all my expectations!",
  "Would not recommend to anyone."
)

sentiments <- run(classifier, text = reviews, .llm = chat)
purrr::map_chr(sentiments, "sentiment")
#> [1] "positive" "negative" "neutral"  "positive" "negative"
```

Each element is still its own request to the model, and by default they
are sent one after another.
[`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md)
lets several run at once.

## Add instructions

Instructions replace the default line. Use them to settle cases the
labels leave open, such as mixed reviews:

``` r

sig <- signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the overall sentiment. If mixed, choose the dominant emotion."
)

classifier2 <- module(sig)
```

The new module takes the same inputs, one at a time or as a vector:

``` r

run(classifier2, text = "This is fantastic!", .llm = chat)
#> $sentiment
#> [1] "positive"
```

``` r

run(
  classifier2,
  text = c("Love it!", "Hate it!", "It's fine"),
  .llm = chat
)
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
```

This is the full return value of a vectorized call: a list of named
lists.

## Classify a data frame

[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
takes a data frame with a column for each input. It returns the data
frame with a `result` list-column added, and other columns, like `id`,
come through unchanged:

``` r

reviews_df <- tibble::tibble(
  id = 1:4,
  text = c(
    "Absolutely wonderful experience!",
    "Never buying from them again.",
    "Solid product, fair price.",
    "Changed my life for the better."
  )
)

results <- run_dataset(classifier2, reviews_df, .llm = chat)
results$sentiment <- purrr::map_chr(results$result, "sentiment")
results
#> # A tibble: 4 × 4
#>      id text                             result           sentiment
#>   <int> <chr>                            <list>           <chr>    
#> 1     1 Absolutely wonderful experience! <named list [1]> positive 
#> 2     2 Never buying from them again.    <named list [1]> negative 
#> 3     3 Solid product, fair price.       <named list [1]> positive 
#> 4     4 Changed my life for the better.  <named list [1]> positive
```

Like [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
sends one request per row. The module records every call, and
`summarize_traces(classifier2)` reports how many requests it made and
the tokens they used.

## Describe the inputs

A signature can also be built from
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md)
objects and an ellmer type. An input’s description becomes its heading
in the prompt, in place of the input’s name:

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
#> [1] "positive"
```

The output type here is a bare
[`type_enum()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
rather than a named field, so
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) returns
the label itself instead of a list.

Next, [Tutorial
3](https://jameshwade.github.io/dsprrr/articles/tutorial-structured-outputs.md)
returns several fields at once, including lists and nested records.
