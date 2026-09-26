# Tutorial 4: Improve with examples

A module can return the right shape and still pick the wrong answer. By
the end of this tutorial you can measure how often a support-ticket
classifier is right, add worked examples (demos) to its prompt by hand
or with `LabeledFewShot`, and check with
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
whether they helped.

``` r

library(dsprrr)
library(ellmer)

chat <- chat_openai(model = "gpt-6-luna")
```

The answers on this page were recorded with `gpt-5-mini` and are
replayed when the site is built, so `gpt-6-luna` may word them
differently.

## A ticket classifier

The classifier sorts support tickets into four categories:

``` r

sig <- signature(
  "ticket -> category: enum('billing', 'technical', 'shipping', 'general')",
  instructions = "Classify the customer support ticket."
)

classifier <- module(sig)
```

``` r

run(classifier, ticket = "My package hasn't arrived yet", .llm = chat)
#> $category
#> [1] "shipping"

run(classifier, ticket = "I was charged twice for my order", .llm = chat)
#> $category
#> [1] "billing"

run(classifier, ticket = "The app keeps crashing when I try to login", .llm = chat)
#> $category
#> [1] "technical"
```

Three right answers on tickets you picked yourself say little about
accuracy. For that you need tickets with known answers.

## Measure a baseline

Label some tickets with the category you expect. Treat them as a test
set: they are for scoring the classifier, not for showing to it.

``` r

tickets <- tibble::tribble(
  ~ticket,                                  ~category,
  "I was charged twice for the same item",  "billing",
  "How do I update my payment method?",     "billing",
  "The website won't load on my phone",     "technical",
  "My password reset email never arrived",  "technical",
  "When will my order ship?",               "shipping",
  "Can I change my delivery address?",      "shipping",
  "The product arrived damaged",            "shipping",
  "I need a refund for my subscription",    "billing",
  "How do I contact customer service?",     "general",
  "What are your business hours?",          "general"
)
```

Run the classifier on every ticket and compare its answers with the
labels.
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
returns your columns plus a `result` column holding the module’s answer
for each row. Take the predictions from `result`: the `category` column
in the output is your label, copied from the input.

``` r

baseline_results <- run_dataset(classifier, tickets, .llm = chat)

predicted <- vapply(baseline_results$result, \(r) r$category, character(1))
mean(predicted == tickets$category)
#> [1] 1

# The tickets it got wrong
tickets[predicted != tickets$category, ]
#> # A tibble: 0 × 2
#> # ℹ 2 variables: ticket <chr>, category <chr>
```

In the recorded run, gpt-5-mini labels all ten tickets correctly, so the
list of misses is empty. That is the check to make before adding demos.
Demos lengthen every prompt, and they can only help on tickets the model
gets wrong, so with this baseline there is nothing for them to fix. When
your baseline does miss tickets, read the misses: they tell you which
kinds of ticket need an example.

The rest of this tutorial adds demos anyway, to show how, and measures
them on the same ten tickets.

## Add demos by hand

A demo is a worked example: an input and the output you want for it.
Pass a list of them to
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md):

``` r

classifier_with_demos <- module(
  sig,
  demos = list(
    list(
      inputs = list(ticket = "I was charged twice for the same item"),
      output = list(category = "billing")
    ),
    list(
      inputs = list(ticket = "The app crashes on startup"),
      output = list(category = "technical")
    ),
    list(
      inputs = list(ticket = "My package is late"),
      output = list(category = "shipping")
    )
  )
)
```

``` r

run(classifier_with_demos, ticket = "I need a receipt for my purchase", .llm = chat)
#> $category
#> [1] "billing"

run(classifier_with_demos, ticket = "The button doesn't respond when clicked", .llm = chat)
#> $category
#> [1] "technical"
```

dsprrr puts the demos in the prompt, ahead of the new ticket. This is
the prompt for the second call:

``` r

cat(get_last_prompt()$prompt)
#> Classify the customer support ticket.
#> 
#> Example 1:
#> ticket: I was charged twice for the same item
#> Output: {"category":"billing"}
#> 
#> Example 2:
#> ticket: The app crashes on startup
#> Output: {"category":"technical"}
#> 
#> Example 3:
#> ticket: My package is late
#> Output: {"category":"shipping"}
#> 
#> 
#> # Input: ticket
#> ticket: The button doesn't respond when clicked
```

## Sample demos with `LabeledFewShot`

`LabeledFewShot` builds demos from the rows of a labeled data frame
instead. dsprrr calls objects like this teleprompters:
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
applies one to a module and returns a new module, leaving the original
unchanged. Here the rows come from `tickets`, which is a shortcut with a
cost, explained below.

``` r

compiled <- classifier |> compile(LabeledFewShot(k = 3L), tickets)

best_demos(compiled, as_tibble = TRUE)
#> # A tibble: 3 × 2
#>   ticket                             output   
#>   <chr>                              <chr>    
#> 1 The website won't load on my phone technical
#> 2 What are your business hours?      general  
#> 3 How do I update my payment method? billing
```

`LabeledFewShot` does not look for good examples. It samples `k` rows at
random, with a fixed seed (`seed = 123L` by default) so that you get the
same rows each time. This sample has a technical, a general and a
billing ticket, and no shipping ticket, so check what it picked before
you rely on it.

## Compare the versions

[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
runs a module on every row of a data frame and scores each answer with a
metric. `metric_exact_match(field = "category")` scores 1 when the
returned `category` equals the row’s `category` column and 0 otherwise,
so the mean score is accuracy:

``` r

metric <- metric_exact_match(field = "category")

versions <- list(
  "no demos" = classifier,
  "hand-written demos" = classifier_with_demos,
  "LabeledFewShot demos" = compiled
)

vapply(
  versions,
  \(m) evaluate(m, tickets, metric = metric, .llm = chat)$mean_score,
  numeric(1)
)
#>             no demos   hand-written demos LabeledFewShot demos 
#>                    1                    1                    1
```

All three versions score 1. On this task, the recorded model does as
well without demos as with them.

The comparison also favors the demo versions, because some of the scored
tickets sit in their prompts with the answers attached: the first
hand-written demo is ticket 1 word for word, and `LabeledFewShot` drew
tickets 2, 3 and 10. On your own data, draw demos from a training set
and score every version on tickets outside it. Make that test set large
enough to trust, too: with ten tickets, one answer moves accuracy by 10
points.

Every demo adds tokens to every call. If you try several values of `k`,
keep the smallest one that scores as well as the larger ones.

Next, [Tutorial
5](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md)
splits the data into training and test sets and lets dsprrr search over
instructions and demos.
