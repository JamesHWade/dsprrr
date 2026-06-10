# Tutorial 3: Extracting Structured Data

In [Tutorial
2](https://jameshwade.github.io/dsprrr/articles/tutorial-build-classifier.md),
you built a classifier that returns a single value. But real extraction
tasks need multiple fields: names *and* dates, sentiment *and*
confidence, entities *and* relationships.

In this tutorial, you’ll extract complex, structured data from text.
We’re switching the running example from sentiment labels to news
articles and emails because extraction is where multi-field outputs
shine—but the workflow you learned in Tutorial 2 (signature → module →
run) stays exactly the same. Only the output type grows richer.

**Time**: 25-30 minutes

## What You’ll Build

An entity extractor that pulls structured information from news articles
and emails.

## Prerequisites

- Completed [Tutorial
  2](https://jameshwade.github.io/dsprrr/articles/tutorial-build-classifier.md)
- `OPENAI_API_KEY` set in your environment

``` r

library(dsprrr)
library(ellmer)
library(tibble)

chat <- chat_openai()
```

## Step 1: Multiple Output Fields

So far you’ve seen single outputs like `-> answer` or `-> sentiment`.
Add more outputs with commas:

``` r

sig <- signature("text -> sentiment, confidence: number")

extractor <- module(sig, type = "predict")

run(extractor, text = "This product is absolutely fantastic!", .llm = chat)
```

You get back both `sentiment` and `confidence` in a structured result.

Try another:

``` r

run(extractor, text = "It was okay, nothing special.", .llm = chat)
```

Notice the confidence is lower for ambiguous text.

## Step 2: Typed Multiple Outputs

Add types to each output field:

``` r

sig <- signature(
  "review -> sentiment: enum('positive', 'negative', 'neutral'), stars: int, summary: string"
)

analyzer <- module(sig, type = "predict")

result <- run(
  analyzer,
  review = "I've been using this blender for 6 months now. It's incredibly powerful and easy to clean. The only downside is it's quite loud. Overall, I'm very happy with it.",
  .llm = chat
)

result
```

You get sentiment, a star rating, *and* a summary—all typed correctly.

## Step 3: Complex Structures with `type_object()`

For nested or complex data, use ellmer’s type system directly:

``` r

sig <- signature(
  inputs = list(
    input("article", description = "News article to analyze")
  ),
  output_type = type_object(
    headline = type_string("A concise headline"),
    sentiment = type_enum(values = c("positive", "negative", "neutral")),
    word_count = type_integer()
  ),
  instructions = "Analyze the news article."
)

article_analyzer <- module(sig, type = "predict")
```

Test it with a news snippet:

``` r

article <- "
Scientists at MIT announced a breakthrough in solar panel efficiency today.
The new panels can convert 47% of sunlight to electricity, nearly double
the current commercial standard. The technology uses a novel layered
approach that captures more of the light spectrum. Researchers expect
commercial applications within 3-5 years.
"

run(article_analyzer, article = article, .llm = chat)
```

## Step 4: Arrays of Values

Extract lists of items with
[`type_array()`](https://ellmer.tidyverse.org/reference/type_boolean.html):

``` r

sig <- signature(
  inputs = list(
    input("text", description = "Text to extract entities from")
  ),
  output_type = type_object(
    people = type_array(type_string(), description = "Names of people mentioned"),
    organizations = type_array(type_string(), description = "Organizations mentioned"),
    locations = type_array(type_string(), description = "Places mentioned")
  ),
  instructions = "Extract named entities from the text."
)

entity_extractor <- module(sig, type = "predict")
```

Test with a news article:

``` r

news <- "
Apple CEO Tim Cook met with President Biden at the White House yesterday
to discuss manufacturing jobs. Cook announced that Apple will invest
$430 billion in the United States over the next five years, creating
20,000 new jobs. The meeting also included Treasury Secretary Janet Yellen
and Commerce Secretary Gina Raimondo.
"

result <- run(entity_extractor, text = news, .llm = chat)
result
```

Access the arrays directly:

``` r

result$people
result$organizations
```

## Step 5: Nested Objects

For hierarchical data, nest
[`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
calls:

``` r

sig <- signature(
  inputs = list(
    input("email", description = "Email message to parse")
  ),
  output_type = type_object(
    sender = type_object(
      name = type_string(),
      email = type_string()
    ),
    subject = type_string(),
    priority = type_enum(values = c("low", "normal", "high", "urgent")),
    action_items = type_array(type_string()),
    requires_response = type_boolean()
  ),
  instructions = "Parse the email and extract key information."
)

email_parser <- module(sig, type = "predict")
```

Test with an email:

``` r

email <- "
From: Sarah Johnson <sarah.johnson@techcorp.com>
Subject: Q4 Budget Review - Action Required

Hi team,

Please review the attached Q4 budget proposal by Friday. We need to:
1. Confirm department allocations
2. Identify any cost-saving opportunities
3. Submit final numbers to finance

This is time-sensitive as the board meeting is next Monday.

Thanks,
Sarah
"

result <- run(email_parser, email = email, .llm = chat)
result
```

Access nested fields:

``` r

result$sender$name
result$action_items
result$priority
```

## Step 6: Building an Email Triage System

Let’s combine what you’ve learned into a practical system:

``` r

sig <- signature(
  inputs = list(
    input("email", description = "Email to triage")
  ),
  output_type = type_object(
    category = type_enum(
      values = c("meeting", "task", "fyi", "urgent", "spam"),
      description = "Email category"
    ),
    summary = type_string("One-sentence summary"),
    action_required = type_boolean(),
    suggested_response = type_enum(
      values = c("reply_now", "reply_later", "forward", "archive", "delete"),
      description = "Recommended action"
    )
  ),
  instructions = "Triage the email for inbox management."
)

triage <- module(sig, type = "predict")
```

Process a batch of emails:

``` r

emails <- tibble(
  id = 1:3,
  email = c(
    "Meeting tomorrow at 3pm to discuss Q1 results. Please confirm attendance.",
    "FYI - The office will be closed on Monday for the holiday.",
    "URGENT: Server down! Need immediate assistance to restore services."
  )
)

results <- run_dataset(triage, emails, .llm = chat)
results
```

## Step 7: Handling Optional Fields

Some fields might not always be present. Make them nullable:

``` r

sig <- signature(
  inputs = list(
    input("text", description = "Text that may mention a date")
  ),
  output_type = type_object(
    has_date = type_boolean(),
    date = type_string("Date in YYYY-MM-DD format, or null if no date mentioned"),
    confidence = type_number()
  ),
  instructions = "Extract date information if present. Set date to empty string if no date."
)

date_extractor <- module(sig, type = "predict")

run(date_extractor, text = "Let's meet next Tuesday", .llm = chat)
run(date_extractor, text = "Great weather today!", .llm = chat)
```

## What You Learned

In this tutorial, you:

1.  Extracted multiple output fields with comma notation
2.  Added types to each field
3.  Built complex structures with
    [`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
4.  Extracted lists with
    [`type_array()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
5.  Created nested/hierarchical outputs
6.  Built a practical email triage system
7.  Handled optional fields

## The Structure Advantage

Structured extraction is powerful because:

- **Guaranteed types**: Numbers come back as numbers, not strings
- **Predictable shape**: Your downstream code knows exactly what to
  expect
- **Validation**: The LLM must conform to your schema
- **Composability**: Results plug directly into R data structures

## Next Steps

Your extractor works, but can it be improved? Continue to:

- **[Tutorial 4: Improving with
  Examples](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md)**
  — Add demonstrations to improve accuracy
- **[Quick
  Reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.md)**
  — All output types at a glance
- **[How Optimization
  Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md)**
  — Why examples improve LLM performance
