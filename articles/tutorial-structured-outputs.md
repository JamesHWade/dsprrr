# Tutorial 3: Extract structured data

The classifier in [Tutorial
2](https://jameshwade.github.io/dsprrr/articles/tutorial-build-classifier.md)
returns one label per text. Extraction needs more: a headline and a
sentiment, the people named in an article, the sender and action items
of an email. By the end of this tutorial you can declare outputs like
these and get them back as R lists and tibbles with a fixed shape.

``` r

library(dsprrr)
library(ellmer)
library(tibble)

chat <- chat_openai(model = "gpt-6-luna")
```

The answers on this page were recorded with `gpt-4.1` and are replayed
when the site is built, so `gpt-6-luna` may word them differently.

## Return several fields

Separate outputs with commas. An output without a type, like `sentiment`
here, is a string:

``` r

sig <- signature("text -> sentiment, confidence: number")

extractor <- module(sig)

run(extractor, text = "This product is absolutely fantastic!", .llm = chat)
#> $sentiment
#> [1] "positive"
#> 
#> $confidence
#> [1] 0.98
```

``` r

run(extractor, text = "It was okay, nothing special.", .llm = chat)
#> $sentiment
#> [1] "neutral"
#> 
#> $confidence
#> [1] 0.85
```

`confidence` is the model’s own estimate, not a calibrated probability,
so treat it as a rough signal.

## Give each field a type

The types from Tutorial 1 work on every field:

``` r

sig <- signature(
  "review -> sentiment: enum('positive', 'negative', 'neutral'), stars: int, summary: string"
)

analyzer <- module(sig)

result <- run(
  analyzer,
  review = "I've been using this blender for 6 months now. It's incredibly powerful and easy to clean. The only downside is it's quite loud. Overall, I'm very happy with it.",
  .llm = chat
)

str(result)
#> List of 3
#>  $ sentiment: chr "positive"
#>  $ stars    : int 4
#>  $ summary  : chr "Powerful and easy-to-clean blender, but a bit loud."
```

[`str()`](https://rdrr.io/r/utils/str.html) shows the R types: `stars`
is an integer and the other two fields are character strings.

## Describe fields with `type_object()`

The string notation cannot describe an output. For that, and for nested
records, build the output from ellmer types.
[`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
takes one argument per field:

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

article_analyzer <- module(sig)
```

``` r

article <- "
Scientists at MIT announced a breakthrough in solar panel efficiency today.
The new panels can convert 47% of sunlight to electricity, nearly double
the current commercial standard. The technology uses a novel layered
approach that captures more of the light spectrum. Researchers expect
commercial applications within 3-5 years.
"

analysis <- run(article_analyzer, article = article, .llm = chat)
analysis
#> $headline
#> [1] "MIT Scientists Achieve Breakthrough in Solar Panel Efficiency"
#> 
#> $sentiment
#> [1] "positive"
#> 
#> $word_count
#> [1] 54
```

The headline and sentiment are reasonable. The word count is not;
compare it with a count done in R:

``` r

c(
  model = analysis$word_count,
  actual = lengths(strsplit(trimws(article), "\\s+"))
)
#>  model actual 
#>     54     48
```

Language models estimate counts rather than compute them. Compute
anything countable in R, and ask the model for judgment calls like the
headline.

## Extract lists with `type_array()`

[`type_array()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
holds any number of values of one type. An array of strings arrives in R
as a character vector:

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

entity_extractor <- module(sig)
```

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
#> $people
#> [1] "Tim Cook"        "President Biden" "Janet Yellen"    "Gina Raimondo"  
#> 
#> $organizations
#> [1] "Apple"       "White House" "Treasury"    "Commerce"   
#> 
#> $locations
#> [1] "United States"
```

The model decides what counts as a person or an organization. If you
want names without titles, say so in the field’s description.

## Nest objects

A field can itself be an object. The result is a nested list:

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

email_parser <- module(sig)
```

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
str(result)
#> List of 5
#>  $ sender           :List of 2
#>   ..$ name : chr "Sarah Johnson"
#>   ..$ email: chr "sarah.johnson@techcorp.com"
#>  $ subject          : chr "Q4 Budget Review - Action Required"
#>  $ priority         : chr "urgent"
#>  $ action_items     : chr [1:4] "Review the attached Q4 budget proposal by Friday" "Confirm department allocations" "Identify any cost-saving opportunities" "Submit final numbers to finance"
#>  $ requires_response: logi TRUE
```

Reach nested fields with `$`:

``` r

result$sender$name
#> [1] "Sarah Johnson"
```

## Triage a batch of emails

The same approach works on a data frame. This signature triages emails:

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

triage <- module(sig)
```

As in Tutorial 2,
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
returns a `result` list-column of named lists. Turn each record into a
one-row tibble and bind them to get one row per email and a typed column
per field:

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

results$result |>
  purrr::map(as_tibble) |>
  purrr::list_rbind()
#> # A tibble: 3 × 4
#>   category summary                            action_required suggested_response
#>   <chr>    <chr>                              <lgl>           <chr>             
#> 1 meeting  Meeting scheduled for tomorrow at… TRUE            reply_now         
#> 2 fyi      Office closure on Monday due to t… FALSE           archive           
#> 3 urgent   Server is down and immediate assi… TRUE            reply_now
```

## Allow missing values

Some fields are not always there. Mark them with `required = FALSE`: the
schema then allows the model to return null for that field, and a null
arrives in R as `NULL`.

``` r

sig <- signature(
  inputs = list(
    input("text", description = "Text that may mention an event")
  ),
  output_type = type_object(
    event = type_string("What is planned"),
    date = type_string("Date in YYYY-MM-DD format", required = FALSE)
  ),
  instructions = "Extract the event and its date. Use null if no date is given."
)

event_extractor <- module(sig)

run(event_extractor, text = "The contract renews on 1 March 2027.", .llm = chat)

coffee <- run(event_extractor, text = "We should get coffee sometime.", .llm = chat)
is.null(coffee$date)
```

When the model leaves the date out, `coffee$date` is `NULL`, so test for
it with [`is.null()`](https://rdrr.io/r/base/NULL.html) before using it.

Next, [Tutorial
4](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md)
measures how often a module is right and improves it by adding worked
examples to the prompt.
