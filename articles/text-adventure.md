# Build a text adventure

This project builds a text adventure that runs in the R console. A
language model invents the places and narrates what happens; R keeps the
map, the inventory and the rules. By the end you will have a working
game loop, a skill check whose strictness you set in R, and a way to
test both the referee and the narrator. It is adapted from the [DSPy
text adventure tutorial](https://dspy.ai/tutorials/ai_text_game/).

You could write the game with ellmer alone, since
`chat$chat_structured()` already returns typed data. dsprrr earns its
place in three spots:

| The game needs | With ellmer alone | With dsprrr |
|----|----|----|
| New rooms as data | Paste a prompt together and pass a [`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html) | A signature bundles inputs, instructions and output type; [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md), [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md) and [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md) all take it |
| A skill check | Ask for `TRUE` or `FALSE`; to make checks harder, rewrite the prompt | [`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md) asks for a probability and applies a threshold you set in R |
| A fair referee | Play and judge by feel | Score it with [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md); fit the threshold with [`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md) |

``` r

library(dsprrr)
library(ellmer)
```

## Rooms as data

The game starts somewhere and never needs a map drawn in advance.
Whenever the player reaches a new place, a module describes it:

``` r

setting <- "a storm-bound lighthouse island whose keeper vanished last winter"

describe_room <- module(signature(
  inputs = list(
    input("setting", description = "The world of the game"),
    input("place", description = "The place the player has just reached"),
    input("came_from", description = "The place the player came from")
  ),
  output_type = type_object(
    description = type_string("Two or three sentences, in the second person"),
    exits = type_array(type_string(), "Two to four nearby places, as short names"),
    items = type_array(type_string(), "Up to three portable objects, lower case")
  ),
  instructions = "Narrate a text adventure. Invent the place to fit the setting."
))

describe_room
#> 
#> ── PredictModule ──
#> 
#> ── Signature
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • setting: "string" - The world of the game
#> • place: "string" - The place the player has just reached
#> • came_from: "string" - The place the player came from
#> 
#> ── Output
#> Type: "object(description: string, exits: array(string), items: array(string))"
#> 
#> ── Instructions
#> Narrate a text adventure. Invent the place to fit the setting.
```

`exits` and `items` come back as character vectors, so moving and
picking things up are ordinary R operations. That also lets R hold the
model to what it described: the player can only take an item the room
contains.

## A skill check with a threshold

When the player tries something risky, a referee decides whether it
works. A plain
[`type_boolean()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
output would make the model answer yes or no, and the only way to make
it stricter would be to reword the prompt.
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md)
changes the question: the model reports the probability that the action
succeeds, and dsprrr compares it with a threshold that stays in R (see
[Calibrated
decisions](https://jameshwade.github.io/dsprrr/articles/calibrated-decisions.md)).

``` r

skill_check <- module(signature(
  inputs = list(
    input("action", description = "What the player tries to do"),
    input("scene", description = "Where the player is and what is there"),
    input("inventory", description = "What the player carries")
  ),
  output_type = type_object(
    success = type_boolean("Does the action succeed?")
  ),
  instructions = "Referee a text adventure. The right item makes an action
easier; reckless actions usually fail."
)) |>
  with_decisions(success = decision_bool())

decision_settings(skill_check)
#> # A tibble: 1 × 5
#>   field   kind  threshold cuts   weights
#>   <chr>   <chr>     <dbl> <list> <list> 
#> 1 success bool        0.5 <NULL> <NULL>
```

The threshold is never sent to the model. A stricter referee is the same
module with a higher threshold, and for actions it has already judged it
re-reads the cached probability instead of calling the model again:

``` r

strict_check <- skill_check |>
  with_decisions(success = decision_bool(threshold = 0.7))
```

## The narrator

The verdict comes first and the story second. If one call did both, the
model could pick whichever outcome made the better story. The narrator
is told the verdict and reports what changed hands:

``` r

narrate <- module(signature(
  inputs = list(
    input("action", description = "What the player tried"),
    input("succeeded", "boolean", description = "The referee's verdict"),
    input("scene", description = "Where the player is and what is there"),
    input("inventory", description = "What the player carries")
  ),
  output_type = type_object(
    story = type_string("Two or three sentences on what happens"),
    gained = type_array(type_string(), "Items from the scene the player now holds"),
    lost = type_array(type_string(), "Items the player no longer holds")
  ),
  instructions = "Narrate the result of the player's action. Follow the verdict."
))
```

## The game loop

`play()` keeps the state in plain R objects. `go` and `take` are handled
in R without a model call; any other command goes to the referee and
then the narrator.

``` r

match_one <- function(text, options) {
  hits <- options[grepl(tolower(text), tolower(options), fixed = TRUE)]
  if (nzchar(text) && length(hits) > 0) hits[[1]] else NA_character_
}

play <- function(llm = chat_openai(model = "gpt-6-luna")) {
  if (!interactive()) stop("play() needs an interactive R session.")
  # A fresh copy of the chat per call: the model sees only what is passed in.
  ask <- function(mod, ...) run(mod, ..., .llm = llm$clone())
  rooms <- list()
  place <- "the jetty"
  came_from <- "the supply boat"
  inventory <- "lantern"

  repeat {
    if (is.null(rooms[[place]])) {
      room <- ask(
        describe_room,
        setting = setting, place = place, came_from = came_from
      )
      room$exits <- union(setdiff(room$exits, place), came_from)
      rooms[[place]] <- room
    }
    here <- rooms[[place]]
    cat("\n", toupper(place), "\n", here$description, "\n", sep = "")
    cat("Exits:", toString(here$exits), "\n")
    if (length(here$items) > 0) cat("You see:", toString(here$items), "\n")

    cmd <- trimws(readline("> "))
    if (cmd == "quit") break
    if (!nzchar(cmd)) next
    if (startsWith(cmd, "go ")) {
      to <- match_one(substring(cmd, 4), here$exits)
      if (is.na(to)) {
        cat("You can't go that way.\n")
      } else {
        came_from <- place
        place <- to
      }
      next
    }
    if (startsWith(cmd, "take ")) {
      item <- match_one(substring(cmd, 6), here$items)
      if (!is.na(item)) {
        inventory <- c(inventory, item)
        rooms[[place]]$items <- setdiff(here$items, item)
        next
      }
    }

    scene <- paste0(
      place, ". ", here$description, " Items: ", toString(here$items)
    )
    verdict <- ask(
      skill_check,
      action = cmd, scene = scene, inventory = toString(inventory),
      .return_format = "structured"
    )
    ok <- verdict$output$success
    odds <- decision_evidence(verdict)$probability
    cat(sprintf(
      "[referee: %.0f%% likely, %s]\n",
      100 * odds, if (ok) "success" else "failure"
    ))

    result <- ask(
      narrate,
      action = cmd, succeeded = ok, scene = scene, inventory = toString(inventory)
    )
    cat(result$story, "\n")
    # Only items that are really here can be gained
    gained <- intersect(result$gained, here$items)
    rooms[[place]]$items <- setdiff(here$items, gained)
    inventory <- union(setdiff(inventory, result$lost), gained)
  }
  invisible(list(place = place, inventory = inventory, rooms = rooms))
}
```

Start a game with:

``` r

play()
```

Each call gets its own copy of the chat, so the model knows only what
the state tells it. The game’s memory lives in `rooms`: go back to a
place and you get the description it had the first time.

dsprrr caches responses, so the same action in the same situation gets
the same verdict. A lock that resisted you stays shut until something
changes, such as what you carry. To let players try again, add
`.cache = FALSE` to the
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) call in
`ask()`. When the narrator says something strange,
[`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md)
shows exactly what it was sent.

## Test the referee

The referee is a classifier, so you can grade it. Write down rulings you
consider fair, score the module, and let
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md)
fit the threshold against them. It runs the module over the rulings
once, tries thresholds against the recorded probabilities without
calling the model again, and keeps a new threshold only if it scores
better on held-out folds. The prompt does not change.

``` r

rulings <- tibble::tribble(
  ~action,                 ~scene,                       ~inventory, ~success,
  "unlock the sea chest",  "a locked sea chest",         "brass key", TRUE,
  "unlock the sea chest",  "a locked sea chest",         "lantern",   FALSE,
  "leap across the gap",   "a thirty-foot gap",          "lantern",   FALSE,
  "climb down to a ledge", "a thirty-foot drop",         "rope",      TRUE,
  "light the great lamp",  "a dry lamp and an oil can",  "matches",   TRUE,
  "sing to calm the gulls", "screaming gulls overhead",  "lantern",   FALSE
)

llm <- chat_openai(model = "gpt-6-luna")
referee_metric <- metric_exact_match(field = "success")

evaluate(skill_check, rulings, metric = referee_metric, .llm = llm)$mean_score

fair_check <- compile(
  skill_check,
  ReAnchor(metric = referee_metric, fields = "success"),
  rulings,
  .llm = llm
)
decision_settings(fair_check)
```

Six rulings show the idea; a threshold you rely on needs many more.

## Grade the narrator

The narrator needs no reference answers to be graded. One rule it should
never break: it can only hand the player items that are in the scene.
The game already drops anything else, but then the story and the state
disagree. A metric counts how often that happens:

``` r

keeps_items_honest <- function(prediction, expected) {
  here <- strsplit(expected$items_here, ", ")[[1]]
  all(prediction$gained %in% here)
}

keeps_items_honest(
  list(gained = c("brass key", "golden crown")),
  tibble::tibble(items_here = "brass key, coil of rope")
)
#> [1] FALSE
```

[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
passes each whole row to the metric as `expected`, so the metric can
read `items_here` even though the narrator never sees that column:

``` r

situations <- tibble::tribble(
  ~action,              ~succeeded, ~scene,                ~inventory, ~items_here,
  "search the desk",    TRUE,       "a desk, a brass key", "lantern",  "brass key",
  "pry open the crate", FALSE,      "a crate, some rope",  "lantern",  "rope"
)

evaluate(narrate, situations, metric = keeps_items_honest, .llm = llm)$mean_score
```

With a score, changing the narrator’s instructions stops being a matter
of taste: edit them, rerun
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
and keep what scores better. [Metrics and
evaluation](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md)
covers how to design metrics like these.
