# metric_with_trace validates its contract

    Code
      metric_with_trace("not a function")
    Condition
      Error in `metric_with_trace()`:
      ! `fn` must be a function

---

    Code
      metric_with_trace(function(prediction, expected) 1)
    Condition
      Error in `metric_with_trace()`:
      ! `fn` must accept a third `program_trace` argument
      i Use `function(prediction, expected, program_trace) ...`.

---

    Code
      metric_with_trace(function(prediction, expected, program_trace) 1, field = c(
        "answer", "other"))
    Condition
      Error in `metric_with_trace()`:
      ! `field` must be a single character string or NULL
