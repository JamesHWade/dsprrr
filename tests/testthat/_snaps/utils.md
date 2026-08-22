# dataset APIs reject removed arguments before empty returns

    Code
      run_dataset(mod, empty, .parallel = TRUE)
    Condition
      Error in `validate_reserved_input_names()`:
      ! Unknown dot-prefixed argument: `.parallel`
      x These are not treated as signature fields.
      i Use `.concurrency` with `concurrency_control()`.

---

    Code
      evaluate(mod, empty, metric = function(...) 1, .parallel_method = "mirai")
    Condition
      Error in `validate_reserved_input_names()`:
      ! Unknown dot-prefixed argument: `.parallel_method`
      x These are not treated as signature fields.
      i Use `.concurrency` with `concurrency_control()`.
