# executable Flex requires a top-level forward function

    Code
      dsprrr:::flex_validate_code_source("answer <- 42")
    Condition
      Error in `dsprrr:::flex_validate_code_source()`:
      ! Executable Flex source must define `forward <- function(...) ...`
      i The complete source is evaluated only inside the configured interpreter.
