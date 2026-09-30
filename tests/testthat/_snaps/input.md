# input() rejects legacy and unknown type specifications

    Code
      input("text", "str")
    Condition
      Error in `normalize_input_type()`:
      ! Unsupported `type` for `input()`
      i Use an ellmer type or exactly one of: "string", "number", "integer", "boolean", "array", and "object".

---

    Code
      input("text", " String ")
    Condition
      Error in `normalize_input_type()`:
      ! Unsupported `type` for `input()`
      i Use an ellmer type or exactly one of: "string", "number", "integer", "boolean", "array", and "object".

---

    Code
      input("text", S7::class_character)
    Condition
      Error in `normalize_input_type()`:
      ! Unsupported `type` for `input()`
      i Use an ellmer type or exactly one of: "string", "number", "integer", "boolean", "array", and "object".

---

    Code
      input("text", class = S7::class_character)
    Condition
      Error in `input()`:
      ! `class` is not a supported input specification
      i Pass an ellmer type or a canonical type label via `type`: "string", "number", "integer", "boolean", "array", or "object".
