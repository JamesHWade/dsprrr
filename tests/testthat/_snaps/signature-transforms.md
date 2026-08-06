# instruction transforms reject ambiguous inputs

    Code
      append_instructions(sig, NA_character_)
    Condition
      Error in `validate_signature_instructions()`:
      ! `instructions` must be one non-missing character string

---

    Code
      with_instructions(sig, c("one", "two"))
    Condition
      Error in `validate_signature_instructions()`:
      ! `instructions` must be one non-missing character string

---

    Code
      append_instructions(list(sig), "More detail.")
    Condition
      Error in `signature_transform_input()`:
      ! `x` must be a Signature or one signature string
      x Received <list>.
