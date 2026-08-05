# GEPA validates properties

    Code
      GEPA(component_selector = "random")
    Condition
      Error:
      ! <dsprrr::GEPA> object properties are invalid:
      - @component_selector component_selector must be 'round_robin', 'all', or a function

---

    Code
      GEPA(max_merge_invocations = -1L)
    Condition
      Error:
      ! <dsprrr::GEPA> object properties are invalid:
      - @max_merge_invocations max_merge_invocations must be a non-negative integer or NULL
