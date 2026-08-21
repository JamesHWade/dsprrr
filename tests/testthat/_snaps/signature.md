# Signature rejects missing instructions

    Code
      Signature(inputs = list(input(name = "text", type = "string")), output_type = ellmer::type_string(),
      instructions = NA_character_)
    Condition
      Error:
      ! <dsprrr::Signature> object properties are invalid:
      - @instructions instructions must be a single non-missing character string
