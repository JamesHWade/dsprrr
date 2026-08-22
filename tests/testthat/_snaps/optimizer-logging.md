# invalid schema_version shapes retain the typed diagnostic

    Code
      validate_trial_record(record)
    Condition
      Error in `validate_trial_record()`:
      ! Trial record does not match the current schema
      i Its schema_version must be one non-missing scalar; this dsprrr reads only 1.
