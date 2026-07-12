# configure_cache validates its privacy mode

    Code
      configure_cache(disk_private = NA)
    Condition
      Error in `configure_cache()`:
      ! `disk_private` must be a single non-missing logical value

---

    Code
      configure_cache(disk_private = "yes")
    Condition
      Error in `configure_cache()`:
      ! `disk_private` must be a single non-missing logical value
