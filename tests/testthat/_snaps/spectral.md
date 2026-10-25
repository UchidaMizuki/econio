# io_spectral_embedding aborts when normalizing an industry with no transactions

    Code
      io_spectral_embedding(iotable)
    Condition
      Error in `io_spectral_embedding()`:
      ! The normalized embedding requires every industry to have domestic intermediate transactions.
      x 1 industry has none: "industry_1".
      i Remove these industries or use `normalized = FALSE`.

# io_spectral_embedding validates normalized

    Code
      io_spectral_embedding(iotable, normalized = "yes")
    Condition
      Error in `io_spectral_embedding()`:
      ! `normalized` must be `TRUE` or `FALSE`, not the string "yes".

# io_spectral_embedding validates dims

    Code
      io_spectral_embedding(iotable, dims = 0)
    Condition
      Error in `io_spectral_embedding()`:
      ! `dims` must be a whole number between 1 and 2, not the number 0.

