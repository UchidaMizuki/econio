# io_spectral_embedding returns NA for an industry with no links to others

    Code
      embedding <- io_spectral_embedding(iotable_isolated)
    Message
      Returning `NA` for 1 industry without domestic intermediate transactions with other industries: "industry_1".

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

