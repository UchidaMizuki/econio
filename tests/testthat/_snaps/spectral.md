# io_spectral_embedding aborts on a disconnected network

    Code
      io_spectral_embedding(iotable_isolated)
    Condition
      Error in `io_spectral_embedding()`:
      ! The domestic production network must be connected.
      x 1 industry has no domestic intermediate transactions: "industry_1".
      i Results for separate parts of the network are not comparable.
      i Remove or merge industries that are not linked to the others.

---

    Code
      io_spectral_embedding(iotable_self_loop)
    Condition
      Error in `io_spectral_embedding()`:
      ! The domestic production network must be connected.
      i Results for separate parts of the network are not comparable.
      i Remove or merge industries that are not linked to the others.

---

    Code
      io_spectral_embedding(iotable_self_loop, normalized = FALSE)
    Condition
      Error in `io_spectral_embedding()`:
      ! The domestic production network must be connected.
      i Results for separate parts of the network are not comparable.
      i Remove or merge industries that are not linked to the others.

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

