# io_laplacian() informs only when converting implicitly

    Code
      laplacian <- io_laplacian(iotable_noncompetitive)

---

    Code
      laplacian <- io_laplacian(iotable_competitive)
    Message
      Converting to a noncompetitive import type table with `io_table_to_noncompetitive_import()`.

# io_laplacian() aborts on negative transactions

    Code
      io_laplacian(iotable_negative)
    Condition
      Error in `io_laplacian()`:
      ! Found 1 negative domestic intermediate transaction (e.g. from byproduct treatment such as the Stone method).
      i Use `io_table_to_byproduct_transfer()` to convert negative entries to the transfer method before building the industry network.

