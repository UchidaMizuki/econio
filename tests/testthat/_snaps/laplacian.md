# io_laplacian() informs only when converting implicitly

    Code
      laplacian <- io_laplacian(iotable_noncompetitive)

---

    Code
      laplacian <- io_laplacian(iotable_competitive)
    Message
      Converting to a noncompetitive import type table with `io_table_to_noncompetitive_import()`.

# io_laplacian() zeros and informs about negative transactions

    Code
      laplacian <- io_laplacian(iotable_negative)
    Message
      Zeroing 1 negative domestic intermediate transaction (e.g. from byproduct treatment such as the Stone method).

