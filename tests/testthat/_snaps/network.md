# io_network() informs only when converting implicitly

    Code
      network <- io_network(iotable_noncompetitive)

---

    Code
      network <- io_network(iotable_competitive)
    Message
      Converting to a noncompetitive import type table with `io_table_to_noncompetitive_import()`.

# io_network() aborts on negative transactions

    Code
      io_network(iotable_negative)
    Condition
      Error in `io_network()`:
      ! Found 1 negative domestic intermediate transaction (e.g. from byproduct treatment such as the Stone method).
      i Use `io_table_to_byproduct_transfer()` to convert negative entries to the transfer method before building the industry network.

# io_network_all() aborts when the sums over paths diverge

    Code
      io_network_all(weight, c(1, 1), tibble::tibble(sector = 1:2))
    Condition
      Error:
      ! The sums of transactions over all paths do not converge.
      x The spectral radius of the domestic input coefficient matrix is 2, not less than 1.
      i It is less than 1 when every industry has positive value added plus imports.

