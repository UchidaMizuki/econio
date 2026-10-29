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

