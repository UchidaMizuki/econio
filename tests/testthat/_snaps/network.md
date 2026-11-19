# io_network() aborts on negative transactions

    Code
      io_network(iotable_negative)
    Condition
      Error:
      ! Can't build the industry network from negative domestic intermediate transactions.
      x Found 1 negative transaction.

# io_network_all() aborts when the sums over paths diverge

    Code
      io_network_all(weight, c(1, 1), tibble::tibble(sector = 1:2))
    Condition
      Error:
      ! The sums of transactions over all paths do not converge.
      x The spectral radius of the domestic input coefficient matrix is 2, not less than 1.
      i It is less than 1 when every industry has positive value added plus imports.

