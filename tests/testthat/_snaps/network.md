# io_network() aborts on negative transactions

    Code
      io_network(iotable_negative)
    Condition
      Error:
      ! Can't build the industry network from negative domestic intermediate transactions.
      x industry_1 to industry_1: -99.
      i 1 is already negative in `data`, e.g. byproducts recorded as negative inputs (the Stone method).
      i `io_table_transfer_byproduct()` moves negative intermediate transactions to the reverse-direction cell.

---

    Code
      io_network(iotable_negative)
    Condition
      Error:
      ! Can't build the industry network from negative domestic intermediate transactions.
      x industry_1 to industry_1: -1.
      x industry_1 to industry_2: -1.
      x industry_1 to industry_3: -1.
      x industry_2 to industry_1: -1.
      x industry_2 to industry_2: -1.
      x ... and 4 more.
      i 9 are already negative in `data`, e.g. byproducts recorded as negative inputs (the Stone method).
      i `io_table_transfer_byproduct()` moves negative intermediate transactions to the reverse-direction cell.

# io_network() aborts on negatives from the import split

    Code
      io_network(iotable)
    Condition
      Warning:
      Some import coefficients are above 1 or negative, so some domestic or imported transactions become negative.
      * industry_1: import coefficient 1.142857 (imports 40, regional demand 35).
      Error:
      ! Can't build the industry network from negative domestic intermediate transactions.
      x industry_1 to industry_1: -1.428571.
      x industry_1 to industry_2: -0.7142857.
      i 2 became negative when `io_table_to_noncompetitive_import()` split imports by import coefficients above 1.

# io_network_all() aborts when the sums over paths diverge

    Code
      io_network_all(weight, c(1, 1), tibble::tibble(sector = 1:2))
    Condition
      Error:
      ! The sums of transactions over all paths do not converge.
      x The spectral radius of the domestic input coefficient matrix is 2, not less than 1.
      i It is less than 1 when every industry has positive value added plus imports.

