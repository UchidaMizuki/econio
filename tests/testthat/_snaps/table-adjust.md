# io_table_remove_reexport() warns and removes up to the exports when the excess exceeds them

    Code
      actual <- io_table_remove_reexport(iotable)
    Condition
      Warning:
      Imports exceed regional demand by more than exports, so some import coefficients stay above 1.
      x industry_1: excess 5, exports 2.
      i Only up to the exports are removed as re-exports.
      i This happens when output is negative, or when re-exports go to other regions within a multiregional table.

# io_table_remove_reexport() errors for noncompetitive import type tables

    Code
      io_table_remove_reexport(iotable)
    Condition
      Error in `io_table_remove_reexport()`:
      ! `data` must be a competitive import type input-output table.
      i Re-exports can only be identified when imports are recorded by product.

