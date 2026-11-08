# io_table_remove_reexport() errors when re-exports exceed exports

    Code
      io_table_remove_reexport(iotable)
    Condition
      Error in `io_table_remove_reexport()`:
      ! Re-exports must not exceed exports.
      x industry_1: imports exceed regional demand by 5, but exports are 2.
      i This happens when output is negative, or when re-exports go to other regions, which can't be removed without changing their demand.

# io_table_remove_reexport() errors for noncompetitive import type tables

    Code
      io_table_remove_reexport(iotable)
    Condition
      Error in `io_table_remove_reexport()`:
      ! `data` must be a competitive import type input-output table.
      i Re-exports can only be identified when imports are recorded by product.

