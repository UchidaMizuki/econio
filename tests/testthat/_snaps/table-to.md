# io_table_to_*() validate their scalar arguments

    Code
      io_table_to_competitive_import(iotable, import_sector_name = 5)
    Condition
      Error in `io_table_to_competitive_import()`:
      ! `import_sector_name` must be a single string or `NA`, not the number 5.

---

    Code
      io_table_to_noncompetitive_import(iotable, import_total_tolerance = "x")
    Condition
      Error in `io_table_to_noncompetitive_import()`:
      ! `import_total_tolerance` must be a number, not the string "x".

