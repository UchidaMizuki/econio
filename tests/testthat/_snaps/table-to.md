# io_table_to_*_import() abort when data_import totals do not match

    Code
      io_table_to_noncompetitive_import(iotable, data_import = import * 2)
    Condition
      Error in `io_table_to_noncompetitive_import()`:
      ! The total import values do not match.
      x region_1/industry_1: 20 (`actual`) not nearly equal to 10 (`expected`).
      x region_1/industry_2: 18 (`actual`) not nearly equal to 9 (`expected`).
      x region_1/industry_3: 48 (`actual`) not nearly equal to 24 (`expected`).
      x region_2/industry_1: 72 (`actual`) not nearly equal to 36 (`expected`).
      x region_2/industry_2: 30 (`actual`) not nearly equal to 15 (`expected`).
      x region_2/industry_3: 60 (`actual`) not nearly equal to 30 (`expected`).
      x region_3/industry_1: 84 (`actual`) not nearly equal to 42 (`expected`).
      x region_3/industry_2: 42 (`actual`) not nearly equal to 21 (`expected`).
      x region_3/industry_3: 72 (`actual`) not nearly equal to 36 (`expected`).

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

