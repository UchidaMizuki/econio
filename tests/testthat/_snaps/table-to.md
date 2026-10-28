# io_table_to_*() accept data_import for multiregional tables

    Code
      io_table_to(iotable, data_import = data_import * 2)
    Condition
      Error in `io_check_import_totals()`:
      ! The total import values do not match.
      x region_1/industry_1: 60 (`actual`) not nearly equal to 30 (`expected`).
      x region_1/industry_2: 18 (`actual`) not nearly equal to 9 (`expected`).
      x region_1/industry_3: 48 (`actual`) not nearly equal to 24 (`expected`).
      x region_1/final_demand_1: 66 (`actual`) not nearly equal to 33 (`expected`).
      x region_1/final_demand_2: 24 (`actual`) not nearly equal to 12 (`expected`).
      x region_2/industry_1: 72 (`actual`) not nearly equal to 36 (`expected`).
      x region_2/industry_2: 30 (`actual`) not nearly equal to 15 (`expected`).
      x region_2/industry_3: 60 (`actual`) not nearly equal to 30 (`expected`).
      x region_2/final_demand_1: 78 (`actual`) not nearly equal to 39 (`expected`).
      x region_2/final_demand_2: 36 (`actual`) not nearly equal to 18 (`expected`).
      x region_3/industry_1: 84 (`actual`) not nearly equal to 42 (`expected`).
      x region_3/industry_2: 42 (`actual`) not nearly equal to 21 (`expected`).
      x region_3/industry_3: 72 (`actual`) not nearly equal to 36 (`expected`).
      x region_3/final_demand_1: 90 (`actual`) not nearly equal to 45 (`expected`).
      x region_3/final_demand_2: 48 (`actual`) not nearly equal to 24 (`expected`).

---

    Code
      io_table_to(iotable, data_import = data_import * 2)
    Condition
      Error in `io_check_import_totals()`:
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

# io_table_to_regional() errors for regional tables

    Code
      io_table_to_regional(iotable)
    Condition
      Error in `io_table_to_regional()`:
      ! `data` must be a multi-regional input-output table created with `io_table_multiregional()`.

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

