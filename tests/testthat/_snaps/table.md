# io_table_regional() and io_table_multiregional() work

    Code
      io_table(iotable_dummy[[name]], import_type = import_type_flipped)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `input_sector = sector_function(input_sector_type, input_sector_name, import_type = import_type)`.
      Caused by error in `sector_function()`:
      ! `"import"` is not allowed in input sector types when `import_type = "competitive_import"`.

---

    Code
      io_table(iotable_wrong_total_input, import_type = import_type)
    Condition
      Error in `io_check_totals()`:
      ! The total input values do not match.
      x industry_1: 27 (`actual`) not nearly equal to 27.33333 (`expected`).
      x industry_2: 29 (`actual`) not nearly equal to 29.33333 (`expected`).
      x industry_3: 31 (`actual`) not nearly equal to 31.33333 (`expected`).

---

    Code
      io_table(iotable_wrong_total_output, import_type = import_type)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_1: 27 (`actual`) not nearly equal to 27.14286 (`expected`).
      x industry_2: 29 (`actual`) not nearly equal to 29.14286 (`expected`).
      x industry_3: 31 (`actual`) not nearly equal to 31.14286 (`expected`).

---

    Code
      io_table(iotable_dummy[[name]], import_type = import_type_flipped)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `output_sector = sector_function(output_sector_type, output_sector_name, import_type = import_type)`.
      Caused by error in `sector_function()`:
      ! `"import"` is not allowed in output sector types when `import_type = "noncompetitive_import"`.

---

    Code
      io_table(iotable_wrong_total_input, import_type = import_type)
    Condition
      Error in `io_check_totals()`:
      ! The total input values do not match.
      x industry_1: 19 (`actual`) not nearly equal to 19.33333 (`expected`).
      x industry_2: 28 (`actual`) not nearly equal to 28.33333 (`expected`).
      x industry_3: 25 (`actual`) not nearly equal to 25.33333 (`expected`).

---

    Code
      io_table(iotable_wrong_total_output, import_type = import_type)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_1: 19 (`actual`) not nearly equal to 19.14286 (`expected`).
      x industry_2: 28 (`actual`) not nearly equal to 28.14286 (`expected`).
      x industry_3: 25 (`actual`) not nearly equal to 25.14286 (`expected`).

---

    Code
      io_table(iotable_dummy[[name]], import_type = import_type_flipped)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `input_sector = sector_function(input_sector_type, input_sector_name, import_type = import_type)`.
      Caused by error in `sector_function()`:
      ! `"import"` is not allowed in input sector types when `import_type = "competitive_import"`.

---

    Code
      io_table(iotable_wrong_total_input, import_type = import_type)
    Condition
      Error in `io_check_totals()`:
      ! The total input values do not match.
      x region_1/industry_1: 117 (`actual`) not nearly equal to 117.3333 (`expected`).
      x region_1/industry_2: 123 (`actual`) not nearly equal to 123.3333 (`expected`).
      x region_1/industry_3: 129 (`actual`) not nearly equal to 129.3333 (`expected`).
      x region_2/industry_1: 153 (`actual`) not nearly equal to 153.3333 (`expected`).
      x region_2/industry_2: 159 (`actual`) not nearly equal to 159.3333 (`expected`).
      x region_2/industry_3: 165 (`actual`) not nearly equal to 165.3333 (`expected`).
      x region_3/industry_1: 189 (`actual`) not nearly equal to 189.3333 (`expected`).
      x region_3/industry_2: 195 (`actual`) not nearly equal to 195.3333 (`expected`).
      x region_3/industry_3: 201 (`actual`) not nearly equal to 201.3333 (`expected`).

---

    Code
      io_table(iotable_wrong_total_output, import_type = import_type)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x region_1/industry_1: 117 (`actual`) not nearly equal to 117.1429 (`expected`).
      x region_1/industry_2: 123 (`actual`) not nearly equal to 123.1429 (`expected`).
      x region_1/industry_3: 129 (`actual`) not nearly equal to 129.1429 (`expected`).
      x region_2/industry_1: 153 (`actual`) not nearly equal to 153.1429 (`expected`).
      x region_2/industry_2: 159 (`actual`) not nearly equal to 159.1429 (`expected`).
      x region_2/industry_3: 165 (`actual`) not nearly equal to 165.1429 (`expected`).
      x region_3/industry_1: 189 (`actual`) not nearly equal to 189.1429 (`expected`).
      x region_3/industry_2: 195 (`actual`) not nearly equal to 195.1429 (`expected`).
      x region_3/industry_3: 201 (`actual`) not nearly equal to 201.1429 (`expected`).

---

    Code
      io_table(iotable_dummy[[name]], import_type = import_type_flipped)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `output_sector = sector_function(output_sector_type, output_sector_name, import_type = import_type)`.
      Caused by error in `sector_function()`:
      ! `"import"` is not allowed in output sector types when `import_type = "noncompetitive_import"`.

---

    Code
      io_table(iotable_wrong_total_input, import_type = import_type)
    Condition
      Error in `io_check_totals()`:
      ! The total input values do not match.
      x region_1/industry_1: 107 (`actual`) not nearly equal to 107.3333 (`expected`).
      x region_1/industry_2: 114 (`actual`) not nearly equal to 114.3333 (`expected`).
      x region_1/industry_3: 105 (`actual`) not nearly equal to 105.3333 (`expected`).
      x region_2/industry_1: 117 (`actual`) not nearly equal to 117.3333 (`expected`).
      x region_2/industry_2: 144 (`actual`) not nearly equal to 144.3333 (`expected`).
      x region_2/industry_3: 135 (`actual`) not nearly equal to 135.3333 (`expected`).
      x region_3/industry_1: 147 (`actual`) not nearly equal to 147.3333 (`expected`).
      x region_3/industry_2: 174 (`actual`) not nearly equal to 174.3333 (`expected`).
      x region_3/industry_3: 165 (`actual`) not nearly equal to 165.3333 (`expected`).

---

    Code
      io_table(iotable_wrong_total_output, import_type = import_type)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x region_1/industry_1: 107 (`actual`) not nearly equal to 107.1429 (`expected`).
      x region_1/industry_2: 114 (`actual`) not nearly equal to 114.1429 (`expected`).
      x region_1/industry_3: 105 (`actual`) not nearly equal to 105.1429 (`expected`).
      x region_2/industry_1: 117 (`actual`) not nearly equal to 117.1429 (`expected`).
      x region_2/industry_2: 144 (`actual`) not nearly equal to 144.1429 (`expected`).
      x region_2/industry_3: 135 (`actual`) not nearly equal to 135.1429 (`expected`).
      x region_3/industry_1: 147 (`actual`) not nearly equal to 147.1429 (`expected`).
      x region_3/industry_2: 174 (`actual`) not nearly equal to 174.1429 (`expected`).
      x region_3/industry_3: 165 (`actual`) not nearly equal to 165.1429 (`expected`).

# io_table_regional() validates its scalar arguments

    Code
      io_table_regional(data, import_type = "competitive_import", check_axes = "yes")
    Condition
      Error in `io_table_regional()`:
      ! `check_axes` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      io_table_regional(data, import_type = "competitive_import", total_tolerance = -
        1)
    Condition
      Error in `io_table_regional()`:
      ! `total_tolerance` must be a number larger than or equal to 0, not the number -1.

---

    Code
      io_table_regional(data, import_type = "yes")
    Condition
      Error in `io_table_regional()`:
      ! `import_type` must be one of "competitive_import" or "noncompetitive_import", not "yes".

# io_table_regional() checks totals relative to total output

    Code
      io_table_regional(add_zero_output_industry(data, 0.001), import_type = "competitive_import")
    Condition
      Error in `io_check_totals()`:
      ! The total input values do not match.
      x industry_4: 0.001 (`actual`) not nearly equal to 0 (`expected`).

# io_table_regional() checks the other totals when a total is missing

    Code
      io_table_regional(data, import_type = "competitive_import")
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_2: 28 (`actual`) not nearly equal to 28.28 (`expected`).

# io_check_totals() validates total_tolerance

    Code
      io_check_totals(iotable, total_tolerance = "x")
    Condition
      Error in `io_check_totals()`:
      ! `total_tolerance` must be a number, not the string "x".

# io_check_axes() detects axis mismatches

    Code
      io_check_axes(iotable_mismatch)
    Condition
      Error in `io_check_axes()`:
      ! The input and output axes do not match.
      x Input axis mismatch: industry_1

# io_check_axes() reports regions of mismatched multiregional axes

    Code
      io_check_axes(iotable_mismatch)
    Condition
      Error in `io_check_axes()`:
      ! The input and output axes do not match.
      x Input axis mismatch: region_2/industry_1

