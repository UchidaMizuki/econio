# io_table_regional() and io_table_multiregional() work

    Code
      io_table(iotable_dummy[[name]], competitive_import = !competitive_import)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `input_sector = sector_function(input_sector_type, input_sector_name, competitive_import = competitive_import)`.
      Caused by error in `sector_function()`:
      ! `"import"` is not allowed in input sector types when `competitive_import = TRUE`.

---

    Code
      io_table(iotable_wrong_total_input, competitive_import = competitive_import)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_1: 27 (`actual`) not nearly equal to 27.3333333333333 (`expected`).
      x industry_2: 29 (`actual`) not nearly equal to 29.3333333333333 (`expected`).
      x industry_3: 31 (`actual`) not nearly equal to 31.3333333333333 (`expected`).

---

    Code
      io_table(iotable_wrong_total_output, competitive_import = competitive_import)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_1: 27 (`actual`) not nearly equal to 27.1428571428571 (`expected`).
      x industry_2: 29 (`actual`) not nearly equal to 29.1428571428571 (`expected`).
      x industry_3: 31 (`actual`) not nearly equal to 31.1428571428571 (`expected`).

---

    Code
      io_table(iotable_dummy[[name]], competitive_import = !competitive_import)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `output_sector = sector_function(output_sector_type, output_sector_name, competitive_import = competitive_import)`.
      Caused by error in `sector_function()`:
      ! `"import"` is not allowed in output sector types when `competitive_import = FALSE`.

---

    Code
      io_table(iotable_wrong_total_input, competitive_import = competitive_import)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_1: 19 (`actual`) not nearly equal to 19.3333333333333 (`expected`).
      x industry_2: 28 (`actual`) not nearly equal to 28.3333333333333 (`expected`).
      x industry_3: 25 (`actual`) not nearly equal to 25.3333333333333 (`expected`).

---

    Code
      io_table(iotable_wrong_total_output, competitive_import = competitive_import)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_1: 19 (`actual`) not nearly equal to 19.1428571428571 (`expected`).
      x industry_2: 28 (`actual`) not nearly equal to 28.1428571428571 (`expected`).
      x industry_3: 25 (`actual`) not nearly equal to 25.1428571428571 (`expected`).

---

    Code
      io_table(iotable_dummy[[name]], competitive_import = !competitive_import)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `input_sector = sector_function(input_sector_type, input_sector_name, competitive_import = competitive_import)`.
      Caused by error in `sector_function()`:
      ! `"import"` is not allowed in input sector types when `competitive_import = TRUE`.

---

    Code
      io_table(iotable_wrong_total_input, competitive_import = competitive_import)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_1: 117 (`actual`) not nearly equal to 117.333333333333 (`expected`).
      x industry_2: 123 (`actual`) not nearly equal to 123.333333333333 (`expected`).
      x industry_3: 129 (`actual`) not nearly equal to 129.333333333333 (`expected`).
      x industry_1: 153 (`actual`) not nearly equal to 153.333333333333 (`expected`).
      x industry_2: 159 (`actual`) not nearly equal to 159.333333333333 (`expected`).
      x industry_3: 165 (`actual`) not nearly equal to 165.333333333333 (`expected`).
      x industry_1: 189 (`actual`) not nearly equal to 189.333333333333 (`expected`).
      x industry_2: 195 (`actual`) not nearly equal to 195.333333333333 (`expected`).
      x industry_3: 201 (`actual`) not nearly equal to 201.333333333333 (`expected`).

---

    Code
      io_table(iotable_wrong_total_output, competitive_import = competitive_import)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_1: 117 (`actual`) not nearly equal to 117.142857142857 (`expected`).
      x industry_2: 123 (`actual`) not nearly equal to 123.142857142857 (`expected`).
      x industry_3: 129 (`actual`) not nearly equal to 129.142857142857 (`expected`).
      x industry_1: 153 (`actual`) not nearly equal to 153.142857142857 (`expected`).
      x industry_2: 159 (`actual`) not nearly equal to 159.142857142857 (`expected`).
      x industry_3: 165 (`actual`) not nearly equal to 165.142857142857 (`expected`).
      x industry_1: 189 (`actual`) not nearly equal to 189.142857142857 (`expected`).
      x industry_2: 195 (`actual`) not nearly equal to 195.142857142857 (`expected`).
      x industry_3: 201 (`actual`) not nearly equal to 201.142857142857 (`expected`).

---

    Code
      io_table(iotable_dummy[[name]], competitive_import = !competitive_import)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `output_sector = sector_function(output_sector_type, output_sector_name, competitive_import = competitive_import)`.
      Caused by error in `sector_function()`:
      ! `"import"` is not allowed in output sector types when `competitive_import = FALSE`.

---

    Code
      io_table(iotable_wrong_total_input, competitive_import = competitive_import)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_1: 87 (`actual`) not nearly equal to 87.3333333333333 (`expected`).
      x industry_2: 114 (`actual`) not nearly equal to 114.333333333333 (`expected`).
      x industry_3: 105 (`actual`) not nearly equal to 105.333333333333 (`expected`).
      x industry_1: 117 (`actual`) not nearly equal to 117.333333333333 (`expected`).
      x industry_2: 144 (`actual`) not nearly equal to 144.333333333333 (`expected`).
      x industry_3: 135 (`actual`) not nearly equal to 135.333333333333 (`expected`).
      x industry_1: 147 (`actual`) not nearly equal to 147.333333333333 (`expected`).
      x industry_2: 174 (`actual`) not nearly equal to 174.333333333333 (`expected`).
      x industry_3: 165 (`actual`) not nearly equal to 165.333333333333 (`expected`).

---

    Code
      io_table(iotable_wrong_total_output, competitive_import = competitive_import)
    Condition
      Error in `io_check_totals()`:
      ! The total output values do not match.
      x industry_1: 87 (`actual`) not nearly equal to 87.1428571428571 (`expected`).
      x industry_2: 114 (`actual`) not nearly equal to 114.142857142857 (`expected`).
      x industry_3: 105 (`actual`) not nearly equal to 105.142857142857 (`expected`).
      x industry_1: 117 (`actual`) not nearly equal to 117.142857142857 (`expected`).
      x industry_2: 144 (`actual`) not nearly equal to 144.142857142857 (`expected`).
      x industry_3: 135 (`actual`) not nearly equal to 135.142857142857 (`expected`).
      x industry_1: 147 (`actual`) not nearly equal to 147.142857142857 (`expected`).
      x industry_2: 174 (`actual`) not nearly equal to 174.142857142857 (`expected`).
      x industry_3: 165 (`actual`) not nearly equal to 165.142857142857 (`expected`).

# io_check_axes() detects axis mismatches

    Code
      io_check_axes(iotable_mismatch)
    Condition
      Error in `io_check_axes()`:
      ! The input and output axes do not match.
      x Input axis mismatch: industry_1
      x Output axis mismatch: character(0)

