# io_reclass() names the values whose weights don't sum to 1

    Code
      io_reclass(iotable, input_sector_data = input_sector_data)
    Condition
      Error in `io_reclass()`:
      ! The weights in `input_sector_data` must sum to 1 for each `from` value.
      x industry_2: 1.1.

# io_reclass() validates its scalar arguments

    Code
      io_reclass(iotable, weight_tolerance = "x")
    Condition
      Error in `io_reclass()`:
      ! `weight_tolerance` must be a number, not the string "x".

---

    Code
      io_reclass(iotable, check_axes = "yes")
    Condition
      Error in `io_reclass()`:
      ! `check_axes` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      io_reclass(iotable, from_col = 1)
    Condition
      Error in `io_reclass()`:
      ! `from_col` must be a single string, not the number 1.

