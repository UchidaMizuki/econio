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

