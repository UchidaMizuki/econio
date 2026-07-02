# io_total_input() and io_total_output() validate same_region

    Code
      io_total_input(iotable, same_region = NA)
    Condition
      Error in `io_total_input()`:
      ! `same_region` must be `TRUE` or `FALSE`, not `NA`.

---

    Code
      io_total_output(iotable, same_region = "x")
    Condition
      Error in `io_total_output()`:
      ! `same_region` must be `TRUE` or `FALSE`, not the string "x".

