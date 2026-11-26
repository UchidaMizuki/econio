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

# io_regional_demand() validates exclude_negative

    Code
      io_regional_demand(get_iotable_negative_demand(), exclude_negative = "yes")
    Condition
      Error in `io_regional_demand()`:
      ! `exclude_negative` must be `TRUE` or `FALSE`, not the string "yes".

