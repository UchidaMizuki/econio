# open_economy defaults to TRUE for competitive import tables

    Code
      input_coef <- io_input_coef(iotable)
    Message
      Assuming `open_economy = TRUE`.

---

    Code
      output_coef <- io_output_coef(iotable)
    Message
      Assuming `open_economy = TRUE`.

# io_input_coef() and io_output_coef() handle invalid open_economy

    Code
      io_input_coef(iotable_noncomp, open_economy = TRUE)
    Condition
      Error in `io_input_coef()`:
      ! `open_economy = NULL` is required for <io_table_noncompetitive_import/io_table_regional/econ_io_table/ddf_col>.

---

    Code
      io_output_coef(iotable_noncomp, open_economy = FALSE)
    Condition
      Error in `io_output_coef()`:
      ! `open_economy = NULL` is required for <io_table_noncompetitive_import/io_table_regional/econ_io_table/ddf_col>.

---

    Code
      io_input_coef(iotable_comp, open_economy = "yes")
    Condition
      Error in `io_input_coef()`:
      ! `open_economy` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      io_output_coef(iotable_comp, open_economy = c(TRUE, FALSE))
    Condition
      Error in `io_output_coef()`:
      ! `open_economy` must be `TRUE` or `FALSE`, not a logical vector.

