#' Leontief inverse matrix
#'
#' @param data An `econ_io_table` object.
#' @param endogenize_import A scalar logical. If `TRUE`, imports are
#' endogenized.
#'
#' @return An `econ_io_table` object of the Leontief inverse matrix.
#'
#' @export
io_leontief_inverse <- function(data,
                                endogenize_import = NULL) {
  endogenize_import <- io_endogenize_import(data, endogenize_import)

  if (inherits(data, "io_table_noncompetitive_import")) {
    input_coef <- io_input_coef(data)
    dibble::broadcast(solve(dibble::eye(input_coef) - input_coef),
                      dim_names = c("output", "input"))
  } else if (inherits(data, "io_table_competitive_import")) {
    if (endogenize_import) {
      input_coef <- io_input_coef(data)
      import_coef <- io_import_coef(data)
      input_coef_same_region <- io_input_coef(data,
                                              same_region = TRUE)

      solve(dibble::eye(input_coef) - (input_coef - import_coef * input_coef_same_region)) |>
        dibble::broadcast(dim_names = c("output", "input"))
    } else {
      input_coef <- io_input_coef(data)
      dibble::broadcast(solve(dibble::eye(input_coef) - input_coef),
                        dim_names = c("output", "input"))
    }
  }
}
