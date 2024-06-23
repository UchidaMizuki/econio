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
    input_coef <- io_input_coefficients(data)
    dibble::broadcast(solve(dibble::eye(input_coef) - input_coef),
                      dim_names = c("output", "input"))
  } else if (inherits(data, "io_table_competitive_import")) {
    if (endogenize_import) {
      input_coef <- io_input_coefficients(data)
      import_coef <- io_import_coefficients(data)
      input_coef_same_region <- io_input_coefficients(data,
                                                      same_region = TRUE)

      solve(dibble::eye(input_coef) - (input_coef - import_coef * input_coef_same_region)) |>
        dibble::broadcast(dim_names = c("output", "input"))
    } else {
      input_coef <- io_input_coefficients(data)
      dibble::broadcast(solve(dibble::eye(input_coef) - input_coef),
                        dim_names = c("output", "input"))
    }
  }
}

io_endogenize_import <- function(data, endogenize_import) {
  if (inherits(data, "io_table_noncompetitive_import")) {
    endogenize_import <- endogenize_import %||% TRUE
    if (!endogenize_import) {
      cli::cli_abort("{.code endogenize_import = FALSE} is not allowed for {.cls {class(data)}}.")
    }
  } else if (inherits(data, "io_table_competitive_import")) {
    if (is.null(endogenize_import)) {
      endogenize_import <- TRUE
      cli::cli_inform("Assuming {.code endogenize_import = TRUE}.")
    }
  }

  if (!rlang::is_scalar_logical(endogenize_import)) {
    cli::cli_abort('{.code endogenize_import} must be a scalar logical.')
  }
  endogenize_import
}
