#' Input coefficients
#'
#' @param data An `econ_io_table` object.
#' @param same_region A scalar logical. If `TRUE`, values between different
#' regions are set to zero.
#'
#' @return An `econ_io_table` object of input coefficients.
#'
#' @export
io_input_coef <- function(data,
                          same_region = FALSE) {
  input <- data |>
    dplyr::filter(io_sector_type(.data$input) == "industry",
                  io_sector_type(.data$output) == "industry")
  total_input <- io_total_input(data)
  input_coef <- dibble::broadcast(input / total_input,
                                  dim_names = c("input", "output"))
  if (same_region) {
    input_coef <- dibble::broadcast(input_coef * io_same_region(data),
                                    dim_names = dimnames(input_coef))
  }
  input_coef
}

#' Import coefficients
#'
#' @param data An `econ_io_table` object.
#' @param axis A scalar character. By default, `"input"`.
#'
#' @return An `econ_io_table` object of import coefficients.
#'
#' @export
io_import_coef <- function(data,
                           axis = c("input", "output")) {
  axis <- rlang::arg_match(axis, c("input", "output"))

  if (inherits(data, "io_table_noncompetitive_import")) {
    if (axis == "input") {
      data |>
        dplyr::filter(io_sector_type(.data$input) == "industry") |>
        dibble::apply("input", \(x) 0)
    } else if (axis == "output") {
      same_region <- io_same_region(data)

      import <- data |>
        dplyr::filter(io_sector_type(.data$input) == "import",
                      io_sector_type(.data$output) %in% c("industry", "final_demand")) |>
        dibble::apply("output", sum)

      regional_demand <- data |>
        dplyr::filter(io_sector_type(.data$input) == "industry",
                      io_sector_type(.data$output) %in% c("industry", "final_demand"))
      regional_demand_same_region <- dibble::broadcast(regional_demand * same_region,
                                                       dim_names = dimnames(regional_demand)) |>
        dibble::apply("output", sum)

      import / regional_demand_same_region
    }
  } else if (inherits(data, "io_table_competitive_import")) {
    if (axis == "input") {
      same_region <- io_same_region(data)

      import <- data |>
        dplyr::filter(io_sector_type(.data$input) == "industry",
                      io_sector_type(.data$output) == "import") |>
        dibble::apply("input", sum)

      regional_demand <- data |>
        dplyr::filter(io_sector_type(.data$input) == "industry",
                      io_sector_type(.data$output) %in% c("industry", "final_demand"))
      regional_demand_same_region <- dibble::broadcast(regional_demand * same_region,
                                                       dim_names = dimnames(regional_demand)) |>
        dibble::apply("input", sum)

      -import / regional_demand_same_region
    } else if (axis == "output") {
      data |>
        dplyr::filter(io_sector_type(.data$output) %in% c("industry", "final_demand")) |>
        dibble::apply("output", \(x) 0)
    }
  } else {
    cli::cli_abort("{.fn io_import_coef} is not implemented for {.cls {class(data)}}.")
  }
}
