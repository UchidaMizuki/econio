#' Get total input
#'
#' @param data An `econ_io_table` object.
#' @param input_sector_type A character vector of input sector types.
#' @param output_sector_type A character vector of output sector types.
#' @param same_region A logical scalar. If `TRUE`, values between different
#' regions are ignored.
#'
#' @return An `econ_io_table` object of total input.
#'
#' @export
io_total_input <- function(
  data,
  input_sector_type = c("industry", "import", "value_added"),
  output_sector_type = "industry",
  same_region = FALSE
) {
  rlang::check_bool(same_region)

  io_total(
    data,
    keep_axis = "output",
    input_sector_type = input_sector_type,
    output_sector_type = output_sector_type,
    same_region = same_region
  )
}

#' Get total output
#'
#' @param data An `econ_io_table` object.
#' @param input_sector_type A character vector of input sector types.
#' @param output_sector_type A character vector of output sector types.
#' @param same_region A logical scalar. If `TRUE`, values between different
#' regions are ignored.
#'
#' @return An `econ_io_table` object of total output.
#'
#' @export
io_total_output <- function(
  data,
  input_sector_type = "industry",
  output_sector_type = c("industry", "final_demand", "export", "import"),
  same_region = FALSE
) {
  rlang::check_bool(same_region)

  io_total(
    data,
    keep_axis = "input",
    input_sector_type = input_sector_type,
    output_sector_type = output_sector_type,
    same_region = same_region
  )
}

#' Get regional demand
#'
#' Get the intermediate and final demand within the same region, i.e. the
#' transactions from industries to industries and final demand sectors of the
#' same region, transaction by transaction. Their sums by input sector are the
#' regional demand that import coefficients divide imports by (see
#' [io_import_coef()]). For a noncompetitive import type table, these are
#' domestic transactions only.
#'
#' @param data An `econ_io_table` object.
#' @param exclude_negative A logical scalar. If `TRUE`, negative transactions,
#'   e.g. byproducts recorded as negative inputs (the Stone method) or decreases
#'   in inventories, are set to 0, i.e. treated as domestic rather than as uses
#'   of the product, so that they get no share of the imports. By default,
#'   `FALSE`, so that the sums by input sector are those of
#'   `io_total_output(data, output_sector_type = c("industry", "final_demand"),
#'   same_region = TRUE)`.
#'
#' @return An `econ_io_table` object of regional demand, with industries on the
#' input axis and industries and final demand sectors on the output axis.
#' Transactions between different regions are 0.
#'
#' @export
io_regional_demand <- function(data, exclude_negative = FALSE) {
  rlang::check_bool(exclude_negative)

  demand <- data |>
    dplyr::filter(
      io_sector_type(.data$input) == "industry",
      io_sector_type(.data$output) %in% c("industry", "final_demand")
    )
  if (exclude_negative) {
    demand <- dibble::pmax(demand, 0)
  }
  dibble::broadcast(
    demand * io_same_region(demand),
    dim_names = c("input", "output")
  )
}

# The commodity balance of each industry of a competitive import type table,
# `total_output = total_regional_demand + total_export - total_import`, where
# `total_export` includes the shipments to the other regions of a
# multiregional table. With `exclude_negative = TRUE`, the negative
# transactions left out of `total_regional_demand` break the identity.
io_commodity_balance <- function(data, exclude_negative = FALSE) {
  demand <- c("industry", "final_demand")
  dibble::dibble(
    total_output = io_total_output(data),
    total_regional_demand = io_regional_demand(data, exclude_negative) |>
      dibble::apply("input", sum),
    total_export = io_total_output(
      data,
      output_sector_type = c(demand, "export")
    ) -
      io_total_output(data, output_sector_type = demand, same_region = TRUE),
    total_import = -io_total_output(data, output_sector_type = "import")
  )
}

io_total <- function(
  data,
  keep_axis,
  input_sector_type,
  output_sector_type,
  same_region
) {
  input_sector_type <- rlang::arg_match(
    input_sector_type,
    c("industry", "import", "value_added"),
    multiple = TRUE
  )
  output_sector_type <- rlang::arg_match(
    output_sector_type,
    c("industry", "final_demand", "export", "import"),
    multiple = TRUE
  )

  total <- data |>
    dplyr::filter(
      io_sector_type(.data$input) %in% input_sector_type,
      io_sector_type(.data$output) %in% output_sector_type
    )
  if (same_region) {
    total <- dibble::broadcast(
      total * io_same_region(data),
      dim_names = dimnames(total)
    )
  }
  total |>
    dibble::apply(keep_axis, sum)
}
