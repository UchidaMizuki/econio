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
  io_total(
    data,
    keep_axis = "input",
    input_sector_type = input_sector_type,
    output_sector_type = output_sector_type,
    same_region = same_region
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
