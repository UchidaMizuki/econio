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

  input <- data |>
    dplyr::filter(
      io_sector_type(.data$input) %in% input_sector_type,
      io_sector_type(.data$output) %in% output_sector_type
    )
  if (same_region) {
    input <- dibble::broadcast(
      input * io_same_region(data),
      dim_names = dimnames(input)
    )
  }
  input |>
    dibble::apply("output", sum)
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

  output <- data |>
    dplyr::filter(
      io_sector_type(.data$input) %in% input_sector_type,
      io_sector_type(.data$output) %in% output_sector_type
    )
  if (same_region) {
    output <- dibble::broadcast(
      output * io_same_region(data),
      dim_names = dimnames(output)
    )
  }
  output |>
    dibble::apply("input", sum)
}
