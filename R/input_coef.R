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
