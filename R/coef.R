#' Input coefficients
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical. If `TRUE`, open economy assumptions are
#' used.
#'
#' @return An `econ_io_table` object of input coefficients.
#'
#' @export
io_input_coef <- function(data, open_economy = NULL) {
  open_economy <- io_open_economy(data, open_economy)

  input <- data |>
    dplyr::filter(
      io_sector_type(.data$input) == "industry",
      io_sector_type(.data$output) == "industry"
    )
  total_input <- io_total_input(data)
  input_coef <- dibble::broadcast(
    safe_divide(input, total_input),
    dim_names = c("input", "output")
  )

  if (isTRUE(open_economy)) {
    import_coef <- io_import_coef(data)
    input_coef_same_region <- dibble::broadcast(
      input_coef * io_same_region(data),
      dim_names = dimnames(input_coef)
    )
    dibble::broadcast(
      input_coef - import_coef * input_coef_same_region,
      dim_names = c("input", "output")
    )
  } else {
    input_coef
  }
}

#' Output coefficients
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical. If `TRUE`, open economy assumptions are
#' used.
#'
#' @return An `econ_io_table` object of output coefficients.
#'
#' @export
io_output_coef <- function(data, open_economy = NULL) {
  open_economy <- io_open_economy(data, open_economy)

  output <- data |>
    dplyr::filter(
      io_sector_type(.data$input) == "industry",
      io_sector_type(.data$output) == "industry"
    )
  if (isTRUE(open_economy)) {
    total_output <- io_total_output(
      data,
      output_sector_type = c("industry", "final_demand"),
      same_region = TRUE
    )
  } else {
    total_output <- io_total_output(data)
  }

  dibble::broadcast(
    safe_divide(output, total_output),
    dim_names = c("input", "output")
  )
}

io_open_economy <- function(data, open_economy) {
  if (inherits(data, "io_table_noncompetitive_import")) {
    if (!is.null(open_economy)) {
      cli::cli_abort(
        "{.code open_economy = NULL} is required for {.cls {class(data)}}."
      )
    }
  } else if (inherits(data, "io_table_competitive_import")) {
    if (is.null(open_economy)) {
      open_economy <- FALSE
      cli::cli_inform(
        "Assuming {.code open_economy = {open_economy}}."
      )
    } else if (!rlang::is_scalar_logical(open_economy)) {
      cli::cli_abort('{.code open_economy} must be a scalar logical.')
    }
  }
  open_economy
}

#' Import coefficients
#'
#' @param data An `econ_io_table` object.
#' @param axis A scalar character. By default, `"input"`.
#'
#' @return An `econ_io_table` object of import coefficients.
#'
#' @export
io_import_coef <- function(data, axis = c("input", "output")) {
  axis <- rlang::arg_match(axis, c("input", "output"))

  if (inherits(data, "io_table_noncompetitive_import")) {
    if (axis == "input") {
      data |>
        dplyr::filter(io_sector_type(.data$input) == "industry") |>
        dibble::apply("input", \(x) 0)
    } else if (axis == "output") {
      same_region <- io_same_region(data)

      import <- data |>
        dplyr::filter(
          io_sector_type(.data$input) == "import",
          io_sector_type(.data$output) %in% c("industry", "final_demand")
        ) |>
        dibble::apply("output", sum)

      regional_demand <- data |>
        dplyr::filter(
          io_sector_type(.data$input) == "industry",
          io_sector_type(.data$output) %in% c("industry", "final_demand")
        )
      regional_demand_same_region <- dibble::broadcast(
        regional_demand * same_region,
        dim_names = dimnames(regional_demand)
      ) |>
        dibble::apply("output", sum)

      safe_divide(import, regional_demand_same_region)
    }
  } else if (inherits(data, "io_table_competitive_import")) {
    if (axis == "input") {
      same_region <- io_same_region(data)

      import <- data |>
        dplyr::filter(
          io_sector_type(.data$input) == "industry",
          io_sector_type(.data$output) == "import"
        ) |>
        dibble::apply("input", sum)

      regional_demand <- data |>
        dplyr::filter(
          io_sector_type(.data$input) == "industry",
          io_sector_type(.data$output) %in% c("industry", "final_demand")
        )
      regional_demand_same_region <- dibble::broadcast(
        regional_demand * same_region,
        dim_names = dimnames(regional_demand)
      ) |>
        dibble::apply("input", sum)

      -safe_divide(import, regional_demand_same_region)
    } else if (axis == "output") {
      data |>
        dplyr::filter(
          io_sector_type(.data$output) %in% c("industry", "final_demand")
        ) |>
        dibble::apply("output", \(x) 0)
    }
  } else {
    cli::cli_abort(
      "{.fn io_import_coef} is not implemented for {.cls {class(data)}}."
    )
  }
}
