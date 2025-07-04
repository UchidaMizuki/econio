#' Leontief inverse matrix
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical. If `TRUE`, imports are
#' endogenized.
#'
#' @return An `econ_io_table` object of the Leontief inverse matrix.
#'
#' @export
io_leontief_inverse <- function(data, open_economy = NULL) {
  open_economy <- io_open_economy(data, open_economy)

  if (inherits(data, "io_table_noncompetitive_import")) {
    input_coef <- io_input_coef(data)
    dibble::broadcast(
      solve(dibble::eye(input_coef) - input_coef),
      dim_names = c("output", "input")
    )
  } else if (inherits(data, "io_table_competitive_import")) {
    if (open_economy) {
      input_coef <- io_input_coef(data)
      import_coef <- io_import_coef(data)
      input_coef_same_region <- io_input_coef(data, same_region = TRUE)

      solve(
        dibble::eye(input_coef) -
          (input_coef - import_coef * input_coef_same_region)
      ) |>
        dibble::broadcast(dim_names = c("output", "input"))
    } else {
      input_coef <- io_input_coef(data)
      dibble::broadcast(
        solve(dibble::eye(input_coef) - input_coef),
        dim_names = c("output", "input")
      )
    }
  }
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

io_production_induce <- function(data, open_economy = NULL) {
  open_economy <- io_open_economy(data, open_economy)

  leontief_inverse <- io_leontief_inverse(
    data,
    open_economy = open_economy
  )
  total_final_demand <- io_total_output(
    data,
    output_sector_type = "final_demand"
  )
  total_export <- io_total_output(data, output_sector_type = "export")

  if (inherits(data, "io_table_noncompetitive_import")) {
    dibble::dibble(
      total_final_demand = leontief_inverse %*% total_final_demand,
      total_export = leontief_inverse %*% total_export
    )
  } else if (inherits(data, "io_table_competitive_import")) {
    if (open_economy) {
      import_coef <- io_import_coef(data)
      dibble::dibble(
        total_final_demand = leontief_inverse %*%
          ((1 - import_coef) * total_final_demand),
        total_export = leontief_inverse %*% total_export
      )
    } else {
      total_import <- io_total_output(data, output_sector_type = "import")
      dibble::dibble(
        total_final_demand = leontief_inverse %*% total_final_demand,
        total_export = leontief_inverse %*% total_export,
        total_import = -leontief_inverse %*% total_import
      )
    }
  }
}

io_self_sufficiency_rate <- function(data, summary = FALSE) {
  if (!inherits(data, "io_table_competitive_import")) {
    cli::cli_abort(
      "{.fn io_self_sufficiency_rate} is not implemented for {.cls {class(data)}}."
    )
  }

  production_induce <- io_production_induce(data, open_economy = FALSE)
  total_final_demand <- production_induce$total_final_demand
  total_export <- production_induce$total_export
  total_import <- production_induce$total_import

  if (summary) {
    safe_divide(
      sum(total_final_demand) + sum(total_export) - sum(total_import),
      sum(total_final_demand)
    )
  } else {
    safe_divide(
      total_final_demand + total_export - total_import,
      total_final_demand
    )
  }
}
