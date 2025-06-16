#' Leontief inverse matrix
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical. If `TRUE`, open economy assumptions are
#' used.
#'
#' @return An `econ_io_table` object of the Leontief inverse matrix.
#'
#' @export
io_leontief_inverse <- function(data, open_economy = NULL) {
  open_economy <- io_open_economy(data, open_economy)

  input_coef <- io_input_coef(data, open_economy = open_economy)
  dibble::broadcast(
    solve(dibble::eye(input_coef) - input_coef),
    dim_names = c("output", "input")
  )
}

#' Ghosh inverse matrix
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical. If `TRUE`, open economy assumptions are
#' used.
#'
#' @return An `econ_io_table` object of the Ghosh inverse matrix.
#'
#' @export
io_ghosh_inverse <- function(data, open_economy = NULL) {
  open_economy <- io_open_economy(data, open_economy)

  output_coef <- io_output_coef(data, open_economy = open_economy)
  dibble::broadcast(
    solve(dibble::eye(output_coef) - output_coef),
    dim_names = c("output", "input")
  )
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
