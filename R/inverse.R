#' Leontief inverse matrix
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical. If `TRUE`, open economy assumptions are
#' used.
#'
#' @return An `econ_io_table` object of the Leontief inverse matrix.
#'
#' @seealso [io_downstreamness()] for its column sums (backward linkage).
#'
#' @export
io_leontief_inverse <- function(data, open_economy = NULL) {
  open_economy <- io_open_economy(data, open_economy)
  data <- io_check_axes(data)

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
#' @seealso [io_upstreamness()] for its row sums (forward linkage).
#'
#' @export
io_ghosh_inverse <- function(data, open_economy = NULL) {
  open_economy <- io_open_economy(data, open_economy)
  data <- io_check_axes(data)

  output_coef <- io_output_coef(data, open_economy = open_economy)
  dibble::broadcast(
    solve(dibble::eye(output_coef) - output_coef),
    dim_names = c("output", "input")
  )
}

#' Induced production
#'
#' Computes the production of each industry induced, directly and indirectly
#' through the Leontief inverse, by each type of final demand. With `L` the
#' Leontief inverse, `f` final demand and `e` exports:
#'
#' * For noncompetitive import type tables: `final_demand = L f` and
#'   `export = L e`.
#' * For competitive import type tables with `open_economy = TRUE`:
#'   `final_demand = L (f - M f_r)` and `export = L e`, where `M` is the import
#'   coefficient (see [io_import_coef()]) and `f_r` the final demand of the
#'   same region.
#' * For competitive import type tables with `open_economy = FALSE`:
#'   `final_demand = L f`, `export = L e` and `import = L m`, where `m` is the
#'   imports as positive values, i.e. the production that imports replace.
#'
#' In all cases, `final_demand + export - import` (with `import` only when
#' present) equals the total input of each industry ([io_total_input()]).
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical passed to [io_leontief_inverse()]. If
#'   `TRUE`, open economy assumptions are used.
#'
#' @return A `dibble` over the `output` industries with the columns
#'   `final_demand`, `export` and, for competitive import type tables with
#'   `open_economy = FALSE`, `import`.
#'
#' @seealso [io_leontief_inverse()] for the underlying inverse matrix.
#'
#' @examples
#' \dontrun{
#' # `iotable` is a competitive import type `econ_io_table`.
#' io_induced_production(iotable, open_economy = FALSE)
#' }
#'
#' @export
io_induced_production <- function(data, open_economy = NULL) {
  if (!inherits(data, "econ_io_table")) {
    cli::cli_abort(
      "{.arg data} must be an {.cls econ_io_table} object, not {.obj_type_friendly {data}}."
    )
  }
  open_economy <- io_open_economy(data, open_economy)

  leontief_inverse <- io_leontief_inverse(
    data,
    open_economy = open_economy
  )
  final_demand <- io_total_output(data, output_sector_type = "final_demand")
  export <- io_total_output(data, output_sector_type = "export")

  if (inherits(data, "io_table_competitive_import") && open_economy) {
    final_demand_same_region <- io_total_output(
      data,
      output_sector_type = "final_demand",
      same_region = TRUE
    )
    final_demand <- final_demand -
      io_import_coef(data) * final_demand_same_region
  }

  if (inherits(data, "io_table_competitive_import") && !open_economy) {
    import <- -io_total_output(data, output_sector_type = "import")
    dibble::dibble(
      final_demand = leontief_inverse %*% final_demand,
      export = leontief_inverse %*% export,
      import = leontief_inverse %*% import
    )
  } else {
    dibble::dibble(
      final_demand = leontief_inverse %*% final_demand,
      export = leontief_inverse %*% export
    )
  }
}

io_self_sufficiency_rate <- function(data, summary = FALSE) {
  if (!inherits(data, "io_table_competitive_import")) {
    cli::cli_abort(
      "{.fn io_self_sufficiency_rate} is not implemented for {.cls {class(data)}}."
    )
  }

  induced_production <- io_induced_production(data, open_economy = FALSE)
  total_final_demand <- induced_production$final_demand
  total_export <- induced_production$export
  total_import <- induced_production$import

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
