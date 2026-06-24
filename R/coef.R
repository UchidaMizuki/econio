#' Input coefficients
#'
#' Calculate the industry-by-industry input coefficient matrix, also known as
#' the technical coefficient matrix or A matrix.
#'
#' Each element `a[i, j]` represents the amount of input from industry `i`
#' required to produce one unit of output from industry `j`. The coefficient
#' is calculated as the ratio of intermediate input to total input (gross
#' output).
#'
#' For competitive import type tables, `open_economy` controls whether to
#' adjust the coefficients for import competition. When `open_economy = TRUE`,
#' the coefficients are adjusted to exclude imports: `a[i, j] - m[i] * a[i, j]`
#' where `m[i]` is the import coefficient for industry `i` in the same region.
#' This gives the domestic (non-import) input coefficient used in open economy
#' Leontief models. For noncompetitive import tables, `open_economy` must be
#' `NULL` as imports are already separated.
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical. If `TRUE`, open economy assumptions
#' are used (competitive import tables only). Must be `NULL` for noncompetitive
#' import tables.
#'
#' @return An `econ_io_table` object of input coefficients with dimensions
#' `c("input", "output")`.
#'
#' @examples
#' \dontrun{
#' # Noncompetitive import table
#' io_input_coef(iotable_noncomp)
#'
#' # Competitive import table, closed economy
#' io_input_coef(iotable_comp, open_economy = FALSE)
#'
#' # Competitive import table, open economy (domestic coefficients)
#' io_input_coef(iotable_comp, open_economy = TRUE)
#' }
#'
#' @export
io_input_coef <- function(data, open_economy = NULL) {
  open_economy <- io_open_economy(data, open_economy)

  input <- io_inter_industry(data)
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
#' Calculate the industry-by-industry output coefficient matrix, also known as
#' the allocation coefficient matrix or B matrix, used in the Ghosh model.
#'
#' Each element `b[i, j]` represents the proportion of industry `i`'s output
#' that is sold to industry `j`. The coefficient is calculated as the ratio
#' of intermediate output to total output (gross output).
#'
#' For competitive import type tables, `open_economy` controls the denominator
#' calculation. When `open_economy = TRUE`, the denominator is adjusted to
#' `total_output - (total_export - total_import)` to represent domestic
#' production sold domestically. When `open_economy = FALSE`, the denominator
#' is simply total output. For noncompetitive import tables, `open_economy`
#' must be `NULL`.
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical. If `TRUE`, open economy assumptions
#' are used (competitive import tables only). Must be `NULL` for noncompetitive
#' import tables.
#'
#' @return An `econ_io_table` object of output coefficients with dimensions
#' `c("input", "output")`.
#'
#' @examples
#' \dontrun{
#' # Noncompetitive import table
#' io_output_coef(iotable_noncomp)
#'
#' # Competitive import table, closed economy
#' io_output_coef(iotable_comp, open_economy = FALSE)
#'
#' # Competitive import table, open economy
#' io_output_coef(iotable_comp, open_economy = TRUE)
#' }
#'
#' @export
io_output_coef <- function(data, open_economy = NULL) {
  open_economy <- io_open_economy(data, open_economy)

  output <- io_inter_industry(data)

  total_output <- io_total_output(data)

  if (isTRUE(open_economy)) {
    total_export <- io_total_output(
      data,
      output_sector_type = "export"
    )
    total_import <- -io_total_output(
      data,
      output_sector_type = "import"
    )

    dibble::broadcast(
      safe_divide(
        output,
        total_output - (total_export - total_import)
      ),
      dim_names = c("input", "output")
    )
  } else {
    dibble::broadcast(
      safe_divide(output, total_output),
      dim_names = c("input", "output")
    )
  }
}

# import ------------------------------------------------------------------

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
#' Calculate import coefficients for input-output tables. The calculation and
#' interpretation differ based on the import type (competitive vs
#' noncompetitive) and the specified axis.
#'
#' For **noncompetitive import** tables, imports are recorded as a separate
#' input row. When `axis = "input"`, all coefficients are zero (imports are
#' not allocated to industries as inputs). When `axis = "output"`, coefficients
#' represent the import share of regional demand: `import / regional_demand`
#' for same-region demand, zero for cross-region flows.
#'
#' For **competitive import** tables, imports are recorded as a separate output
#' column. When `axis = "input"`, coefficients represent the import share of
#' regional demand (negative values): `-import / regional_demand` for
#' same-region demand. When `axis = "output"`, all coefficients are zero.
#'
#' The axis parameter determines whether the coefficient varies by input
#' industry (axis = "input") or output industry/final demand (axis = "output").
#'
#' @param data An `econ_io_table` object.
#' @param axis A scalar character. Either `"input"` (default) or `"output"`,
#' specifying whether to calculate import coefficients by input industry or by
#' output category.
#'
#' @return An `econ_io_table` object of import coefficients. The dimensions
#' depend on `axis` and the table type.
#'
#' @examples
#' \dontrun{
#' # Noncompetitive import table
#' io_import_coef(iotable_noncomp, axis = "input")  # all zeros
#' io_import_coef(iotable_noncomp, axis = "output") # import shares by output
#'
#' # Competitive import table
#' io_import_coef(iotable_comp, axis = "input")  # import shares by input
#' io_import_coef(iotable_comp, axis = "output") # all zeros
#' }
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
