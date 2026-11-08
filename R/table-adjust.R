#' Convert byproduct transactions from the Stone method to the transfer method
#'
#' Functions that build the domestic production network (`io_trophic_level()`,
#' `io_trophic_incoherence()`, and `io_spectral_embedding()`) convert to
#' noncompetitive import type internally as needed, so there is usually no
#' need to call {\link{io_table_to_noncompetitive_import}} yourself first.
#' In fact, converting first is best avoided: a leftover negative domestic
#' intermediate transaction can leak into the import allocation during that
#' conversion and produce a negative import value.
#'
#' @param data An input-output table.
#'
#' @return An input-output table with negative domestic intermediate
#' transactions transferred to the corresponding reverse-direction cell.
#'
#' @export
io_table_transfer_byproduct <- function(data) {
  data <- io_check_axes(data)

  inter_industry <- io_inter_industry(data)
  dim_names <- dimnames(inter_industry)
  weight <- as.matrix(inter_industry)

  if (sum(weight < 0) == 0) {
    return(data)
  }

  weight <- pmax(weight, 0) + t(pmax(-weight, 0))
  inter_industry <- dibble::dibble(as.vector(weight), .dim_names = dim_names)

  data |>
    dplyr::rows_update(inter_industry)
}

#' Remove re-exports from an input-output table
#'
#' Treats the imports of each sector that exceed its regional demand
#' (intermediate plus final demand within the same region) as re-exports, i.e.
#' goods that are imported and then exported again, and subtracts them from
#' both the imports and the exports of the sector. The excess is split over the
#' import columns (e.g. imports and inflows) and the export columns (e.g.
#' exports and outflows) of the sector in proportion to their values.
#'
#' Total output and regional demand stay the same, and the import coefficient
#' of each such sector (see [io_import_coef()]) becomes 1, so that
#' [io_table_to_noncompetitive_import()] no longer makes its domestic
#' transactions negative. Sectors whose imports are within their regional demand
#' are left unchanged.
#'
#' @param data A competitive import type input-output table.
#'
#' @return An input-output table with re-exports removed from its imports and
#' exports.
#'
#' @export
io_table_remove_reexport <- function(data) {
  if (!inherits(data, "io_table_competitive_import")) {
    cli::cli_abort(
      c(
        "{.arg data} must be a competitive import type input-output table.",
        "i" = "Re-exports can only be identified when imports are recorded by product."
      )
    )
  }

  excess <- io_import_excess(data)
  is_excess <- excess$excess > 0
  if (!any(is_excess)) {
    return(data)
  }

  infeasible <- excess |>
    tibble::as_tibble() |>
    dplyr::filter(exceeds(.data$excess, .data$export))
  if (!vctrs::vec_is_empty(infeasible)) {
    cli::cli_abort(
      c(
        "Re-exports must not exceed exports.",
        glue::glue(
          "{io_axis_label(infeasible$input)}: imports exceed regional demand by {signif(infeasible$excess, 4)}, but exports are {signif(infeasible$export, 4)}."
        ) |>
          rlang::set_names("x"),
        "i" = "This happens when output is negative, or when re-exports go to other regions, which can't be removed without changing their demand."
      )
    )
  }

  import_scale <- dibble::ifelse(
    is_excess,
    excess$regional_demand / excess$import,
    1
  )
  # Rounding can leave the excess slightly above the exports.
  export_scale <- dibble::ifelse(
    is_excess,
    pmax(1 - excess$excess / excess$export, 0),
    1
  )

  import <- data |>
    dplyr::filter(
      io_sector_type(.data$input) == "industry",
      io_sector_type(.data$output) == "import"
    )
  export <- data |>
    dplyr::filter(
      io_sector_type(.data$input) == "industry",
      io_sector_type(.data$output) == "export"
    )
  data |>
    dplyr::rows_update(
      dibble::broadcast(import * import_scale, dim_names = c("input", "output"))
    ) |>
    dplyr::rows_update(
      dibble::broadcast(export * export_scale, dim_names = c("input", "output"))
    )
}

# Imports, regional demand, the excess of imports over regional demand, and
# exports by industry of a competitive import type table, where regional demand
# is the intermediate and final demand within the same region.
io_import_excess <- function(data) {
  import <- -io_total_output(data, output_sector_type = "import")
  regional_demand <- io_total_output(
    data,
    output_sector_type = c("industry", "final_demand"),
    same_region = TRUE
  )
  dibble::dibble(
    import = import,
    regional_demand = regional_demand,
    excess = import - regional_demand,
    export = io_total_output(data, output_sector_type = "export")
  )
}
