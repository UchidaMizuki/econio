#' Convert an input-output table to a competitive import type table
#'
#' @param data An input-output table
#' @param import_sector_name A name for the import sector. By default,
#' `NA_character_`.
#'
#' @return A competitive import type input-output table.
#'
#' @export
io_table_to_competitive_import <- function(data,
                                           import_sector_name = NA_character_) {
  if (inherits(data, "io_table_competitive_import")) {
    return(data)
  }

  regional_demand <- data |>
    dplyr::filter(io_sector_type(.data$input) == "industry",
                  io_sector_type(.data$output) %in% c("industry", "final_demand"))
  import <- dibble::broadcast(regional_demand * io_same_region(regional_demand) * io_import_coef(data, axis = "input"),
                              dim_names = c("input", "output"))

  regional_demand <- regional_demand + import

  dim_names <- dimnames(regional_demand)
  dim_names$output$sector <- io_output_sector("import", import_sector_name,
                                              competitive_import = TRUE)
  dim_names$output <- vctrs::vec_unique(dim_names$output)

  import <- -import |>
    dibble::apply("input", sum) |>
    dibble::broadcast(dim_names = dim_names) |>
    dibble::broadcast(dim_names = c("input", "output"))
  import <- import * io_same_region(import)

  out <- data |>
    dplyr::filter(io_sector_type(.data$input) != "import") |>
    dplyr::rows_update(regional_demand) |>
    dplyr::rows_insert(import) |>
    dibble::broadcast(dim_names = c("input", "output")) |>
    tidyr::replace_na(0)
  class(out)[class(out) == "io_table_noncompetitive_import"] <- "io_table_competitive_import"
  out
}

#' Convert an input-output table to a noncompetitive import type table
#'
#' @param data An input-output table
#' @param import_sector_name A name for the import sector. By default,
#' `NA_character_`.
#'
#' @return A noncompetitive import type input-output table.
#'
#' @export
io_table_to_noncompetitive_import <- function(data,
                                              import_sector_name = NA_character_) {
  if (inherits(data, "io_table_noncompetitive_import")) {
    return(data)
  }

  regional_demand <- data |>
    dplyr::filter(io_sector_type(.data$input) == "industry",
                  io_sector_type(.data$output) %in% c("industry", "final_demand"))
  import <- dibble::broadcast(regional_demand * io_same_region(regional_demand) * io_import_coef(data),
                              dim_names = c("input", "output"))

  regional_demand <- regional_demand - import

  dim_names <- dimnames(regional_demand)
  dim_names$input$sector <- io_input_sector("import", import_sector_name,
                                            competitive_import = FALSE)
  dim_names$input <- vctrs::vec_unique(dim_names$input)

  import <- import |>
    dibble::apply("output", sum) |>
    dibble::broadcast(dim_names = dim_names) |>
    dibble::broadcast(dim_names = c("input", "output"))
  import <- import * io_same_region(import)

  out <- data |>
    dplyr::filter(io_sector_type(.data$output) != "import") |>
    dplyr::rows_update(regional_demand) |>
    dplyr::rows_insert(import) |>
    dibble::broadcast(dim_names = c("input", "output")) |>
    tidyr::replace_na(0)
  class(out)[class(out) == "io_table_competitive_import"] <- "io_table_noncompetitive_import"
  out
}

io_table_to_blocks <- function(data) {
  inter_industry <- data |>
    dplyr::filter(io_sector_type(.data$input) == "industry",
                  io_sector_type(.data$output) == "industry")
  value_added <- data |>
    dplyr::filter(io_sector_type(.data$input) == "value_added",
                  io_sector_type(.data$output) == "industry")
  final_demand <- data |>
    dplyr::filter(io_sector_type(.data$input) == "industry",
                  io_sector_type(.data$output) == "final_demand")
  export <- data |>
    dplyr::filter(io_sector_type(.data$input) == "industry",
                  io_sector_type(.data$output) == "export")

  if (inherits(data, "io_table_competitive_import")) {
    import <- data |>
      dplyr::filter(io_sector_type(.data$input) == "industry",
                    io_sector_type(.data$output) == "import")
    list(inter_industry = inter_industry,
         value_added = value_added,
         final_demand = final_demand,
         export = export,
         import = import)
  } else if (inherits(data, "io_table_noncompetitive_import")) {
    import <- data |>
      dplyr::filter(io_sector_type(.data$input) == "import",
                    io_sector_type(.data$output) == "industry")
    list(inter_industry = inter_industry,
         import = import,
         value_added = value_added,
         final_demand = final_demand,
         export = export)
  }
}

#' Convert a multiregional input-output table to a regional input-output table
#'
#' @param data A multiregional input-output table
#' @param export_sector_name A name for the export sector. By default,
#' `NA_character_`.
#' @param import_sector_name A name for the import sector. By default,
#' `NA_character_`.
#'
#' @return A list of regional input-output tables.
#'
#' @export
io_table_to_regional <- function(data,
                                 export_sector_name = NA_character_,
                                 import_sector_name = NA_character_) {
  region <- io_region(data)

  blocks <- io_table_to_blocks(data)

  regional_demand <- rbind(blocks$inter_industry,
                           blocks$final_demand) |>
    dibble::broadcast(dim_names = c("input", "output"))
  same_region_regional_demand <- io_same_region(regional_demand)

  if (inherits(data, "io_table_competitive_import")) {
    regional_demand_regional <- io_sum_region(regional_demand,
                                              axis = "input")
    import_sector <- io_output_sector("import", import_sector_name,
                                      competitive_import = TRUE)
    regional_demand_inflow <- io_sum_region(regional_demand * !same_region_regional_demand,
                                            axis = "output") |>
      purrr::map(\(x) {
        x <- -dibble::apply(x, "input", sum)

        dim_names <- list(input = dimnames(x)$input,
                          output = tibble::tibble(sector = import_sector))
        dibble::broadcast(x,
                          dim_names = dim_names) |>
          dibble::broadcast(dim_names = c("input", "output"))
      })
  } else if (inherits(data, "io_table_noncompetitive_import")) {
    regional_demand_regional <- io_sum_region(regional_demand * same_region_regional_demand,
                                              axis = "input")
    import_sector <- io_input_sector("import", import_sector_name,
                                     competitive_import = FALSE)
    regional_demand_inflow <- io_sum_region(regional_demand * !same_region_regional_demand,
                                            axis = "output") |>
      purrr::map(\(x) {
        x <- dibble::apply(x, "output", sum)

        dim_names <- list(input = tibble::tibble(sector = import_sector),
                          output = dimnames(x)$output)
        dibble::broadcast(x,
                          dim_names = dim_names) |>
          dibble::broadcast(dim_names = c("input", "output"))
      })
  }

  export_sector <- io_output_sector("export", export_sector_name,
                                    competitive_import = inherits(data, "io_table_competitive_import"))
  regional_demand_outflow <- io_sum_region(regional_demand * !same_region_regional_demand,
                                           axis = "input") |>
    purrr::map(\(x) {
      x <- dibble::apply(x, "input", sum)

      dim_names <- list(input = dimnames(x)$input,
                        output = tibble::tibble(sector = export_sector))
      dibble::broadcast(x,
                        dim_names = dim_names) |>
        dibble::broadcast(dim_names = c("input", "output"))
    })

  value_added <- blocks$value_added
  value_added_regional <- io_sum_region(value_added,
                                        axis = "output")

  export <- blocks$export
  export_regional <- io_sum_region(export,
                                   axis = "input")

  import <- blocks$import
  if (inherits(data, "io_table_competitive_import")) {
    import_regional <- io_sum_region(import,
                                     axis = "input")
  } else if (inherits(data, "io_table_noncompetitive_import")) {
    import_regional <- io_sum_region(import,
                                     axis = "output")
  }

  list(regional_demand_regional,
       regional_demand_inflow,
       regional_demand_outflow,
       value_added_regional,
       export_regional,
       import_regional) |>
    purrr::pmap(\(regional_demand_regional,
                  regional_demand_inflow,
                  regional_demand_outflow,
                  value_added_regional,
                  export_regional,
                  import_regional) {
      rbind(regional_demand_regional,
            regional_demand_inflow,
            regional_demand_outflow,
            value_added_regional,
            export_regional,
            import_regional) |>
        dibble::broadcast(dim_names = c("input", "output")) |>
        tidyr::replace_na(0)
    })
}
