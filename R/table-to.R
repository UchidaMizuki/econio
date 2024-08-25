#' Convert an input-output table to a competitive import type table
#'
#' @param data An input-output table
#'
#' @return A competitive import type input-output table
#'
#' @export
io_table_to_competitive_import <- function(data) {
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
  dim_names$output$sector <- io_output_sector("import", NA_character_,
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
#'
#' @return A noncompetitive import type input-output table
#'
#' @export
io_table_to_noncompetitive_import <- function(data) {
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
  dim_names$input$sector <- io_input_sector("import", NA_character_,
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
