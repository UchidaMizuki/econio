new_io_table <- function(data, ...,
                         class = character()) {
  class(data) <- c(class, "econ_io_table", class(data))
  data
}

#' Create a regional input-output table
#'
#' @param data A data frame.
#' @param input <[`tidy-select`][dplyr_tidy_select]> Input sector type and name
#' columns.
#' @param output <[`tidy-select`][dplyr_tidy_select]> Output sector type and
#' name columns.
#' @param competitive_import A scalar logical. If `TRUE`, the table is assumed
#' to be a competitive import type.
#' @param check_total A scalar logical. If `TRUE`, the total input and output
#' values are checked. By default, `TRUE`.
#'
#' @return An `econ_io_table` object.
#'
#' @export
io_table_regional <- function(data,
                              input = c("input_sector_type", "input_sector_name"),
                              output = c("output_sector_type", "output_sector_name"),
                              competitive_import = NULL,
                              check_total = TRUE) {
  names_input <- names(tidyselect::eval_select(rlang::enquo(input), data))
  names_output <- names(tidyselect::eval_select(rlang::enquo(output), data))

  competitive_import <- io_competitive_import(data,
                                              input_sector_type = names_input[[1]],
                                              output_sector_type = names_output[[1]],
                                              competitive_import = competitive_import)

  data |>
    io_add_sector("input",
                  sector_type = !!rlang::sym(names_input[[1]]),
                  sector_name = !!rlang::sym(names_input[[2]]),
                  competitive_import = competitive_import) |>
    io_add_sector("output",
                  sector_type = !!rlang::sym(names_output[[1]]),
                  sector_name = !!rlang::sym(names_output[[2]]),
                  competitive_import = competitive_import) |>
    dibble::dibble_by(input = "input_sector",
                      output = "output_sector",
                      .names_sep = "_") |>
    io_check_total(check_total = check_total) |>
    new_io_table(class = c(if (competitive_import) "io_table_competitive_import" else "io_table_noncompetitive_import",
                           "io_table_regional"))
}

#' Create a multi-regional input-output table
#'
#' @param data A data frame.
#' @param input <[`tidy-select`][dplyr_tidy_select]> Input region, sector type
#' and name columns.
#' @param output <[`tidy-select`][dplyr_tidy_select]> Output region, sector type
#' and name columns.
#' @param competitive_import A scalar logical. If `TRUE`, the table is assumed
#' to be a competitive import type.
#' @param check_total A scalar logical. If `TRUE`, the total input and output
#' values are checked. By default, `TRUE`.
#'
#' @return An `econ_io_table` object.
#'
#' @export
io_table_multiregional <- function(data,
                                   input = c("input_region", "input_sector_type", "input_sector_name"),
                                   output = c("output_region", "output_sector_type", "output_sector_name"),
                                   competitive_import = NULL,
                                   check_total = TRUE) {
  names_input <- names(tidyselect::eval_select(rlang::enquo(input), data))
  names_output <- names(tidyselect::eval_select(rlang::enquo(output), data))

  competitive_import <- io_competitive_import(data,
                                              input_sector_type = names_input[[2]],
                                              output_sector_type = names_output[[2]],
                                              competitive_import = competitive_import)

  data <- data |>
    io_add_sector("input",
                  sector_type = !!rlang::sym(names_input[[2]]),
                  sector_name = !!rlang::sym(names_input[[3]]),
                  competitive_import = competitive_import) |>
    io_add_region("input",
                  region = !!rlang::sym(names_input[[1]])) |>
    io_add_sector("output",
                  sector_type = !!rlang::sym(names_output[[2]]),
                  sector_name = !!rlang::sym(names_output[[3]]),
                  competitive_import = competitive_import) |>
    io_add_region("output",
                  region = !!rlang::sym(names_output[[1]])) |>
    dibble::dibble_by(input = c("input_region", "input_sector"),
                      output = c("output_region", "output_sector"),
                      .names_sep = "_") |>
    io_check_total(check_total = check_total) |>
    new_io_table(class = c(if (competitive_import) "io_table_competitive_import" else "io_table_noncompetitive_import",
                           "io_table_multiregional"))
}

io_competitive_import <- function(data, input_sector_type, output_sector_type, competitive_import) {
  if (is.null(competitive_import)) {
    input_sector_type <- data |>
      dplyr::pull({{ input_sector_type }})
    output_sector_type <- data |>
      dplyr::pull({{ output_sector_type }})

    if ("import" %in% input_sector_type && "import" %in% output_sector_type) {
      cli::cli_abort('{.code "import"} must not be in both input and output sector types.')
    } else if ("import" %in% input_sector_type) {
      competitive_import <- FALSE
    } else if ("import" %in% output_sector_type) {
      competitive_import <- TRUE
    } else {
      cli::cli_abort('{.code "import"} must be in either input or output sector types.')
    }
    cli::cli_inform('Assuming {.code competitive_import = {competitive_import}}.')
  }

  if (!rlang::is_scalar_logical(competitive_import)) {
    cli::cli_abort('{.code competitive_import} must be a scalar logical.')
  }
  competitive_import
}

io_add_sector <- function(data, axis, sector_type, sector_name, competitive_import) {
  sector_column <- switch(axis,
                          input = "input_sector",
                          output = "output_sector")
  sector_function <- switch(axis,
                            input = io_input_sector,
                            output = io_output_sector)
  data |>
    dplyr::mutate(!!sector_column := sector_function({{ sector_type }}, {{ sector_name }},
                                                     competitive_import = competitive_import),
                  .keep = "unused",
                  .before = {{ sector_type }})
}

io_add_region <- function(data, axis, region) {
  region_column <- switch(axis,
                          input = "input_region",
                          output = "output_region")
  sector_column <- switch(axis,
                          input = "input_sector",
                          output = "output_sector")
  data |>
    dplyr::rename(!!region_column := {{ region }}) |>
    dplyr::relocate(!!region_column,
                    .before = !!sector_column)
}

io_check_total <- function(data, check_total) {
  if (dibble::ncol(data) != 1) {
    cli::cli_abort("An input-output table must have only one column of amounts.")
  }
  data <- data[[1]]

  if (check_total) {
    # Check total input
    c("industry_total", "import_total", "value_added_total", "total") |>
      purrr::walk(\(input_sector_type_total) {
        data_input_total_expected <- data |>
          dplyr::filter(io_sector_type(.data$input) == .env$input_sector_type_total)

        if (!vctrs::vec_is_empty(dimnames(data_input_total_expected)[["input"]])) {
          data_input_total_expected <- data_input_total_expected |>
            dibble::apply("output", sum)

          input_sector_type <- switch(input_sector_type_total,
                                      "industry_total" = "industry",
                                      "import_total" = "import",
                                      "value_added_total" = "value_added",
                                      "total" = c("industry", "import", "value_added"))
          data_input_total <- data |>
            dplyr::filter(io_sector_type(.data$input) %in% .env$input_sector_type) |>
            dibble::apply("output", sum,
                          na.rm = TRUE)
          if (!all(dplyr::near(data_input_total, data_input_total_expected), na.rm = TRUE)) {
            cli::cli_abort("The total input values do not match in {.code {input_sector_type_total}}.")
          }
        }
      })

    # Check total output
    c("industry_total", "final_demand_total", "export_total", "import_total", "total") |>
      purrr::walk(\(output_sector_type_total) {
        data_output_total_expected <- data |>
          dplyr::filter(io_sector_type(.data$output) == .env$output_sector_type_total)

        if (!vctrs::vec_is_empty(dimnames(data_output_total_expected)[["output"]])) {
          data_output_total_expected <- data_output_total_expected |>
            dibble::apply("input", sum)

          output_sector_type <- switch(output_sector_type_total,
                                       "industry_total" = "industry",
                                       "final_demand_total" = "final_demand",
                                       "export_total" = "export",
                                       "import_total" = "import",
                                       "total" = c("industry", "final_demand", "export", "import"))
          data_output_total <- data |>
            dplyr::filter(io_sector_type(.data$output) %in% .env$output_sector_type) |>
            dibble::apply("input", sum,
                          na.rm = TRUE)
          if (!all(dplyr::near(data_output_total, data_output_total_expected), na.rm = TRUE)) {
            cli::cli_abort("The total output values do not match in {.code {output_sector_type_total}}.")
          }
        }
      })
  }
  data |>
    dplyr::filter(io_sector_type(.data$input) %in% c("industry", "import", "value_added"),
                  io_sector_type(.data$output) %in% c("industry", "final_demand", "export", "import")) |>
    tidyr::replace_na(0)
}

#' @export
tbl_format_setup.io_table_multiregional <- function(x, ...) {
  setup <- NextMethod()

  dimnames <- dimnames(x)
  names_dimnames <- names(dimnames)

  if ("input" %in% names_dimnames) {
    count_input_region <- vctrs::vec_unique_count(dimnames$input$region)
    count_input_industry <- vctrs::vec_unique_count(dimnames$input$sector[io_sector_type(dimnames$input$sector) == "industry"])
    tbl_sum_input <- c("Input" = cli::pluralize("{count_input_region} region{?s}, {count_input_industry} industr{?y/ies}"))
  } else {
    tbl_sum_input <- character()
  }

  if ("output" %in% names_dimnames) {
    count_output_region <- vctrs::vec_unique_count(dimnames$output$region)
    count_output_industry <- vctrs::vec_unique_count(dimnames$output$sector[io_sector_type(dimnames$output$sector) == "industry"])
    tbl_sum_output <- c("Output" = cli::pluralize("{count_output_region} region{?s}, {count_output_industry} industr{?y/ies}"))
  } else {
    tbl_sum_output <- character()
  }

  tbl_sum <- setup$tbl_sum
  names(tbl_sum)[1] <- "Input-output table"
  tbl_sum[1] <- "multi-regional"
  tbl_sum <- c(tbl_sum, tbl_sum_input, tbl_sum_output)

  setup$tbl_sum <- tbl_sum
  setup
}

#' @export
tbl_format_setup.io_table_regional <- function(x, ...) {
  setup <- NextMethod()

  dimnames <- dimnames(x)
  names_dimnames <- names(dimnames)

  if ("input" %in% names_dimnames) {
    count_input_sector <- vctrs::vec_unique_count(dimnames$input$sector)
    tbl_sum_input <- c("Input" = cli::pluralize("{count_input_sector} sector{?s}"))
  } else {
    tbl_sum_input <- character()
  }

  if ("output" %in% names_dimnames) {
    count_output_sector <- vctrs::vec_unique_count(dimnames$output$sector)
    tbl_sum_output <- c("Output" = cli::pluralize("{count_output_sector} sector{?s}"))
  } else {
    tbl_sum_output <- character()
  }

  tbl_sum <- setup$tbl_sum
  names(tbl_sum)[1] <- "Input-output table"
  tbl_sum[1] <- "regional"
  tbl_sum <- c(tbl_sum, tbl_sum_input, tbl_sum_output)

  setup$tbl_sum <- tbl_sum
  setup
}

#' @export
tbl_format_setup.io_table_noncompetitive_import <- function(x, ...) {
  setup <- NextMethod()

  tbl_sum <- c(setup$tbl_sum,
               "Import type" = "noncompetitive")

  setup$tbl_sum <- tbl_sum
  setup
}

#' @export
tbl_format_setup.io_table_competitive_import <- function(x, ...) {
  setup <- NextMethod()

  tbl_sum <- c(setup$tbl_sum,
               "Import type" = "competitive")

  setup$tbl_sum <- tbl_sum
  setup
}
