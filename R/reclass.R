#' Reclassify an input-output table
#'
#' @param data An input-output table.
#' @param input_region_data,input_sector_data,output_region_data,output_sector_data
#' Data frames used to reclassify the input and output regions and sectors.
#' If `NULL`, no reclassification is performed for that axis. By default,
#' `NULL`.
#' @param from_col,to_col,weight_col Names of the columns in the
#' reclassification data. Defaults are `"from"`, `"to"`, and `"weight"`.
#' @param weight_tolerance Tolerance for checking that weights sum to 1.
#' By default, `.Machine$double.eps^0.5`.
#' @param check_axes A scalar logical. If `TRUE`, the input and output axes are
#' checked to be identical. By default, `TRUE`.
#'
#' @return A reclassified input-output table.
#'
#' @export
io_reclass <- function(
  data,
  input_region_data = NULL,
  input_sector_data = NULL,
  output_region_data = NULL,
  output_sector_data = NULL,
  from_col = "from",
  to_col = "to",
  weight_col = "weight",
  weight_tolerance = .Machine$double.eps^0.5,
  check_axes = TRUE
) {
  dim_names <- dimnames(data)
  dim_name_input <- dim_names$input
  dim_name_output <- dim_names$output

  input_sector_data <- io_reclass_axis_data(
    axis_data = input_sector_data,
    dim_name = dim_name_input,
    type = "sector",
    from_col = {{ from_col }},
    to_col = {{ to_col }},
    weight_col = {{ weight_col }},
    weight_tolerance = weight_tolerance
  )
  output_sector_data <- io_reclass_axis_data(
    axis_data = output_sector_data,
    dim_name = dim_name_output,
    type = "sector",
    from_col = {{ from_col }},
    to_col = {{ to_col }},
    weight_col = {{ weight_col }},
    weight_tolerance = weight_tolerance
  )

  if (inherits(data, "io_table_regional")) {
    input_data <- input_sector_data |>
      dplyr::rename_with(\(x) paste(x, "input_sector", sep = "_"), !"weight") |>
      dibble::dibble_by(
        to_input = "to_input_sector",
        from_input = "from_input_sector",
        .names_sep = "_"
      ) |>
      purrr::chuck("weight") |>
      tidyr::replace_na(0)
    output_data <- output_sector_data |>
      dplyr::rename_with(
        \(x) paste(x, "output_sector", sep = "_"),
        !"weight"
      ) |>
      dibble::dibble_by(
        from_output = "from_output_sector",
        to_output = "to_output_sector",
        .names_sep = "_"
      ) |>
      purrr::chuck("weight") |>
      tidyr::replace_na(0)
  } else if (inherits(data, "io_table_multiregional")) {
    input_region_data <- io_reclass_axis_data(
      axis_data = input_region_data,
      dim_name = dim_name_input,
      type = "region",
      from_col = {{ from_col }},
      to_col = {{ to_col }},
      weight_col = {{ weight_col }},
      weight_tolerance = weight_tolerance
    )
    output_region_data <- io_reclass_axis_data(
      axis_data = output_region_data,
      dim_name = dim_name_output,
      type = "region",
      from_col = {{ from_col }},
      to_col = {{ to_col }},
      weight_col = {{ weight_col }},
      weight_tolerance = weight_tolerance
    )

    input_data <- tidyr::expand_grid(
      input_region_data |>
        dplyr::rename_with(\(x) paste(x, "input_region", sep = "_")),
      input_sector_data |>
        dplyr::rename_with(\(x) paste(x, "input_sector", sep = "_"))
    ) |>
      dplyr::mutate(
        weight = .data$weight_input_region * .data$weight_input_sector,
        .keep = "unused"
      ) |>
      dibble::dibble_by(
        to_input = c("to_input_region", "to_input_sector"),
        from_input = c("from_input_region", "from_input_sector"),
        .names_sep = "_"
      ) |>
      purrr::chuck("weight") |>
      tidyr::replace_na(0)
    output_data <- tidyr::expand_grid(
      output_region_data |>
        dplyr::rename_with(\(x) paste(x, "output_region", sep = "_")),
      output_sector_data |>
        dplyr::rename_with(\(x) paste(x, "output_sector", sep = "_"))
    ) |>
      dplyr::mutate(
        weight = .data$weight_output_region * .data$weight_output_sector,
        .keep = "unused"
      ) |>
      dibble::dibble_by(
        from_output = c("from_output_region", "from_output_sector"),
        to_output = c("to_output_region", "to_output_sector"),
        .names_sep = "_"
      ) |>
      purrr::chuck("weight") |>
      tidyr::replace_na(0)
  }

  class_data <- class(data)
  data <- data |>
    dplyr::rename(from_input = "input", from_output = "output")

  data <- input_data %*% data %*% output_data
  data <- data |>
    dplyr::rename(input = "to_input", output = "to_output")
  class(data) <- class_data

  if (check_axes) {
    io_check_axes(data)
  }
  data
}

io_reclass_axis_data <- function(
  axis_data,
  dim_name,
  type,
  from_col,
  to_col,
  weight_col,
  weight_tolerance
) {
  axis_data_name <- rlang::as_label(rlang::enquo(axis_data))

  if (is.null(axis_data)) {
    value <- vctrs::vec_unique(dim_name[[type]])
    axis_data <- tibble::tibble(
      from = value,
      to = value,
      weight = 1
    )
  } else {
    axis_data <- axis_data |>
      dplyr::rename(
        from = {{ from_col }},
        to = {{ to_col }},
        weight = {{ weight_col }}
      )
    weight_total <- axis_data |>
      dplyr::summarise(weight_total = sum(.data$weight), .by = "from")
    if (any(abs(weight_total$weight_total - 1) > weight_tolerance)) {
      cli::cli_abort(c(
        "{.arg {axis_data_name}} has weights that do not sum to 1 for some {.var from} values.",
        "i" = "Check the {.var weight} column or set {.arg weight_tolerance} to a higher value if appropriate."
      ))
    }
    if (type == "sector") {
      axis_data <- axis_data |>
        dplyr::mutate(
          sector_type = vctrs::vec_slice(
            io_sector_type(dim_name),
            vctrs::vec_match(.data$from, io_sector_name(dim_name))
          )
        ) |>
        dplyr::mutate(
          from = io_sector(.data$sector_type, .data$from),
          to = io_sector(.data$sector_type, .data$to),
          .keep = "unused"
        ) |>
        tidyr::replace_na(list(weight = 1))
    }

    from_axis_data <- axis_data$from
    axis_data <- tibble::tibble(
      from = vctrs::vec_unique(dim_name[[type]])
    ) |>
      dplyr::left_join(axis_data, by = "from") |>
      dplyr::mutate(
        to = .data$to |>
          dplyr::coalesce(.data$from)
      ) |>
      tidyr::replace_na(list(weight = 1))
  }
  axis_data
}
