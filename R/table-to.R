#' Convert an input-output table to a competitive import type table
#'
#' @param data An input-output table.
#' @param data_import An optional import table. By default, `NULL`.
#' @param import_sector_name A name for the import sector. By default,
#' `NA_character_`.
#' @param import_total_tolerance A numeric tolerance for checking import totals.
#' By default, `.Machine$double.eps^0.5`.
#'
#' @return A competitive import type input-output table.
#'
#' @export
io_table_to_competitive_import <- function(
  data,
  data_import = NULL,
  import_sector_name = NA_character_,
  import_total_tolerance = .Machine$double.eps^0.5
) {
  rlang::check_string(import_sector_name, allow_na = TRUE)
  rlang::check_number_decimal(import_total_tolerance, min = 0)

  if (inherits(data, "io_table_competitive_import")) {
    return(data)
  }

  regional_demand <- data |>
    dplyr::filter(
      io_sector_type(.data$input) == "industry",
      io_sector_type(.data$output) %in% c("industry", "final_demand")
    )

  if (!is.null(data_import)) {
    import <- data_import |>
      dplyr::filter(
        io_sector_type(.data$input) == "industry",
        io_sector_type(.data$output) %in% c("industry", "final_demand")
      ) |>
      io_check_import_totals(
        data,
        import_total_tolerance = import_total_tolerance
      )
  } else {
    import <- dibble::broadcast(
      regional_demand *
        io_same_region(regional_demand) *
        io_import_coef(data, axis = "output"),
      dim_names = c("input", "output")
    )
  }

  regional_demand <- regional_demand + import

  dim_names <- dimnames(regional_demand)
  dim_names$output$sector <- io_output_sector(
    "import",
    import_sector_name,
    import_type = "competitive_import"
  )
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
  class(out)[
    class(out) == "io_table_noncompetitive_import"
  ] <- "io_table_competitive_import"
  out
}

#' Convert an input-output table to a noncompetitive import type table
#'
#' Without `data_import`, the imports of each sector are allocated over its
#' regional demand (intermediate plus final demand) in proportion to the import
#' coefficient (see [io_import_coef()]). If the imports of a sector exceed its
#' regional demand, i.e. its import coefficient is above 1, its domestic
#' transactions would become negative, so this aborts. This typically happens
#' when goods are imported and then re-exported; use
#' [io_table_remove_reexport()] to remove such re-exports first.
#'
#' @param data An input-output table.
#' @param data_import An optional import table. By default, `NULL`.
#' @param import_sector_name A name for the import sector. By default,
#' `NA_character_`.
#' @param import_total_tolerance A numeric tolerance for checking import totals.
#' By default, `.Machine$double.eps^0.5`.
#'
#' @return A noncompetitive import type input-output table.
#'
#' @export
io_table_to_noncompetitive_import <- function(
  data,
  data_import = NULL,
  import_sector_name = NA_character_,
  import_total_tolerance = .Machine$double.eps^0.5
) {
  rlang::check_string(import_sector_name, allow_na = TRUE)
  rlang::check_number_decimal(import_total_tolerance, min = 0)

  if (inherits(data, "io_table_noncompetitive_import")) {
    return(data)
  }

  regional_demand <- data |>
    dplyr::filter(
      io_sector_type(.data$input) == "industry",
      io_sector_type(.data$output) %in% c("industry", "final_demand")
    )

  if (!is.null(data_import)) {
    import <- data_import |>
      dplyr::filter(
        io_sector_type(.data$input) == "industry",
        io_sector_type(.data$output) %in% c("industry", "final_demand")
      ) |>
      io_check_import_totals(
        data,
        import_total_tolerance = import_total_tolerance
      )
  } else {
    import_coef <- io_import_coef(data)
    if (any(import_coef > 1 + .Machine$double.eps^0.5)) {
      io_abort_import_coef(data)
    }
    # Removing re-exports can leave coefficients just above 1 by rounding.
    import_coef <- dibble::ifelse(import_coef > 1, 1, import_coef)

    import <- dibble::broadcast(
      regional_demand * io_same_region(regional_demand) * import_coef,
      dim_names = c("input", "output")
    )
  }

  regional_demand <- regional_demand - import

  dim_names <- dimnames(regional_demand)
  dim_names$input$sector <- io_input_sector(
    "import",
    import_sector_name,
    import_type = "noncompetitive_import"
  )
  dim_names$input <- vctrs::vec_unique(dim_names$input)

  import <- import |>
    dibble::apply("output", sum) |>
    dibble::broadcast(dim_names = dim_names) |>
    dibble::broadcast(dim_names = c("input", "output"))
  import <- import * io_same_region(import)

  data <- data |>
    dplyr::filter(io_sector_type(.data$output) != "import")

  dim_names <- dimnames(data)
  dim_names$input <- vctrs::vec_rbind(
    dim_names$input |>
      dplyr::filter(io_sector_type(.data$sector) == "industry"),
    dimnames(import)$input,
    dim_names$input |>
      dplyr::filter(io_sector_type(.data$sector) == "value_added")
  )

  out <- data |>
    dibble::broadcast(dim_names = dim_names) |>
    dibble::broadcast(dim_names = c("input", "output")) |>
    dplyr::rows_update(regional_demand) |>
    dplyr::rows_update(import) |>
    tidyr::replace_na(0)
  class(out)[
    class(out) == "io_table_competitive_import"
  ] <- "io_table_noncompetitive_import"
  out
}

io_table_to_blocks <- function(data) {
  inter_industry <- io_inter_industry(data)
  value_added <- data |>
    dplyr::filter(
      io_sector_type(.data$input) == "value_added",
      io_sector_type(.data$output) == "industry"
    )
  final_demand <- data |>
    dplyr::filter(
      io_sector_type(.data$input) == "industry",
      io_sector_type(.data$output) == "final_demand"
    )
  export <- data |>
    dplyr::filter(
      io_sector_type(.data$input) == "industry",
      io_sector_type(.data$output) == "export"
    )

  if (inherits(data, "io_table_competitive_import")) {
    import <- data |>
      dplyr::filter(
        io_sector_type(.data$input) == "industry",
        io_sector_type(.data$output) == "import"
      )
    list(
      inter_industry = inter_industry,
      value_added = value_added,
      final_demand = final_demand,
      export = export,
      import = import
    )
  } else if (inherits(data, "io_table_noncompetitive_import")) {
    import <- data |>
      dplyr::filter(
        io_sector_type(.data$input) == "import",
        io_sector_type(.data$output) %in% c("industry", "final_demand")
      )
    list(
      inter_industry = inter_industry,
      import = import,
      value_added = value_added,
      final_demand = final_demand,
      export = export
    )
  }
}

#' Convert a multiregional input-output table to a regional input-output table
#'
#' @param data A multiregional input-output table.
#' @param export_sector_name A name for the export sector. By default,
#' `NA_character_`.
#' @param import_sector_name A name for the import sector. By default,
#' `NA_character_`.
#'
#' @return A list of regional input-output tables.
#'
#' @export
io_table_to_regional <- function(
  data,
  export_sector_name = NA_character_,
  import_sector_name = NA_character_
) {
  rlang::check_string(export_sector_name, allow_na = TRUE)
  rlang::check_string(import_sector_name, allow_na = TRUE)

  if (!inherits(data, "io_table_multiregional")) {
    cli::cli_abort(
      "{.arg data} must be a multi-regional input-output table created with {.fn io_table_multiregional}."
    )
  }

  region <- io_region(data)

  blocks <- io_table_to_blocks(data)

  regional_demand <- rbind(blocks$inter_industry, blocks$final_demand) |>
    dibble::broadcast(dim_names = c("input", "output"))
  same_region_regional_demand <- io_same_region(regional_demand)

  if (inherits(data, "io_table_competitive_import")) {
    regional_demand_regional <- io_sum_region(regional_demand, axis = "output")
    import_sector <- io_output_sector(
      "import",
      import_sector_name,
      import_type = "competitive_import"
    )
    regional_demand_inflow <- io_sum_region(
      regional_demand * !same_region_regional_demand,
      axis = "output"
    ) |>
      purrr::map(\(x) {
        x <- -dibble::apply(x, "input", sum)

        dim_names <- list(
          input = dimnames(x)$input,
          output = tibble::tibble(sector = import_sector)
        )
        dibble::broadcast(x, dim_names = dim_names) |>
          dibble::broadcast(dim_names = c("input", "output"))
      })
  } else if (inherits(data, "io_table_noncompetitive_import")) {
    regional_demand_regional <- io_sum_region(
      regional_demand * same_region_regional_demand,
      axis = "output"
    )
    import_sector <- io_input_sector(
      "import",
      import_sector_name,
      import_type = "noncompetitive_import"
    )
    regional_demand_inflow <- io_sum_region(
      regional_demand * !same_region_regional_demand,
      axis = "output"
    ) |>
      purrr::map(\(x) {
        x <- dibble::apply(x, "output", sum)

        dim_names <- list(
          input = tibble::tibble(sector = import_sector),
          output = dimnames(x)$output
        )
        dibble::broadcast(x, dim_names = dim_names) |>
          dibble::broadcast(dim_names = c("input", "output"))
      })
  }

  export_sector <- io_output_sector(
    "export",
    export_sector_name,
    import_type = if (inherits(data, "io_table_competitive_import")) {
      "competitive_import"
    } else {
      "noncompetitive_import"
    }
  )
  regional_demand_outflow <- io_sum_region(
    regional_demand * !same_region_regional_demand,
    axis = "input"
  ) |>
    purrr::map(\(x) {
      x <- dibble::apply(x, "input", sum)

      dim_names <- list(
        input = dimnames(x)$input,
        output = tibble::tibble(sector = export_sector)
      )
      dibble::broadcast(x, dim_names = dim_names) |>
        dibble::broadcast(dim_names = c("input", "output"))
    })

  value_added <- blocks$value_added
  value_added_regional <- io_sum_region(value_added, axis = "output")

  export <- blocks$export
  export_regional <- io_sum_region(export, axis = "input")

  import <- blocks$import
  if (inherits(data, "io_table_competitive_import")) {
    import_regional <- io_sum_region(import, axis = "input")
  } else if (inherits(data, "io_table_noncompetitive_import")) {
    import_regional <- io_sum_region(import, axis = "output")
  }

  list(
    regional_demand_regional,
    regional_demand_inflow,
    regional_demand_outflow,
    value_added_regional,
    export_regional,
    import_regional
  ) |>
    purrr::pmap(\(
      regional_demand_regional,
      regional_demand_inflow,
      regional_demand_outflow,
      value_added_regional,
      export_regional,
      import_regional
    ) {
      rbind(
        regional_demand_regional,
        regional_demand_inflow,
        regional_demand_outflow,
        value_added_regional,
        export_regional,
        import_regional
      ) |>
        dibble::broadcast(dim_names = c("input", "output")) |>
        tidyr::replace_na(0)
    })
}

io_check_import_totals <- function(
  data_import,
  data,
  import_total_tolerance = .Machine$double.eps^0.5,
  call = rlang::caller_env()
) {
  if (inherits(data, "io_table_noncompetitive_import")) {
    axis <- "input"
    sign_data_import_total_expected <- 1
  } else if (inherits(data, "io_table_competitive_import")) {
    axis <- "output"
    sign_data_import_total_expected <- -1
  }
  axis_other <- switch(
    axis,
    input = "output",
    output = "input"
  )

  data_import_total_actual <- data_import |>
    dibble::apply(axis_other, sum)
  data_import_total_expected <- data |>
    dplyr::filter(
      io_sector_type(.data[[axis]]) == "import",
      io_sector_type(.data[[axis_other]]) %in% c("industry", "final_demand")
    ) |>
    dibble::apply(axis_other, sum)

  data_import_total <- dibble::dibble(
    import_total_actual = data_import_total_actual,
    import_total_expected = data_import_total_expected *
      sign_data_import_total_expected,
  ) |>
    tibble::as_tibble() |>
    dplyr::filter(
      !dplyr::near(
        .data$import_total_actual,
        .data$import_total_expected,
        tol = .env$import_total_tolerance
      )
    )

  if (!vctrs::vec_is_empty(data_import_total)) {
    cli::cli_abort(
      c(
        "The total import values do not match.",
        glue::glue(
          "{io_axis_label(data_import_total[[axis_other]])}: {data_import_total$import_total_actual} (`actual`) not nearly equal to {data_import_total$import_total_expected} (`expected`)."
        ) |>
          rlang::set_names("x")
      ),
      call = call
    )
  }
  data_import
}

io_abort_import_coef <- function(data, call = rlang::caller_env()) {
  excess <- io_import_excess(data) |>
    tibble::as_tibble() |>
    dplyr::filter(
      .data$excess > .Machine$double.eps^0.5 * .data$regional_demand
    )

  cli::cli_abort(
    c(
      "Imports must not exceed regional demand (intermediate plus final demand), or domestic transactions become negative.",
      glue::glue(
        "{io_axis_label(excess$input)}: imports of {signif(excess$import, 4)} exceed regional demand of {signif(excess$regional_demand, 4)} (import coefficient {signif(excess$import / excess$regional_demand, 4)})."
      ) |>
        rlang::set_names("x"),
      "i" = "Imports beyond regional demand are typically re-exports.",
      "i" = "Use {.fn io_table_remove_reexport} to remove them from both imports and exports first."
    ),
    call = call
  )
}
