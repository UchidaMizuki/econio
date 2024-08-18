io_same_region <- function(data) {
  if (inherits(data, "io_table_regional")) {
    dibble::ones(data)
  } else if (inherits(data, "io_table_multiregional")) {
    dimnames <- dimnames(data)
    region <- union(dimnames$input$region, dimnames$output$region)

    args <- vctrs::vec_init(list(), length(region))
    for (i in seq_along(region)) {
      args[[i]] <- data |>
        dplyr::filter(.data$input$region == region[[i]],
                      .data$output$region == region[[i]]) |>
        dibble::ones()
    }
    dibble::broadcast(rlang::exec(rbind, !!!args),
                      dim_names = c("input", "output")) |>
      tidyr::replace_na(0)
  }
}

io_endogenize_import <- function(data, endogenize_import) {
  if (inherits(data, "io_table_noncompetitive_import")) {
    if (!is.null(endogenize_import)) {
      cli::cli_abort("{.code endogenize_import = NULL} is required for {.cls {class(data)}}.")
    }
  } else if (inherits(data, "io_table_competitive_import")) {
    if (is.null(endogenize_import)) {
      endogenize_import <- TRUE
      cli::cli_inform("Assuming {.code endogenize_import = TRUE}.")
    } else if (!rlang::is_scalar_logical(endogenize_import)) {
      cli::cli_abort('{.code endogenize_import} must be a scalar logical.')
    }
  }
  endogenize_import
}
