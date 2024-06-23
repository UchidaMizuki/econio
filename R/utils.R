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
