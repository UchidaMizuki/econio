io_region <- function(data) {
  dimnames <- dimnames(data)
  union(dimnames$input$region, dimnames$output$region)
}

io_same_region <- function(data) {
  if (inherits(data, "io_table_regional")) {
    dibble::ones(data)
  } else if (inherits(data, "io_table_multiregional")) {
    region <- io_region(data)

    outs <- vctrs::vec_init(list(), length(region))
    for (i in seq_along(region)) {
      outs[[i]] <- data |>
        dplyr::filter(.data$input$region == region[[i]],
                      .data$output$region == region[[i]]) |>
        dibble::ones()
    }
    dibble::broadcast(rlang::exec(rbind, !!!outs),
                      dim_names = c("input", "output")) |>
      tidyr::replace_na(0)
  }
}

io_sum_region <- function(data, axis) {
  axis <- rlang::arg_match(axis, c("input", "output"))

  dim_names <- dimnames(data)
  region <- io_region(data)

  if (axis == "input") {
    output <- tibble::tibble(output_old = dim_names$output,
                             output = dim_names$output |>
                               dplyr::select("sector")) |>
      tibble::tibble(value = 1) |>
      dibble::dibble_by("output_old", "output") |>
      purrr::chuck("value") |>
      tidyr::replace_na(0)
    data <- data %*% output

    outs <- vctrs::vec_init(list(), length(region)) |>
      rlang::set_names(region)
    for (i in seq_along(region)) {
      out <- data |>
        dplyr::filter(.data$input$region == region[[i]])
      dimnames(out)$input <- dimnames(out)$input |>
        dplyr::select("sector")
      class(out)[class(out) == "io_table_multiregional"] <- "io_table_regional"
      outs[[i]] <- out
    }
  } else {
    input <- tibble::tibble(input = dim_names$input |>
                              dplyr::select("sector"),
                            input_old = dim_names$input) |>
      tibble::tibble(value = 1) |>
      dibble::dibble_by("input", "input_old") |>
      purrr::chuck("value") |>
      tidyr::replace_na(0)
    data <- input %*% data

    outs <- vctrs::vec_init(list(), length(region)) |>
      rlang::set_names(region)
    for (i in seq_along(region)) {
      out <- data |>
        dplyr::filter(.data$output$region == region[[i]])
      dimnames(out)$output <- dimnames(out)$output |>
        dplyr::select("sector")
      class(out)[class(out) == "io_table_multiregional"] <- "io_table_regional"
      outs[[i]] <- out
    }
  }
  outs
}
