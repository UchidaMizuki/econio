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

  region <- io_region(data)

  outs <- vctrs::vec_init(list(), length(region))
  for (i in seq_along(region)) {
    if (axis == "input") {
      out <- data |>
        dplyr::filter(.data$output$region == region[[i]])
    } else {
      out <- data |>
        dplyr::filter(.data$input$region == region[[i]])
    }

    dim_names <- dimnames(out)
    if (axis == "input") {
      dimnames(out) <- list(input = dim_names$input,
                            output = dim_names$output |>
                              dplyr::select(!"region"))
    } else {
      dimnames(out) <- list(input = dim_names$input |>
                              dplyr::select(!"region"),
                            output = dim_names$output)
    }
    outs[[i]] <- out
  }
  data <- purrr::reduce(outs, `+`)

  outs <- vctrs::vec_init(list(), length(region)) |>
    rlang::set_names(region)
  for (i in seq_along(region)) {
    if (axis == "input") {
      out <- data |>
        dplyr::filter(.data$input$region == region[[i]])
    } else {
      out <- data |>
        dplyr::filter(.data$output$region == region[[i]])
    }

    dim_names <- dimnames(out)
    if (axis == "input") {
      dimnames(out) <- list(input = dim_names$input |>
                              dplyr::select(!"region"),
                            output = dim_names$output)
    } else {
      dimnames(out) <- list(input = dim_names$input,
                            output = dim_names$output |>
                              dplyr::select(!"region"))
    }

    class(out)[class(out) == "io_table_multiregional"] <- "io_table_regional"
    outs[[i]] <- out
  }
  outs
}
