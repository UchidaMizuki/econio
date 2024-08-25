io_tidy_skyline <- function(x, ...) {
  rlang::check_dots_empty()

  if (!inherits(x, "io_table_competitive_import")) {
    cli::cli_abort("{.fn io_tidy_skyline} is not implemented for {.cls {class(x)}}.")
  }

  io_production_induce(x,
                       endogenize_import = FALSE) |>
    tibble::as_tibble() |>
    tidyr::unpack("output") |>
    dplyr::mutate(total = .data$total_final_demand + .data$total_export - .data$total_import,
                  xmin = cumsum(.data$total) - .data$total,
                  xmax = .data$xmin + .data$total,

                  rate_self_sufficiency_rate = .data$total / .data$total_final_demand,
                  rate_import_rate = .data$total_import / .data$total_final_demand,

                  ymin_self_sufficiency_rate = 0,
                  ymin_import_rate = .data$rate_self_sufficiency_rate,

                  ymax_self_sufficiency_rate = .data$rate_self_sufficiency_rate,
                  ymax_import_rate = .data$rate_self_sufficiency_rate + .data$rate_import_rate,

                  yend_self_sufficiency_rate = dplyr::lead(.data$ymax_self_sufficiency_rate),

                  .keep = "unused") |>
    tidyr::pivot_longer(tidyselect::starts_with(c("rate", "ymin", "ymax", "yend")),
                        names_to = c(".value", "rate_type"),
                        names_pattern = "^(rate|ymin|ymax|yend)_(.*)",
                        names_transform = list(rate_type = \(x) factor(x, c("import_rate", "self_sufficiency_rate"))))
}

io_autolayer_skyline <- function(object, geom, ...) {
  geom <- rlang::arg_match(geom, c("rect", "segment"))

  switch(geom,
         rect = io_autolayer_skyline_rect(object, ...),
         segment = io_autolayer_skyline_segment(object, ...))
}

io_autolayer_skyline_rect <- function(object, ...) {
  data <- broom::tidy(object,
                      type = "skyline")

  ggplot2::geom_rect(data = data,
                     ggplot2::aes(xmin = .data$xmin,
                                  xmax = .data$xmax,
                                  ymin = .data$ymin,
                                  ymax = .data$ymax,
                                  fill = .data$rate_type), ...)
}

io_autolayer_skyline_segment <- function(object, ...) {
  data <- broom::tidy(object,
                      type = "skyline") |>
    dplyr::filter(.data$rate_type == "self_sufficiency_rate")

  list(ggplot2::geom_segment(data = data,
                             ggplot2::aes(x = .data$xmin,
                                          xend = .data$xmax,
                                          y = .data$ymax,
                                          yend = .data$ymax), ...),
       ggplot2::geom_segment(data = data |>
                               dplyr::filter(!is.na(.data$yend)),
                             ggplot2::aes(x = .data$xmax,
                                          xend = .data$xmax,
                                          y = .data$ymax,
                                          yend = .data$yend), ...))
}

io_autoplot_skyline <- function(object, ...) {
  data <- broom::tidy(object,
                      type = "skyline") |>
    dplyr::filter(.data$rate_type == "self_sufficiency_rate") |>
    dplyr::mutate(x = (.data$xmin + .data$xmax) / 2,
                  label = io_sector_name(.data$sector))

  ggplot2::ggplot() +
    ggplot2::autolayer(object,
                       type = "skyline",
                       geom = "rect",
                       color = "dimgray") +
    ggplot2::autolayer(object,
                       type = "skyline",
                       geom = "segment",
                       color = "red") +
    ggplot2::geom_hline(yintercept = 1,
                        linetype = "dashed") +
    ggplot2::scale_x_continuous("Total",
                                labels = scales::label_comma(),
                                position = "top",
                                sec.axis = ggplot2::sec_axis(identity,
                                                             name = NULL,
                                                             breaks = data$x,
                                                             labels = data$label,
                                                             guide = ggplot2::guide_axis(angle = 90))) +
    ggplot2::scale_y_continuous("Rate",
                                labels = scales::label_percent()) +
    ggplot2::scale_fill_manual("Rate type",
                               values = c("self_sufficiency_rate" = "white",
                                          "import_rate" = "gray"),
                               labels = c("self_sufficiency_rate" = "Self-sufficiency rate",
                                          "import_rate" = "Import rate"))
}
