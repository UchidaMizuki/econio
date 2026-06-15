#' Tidy, layer, and plot an input-output table
#'
#' Methods for [generics::tidy()], [ggplot2::autolayer()], and
#' [ggplot2::autoplot()] that summarise an `econ_io_table` for visualization.
#'
#' Currently only `type = "skyline"` is supported, which produces a *skyline
#' chart* (a.k.a. an industry-by-industry diagram of production inducement).
#' For each industry the chart shows, as a stacked bar:
#'
#' * the width: the total production induced by final demand and exports, and
#' * the height: the self-sufficiency rate and the import rate, expressed as a
#'   share of domestic final demand.
#'
#' The skyline chart is only defined for competitive import type tables
#' (created with [io_table_to_competitive_import()] or
#' [io_table_regional()]/[io_table_multiregional()] with
#' `competitive_import = TRUE`).
#'
#' * `tidy()` returns the underlying data in a tidy [tibble::tibble()] (one row
#'   per industry and rate type), suitable for custom plots.
#' * `autolayer()` returns \pkg{ggplot2} layers to add to an existing plot.
#'   Pass `geom = "rect"` (default) for the skyline bars or `geom = "segment"`
#'   for the self-sufficiency outline.
#' * `autoplot()` returns a complete \pkg{ggplot2} skyline chart.
#'
#' @param x,object An `econ_io_table` object.
#' @param type A scalar character. The type of summary. Currently only
#' `"skyline"` is supported.
#' @param ... Additional arguments passed to the underlying summary. For
#' `autolayer()`, this includes `geom` (`"rect"` (default) or `"segment"`) and
#' any further arguments passed to the underlying geometry.
#'
#' @return
#' * `tidy()`: a [tibble::tibble()].
#' * `autolayer()`: a list of \pkg{ggplot2} layers.
#' * `autoplot()`: a [ggplot2::ggplot()] object.
#'
#' @examples
#' \dontrun{
#' # `iotable` is a competitive import type `econ_io_table`.
#' library(ggplot2)
#'
#' # Tidy data for a custom plot.
#' tidy(iotable, type = "skyline")
#'
#' # A complete skyline chart.
#' autoplot(iotable, type = "skyline")
#'
#' # Or build one up from layers.
#' ggplot() +
#'   autolayer(iotable, type = "skyline", geom = "rect")
#' }
#'
#' @name io_skyline
NULL

#' @rdname io_skyline
#' @export
tidy.econ_io_table <- function(x, type, ...) {
  type <- rlang::arg_match(type, c("skyline"))

  switch(type, skyline = io_tidy_skyline(x, ...))
}

#' @rdname io_skyline
#' @export
autolayer.econ_io_table <- function(object, type, ...) {
  type <- rlang::arg_match(type, c("skyline"))

  switch(type, skyline = io_autolayer_skyline(object, ...))
}

#' @rdname io_skyline
#' @export
autoplot.econ_io_table <- function(object, type, ...) {
  type <- rlang::arg_match(type, c("skyline"))

  switch(type, skyline = io_autoplot_skyline(object, ...))
}
