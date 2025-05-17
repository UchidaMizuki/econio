#' @export
tidy.econ_io_table <- function(x, type, ...) {
  type <- rlang::arg_match(type, c("skyline"))

  switch(type, skyline = io_tidy_skyline(x, ...))
}

#' @export
autolayer.econ_io_table <- function(object, type, ...) {
  type <- rlang::arg_match(type, c("skyline"))

  switch(type, skyline = io_autolayer_skyline(object, ...))
}

#' @export
autoplot.econ_io_table <- function(object, type, ...) {
  type <- rlang::arg_match(type, c("skyline"))

  switch(type, skyline = io_autoplot_skyline(object, ...))
}
