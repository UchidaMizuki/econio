#' @export
autolayer.econ_io_table <- function(object, type, ...) {
  type <- rlang::arg_match(type,
                           c("skyline"))

  switch(type,
         skyline = io_autolayer_skyline(object, ...))
}

#' @export
autoplot.econ_io_table <- function(object, type, ...) {
  type <- rlang::arg_match(type,
                           c("skyline"))

  switch(type,
         skyline = io_autoplot_skyline(object, ...))
}
