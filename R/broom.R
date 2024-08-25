#' @export
tidy.econ_io_table <- function(x, type, ...) {
  type <- rlang::arg_match(type,
                           c("skyline"))

  switch(type,
         skyline = io_tidy_skyline(x, ...))
}
