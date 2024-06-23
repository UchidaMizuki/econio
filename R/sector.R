new_io_sector <- function(sector_type, sector_name) {
  vctrs::new_rcrd(list(type = sector_type,
                       name = sector_name),
                  class = "econ_io_sector")
}

io_input_sector <- function(sector_type, sector_name, competitive_import) {
  values <- if(competitive_import) {
    c("industry", "value_added")
  } else {
    c("industry", "import", "value_added")
  }
  if (competitive_import && "import" %in% sector_type) {
    cli::cli_abort('{.code "import"} is not allowed in input sector types when {.code competitive_import = TRUE}.')
  }
  sector_type <- rlang::arg_match(sector_type, values,
                                  multiple = TRUE)
  sector_type <- factor(sector_type, values)

  new_io_sector(sector_type, sector_name)
}

io_output_sector <- function(sector_type, sector_name, competitive_import) {
  values <- if (competitive_import) {
    c("industry", "final_demand", "export", "import")
  } else {
    c("industry", "final_demand", "export")
  }
  if (!competitive_import && "import" %in% sector_type) {
    cli::cli_abort('{.code "import"} is not allowed in output sector types when {.code competitive_import = FALSE}.')
  }
  sector_type <- rlang::arg_match(sector_type, values,
                                  multiple = TRUE)
  sector_type <- factor(sector_type, values)

  new_io_sector(sector_type, sector_name)
}

#' Get sector types
#'
#' @param x An `econ_io_sector` object or a data frame with a column named
#' `sector`.
#'
#' @return A vector of sector types.
#'
#' @export
io_sector_type <- function(x) {
  if (is.data.frame(x)) {
    x <- x$sector
  }
  vctrs::field(x, "type")
}

#' Get sector names
#'
#' @param x An `econ_io_sector` object or a data frame with a column named
#' `sector`.
#'
#' @return A vector of sector names.
#'
#' @export
io_sector_name <- function(x) {
  if (is.data.frame(x)) {
    x <- x$sector
  }
  vctrs::field(x, "name")
}

#' @export
format.econ_io_sector <- function(x, ...) {
  sector_type <- vctrs::field(x, "type")
  sector_name <- vctrs::field(x, "name")
  paste(paste0("<", sector_type, ">"), sector_name)
}

#' @export
pillar_shaft.econ_io_sector <- function(x, ...) {
  sector_type <- vctrs::field(x, "type")
  sector_name <- vctrs::field(x, "name")
  formatted <- paste(pillar::style_subtle(paste0("<", sector_type, ">")), sector_name)
  pillar::new_pillar_shaft_simple(formatted)
}

#' @export
vec_ptype_full.econ_io_sector <- function(x, ...) {
  "sector"
}

#' @export
vec_ptype_abbr.econ_io_sector <- function(x, ...) {
  "sector"
}
