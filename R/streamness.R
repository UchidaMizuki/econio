#' Upstreamness and downstreamness
#'
#' Sector-level summaries of how far each industry is from final demand
#' (upstreamness) and from primary inputs (downstreamness), derived from the
#' Ghosh and Leontief inverse matrices respectively.
#'
#' `io_upstreamness()` returns the row sums of the Ghosh inverse, i.e. the
#' total forward linkage (also called *output upstreamness*). A larger value
#' means the industry sells more of its output, directly and indirectly, to
#' other industries rather than to final demand.
#'
#' `io_downstreamness()` returns the column sums of the Leontief inverse. This
#' is identical to the **output multiplier** (the simple, Type I multiplier)
#' and to the **backward linkage** (also called *input downstreamness*). A
#' larger value means a unit of final demand for the industry pulls more total
#' output, directly and indirectly, from the rest of the economy.
#'
#' When `normalize = TRUE` the measure is rescaled so that its mean across
#' sectors equals 1. The normalized `io_downstreamness()` is the **power of
#' dispersion** (a backward-linkage index). The normalized `io_upstreamness()`
#' is the Ghosh-based forward-linkage index; note that this is **not** the same
#' as the classical *sensitivity of dispersion*, which is based on the row sums
#' of the Leontief (not Ghosh) inverse.
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical passed to [io_leontief_inverse()] /
#' [io_ghosh_inverse()]. If `TRUE`, open economy assumptions are used.
#' @param normalize A scalar logical. If `TRUE`, the measure is rescaled to
#' have a mean of 1 across sectors. By default, `FALSE`.
#'
#' @return An `econ_io_table` object with one value per industry.
#'
#' @examples
#' \dontrun{
#' # `iotable` is a competitive import type `econ_io_table`.
#' io_upstreamness(iotable)
#' io_downstreamness(iotable) # equals the output multiplier
#' io_downstreamness(iotable, normalize = TRUE) # power of dispersion
#' }
#'
#' @name io_streamness
NULL

#' @rdname io_streamness
#' @export
io_upstreamness <- function(data, open_economy = NULL, normalize = FALSE) {
  ghosh_inverse <- io_ghosh_inverse(data, open_economy = open_economy)
  upstreamness <- dibble::apply(ghosh_inverse, "output", sum)
  io_normalize(upstreamness, normalize = normalize)
}

#' @rdname io_streamness
#' @export
io_downstreamness <- function(data, open_economy = NULL, normalize = FALSE) {
  leontief_inverse <- io_leontief_inverse(data, open_economy = open_economy)
  downstreamness <- dibble::apply(leontief_inverse, "input", sum)
  io_normalize(downstreamness, normalize = normalize)
}

io_normalize <- function(x, normalize) {
  if (!rlang::is_scalar_logical(normalize)) {
    cli::cli_abort("{.arg normalize} must be a scalar logical.")
  }
  if (normalize) {
    n <- vctrs::vec_size(dimnames(x)[[1]])
    x <- x * (n / sum(x))
  }
  x
}
