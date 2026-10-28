#' Upstreamness and downstreamness (forward and backward linkages)
#'
#' Sector-level summaries of how far each industry is from final demand
#' (upstreamness) and from primary inputs (downstreamness), derived from the
#' Ghosh and Leontief inverse matrices respectively. Together they are the
#' total forward and backward linkages of input-output linkage analysis; Miller
#' and Temurshoev (2017) show that they coincide with output upstreamness and
#' input downstreamness.
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
#' @references
#' Antràs, P. and Chor, D. (2018). On the measurement of upstreamness and
#' downstreamness in global value chains. NBER Working Paper No. 24185.
#'
#' Miller, R. E. and Temurshoev, U. (2017). Output upstreamness and input
#' downstreamness of industries/countries in world production. *International
#' Regional Science Review*, 40(5), 443-475.
#'
#' @seealso [io_leontief_inverse()] and [io_ghosh_inverse()] for the
#' underlying inverse matrices.
#'
#' @examples
#' \dontrun{
#' # `iotable` is a competitive import type `econ_io_table`.
#' io_upstreamness(iotable)
#' io_downstreamness(iotable) # equals the output multiplier
#' io_downstreamness(iotable, normalize = TRUE) # power of dispersion
#' }
#'
#' @name io_linkage
NULL

#' @rdname io_linkage
#' @export
io_upstreamness <- function(data, open_economy = NULL, normalize = FALSE) {
  rlang::check_bool(normalize)

  ghosh_inverse <- io_ghosh_inverse(data, open_economy = open_economy)
  upstreamness <- dibble::apply(ghosh_inverse, "output", sum) |>
    dplyr::rename(industry = "output")
  io_normalize(upstreamness, normalize = normalize)
}

#' @rdname io_linkage
#' @export
io_downstreamness <- function(data, open_economy = NULL, normalize = FALSE) {
  rlang::check_bool(normalize)

  leontief_inverse <- io_leontief_inverse(data, open_economy = open_economy)
  downstreamness <- dibble::apply(leontief_inverse, "input", sum) |>
    dplyr::rename(industry = "input")
  io_normalize(downstreamness, normalize = normalize)
}

io_normalize <- function(x, normalize) {
  if (normalize) {
    n <- vctrs::vec_size(dimnames(x)[[1]])
    x <- x * (n / sum(x))
  }
  x
}
