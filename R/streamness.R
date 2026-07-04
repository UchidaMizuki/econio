#' Upstreamness, downstreamness, and supply chain length
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
#' `io_streamness_length()` returns the sum of `io_upstreamness()` and
#' `io_downstreamness()`. Following Antràs and Chor (2018), this sum measures
#' the full length of the supply chain passing through the industry, i.e. the
#' total number of production stages from pure value added to final
#' consumption.
#'
#' When `normalize = TRUE` the measure is rescaled so that its mean across
#' sectors equals 1. The normalized `io_downstreamness()` is the **power of
#' dispersion** (a backward-linkage index). The normalized `io_upstreamness()`
#' is the Ghosh-based forward-linkage index; note that this is **not** the same
#' as the classical *sensitivity of dispersion*, which is based on the row sums
#' of the Leontief (not Ghosh) inverse. When `normalize = TRUE`,
#' `io_streamness_length()` has a mean of 2 across sectors.
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
#' @seealso [io_streamness_position()]
#'
#' @examples
#' \dontrun{
#' # `iotable` is a competitive import type `econ_io_table`.
#' io_upstreamness(iotable)
#' io_downstreamness(iotable) # equals the output multiplier
#' io_downstreamness(iotable, normalize = TRUE) # power of dispersion
#' io_streamness_length(iotable) # total number of production stages
#' }
#'
#' @name io_streamness
NULL

#' @rdname io_streamness
#' @export
io_upstreamness <- function(data, open_economy = NULL, normalize = FALSE) {
  rlang::check_bool(normalize)

  ghosh_inverse <- io_ghosh_inverse(data, open_economy = open_economy)
  upstreamness <- dibble::apply(ghosh_inverse, "output", sum) |>
    dplyr::rename(industry = "output")
  io_normalize(upstreamness, normalize = normalize)
}

#' @rdname io_streamness
#' @export
io_downstreamness <- function(data, open_economy = NULL, normalize = FALSE) {
  rlang::check_bool(normalize)

  leontief_inverse <- io_leontief_inverse(data, open_economy = open_economy)
  downstreamness <- dibble::apply(leontief_inverse, "input", sum) |>
    dplyr::rename(industry = "input")
  io_normalize(downstreamness, normalize = normalize)
}

#' @rdname io_streamness
#' @export
io_streamness_length <- function(data, open_economy = NULL, normalize = FALSE) {
  rlang::check_bool(normalize)

  upstreamness <- io_upstreamness(
    data,
    open_economy = open_economy,
    normalize = normalize
  )
  downstreamness <- io_downstreamness(
    data,
    open_economy = open_economy,
    normalize = normalize
  )
  upstreamness + downstreamness
}

#' Supply chain position
#'
#' `r lifecycle::badge("experimental")`
#'
#' Net position of each industry within the supply chain, derived from
#' `io_upstreamness()` (`U`) and `io_downstreamness()` (`D`). A positive
#' value indicates that the industry sits relatively further upstream
#' (closer to primary inputs); a negative value indicates it sits relatively
#' further downstream (closer to final demand).
#'
#' `type` selects the combination:
#' * `"difference"` (default): `U - D`.
#' * `"relative"`: `(U - D) / (U + D)`, bounded in `[-1, 1]`.
#' * `"log_ratio"`: `log(U / D)`, scale-robust.
#'
#' Unlike [io_streamness_length()], this is not a single named index from a
#' specific paper; these are natural arithmetic complements to
#' `io_upstreamness()` and `io_downstreamness()`, provided as an experimental
#' convenience. The exact formulation(s) may change in future releases.
#'
#' When `normalize = TRUE`, `U` and `D` are each rescaled to a mean of 1
#' before combining, so `type = "difference"` has a mean of 0 across
#' industries: a relative-position measure centered on the average.
#'
#' @param data An `econ_io_table` object.
#' @param open_economy A scalar logical passed to [io_leontief_inverse()] /
#' [io_ghosh_inverse()]. If `TRUE`, open economy assumptions are used.
#' @param normalize A scalar logical. If `TRUE`, the measure is rescaled to
#' have a mean of 1 across sectors. By default, `FALSE`.
#' @param type One of `"difference"`, `"relative"`, or `"log_ratio"`. See
#' Details.
#'
#' @return An `econ_io_table` object with one value per industry.
#'
#' @seealso [io_streamness_length()]
#'
#' @examples
#' \dontrun{
#' io_streamness_position(iotable)
#' io_streamness_position(iotable, normalize = TRUE) # mean 0 across industries
#' io_streamness_position(iotable, type = "relative")
#' io_streamness_position(iotable, type = "log_ratio")
#' }
#'
#' @export
io_streamness_position <- function(
  data,
  open_economy = NULL,
  normalize = FALSE,
  type = c("difference", "relative", "log_ratio")
) {
  rlang::check_bool(normalize)
  type <- rlang::arg_match(type)

  upstreamness <- io_upstreamness(
    data,
    open_economy = open_economy,
    normalize = normalize
  )
  downstreamness <- io_downstreamness(
    data,
    open_economy = open_economy,
    normalize = normalize
  )

  switch(
    type,
    difference = upstreamness - downstreamness,
    relative = (upstreamness - downstreamness) /
      (upstreamness + downstreamness),
    log_ratio = log(upstreamness / downstreamness)
  )
}

io_normalize <- function(x, normalize) {
  if (normalize) {
    n <- vctrs::vec_size(dimnames(x)[[1]])
    x <- x * (n / sum(x))
  }
  x
}
