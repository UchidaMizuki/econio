#' Trophic levels and trophic incoherence
#'
#' Network measures of where each industry sits in the domestic production
#' hierarchy, following the directed-network trophic level of MacKay, Johnson
#' and Sansom (2020).
#'
#' The table is first converted to a noncompetitive import type table with
#' [io_table_to_noncompetitive_import()] so that the industry-by-industry block
#' holds purely domestic intermediate transactions. These transactions are
#' treated as a weighted directed network `w[i, j]` (the flow from industry `i`
#' to industry `j`), and the trophic level `h` solves the Laplacian system
#'
#' \deqn{(\mathrm{diag}(u) - (W + W^\top))\,h = v,}
#'
#' where `u` is the total weight (in plus out) of each node and `v` is its
#' imbalance (in minus out). The levels are shifted so that the minimum is 0.
#'
#' Value-added and final-demand sectors are not nodes and do not receive a
#' trophic level, but they are **not** ignored: because the table balances, an
#' industry's imbalance equals `(final demand + exports) - (value added +
#' imports)`. Primary, value-added-heavy industries therefore receive low
#' trophic levels and final-demand-facing industries receive high ones (the
#' level increases with distance from primary inputs).
#'
#' `io_trophic_incoherence()` returns the trophic incoherence
#'
#' \deqn{F_0 = \frac{\sum_{ij} w_{ij} (h_j - h_i - 1)^2}{\sum_{ij} w_{ij}},}
#'
#' a scalar in `[0, 1]`-ish range that is 0 when the network is perfectly
#' coherent (every edge spans exactly one level) and larger when production
#' contains more cycles and feedback.
#'
#' @param data An `econ_io_table` object.
#'
#' @return
#' * `io_trophic_level()`: a `dibble` with one trophic level per industry.
#' * `io_trophic_incoherence()`: a scalar numeric.
#'
#' @references
#' MacKay, R. S., Johnson, S. and Sansom, B. (2020). How directed is a directed
#' network? *Royal Society Open Science*, 7(9), 201138.
#'
#' @examples
#' \dontrun{
#' io_trophic_level(iotable)
#' io_trophic_incoherence(iotable)
#' }
#'
#' @name io_trophic
NULL

#' @rdname io_trophic
#' @export
io_trophic_level <- function(data) {
  trophic <- io_trophic(data)
  dibble::dibble(trophic$level, .dim_names = list(industry = trophic$dim_name))
}

#' @rdname io_trophic
#' @export
io_trophic_incoherence <- function(data) {
  trophic <- io_trophic(data)
  weight <- trophic$weight
  level <- trophic$level
  level_diff <- outer(level, level, \(from, to) to - from)
  sum(weight * (level_diff - 1)^2) / sum(weight)
}

io_trophic <- function(data) {
  data <- io_table_to_noncompetitive_import(data) |>
    io_check_axes()

  inter_industry <- io_inter_industry(data)
  dim_name <- dimnames(inter_industry)$input

  # In- and out-weight of each industry node, via dibble reductions.
  in_weight <- as.vector(dibble::apply(inter_industry, "output", sum))
  out_weight <- as.vector(dibble::apply(inter_industry, "input", sum))

  # The symmetric adjacency `weight + t(weight)` and the Laplacian solve act on
  # a single node set, which dibble's distinct input/output axes cannot
  # represent, so this core uses a plain matrix. The input and output industry
  # axes are identical, so the matrix and the weight vectors share one ordering.
  weight <- as.matrix(inter_industry)
  laplacian <- diag(in_weight + out_weight) - (weight + t(weight))
  imbalance <- in_weight - out_weight

  level <- as.numeric(MASS::ginv(laplacian) %*% imbalance)
  level <- level - min(level)

  list(dim_name = dim_name, weight = weight, level = level)
}
