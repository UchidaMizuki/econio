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
#' imbalance (in minus out). The Laplacian is singular, so the levels are only
#' determined up to an additive constant; they are centered so that their
#' weighted mean is 0, \eqn{\sum_i u_i h_i = 0}. A negative level means an
#' industry sits upstream of the weighted average, and a positive level means
#' it sits downstream. Because `u` includes transactions within an industry,
#' merging industries with equal levels leaves `u`'s total and therefore the
#' levels of the other industries unchanged. The levels are obtained by solving
#' the system together with this constraint, which has a unique solution when
#' the network is connected. Trophic levels in separate parts of a disconnected
#' network are not comparable, so both functions abort if the network
#' (treating each transaction as an undirected link) is not connected, e.g.
#' when an industry has no transactions with any other industry.
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
  trophic <- io_trophic(io_network(data))
  dibble::dibble(trophic$level, .dim_names = list(industry = trophic$dim_name))
}

#' @rdname io_trophic
#' @export
io_trophic_incoherence <- function(data) {
  trophic <- io_trophic(io_network(data))
  weight <- trophic$weight
  level <- trophic$level
  level_diff <- outer(level, level, \(from, to) to - from)
  sum(weight * (level_diff - 1)^2) / sum(weight)
}

io_trophic <- function(network, call = rlang::caller_env()) {
  imbalance <- network$in_weight - network$out_weight
  total_weight <- network$in_weight + network$out_weight
  n_industry <- length(imbalance)

  # The Laplacian is singular (its rows sum to 0), so the levels are only
  # defined up to an additive constant, which is fixed by `sum(u * h) = 0`.
  # Solve `Λ h = v` subject to this constraint as one bordered (KKT) system.
  # Its multiplier is 0 because the imbalance sums to 0, and the system is
  # singular exactly when the network is disconnected.
  system <- rbind(
    cbind(network$laplacian, total_weight),
    c(total_weight, 0)
  )
  solution <- rlang::try_fetch(
    solve(system, c(imbalance, 0)),
    error = function(cnd) io_abort_disconnected(network, call = call)
  )

  list(
    dim_name = network$dim_name,
    weight = network$weight,
    level = unname(solution[seq_len(n_industry)])
  )
}
