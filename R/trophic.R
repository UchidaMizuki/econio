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
#' @section Direct and indirect paths:
#' `r lifecycle::badge("experimental")`
#'
#' With `paths = "all"`, indirect transactions are included as well, using the
#' extension of MacKay, Johnson and Sansom (2020, section 7) to arbitrary
#' target level differences \eqn{\tau_{ij}}:
#'
#' \deqn{F_0 = \min_h \frac{\sum_{ij} w_{ij} (h_j - h_i - \tau_{ij})^2}
#' {\sum_{ij} w_{ij} \tau_{ij}^2}.}
#'
#' Let \eqn{Z_n = A^n \mathrm{diag}(x)} be the transactions along paths of
#' length `n`, where `A` is the domestic input coefficient matrix and `x` the
#' domestic output. The edge weights are all direct and indirect transactions,
#' \eqn{W = \sum_{n \ge 1} Z_n = (L - I) \mathrm{diag}(x)}, where `L` is the
#' Leontief inverse, and the targets are the average propagation lengths
#' \eqn{\tau_{ij} = \sum_n n (Z_n)_{ij} / W_{ij} = [L (L - I)]_{ij} / (L -
#' I)_{ij}} (Dietzenbacher, Romero Luna and Bosma, 2005). With `paths =
#' "direct"`, \eqn{W} is the direct transactions and \eqn{\tau_{ij} = 1}, which
#' gives the definitions above. The levels also minimize the sum over paths
#' \eqn{\sum_n \sum_{ij} (Z_n)_{ij} (h_j - h_i - n)^2}, i.e. each path of length
#' `n` targets a level difference of `n`. The problem is equivalent to the
#' Helmholtz-Hodge decomposition of the flows \eqn{W_{ij} \tau_{ij}} with
#' conductivities \eqn{W_{ij}} (Kichikawa et al., 2019; MacKay, Johnson and
#' Sansom, 2020, appendix A.14).
#'
#' Merging industries with equal levels leaves the other levels unchanged only
#' if `W` and \eqn{W_{ij} \tau_{ij}} are computed from the disaggregated table
#' and then summed: the Leontief inverse of an aggregated table is not the
#' aggregate of the disaggregated one.
#'
#' The sums over paths converge only if the spectral radius of `A` is less
#' than 1, which holds when every industry has positive value added plus
#' imports. Otherwise, the functions abort.
#'
#' @param data An `econ_io_table` object.
#' @param paths A scalar character. `"direct"` (the default) uses the direct
#' domestic intermediate transactions, each targeting a level difference of 1.
#' `"all"` (experimental) uses all direct and indirect transactions, each
#' targeting its average propagation length.
#'
#' @return
#' * `io_trophic_level()`: a `dibble` with one trophic level per industry.
#' * `io_trophic_incoherence()`: a scalar numeric.
#'
#' @references
#' Dietzenbacher, E., Romero Luna, I. and Bosma, N. S. (2005). Using average
#' propagation lengths to identify production chains in the Andalusian economy.
#' *Estudios de Economía Aplicada*, 23(2), 405-422.
#'
#' Kichikawa, Y., Iyetomi, H., Iino, T. and Inoue, H. (2019). Community
#' structure based on circular flow in a large-scale transaction network.
#' *Applied Network Science*, 4, 92.
#'
#' MacKay, R. S., Johnson, S. and Sansom, B. (2020). How directed is a directed
#' network? *Royal Society Open Science*, 7(9), 201138.
#'
#' @examples
#' \dontrun{
#' io_trophic_level(iotable)
#' io_trophic_incoherence(iotable)
#' io_trophic_level(iotable, paths = "all")
#' }
#'
#' @name io_trophic
NULL

#' @rdname io_trophic
#' @export
io_trophic_level <- function(data, paths = c("direct", "all")) {
  trophic <- io_trophic(io_network(data, paths))
  dibble::dibble(trophic$level, .dim_names = list(industry = trophic$dim_name))
}

#' @rdname io_trophic
#' @export
io_trophic_incoherence <- function(data, paths = c("direct", "all")) {
  network <- io_network(data, paths)
  level <- io_trophic(network)$level
  level_diff <- outer(level, level, \(from, to) to - from)
  sum(network$weight * (level_diff - network$path_length)^2) /
    sum(network$weight * network$path_length^2)
}

io_trophic <- function(network, call = rlang::caller_env()) {
  # `v` in `Λ h = v`, which is in minus out for direct transactions.
  length_weight <- network$weight * network$path_length
  imbalance <- colSums(length_weight) - rowSums(length_weight)
  total_weight <- network$total_weight
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
    level = unname(solution[seq_len(n_industry)])
  )
}
