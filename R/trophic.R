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
#' levels of the other industries unchanged. The system is solved by fixing the
#' level of one industry and dropping its (redundant) equation, which leaves a
#' positive definite system when the network is connected. Trophic levels in separate parts of a disconnected
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
  trophic <- io_trophic(io_laplacian(data))
  dibble::dibble(trophic$level, .dim_names = list(industry = trophic$dim_name))
}

#' @rdname io_trophic
#' @export
io_trophic_incoherence <- function(data) {
  trophic <- io_trophic(io_laplacian(data))
  weight <- trophic$weight
  level <- trophic$level
  level_diff <- outer(level, level, \(from, to) to - from)
  sum(weight * (level_diff - 1)^2) / sum(weight)
}

io_trophic <- function(laplacian, call = rlang::caller_env()) {
  imbalance <- laplacian$in_weight - laplacian$out_weight
  level <- numeric(length(imbalance))

  # The Laplacian is singular (its rows sum to 0), so the levels are only
  # defined up to an additive constant. Fixing the level of one industry to 0
  # and dropping its equation, which is redundant because the equations sum to
  # 0, leaves a positive definite system for a connected network. Fixing the
  # industry with the largest total weight keeps the system well conditioned.
  # For a disconnected network the remaining system is singular. The levels
  # are then centered to a weighted mean of 0.
  if (length(level) > 1L) {
    total_weight <- laplacian$in_weight + laplacian$out_weight
    fixed <- which.max(total_weight)
    level[-fixed] <- rlang::try_fetch(
      solve(
        laplacian$laplacian[-fixed, -fixed, drop = FALSE],
        imbalance[-fixed]
      ),
      error = function(cnd) {
        cli::cli_abort(
          c(
            "Can't compute trophic levels because the domestic production network is not connected.",
            "i" = "Trophic levels in separate components are not comparable.",
            "i" = "Remove or merge industries that have no transactions with the others."
          ),
          call = call
        )
      }
    )
    level <- level - sum(total_weight * level) / sum(total_weight)
  }

  list(dim_name = laplacian$dim_name, weight = laplacian$weight, level = level)
}
