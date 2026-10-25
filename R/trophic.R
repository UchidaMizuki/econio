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
#' determined up to an additive constant; they are shifted so that the minimum
#' is 0. The system is solved by fixing the level of one industry and dropping
#' its (redundant) equation, which leaves a positive definite system when the
#' network is connected. Trophic levels in separate parts of a disconnected
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

io_trophic <- function(data, call = rlang::caller_env()) {
  laplacian <- io_laplacian(data)
  io_check_connected(laplacian, call = call)
  level <- io_trophic_solve(laplacian)
  list(dim_name = laplacian$dim_name, weight = laplacian$weight, level = level)
}

io_trophic_solve <- function(laplacian) {
  imbalance <- laplacian$in_weight - laplacian$out_weight
  level <- numeric(length(imbalance))
  if (length(level) == 1L) {
    return(level)
  }

  # The Laplacian is singular (its rows sum to 0), so the levels are only
  # defined up to an additive constant. Fixing the level of one industry to 0
  # and dropping its equation, which is redundant because the equations sum to
  # 0, leaves a positive definite system for a connected network. Fixing the
  # industry with the largest total weight keeps the system well conditioned.
  total_weight <- laplacian$in_weight + laplacian$out_weight
  fixed <- which.max(total_weight)
  level[-fixed] <- solve(
    laplacian$laplacian[-fixed, -fixed, drop = FALSE],
    imbalance[-fixed]
  )
  level - min(level)
}

io_check_connected <- function(laplacian, call = rlang::caller_env()) {
  component <- io_network_component(laplacian$weight)
  n_component <- max(component)
  if (n_component == 1L) {
    return(invisible(laplacian))
  }

  component_size <- tabulate(component)
  isolated <- component_size[component] == 1L
  industry <- io_industry_label(laplacian$dim_name)
  cli::cli_abort(
    c(
      "The domestic production network must be connected, but it has {n_component} separate components.",
      "i" = "Trophic levels in separate components are not comparable.",
      "i" = if (any(isolated)) {
        "{sum(isolated)} industr{?y has/ies have} no transactions with other industries: {.val {industry[isolated]}}."
      },
      "i" = if (any(!isolated)) {
        "Component sizes: {component_size}."
      },
      "i" = "Remove or merge disconnected industries before computing trophic levels."
    ),
    call = call
  )
}

io_network_component <- function(weight) {
  adjacency <- weight + t(weight) > 0
  n_industry <- nrow(adjacency)
  component <- integer(n_industry)
  id <- 0L
  for (start in seq_len(n_industry)) {
    if (component[[start]] != 0L) {
      next
    }
    id <- id + 1L
    component[[start]] <- id
    frontier <- start
    while (length(frontier) > 0) {
      neighbor <- which(
        colSums(adjacency[frontier, , drop = FALSE]) > 0 & component == 0L
      )
      component[neighbor] <- id
      frontier <- neighbor
    }
  }
  component
}

io_industry_label <- function(dim_name) {
  label <- purrr::map(dim_name, function(x) {
    if (inherits(x, "econ_io_sector")) io_sector_name(x) else as.character(x)
  })
  do.call(paste, c(unname(label), sep = "/"))
}
