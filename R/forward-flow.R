#' Forward flow of a production network
#'
#' Given a per-industry ordering (e.g. from [io_trophic_level()]), zero out
#' every domestic intermediate transaction that does not run strictly from a
#' lower-ordered (more upstream) industry to a higher-ordered (more
#' downstream) one.
#'
#' The table is first converted to a noncompetitive import type table with
#' [io_table_to_noncompetitive_import()], as in [io_trophic_level()], so that
#' the industry-by-industry block `w[i, j]` holds purely domestic intermediate
#' transactions. An entry `w[i, j]` is kept only if `order[i] < order[j]`; it
#' is zeroed otherwise, which also removes self-loops (`i == j`) and
#' transactions between industries at the same level.
#'
#' `io_forward_flow()` does **not** search for an ordering that minimizes the
#' zeroed-out (backward) flow; it only applies the ordering supplied in
#' `order`. Finding an optimal ordering is a combinatorial optimization (see
#' Simpson and Tsukui, 1965) that this function does not perform.
#'
#' @param data An `econ_io_table` object.
#' @param order A `dibble` with a single `industry` dimension and one numeric
#' value per industry, such as the output of [io_trophic_level()],
#' [io_upstreamness()] or [io_downstreamness()].
#'
#' @return A `dibble` of domestic intermediate transactions with dimensions
#' `c("input", "output")`, with backward and same-level transactions set to
#' `0`.
#'
#' @references
#' Simpson, D. and Tsukui, J. (1965). The fundamental structure of
#' input-output tables: An international comparison. *The Review of Economics
#' and Statistics*, 47(4), 434-446.
#'
#' @examples
#' \dontrun{
#' io_forward_flow(iotable, order = io_trophic_level(iotable))
#' }
#'
#' @export
io_forward_flow <- function(data, order) {
  network <- io_industry_network(data)
  order_values <- io_industry_order_values(order, network$dim_name)

  weight <- network$weight
  weight[!outer(order_values, order_values, `<`)] <- 0

  dibble::dibble(
    as.vector(weight),
    .dim_names = list(input = network$dim_name, output = network$dim_name)
  )
}

io_industry_order_values <- function(order, dim_name) {
  order_dim_names <- dimnames(order)
  if (!identical(names(order_dim_names), "industry")) {
    cli::cli_abort(
      "{.arg order} must be a dibble with a single {.field industry} dimension."
    )
  }

  order_industry <- order_dim_names$industry
  match_index <- vctrs::vec_match(dim_name, order_industry)
  if (anyNA(match_index)) {
    cli::cli_abort("{.arg order} is missing values for some industries.")
  }

  as.vector(as.matrix(order))[match_index]
}
