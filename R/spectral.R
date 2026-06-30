#' Laplacian spectral embedding
#'
#' Embeds the industries of the domestic production network into a
#' low-dimensional space by minimizing the graph Dirichlet energy
#'
#' \deqn{\sum_{ij} w_{ij} (h_i - h_j)^2 = h^\top \Lambda h,}
#'
#' subject to `h` being orthonormal and orthogonal to the constant vector. The
#' minimizers are the eigenvectors of the symmetric graph Laplacian
#' \eqn{\Lambda = \mathrm{diag}(u) - (W + W^\top)} associated with the smallest
#' non-zero eigenvalues, where `u` is the total weight (in plus out) of each
#' node. With `dims = 1` this is the **Fiedler vector**.
#'
#' Like [io_trophic_level()], the table is first converted to a noncompetitive
#' import type table with [io_table_to_noncompetitive_import()] so that the
#' industry-by-industry block holds purely domestic intermediate transactions,
#' which are treated as the edge weights `w[i, j]`.
#'
#' The embedding is **undirected**: it uses only the symmetrized weights
#' `W + Wt` and therefore captures industry clusters but not the direction of
#' production. It is complementary to the directed [io_trophic_level()]; pairing
#' the two gives a map of the production network with a clustering coordinate
#' (this function) and a flow level ([io_trophic_level()]).
#'
#' The sign of each eigenvector is arbitrary, so it is fixed deterministically
#' by orienting each component to make its largest-magnitude entry positive.
#'
#' @param data An `econ_io_table` object.
#' @param dims A scalar positive integer giving the number of embedding
#' dimensions (the number of smallest non-zero Laplacian eigenvectors to
#' return). By default, `1`, i.e. the Fiedler vector.
#'
#' @return A `dibble`. With `dims = 1`, one value per industry. With
#' `dims > 1`, an `industry` by `component` array.
#'
#' @seealso [io_trophic_level()]
#'
#' @examples
#' \dontrun{
#' io_spectral_embedding(iotable)
#' io_spectral_embedding(iotable, dims = 2)
#' }
#'
#' @export
io_spectral_embedding <- function(data, dims = 1) {
  network <- io_industry_network(data)
  n_industry <- ncol(network$laplacian)

  if (!rlang::is_scalar_integerish(dims) || dims < 1 || dims > n_industry - 1) {
    cli::cli_abort(
      "{.arg dims} must be a scalar integer between 1 and {n_industry - 1}."
    )
  }

  decomposition <- eigen(network$laplacian, symmetric = TRUE)
  # Eigenvalues are returned in decreasing order, so the trivial constant mode
  # (eigenvalue ~ 0) is last. Take the `dims` smallest non-zero eigenvectors,
  # i.e. columns (n_industry - 1), (n_industry - 2), ..., the Fiedler vector
  # first.
  columns <- seq.int(n_industry - 1L, n_industry - dims)
  eigenvectors <- decomposition$vectors[, columns, drop = FALSE]

  # The sign of an eigenvector is arbitrary; orient each so that its largest
  # magnitude entry is positive, for reproducible output.
  signs <- apply(
    eigenvectors,
    2,
    function(column) sign(column[which.max(abs(column))])
  )
  eigenvectors <- sweep(eigenvectors, 2, signs, `*`)

  if (dims == 1) {
    dibble::dibble(
      as.vector(eigenvectors),
      .dim_names = list(industry = network$dim_name)
    )
  } else {
    dibble::dibble(
      as.vector(eigenvectors),
      .dim_names = list(industry = network$dim_name, component = seq_len(dims))
    )
  }
}
