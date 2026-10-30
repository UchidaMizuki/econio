#' Laplacian spectral embedding
#'
#' `r lifecycle::badge("experimental")`
#'
#' Embeds the industries of the domestic production network into a
#' low-dimensional space by minimizing the graph Dirichlet energy
#'
#' \deqn{\sum_{ij} w_{ij} (h_i - h_j)^2 = h^\top \Lambda h,}
#'
#' where \eqn{\Lambda = \mathrm{diag}(u) - (W + W^\top)} is the symmetric graph
#' Laplacian and `u` is the total weight (in plus out) of each node.
#'
#' By default (`normalized = TRUE`), the energy is minimized subject to
#' \eqn{\sum_i u_i h_i = 0} and \eqn{\sum_i u_i h_i^2 = 1}, i.e. the
#' constraints are weighted by `u`. The minimizers solve the generalized
#' eigenproblem \eqn{\Lambda h = \lambda \, \mathrm{diag}(u) \, h}, whose
#' eigenvalues lie in `[0, 2]`. This is the normalized Laplacian used by
#' Laplacian eigenmaps (Belkin and Niyogi, 2003) and normalized cuts (Shi and
#' Malik, 2000). It prevents the embedding from concentrating on industries
#' with little intermediate trade. `u` is the same weight that centers
#' [io_trophic_level()].
#'
#' With `normalized = FALSE`, `h` is instead constrained to be orthonormal and
#' orthogonal to the constant vector, and the minimizers are the eigenvectors
#' of \eqn{\Lambda} itself.
#'
#' In both cases the eigenvectors associated with the smallest non-zero
#' eigenvalues are returned. With `dims = 1` this is the **Fiedler vector**.
#' A disconnected network has one zero eigenvalue per component, which makes
#' the embedding arbitrary, so the function aborts if the network (treating
#' each transaction as an undirected link) is not connected.
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
#' @param normalized A scalar logical. If `TRUE` (the default), use the
#' normalized Laplacian, weighting the constraints by the total weight of each
#' industry. If `FALSE`, use the unnormalized Laplacian.
#' @param paths A scalar character. `"direct"` (the default) weights the network by the
#' direct domestic intermediate transactions. `"all"` (experimental) weights it
#' by all direct and indirect transactions, \eqn{(L - I) \mathrm{diag}(x)};
#' see the "Direct and indirect paths" section of [io_trophic_level()].
#'
#' @return A `dibble`. With `dims = 1`, one value per industry. With
#' `dims > 1`, an `industry` by `component` array.
#'
#' @references
#' Belkin, M. and Niyogi, P. (2003). Laplacian eigenmaps for dimensionality
#' reduction and data representation. *Neural Computation*, 15(6), 1373-1396.
#'
#' Shi, J. and Malik, J. (2000). Normalized cuts and image segmentation. *IEEE
#' Transactions on Pattern Analysis and Machine Intelligence*, 22(8), 888-905.
#'
#' @seealso [io_trophic_level()]
#'
#' @examples
#' \dontrun{
#' io_spectral_embedding(iotable)
#' io_spectral_embedding(iotable, dims = 2)
#' io_spectral_embedding(iotable, normalized = FALSE)
#' }
#'
#' @export
io_spectral_embedding <- function(
  data,
  dims = 1,
  normalized = TRUE,
  paths = c("direct", "all")
) {
  rlang::check_bool(normalized)

  network <- io_network(data, paths)
  n_industry <- ncol(network$laplacian)

  rlang::check_number_whole(dims, min = 1, max = n_industry - 1)

  total_weight <- network$total_weight
  if (!normalized) {
    scale <- rep(1, n_industry)
  } else if (any(total_weight == 0)) {
    io_abort_disconnected(network, call = rlang::current_env())
  } else {
    scale <- sqrt(total_weight)
  }
  # With `normalized = TRUE`, solve the generalized problem
  # `Λ h = λ diag(u) h` via the symmetric matrix
  # `diag(u)^(-1/2) Λ diag(u)^(-1/2)`, then back-transform.
  decomposition <- eigen(
    network$laplacian / (scale %o% scale),
    symmetric = TRUE
  )

  # Eigenvalues are returned in decreasing order, so the trivial mode
  # (eigenvalue ~ 0) is last. A disconnected network has one zero eigenvalue
  # per component, so the second smallest must be numerically non-zero.
  eigenvalues <- decomposition$values
  if (
    eigenvalues[[n_industry - 1L]] <=
      n_industry * .Machine$double.eps * eigenvalues[[1]]
  ) {
    io_abort_disconnected(network, call = rlang::current_env())
  }

  # Take the `dims` smallest non-zero eigenvectors, i.e. columns
  # (n_industry - 1), (n_industry - 2), ..., the Fiedler vector first.
  columns <- seq.int(n_industry - 1L, n_industry - dims)
  eigenvectors <- decomposition$vectors[, columns, drop = FALSE] / scale

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
      .dim_names = list(
        industry = network$dim_name,
        component = seq_len(dims)
      )
    )
  }
}
