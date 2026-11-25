io_network <- function(
  data,
  paths = c("direct", "all"),
  call = rlang::caller_env()
) {
  paths <- rlang::arg_match(paths, error_call = call)

  data <- io_table_to_noncompetitive_import(data) |>
    io_check_axes()
  inter_industry <- io_inter_industry(data)

  n_negative <- sum(inter_industry < 0)
  if (n_negative > 0) {
    cli::cli_abort(
      c(
        "Can't build the industry network from negative domestic intermediate transactions.",
        "x" = "Found {n_negative} negative transaction{?s}."
      ),
      call = call
    )
  }

  path <- switch(
    paths,
    direct = io_paths_direct(data),
    all = io_paths_all(data, call = call)
  )
  io_network_weight(
    path$weight,
    path_length = path$path_length,
    dim_name = dimnames(inter_industry)$input
  )
}

# Direct transactions span paths of length 1.
io_paths_direct <- function(data) {
  list(weight = as.matrix(io_inter_industry(data)), path_length = 1)
}

# With `Z_n = A^n diag(x) = A^(n - 1) Z` the transactions along paths of length
# `n`, the edge weights are `W = sum_n Z_n = L Z` and the path lengths are the
# average propagation lengths `sum_n n Z_n / W = L W / W` (elementwise), where
# `L` is the Leontief inverse.
io_paths_all <- function(data, call = rlang::caller_env()) {
  # `L = I + A + A^2 + ...` converges, and `L` is then nonnegative, only if the
  # spectral radius of `A` is below 1. Otherwise `solve()` can still give an
  # inverse, but with meaningless, e.g. negative, weights.
  eigenvalues <- eigen(
    as.matrix(io_input_coef(data)),
    only.values = TRUE
  )$values
  spectral_radius <- max(Mod(eigenvalues))
  if (spectral_radius >= 1) {
    cli::cli_abort(
      c(
        "The sums of transactions over all paths do not converge.",
        "x" = "The spectral radius of the domestic input coefficient matrix is {format_number(spectral_radius, compare_to = 1)}, not less than 1.",
        "i" = "It is less than 1 when every industry has positive value added plus imports."
      ),
      call = call
    )
  }

  # The products below chain transactions between one set of industries, which
  # the separate `input` and `output` axes of the table can't express, so they
  # use plain matrices.
  leontief <- as.matrix(io_leontief_inverse(data))
  path_weight <- leontief %*% as.matrix(io_inter_industry(data))
  path_length <- leontief %*% path_weight / path_weight
  # Pairs without any path have no weight, so their length is irrelevant.
  path_length[path_weight == 0] <- 0
  list(weight = path_weight, path_length = path_length)
}

# `path_length` is the target level difference of each edge (MacKay, Johnson
# and Sansom, 2020, section 7). `total_weight` is the weight in plus out of
# each industry.
io_network_weight <- function(weight, path_length, dim_name) {
  total_weight <- colSums(weight) + rowSums(weight)
  laplacian <- diag(total_weight, nrow = length(total_weight)) -
    (weight + t(weight))
  list(
    weight = weight,
    path_length = path_length,
    dim_name = dim_name,
    total_weight = total_weight,
    laplacian = laplacian
  )
}

# Eigendecomposition of the Laplacian `Λ`, or with `normalized = TRUE` of the
# normalized Laplacian `D^(-1/2) Λ D^(-1/2)` with `D = diag(u)`. `D^(-1/2)` is
# the pseudo-inverse of `D^(1/2)`, i.e. 0 for industries with `u = 0`, and
# `scale` is its diagonal. `zero` marks the eigenvalues that are 0 up to
# rounding error, one per connected part of the network, with a tolerance of
# `sqrt(eps)` relative to the largest eigenvalue, as in `MASS::ginv()`:
# rounding can leave them a few times `eps` above 0.
io_laplacian_eigen <- function(network, normalized = TRUE) {
  total_weight <- network$total_weight
  scale <- if (normalized) {
    ifelse(total_weight > 0, 1 / sqrt(total_weight), 0)
  } else {
    rep(1, length(total_weight))
  }
  decomposition <- eigen(
    network$laplacian * (scale %o% scale),
    symmetric = TRUE
  )
  values <- decomposition$values
  list(
    values = values,
    vectors = decomposition$vectors,
    scale = scale,
    zero = values <= .Machine$double.eps^0.5 * max(values)
  )
}
