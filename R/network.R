io_network <- function(
  data,
  paths = c("direct", "all"),
  call = rlang::caller_env()
) {
  paths <- rlang::arg_match(paths, error_call = call)

  if (!inherits(data, "io_table_noncompetitive_import")) {
    cli::cli_inform(
      "Converting to a noncompetitive import type table with {.fn io_table_to_noncompetitive_import}."
    )
  }
  data <- io_table_to_noncompetitive_import(data) |>
    io_check_axes()
  inter_industry <- io_inter_industry(data)
  dim_name <- dimnames(inter_industry)$input
  weight <- as.matrix(inter_industry)

  n_negative <- sum(weight < 0)
  if (n_negative > 0) {
    cli::cli_abort(
      c(
        "Found {n_negative} negative domestic intermediate transaction{?s}.",
        "i" = "For negatives from byproduct treatment such as the Stone method, use {.fn io_table_transfer_byproduct} to convert them to the transfer method before building the industry network.",
        "i" = "For negatives from imports that exceed regional demand (an import coefficient above 1), e.g. re-exports, use {.fn io_table_remove_reexport} before converting to a noncompetitive import type table."
      ),
      call = call
    )
  }

  # Industries without transactions with other industries (e.g. with zero
  # output in a regional table) don't affect the results of the others, and
  # have no results of their own, so they are left out of the network.
  off_diagonal <- weight
  diag(off_diagonal) <- 0
  connected <- rowSums(off_diagonal) + colSums(off_diagonal) > 0
  if (!any(connected)) {
    cli::cli_abort(
      "No industries have domestic intermediate transactions with other industries.",
      call = call
    )
  }
  if (!all(connected)) {
    isolated <- io_axis_label(dim_name)[!connected]
    cli::cli_inform(
      "Returning {.code NA} for {length(isolated)} industr{?y/ies} without domestic intermediate transactions with other industries: {.val {isolated}}."
    )
  }

  connected_weight <- weight[connected, connected, drop = FALSE]
  connected_dim_name <- vctrs::vec_slice(dim_name, connected)
  network <- switch(
    paths,
    direct = io_network_direct(connected_weight, connected_dim_name),
    all = io_network_all(
      connected_weight,
      as.vector(io_total_input(data))[connected],
      connected_dim_name,
      call = call
    )
  )
  network$all_dim_name <- dim_name
  network$connected <- connected
  network
}

# Expands `value`, with one row per connected industry of `network`, to all
# industries, with `NA` for the industries left out of the network.
io_network_expand <- function(value, network) {
  value <- as.matrix(value)
  out <- matrix(NA_real_, length(network$connected), ncol(value))
  out[network$connected, ] <- value
  out
}

# Direct transactions span paths of length 1.
io_network_direct <- function(weight, dim_name) {
  io_network_weight(weight, path_length = 1, dim_name = dim_name)
}

# With `Z_n = A^n diag(x)` the transactions along paths of length `n`, the edge
# weights are `W = sum_n Z_n = (L - I) diag(x)` and the path lengths are the
# average propagation lengths `sum_n n Z_n / W = L (L - I) / (L - I)`
# (elementwise), where `L` is the Leontief inverse.
io_network_all <- function(
  weight,
  total_input,
  dim_name,
  call = rlang::caller_env()
) {
  eye <- diag(nrow(weight))
  input_coef <- sweep(weight, 2, total_input, "/")
  input_coef[, total_input == 0] <- 0

  # The sums over paths converge only if the spectral radius of `A` is below 1.
  spectral_radius <- max(Mod(eigen(input_coef, only.values = TRUE)$values))
  if (spectral_radius >= 1) {
    cli::cli_abort(
      c(
        "The sums of transactions over all paths do not converge.",
        "x" = "The spectral radius of the domestic input coefficient matrix is {format(spectral_radius, digits = 3)}, not less than 1.",
        "i" = "It is less than 1 when every industry has positive value added plus imports."
      ),
      call = call
    )
  }
  leontief <- solve(eye - input_coef)

  path_sum <- leontief - eye
  path_length <- leontief %*% path_sum / path_sum
  # Pairs without any path have no weight, so their length is irrelevant.
  path_length[path_sum == 0] <- 0
  io_network_weight(
    sweep(path_sum, 2, total_input, "*"),
    path_length = path_length,
    dim_name = dim_name
  )
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

io_abort_disconnected <- function(network, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "The domestic production network must be connected.",
      "i" = "Results for separate parts of the network are not comparable.",
      "i" = "Remove or merge industries that are not linked to the others."
    ),
    call = call
  )
}
