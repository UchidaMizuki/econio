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
        "Found {n_negative} negative domestic intermediate transaction{?s} (e.g. from byproduct treatment such as the Stone method).",
        "i" = "Use {.fn io_table_to_byproduct_transfer} to convert negative entries to the transfer method before building the industry network."
      )
    )
  }

  switch(
    paths,
    direct = io_network_direct(weight, dim_name),
    all = io_network_all(
      weight,
      as.vector(io_total_input(data)),
      dim_name,
      call = call
    )
  )
}

# Direct transactions span paths of length 1.
io_network_direct <- function(weight, dim_name) {
  io_network_weight(
    weight,
    weighted_length = weight,
    weighted_squared_length = weight,
    dim_name = dim_name
  )
}

# With `Z_n = A^n diag(x)` the transactions along paths of length `n`, the
# edge weights are `sum_n Z_n`, and `sum_n n Z_n` and `sum_n n^2 Z_n` give the
# first two moments of the path length of each edge. The sums over `n >= 1` of
# `A^n`, `n A^n` and `n^2 A^n` are `L - I`, `L (L - I)` and
# `(2 L - I) L (L - I)`, where `L` is the Leontief inverse.
io_network_all <- function(
  weight,
  output,
  dim_name,
  call = rlang::caller_env()
) {
  identity <- diag(nrow(weight))
  input_coef <- sweep(weight, 2, output, "/")
  input_coef[, output == 0] <- 0

  # The sums over paths converge only if the spectral radius of `A` is below 1.
  radius <- max(Mod(eigen(input_coef, only.values = TRUE)$values))
  if (radius >= 1) {
    cli::cli_abort(
      c(
        "The sums of transactions over all paths do not converge.",
        "x" = "The spectral radius of the domestic input coefficient matrix is {format(radius, digits = 3)}, not less than 1.",
        "i" = "It is less than 1 when every industry has positive value added plus imports."
      ),
      call = call
    )
  }
  leontief <- solve(identity - input_coef)

  path_sum <- leontief - identity
  path_length_sum <- leontief %*% path_sum
  path_squared_length_sum <- (2 * leontief - identity) %*% path_length_sum
  io_network_weight(
    sweep(path_sum, 2, output, "*"),
    weighted_length = sweep(path_length_sum, 2, output, "*"),
    weighted_squared_length = sweep(path_squared_length_sum, 2, output, "*"),
    dim_name = dim_name
  )
}

# `weighted_length` and `weighted_squared_length` are the (squared) path
# lengths weighted by the transactions along each path, summed over paths.
# `total_weight` is the weight in plus out of each industry.
io_network_weight <- function(
  weight,
  weighted_length,
  weighted_squared_length,
  dim_name
) {
  total_weight <- colSums(weight) + rowSums(weight)
  laplacian <- diag(total_weight, nrow = length(total_weight)) -
    (weight + t(weight))
  list(
    weight = weight,
    weighted_length = weighted_length,
    weighted_squared_length = weighted_squared_length,
    dim_name = dim_name,
    total_weight = total_weight,
    laplacian = laplacian
  )
}

io_abort_disconnected <- function(network, call = rlang::caller_env()) {
  industry <- io_axis_label(network$dim_name)[network$total_weight == 0]
  cli::cli_abort(
    c(
      "The domestic production network must be connected.",
      "x" = if (length(industry) > 0) {
        "{length(industry)} industr{?y has/ies have} no domestic intermediate transactions: {.val {industry}}."
      },
      "i" = "Results for separate parts of the network are not comparable.",
      "i" = "Remove or merge industries that are not linked to the others."
    ),
    call = call
  )
}
