io_laplacian <- function(
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
    direct = io_laplacian_direct(weight, dim_name),
    all = io_laplacian_all(weight, as.vector(io_total_input(data)), dim_name)
  )
}

# Direct transactions span paths of length 1.
io_laplacian_direct <- function(weight, dim_name) {
  io_laplacian_weight(
    weight,
    dim_name,
    length_weight = weight,
    squared_length_weight = weight
  )
}

# With `Z_n = A^n diag(x)` the transactions along paths of length `n`, the
# edge weights are `sum_n Z_n`, and `sum_n n Z_n` and `sum_n n^2 Z_n` give the
# first two moments of the path length of each edge. The sums over `n >= 1` of
# `A^n`, `n A^n` and `n^2 A^n` are `L - I`, `L (L - I)` and
# `(2 L - I) L (L - I)`, where `L` is the Leontief inverse.
io_laplacian_all <- function(weight, output, dim_name) {
  identity <- diag(nrow(weight))
  input_coef <- sweep(weight, 2, output, "/")
  input_coef[, output == 0] <- 0
  leontief <- solve(identity - input_coef)

  path_sum <- leontief - identity
  path_length_sum <- leontief %*% path_sum
  path_squared_length_sum <- (2 * leontief - identity) %*% path_length_sum
  io_laplacian_weight(
    sweep(path_sum, 2, output, "*"),
    dim_name,
    length_weight = sweep(path_length_sum, 2, output, "*"),
    squared_length_weight = sweep(path_squared_length_sum, 2, output, "*")
  )
}

# `length_weight` and `squared_length_weight` are the edge weights multiplied
# by the (squared) path length, summed over paths.
io_laplacian_weight <- function(
  weight,
  dim_name,
  length_weight,
  squared_length_weight
) {
  in_weight <- colSums(weight)
  out_weight <- rowSums(weight)
  total_weight <- in_weight + out_weight
  laplacian <- diag(total_weight, nrow = length(total_weight)) -
    (weight + t(weight))
  list(
    dim_name = dim_name,
    weight = weight,
    length_weight = length_weight,
    squared_length_weight = squared_length_weight,
    in_weight = in_weight,
    out_weight = out_weight,
    laplacian = laplacian
  )
}

io_abort_disconnected <- function(laplacian, call = rlang::caller_env()) {
  total_weight <- laplacian$in_weight + laplacian$out_weight
  industry <- io_axis_label(laplacian$dim_name)[total_weight == 0]
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
