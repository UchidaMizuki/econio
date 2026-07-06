io_laplacian <- function(data) {
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
    cli::cli_inform(
      "Zeroing {n_negative} negative domestic intermediate transaction{?s} (e.g. from byproduct treatment such as the Stone method)."
    )
    weight[weight < 0] <- 0
  }

  in_weight <- colSums(weight)
  out_weight <- rowSums(weight)
  laplacian <- diag(in_weight + out_weight) - (weight + t(weight))
  list(
    dim_name = dim_name,
    weight = weight,
    in_weight = in_weight,
    out_weight = out_weight,
    laplacian = laplacian
  )
}
