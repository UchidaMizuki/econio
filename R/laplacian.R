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
    cli::cli_abort(
      c(
        "Found {n_negative} negative domestic intermediate transaction{?s} (e.g. from byproduct treatment such as the Stone method).",
        "i" = "Use {.fn io_table_to_byproduct_transfer} to convert negative entries to the transfer method before building the industry network."
      )
    )
  }

  io_laplacian_weight(weight, dim_name)
}

io_laplacian_weight <- function(weight, dim_name) {
  in_weight <- colSums(weight)
  out_weight <- rowSums(weight)
  total_weight <- in_weight + out_weight
  laplacian <- diag(total_weight, nrow = length(total_weight)) -
    (weight + t(weight))
  list(
    dim_name = dim_name,
    weight = weight,
    in_weight = in_weight,
    out_weight = out_weight,
    laplacian = laplacian
  )
}

io_industry_label <- function(dim_name) {
  label <- purrr::map(dim_name, function(x) {
    if (inherits(x, "econ_io_sector")) io_sector_name(x) else as.character(x)
  })
  do.call(paste, c(unname(label), sep = "/"))
}
