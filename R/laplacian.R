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
  label <- dim_name$sector
  if (inherits(label, "econ_io_sector")) {
    label <- io_sector_name(label)
  }
  if ("region" %in% names(dim_name)) {
    label <- paste(dim_name$region, label, sep = "/")
  }
  label
}

io_abort_disconnected <- function(laplacian, call = rlang::caller_env()) {
  total_weight <- laplacian$in_weight + laplacian$out_weight
  industry <- io_industry_label(laplacian$dim_name)[total_weight == 0]
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
