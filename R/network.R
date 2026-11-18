io_network <- function(
  data,
  paths = c("direct", "all"),
  call = rlang::caller_env()
) {
  paths <- rlang::arg_match(paths, error_call = call)

  original <- io_inter_industry(data)
  data <- io_table_to_noncompetitive_import(data) |>
    io_check_axes()
  inter_industry <- io_inter_industry(data)
  dim_name <- dimnames(inter_industry)$input
  weight <- as.matrix(inter_industry)

  if (any(weight < 0)) {
    io_abort_negative_transaction(inter_industry, original, call = call)
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
        "x" = "The spectral radius of the domestic input coefficient matrix is {format_number_near_one(spectral_radius)}, not less than 1.",
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

# `domestic` and `original` are the inter-industry blocks after and before the
# conversion to noncompetitive import type.
io_abort_negative_transaction <- function(
  domestic,
  original,
  call = rlang::caller_env()
) {
  negative <- dibble::dibble(domestic = domestic, original = original) |>
    tibble::as_tibble() |>
    dplyr::filter(.data$domestic < 0) |>
    dplyr::arrange(.data$domestic)
  n_negative <- nrow(negative)
  n_original <- sum(negative$original < 0)
  n_shown <- min(n_negative, 5)
  shown <- negative[seq_len(n_shown), ]

  cli::cli_abort(
    c(
      "Can't build the industry network from negative domestic intermediate transactions.",
      glue::glue(
        "{io_axis_label(shown$input)} to {io_axis_label(shown$output)}: {format_number(shown$domestic)}."
      ) |>
        rlang::set_names("x"),
      if (n_negative > n_shown) {
        c("x" = "... and {n_negative - n_shown} more.")
      },
      if (n_original > 0) {
        c(
          "i" = "{n_original} {?is/are} already negative in {.arg data}, e.g. byproducts recorded as negative inputs (the Stone method).",
          "i" = "{.fn io_table_transfer_byproduct} moves negative intermediate transactions to the reverse-direction cell."
        )
      },
      if (n_original < n_negative) {
        c(
          "i" = "{n_negative - n_original} became negative when {.fn io_table_to_noncompetitive_import} split imports by import coefficients above 1."
        )
      }
    ),
    call = call
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
