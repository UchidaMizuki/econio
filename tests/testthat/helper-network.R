# Domestic transactions along paths of length 1, 2, ..., `n_paths`, i.e.
# `Z_n = A^n diag(x)`.
get_path_transactions <- function(iotable, n_paths) {
  weight <- io_network(iotable)$weight
  total_input <- as.vector(io_total_input(iotable))
  input_coef <- sweep(weight, 2, total_input, "/")
  powers <- purrr::accumulate(
    seq_len(n_paths),
    \(power, n) power %*% input_coef,
    .init = diag(nrow(input_coef))
  )
  purrr::map(powers[-1], \(power) sweep(power, 2, total_input, "*"))
}

# A regional noncompetitive import type table with the domestic transactions
# `weight` between industries, balanced to the total input `total_input` with
# value added and final demand. By default, every industry has as much value
# added and final demand as it has intermediate output and input.
get_iotable_weight <- function(
  weight,
  total_input = rowSums(weight) + colSums(weight)
) {
  sector_name <- paste0("industry_", seq_len(nrow(weight)))
  value <- rbind(
    cbind(weight, total_input - rowSums(weight)),
    c(total_input - colSums(weight), 0)
  )
  tidyr::expand_grid(
    input = c(sector_name, "value_added_1"),
    output = c(sector_name, "final_demand_1")
  ) |>
    dplyr::mutate(
      input_sector_type = sub("_[0-9]+$", "", .data$input),
      output_sector_type = sub("_[0-9]+$", "", .data$output),
      value = as.vector(t(value))
    ) |>
    dplyr::select(
      "input_sector_type",
      input_sector_name = "input",
      "output_sector_type",
      output_sector_name = "output",
      "value"
    ) |>
    io_table_regional(import_type = "noncompetitive_import")
}
