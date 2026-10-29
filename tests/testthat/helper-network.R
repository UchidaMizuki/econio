# Domestic transactions along paths of length 1, 2, ..., `n_paths`, i.e.
# `Z_n = A^n diag(x)`.
get_path_transactions <- function(iotable, n_paths) {
  weight <- io_network(iotable)$weight
  output <- as.vector(io_total_input(iotable))
  input_coef <- sweep(weight, 2, output, "/")
  powers <- purrr::accumulate(
    seq_len(n_paths),
    \(power, n) power %*% input_coef,
    .init = diag(nrow(input_coef))
  )
  purrr::map(powers[-1], \(power) sweep(power, 2, output, "*"))
}
