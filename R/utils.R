safe_divide <- function(x, y) {
  dibble::ifelse(x == 0 & y == 0, 0, x / y)
}

# Whether `x` exceeds `y` by more than rounding error, with a tolerance relative
# to `y` as in `all.equal()`.
exceeds <- function(x, y, tolerance = .Machine$double.eps^0.5) {
  x - y > tolerance * abs(y)
}

# Whether the totals `actual` equal `expected` up to `tolerance` times the sum of
# the totals, e.g. total output. Unlike a tolerance relative to each total, this
# allows rounding error in totals of 0, and unlike an absolute tolerance, it
# doesn't depend on the units of the table. Totals that aren't finite are left
# out of the sum, so that one of them doesn't make every other comparison NA or
# TRUE. A comparison with NA or NaN, e.g. from dividing by zero, is NA, so drop
# the matching totals with `dplyr::filter_out()` to keep it as a mismatch.
nearly_equal_totals <- function(actual, expected, tolerance) {
  scale <- sum(abs(expected[is.finite(expected)]))
  abs(actual - expected) <= tolerance * scale
}

# Format numbers for messages one by one, with the significant digits of
# `getOption("digits")` as in `print()`. If `x` is compared with `compare_to`,
# e.g. import coefficients with 1, use more digits if needed to show that they
# differ, e.g. 1.0000001 rather than 1, up to the 15 significant decimal digits
# of doubles.
format_number <- function(x, compare_to = NULL) {
  digits <- getOption("digits")
  if (!is.null(compare_to)) {
    digits_needed <- ceiling(-log10(abs(x - compare_to) / abs(compare_to))) + 1
    digits <- pmin(pmax(digits_needed, digits, na.rm = TRUE), 15)
  }
  purrr::map2_chr(x, digits, \(x, digits) format(x, digits = digits))
}
