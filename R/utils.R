safe_divide <- function(x, y) {
  dibble::ifelse(x == 0 & y == 0, 0, x / y)
}

# Whether `x` exceeds `y` by more than rounding error, with a tolerance relative
# to `y` as in `all.equal()`.
exceeds <- function(x, y, tolerance = .Machine$double.eps^0.5) {
  x - y > tolerance * abs(y)
}

# Format numbers for messages one by one, with the significant digits of
# `getOption("digits")` as in `print()`.
format_number <- function(x, digits = getOption("digits")) {
  purrr::map2_chr(x, digits, \(x, digits) format(x, digits = digits))
}

# Format numbers compared with 1, e.g. import coefficients, like
# `format_number()`, but with more digits if needed to show that they differ
# from 1, e.g. 1.0000001 rather than 1. Doubles have at most 15 significant
# decimal digits.
format_number_near_one <- function(x) {
  digits <- ceiling(-log10(abs(x - 1))) + 1
  format_number(x, digits = pmin(pmax(digits, getOption("digits")), 15))
}
