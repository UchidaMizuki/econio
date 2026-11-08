safe_divide <- function(x, y) {
  dibble::ifelse(x == 0 & y == 0, 0, x / y)
}

# Whether `x` exceeds `y` by more than rounding error, with a tolerance relative
# to `y` as in `all.equal()`.
exceeds <- function(x, y, tolerance = .Machine$double.eps^0.5) {
  x - y > tolerance * abs(y)
}
