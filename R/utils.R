safe_divide <- function(x, y) {
  dibble::ifelse(x == 0 & y == 0, 0, x / y)
}
