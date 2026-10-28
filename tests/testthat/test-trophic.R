test_that("io_trophic_level is centered to a weighted mean of 0", {
  for (name in c(
    "regional_noncompetitive_import",
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    laplacian <- suppressMessages(io_laplacian(iotable))
    total_weight <- laplacian$in_weight + laplacian$out_weight

    actual <- suppressMessages(as.numeric(as.matrix(io_trophic_level(iotable))))
    expect_equal(sum(total_weight * actual), 0)
    expect_all_true(is.finite(actual))
  }
})

test_that("io_trophic_incoherence is a non-negative scalar", {
  for (name in c(
    "regional_noncompetitive_import",
    "regional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)

    f0 <- io_trophic_incoherence(iotable)
    expect_length(f0, 1L)
    expect_true(is.finite(f0))
    expect_gte(f0, 0)
  }
})

test_that("io_trophic_level solves the Laplacian system", {
  for (name in c(
    "regional_noncompetitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- suppressMessages(read_iotable_dummy(name))
    laplacian <- suppressMessages(io_laplacian(iotable))
    imbalance <- laplacian$in_weight - laplacian$out_weight

    level <- io_trophic(laplacian)$level
    expect_equal(as.vector(laplacian$laplacian %*% level), imbalance)
  }
})

test_that("io_trophic_level matches the pseudo-inverse solution", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  laplacian <- suppressMessages(io_laplacian(iotable))
  imbalance <- laplacian$in_weight - laplacian$out_weight

  decomposition <- eigen(laplacian$laplacian, symmetric = TRUE)
  values <- decomposition$values
  inverse_values <- ifelse(values > 1e-8 * max(values), 1 / values, 0)
  expected <- decomposition$vectors %*%
    (inverse_values * crossprod(decomposition$vectors, imbalance))
  total_weight <- laplacian$in_weight + laplacian$out_weight
  expected <- as.vector(
    expected - sum(total_weight * expected) / sum(total_weight)
  )

  actual <- suppressMessages(as.numeric(as.matrix(io_trophic_level(iotable))))
  expect_equal(actual, expected)
})

test_that("io_trophic gives exact levels for a coherent chain", {
  weight <- rbind(c(0, 5, 0), c(0, 0, 5), c(0, 0, 0))
  laplacian <- io_laplacian_weight(weight, tibble::tibble(sector = 1:3))
  # u = (5, 10, 5), so the weighted mean of (0, 1, 2) is 1.
  expect_equal(io_trophic(laplacian)$level, c(-1, 0, 1))
})

test_that("io_trophic matches the two-industry closed form", {
  weight <- rbind(c(4, 3), c(1, 7))
  laplacian <- io_laplacian_weight(weight, tibble::tibble(sector = 1:2))
  # h_2 - h_1 = (3 - 1) / (3 + 1) = 0.5, centered with u = (12, 18).
  expect_equal(io_trophic(laplacian)$level, c(-0.3, 0.2))
})

test_that("io_trophic leaves other levels unchanged when merging industries with equal levels", {
  # Industries 2 and 3 are symmetric, so they have equal levels.
  weight <- rbind(
    c(0, 1, 1, 0),
    c(0, 0, 1, 1),
    c(0, 1, 0, 1),
    c(0, 0, 0, 0)
  )
  laplacian <- io_laplacian_weight(weight, tibble::tibble(sector = 1:4))
  level <- io_trophic(laplacian)$level
  expect_equal(level[[2]], level[[3]])

  # Merging them turns their mutual transactions into a self-loop.
  merge <- rbind(c(1, 0, 0, 0), c(0, 1, 1, 0), c(0, 0, 0, 1))
  weight_merged <- merge %*% weight %*% t(merge)
  laplacian_merged <- io_laplacian_weight(
    weight_merged,
    tibble::tibble(sector = 1:3)
  )
  expect_equal(io_trophic(laplacian_merged)$level, level[c(1, 2, 4)])
})

test_that("io_trophic returns 0 for a single industry", {
  laplacian <- io_laplacian_weight(matrix(3), tibble::tibble(sector = 1))
  expect_equal(io_trophic(laplacian)$level, 0)
})

test_that("io_trophic is invariant to the unit of the table", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  laplacian <- suppressMessages(io_laplacian(iotable))
  expected <- io_trophic(laplacian)$level

  for (unit in c(1e-6, 1e9)) {
    laplacian_scaled <- io_laplacian_weight(
      laplacian$weight * unit,
      laplacian$dim_name
    )
    expect_equal(io_trophic(laplacian_scaled)$level, expected)
  }
})

test_that("io_trophic_level aborts on an industry with no links to others", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  iotable[1, 2:3] <- 0
  iotable[2:3, 1] <- 0

  expect_snapshot(io_trophic_level(iotable), error = TRUE)
  expect_snapshot(io_trophic_incoherence(iotable), error = TRUE)
})

test_that("io_trophic aborts on separate components", {
  weight <- rbind(
    c(0, 1, 0, 0),
    c(1, 0, 0, 0),
    c(0, 0, 0, 2),
    c(0, 0, 0, 0)
  )
  laplacian <- io_laplacian_weight(weight, tibble::tibble(sector = 1:4))
  expect_snapshot(io_trophic(laplacian), error = TRUE)
})

test_that("io_trophic_level agrees with numerically minimizing the trophic objective", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  weight <- suppressMessages(io_laplacian(iotable))$weight

  objective <- function(level) {
    sum(weight * (outer(level, level, \(from, to) to - from) - 1)^2)
  }
  optimum <- optim(
    rep(0, nrow(weight)),
    objective,
    method = "BFGS",
    control = list(reltol = 1e-14)
  )
  total_weight <- rowSums(weight) + colSums(weight)
  expected <- optimum$par -
    sum(total_weight * optimum$par) / sum(total_weight)

  actual <- suppressMessages(as.numeric(as.matrix(io_trophic_level(iotable))))
  expect_equal(actual, expected, tolerance = 1e-4)
  # Allow for rounding: the analytic solution must not be worse than optim().
  expect_lte(objective(actual), optimum$value * (1 + 1e-10))
})
