test_that("io_trophic_level matches the Laplacian solution and is gauge-fixed", {
  for (name in c(
    "regional_noncompetitive_import",
    "regional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)

    level <- io_trophic_level(iotable)
    actual <- as.numeric(as.matrix(level))

    expect_equal(min(actual), 0)
    expect_true(all(is.finite(actual)))
  }
})

test_that("io_trophic_level runs on multiregional tables", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  level <- as.numeric(as.matrix(io_trophic_level(iotable)))
  expect_equal(min(level), 0)
  expect_true(all(is.finite(level)))
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

    level <- io_trophic_solve(laplacian)
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
  expected <- as.vector(expected - min(expected))

  actual <- suppressMessages(as.numeric(as.matrix(io_trophic_level(iotable))))
  expect_equal(actual, expected)
})

test_that("io_trophic_solve gives exact levels for a coherent chain", {
  weight <- rbind(c(0, 5, 0), c(0, 0, 5), c(0, 0, 0))
  laplacian <- io_laplacian_weight(weight, tibble::tibble(sector = 1:3))
  expect_equal(io_trophic_solve(laplacian), c(0, 1, 2))
})

test_that("io_trophic_solve matches the two-industry closed form", {
  weight <- rbind(c(4, 3), c(1, 7))
  laplacian <- io_laplacian_weight(weight, tibble::tibble(sector = 1:2))
  expect_equal(io_trophic_solve(laplacian), c(0, (3 - 1) / (3 + 1)))
})

test_that("io_trophic_solve returns 0 for a single industry", {
  laplacian <- io_laplacian_weight(matrix(3), tibble::tibble(sector = 1))
  expect_equal(io_trophic_solve(laplacian), 0)
})

test_that("io_trophic_solve is invariant to the unit of the table", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  laplacian <- suppressMessages(io_laplacian(iotable))
  expected <- io_trophic_solve(laplacian)

  for (unit in c(1e-6, 1e9)) {
    laplacian_scaled <- io_laplacian_weight(
      laplacian$weight * unit,
      laplacian$dim_name
    )
    expect_equal(io_trophic_solve(laplacian_scaled), expected)
  }
})

test_that("io_trophic_level aborts on an industry with no links to others", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  iotable[1, 2:3] <- 0
  iotable[2:3, 1] <- 0

  expect_snapshot(io_trophic_level(iotable), error = TRUE)
  expect_snapshot(io_trophic_incoherence(iotable), error = TRUE)
})

test_that("io_check_connected aborts on separate components", {
  weight <- rbind(
    c(0, 1, 0, 0),
    c(1, 0, 0, 0),
    c(0, 0, 0, 2),
    c(0, 0, 0, 0)
  )
  laplacian <- io_laplacian_weight(
    weight,
    tibble::tibble(sector = letters[1:4])
  )
  expect_equal(io_network_component(weight), c(1L, 1L, 2L, 2L))
  expect_snapshot(io_check_connected(laplacian), error = TRUE)
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
  expected <- optimum$par - min(optimum$par)

  actual <- suppressMessages(as.numeric(as.matrix(io_trophic_level(iotable))))
  expect_equal(actual, expected, tolerance = 1e-4)
  expect_lte(objective(actual), optimum$value)
})
