test_that("io_trophic_level is centered to a weighted mean of 0", {
  for (name in c(
    "regional_noncompetitive_import",
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    network <- suppressMessages(io_network(iotable))
    total_weight <- network$total_weight

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
    network <- suppressMessages(io_network(iotable))
    imbalance <- colSums(network$weight) - rowSums(network$weight)

    level <- io_trophic(network)$level
    expect_equal(as.vector(network$laplacian %*% level), imbalance)
  }
})

test_that("io_trophic_level matches the pseudo-inverse solution", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  network <- suppressMessages(io_network(iotable))
  imbalance <- colSums(network$weight) - rowSums(network$weight)

  decomposition <- eigen(network$laplacian, symmetric = TRUE)
  values <- decomposition$values
  inverse_values <- ifelse(values > 1e-8 * max(values), 1 / values, 0)
  expected <- decomposition$vectors %*%
    (inverse_values * crossprod(decomposition$vectors, imbalance))
  total_weight <- network$total_weight
  expected <- as.vector(
    expected - sum(total_weight * expected) / sum(total_weight)
  )

  actual <- suppressMessages(as.numeric(as.matrix(io_trophic_level(iotable))))
  expect_equal(actual, expected)
})

test_that("io_trophic gives exact levels for a coherent chain", {
  weight <- rbind(c(0, 5, 0), c(0, 0, 5), c(0, 0, 0))
  network <- io_network_direct(weight, tibble::tibble(sector = 1:3))
  # u = (5, 10, 5), so the weighted mean of (0, 1, 2) is 1.
  expect_equal(io_trophic(network)$level, c(-1, 0, 1))
})

test_that("io_trophic matches the two-industry closed form", {
  weight <- rbind(c(4, 3), c(1, 7))
  network <- io_network_direct(weight, tibble::tibble(sector = 1:2))
  # h_2 - h_1 = (3 - 1) / (3 + 1) = 0.5, centered with u = (12, 18).
  expect_equal(io_trophic(network)$level, c(-0.3, 0.2))
})

test_that("io_trophic leaves other levels unchanged when merging industries with equal levels", {
  # Industries 2 and 3 are symmetric, so they have equal levels.
  weight <- rbind(
    c(0, 1, 1, 0),
    c(0, 0, 1, 1),
    c(0, 1, 0, 1),
    c(0, 0, 0, 0)
  )
  network <- io_network_direct(weight, tibble::tibble(sector = 1:4))
  level <- io_trophic(network)$level
  expect_equal(level[[2]], level[[3]])

  # Merging them turns their mutual transactions into a self-loop.
  merge <- rbind(c(1, 0, 0, 0), c(0, 1, 1, 0), c(0, 0, 0, 1))
  weight_merged <- merge %*% weight %*% t(merge)
  network_merged <- io_network_direct(
    weight_merged,
    tibble::tibble(sector = 1:3)
  )
  expect_equal(io_trophic(network_merged)$level, level[c(1, 2, 4)])
})

test_that("io_trophic returns 0 for a single industry", {
  network <- io_network_direct(matrix(3), tibble::tibble(sector = 1))
  expect_equal(io_trophic(network)$level, 0)
})

test_that("io_trophic is invariant to the unit of the table", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  network <- suppressMessages(io_network(iotable))
  expected <- io_trophic(network)$level

  for (unit in c(1e-6, 1e9)) {
    network_scaled <- io_network_direct(
      network$weight * unit,
      network$dim_name
    )
    expect_equal(io_trophic(network_scaled)$level, expected)
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
  network <- io_network_direct(weight, tibble::tibble(sector = 1:4))
  expect_snapshot(io_trophic(network), error = TRUE)
})

test_that("io_trophic_level agrees with numerically minimizing the trophic objective", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  weight <- suppressMessages(io_network(iotable))$weight

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

test_that("io_trophic_incoherence with paths = \"direct\" is F_0", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  weight <- io_network(iotable)$weight
  level <- as.numeric(as.matrix(io_trophic_level(iotable)))
  level_diff <- outer(level, level, \(from, to) to - from)

  expect_equal(
    io_trophic_incoherence(iotable, paths = "direct"),
    sum(weight * (level_diff - 1)^2) / sum(weight)
  )
})

test_that("io_trophic matches the two-industry closed form with path lengths", {
  weight <- rbind(c(4, 3), c(1, 7))
  weighted_length <- rbind(c(5, 6), c(3, 9))
  network <- io_network_weight(
    weight,
    weighted_length = weighted_length,
    weighted_squared_length = weighted_length^2 / weight,
    dim_name = tibble::tibble(sector = 1:2)
  )
  # h_2 - h_1 = (6 - 3) / (3 + 1) = 0.75, centered with u = (12, 18).
  expect_equal(io_trophic(network)$level, c(-0.45, 0.3))
})

test_that("io_trophic with paths = \"all\" leaves other levels unchanged when merging path weights", {
  weight <- rbind(
    c(0, 1, 1, 0),
    c(0, 0, 1, 1),
    c(0, 1, 0, 1),
    c(0, 0, 0, 0)
  )
  network <- io_network_all(
    weight,
    c(3, 4, 4, 4),
    tibble::tibble(sector = 1:4)
  )
  level <- io_trophic(network)$level
  expect_equal(level[[2]], level[[3]])

  merge <- rbind(c(1, 0, 0, 0), c(0, 1, 1, 0), c(0, 0, 0, 1))
  aggregate <- \(x) merge %*% x %*% t(merge)
  network_merged <- io_network_weight(
    aggregate(network$weight),
    weighted_length = aggregate(network$weighted_length),
    weighted_squared_length = aggregate(network$weighted_squared_length),
    dim_name = tibble::tibble(sector = 1:3)
  )
  expect_equal(io_trophic(network_merged)$level, level[c(1, 2, 4)])
})

test_that("io_trophic_level with paths = \"all\" agrees with numerically minimizing the path objective", {
  iotable <- read_iotable_dummy("multiregional_noncompetitive_import")
  transactions <- get_path_transactions(iotable, 100)
  weight <- Reduce(`+`, transactions)

  objective <- function(level) {
    level_diff <- outer(level, level, \(from, to) to - from)
    sum(purrr::imap_dbl(transactions, \(z, n) sum(z * (level_diff - n)^2)))
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

  actual <- as.numeric(as.matrix(io_trophic_level(iotable, paths = "all")))
  expect_equal(actual, expected, tolerance = 1e-4)
  expect_lte(objective(actual), optimum$value * (1 + 1e-10))
  expect_equal(
    io_trophic_incoherence(iotable, paths = "all"),
    objective(actual) / sum(weight)
  )
})

test_that("io_trophic_incoherence satisfies F = (sum(Q) - h'v) / sum(W)", {
  iotable <- read_iotable_dummy("multiregional_noncompetitive_import")
  for (paths in c("direct", "all")) {
    network <- io_network(iotable, paths = paths)
    level <- io_trophic(network)$level
    imbalance <- colSums(network$weighted_length) -
      rowSums(network$weighted_length)

    expect_equal(
      io_trophic_incoherence(iotable, paths = paths),
      (sum(network$weighted_squared_length) - sum(level * imbalance)) /
        sum(network$weight)
    )
  }
})

test_that("io_trophic_level validates paths", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  expect_snapshot(io_trophic_level(iotable, paths = "indirect"), error = TRUE)
})
