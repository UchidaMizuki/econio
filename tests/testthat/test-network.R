test_that("io_network() informs only when converting implicitly", {
  iotable_noncompetitive <- read_iotable_dummy("regional_noncompetitive_import")
  expect_snapshot(network <- io_network(iotable_noncompetitive))

  iotable_competitive <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(network <- io_network(iotable_competitive))
})

test_that("io_network() aborts on negative transactions", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  iotable_negative <- iotable
  iotable_negative[1, 1] <- -99

  expect_snapshot(io_network(iotable_negative), error = TRUE)
})

test_that("io_network(paths = \"all\") gives all-path weights and average propagation lengths", {
  iotable <- read_iotable_dummy("multiregional_noncompetitive_import")
  network <- io_network(iotable, paths = "all")

  transactions <- get_path_transactions(iotable, 200)
  weight <- Reduce(`+`, transactions)
  expect_equal(network$weight, weight)
  expect_equal(
    network$path_length,
    Reduce(`+`, Map(`*`, seq_along(transactions), transactions)) / weight
  )
})

test_that("io_network_all() aborts when the sums over paths diverge", {
  weight <- rbind(c(0, 2), c(2, 0))
  expect_snapshot(
    io_network_all(weight, c(1, 1), tibble::tibble(sector = 1:2)),
    error = TRUE
  )
})
