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
