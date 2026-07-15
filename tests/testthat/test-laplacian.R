test_that("io_laplacian() informs only when converting implicitly", {
  iotable_noncompetitive <- read_iotable_dummy("regional_noncompetitive_import")
  expect_snapshot(laplacian <- io_laplacian(iotable_noncompetitive))

  iotable_competitive <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(laplacian <- io_laplacian(iotable_competitive))
})

test_that("io_laplacian() aborts on negative transactions", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  iotable_negative <- iotable
  iotable_negative[1, 1] <- -99

  expect_snapshot(io_laplacian(iotable_negative), error = TRUE)
})
