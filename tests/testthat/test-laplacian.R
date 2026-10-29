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

test_that("io_laplacian(paths = \"all\") sums transactions over all paths", {
  iotable <- read_iotable_dummy("multiregional_noncompetitive_import")
  laplacian <- io_laplacian(iotable, paths = "all")

  transactions <- get_path_transactions(iotable, 200)
  n <- seq_along(transactions)
  expect_equal(laplacian$weight, Reduce(`+`, transactions))
  expect_equal(
    laplacian$length_weight,
    Reduce(`+`, Map(`*`, n, transactions))
  )
  expect_equal(
    laplacian$squared_length_weight,
    Reduce(`+`, Map(`*`, n^2, transactions))
  )
})
