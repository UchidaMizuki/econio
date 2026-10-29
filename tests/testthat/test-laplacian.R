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
    laplacian$weighted_length,
    Reduce(`+`, Map(`*`, n, transactions))
  )
  expect_equal(
    laplacian$weighted_squared_length,
    Reduce(`+`, Map(`*`, n^2, transactions))
  )
})

test_that("io_laplacian_all() aborts when the sums over paths diverge", {
  weight <- rbind(c(0, 2), c(2, 0))
  expect_snapshot(
    io_laplacian_all(weight, c(1, 1), tibble::tibble(sector = 1:2)),
    error = TRUE
  )
})
