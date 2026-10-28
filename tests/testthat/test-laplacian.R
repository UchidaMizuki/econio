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
  direct <- io_laplacian(iotable)
  all <- io_laplacian(iotable, paths = "all")

  output <- as.vector(io_total_input(iotable))
  input_coef <- sweep(direct$weight, 2, output, "/")
  power <- diag(nrow(input_coef))
  expected <- list(0, 0, 0)
  for (n in 1:200) {
    power <- power %*% input_coef
    transaction <- sweep(power, 2, output, "*")
    expected <- Map(\(moment, k) moment + n^k * transaction, expected, 0:2)
  }

  expect_equal(all$weight, expected[[1]])
  expect_equal(all$length_weight, expected[[2]])
  expect_equal(all$squared_length_weight, expected[[3]])
})
