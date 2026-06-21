test_that("io_downstreamness equals the Leontief inverse column sums", {
  for (name in c(
    "regional_competitive_import",
    "regional_noncompetitive_import"
  )) {
    iotable <- read_iotable_dummy(name)

    expected <- colSums(as.matrix(io_leontief_inverse(iotable)))
    actual <- as.numeric(as.matrix(io_downstreamness(iotable)))
    expect_equal(sort(actual), sort(unname(expected)))
  }
})

test_that("io_upstreamness equals the Ghosh inverse row sums", {
  for (name in c(
    "regional_competitive_import",
    "regional_noncompetitive_import"
  )) {
    iotable <- read_iotable_dummy(name)

    expected <- rowSums(as.matrix(io_ghosh_inverse(iotable)))
    actual <- as.numeric(as.matrix(io_upstreamness(iotable)))
    expect_equal(sort(actual), sort(unname(expected)))
  }
})

test_that("normalize rescales the measure to a mean of 1", {
  for (name in c(
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)

    down <- as.numeric(as.matrix(io_downstreamness(iotable, normalize = TRUE)))
    up <- as.numeric(as.matrix(io_upstreamness(iotable, normalize = TRUE)))
    expect_equal(mean(down), 1)
    expect_equal(mean(up), 1)
  }
})

test_that("normalize must be a scalar logical", {
  iotable <- read_iotable_dummy("regional_competitive_import")
  expect_error(io_downstreamness(iotable, normalize = "yes"))
})
