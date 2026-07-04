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
  expect_snapshot(io_downstreamness(iotable, normalize = "yes"), error = TRUE)
})

test_that("io_streamness_length equals the sum of upstreamness and downstreamness", {
  for (name in c(
    "regional_competitive_import",
    "regional_noncompetitive_import"
  )) {
    iotable <- read_iotable_dummy(name)

    expected <- as.numeric(as.matrix(io_upstreamness(iotable))) +
      as.numeric(as.matrix(io_downstreamness(iotable)))
    actual <- as.numeric(as.matrix(io_streamness_length(iotable)))
    expect_equal(sort(actual), sort(expected))
  }
})

test_that("io_streamness_length normalize must be a scalar logical", {
  iotable <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(
    io_streamness_length(iotable, normalize = "yes"),
    error = TRUE
  )
})

test_that("io_streamness_position computes each type from upstreamness and downstreamness", {
  iotable <- read_iotable_dummy("regional_competitive_import")

  up <- as.numeric(as.matrix(io_upstreamness(iotable)))
  down <- as.numeric(as.matrix(io_downstreamness(iotable)))

  difference <- as.numeric(as.matrix(io_streamness_position(iotable)))
  expect_equal(sort(difference), sort(up - down))

  relative <- as.numeric(as.matrix(io_streamness_position(
    iotable,
    type = "relative"
  )))
  expect_equal(sort(relative), sort((up - down) / (up + down)))

  log_ratio <- as.numeric(as.matrix(io_streamness_position(
    iotable,
    type = "log_ratio"
  )))
  expect_equal(sort(log_ratio), sort(log(up / down)))
})

test_that("io_streamness_position normalize rescales the difference to a mean of 0", {
  iotable <- read_iotable_dummy("regional_competitive_import")

  position <- as.numeric(as.matrix(io_streamness_position(
    iotable,
    normalize = TRUE
  )))
  expect_equal(mean(position), 0)
})

test_that("io_streamness_position normalize must be a scalar logical", {
  iotable <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(
    io_streamness_position(iotable, normalize = "yes"),
    error = TRUE
  )
})

test_that("io_streamness_position type must be one of the allowed values", {
  iotable <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(
    io_streamness_position(iotable, type = "invalid"),
    error = TRUE
  )
})
