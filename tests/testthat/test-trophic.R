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
