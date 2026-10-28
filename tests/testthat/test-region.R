test_that("io_same_region() is one for every cell of regional tables", {
  iotable <- read_iotable_dummy("regional_competitive_import")
  same_region <- as_long_tibble(io_same_region(iotable))
  expect_all_equal(same_region$value, 1)
})

test_that("io_same_region() is one only within the same region for multiregional tables", {
  for (name in c(
    "multiregional_noncompetitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    same_region <- as_long_tibble(io_same_region(iotable))
    expect_equal(
      same_region$value,
      as.double(same_region$input_region == same_region$output_region)
    )
  }
})
