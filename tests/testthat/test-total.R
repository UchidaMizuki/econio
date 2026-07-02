test_that("io_total_input() and io_total_output() validate same_region", {
  iotable <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(io_total_input(iotable, same_region = NA), error = TRUE)
  expect_snapshot(io_total_output(iotable, same_region = "x"), error = TRUE)
})
