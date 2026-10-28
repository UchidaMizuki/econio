test_that("io_total_input() and io_total_output() validate same_region", {
  iotable <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(io_total_input(iotable, same_region = NA), error = TRUE)
  expect_snapshot(io_total_output(iotable, same_region = "x"), error = TRUE)
})

test_that("io_total_output() and io_total_input() ignore inter-regional values with same_region = TRUE", {
  raw <- read_iotable_dummy_raw("multiregional_noncompetitive_import")
  iotable <- read_iotable_dummy("multiregional_noncompetitive_import")
  raw_same_region <- raw |>
    dplyr::filter(input_region == output_region)

  expect_equal(
    io_total_output(
      iotable,
      output_sector_type = "final_demand",
      same_region = TRUE
    ) |>
      as_long_tibble(),
    raw_same_region |>
      dplyr::filter(
        input_sector_type == "industry",
        output_sector_type == "final_demand"
      ) |>
      dplyr::summarise(
        value = sum(value),
        .by = c("input_region", "input_sector_type", "input_sector_name")
      )
  )
  expect_equal(
    io_total_input(iotable, same_region = TRUE) |>
      as_long_tibble(),
    raw_same_region |>
      dplyr::filter(
        input_sector_type %in% c("industry", "import", "value_added"),
        output_sector_type == "industry"
      ) |>
      dplyr::summarise(
        value = sum(value),
        .by = c("output_region", "output_sector_type", "output_sector_name")
      )
  )
})

test_that("same_region = TRUE has no effect on regional tables", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  expect_equal(
    io_total_output(iotable, same_region = TRUE),
    io_total_output(iotable)
  )
  expect_equal(
    io_total_input(iotable, same_region = TRUE),
    io_total_input(iotable)
  )
})
