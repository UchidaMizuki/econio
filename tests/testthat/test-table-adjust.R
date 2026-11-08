test_that("io_table_transfer_byproduct() returns data unchanged when there are no negative transactions", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  expect_identical(io_table_transfer_byproduct(iotable), iotable)
})

test_that("io_table_transfer_byproduct() transfers a negative transaction to the reverse cell", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  iotable_negative <- iotable
  iotable_negative[1, 3] <- -10

  weight <- io_table_transfer_byproduct(iotable_negative) |>
    io_inter_industry() |>
    as.matrix()

  expect_equal(sum(weight < 0), 0)
  expect_equal(weight[1, 3], 0)
  expect_equal(weight[3, 1], 13)
})

test_that("io_table_transfer_byproduct() handles negative transactions in both directions", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  iotable_negative <- iotable
  iotable_negative[1, 3] <- -10
  iotable_negative[3, 1] <- -4

  weight <- io_table_transfer_byproduct(iotable_negative) |>
    io_inter_industry() |>
    as.matrix()

  expect_equal(sum(weight < 0), 0)
  expect_equal(weight[1, 3], 4)
  expect_equal(weight[3, 1], 10)
})

test_that("io_table_remove_reexport() removes re-exports from imports and exports", {
  iotable <- get_iotable_reexport()
  actual <- io_table_remove_reexport(iotable)

  # The excess of 5 is split 3:1 over the export and import columns.
  expected <- get_iotable_reexport(
    export_1 = c(export = 26.25, outflow = 8.75),
    import_1 = c(import = -26.25, inflow = -8.75)
  )
  expect_equal(actual, expected)
  expect_equal(io_total_output(actual), io_total_output(iotable))

  domestic <- io_table_to_noncompetitive_import(actual) |>
    get_data_regional_demand() |>
    as.matrix()
  expect_all_equal(domestic[1, ], 0)
})

test_that("io_table_remove_reexport() returns data unchanged without re-exports", {
  for (name in c(
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    expect_identical(io_table_remove_reexport(iotable), iotable)
  }
})

test_that("io_table_remove_reexport() leaves sectors with non-positive regional demand unchanged", {
  iotable <- get_iotable_nonpositive_demand()
  expect_identical(io_table_remove_reexport(iotable), iotable)
})

test_that("io_table_remove_reexport() removes all exports when re-exports equal exports", {
  iotable <- get_iotable_reexport(export_1 = c(export = 3.75, outflow = 1.25))
  export <- io_table_remove_reexport(iotable) |>
    io_total_output(output_sector_type = "export")
  expect_equal(as.vector(export)[[1]], 0)
})

test_that("io_table_remove_reexport() errors when re-exports exceed exports", {
  iotable <- get_iotable_reexport(export_1 = c(export = 1, outflow = 1))
  expect_snapshot(io_table_remove_reexport(iotable), error = TRUE)
})

test_that("io_table_remove_reexport() errors for noncompetitive import type tables", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  expect_snapshot(io_table_remove_reexport(iotable), error = TRUE)
})
