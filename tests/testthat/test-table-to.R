test_that("io_table_to_competitive_import() works", {
  names <- c("regional_noncompetitive_import",
             "regional_competitive_import",
             "multiregional_noncompetitive_import",
             "multiregional_competitive_import")

  for (name in names) {
    iotable_dummy <- read_iotable_dummy(name)
    iotable_dummy_competitive_import <- io_table_to_competitive_import(iotable_dummy)

    expect_s3_class(iotable_dummy_competitive_import, "io_table_competitive_import")
    expect_true(all(dplyr::near(io_total_output(iotable_dummy_competitive_import),
                                io_total_output(iotable_dummy))))
    expect_true(all(dplyr::near(io_total_input(iotable_dummy_competitive_import),
                                io_total_input(iotable_dummy))))
  }
})

test_that("io_table_to_noncompetitive_import() works", {
  names <- c("regional_noncompetitive_import",
             "regional_competitive_import",
             "multiregional_noncompetitive_import",
             "multiregional_competitive_import")

  for (name in names) {
    iotable_dummy <- read_iotable_dummy(name)
    iotable_dummy_noncompetitive_import <- io_table_to_noncompetitive_import(iotable_dummy)

    expect_s3_class(iotable_dummy_noncompetitive_import, "io_table_noncompetitive_import")
    expect_true(all(dplyr::near(io_total_output(iotable_dummy_noncompetitive_import),
                                io_total_output(iotable_dummy))))
    expect_true(all(dplyr::near(io_total_input(iotable_dummy_noncompetitive_import),
                                io_total_input(iotable_dummy))))
  }
})
