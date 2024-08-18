test_that("iotable_regional_noncompetitive_import works", {
  iotable_dummy <- readRDS(test_path("data", "iotable_dummy.rds"))

  name <- "regional_noncompetitive_import"

  expect_error(io_table_regional(iotable_dummy[[name]],
                                 competitive_import = TRUE))
  iotable <- io_table_regional(iotable_dummy[[name]],
                               competitive_import = FALSE)

  data_total_output <- get_data_total_output(iotable)
  expect_true(all(data_total_output$object == data_total_output$expected))

  data_total_input <- get_data_total_input(iotable)
  expect_true(all(dplyr::near(data_total_input$object, data_total_input$expected)))
})

test_that("iotable_regional_competitive_import works", {
  iotable_dummy <- readRDS(test_path("data", "iotable_dummy.rds"))

  name <- "regional_competitive_import"

  expect_error(io_table_regional(iotable_dummy[[name]],
                                 competitive_import = FALSE))
  iotable <- io_table_regional(iotable_dummy[[name]],
                               competitive_import = TRUE)

  data_total_output <- get_data_total_output(iotable)
  expect_true(all(data_total_output$object == data_total_output$expected))

  data_total_input <- get_data_total_input(iotable,
                                           endogenize_import = TRUE)
  expect_true(all(dplyr::near(data_total_input$object, data_total_input$expected)))

  data_total_input <- get_data_total_input(iotable,
                                           endogenize_import = FALSE)
  expect_true(all(dplyr::near(data_total_input$object, data_total_input$expected)))
})

test_that("iotable_multiregional_noncompetitive_import works", {
  iotable_dummy <- readRDS(test_path("data", "iotable_dummy.rds"))

  name <- "multiregional_noncompetitive_import"

  expect_error(io_table_multiregional(iotable_dummy[[name]],
                                      competitive_import = TRUE))
  iotable <- io_table_multiregional(iotable_dummy[[name]],
                                    competitive_import = FALSE)

  data_total_output <- get_data_total_output(iotable)
  expect_true(all(dplyr::near(data_total_output$object, data_total_output$expected)))

  data_total_input <- get_data_total_input(iotable)
  expect_true(all(dplyr::near(data_total_input$object, data_total_input$expected)))
})

test_that("iotable_multiregional_competitive_import works", {
  iotable_dummy <- readRDS(test_path("data", "iotable_dummy.rds"))

  name <- "multiregional_competitive_import"

  expect_error(io_table_multiregional(iotable_dummy[[name]],
                                      competitive_import = FALSE))
  iotable <- io_table_multiregional(iotable_dummy[[name]],
                                    competitive_import = TRUE)

  data_total_output <- get_data_total_output(iotable)
  expect_true(all(data_total_output$object == data_total_output$expected))


  data_total_input <- get_data_total_input(iotable,
                                           endogenize_import = TRUE)
  expect_true(all(dplyr::near(data_total_input$object, data_total_input$expected)))

  data_total_input <- get_data_total_input(iotable,
                                           endogenize_import = FALSE)
  expect_true(all(dplyr::near(data_total_input$object, data_total_input$expected)))
})
