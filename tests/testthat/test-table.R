test_that("io_table_regional() and io_table_multiregional() work", {
  iotable_dummy <- readRDS(test_path("data", "iotable_dummy.rds"))

  names <- c(
    "regional_noncompetitive_import",
    "regional_competitive_import",
    "multiregional_noncompetitive_import",
    "multiregional_competitive_import"
  )

  for (name in names) {
    io_table <- switch(
      name,
      regional_noncompetitive_import = io_table_regional,
      regional_competitive_import = io_table_regional,
      multiregional_noncompetitive_import = io_table_multiregional,
      multiregional_competitive_import = io_table_multiregional
    )
    competitive_import <- switch(
      name,
      regional_noncompetitive_import = FALSE,
      regional_competitive_import = TRUE,
      multiregional_noncompetitive_import = FALSE,
      multiregional_competitive_import = TRUE
    )

    expect_error(io_table(
      iotable_dummy[[name]],
      competitive_import = !competitive_import
    ))
    iotable <- io_table(
      iotable_dummy[[name]],
      competitive_import = competitive_import
    )

    data_total_output <- get_data_total_output(iotable)
    expect_true(all(dplyr::near(
      data_total_output$actual,
      data_total_output$expected
    )))

    if (competitive_import) {
      data_total_input <- get_data_total_input(
        iotable,
        endogenize_import = TRUE
      )
      expect_true(all(dplyr::near(
        data_total_input$actual,
        data_total_input$expected
      )))

      data_total_input <- get_data_total_input(
        iotable,
        endogenize_import = FALSE
      )
      expect_true(all(dplyr::near(
        data_total_input$actual,
        data_total_input$expected
      )))
    } else {
      data_total_input <- get_data_total_input(iotable)
      expect_true(all(dplyr::near(
        data_total_input$actual,
        data_total_input$expected
      )))
    }

    # Total check works
    iotable_wrong_total_input <- iotable_dummy[[name]] |>
      dplyr::mutate(
        value = dplyr::if_else(
          output_sector_type == "total",
          value + 1 / 3,
          value
        )
      )
    expect_error(
      io_table(
        iotable_wrong_total,
        competitive_import = competitive_import
      )
    )

    iotable_wrong_total_output <- iotable_dummy[[name]] |>
      dplyr::mutate(
        value = dplyr::if_else(
          output_sector_type == "total",
          value + 1 / 7,
          value
        )
      )
    expect_error(
      io_table(
        iotable_wrong_total,
        competitive_import = competitive_import
      )
    )
  }
})
