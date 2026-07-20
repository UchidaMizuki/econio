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
    import_type <- switch(
      name,
      regional_noncompetitive_import = "noncompetitive_import",
      regional_competitive_import = "competitive_import",
      multiregional_noncompetitive_import = "noncompetitive_import",
      multiregional_competitive_import = "competitive_import"
    )
    import_type_flipped <- switch(
      import_type,
      competitive_import = "noncompetitive_import",
      noncompetitive_import = "competitive_import"
    )

    expect_snapshot(
      io_table(
        iotable_dummy[[name]],
        import_type = import_type_flipped
      ),
      error = TRUE
    )
    iotable <- io_table(
      iotable_dummy[[name]],
      import_type = import_type
    )

    data_total_output <- get_data_total_output(iotable)
    expect_true(all(dplyr::near(
      data_total_output$actual,
      data_total_output$expected
    )))

    if (import_type == "competitive_import") {
      data_total_input <- get_data_total_input(
        iotable,
        open_economy = TRUE
      )
      expect_true(all(dplyr::near(
        data_total_input$actual,
        data_total_input$expected
      )))

      data_total_input <- get_data_total_input(
        iotable,
        open_economy = FALSE
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
    expect_snapshot(
      io_table(
        iotable_wrong_total_input,
        import_type = import_type
      ),
      error = TRUE
    )

    iotable_wrong_total_output <- iotable_dummy[[name]] |>
      dplyr::mutate(
        value = dplyr::if_else(
          output_sector_type == "total",
          value + 1 / 7,
          value
        )
      )
    expect_snapshot(
      io_table(
        iotable_wrong_total_output,
        import_type = import_type
      ),
      error = TRUE
    )
  }
})

test_that("io_table_regional() validates its scalar arguments", {
  iotable_dummy <- readRDS(test_path("data", "iotable_dummy.rds"))
  data <- iotable_dummy$regional_competitive_import
  expect_snapshot(
    io_table_regional(
      data,
      import_type = "competitive_import",
      check_axes = "yes"
    ),
    error = TRUE
  )
  expect_snapshot(
    io_table_regional(
      data,
      import_type = "competitive_import",
      total_tolerance = -1
    ),
    error = TRUE
  )
  expect_snapshot(
    io_table_regional(data, import_type = "yes"),
    error = TRUE
  )
})

test_that("io_check_totals() validates total_tolerance", {
  iotable <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(io_check_totals(iotable, total_tolerance = "x"), error = TRUE)
})

test_that("io_check_axes() detects axis mismatches", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  dim_names <- dimnames(iotable)
  output_industry_sector <- dim_names$output |>
    dplyr::filter(io_sector_type(.data$sector) == "industry") |>
    dplyr::slice(1)
  dim_names$output <- dim_names$output |>
    dplyr::anti_join(output_industry_sector, by = "sector")
  iotable_mismatch <- dibble::broadcast(iotable, dim_names = dim_names)
  expect_snapshot(io_check_axes(iotable_mismatch), error = TRUE)
})
