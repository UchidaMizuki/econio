test_that("io_reclass() works", {
  names <- c(
    "regional_noncompetitive_import",
    "regional_competitive_import",
    "multiregional_noncompetitive_import",
    "multiregional_competitive_import"
  )

  for (name in names) {
    iotable_dummy <- read_iotable_dummy(name)

    expect_equal(io_reclass(iotable_dummy), iotable_dummy)

    input_sector_data <- tibble::tribble(
      ~from        , ~to            , ~weight ,
      "industry_2" , "industry_2_1" , 0.25    ,
      "industry_2" , "industry_2_2" , 0.75
    )
    input_sector_data_inverse <- input_sector_data |>
      dplyr::rename(to = from, from = to) |>
      dplyr::mutate(weight = 1)
    output_sector_data <- tibble::tribble(
      ~from        , ~to            , ~weight ,
      "industry_3" , "industry_3_1" , 0.75    ,
      "industry_3" , "industry_3_2" , 0.25
    )
    output_sector_data_inverse <- output_sector_data |>
      dplyr::rename(to = from, from = to) |>
      dplyr::mutate(weight = 1)

    expect_equal(
      io_reclass(
        iotable_dummy,
        input_sector_data = input_sector_data,
        output_sector_data = output_sector_data,
        check_axes = FALSE
      ) |>
        io_reclass(
          input_sector_data = input_sector_data_inverse,
          output_sector_data = output_sector_data_inverse
        ),
      iotable_dummy
    )
    expect_error(
      io_reclass(
        iotable_dummy,
        input_sector_data = input_sector_data |>
          dplyr::mutate(weight = .data$weight * 2)
      )
    )
  }
})
