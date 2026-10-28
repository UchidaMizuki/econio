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

test_that("io_reclass() aggregates regions", {
  region_data <- tibble::tribble(
    ~from      , ~to    , ~weight ,
    "region_2" , "rest" ,       1 ,
    "region_3" , "rest" ,       1
  )

  for (name in c(
    "multiregional_noncompetitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    iotable_aggregated <- io_reclass(
      iotable,
      input_region_data = region_data,
      output_region_data = region_data
    )

    expect_s3_class(iotable_aggregated, "io_table_multiregional")
    expect_equal(
      vctrs::vec_unique(dimnames(iotable_aggregated)$input$region),
      c("region_1", "rest")
    )
    expect_equal(
      as_long_tibble(io_total_output(iotable_aggregated)),
      as_long_tibble(io_total_output(iotable)) |>
        dplyr::mutate(
          input_region = dplyr::if_else(
            input_region == "region_1",
            "region_1",
            "rest"
          )
        ) |>
        dplyr::summarise(
          value = sum(value),
          .by = c("input_region", "input_sector_type", "input_sector_name")
        )
    )
  }
})

test_that("io_reclass() splits regions and aggregates them back", {
  region_data <- tibble::tribble(
    ~from      , ~to   , ~weight ,
    "region_1" , "r1a" , 0.4     ,
    "region_1" , "r1b" , 0.6
  )
  region_data_inverse <- region_data |>
    dplyr::rename(to = from, from = to) |>
    dplyr::mutate(weight = 1)

  for (name in c(
    "multiregional_noncompetitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    iotable_split <- io_reclass(
      iotable,
      input_region_data = region_data,
      output_region_data = region_data
    )

    expect_equal(
      vctrs::vec_unique(dimnames(iotable_split)$input$region),
      c("r1a", "r1b", "region_2", "region_3")
    )
    expect_equal(
      io_reclass(
        iotable_split,
        input_region_data = region_data_inverse,
        output_region_data = region_data_inverse
      ),
      iotable
    )
  }
})

test_that("io_reclass() validates its scalar arguments", {
  iotable <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(io_reclass(iotable, weight_tolerance = "x"), error = TRUE)
  expect_snapshot(io_reclass(iotable, check_axes = "yes"), error = TRUE)
  expect_snapshot(io_reclass(iotable, from_col = 1), error = TRUE)
})
