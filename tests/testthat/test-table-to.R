test_that("io_table_to_competitive_import() works", {
  names <- c(
    "regional_noncompetitive_import",
    "regional_competitive_import",
    "multiregional_noncompetitive_import",
    "multiregional_competitive_import"
  )

  for (name in names) {
    iotable_dummy <- read_iotable_dummy(name)
    iotable_dummy_competitive_import <- io_table_to_competitive_import(
      iotable_dummy
    )

    expect_s3_class(
      iotable_dummy_competitive_import,
      "io_table_competitive_import"
    )
    expect_true(all(dplyr::near(
      io_total_output(iotable_dummy_competitive_import),
      io_total_output(iotable_dummy)
    )))
    expect_true(all(dplyr::near(
      io_total_input(iotable_dummy_competitive_import),
      io_total_input(iotable_dummy)
    )))
  }
})

test_that("io_table_to_noncompetitive_import() works", {
  names <- c(
    "regional_noncompetitive_import",
    "regional_competitive_import",
    "multiregional_noncompetitive_import",
    "multiregional_competitive_import"
  )

  for (name in names) {
    iotable_dummy <- read_iotable_dummy(name)
    iotable_dummy_noncompetitive_import <- io_table_to_noncompetitive_import(
      iotable_dummy
    )

    expect_s3_class(
      iotable_dummy_noncompetitive_import,
      "io_table_noncompetitive_import"
    )
    expect_true(all(dplyr::near(
      io_total_output(iotable_dummy_noncompetitive_import),
      io_total_output(iotable_dummy)
    )))
    expect_true(all(dplyr::near(
      io_total_input(iotable_dummy_noncompetitive_import),
      io_total_input(iotable_dummy)
    )))
  }
})

test_that("io_table_to_regional() works", {
  names <- c(
    "multiregional_noncompetitive_import",
    "multiregional_competitive_import"
  )

  for (name in names) {
    iotable_dummy <- read_iotable_dummy(name)
    total_output <- io_total_output(iotable_dummy) |>
      tibble::as_tibble()
    total_input <- io_total_input(iotable_dummy) |>
      tibble::as_tibble()

    iotable_dummy_regional <- io_table_to_regional(iotable_dummy)
    total_output_regional <- iotable_dummy_regional |>
      purrr::imap(\(x, i) {
        io_total_output(x) |>
          tibble::as_tibble() |>
          dplyr::mutate(
            input = input |>
              tibble::add_column(region = i, .before = 1)
          )
      }) |>
      dplyr::bind_rows()
    total_input_regional <- iotable_dummy_regional |>
      purrr::imap(\(x, i) {
        io_total_input(x) |>
          tibble::as_tibble() |>
          dplyr::mutate(
            output = output |>
              tibble::add_column(region = i, .before = 1)
          )
      }) |>
      dplyr::bind_rows()

    expect_equal(total_output_regional, total_output)
    expect_equal(total_input_regional, total_input)
  }
})
