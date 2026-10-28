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
    expect_length(
      which(
        io_sector_type(
          dimnames(iotable_dummy_noncompetitive_import)$output$sector
        ) ==
          "import"
      ),
      0
    )
  }
})

test_that("io_table_to_noncompetitive_import() uses data_import", {
  for (name in c(
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    iotable_noncompetitive_import <- io_table_to_noncompetitive_import(iotable)
    import <- get_data_regional_demand(iotable) -
      get_data_regional_demand(iotable_noncompetitive_import)
    expect_equal(
      io_table_to_noncompetitive_import(iotable, data_import = import),
      iotable_noncompetitive_import
    )

    import_uniform <- get_data_import_uniform(import)
    actual <- io_table_to_noncompetitive_import(
      iotable,
      data_import = import_uniform
    )
    expect_equal(
      get_data_regional_demand(actual),
      get_data_regional_demand(iotable) - import_uniform
    )
  }
})

test_that("io_table_to_competitive_import() with data_import inverts io_table_to_noncompetitive_import()", {
  for (name in c(
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    import <- get_data_regional_demand(iotable) -
      get_data_regional_demand(io_table_to_noncompetitive_import(iotable))
    import <- get_data_import_uniform(import)

    actual <- iotable |>
      io_table_to_noncompetitive_import(data_import = import) |>
      io_table_to_competitive_import(data_import = import)
    expect_equal(
      as.vector(as.matrix(get_data_regional_demand(actual))),
      as.vector(as.matrix(get_data_regional_demand(iotable)))
    )
    expect_equal(
      as.vector(io_total_output(actual, output_sector_type = "import")),
      as.vector(io_total_output(iotable, output_sector_type = "import"))
    )
  }
})

test_that("io_table_to_*_import() abort when data_import totals do not match", {
  iotable <- read_iotable_dummy("multiregional_competitive_import")
  import <- get_data_regional_demand(iotable) -
    get_data_regional_demand(io_table_to_noncompetitive_import(iotable))

  expect_snapshot(
    io_table_to_noncompetitive_import(iotable, data_import = import * 2),
    error = TRUE
  )
})

test_that("io_table_to_byproduct_transfer() returns data unchanged when there are no negative transactions", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  expect_identical(io_table_to_byproduct_transfer(iotable), iotable)
})

test_that("io_table_to_byproduct_transfer() transfers a negative transaction to the reverse cell", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  iotable_negative <- iotable
  iotable_negative[1, 3] <- -10

  weight <- io_table_to_byproduct_transfer(iotable_negative) |>
    io_inter_industry() |>
    as.matrix()

  expect_equal(sum(weight < 0), 0)
  expect_equal(weight[1, 3], 0)
  expect_equal(weight[3, 1], 13)
})

test_that("io_table_to_byproduct_transfer() handles negative transactions in both directions", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  iotable_negative <- iotable
  iotable_negative[1, 3] <- -10
  iotable_negative[3, 1] <- -4

  weight <- io_table_to_byproduct_transfer(iotable_negative) |>
    io_inter_industry() |>
    as.matrix()

  expect_equal(sum(weight < 0), 0)
  expect_equal(weight[1, 3], 4)
  expect_equal(weight[3, 1], 10)
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

test_that("io_table_to_*() validate their scalar arguments", {
  iotable <- read_iotable_dummy("regional_noncompetitive_import")
  expect_snapshot(
    io_table_to_competitive_import(iotable, import_sector_name = 5),
    error = TRUE
  )
  expect_snapshot(
    io_table_to_noncompetitive_import(iotable, import_total_tolerance = "x"),
    error = TRUE
  )
})
