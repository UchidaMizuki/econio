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

test_that("io_table_to_competitive_import() keeps imports within their region for multiregional tables", {
  raw <- read_iotable_dummy_raw("multiregional_noncompetitive_import")
  iotable <- read_iotable_dummy("multiregional_noncompetitive_import")

  import <- io_table_to_competitive_import(iotable) |>
    as_long_tibble() |>
    dplyr::filter(output_sector_type == "import")
  expect_all_equal(
    import |>
      dplyr::filter(input_region != output_region) |>
      dplyr::pull(value),
    0
  )
  expect_equal(
    import |>
      dplyr::summarise(value = -sum(value), .by = "output_region"),
    raw |>
      dplyr::filter(
        input_sector_type == "import",
        output_sector_type %in% c("industry", "final_demand")
      ) |>
      dplyr::summarise(value = sum(value), .by = "input_region") |>
      dplyr::rename(output_region = "input_region")
  )
})

test_that("io_table_to_noncompetitive_import() keeps imports within their region for multiregional tables", {
  raw <- read_iotable_dummy_raw("multiregional_competitive_import")
  iotable <- read_iotable_dummy("multiregional_competitive_import")

  import <- io_table_to_noncompetitive_import(iotable) |>
    as_long_tibble() |>
    dplyr::filter(input_sector_type == "import")
  expect_all_equal(
    import |>
      dplyr::filter(input_region != output_region) |>
      dplyr::pull(value),
    0
  )
  expect_equal(
    import |>
      dplyr::summarise(value = sum(value), .by = "input_region"),
    raw |>
      dplyr::filter(
        input_sector_type == "industry",
        output_sector_type == "import"
      ) |>
      dplyr::summarise(value = -sum(value), .by = "output_region") |>
      dplyr::rename(input_region = "output_region")
  )
})

test_that("io_table_to_*() accept data_import for multiregional tables", {
  for (name in c(
    "multiregional_noncompetitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    if (name == "multiregional_noncompetitive_import") {
      io_table_to <- io_table_to_competitive_import
      axis <- "output"
    } else {
      io_table_to <- io_table_to_noncompetitive_import
      axis <- "input"
    }

    regional_demand <- iotable |>
      dplyr::filter(
        io_sector_type(.data$input) == "industry",
        io_sector_type(.data$output) %in% c("industry", "final_demand")
      )
    data_import <- dibble::broadcast(
      regional_demand *
        io_same_region(regional_demand) *
        io_import_coef(iotable, axis = axis),
      dim_names = c("input", "output")
    )

    expect_equal(
      io_table_to(iotable, data_import = data_import),
      io_table_to(iotable)
    )
    expect_snapshot(
      io_table_to(iotable, data_import = data_import * 2),
      error = TRUE
    )
  }
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

test_that("io_table_to_regional() records inter-regional flows for noncompetitive import tables", {
  raw <- read_iotable_dummy_raw("multiregional_noncompetitive_import")
  regional <- read_iotable_dummy("multiregional_noncompetitive_import") |>
    io_table_to_regional(
      export_sector_name = "outflow",
      import_sector_name = "inflow"
    )
  expect_named(regional, c("region_1", "region_2", "region_3"))
  expect_s3_class(regional$region_1, "io_table_regional")
  expect_s3_class(regional$region_1, "io_table_noncompetitive_import")

  region_1 <- as_long_tibble(regional$region_1)
  regional_demand <- raw |>
    dplyr::filter(
      input_sector_type == "industry",
      output_sector_type %in% c("industry", "final_demand")
    )

  expect_equal(
    region_1 |>
      dplyr::filter(
        input_sector_type == "industry",
        output_sector_type %in% c("industry", "final_demand")
      ) |>
      dplyr::select("input_sector_name", "output_sector_name", "value"),
    regional_demand |>
      dplyr::filter(input_region == "region_1", output_region == "region_1") |>
      dplyr::select("input_sector_name", "output_sector_name", "value")
  )
  expect_equal(
    region_1 |>
      dplyr::filter(
        input_sector_name %in% "inflow",
        output_sector_type %in% c("industry", "final_demand")
      ) |>
      dplyr::select("output_sector_name", "value"),
    regional_demand |>
      dplyr::filter(input_region != "region_1", output_region == "region_1") |>
      dplyr::summarise(value = sum(value), .by = "output_sector_name")
  )
  expect_equal(
    region_1 |>
      dplyr::filter(
        input_sector_type == "industry",
        output_sector_name %in% "outflow"
      ) |>
      dplyr::select("input_sector_name", "value"),
    regional_demand |>
      dplyr::filter(input_region == "region_1", output_region != "region_1") |>
      dplyr::summarise(value = sum(value), .by = "input_sector_name")
  )
  expect_equal(
    region_1 |>
      dplyr::filter(
        input_sector_name %in% "import_1",
        output_sector_type %in% c("industry", "final_demand")
      ) |>
      dplyr::select("output_sector_name", "value"),
    raw |>
      dplyr::filter(
        input_region == "region_1",
        input_sector_type == "import",
        output_region == "region_1",
        output_sector_type %in% c("industry", "final_demand")
      ) |>
      dplyr::select("output_sector_name", "value")
  )
})

test_that("io_table_to_regional() records inter-regional flows for competitive import tables", {
  raw <- read_iotable_dummy_raw("multiregional_competitive_import")
  regional <- read_iotable_dummy("multiregional_competitive_import") |>
    io_table_to_regional(
      export_sector_name = "outflow",
      import_sector_name = "inflow"
    )
  expect_named(regional, c("region_1", "region_2", "region_3"))
  expect_s3_class(regional$region_1, "io_table_regional")
  expect_s3_class(regional$region_1, "io_table_competitive_import")

  region_1 <- as_long_tibble(regional$region_1)
  regional_demand <- raw |>
    dplyr::filter(
      input_sector_type == "industry",
      output_sector_type %in% c("industry", "final_demand")
    )

  expect_equal(
    region_1 |>
      dplyr::filter(
        input_sector_type == "industry",
        output_sector_type %in% c("industry", "final_demand")
      ) |>
      dplyr::select("input_sector_name", "output_sector_name", "value"),
    regional_demand |>
      dplyr::filter(output_region == "region_1") |>
      dplyr::summarise(
        value = sum(value),
        .by = c("input_sector_name", "output_sector_name")
      )
  )
  expect_equal(
    region_1 |>
      dplyr::filter(
        input_sector_type == "industry",
        output_sector_name %in% "inflow"
      ) |>
      dplyr::select("input_sector_name", "value"),
    regional_demand |>
      dplyr::filter(input_region != "region_1", output_region == "region_1") |>
      dplyr::summarise(value = -sum(value), .by = "input_sector_name")
  )
  expect_equal(
    region_1 |>
      dplyr::filter(
        input_sector_type == "industry",
        output_sector_name %in% "outflow"
      ) |>
      dplyr::select("input_sector_name", "value"),
    regional_demand |>
      dplyr::filter(input_region == "region_1", output_region != "region_1") |>
      dplyr::summarise(value = sum(value), .by = "input_sector_name")
  )
  expect_equal(
    region_1 |>
      dplyr::filter(
        input_sector_type == "industry",
        output_sector_name %in% "import_1"
      ) |>
      dplyr::select("input_sector_name", "value"),
    raw |>
      dplyr::filter(
        input_region == "region_1",
        input_sector_type == "industry",
        output_region == "region_1",
        output_sector_type == "import"
      ) |>
      dplyr::select("input_sector_name", "value")
  )
})

test_that("io_table_to_regional() errors for regional tables", {
  iotable <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(io_table_to_regional(iotable), error = TRUE)
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
