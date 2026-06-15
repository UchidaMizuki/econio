test_that("skyline tidy data has the expected structure", {
  for (name in c("regional_competitive_import", "multiregional_competitive_import")) {
    iotable <- read_iotable_dummy(name)

    sky <- broom::tidy(iotable, type = "skyline")

    expect_s3_class(sky, "tbl_df")
    expect_true(all(
      c("sector", "total", "xmin", "xmax", "rate_type", "rate", "ymin", "ymax") %in%
        names(sky)
    ))
    expect_setequal(
      levels(sky$rate_type),
      c("import_rate", "self_sufficiency_rate")
    )

    # Two rows (one per rate type) for each bar.
    bars <- sky |>
      dplyr::distinct(dplyr::across(
        !c("rate_type", "rate", "ymin", "ymax", "yend")
      ))
    expect_equal(nrow(sky), 2 * nrow(bars))
    expect_equal(
      unname(as.integer(table(sky$rate_type))),
      c(nrow(bars), nrow(bars))
    )

    # The bars are stacked left to right without gaps or overlaps.
    expect_true(all(dplyr::near(bars$xmax - bars$xmin, bars$total)))
    expect_true(all(diff(bars$xmin) >= 0))
    expect_true(all(dplyr::near(bars$xmax[-nrow(bars)], bars$xmin[-1])))

    # Stacked rates are non-negative and ordered.
    expect_true(all(sky$ymin >= 0 | dplyr::near(sky$ymin, 0)))
    expect_true(all(sky$ymax >= sky$ymin))
  }
})

test_that("skyline autoplot and autolayer return ggplot objects", {
  iotable <- read_iotable_dummy("regional_competitive_import")

  expect_s3_class(ggplot2::autoplot(iotable, type = "skyline"), "ggplot")

  layer_rect <- ggplot2::autolayer(iotable, type = "skyline", geom = "rect")
  expect_s3_class(layer_rect, "Layer")

  layer_segment <- ggplot2::autolayer(iotable, type = "skyline", geom = "segment")
  expect_type(layer_segment, "list")
  expect_true(all(purrr::map_lgl(layer_segment, \(x) inherits(x, "Layer"))))
})

test_that("skyline is not defined for noncompetitive import tables", {
  for (name in c("regional_noncompetitive_import", "multiregional_noncompetitive_import")) {
    iotable <- read_iotable_dummy(name)

    expect_error(broom::tidy(iotable, type = "skyline"))
    expect_error(ggplot2::autoplot(iotable, type = "skyline"))
  }
})

test_that("tidy/autoplot reject unknown types", {
  iotable <- read_iotable_dummy("regional_competitive_import")

  expect_error(broom::tidy(iotable, type = "not_a_type"))
  expect_error(ggplot2::autoplot(iotable, type = "not_a_type"))
})
