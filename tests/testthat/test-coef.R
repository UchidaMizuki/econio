test_that("io_input_coef() calculates basic coefficients correctly", {
  for (name in c(
    "regional_noncompetitive_import",
    "regional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    coef <- io_input_coef(iotable)
    expect_true(all(is.finite(as.numeric(as.matrix(coef)))))
    expect_equal(names(dimnames(coef)), c("input", "output"))
  }
})

test_that("io_input_coef() works with open_economy", {
  for (name in c(
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    coef_closed <- io_input_coef(iotable, open_economy = FALSE)
    coef_open <- io_input_coef(iotable, open_economy = TRUE)
    expect_true(all(is.finite(as.numeric(as.matrix(coef_closed)))))
    expect_true(all(is.finite(as.numeric(as.matrix(coef_open)))))
  }
})

test_that("io_output_coef() calculates basic coefficients correctly", {
  for (name in c(
    "regional_noncompetitive_import",
    "regional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    coef <- io_output_coef(iotable)
    expect_true(all(is.finite(as.numeric(as.matrix(coef)))))
    expect_equal(names(dimnames(coef)), c("input", "output"))
  }
})

test_that("io_output_coef() works with open_economy", {
  for (name in c(
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    coef_closed <- io_output_coef(iotable, open_economy = FALSE)
    coef_open <- io_output_coef(iotable, open_economy = TRUE)
    expect_true(all(is.finite(as.numeric(as.matrix(coef_closed)))))
    expect_true(all(is.finite(as.numeric(as.matrix(coef_open)))))
  }
})

test_that("io_import_coef() works for all table types and axes", {
  for (name in c(
    "regional_noncompetitive_import",
    "multiregional_noncompetitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    coef_input <- io_import_coef(iotable, axis = "input")
    coef_output <- io_import_coef(iotable, axis = "output")
    expect_true(all(is.finite(as.numeric(as.matrix(coef_input)))))
    expect_true(all(is.finite(as.numeric(as.matrix(coef_output)))))
  }

  for (name in c(
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    coef_input <- io_import_coef(iotable, axis = "input")
    coef_output <- io_import_coef(iotable, axis = "output")
    expect_true(all(is.finite(as.numeric(as.matrix(coef_input)))))
    expect_true(all(is.finite(as.numeric(as.matrix(coef_output)))))
  }
})

test_that("io_input_coef() and io_output_coef() handle invalid open_economy", {
  iotable_noncomp <- read_iotable_dummy("regional_noncompetitive_import")
  expect_snapshot(
    io_input_coef(iotable_noncomp, open_economy = TRUE),
    error = TRUE
  )
  expect_snapshot(
    io_output_coef(iotable_noncomp, open_economy = FALSE),
    error = TRUE
  )

  iotable_comp <- read_iotable_dummy("regional_competitive_import")
  expect_snapshot(
    io_input_coef(iotable_comp, open_economy = "yes"),
    error = TRUE
  )
  expect_snapshot(
    io_output_coef(iotable_comp, open_economy = c(TRUE, FALSE)),
    error = TRUE
  )
})
