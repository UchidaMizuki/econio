test_that("io_production_induce returns the expected components", {
  # Noncompetitive import: final demand and export inducement only.
  for (name in c(
    "regional_noncompetitive_import",
    "multiregional_noncompetitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    pi <- io_production_induce(iotable)
    expect_setequal(names(pi), c("total_final_demand", "total_export"))
  }

  # Competitive import, closed economy: import inducement is also returned.
  for (name in c(
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    pi <- io_production_induce(iotable, open_economy = FALSE)
    expect_setequal(
      names(pi),
      c("total_final_demand", "total_export", "total_import")
    )

    # Competitive import, open economy: no separate import column.
    pi_open <- io_production_induce(iotable, open_economy = TRUE)
    expect_setequal(names(pi_open), c("total_final_demand", "total_export"))
  }
})

test_that("io_self_sufficiency_rate is only defined for competitive import tables", {
  for (name in c(
    "regional_noncompetitive_import",
    "multiregional_noncompetitive_import"
  )) {
    expect_error(io_self_sufficiency_rate(read_iotable_dummy(name)))
  }

  for (name in c(
    "regional_competitive_import",
    "multiregional_competitive_import"
  )) {
    iotable <- read_iotable_dummy(name)

    rate_summary <- io_self_sufficiency_rate(iotable, summary = TRUE)
    expect_length(as.numeric(rate_summary), 1L)
    expect_true(is.finite(as.numeric(rate_summary)))

    rate <- io_self_sufficiency_rate(iotable, summary = FALSE)
    expect_equal(names(dimnames(rate)), "output")
    expect_true(all(is.finite(as.numeric(rate))))
  }
})

test_that("io_ghosh_inverse returns an output-by-input matrix", {
  for (name in c(
    "regional_competitive_import",
    "regional_noncompetitive_import"
  )) {
    iotable <- read_iotable_dummy(name)
    g <- io_ghosh_inverse(iotable)
    expect_equal(names(dimnames(g)), c("output", "input"))
    expect_true(all(is.finite(as.numeric(as.matrix(g)))))
  }
})
