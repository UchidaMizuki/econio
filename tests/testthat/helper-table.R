get_data_total_output <- function(iotable) {
  input_coef <- io_input_coef(iotable)
  total_input <- io_total_input(iotable)
  list(
    actual = input_coef %*%
      total_input +
      io_total_output(
        iotable,
        output_sector_type = c("final_demand", "export", "import")
      ),
    expected = io_total_output(iotable)
  )
}

get_data_total_input <- function(iotable, endogenize_import = NULL) {
  leontief_inv <- io_leontief_inverse(
    iotable,
    endogenize_import = endogenize_import
  )

  if (is.null(endogenize_import) || endogenize_import) {
    total_final_demand_export <- io_total_output(
      iotable,
      output_sector_type = c("final_demand", "export")
    )
    final_demand <- io_total_output(
      iotable,
      output_sector_type = "final_demand"
    )
    final_demand_saml_region <- io_total_output(
      iotable,
      output_sector_type = "final_demand",
      same_region = TRUE
    )

    list(
      actual = leontief_inv %*%
        (total_final_demand_export -
          io_import_coef(iotable) * final_demand_saml_region),
      expected = io_total_input(iotable)
    )
  } else {
    total_final_demand_export_import <- io_total_output(
      iotable,
      output_sector_type = c("final_demand", "export", "import")
    )

    list(
      actual = leontief_inv %*% total_final_demand_export_import,
      expected = io_total_input(iotable)
    )
  }
}

read_iotable_dummy <- function(name) {
  name <- rlang::arg_match(
    name,
    c(
      "regional_noncompetitive_import",
      "regional_competitive_import",
      "multiregional_noncompetitive_import",
      "multiregional_competitive_import"
    )
  )
  iotable_dummy <- readRDS(test_path("data", "iotable_dummy.rds"))

  if (name == "regional_noncompetitive_import") {
    io_table_regional(
      iotable_dummy$regional_noncompetitive_import,
      competitive_import = FALSE
    )
  } else if (name == "regional_competitive_import") {
    io_table_regional(
      iotable_dummy$regional_competitive_import,
      competitive_import = TRUE
    )
  } else if (name == "multiregional_noncompetitive_import") {
    io_table_multiregional(
      iotable_dummy$multiregional_noncompetitive_import,
      competitive_import = FALSE
    )
  } else if (name == "multiregional_competitive_import") {
    io_table_multiregional(
      iotable_dummy$multiregional_competitive_import,
      competitive_import = TRUE
    )
  }
}
