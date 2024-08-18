get_data_total_output <- function(iotable) {
  input_coef <- io_input_coef(iotable)
  total_input <- io_total_input(iotable)
  list(object = input_coef %*% total_input + io_total_output(iotable, c("final_demand", "export", "import")),
       expected = io_total_output(iotable))
}

get_data_total_input <- function(iotable,
                                 endogenize_import = TRUE) {
  leontief_inv <- io_leontief_inverse(iotable,
                                      endogenize_import = endogenize_import)

  if (endogenize_import) {
    total_final_demand_export <- io_total_output(iotable,
                                                 output_sector_type = c("final_demand", "export"))
    final_demand <- io_total_output(iotable,
                                    output_sector_type = "final_demand")
    final_demand_saml_region <- io_total_output(iotable,
                                                output_sector_type = "final_demand",
                                                same_region = TRUE)

    list(object = leontief_inv %*% (total_final_demand_export - io_import_coef(iotable) * final_demand_saml_region),
         expected = io_total_input(iotable))
  } else {
    total_final_demand_export_import <- io_total_output(iotable,
                                                        output_sector_type = c("final_demand", "export", "import"))

    list(object = leontief_inv %*% total_final_demand_export_import,
         expected = io_total_input(iotable))
  }
}
