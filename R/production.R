io_production_induce <- function(data,
                                 endogenize_import = NULL) {
  endogenize_import <- io_endogenize_import(data, endogenize_import)

  leontief_inverse <- io_leontief_inverse(data,
                                          endogenize_import = endogenize_import)
  total_final_demand <- io_total_output(data,
                                        output_sector_type = "final_demand")
  total_export <- io_total_output(data,
                                  output_sector_type = "export")

  if (inherits(data, "io_table_noncompetitive_import")) {
    dibble::dibble(total_final_demand = leontief_inverse %*% total_final_demand,
                   total_export = leontief_inverse %*% total_export)
  } else if (inherits(data, "io_table_competitive_import")) {
    if (endogenize_import) {
      import_coef <- io_import_coef(data)
      dibble::dibble(total_final_demand = leontief_inverse %*% ((1 - import_coef) * total_final_demand),
                     total_export = leontief_inverse %*% total_export)
    } else {
      total_import <- io_total_output(data,
                                      output_sector_type = "import")
      dibble::dibble(total_final_demand = leontief_inverse %*% total_final_demand,
                     total_export = leontief_inverse %*% total_export,
                     total_import = -leontief_inverse %*% total_import)
    }
  }
}

io_self_sufficiency_rate <- function(data,
                                     summary = FALSE) {
  if (!inherits(data, "io_table_competitive_import")) {
    cli::cli_abort("{.fn io_self_sufficiency_rate} is not implemented for {.cls {class(data)}}.")
  }

  production_induce <- io_production_induce(data,
                                              endogenize_import = FALSE)
  total_final_demand <- production_induce$total_final_demand
  total_export <- production_induce$total_export
  total_import <- production_induce$total_import

  if (summary) {
    (sum(total_final_demand) + sum(total_export) - sum(total_import)) / sum(total_final_demand)
  } else {
    (total_final_demand + total_export - total_import) / total_final_demand
  }
}
