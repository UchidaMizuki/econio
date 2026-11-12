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

get_data_total_input <- function(iotable, open_economy = NULL) {
  leontief_inv <- io_leontief_inverse(
    iotable,
    open_economy = open_economy
  )

  if (is.null(open_economy) || open_economy) {
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

read_iotable_dummy_raw <- function(name) {
  readRDS(test_path("data", "iotable_dummy.rds"))[[name]]
}

# Flatten a dibble into the long format of the raw dummy data, i.e. columns
# such as `input_region`, `input_sector_type`, `input_sector_name`, and `value`.
as_long_tibble <- function(x) {
  x <- tibble::as_tibble(x)
  value <- x[[ncol(x)]]
  axes <- x[-ncol(x)] |>
    purrr::imap(\(axis, name) {
      axis |>
        dplyr::mutate(
          sector_type = as.character(io_sector_type(.data$sector)),
          sector_name = io_sector_name(.data$sector),
          .keep = "unused"
        ) |>
        dplyr::rename_with(\(col) paste(name, col, sep = "_"))
    })
  dplyr::bind_cols(unname(axes), value = value)
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
      import_type = "noncompetitive_import"
    )
  } else if (name == "regional_competitive_import") {
    io_table_regional(
      iotable_dummy$regional_competitive_import,
      import_type = "competitive_import"
    )
  } else if (name == "multiregional_noncompetitive_import") {
    io_table_multiregional(
      iotable_dummy$multiregional_noncompetitive_import,
      import_type = "noncompetitive_import"
    )
  } else if (name == "multiregional_competitive_import") {
    io_table_multiregional(
      iotable_dummy$multiregional_competitive_import,
      import_type = "competitive_import"
    )
  }
}

get_data_regional_demand <- function(iotable) {
  iotable |>
    dplyr::filter(
      io_sector_type(.data$input) == "industry",
      io_sector_type(.data$output) %in% c("industry", "final_demand")
    )
}

# An import table with the same totals by input as `import`, allocated
# uniformly over the outputs within the same region.
get_data_import_uniform <- function(import) {
  same_region <- io_same_region(import)
  total <- dibble::apply(import, "input", sum)
  n_output <- dibble::apply(same_region, "input", sum)
  dibble::broadcast(
    same_region * (total / n_output),
    dim_names = c("input", "output")
  )
}

# A regional competitive import type table whose `industry_1` imports 40, more
# than its regional demand of 35, e.g. because of re-exports. Its export and
# import columns can be changed while keeping total output at 35.
get_iotable_reexport <- function(
  export_1 = c(export = 30, outflow = 10),
  import_1 = c(import = -30, inflow = -10),
  final_demand_1 = 20
) {
  output <- tibble::tribble(
    ~output_sector_type , ~output_sector_name ,
    "industry"          , "industry_1"        ,
    "industry"          , "industry_2"        ,
    "final_demand"      , "final_demand_1"    ,
    "export"            , "export"            ,
    "export"            , "outflow"           ,
    "import"            , "import"            ,
    "import"            , "inflow"
  )
  value <- list(
    industry_1 = c(10, 5, final_demand_1, export_1, import_1),
    industry_2 = c(4, 6, 30, 5, 5, -10, -5),
    value_added_1 = c(21, 24, 0, 0, 0, 0, 0)
  )
  value |>
    purrr::imap(\(value, input_sector_name) {
      output |>
        dplyr::mutate(
          input_sector_type = sub("_[0-9]+$", "", input_sector_name),
          input_sector_name = input_sector_name,
          value = unname(value),
          .before = 1
        )
    }) |>
    purrr::list_rbind() |>
    io_table_regional(import_type = "competitive_import")
}

# industry_1 has imports of 20 but regional demand of -5.
get_iotable_negative_demand <- function() {
  get_iotable_reexport(
    export_1 = c(export = 45, outflow = 15),
    import_1 = c(import = -15, inflow = -5),
    final_demand_1 = -20
  )
}
