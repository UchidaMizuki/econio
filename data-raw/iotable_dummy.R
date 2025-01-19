source("data-raw/setup.R")

# iotable_dummy -----------------------------------------------------------

read_iotable_dummy_regional <- function(file) {
  col_names <- read_csv(file,
                        col_names = FALSE,
                        col_types = cols(.default = "c"),
                        n_max = 2,
                        name_repair = "minimal") |>
    t() |>
    as_tibble(.name_repair = ~str_c("col_name_", 1:2)) |>
    slice(-(1:2)) |>
    unite("col_name", 1:2,
          sep = "/") |>
    pull(col_name)

  read_csv(file,
           col_names = c("input_sector_type", "input_sector_name", col_names),
           col_types = cols(.default = "c"),
           skip = 2) |>
    pivot_longer(!starts_with("input"),
                 names_to = c("output_sector_type", "output_sector_name"),
                 names_sep = "/",
                 values_transform = list(value = parse_number))
}

read_iotable_dummy_multiregional <- function(file) {
  col_names <- read_csv(file,
                        col_names = FALSE,
                        col_types = cols(.default = "c"),
                        n_max = 3,
                        name_repair = "minimal") |>
    t() |>
    as_tibble(.name_repair = ~str_c("col_name_", 1:3)) |>
    slice(-(1:3)) |>
    unite("col_name", 1:3,
          sep = "/") |>
    pull(col_name)

  read_csv(file,
           col_names = c("input_region", "input_sector_type", "input_sector_name", col_names),
           col_types = cols(.default = "c"),
           skip = 3) |>
    pivot_longer(!starts_with("input"),
                 names_to = c("output_region", "output_sector_type", "output_sector_name"),
                 names_sep = "/",
                 values_transform = list(value = parse_number))
}

dir <- "data-raw/iotable_dummy"
iotable_dummy <- list(regional_noncompetitive_import = read_iotable_dummy_regional(path(dir, "regional-noncompetitive_import", ext = "csv")),
                      regional_competitive_import = read_iotable_dummy_regional(path(dir, "regional-competitive_import", ext = "csv")),
                      multiregional_noncompetitive_import = read_iotable_dummy_multiregional(path(dir, "multiregional-noncompetitive_import", ext = "csv")),
                      multiregional_competitive_import = read_iotable_dummy_multiregional(path(dir, "multiregional-competitive_import", ext = "csv")))
write_rds(iotable_dummy, test_path("data", "iotable_dummy.rds"))
