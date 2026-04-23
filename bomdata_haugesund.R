#
source("apar.R")

# Toll stations were changed 03th March 2026
# Before: Haugalandspakken
tolling_station_ids_apar <- c(1, 2, 5:9)

# Fetch all data for all trp_ids for a month, and store
month_string <- "january" # English!
year_number <- 2025

apar_data_for_month <-
  purrr::map_dfr(
    tolling_station_ids_apar,
    ~ get_apar_data(
        dataset_id = haugesund_apar_id_haugalandspakken,
        station_code = .,
        month_string = month_string,
        year_number = year_number
    )
  )


# After: Bypakke Haugesund og fastlands-Karmøy
tolling_station_ids_apar <- c(1:11)

# Fetch all data for all trp_ids for a month, and store
month_string <- "april" # English!
year_number <- 2026

apar_data_for_month <-
  purrr::map_dfr(
    tolling_station_ids_apar,
    ~ get_apar_data(
        dataset_id = haugesund_apar_id_bypakke_haugesund,
        station_code = .,
        month_string = month_string,
        year_number = year_number
    )
  )
