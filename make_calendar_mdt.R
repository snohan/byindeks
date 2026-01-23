# Make calendar adjusted MDT for use in index calculations

# From before:
# - get MDT from API
# - filter for short vehicles, necessary columns and add trp data
# THIS IS SAVED AS "data_indexpoints_tidy/mdt_CITYID.rds"
# - exclusions stored in csv
# - use in calculations where minimum criteria are applied

# Now:
# - get DT from API
# - do the calendar adjustments, including minimum criteria and calculate cMDT
# SAVE AS cmdt_CITYID.rds in data_indexpoints_tidy

# Setup ----
{
  source("get_from_trafficdata_api.R")
  source("calendar_functions.R")
  library(tictoc)
}


# City numbers ----
# Refy City  Cnum
# 2017 NJær   952
# 2018 Oslo   959
# 2018 Berg  8952
# 2019 Tron   960

{
  present_year <- 2025
  index_month <- 12
  city_number <- "960"
}

# City years
source("set_time_references.R")


# City TRPs
city_trps <-
  get_published_pointindex_for_months(city_number, max(index_years), 1)[[1]] |>
  base::sort()

if(city_number == "959") {
  city_trps <-
    city_trps |>
    stringr::str_subset("18012V444303", negate = TRUE) |>
    stringr::str_subset("18573V444291", negate = TRUE)
}

## Nord-Jæren more TRPs
if(city_number == "952") {
  link_trp_id <- readr::read_rds("traffic_link_pop/link_trp_id.rds")

  trps_existing <-
    link_trp_id |>
    dplyr::filter(
      link_id %in% links_in_area$link_id
    )

  city_trps <- trps_existing$trp_id
}

# First, make cMDT per TRP and store them in folder cMDT
# cMDT
# trp_number <- 88

for(i in 1:length(city_trps)) {
  
  # tic()
  print(paste0(i, ": ", city_trps[i]))

  cmdt <-
    purrr::map(
      # From the beginning
      # years_from_reference_to_today,
      # Or just add new data
      2025,
      ~ calculate_calendar_adjusted_mdt(city_trps[i], .x)
    ) |>
    purrr::list_rbind()

  cmdt |>
    readr::write_rds(
      file =
        paste0(
          "cmdt/cmdt_",
          city_number,
          "_",
          city_trps[i],
          # For adding new data in a separate file
          "_2025",
          ".rds"
        )
    )

  # toc()
}


# Then gather all cMDT per city in an rds file
cmdt_city <-
  purrr::map(
    list.files("cmdt", paste0("cmdt_", city_number), full.names = TRUE),
    ~ readr::read_rds(.x)
  ) |>
  purrr::list_rbind()

cmdt_city |>
  readr::write_rds(
    file =
      paste0(
        "data_indexpoints_tidy/cmdt_",
        city_number,
        ".rds"
      )
  )
