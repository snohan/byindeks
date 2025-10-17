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
# 2016 Busk  1952
# 2016 Gren   955
# 2017 NJær   952
# 2018 Oslo   959
# 2018 Berg  8952
# 2019 Tron   960

{
  present_year <- 2024
  index_month <- 12
  city_number <- "959"
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
link_trp_id <- readr::read_rds("traffic_link_pop/link_trp_id.rds")

trps_existing <-
  link_trp_id |>
  dplyr::filter(
    link_id %in% links_nj$link_id
  )

city_trps <- trps_existing$trp_id

# First, make cMDT per TRP and store them in folder cMDT

# cMDT
trp_number <- 88

{
  tic()
  cmdt <-
    purrr::map(
      years_from_reference_to_today,
      ~ calculate_calendar_adjusted_mdt(city_trps[trp_number], .x)
    ) |>
    purrr::list_rbind()

  cmdt |>
    readr::write_rds(
      file =
        paste0(
          "cmdt/cmdt_",
          city_number,
          "_",
          city_trps[trp_number],
          ".rds"
        )
    )

  toc()
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


# mdt_old <- read_rds("data_indexpoints_tidy/mdt_8952.rds")
