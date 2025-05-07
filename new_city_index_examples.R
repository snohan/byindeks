# Calculate city index using new method

# New concepts:
# - Using only MDT (normal situation, seasonally adjusted)
# - Traffic work weights (need traffic links)
# - Chaining when necessary (need to suitably subdivide index period, possibly one road net version per subperiod)
# - Estimate confidence interval (compare and decide which method to use)
# - Measures of representativity

# To be included later:
# - Seasonally adjusted MDT
# - Vehicle classification by type, not just length

# Resolution in time:
# - Month by month
# - So far this year
# - Last 12 months
# - Last 24 months
# - Last 36 months

# Resolution in day type
# - working days
# - non-working days
# - all days

# Resolution in vehicle type:
# - light (short)
# - heavy (long)
# - all

# How to compare:
# - new versus old index results in different time resolutions, separated from new chaining strategies
# - the impact of new chaining strategies leading to better representativity


# What are isolated improvements?
# - measures of representativity, especially traffic work
# - smaller (?) confidence interval, mostly (?) due to finiteness of population
# - traffic work weights

# Suitable cities as examples:
# - Bergen (chaining might be useful)
# - Buskerudbyen (has had representativity issues, no chaining?)
# - Trondheim (representativity issues, no chaining?)
# - Nord-Jæren (with new chaining strategy)


{
  base::Sys.setlocale(locale = "nb.utf8")
  svv_background_color <- "#F5F5F5"

  library(tidyverse)

  source("split_road_system_reference.R")
  source("indexpoints_tidying_functions.R")
  source("index_report_functions.R")
  source("get_from_trafficdata_api.R")
  source("traffic_link_functions.R")
}

# Bergen 8952
city_number <- "8952"
source("city_reference_year.R")

# TRP
this_citys_trps_all_adt_final <-
  readr::read_rds(
    file = paste0(
      "index_trp_metadata/trp_",
      city_number,
      ".rds"
    )
  ) |>
  dplyr::filter(
    stringr::str_sub(road_category_and_number, 1, 1) != "K"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    municipality_name,
    #lat, lon,
    adt, year_aadt, adt_ref
  )

# MDT
mdt_filtered <-
  readr::read_rds(
    paste0(
      "data_indexpoints_tidy/mdt_",
      city_number,
      ".rds"
    )
  )

# TODO: heatmap per TRP per month, one for each year

# Link population
# Made on script city_link_population.R

