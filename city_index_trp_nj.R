# Find eligible TRPs and toll stations

# Setup ----
{
  base::Sys.setlocale(locale = "nb.utf8")
  library(tidyverse)
  library(writexl)
  library(paletteer)
  library(leaflet)
  library(sf)
  source("get_from_trafficdata_api.R")
  source("get_from_nvdb_api.R")
  source("H:/Programmering/R/byindeks/traffic_link_functions.R")
  source("H:/Programmering/R/byindeks/leaflet_nvdb_map_setup.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
}

# Prospective TRPs ----
trp_latest_data <- get_trps_latest_data()

trp_prospective <-
  get_points() |>
  dplyr::group_by(trp_id) |>
  dplyr::slice(which.min(validFrom)) |>
  dplyr::ungroup() |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    municipality_no %in% c(1103, 1124, 1108, 1127)
  ) |>
  split_road_system_reference() |>
  dplyr::left_join(
    trp_latest_data,
    by = "trp_id"
  ) |>
  dplyr::mutate(
    latest_day = lubridate::floor_date(latest_data_by_hour)
  ) |>
  dplyr::select(
    trp_id,
    name,
    county_geono,
    county_name,
    municipality_name,
    road_reference,
    road_category,
    road_category_and_number,
    lat, lon,
    valid_from = validFrom,
    latest_day
  ) |>
  dplyr::mutate(
    road_category =
      dplyr::case_when(
        road_category == "E" ~ "R",
        road_category == "R" ~ "R",
        road_category == "F" ~ "F"
      )
  )

## AADT ----
# Coverage and length_quality
trp_aadt_raw <-
  trp_prospective$trp_id |>
  get_aadt_for_trp_list()

# Setting some criterias to filter out real candidates
trp_aadts <-
  trp_aadt_raw |>
  dplyr::filter(
    !(year %in% c(2020, 2021)),
    year > 2014,
    coverage > 50
  ) |>
  # Check last year for normality with previous years
  dplyr::mutate(
    median_aadt = median(adt, na.rm = TRUE),
    .by = trp_id
  ) |>
  dplyr::mutate(
    normal_year = adt > median_aadt * 0.75 & adt < median_aadt * 1.25,
    valid_length_percent = round(valid_length_volume / adt * 100, digits = 1)
  ) |>
  dplyr::filter(
    year == 2023
    #coverage > 95,
    #adt >= 200,
    #valid_length_percent > 99,
    #normal_year == TRUE
    # Criterias two strong, some will come out abnormal due to road changes before 2023 (irrelevant here)
    # This look at AADT boils down to whether the TRP has a value in 2023 at all
  ) |>
  dplyr::select(
    trp_id,
    adt
  )

# Points in links not in aadt list
# links_nj_trp_no_adt <-
#   links_nj_trp |>
#   sf::st_drop_geometry() |>
#   dplyr::filter(
#     !(point_id %in% trp_aadts$trp_id)
#   )


# Using the central links as defined in index_representativeness.R
# Need only those with a TRP
links_nj_trp <-
  readr::read_rds("representativity/link_population_nj.rds") |>
  dplyr::filter(
    !is.na(point_id),
    point_id %in% trp_aadts$trp_id,
    !(point_id %in% c(
      "89561V320633", # Tjelta, nedlagt
      "83652V319725" # Strandgata nord, bygges om til bussvei med gjennomkjøring forbudt
    ))
  )# |>
  #dplyr::mutate(
  #  trp_label = paste(name, road_reference, sep = "<br/>"),
  #  trp_label = lapply(trp_label, htmltools::HTML)
  #)

readr::write_rds(
  links_nj_trp,
  "representativity/nj_trp_links.rds"
)


# Need the point geometry for separate map layer
points_nj_trp <-
  links_nj_trp |>
  sf::st_drop_geometry() |>
  dplyr::select(point_id)


trp_metadata <-
  get_trp_metadata_by_list(points_nj_trp$point_id) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    lat, lon
  ) |>
  dplyr::distinct() |>
  split_road_system_reference() |>
  dplyr::select(
    point_id = trp_id,
    name,
    road_category,
    road_reference,
    lat, lon
  ) |>
  dplyr::mutate(
    trp_label = paste(name, road_reference, sep = "<br/>"),
    trp_label = lapply(trp_label, htmltools::HTML)
  )

trp_metadata |>
  dplyr::select(
    -trp_label
  ) |>
  writexl::write_xlsx(
    "spesialuttak/nj_trps.xlsx"
  )

readr::write_rds(
  trp_metadata,
  "representativity/nj_trps.rds"
)


# Toll stations
municipality_ids_nj <- c(1103, 1124, 1108)
# Without Randaberg, which doesn't have any (would give error)
# Turns out, only a few have data in both directions, and only three of these are eligible (on Buøy)

toll_stations <-
  purrr::map(
    municipality_ids_nj,
    ~ get_tolling_stations(.)
  ) |>
  purrr::list_rbind() |>
  split_road_system_reference() |>
  dplyr::select(
    point_id = nvdb_id,
    name,
    road_category,
    road_reference,
    lat, lon
  ) |>
  dplyr::filter(
    # Choosing only eligible ones
    point_id %in% c(
      1013292386,
      1013292378,
      1013292473
    )
  )



