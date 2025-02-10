# The purpose of this script is to revise TRPs used in VTI, comprising:
# a. Remove TRPs that won't give us any index data this coming year
# b. Add some more TRPs to compensate

# We want three files:
# 1. RDS with traffic links for TRP candidates, to be shown in map.
# 2. RDS with table summarising n TRP and measured traffic work per county and road category.
# 3. Excel file with TRPs to keep and add to VTI.

{
  source("H:/Programmering/R/byindeks/rmd_setup.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  library(writexl)
}


# Get data ----
## Counties ----
counties <- get_counties()


## Point index last year ----
trps_last_year <- get_published_pointindex_for_months_paginated(962, 2024, 12)[[1]]

# Fetch once and save?
# readr::write_rds(
#   trps_last_year,
#   file = "new_vti/trps_last_year.rds"
# )


## TRP ----
trp_latest_data <- get_trps_latest_data()

trp_for_vti <-
  get_points() |>
  dplyr::group_by(trp_id) |>
  dplyr::slice(which.min(validFrom)) |>
  dplyr::ungroup() |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    operational_status == "OPERATIONAL"
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
      ),
    used_last_year =
      dplyr::case_when(
        trp_id %in% trps_last_year ~ TRUE,
        TRUE ~ FALSE
      )
  ) |>
  dplyr::filter(
    road_category %in% c("F", "R", "K"),
    valid_from < "2024-01-01",
    latest_day > "2025-02-01"
    #stringr::str_detect(road_reference, "SD", negate = TRUE),
    #stringr::str_detect(road_reference, "KD", negate = TRUE)
  )


## AADT ----
# Coverage and length_quality previous year
trp_aadt_raw <-
  trp_for_vti$trp_id |>
  get_aadt_for_trp_list()

readr::write_rds(
  trp_aadt_raw,
  file = "trp_aadt.rds"
)

remove(trp_aadt_raw)

trp_aadts <-
  readr::read_rds("trp_aadt.rds") |>
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
    year == 2024,
    coverage > 95,
    adt >= 200,
    valid_length_percent > 99,
    normal_year == TRUE
  ) |>
  dplyr::select(
    trp_id,
    adt
  )


## Traffic links ----
# Geojson from ADM
# All links on R and F
links <-
  sf::read_sf(
    "C:/Users/snohan/Desktop/traffic_links_2024_2025-02-10.geojson",
    query =
      "
      SELECT
        id,
        countyIds,
        roadCategory,
        roadSystemReferences,
        functionClass,
        hasOnlyPublicTransportLanes,
        isFerryRoute,
        associatedTrpIds,
        trafficVolumes
      FROM \"traffic_links_2024_2025-02-10\"
      "
  ) |>
  dplyr::rename(,
    road_category = roadCategory
  ) |>
  dplyr::mutate(
    road_category =
      dplyr::case_when(
        road_category == "Europaveg" ~ "R",
        road_category == "Riksveg" ~ "R",
        road_category == "Fylkesveg" ~ "F",
        TRUE ~ road_category
      )
  ) |>
  dplyr::filter(
    road_category %in% c("R", "F"),
    hasOnlyPublicTransportLanes == FALSE,
    isFerryRoute == FALSE
  ) |>
  dplyr::select(
    -hasOnlyPublicTransportLanes,
    -isFerryRoute
  )


## County ID ----
# Keep only one county id for border crossing ones
link_county_id <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    countyIds
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    county_id_single = purrr::pluck(countyIds, 1, 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    id,
    county_id = county_id_single
  )


## Traffic work ----
traffic_work_per_link <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    trafficVolumes
  ) |>
  dplyr::filter(
    !(is.na(trafficVolumes)),
    !(trafficVolumes == "[ ]")
  ) |>
  dplyr::mutate(
    volumes = purrr::map(trafficVolumes, ~ jsonlite::fromJSON(.x))
  ) |>
  tidyr::unnest(
    volumes
  ) |>
  dplyr::filter(
    trafficVolumeType == "GUESSTIMATED",
    year == 2023,
    trafficVolumeResolution == "ADT",
    trafficWorkValue > 0
  ) |>
  dplyr::select(
    id,
    traffic_work_km = trafficWorkValue
  )


## TRP on link ----
# All links and TRP ids
link_trp_id <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    associatedTrpIds
  ) |>
  dplyr::filter(
    !(associatedTrpIds == "[ ]")
  ) |>
  dplyr::mutate(
    trp_id = purrr::map(associatedTrpIds, ~ jsonlite::fromJSON(.x))
  ) |>
  tidyr::unnest(
    trp_id
  ) |>
  dplyr::select(
    -associatedTrpIds
  ) |>
  dplyr::filter(
    trp_id %in% trp_aadts$trp_id
  ) #|>
  # Some links will have two TRPs (one per direction)
  #dplyr::mutate(
  #  n_id = n(),
  #  .by = id
  #)


# Tidy ----
## Tidy TRPs ----
trp_for_vti_tidy <-
  trp_for_vti |>
  dplyr::inner_join(
    trp_aadts,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    name,
    county_geono,
    county_name,
    municipality_name,
    road_category,
    road_category_and_number,
    road_reference,
    #road_link_position,
    lat, lon,
    used_last_year,
    adt
  ) |>
  dplyr::arrange(
    county_geono,
    road_category
  )

readr::write_rds(
  trp_for_vti_tidy,
  file = "new_vti/trp_for_vti_tidy.rds"
)

#remove(trp_for_vti)

## Tidy links ----
links_tidy <-
  links |>
  dplyr::left_join(
    link_county_id,
    by = join_by(id)
  ) |>
  dplyr::left_join(
    traffic_work_per_link,
    by = join_by(id)
  ) |>
  dplyr::filter(
    !is.na(traffic_work_km)
  ) |>
  dplyr::left_join(
    # Will duplicate som links
    link_trp_id,
    by = join_by(id)
  ) |>
  dplyr::select(
    id,
    county_id,
    road_category,
    roadSystemReferences,
    function_class = functionClass,
    traffic_work_km,
    trp_id
  )

readr::write_rds(
  links_tidy,
  file = "new_vti/links_tidy.rds"
)

#remove(links)


# Read back in ----
# If you are back another day...
trps_last_year <-
  readr::read_rds(
    file = "new_vti/trps_last_year.rds"
  )

trp_for_vti_tidy <-
  readr::read_rds(
    file = "new_vti/trp_for_vti_tidy.rds"
  )

links_tidy <-
  readr::read_rds(
    file = "new_vti/links_tidy.rds"
  )


# TRP chosen ----
# Less work:
# 1. Would like to keep all TRPs from last year, except a few: used_last_year_but_discard
# 2. Would like to not include any new ones, except a few: not_used_last_year_but_include
# Classify all TRPs as keep, discard or add.

# Look at map and decide on which new ones to add
{
source("new_vti/unwanted_vti_trp_2025.R")

trp_for_vti_chosen <-
  trp_for_vti_tidy |>
  dplyr::filter(
    !(trp_id %in% used_last_year_but_discard),
    !(trp_id %in% new_ones_not_to_include$trp_id)
  ) |>
  dplyr::mutate(
    trp_label = paste(name, road_category_and_number, adt, sep = "<br/>"),
    trp_label = lapply(trp_label, htmltools::HTML)
  )

readr::write_rds(
  trp_for_vti_chosen,
  file = "new_vti/vti_trp_chosen.rds"
)
}


## 3. TRPs to add and remove ----
{
trps_last_year_to_keep <-
  trp_for_vti_chosen |>
  dplyr::filter(
    trp_id %in% trps_last_year
  ) |>
  dplyr::mutate(
    change_status = "keep"
  )

trps_last_year_to_discard <-
  tibble::tibble(
    trp_id = trps_last_year
  ) |>
  dplyr::filter(
    !(trp_id %in% trp_for_vti_chosen$trp_id)
  ) |>
  dplyr::left_join(
    trp_for_vti,
    by = "trp_id"
  ) |>
  dplyr::select(
    county_geono,
    county_name,
    trp_id,
    name,
    road_reference
  ) |>
  dplyr::arrange(
    county_geono,
    name
  ) |>
  dplyr::mutate(
    change_status = "discard"
  )

trps_to_add <-
  trp_for_vti_chosen |>
  dplyr::filter(
    !(trp_id %in% trps_last_year)
  ) |>
  dplyr::mutate(
    change_status = "add"
  )

## Write
dplyr::bind_rows(
  trps_last_year_to_keep,
  trps_last_year_to_discard,
  trps_to_add
) |>
  dplyr::select(
    trp_id,
    name,
    county_name,
    municipality_name,
    road_category_and_number,
    change_status
  ) |>
  dplyr::arrange(
    change_status,
    name
  ) |>
  writexl::write_xlsx(
    path = "new_vti/revise_vti_trp_2025.xlsx"
  )
}


## 1. Links chosen ----
{
  links_chosen <-
    links_tidy |>
    dplyr::filter(
      trp_id %in% trp_for_vti_chosen$trp_id
    ) |>
    dplyr::mutate(
      to_keep = dplyr::case_when(
        trp_id %in% trps_last_year_to_keep$trp_id ~ TRUE,
        TRUE ~ FALSE
      )
    )

  readr::write_rds(
    links_chosen,
    file = "new_vti/vti_links_chosen.rds"
  )
}


## Measured traffic work ----
measured_traffic_work <-
  links_chosen |>
  sf::st_drop_geometry() |>
  dplyr::summarise(
    measured_traffic_work_mill_km = sum(aadt * 365 * length) / 1e9,
    .by = c(county_id, road_category)
  ) |>
  dplyr::left_join(
    traffic_work,
    by = c("county_id", "road_category")
  ) |>
  dplyr::mutate(
    measured_percentage_of_traffic_work = round(100 * measured_traffic_work_mill_km / traffic_work_mill_km, 0)
  ) |>
  dplyr::select(
    county_id,
    road_category,
    measured_traffic_work_mill_km,
    measured_percentage_of_traffic_work
  )


## Traffic work stats ----
# traffic_work_stats <-
#   traffic_work_per_link |>
#   dplyr::summarise(
#     traffic_work_mill_km = sum(traffic_work_km) / 1e6,
#     .by = c(county_id, road_category)
#   ) |>
#   dplyr::select(
#     county_id = county_id_single,
#     road_category,
#     traffic_work_mill_km
#   )
#
# readr::write_rds(
#   traffic_work,
#   file = "new_vti/traffic_work_2023.rds"
# )



## 2. Summary tables ----
{
county_table <-
  trp_for_vti_chosen |>
  dplyr::summarise(
    n_trp = n(),
    .by = c(county_no, road_category)
  ) |>
  dplyr::rename(
    county_id = county_no
  ) |>
  dplyr::left_join(
    counties,
    by = dplyr::join_by(county_id == county_number)
  ) |>
  dplyr::select(
    county_id,
    county_name,
    road_category,
    n_trp
  ) |>
  dplyr::left_join(
    traffic_work,
    by = dplyr::join_by(county_id, road_category)
  ) |>
  dplyr::left_join(
    measured_traffic_work,
    by = dplyr::join_by(county_id, road_category)
  )

readr::write_rds(
  county_table,
  file = "new_vti/county_table.rds"
)

country_table <-
  county_table |>
  dplyr::summarise(
    n_trp = sum(n_trp),
    traffic_work_mill_km = sum(traffic_work_mill_km),
    measured_traffic_work_mill_km = sum(measured_traffic_work_mill_km),
    .by = c(road_category)
  ) |>
  dplyr::mutate(
    measured_percentage_of_traffic_work = round(100 * measured_traffic_work_mill_km / traffic_work_mill_km, 0)
  )

readr::write_rds(
  country_table,
  file = "new_vti/country_table.rds"
)
}
