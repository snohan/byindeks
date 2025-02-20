# The purpose of this script is to revise TRPs used in VTI, comprising:
# a. Discard TRPs that won't give us any index data this coming year
# b. Add some more TRPs to compensate
# c. Look at traffic work distribution in function classes (with statistical distances)

# We want three files:
# OUTPUT#1. RDS with traffic links for TRP candidates, to be shown in map and visualized with traffic work distribution.
# OUTPUT#2. RDS with table summarising n TRP and measured traffic work per county and road category.
# OUTPUT#3. Excel file with TRPs to keep, discard and add to VTI.

{
  source("H:/Programmering/R/byindeks/rmd_setup.R")
  source("H:/Programmering/R/byindeks/split_road_system_reference.R")
  source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
  library(writexl)
}


# Get data ----
## Counties ----
counties <-
  get_counties() |>
  dplyr::rename(
    county_id = county_number,
    county_geono = geo_number
  )
# Should use county_geono throughout


## TRP candidates ----
# Fetch once and save
{
  trps_last_year <- get_published_pointindex_for_months_paginated(962, 2024, 12)[[1]]

  readr::write_rds(
    trps_last_year,
    file = "new_vti/trps_last_year.rds"
  )
}
# Read back in
trps_last_year <-
  readr::read_rds(
    file = "new_vti/trps_last_year.rds"
  )

trp_latest_data <- get_trps_latest_data()

# NB! Do not filter anything more here as we need to catch those used last year (some may be lost in subsequent trp dfs)
# Supposedly none have changed registration_frequency
trp_for_vti <-
  get_points() |>
  dplyr::group_by(trp_id) |>
  dplyr::slice(which.min(validFrom)) |>
  dplyr::ungroup() |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS"
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

readr::write_rds(
  trp_for_vti,
  file = "new_vti/trp_for_vti.rds"
)

### AADT ----
# Coverage and length_quality previous year
trp_aadt_raw <-
  trp_for_vti$trp_id |>
  get_aadt_for_trp_list()

readr::write_rds(
  trp_aadt_raw,
  file = "trp_aadt.rds"
)

remove(trp_aadt_raw)

# Setting some criterias to filter out real candidates for next year
trp_aadts <-
  readr::read_rds("trp_aadt.rds") |>
  dplyr::filter(
    !(year %in% c(2020, 2021)),
    year > 2014,
    coverage > 66
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
    "C:/Users/snohan/Desktop/traffic_links_2024_2025-02-13.geojson",
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
      FROM \"traffic_links_2024_2025-02-13\"
      "
  ) |>
  dplyr::rename(,
    road_category = roadCategory,
    function_class = functionClass
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


# Some links may be missing function class
links_na <-
  links |>
  dplyr::filter(
    is.na(function_class)
  )

### County ID ----
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
    # Just picking the one at the top (random?)
    county_id_single = purrr::pluck(countyIds, 1, 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    id,
    county_id = county_id_single
  )


### Traffic work ----
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
# All links, function class and TRP ids
link_trp_id <-
  links |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    associatedTrpIds,
    function_class
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
  # Filter by candidates based on available data
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
  # Filter by available data (very generally)
  dplyr::filter(
    operational_status == "OPERATIONAL",
    road_category %in% c("F", "R"),
    valid_from < "2024-01-01",
    latest_day > "2025-02-15"
    #stringr::str_detect(road_reference, "SD", negate = TRUE),
    #stringr::str_detect(road_reference, "KD", negate = TRUE)
  ) |>
  # Filter by available data (more specifically)
  dplyr::inner_join(
    trp_aadts,
    by = "trp_id"
  ) |>
  # Add link id and function class
  dplyr::left_join(
    link_trp_id,
    by = dplyr::join_by(trp_id)
  ) |>
  dplyr::mutate(
    # Before looking at candidate TRPs, it is either to keep (from last year) or a candidate (maybe add)
    use_in_vti =
      dplyr::case_when(
        trp_id %in% trps_last_year ~ "keep",
        TRUE ~ "candidate"
      ),
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
    function_class,
    lat, lon,
    use_in_vti,
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
    # Will duplicate some links
    link_trp_id |>
      dplyr::select(-function_class),
    by = join_by(id)
  ) |>
  dplyr::left_join(
    counties,
    by = join_by(county_id)
  ) |>
  dplyr::select(
    id,
    county_id,
    county_geono,
    county_name,
    road_category,
    roadSystemReferences,
    function_class,
    traffic_work_km,
    trp_id
  )

readr::write_rds(
  links_tidy,
  file = "new_vti/links_tidy.rds"
)

remove(links)


# Read back in ----
# If you are back another day...
trp_for_vti <-
  readr::read_rds(
    file = "new_vti/trp_for_vti.rds"
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
# Would like to keep all TRPs from last year, except a few: used_last_year_but_discard
# Would like to not add any new ones, except a few: new_ones_to_add
# These are specified in "unwanted_vti_trp_2025.R"

# Classify all TRPs as either one of
# - keep
# - discard
# - add
# - candidate (left as candidates)

# Look at map and decide on which new ones to add
{
source("new_vti/unwanted_vti_trp_2025.R")

trp_for_vti_chosen <-
  trp_for_vti_tidy |>
  dplyr::filter(
    !(trp_id %in% used_last_year_but_discard),
    !(trp_id %in% declined_candidates$trp_id)
  ) |>
  dplyr::mutate(
    # After having considered candidates by looking at map by county, status can change
    # keep <- (discard, keep)
    # candidate <- (add, candidate)
    use_in_vti =
      dplyr::case_when(
        #trp_id %in% used_last_year_but_discard ~ "discard",
        trp_id %in% new_ones_to_add ~ "add",
        TRUE ~ use_in_vti
      ),
    trp_label = paste(name, road_category_and_number, function_class, adt, sep = "<br/>"),
    trp_label = lapply(trp_label, htmltools::HTML)
  )

readr::write_rds(
  trp_for_vti_chosen,
  file = "new_vti/vti_trp_chosen.rds"
)
}


# OUTPUT#3 TRPs ----
# Can not just use trp_for_vti_chosen here as some of the ones used last year may have been lost in all the filtering of candidates for next year
# These need not be shown in map, but they must be in the list which is being used to update the selection in the GUI
# keep and add are complete, but discard must be complemented by looking at the list of those used last year
{
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
    municipality_name,
    trp_id,
    name,
    road_category_and_number
  ) |>
  dplyr::mutate(
    use_in_vti = "discard"
  )

## Write
dplyr::bind_rows(
  trp_for_vti_chosen |>
    dplyr::select(
      county_geono,
      county_name,
      municipality_name,
      trp_id,
      name,
      road_category_and_number,
      use_in_vti
    ),
  trps_last_year_to_discard
) |>
  dplyr::arrange(
    use_in_vti,
    name
  ) |>
  writexl::write_xlsx(
    path = "new_vti/revise_vti_trp_2025.xlsx"
  )
}


# OUTPUT#1 Links ----
{
  # The ones we want to see in map
  trps_chosen_but_not_the_discarded <-
    trp_for_vti_chosen |>
    dplyr::filter(
      !(use_in_vti == "discard"),
      !(trp_id %in% declined_candidates$trp_id)
    ) |>
    dplyr::select(
      trp_id,
      use_in_vti
    )

  links_chosen <-
    links_tidy |>
    dplyr::mutate(
      # Need to keep all links in order to calculate traffic work ratios
      # Retain only trp_ids that are keep, add or candidate
      trp_id =
        dplyr::case_when(
          trp_id %in% trps_chosen_but_not_the_discarded$trp_id ~ trp_id,
          TRUE ~ NA_character_
        )#,
      #to_keep = dplyr::case_when(
      #  trp_id %in% trps_last_year_to_keep$trp_id ~ TRUE,
      #  TRUE ~ FALSE
      #)
    ) |>
    dplyr::left_join(
      trps_chosen_but_not_the_discarded,
      by = dplyr::join_by(trp_id)
    )

  readr::write_rds(
    links_chosen,
    file = "new_vti/vti_links_chosen.rds"
  )
}


# OUTPUT#2 Summary ----
# Measured traffic work
# Traffic work stats
{
traffic_work_stats <-
  traffic_work_per_link |>
  dplyr::left_join(
    links_tidy |>
      sf::st_drop_geometry() |>
      dplyr::select(
        id,
        county_geono,
        road_category
      ),
    by = join_by(id)
  ) |>
  dplyr::summarise(
    traffic_work_mill_km = sum(traffic_work_km) / 1e6,
    .by = c(county_geono, road_category)
  ) |>
  dplyr::select(
    county_geono,
    road_category,
    traffic_work_mill_km
  ) |>
  dplyr::arrange(
    county_geono
  )

readr::write_rds(
  traffic_work_stats,
  file = "new_vti/traffic_work_2023.rds"
)

measured_traffic_work <-
  links_chosen |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    !is.na(trp_id)
  ) |>
  dplyr::summarise(
    measured_traffic_work_mill_km = sum(traffic_work_km) / 1e6,
    .by = c(county_geono, road_category)
  ) |>
  dplyr::left_join(
    traffic_work_stats,
    by = c("county_geono", "road_category")
  ) |>
  dplyr::mutate(
    measured_percentage_of_traffic_work = round(100 * measured_traffic_work_mill_km / traffic_work_mill_km, 0)
  ) |>
  dplyr::select(
    county_geono,
    road_category,
    measured_traffic_work_mill_km,
    measured_percentage_of_traffic_work
  ) |>
  dplyr::arrange(
    county_geono
  )

county_table <-
  trp_for_vti_chosen |>
  dplyr::summarise(
    n_trp = n(),
    .by = c(county_geono, road_category)
  ) |>
  dplyr::left_join(
    counties,
    by = dplyr::join_by(county_geono)
  ) |>
  dplyr::select(
    county_geono,
    county_name,
    road_category,
    n_trp
  ) |>
  dplyr::left_join(
    traffic_work_stats,
    by = dplyr::join_by(county_geono, road_category)
  ) |>
  dplyr::left_join(
    measured_traffic_work,
    by = dplyr::join_by(county_geono, road_category)
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
