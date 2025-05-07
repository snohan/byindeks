# Prepare traffic links for population definition in area index

# Downloaded from Trafikkdata-Adm: urettede trafikklenker med geometri, WGS84.
# All links in the country.

#layers <- sf::st_layers("C:/Users/snohan/Desktop/traffic_links_2023_2024-10-08.geojson")

# Read from file ----
links_raw <-
  sf::st_read(
    #"C:/Users/snohan/Desktop/traffic_links_2024_2025-02-27.geojson",
    "C:/Users/snohan/Desktop/traffic_links_2024_2025-05-07.geojson",
    as_tibble = TRUE,
    query =
      "
      SELECT
        id,
        roadCategory,
        roadSystemReferences,
        length,
        startTrafficNodeId,
        endTrafficNodeId,
        municipalityIds,
        associatedTrpIds,
        tollStationIds,
        hasOnlyPublicTransportLanes,
        isFerryRoute,
        functionClass,
        trafficVolumes
      FROM \"traffic_links_2024_2025-05-07\"
      "
  ) |>
  dplyr::rename(
    link_id = id,
    from = startTrafficNodeId,
    to = endTrafficNodeId,
    function_class = functionClass,
    road_system_references = roadSystemReferences
  ) |>
  dplyr::mutate(
    function_class = as.factor(function_class)
  ) |>
  dplyr::filter(
    hasOnlyPublicTransportLanes == FALSE,
    isFerryRoute == FALSE
  ) |>
  dplyr::select(
    -hasOnlyPublicTransportLanes,
    -isFerryRoute
  )

# Making separate data frames holding the relations between
# link_id and features that can occur multiple times per link.
# Hence, those columns are unnecessary in link df.

# Bare raw links ----
links_raw_bare <-
  links_raw |>
  dplyr::select(
    -length,
    -associatedTrpIds,
    -tollStationIds,
    -trafficVolumes,
    -municipalityIds
  )

readr::write_rds(
  links_raw_bare,
  "traffic_link_pop/links_raw.rds"
)

# TRP ids ----
link_trp_id <-
  links_raw |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id,
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
    link_id,
    trp_id
  )

readr::write_rds(
  link_trp_id,
  "traffic_link_pop/link_trp_id.rds"
)


# Toll station ids ----
link_toll_id <-
  links_raw |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id,
    tollStationIds
  ) |>
  dplyr::filter(
    !(tollStationIds == "[ ]")
  ) |>
  dplyr::mutate(
    toll_id = purrr::map(tollStationIds, ~ jsonlite::fromJSON(.x))
  ) |>
  tidyr::unnest(
    toll_id
  ) |>
  dplyr::select(
    link_id,
    toll_id
  ) |>
  dplyr::mutate(
    toll_id = as.character(toll_id)
  )

readr::write_rds(
  link_toll_id,
  "traffic_link_pop/link_toll_id.rds"
)


# Municipality ids ----
link_municipality_id <-
  links_raw |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id,
    municipalityIds
  ) |>
  tidyr::unnest(
    municipalityIds
  ) |>
  dplyr::select(
    link_id,
    municipality_id = municipalityIds
  )

readr::write_rds(
  link_municipality_id,
  "traffic_link_pop/link_municipality_id.rds"
)


# Traffic volumes, all ----
link_traffic_volumes <-
  links_raw |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id,
    length,
    trafficVolumes
  ) |>
  dplyr::filter(
    !is.na(trafficVolumes)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    traffic_volumes = list(jsonlite::fromJSON(trafficVolumes))
  ) |>
  # Need to remove empty lists before unnesting.
  dplyr::filter(
    purrr::map_int(list(traffic_volumes), ~length(.)) > 0
  ) |>
  tidyr::unnest(
    traffic_volumes
  ) |>
  dplyr::select(
    -trafficVolumes,
    -trafficLinkId
  )

readr::write_rds(
  link_traffic_volumes,
  "traffic_link_pop/link_traffic_volumes.rds"
)

# Traffic volumes, one ----
# A list with just one AADT and TW per link
# Traffic volume type could be either a final one (overridden, model result)
# or a preliminary one (measured, derived, guesstimated)
value_origin_levels <- c("OVERRIDDEN", "MODEL_RESULT")

link_traffic_2024 <-
  link_traffic_volumes |>
  dplyr::filter(
    year == 2024,
    trafficVolumeResolution == "ADT",
    trafficVolumeType %in% value_origin_levels
  ) |>
  dplyr::mutate(
    value_origin_factor =
      base::factor(
        trafficVolumeType,
        levels = value_origin_levels
      ),
    value_origin_numeric = as.numeric(value_origin_factor)
  ) |>
  dplyr::slice_min(
    value_origin_numeric,
    by = link_id
  ) |>
  dplyr::select(
    link_id,
    length_m = length,
    year,
    aadt = trafficVolumeValue,
    tw = trafficWorkValue,
    heavy_ratio = heavyRatio
    #value_origin_factor
  )

# TODO: add estimated standard error from model result?

links_2024 <-
  links_raw_bare |>
  dplyr::right_join(
    link_traffic_2024,
    by = dplyr::join_by(link_id)
  )

readr::write_rds(
  links_2024,
  "traffic_link_pop/link_traffic_2024.rds"
)

missing_links <-
  links_raw_bare |>
  dplyr::filter(
    !(link_id %in% link_traffic_2024$link_id)
  )
# Mostly kommunalveger, and no roads in any of the cities.