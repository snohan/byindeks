# Prepare traffic links for population definition in area index

# Downloaded from Trafikkdata-Adm: urettede trafikklenker med geometri, WGS84.
# All links in the country.

#layers <- sf::st_layers("C:/Users/snohan/Desktop/traffic_links_2023_2024-10-08.geojson")

links_raw <-
  sf::st_read(
    "C:/Users/snohan/Desktop/traffic_links_2024_2025-02-27.geojson",
    as_tibble = TRUE,
    query =
      "
      SELECT
        id,
        roadCategory,
        roadSystemReferences,
        startTrafficNodeId,
        endTrafficNodeId,
        municipalityIds,
        associatedTrpIds,
        tollStationIds,
        hasOnlyPublicTransportLanes,
        isFerryRoute,
        functionClass,
        trafficVolumes
      FROM \"traffic_links_2024_2025-02-27\"
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
# Hence, thos columns are unnecessary in link df.

links_raw |>
  dplyr::select(
    -associatedTrpIds,
    -tollStationIds,
    -trafficVolumes,
    -municipalityIds
  ) |>
  readr::write_rds(
    "traffic_link_pop/links_raw.rds"
  )

# TRPs
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


# Toll stations


# Municipality ids
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


# Traffic volumes
link_traffic_volumes <-
  links_raw |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id,
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