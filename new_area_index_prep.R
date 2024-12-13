# Evaluate an area for index eligibility.

# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
  source("get_from_nvdb_api.R")
  source("traffic_link_functions.R")
  source("city_index_check_dataprep.R")
}

points <- readr::read_rds("trps_for_city_index.rds")

# links_new <-
#   sf::st_read("C:/Users/snohan/Desktop/traffic_links_2023_2024-12-12.geojson") |>
#   dplyr::select(
#     id,
#     roadCategory,
#     roadSystemReferences,
#     functionalRoadClass,
#     functionClass,
#     startTrafficNodeId,
#     endTrafficNodeId,
#     hasOnlyPublicTransportLanes,
#     length,
#     isFerryRoute,
#     associatedTrpIds,
#     tollStationIds,
#     trafficVolumes,
#     municipalityIds
#   ) |>
#   dplyr::filter(
#     hasOnlyPublicTransportLanes == FALSE,
#     isFerryRoute == FALSE
#   ) |>
#   # Unnesting by municipalities will duplicate all links crossing a border
#   tidyr::unnest(municipalityIds) |>
#   dplyr::select(
#     -hasOnlyPublicTransportLanes,
#     -isFerryRoute
#   )

links <-
  sf::st_read("C:/Users/snohan/Desktop/traffic_links_2023_2024-08-06.geojson") |>
  dplyr::select(
    id,
    roadCategory,
    roadSystemReferences,
    functionalRoadClasses,
    functionClasses,
    startTrafficNodeId,
    endTrafficNodeId,
    hasOnlyPublicTransportLanes,
    length,
    isFerryTrafficLink,
    associatedTrpIds,
    associatedTollStationIds,
    trafficVolumes,
    municipalityIds
  ) |>
  dplyr::filter(
    hasOnlyPublicTransportLanes == FALSE,
    isFerryTrafficLink == FALSE
  ) |>
  # Unnesting by municipalities will duplicate all links crossing a border
  tidyr::unnest(municipalityIds) |>
  tidyr::unnest(associatedTollStationIds, keep_empty = TRUE) |>
  dplyr::select(
    -hasOnlyPublicTransportLanes,
    -isFerryTrafficLink
  )


# Area ----
# Use municipalities first, then reduce area as needed "by hand"

## Links ----
get_link_population <- function(area_municipality_ids) {

  # municipality_ids: integer vector

  links |>
    dplyr::filter(
      # If more than one municipality, the list must be deduplicated
      municipalityIds %in% area_municipality_ids
    ) |>
    dplyr::select(
      -municipalityIds
    ) |>
    dplyr::distinct()

}


# Bodø 1804
link_population_bodoe <- get_link_population(1804)

population_geometry <-
  link_population_bodoe |>
  dplyr::select(
    id,
    roadSystemReferences,
    startTrafficNodeId,
    endTrafficNodeId
  )

population_features <-
  link_population_bodoe |>
  sf::st_drop_geometry() |>
  dplyr::rowwise() |>
  dplyr::filter(
    !is.null(unlist(functionalRoadClasses)),
    !is.null(unlist(functionClasses))
  ) |>
  dplyr::mutate(
    functional_class = min(unlist(functionalRoadClasses)),
    function_class = dplyr::first(unlist(functionClasses)) |> stringr::str_sub(1,1)
  ) |>
  dplyr::select(
    -functionClasses,
    -functionalRoadClasses,
    -roadSystemReferences,
    -startTrafficNodeId,
    -endTrafficNodeId
  ) |>
  dplyr::ungroup()

tw <-
  population_features |>
  dplyr::mutate(
    traffic_volumes =
      purrr::map(
        trafficVolumes,
        ~ jsonlite::fromJSON(.)
      )
  )  |>
  tidyr::unnest(
    traffic_volumes
  ) |>
  dplyr::filter(
    year == 2023,
    trafficVolumeResolution == "ADT",
    trafficVolumeType == "GUESSTIMATED"
  ) |>
  dplyr::select(
    id,
    aadt = trafficVolumeValue,
    traffic_work_km = trafficWorkValue
  )


## TRPs ----
trp <-
  population_features |>
  dplyr::mutate(
    traffic_volumes =
      purrr::map(
        trafficVolumes,
        ~ jsonlite::fromJSON(.)
      )
  )  |>
  tidyr::unnest(
    traffic_volumes
  ) |>
  dplyr::filter(
    year == 2023,
    trafficVolumeResolution == "ADT",
    trafficVolumeType != "GUESSTIMATED",
    registrationFrequency == "CONTINUOUS",
    !is.na(sourceType)
  ) |>
  dplyr::select(
    id,
    associatedTrpIds,
    associatedTollStationIds,
    coverage,
    sourceTrpIds
  ) |>
  dplyr::mutate(
    sourceTrpIds = purrr::map(sourceTrpIds, ~ purrr::pluck(., 1)) # NB! What if there is more than one!
  ) |>
  tidyr::unnest(
    sourceTrpIds,
    keep_empty = TRUE
  ) |>
  dplyr::mutate(
    both_trp_and_toll = any(!is.na(sourceTrpIds)) & any(!is.na(associatedTollStationIds)),
    .by = id
  ) |>
  dplyr::filter(
    !(is.na(sourceTrpIds) & both_trp_and_toll)
  ) |>
  dplyr::select(
    id,
    point_id = sourceTrpIds
  )
# TODO: In case of sole toll station, move its ID to trp_id column
# TODO: length quality


## Toll stations
# All toll stations in Bodø is on or very close to links wit TRP, while three are on K-roads. No toll stations will be an asset.
# TODO: toll station meta data


population_bodoe <-
  population_features |>
  dplyr::left_join(
    tw,
    by = join_by(id)
  ) |>
  dplyr::left_join(
    trp,
    by = join_by(id)
  ) |>
  dplyr::select(
    id,
    road_category = roadCategory,
    functional_class,
    function_class,
    length,
    aadt,
    traffic_work_km,
    point_id,
  )


## Graph metrics ----
# ?