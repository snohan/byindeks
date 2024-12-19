# Evaluate an area for index eligibility.

# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
  source("get_from_nvdb_api.R")
  source("traffic_link_functions.R")
  #source("city_index_check_dataprep.R")
}

trp_meta_data <- readr::read_rds("trps_for_city_index.rds")

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


# Area ----
# Use municipalities first, then reduce area as needed "by hand"
# Bodø 1804
# Ålesund 1508
municipality_ids <- c(1508)

# Must reduce Ålesund area
aalesund_polygon <-
  tibble::tibble(
    lon = c(
      6.3695755,
      6.3855143,
      6.1019591,
      6.0859549
    ),
    lat = c(
      62.4898613,
      62.4264377,
      62.4161364,
      62.4805069
    )
  ) |>
  tibble::rowid_to_column("id") |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("POLYGON")


#aalesund_polygon
link_population_raw_3 |>
  leaflet(
    width = "100%",
    height = 700,
    options =
      leafletOptions(
        crs = nvdb_crs,
        zoomControl = F)
  ) |>
  addTiles(
    urlTemplate = nvdb_map_url,
    attribution = nvdb_map_attribution
  ) |>
  addPolylines(
    opacity = 0.4
  )

link_population_raw <-
  get_link_population(municipality_ids) |>
  dplyr::rowwise() |>
  dplyr::filter(
    #!is.null(unlist(functionalRoadClasses)),
    !is.null(unlist(functionClasses))
  ) |>
  dplyr::mutate(
    #functional_class = min(unlist(functionalRoadClasses)),
    function_class = dplyr::first(unlist(functionClasses)) |> stringr::str_sub(1,1)
  ) |>
  dplyr::select(
    -functionClasses,
    #-functionalRoadClasses,
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    function_class %in% c("A", "B", "C", "D")
  )

if(municipality_ids == 1508) {

  link_population_raw <-
    link_population_raw |>
    sf::st_filter(aalesund_polygon, .predicate = st_intersects)

}

population_location <-
  link_population_raw |>
  dplyr::select(
    id,
    roadSystemReferences,
    startTrafficNodeId,
    endTrafficNodeId
  )

population_traffic <-
  link_population_raw |>
  sf::st_drop_geometry() |>
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

population_features <-
  link_population_raw |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id,
    roadCategory,
    length,
    function_class
  ) |>
  dplyr::left_join(
    population_traffic,
    by = join_by(id)
  )

## TRPs ----
population_trp <-
  link_population_raw |>
  sf::st_drop_geometry() |>
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
    # From list to character
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
  ) |>
  # Is there any toll stations we need?
  dplyr::filter(
    !is.na(point_id)
  )

# TODO: In case of sole toll station, move its ID to trp_id column


# length quality
adt_lmv <-
  get_aadt_by_length_for_trp_list(population_trp$point_id) |>
  dplyr::filter(
    length_range == "[..,5.6)",
    year == 2023
  ) |>
  dplyr::select(
    trp_id,
    year,
    aadt_lmv = aadt_length_range,
    aadt_valid_length,
    aadt_total,
    coverage
  ) |>
  dplyr::mutate(
    length_quality = (aadt_valid_length / aadt_total) |> round(2),
    enough_data = coverage > 0.75 & length_quality > 0.975
  )

eligible_selection <-
  population_trp |>
  dplyr::left_join(
    adt_lmv,
    by = join_by(point_id == trp_id)
  ) |>
  dplyr::filter(
    enough_data
  ) |>
  dplyr::left_join(
    trp_meta_data,
    by = join_by(point_id == trp_id)
  ) |>
  dplyr::select(
    id,
    point_id,
    #coverage,
    #length_quality,
    #enough_data,
    name,
    road_reference,
    aadt_lmv
  ) |>
  dplyr::mutate(
    aadt_lmv = round(aadt_lmv, -1)
  )


population_tidy <-
  link_population_raw |>
  dplyr::select(
    id,
    roadSystemReferences,
    startTrafficNodeId,
    endTrafficNodeId,
    roadCategory,
    length,
    function_class
  ) |>
  dplyr::left_join(
    population_traffic,
    by = join_by(id)
  ) |>
  dplyr::left_join(
    eligible_selection,
    by = join_by(id)
  )


readr::write_rds(
  population_tidy,
  #"new_area_index/links_bdo_2023.rds"
  "new_area_index/links_aal_2023.rds"
)

## Toll stations
# All toll stations in Bodø is on or very close to links with TRP, while three are on K-roads. Thus no toll stations will be used for city index.
# TODO: toll station meta data




## Graph metrics ----
# ?