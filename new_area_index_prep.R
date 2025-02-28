# Evaluate an area for index eligibility.

# Setup ----
{
  source("rmd_setup.R")
  source("get_from_trafficdata_api.R")
  source("get_from_nvdb_api.R")
  source("traffic_link_functions.R")
}


# Prerequisites ----
## TRP meta data ----
# From city_index_check_dataprep.R
#source("city_index_check_dataprep.R")
trp_meta_data <- readr::read_rds("trps_for_city_index.rds")

## Traffic links ----
# From traffic_link_prep.R
links <- readr::read_rds("traffic_link_pop/links_raw.rds")

link_trp_id <-
  readr::read_rds("traffic_link_pop/link_trp_id.rds") |>
  dplyr::filter(
    trp_id %in% trp_continuous$trp_id
  )

link_municipality_id <-
  readr::read_rds("traffic_link_pop/link_municipality_id.rds")

traffic_volumes <-
  readr::read_rds("traffic_link_pop/link_traffic_volumes.rds") |>
  dplyr::filter(
    trafficVolumeType == "GUESSTIMATED",
    trafficVolumeResolution == "ADT",
    year == 2023
  ) |>
  dplyr::select(
    link_id,
    aadt = trafficVolumeValue,
    traffic_work_km = trafficWorkValue
  )


## Urban areas ----
# Downloaded fgdb file from Geonorge: tettsteder, UTM33

#urban_layers <- sf::st_layers("C:/Users/snohan/Desktop/tettsteder_2024.gdb")

urban_areas <-
  sf::st_read(
    "C:/Users/snohan/Desktop/tettsteder_2024.gdb",
    as_tibble = TRUE,
    #layer = "tettsted",
    query =
      "SELECT
      tettstednummer, tettstednavn, totalbefolkning, SHAPE
      FROM \"tettsted\"
      WHERE totalbefolkning > 1000
      "
  ) |>
  dplyr::group_by(tettstednummer, tettstednavn, totalbefolkning) |>
  dplyr::summarise(
    geometry = sf::st_union(SHAPE),
    .groups = "drop"
  )


# Cities ----
# Use municipalities first, then reduce area as needed "by hand" or urban polygons

## Bodø ----
municipality_ids <- 1804

urban_area_bodo <-
  urban_areas |>
  dplyr::filter(
    tettstednummer %in% c(7501, 7502, 7503)
  ) |>
  sf::st_transform("wgs84")

urban_area_bodo_convex_hull <-
  urban_area_bodo |>
  dplyr::summarise(
    geometry = sf::st_union(geometry)
  ) |>
  sf::st_convex_hull()


city_link_population_raw <-
  get_link_population_inside_municipalities(municipality_ids) |>
  sf::st_filter(urban_area_bodo_convex_hull, .predicate = st_covered_by) |>
  dplyr::filter(
    function_class %in% c("A", "B", "C", "D")
  )

# HEREON ----



# Ålesund 1508
# Haugesund c(1106, 1149)
# Arendal/Grimstad c(4203, 4202)
# Vestfoldbyen c(3905, 3907, 3909)
# Tønsberg 3905

municipality_ids <-
  #1804
  #1508
  #c(1106, 1149)
  #c(4203, 4202)
  #c(3905, 3907, 3909)
  3905

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

# Must reduce Haugesund are (just the mainland part of Karmøy)
haugesund_polygon <-
  tibble::tibble(
    lon = c(
      5.4023042,
      5.4225312,
      5.3385784,
      5.3236261,
      5.2969286,
      5.2926611,
      5.2236771,
      5.1980626
    ),
    lat = c(
      59.5391317,
      59.2708638,
      59.2651765,
      59.3304398,
      59.3666045,
      59.3846508,
      59.4258514,
      59.5270527
    )
  ) |>
  tibble::rowid_to_column("id") |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("POLYGON")

# For both Arendal/Grimstad and Vestfoldbyen, E18 has high percentage through traffic,
# so it will not be part of population.


# Check polygon on map
#aalesund_polygon
haugesund_polygon |>
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
    !is.null(unlist(functionClasses))
  ) |>
  dplyr::mutate(
    function_class = dplyr::first(unlist(functionClasses)) |> stringr::str_sub(1,1)
  ) |>
  dplyr::select(
    -functionClasses
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

if(1106 %in% municipality_ids) {

  link_population_raw <-
    link_population_raw |>
    sf::st_filter(haugesund_polygon, .predicate = st_intersects)

}

if(4203 | 3905 %in% municipality_ids) {

  link_population_raw <-
    link_population_raw |>
    dplyr::filter(!stringr::str_detect(roadSystemReferences, "EV18")) |>
    dplyr::filter(!stringr::str_detect(roadSystemReferences, "RV19")) # traffic between E18 and Horten, which does not belong to the area of interest

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
  #"new_area_index/links_bdo_2024.rds"
  #"new_area_index/links_aal_2024.rds"
  #"new_area_index/links_hau_2024.rds"
  #"new_area_index/links_arg_2024.rds"
  #"new_area_index/links_vfl_2024.rds"
  "new_area_index/links_tbg_2023.rds"
)

## Toll stations
# TODO: toll station meta data

