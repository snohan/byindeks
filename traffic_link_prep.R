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
  ) |>
  dplyr::mutate(
    # Adding some missing values
    function_class =
      dplyr::case_when(
        link_id %in% c(
          "0.23461012@444232-0.0@3401843"
        ) ~ "A",
        link_id %in% c(
          "0.02123723-0.84417966@283442",
          "0.84417966-1.0@283442",
          "0.0-1.0@3522080",
          "0.57243185@319730-1.0@3522081",
          "0.0-1.0@3341087",
          "0.0@3600894-1.0@443647",
          "0.0-1.0@3342537"
        ) ~ "B",
        link_id %in% c(
          "0.33524051-1.0@3607094",
          "0.0-0.33524051@3607094",
          "0.0-1.0@3444182"
        ) ~ "C",
        link_id %in% c(
          "0.0-0.59197863@443783",
          "0.0@3449804-1.0@443768",
          "0.0@443768-1.0@3341657",
          "0.52129651@3381269-1.0@3857826",
          "0.0-0.56199619@443881",
          "0.0@3447686-1.0@443882"
        ) ~ "D",
        TRUE ~ function_class
      )
  )

readr::write_rds(
  links_raw_bare,
  "traffic_link_pop/links_raw.rds"
)

# TRP ids ----
trp_continuous <-
  get_points() |>
  dplyr::filter(
    registration_frequency == "CONTINUOUS"
  ) |>
  dplyr::select(
    trp_id
  ) |>
  dplyr::distinct()

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
  dplyr::filter(
    trp_id %in% trp_continuous$trp_id
  ) |>
  dplyr::select(
    link_id,
    trp_id
  ) |>
  # Adding some missing ones
  dplyr::bind_rows(
    tibble::tibble(
      link_id = c(
        "0.14008637@3144542-0.15493155@805693" # Rådal
      ),
      trp_id = c(
        "12021V805693"
      )
    )
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


# Link weights ----
link_id_weights_2024 <-
  links_raw_bare |>
  dplyr::right_join(
    link_traffic_2024,
    by = dplyr::join_by(link_id)
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id, length_m, tw
  ) |>
  dplyr::left_join(
    link_trp_id,
    by = dplyr::join_by(link_id)
  )

readr::write_rds(
  link_id_weights_2024,
  "traffic_link_pop/link_id_weights_2024.rds"
)
