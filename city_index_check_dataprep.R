# Point metadata from Traffic Data API
get_points() |>
  dplyr::distinct(trp_id, .keep_all = T) |>
  split_road_system_reference() |>
  dplyr::filter(
    registration_frequency == "CONTINUOUS"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    road_category_and_number,
    county_name,
    municipality_name,
    lat, lon, road_link_position,
    traffic_type
  ) |>
  readr::write_rds(
    "trps_for_city_index.rds"
  )
