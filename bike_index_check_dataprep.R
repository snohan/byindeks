get_points() %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    county_no,
    county_name,
    municipality_name,
    lat, lon,
    road_link_position
  ) %>%
  dplyr::mutate(
    name = stringr::str_to_title(name, locale = "no")
  ) |>
  readr::write_rds(
    "trps.rds"
  )

#writexl::write_xlsx(points, path = "trp.xlsx")