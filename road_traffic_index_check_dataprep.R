# If none published so far this year, use draft csv
# trp_index_january <-
#   read.csv2(
#     "new_vti/punktindeks-2022-01.csv"
#   ) %>%
#   dplyr::rename(
#     trp_id = trpid,
#     day_type = 'døgn'
#   ) %>%
#   dplyr::filter(
#     day_type == "Alle",
#     periode == "Januar",
#     dekning > 0
#   )
#
# road_traffic_trps <-
#   trp_index_january %>%
#   dplyr::distinct(trp_id, .keep_all = T)
#
# pointindices <- data.frame() # No published so far

# Skip to S1
# Else use API

# Fetch
get_published_road_traffic_index_for_months(
    index_id,
    index_year,
    index_month
  ) |>
  readr::write_rds(
    "road_traffic_index_latest.rds"
  )

get_published_pointindex_for_months_paginated(
    index_id,
    index_year,
    index_month
  ) |>
  readr::write_rds(
    "road_traffic_pointindex_latest.rds"
  )

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

get_counties() |>
  readr::write_rds(
    "counties.rds"
  )