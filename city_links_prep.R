source("rmd_setup.R")

traffic_links_all <-
  sf::st_read(
    "C:/Users/snohan/Documents/traffic_links_2022.geojson",
    as_tibble = TRUE
  ) |>
  dplyr::select(
    nvdb_id = nvdbId,
    year = yearAppliesTo,
    primary_trp_id = primaryTrpId,
    associated_trp_ids = associatedTrpIds,
    #traffic_volumes = trafficVolumes,
    road_category = roadCategory,
    municipality_ids = municipalityIds,
    length
  )


# Tromsø ----
traffic_links_tromso <-
  traffic_links_all |>
  dplyr::filter(
    purrr::map_lgl(municipality_ids, ~ 5401 %in% .x)
  )

# TODO: trafikklenker på de viktigste kommunale vegene

readr::write_rds(
  traffic_links_tromso,
  "city_links/city_links_tromso.rds"
)


trp_info_tromso <-
  readr::read_rds(
    file = paste0(
      "index_trp_metadata/trp_",
      16952, # TODO: change this to the new city index
      ".rds"
    )
  ) |>
  dplyr::mutate(
    year_aadt = as.character(year_aadt)
  )
