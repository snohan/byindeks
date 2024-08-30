# Filter traffic links

filter_traffic_links_by_county <- function(county_number) {

  link_ids_in_county <-
    links |>
    sf::st_drop_geometry() |>
    dplyr::select(
      id,
      countyIds
    ) |>
    tidyr::unnest(countyIds) |>
    dplyr::filter(
      countyIds %in% county_number
    ) |>
    dplyr::distinct()

  links_in_county <-
    links |>
    dplyr::filter(
      id %in% link_ids_in_county$id
    )

  return(links_in_county)
}


filter_links_with_trp <- function() {

  # Both TRP and Toll staions
  links_with_trp <-
    dplyr::bind_rows(
      links |>
        sf::st_drop_geometry() |>
        dplyr::select(
          id,
          associatedTrpIds
        ) |>
        tidyr::unnest(
          # Will duplicate links with more than one TRP
          associatedTrpIds,
          keep_empty = FALSE
        ) |>
        dplyr::rename(
          this_area_trp_id = associatedTrpIds
        ),
      links |>
        sf::st_drop_geometry() |>
        dplyr::select(
          id,
          associatedTollStationIds
        ) |>
        tidyr::unnest(
          # No duplicate
          associatedTollStationIds,
          keep_empty = FALSE
        ) |>
        dplyr::rename(
          this_area_trp_id = associatedTollStationIds
        ) |>
        dplyr::mutate(
          this_area_trp_id = as.character(this_area_trp_id)
        )
    ) |>
    # Narrow down list (duplicates disappear)
    dplyr::filter(
      this_area_trp_id %in% trps_meta$trp_id
    )
}
