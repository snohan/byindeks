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

  links_with_trp <-
    links |>
    sf::st_drop_geometry() |>
    dplyr::select(
      id,
      associatedTrpIds
    ) |>
    tidyr::unnest(associatedTrpIds) |>
    dplyr::filter(
      associatedTrpIds %in% trps_meta$trp_id
    ) |>
    dplyr::distinct() |>
    dplyr::rename(
      this_county_trp = associatedTrpIds
    )
}
