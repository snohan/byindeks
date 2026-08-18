# Time ----
present_year <- 2026
index_month <- 7
source("set_time_references.R")


# TRPs ----
this_citys_trps_all_adt_final <-
  readr::read_rds(
    file = paste0("index_trp_metadata/trp_", city_number, ".rds")
  ) |>
  dplyr::filter(
    stringr::str_sub(road_category_and_number, 1, 1) != "K"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    municipality_name,
    adt, year_aadt, adt_ref
  )


# Link population ----
# Made in script city_link_population.R
population_size <- nrow(links_in_area)
population_size_tw_kkm <- base::sum(links_in_area$tw) * 1e-3

function_class_tw <-
  links_in_area |>
  sf::st_drop_geometry() |>
  dplyr::select(
    tw_km = tw,
    function_class
  ) |>
  dplyr::summarise(
    tw_fcl_population_kkm = base::sum(tw_km) * 1e-3,
    n_links = n(),
    .by = function_class
  ) |>
  dplyr::mutate(
    tw_fcl_population_share = tw_fcl_population_kkm / population_size_tw_kkm
  ) |>
  dplyr::arrange(function_class)


if(city_number == "952") {

  # In order to use more than the 23 TRPs
  trp_weights_trp <-
    links_in_area |>
    sf::st_drop_geometry() |>
    dplyr::left_join(
      link_trp_id,
      by = "link_id"
    ) |>
    dplyr::filter(
      !is.na(trp_id),
      # Remove those that are to be replaced by toll stations
      !(trp_id %in% c("03108V320583", "84064V320581", "13433V319582", "71787V2269011", "59675V319722"))
    )  |>
    dplyr::select(
      link_id, trp_id,
      length_m,
      trp_tw_ref_kkm = tw,
      function_class
    )

  trp_weights_toll <-
    links_in_area |>
    sf::st_drop_geometry() |>
    dplyr::left_join(
      link_toll_id,
      by = "link_id"
    ) |>
    dplyr::filter(
      !is.na(toll_id),
      # Remove those that are to be replaced by TRPs
      !(toll_id %in% c("906727259", "906727260", "906727269", "906727253", "906727243", "906727236", "906727240", "906727247", "906727233", "906727265", "906727251"))
    ) |>
    dplyr::select(
      link_id, trp_id = toll_id,
      length_m,
      trp_tw_ref_kkm = tw,
      function_class
    ) 
  
  toll_stations_not_on_link <-
    bomstasjoner_nj |> 
    dplyr::filter(
      !(nvdb_id %in% link_toll_id$toll_id)
    )
    
  trp_weights <-
    dplyr::bind_rows(
      trp_weights_trp,
      trp_weights_toll
    ) |>
    dplyr::mutate(
      length_m = base::round(length_m),
      trp_tw_ref_kkm = trp_tw_ref_kkm * 1e-3
    ) |>
    dplyr::left_join(
      function_class_tw,
      by = "function_class"
    )  
  
  trp_info <-
    dplyr::bind_rows(
      bomstasjoner_nj |> dplyr::filter(nvdb_id %in% trp_weights$trp_id) |> dplyr::select(-trp_id) |> dplyr::rename(trp_id = nvdb_id),
      points |> dplyr::filter(trp_id %in% trp_weights$trp_id) |> dplyr::select(-county_name)
    )

}else{

  trp_weights <-
    links_in_area |>
    sf::st_drop_geometry() |>
    dplyr::filter(
      !is.na(point_id)
    ) |>
    dplyr::select(
      trp_id = point_id,
      length_m,
      trp_tw_ref_kkm = tw,
      function_class
    ) |>
    dplyr::mutate(
      length_m = base::round(length_m),
      trp_tw_ref_kkm = trp_tw_ref_kkm * 1e-3
    ) |>
    dplyr::left_join(
      function_class_tw,
      by = "function_class"
    )
}


# MDT ----
mdt_filtered <-
  readr::read_rds(
    paste0("data_indexpoints_tidy/cmdt_", city_number, ".rds")
  ) |>
  dplyr::filter(length_class == "korte")

# To get the mdt_validated df
source("exclude_cmdt.R")
