adt <- get_aadt_by_length_for_trp_list(city_trps)
  
adt_reference_year <- 
  if(city_number == 16952) {
    2019
  }else{
    reference_year
  }

adt_ref_year <- 
  adt |> 
  dplyr::filter(
    length_range == "[..,5.6)",
    #coverage > 50, # many from NorTraf
    year == adt_reference_year
  ) |> 
  dplyr::select(trp_id, adt_ref = aadt_length_range) |> 
  dplyr::mutate(
    adt_ref = round(adt_ref, -1)
  )
  
if(city_number == 960){
  
  autopass_trp_id <-
    trps_meta |> 
    dplyr::select(
      nvdb_id,
      autopass_id = trp_id
    ) |> 
    dplyr::filter(
      !is.na(nvdb_id)
    )
  
  adt_ref_year_toll <-
    readr::read_rds(
      file = "data_indexpoints_tidy/trd_toll_aadt.rds"
    ) |> 
    dplyr::filter(
      year == 2019,
      class == "lette"
    ) |> 
    dplyr::select(trp_id, adt_ref = aadt) |> 
    dplyr::mutate(
      adt_ref = round(adt_ref, -1)
    ) |> 
    dplyr::left_join(
      autopass_trp_id,
      by = dplyr::join_by(trp_id == autopass_id)
    ) |> 
    dplyr::select(
      -trp_id
    ) |> 
    dplyr::rename(
      trp_id = nvdb_id
    )
  
}
  
adt_ref_year_all <-
  if(city_number == 960) {
    dplyr::bind_rows(
      adt_ref_year,
      adt_ref_year_toll
    )
  }else{
    adt_ref_year
  }
  
adt_filtered <- 
  adt |> 
  dplyr::filter(length_range == "[..,5.6)") |> 
  #dplyr::mutate(length_quality = aadt_valid_length / aadt_total * 100) %>%
  #dplyr::filter(length_quality > 90) %>%
  dplyr::filter(coverage > 50) |> 
  dplyr::group_by(trp_id) |> 
  dplyr::filter(year == max(year)) |> 
  dplyr::select(trp_id, adt = aadt_length_range, year)

if(city_number == 955){

  adt_filtered_bamble <-
    adt |> 
    dplyr::filter(length_range == "[..,5.6)") |> 
    dplyr::filter(coverage > 50) |> 
    dplyr::filter(trp_id %in% c("00344V521377", "86022V521170")) |> 
    dplyr::group_by(trp_id) |> 
    dplyr::filter(year == 2024) |> 
    dplyr::select(trp_id, adt = aadt_length_range, year)

  adt_filtered <-
    adt_filtered |> 
    dplyr::filter(
      !(trp_id %in% c("00344V521377", "86022V521170"))
    ) |>
    dplyr::bind_rows(
      adt_filtered_bamble
    )

}
  
if(city_number == 960){
  
  adt_toll <-
    readr::read_rds(
      file = "data_indexpoints_tidy/trd_toll_aadt.rds"
    ) |> 
    dplyr::slice_max(year, by = trp_id) |> 
    dplyr::filter(class == "lette") |> 
    dplyr::select(trp_id, adt = aadt, year) |> 
    dplyr::mutate(
      adt = round(adt, -1)
    ) |> 
    dplyr::left_join(
      autopass_trp_id,
      by = dplyr::join_by(trp_id == autopass_id)
    ) |> 
    dplyr::select(
      -trp_id
    ) |> 
    dplyr::rename(
      trp_id = nvdb_id
    )
  
  adt_filtered <-
    dplyr::bind_rows(
      adt_filtered,
      adt_toll
    )
    
}

this_citys_trps_all_adt <-
  trps_meta |> 
  dplyr::left_join(
    adt_filtered,
    by = "trp_id"
  )
  

# Special
# adt_light_years <-
#   adt |> 
#   tibble::as_tibble() |> 
#   dplyr::filter(
#     length_range == "[..,5.6)",
#     year >= 2017
#   ) |> 
#   dplyr::select(
#     trp_id,
#     year,
#     aadt = aadt_length_range
#     #aadt_valid_length,
#     #coverage
#   ) |> 
#   tidyr::pivot_wider(
#     names_from = year,
#     values_from = aadt
#   ) |> 
#   dplyr::left_join(
#     trps_meta,
#     by = "trp_id"
#   ) |> 
#   dplyr::select(
#     name,
#     road_reference,
#     road_category_and_number,
#     municipality_name,
#     "2017":"2022"
#   ) |> 
#   dplyr::arrange(
#     road_reference
#   )
# 
# writexl::write_xlsx(
#   adt_light_years,
#   "spesialuttak/nedre_glomma_adt.xlsx"
# )

# adt_filtered <-
#   adt %>%
#   dplyr::filter(
#     length_range %in% c("[..,5.6)", "[5.6,..)")
#     ) %>%
#   dplyr::mutate(
#     length_quality = round(aadt_valid_length / aadt_total * 100)
#   ) %>%
#   #dplyr::filter(
#   #  length_quality > 90
#   #) %>%
#   dplyr::filter(
#     coverage > 50
#   ) %>%
#   dplyr::mutate(
#     length_range =
#       dplyr::case_when(
#         length_range == "[..,5.6)" ~ "lette",
#         length_range == "[5.6,..)" ~ "tunge",
#         TRUE ~ length_range
#       )
#   ) %>%
#   dplyr::select(
#     trp_id,
#     year,
#     length_range,
#     aadt_length_range,
#     coverage,
#     aadt_total,
#     sd_total,
#     length_quality
#   ) %>%
#   tidyr::pivot_wider(
#     names_from = "length_range",
#     names_prefix = "aadt_",
#     values_from = "aadt_length_range"
#   ) %>%
#   dplyr::group_by(trp_id) %>%
#   dplyr::filter(year == max(year)) %>%
#   dplyr::select(
#     trp_id,
#     year,
#     coverage,
#     length_quality,
#     aadt_total,
#     aadt_lette,
#     aadt_tunge
#   )

missing_adt <-
  this_citys_trps_all_adt |> 
  dplyr::filter(is.na(adt))


if(nrow(missing_adt) == 0) {
  
  missing_adt_fixed <- data.frame()
  
}else {

  fetch_adts <-
    purrr::map_dfr(
      missing_adt$road_link_position,
      ~ get_historic_aadt_by_roadlinkposition(
        roadlinkposition = .
      )
    ) |>
    dplyr::group_by(
      road_link_position
    ) |>
    dplyr::slice_max(
      year,
      with_ties = FALSE
    ) |>
    dplyr::mutate(
      adt = round(aadt_total * (1 - heavy_percentage / 100), digits = -1)
    ) |>
    dplyr::select(
      road_link_position,
      adt,
      year
    )
  
  missing_adt_fixed <-
    missing_adt |>
    dplyr::select(-adt, -year) |>
    dplyr::left_join(
      fetch_adts,
      by = "road_link_position"
    )
}
  
  
# Finally all aadt
this_citys_trps_all_adt_final <-
  this_citys_trps_all_adt |> 
  dplyr::filter(!is.na(adt)) |> 
  dplyr::bind_rows(missing_adt_fixed) |> 
  dplyr::mutate(adt = round(adt, -1)) |> 
  dplyr::rename(year_aadt = year) |> 
  dplyr::left_join(
    adt_ref_year_all,
    by = join_by(trp_id)
  )
  
if(city_number == 960){

  trd_station_type <- readr::read_rds("trd_station_type.rds")
  
  this_citys_trps_all_adt_final <-
    this_citys_trps_all_adt_final |> 
    # TRD (city_index_dataprep_trondheim_toll_stations.R)
    dplyr::left_join(
      trd_station_type,
      by = "trp_id"
    ) |>
    dplyr::mutate(
      station_type_short = stringr::str_sub(station_type, 1, 1)
    )
}

readr::write_rds(
  this_citys_trps_all_adt_final,
  file = paste0(
    "index_trp_metadata/trp_",
    city_number,
    ".rds"
  )
)