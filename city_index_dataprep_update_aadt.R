# All AADTs ----
adt <- get_aadt_by_length_for_trp_list(city_trps)
  

# AADT reference year ----
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
    year == adt_reference_year
  ) |> 
  dplyr::select(trp_id, adt_ref = aadt_length_range)  

if(toll_data_is_included) {

  if(city_number == 960){
      adt_toll_raw <- readr::read_rds("data_indexpoints_tidy/trd_toll_aadt.rds")
  }

  if(city_number == 19955){
    adt_toll_raw <- readr::read_rds("data_indexpoints_tidy/haugesund_toll_aadt.rds")
  }

  # Avoid using incomplete year
  if(latest_published_month < 12) {

    adt_toll_raw <-
      adt_toll_raw |> 
      dplyr::filter(
        year != index_year
      )

  }

  adt_ref_year_toll <-
    adt_toll_raw |> 
    dplyr::filter(
      year == reference_year,
      class == "lette"
    ) |> 
    # The trps_meta uses nvdb_id as trp_id for toll stations, but all toll data use autopass_id as trp_id.
    # So in order to match AADT values to the final trp list, we need to swap trp_id here from being autopass_id to become nvdb_id:
    dplyr::select(autopass_id = trp_id, adt_ref = aadt) |> 
    dplyr::inner_join(
      dplyr::select(
        trps_meta,
        trp_id, # here: nvdb_id
        autopass_id
      ),
      by = dplyr::join_by(autopass_id)
    ) |> 
    dplyr::select(
      -autopass_id
    )
  
}
 
adt_ref_year_all <-
  if(toll_data_is_included) {
    dplyr::bind_rows(
      adt_ref_year,
      adt_ref_year_toll
    )
  }else{
    adt_ref_year
  }


# AADT most recent year ----
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
  
if(toll_data_is_included){
  
  adt_toll_most_recent <-
    adt_toll_raw |> 
    dplyr::filter(class == "lette") |> 
    dplyr::slice_max(year, by = trp_id) |> 
    # The trps_meta uses nvdb_id as trp_id for toll stations, but all toll data use autopass_id as trp_id.
    # So in order to match AADT values to the final trp list, we need to swap trp_id here from being autopass_id to become nvdb_id:
    dplyr::select(autopass_id = trp_id, adt = aadt, year) |> 
    dplyr::inner_join(
      dplyr::select(
        trps_meta,
        trp_id, # here: nvdb_id
        autopass_id
      ),
      by = dplyr::join_by(autopass_id)
    ) |> 
    dplyr::select(
      -autopass_id
    )
  
  adt_filtered <-
    dplyr::bind_rows(
      adt_filtered,
      adt_toll_most_recent
    )
    
}

this_citys_trps_all_adt <-
  trps_meta |> 
  dplyr::left_join(
    adt_filtered,
    by = "trp_id"
  )
  

# Fetch missing AADTs from NVDB ----
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
  
  
# Finally all AADT ----
this_citys_trps_all_adt_final <-
  this_citys_trps_all_adt |> 
  dplyr::filter(!is.na(adt)) |> 
  dplyr::bind_rows(missing_adt_fixed) |> 
  dplyr::rename(year_aadt = year) |> 
  dplyr::left_join(
    adt_ref_year_all,
    by = join_by(trp_id)
  ) |> 
  dplyr::mutate(
    adt = round(adt, -1),
    adt_ref = round(adt_ref, -1)
  ) |> 
  dplyr::arrange(
    road_category_and_number, name
  )

readr::write_rds(
  this_citys_trps_all_adt_final,
  file = paste0(
    "index_trp_metadata/trp_",
    city_number,
    ".rds"
  )
)
