## Get MDTs ----
# TRPs
if(fetch_mdt_for_just_present_year) {
  mdt_years <- present_year
}else{
  mdt_years <- years_from_reference_to_today
}

{
  tictoc::tic()
  mdt <-
    purrr::map_dfr(
      mdt_years,
      ~ get_mdt_by_length_for_trp_list(city_trps, .x)
    )
  tictoc::toc()
}

mdt_filtered_update <-
  mdt |>
  dplyr::filter(length_range == "[..,5.6)") |>
  dplyr::mutate(
    mdt_valid_length = dplyr::case_when(
      is.na(total_coverage) ~ mdt_total, # If NorTraf, assume high quality
      TRUE ~ mdt_valid_length
    ),
    length_quality = mdt_valid_length / mdt_total * 100,
    coverage = dplyr::case_when(
      is.na(total_coverage) ~ 100, # If NorTraf, assume high quality
      TRUE ~ total_coverage * length_quality / 100
    ),
    heavy_percentage = (1 - mdt_length_range / mdt_total) * 100
  ) |>
  dplyr::left_join(
    trp_names,
    by = join_by(trp_id)
  ) |>
  dplyr::select(
    trp_id,
    year, month,
    mdt = mdt_length_range,
    coverage,
    length_quality,
    heavy_percentage
    #sub_area = municipality_name
  ) |>
  dplyr::mutate(
    year_month = lubridate::as_date(paste0(year, "-", month, "-01"))
  ) |>
  tibble::as_tibble()


if(fetch_mdt_for_just_present_year) {

  # Read back in for previous years
  mdt_filtered_old <- 
    readr::read_rds(paste0("data_indexpoints_tidy/mdt_", city_number, ".rds")) |> 
    dplyr::filter(
      year != present_year
    )

mdt_filtered <-
  dplyr::bind_rows(
    mdt_filtered_old,
    mdt_filtered_update
  )

}else{
  
  mdt_filtered <- mdt_filtered_update

}

# Toll data
if(toll_data_is_included){

  if(city_number == 960){
    toll_mdt_light_raw <- readr::read_rds("data_indexpoints_tidy/trd_toll_mdt.rds")
  }
  
  if(city_number == 19955){
    toll_mdt_light_raw <- readr::read_rds("data_indexpoints_tidy/haugesund_toll_mdt.rds")
  }

  toll_mdt_light <-
    toll_mdt_light_raw |>
    dplyr::rename(
      year_month = month
    ) |>
    dplyr::mutate(
      year = lubridate::year(year_month),
      month = lubridate::month(year_month),
      length_quality = 100
    ) |>
    dplyr::filter(
      class == "lette",
      year %in% mdt_years
    ) |>
    dplyr::select(
      -class,
      -n_days,
      -traffic
    )
  
  mdt_filtered <-
    mdt_filtered |>
    dplyr::bind_rows(toll_mdt_light)

  }

mdt_filtered |>
  readr::write_rds(
    file =
      paste0(
        "data_indexpoints_tidy/mdt_",
        city_number,
        ".rds"
      )
  )
