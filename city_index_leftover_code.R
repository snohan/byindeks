# Still need to specify csv-files for years before 2020 to get the pointindex as they are not in API
if((city_number %in% c(1952, 955, 952, 959))){
  trp_index_so_far_by_dec_pre_2020 <-
    purrr::map(
      index_years_pre_2020,
      ~ read_pointindex_CSV(
        paste0("data_index_raw/pointindex_", city_number, "_", .x, ".csv")
      ) |>
        dplyr::rename(
          index = 2
        ) |>
        dplyr::mutate(
          year = .x,
          month = 12
          # So far
          #month = index_month
        )
    ) |>
    purrr::list_rbind() |>
    dplyr::left_join(
      trp_id_msnr,
      by = "msnr"
    ) |>
    dplyr::select(-msnr)
}else{
  trp_index_so_far_by_dec_pre_2020 <- data.frame()
}

trp_index_from_2020 <-
  purrr::map2(
    index_years_from_2020,
    index_months_from_2020,
    # So far
    # index_months_from_2020_so_far,
    ~ get_published_pointindex_for_months(city_number, .x, .y)[[2]]
  ) |>
  purrr::list_rbind() |>
  dplyr::filter(
    trp_id != "98963V1719019" # Sandesund sør is wrongly included in API response
  )

trp_index_so_far_by_dec_from_2020 <-
  trp_index_from_2020 |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "year_to_date"
  ) |>
  dplyr::slice_max(
    order_by = month,
    by = c(trp_id, year)
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    base_volume = length_base_volume_short,
    calc_volume = length_calc_volume_short,
    index = index_short
  )

if(!(city_number %in% c(1952, 955, 952, 959))){
  trp_index_year_to_date_dec_bind <-
    dplyr::bind_rows(
      trp_index_so_far_by_dec_from_2020
    )
}else{
  trp_index_year_to_date_dec_bind <-
    dplyr::bind_rows(
      trp_index_so_far_by_dec_pre_2020,
      trp_index_so_far_by_dec_from_2020
    )
}

# Weighting standard error
trp_index_year_to_date_dec <-
  trp_index_year_to_date_dec_bind |>
  dplyr::filter(!is.na(base_volume)) |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    city_base_volume = sum(base_volume),
    squared_weight = (base_volume / sum(base_volume))^2
  ) |>
  dplyr::summarise(
    n_trp = n(),
    sum_of_squared_weights = sum(squared_weight)
  )