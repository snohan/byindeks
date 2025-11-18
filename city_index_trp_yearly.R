# Yearly TRP index

if(so_far) {
  end_month <- index_month
  index_months_chosen <- index_months_from_2020_so_far
  file_name_addition <- "so_far_"
}else{
  end_month <- 12
  index_months_chosen <- index_months_from_2020
  file_name_addition <- ""
}


# Still need to specify csv-files for years before 2020 to get the pointindex as they are not in API
# These old CSV-files have only so-far-values for December,
# but so-far indexes may be reproduced by summing up volumes for the individual months.

if( city_number %in% c(1952, 955, 952, 959) ) {

  trp_index_so_far_pre_2020 <-
    purrr::map(
      index_years_pre_2020,
      ~ read_old_pointindex_csv_monthly_with_volumes(
          filename = paste0("data_index_raw/pointindex_", city_number, "_", .x, ".csv"),
          given_year = .x
      )
    ) |>
    purrr::list_rbind() |>
    dplyr::left_join(
      trp_id_msnr,
      by = "msnr"
    ) |>
    dplyr::select(-msnr) |>
    dplyr::relocate(trp_id) |>
    dplyr::filter(
      month <= end_month
    ) |>
    dplyr::summarise(
      base_volume = base::sum(trafikkmengde.basisaar),
      calc_volume = base::sum(trafikkmengde.indeksaar),
      .by = c(trp_id, year)
    ) |>
    dplyr::mutate(
      month = end_month,
      index = 100 * (calc_volume / base_volume - 1)
    )

}else{

  trp_index_so_far_pre_2020 <- data.frame()

}

trp_index_from_2020 <-
  purrr::map2(
    index_years_from_2020,
    index_months_chosen,
    ~ get_published_pointindex_for_months(city_number, .x, .y)[[2]]
  ) |>
  purrr::list_rbind() |>
  dplyr::filter(
    trp_id != "98963V1719019" # Sandesund sør is wrongly included in API response
  )

trp_index_so_far_from_2020 <-
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

trp_index_so_far <-
  dplyr::bind_rows(
    trp_index_so_far_pre_2020,
    trp_index_so_far_from_2020
  )