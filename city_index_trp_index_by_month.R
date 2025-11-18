# TRP index by month for Excel

if( city_number %in% c(1952, 955, 952, 959) ){

  trp_index_monthly_pre_2020 <-
    purrr::map_dfr(
      index_years_pre_2020,
      ~ read_old_pointindex_csv_monthly(
          paste0("data_index_raw/pointindex_", city_number, "_", .x, ".csv")
        ) |>
        dplyr::mutate(year = .x)
    ) |>
    dplyr::left_join(
      trp_id_msnr,
      by = "msnr"
    ) |>
    dplyr::select(-msnr)

}

trp_index_monthly_from_2020 <-
  trp_index_from_2020 |>
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "month"
  ) |>
  dplyr::select(
    trp_id,
    year,
    month,
    index = index_short
  )

if( !(city_number %in% c(1952, 955, 952, 959)) ){

  trp_index_monthly <- trp_index_monthly_from_2020

}else{

  trp_index_monthly <-
    dplyr::bind_rows(
      trp_index_monthly_pre_2020,
      trp_index_monthly_from_2020
    )

}

trp_index_monthly_wide <-
  trp_index_monthly |>
  tidyr::complete(
    trp_id,
    year,
    month
  ) |>
  dplyr::mutate(
    month_label =
      paste0(
        "m_",
        lubridate::make_date(
          year = 2000,
          month = month,
          day = 1
        ) |>
          lubridate::month(label = TRUE)
      )
  ) |>
  dplyr::select(
    trp_id,
    year,
    month_label,
    index
  ) |>
  tidyr::pivot_wider(
    names_from = "month_label",
    values_from = "index"
  ) |>
  dplyr::left_join(
    this_citys_trps_all_adt_final,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_category_and_number,
    year,
    tidyselect::starts_with("m_")
  ) |>
  dplyr::arrange(
    name,
    trp_id,
    year
  ) |>
  dplyr::rename_with(
    ~ stringr::str_remove(.x, "m_"),
    tidyselect::starts_with("m_")
  )
