## Get MDTs ----
{
  tictoc::tic()
  mdt <-
    purrr::map_dfr(
      years_from_reference_to_today,
      ~ get_mdt_by_length_for_trp_list(city_trps, .x)
    )
  tictoc::toc()
}

mdt_filtered <-
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