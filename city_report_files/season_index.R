# Seasonal index

# Starting with all city indexes from city_index_dataprep.R

city_indexes_tidy <-
  city_indexes |>
  dplyr::filter(
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)"
  ) |>
  dplyr::select(
    year,
    month,
    period,
    index_p,
    index_i,
    base_volume,
    calc_volume
  )

# year_to_date_from_api <-
#   city_indexes_tidy |>
#   dplyr::filter(
#     period == "year_to_date",
#     month == 12
#   )
#
# year_to_date_from_monthly <-
#   city_indexes_tidy |>
#   dplyr::filter(
#     period == "month",
#     year < 2024
#   ) |>
#   dplyr::summarise(
#     sum_base = sum(base_volume),
#     sum_calc = sum(calc_volume),
#     index_i = sum_calc / sum_base,
#     index_p = 100 * (index_i - 1),
#     .by = year
#   )

# Yields same values when all months are included! :)

# Removing summer
index_without_summer <-
  city_indexes_tidy |>
  dplyr::filter(
    period == "month",
    year < 2024,
    !(month %in% c(5:8))
  ) |>
  dplyr::summarise(
    sum_base = sum(base_volume),
    sum_calc = sum(calc_volume),
    index_i = sum_calc / sum_base,
    index_p_without_summer = 100 * (index_i - 1),
    .by = year
  )

index_just_summer <-
  city_indexes_tidy |>
  dplyr::filter(
    period == "month",
    year < 2024,
    month %in% c(5:8)
  ) |>
  dplyr::summarise(
    sum_base = sum(base_volume),
    sum_calc = sum(calc_volume),
    index_i = sum_calc / sum_base,
    index_p_summer = 100 * (index_i - 1),
    .by = year
  )

# Compare
index_seasons <-
  year_to_date_from_api |>
  dplyr::select(
    year,
    index_p_all_year = index_p
  ) |>
  dplyr::left_join(
    index_without_summer,
    by = join_by(year)
  ) |>
  dplyr::left_join(
    index_just_summer,
    by = join_by(year)
  ) |>
  dplyr::select(
    year,
    index_p_all_year,
    index_p_without_summer,
    index_p_summer
  ) |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::starts_with("index_p"),
      ~ round(.x, 1)
    )
  )

# readr::write_csv2(
#   index_seasons,
#   "spesialuttak/krs_summer_index.csv"
# )


# Chaining
chained_index_season <-
  city_indexes_tidy |>
  dplyr::filter(
    period == "month",
    year < 2024
  ) |>
  dplyr::mutate(
    summer = month %in% c(5:8)
  ) |>
  dplyr::summarise(
    index_i = sum(calc_volume) / sum(base_volume),
    .by = c(year, summer),
  ) |>
  dplyr::summarise(
    chained_index_i = prod(index_i),
    .by = c(summer)
  ) |>
  dplyr::mutate(
    chained_index_p = 100 * (chained_index_i - 1),
  )
