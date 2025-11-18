# Yearly city index sub area

city_index_sub_area <-
  trp_index_so_far |>
  #trp_index_year_to_date_dec_bind |>
  dplyr::left_join(sub_areas, by = "trp_id") |>
  dplyr::group_by(year, sub_area) |>
  dplyr::group_modify(
    ~ calculate_area_index(.)
  ) |>
  dplyr::ungroup()

# Weighting standard error
#trp_index_year_to_date_dec_sub <-
trp_index_so_far_sub <-
  #trp_index_year_to_date_dec_bind |>
  trp_index_so_far |>
  dplyr::left_join(
    sub_areas,
    by = "trp_id"
  ) |>
  dplyr::filter(!is.na(base_volume)) |>
  dplyr::group_by(year, sub_area) |>
  dplyr::mutate(
    city_base_volume = sum(base_volume),
    squared_weight = (base_volume / sum(base_volume))^2
  ) |>
  dplyr::summarise(
    sum_of_squared_weights = sum(squared_weight),
    .groups = "drop"
  )

city_index_full_years_sub <-
  city_index_sub_area |>
  dplyr::left_join(
    trp_index_so_far_sub,
    by = dplyr::join_by(year, sub_area)
  ) |>
  dplyr::mutate(
    year_base = year - 1,
    index_i = index_converter(index_p),
    variance = standard_deviation^2,
    standard_error = sqrt(sum_of_squared_weights) * standard_deviation
  ) |>
  dplyr::select(
    sub_area,
    year_base,
    year,
    month,
    index_p,
    index_i,
    standard_deviation,
    variance,
    n_trp,
    standard_error,
    sum_of_squared_weights
  ) |>
  dplyr::arrange(year)

unique_sub_areas <- sub_areas$sub_area |> base::unique()

## Chained city index
city_index_yearly_all_sub <- tibble::tibble()

for(i in 1:length(unique_sub_areas)) {

  city_index_full_years_i <-
    city_index_full_years_sub |>
    dplyr::filter(
      sub_area == unique_sub_areas[i]
    )

  years_1_2 <- calculate_two_year_index(city_index_full_years_i)

  years_1_3 <-
    bind_rows(years_1_2, slice(city_index_full_years_i, 3)) %>%
    calculate_two_year_index()

  years_1_4 <-
    bind_rows(years_1_3, slice(city_index_full_years_i, 4)) %>%
    calculate_two_year_index()

  years_1_5 <-
    bind_rows(years_1_4, slice(city_index_full_years_i, 5)) %>%
    calculate_two_year_index()

  years_1_6 <-
    bind_rows(years_1_5, slice(city_index_full_years_i, 6)) %>%
    calculate_two_year_index()

  years_1_7 <-
    bind_rows(years_1_6, slice(city_index_full_years_i, 7)) %>%
    calculate_two_year_index()

  # years_1_8 <-
  #   bind_rows(years_1_7, slice(city_index_full_years_i, 8)) %>%
  #   calculate_two_year_index()

  city_index_yearly_all_sub_i <-
    city_index_full_years_i |>
    dplyr::mutate(index_type = "direct") |>
    dplyr::bind_rows(
      # Include only for full years
      years_1_2,
      years_1_3,
      years_1_4,
      years_1_5,
      years_1_6,
      years_1_7,
      # years_1_8
    ) |>
    dplyr::mutate(
      year_from_to = paste0(year_base, "-", year),
      sub_area = unique_sub_areas[i],
      month_name_short = lubridate::month(month, label = TRUE),
      period = paste0("jan-", month_name_short),
      index_p = round(index_p, 1),
      ci_lower = round(index_p - 1.96 * standard_error, 1),
      ci_upper = round(index_p + 1.96 * standard_error, 1)
    ) |>
    dplyr::select(
      -standard_deviation,
      -variance,
      -sum_of_squared_weights
    )

  city_index_yearly_all_sub <-
    dplyr::bind_rows(
      city_index_yearly_all_sub,
      city_index_yearly_all_sub_i
    )

}

readr::write_rds(
  city_index_yearly_all_sub,
  file = paste0("data_indexpoints_tidy/byindeks_sub_", file_name_addition, city_number, ".rds")
)

if(so_far) {
  city_index_yearly_all_sub_so_far <- city_index_yearly_all_sub
}