# Yearly city index

# Weighting standard error
city_index_n_trp <-
  trp_index_so_far |>
  dplyr::filter(!is.na(base_volume)) |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    city_base_volume = base::sum(base_volume),
    squared_weight = (base_volume / base::sum(base_volume))^2
  ) |>
  dplyr::summarise(
    n_trp = n(),
    sum_of_squared_weights = base::sum(squared_weight)
  )

if(so_far) {
  city_index_months <- index_months_so_far
}else{
  city_index_months <- index_months
}

{
  city_indexes <-
    purrr::map2(
      index_years,
      city_index_months,
      ~ get_published_index_for_months(city_number, .x, .y)
    ) |>
    purrr::list_rbind() |>
    dplyr::filter(day_type == "ALL")
    # ALL, WEEKDAY or WEEKEND

  city_name <- city_indexes$area_name[nrow(city_indexes)]

  if(city_number == 16952) {
    city_name <- "Tromsø"
  }

  if(city_number == 18952) {
    city_name <- "Nedre Glomma"
  }

  if(city_number == 19953) {
    city_name <- "Kristiansandsregionen"
  }
}

city_index_full_years <-
  city_indexes |>
  dplyr::filter(
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    period == "year_to_date"
  ) |>
  dplyr::slice_max(
    order_by = month,
    by = year
  ) |>
  dplyr::left_join(
    city_index_n_trp,
    by = "year"
  ) |>
  dplyr::mutate(
    year_base = year - 1,
    index_i = index_converter(index_p),
    variance = standard_deviation^2,
    standard_error = sqrt(sum_of_squared_weights) * standard_deviation
  ) |>
  dplyr::select(
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

# Test: this should reproduce city index from TRP index:
test <-
  trp_index_year_to_date_dec_bind |>
  dplyr::group_by(year) |>
  dplyr::group_modify(
    ~ calculate_area_index(.)
  )


## Chained city index
years_1_2 <- calculate_two_year_index(city_index_full_years)

years_1_3 <-
  bind_rows(years_1_2, slice(city_index_full_years, 3)) %>%
  calculate_two_year_index()

years_1_4 <-
  bind_rows(years_1_3, slice(city_index_full_years, 4)) %>%
  calculate_two_year_index()

years_1_5 <-
  bind_rows(years_1_4, slice(city_index_full_years, 5)) %>%
  calculate_two_year_index()

years_1_6 <-
  bind_rows(years_1_5, slice(city_index_full_years, 6)) %>%
  calculate_two_year_index()

years_1_7 <-
  bind_rows(years_1_6, slice(city_index_full_years, 7)) %>%
  calculate_two_year_index()

# years_1_8 <-
#   bind_rows(years_1_7, slice(city_index_full_years, 8)) %>%
#   calculate_two_year_index()

# Skipping intermediate years, adding just from first to last?
city_index_yearly_all <-
  city_index_full_years |>
  dplyr::mutate(
    index_type = "direct"
  ) |>
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
    area_name = city_name,
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

readr::write_rds(
  city_index_yearly_all,
  file = paste0("data_indexpoints_tidy/byindeks_", file_name_addition, city_number, ".rds")
)

if(so_far) {
  city_index_yearly_all_so_far <- city_index_yearly_all
}