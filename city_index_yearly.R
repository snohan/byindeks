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
# test <-
#   trp_index_year_to_date_dec_bind |>
#   dplyr::group_by(year) |>
#   dplyr::group_modify(
#     ~ calculate_area_index(.)
#   )


## Chained city index
source("city_index_yearly_chain.R")
