# Seasonal index

# Starting with all city indexes from city_index_dataprep.R
city_indexes_tidy <-
  city_indexes |>
  dplyr::filter(
    road_category == "EUROPAVEG_RIKSVEG_FYLKESVEG_KOMMUNALVEG",
    length_range == "[..,5.6)",
    period == "month"
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

if(city_number == 16952) {

  city_index_monthly_winter <-
    base::list.files(path = "trp_index/tromso_winter", full.names = TRUE) |>
    purrr::map(~ readr::read_rds(.x)) |>
    purrr::list_rbind()

  trps_with_enough_data_2022 <-
    base::list.files(path = "trp_index/tromso", full.names = TRUE) |>
    purrr::map(~ readr::read_rds(.x)) |>
    purrr::list_rbind() |>
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_months = n(),
      .by = c(trp_id)
    ) |>
    dplyr::filter(
      n_months >= 6
    )
  
  city_index_monthly_2022 <-
    dplyr::bind_rows(
      city_index_monthly_winter |> 
        dplyr::filter(
          month == 12 & years == "2018-2021"
        ),
      base::list.files(path = "trp_index/tromso", full.names = TRUE) |>
      purrr::map(~ readr::read_rds(.x)) |>
      purrr::list_rbind() |>
      dplyr::filter(
        trp_id %in% trps_with_enough_data_2022$trp_id,
        !(month == 12 & years == "2019-2022")
      )
    ) |>
    dplyr::mutate(
      sum_traffic_base = sum(traffic_base),
      sum_traffic_calc = sum(traffic_calc),
      .by = month
    ) |> 
    dplyr::mutate(
      index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
      weight = traffic_base / sum_traffic_base,
      city_index = (sum_traffic_calc / sum_traffic_base - 1 ) * 100,
      deviation = weight * (index_p - city_index)^2
    ) |> 
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_trp = n(),
      sum_squared_weight = sum(weight^2),
      n_eff = 1 / sum_squared_weight,
      variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
      .by = month
    ) |>
    dplyr::mutate(
      year = dplyr::case_when(month == 12 ~ 2021, TRUE ~ 2022),
      index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
      standard_error = sqrt(sum_squared_weight * variance_p),
      ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error, 1),
      ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error, 1)
    ) |>
    dplyr::select(
      year,
      month,
      base_volume = traffic_base,
      calc_volume = traffic_calc
    )

  city_index_dec_21 <-
    city_index_monthly_winter |> 
    dplyr::filter(
      month == 12 & years == "2021-2022"
    ) |> 
    dplyr::mutate(
      sum_traffic_base = sum(traffic_base),
      sum_traffic_calc = sum(traffic_calc),
      .by = month
    ) |> 
    dplyr::mutate(
      index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
      weight = traffic_base / sum_traffic_base,
      city_index = (sum_traffic_calc / sum_traffic_base - 1 ) * 100,
      deviation = weight * (index_p - city_index)^2
    ) |> 
    dplyr::summarise(
      traffic_base = sum(traffic_base),
      traffic_calc = sum(traffic_calc),
      n_trp = n(),
      sum_squared_weight = sum(weight^2),
      n_eff = 1 / sum_squared_weight,
      variance_p = (1 / (1 - sum_squared_weight)) * sum(deviation),
      .by = month
    ) |>
    dplyr::mutate(
      year = 2022,
      index_p = ((traffic_calc / traffic_base - 1) * 100) |> round(1),
      standard_error = sqrt(sum_squared_weight * variance_p),
      ci_lower = round(index_p + stats::qt(0.025, n_trp - 1) * standard_error, 1),
      ci_upper = round(index_p - stats::qt(0.025, n_trp - 1) * standard_error, 1)
    ) |>
    dplyr::select(
      year,
      month,
      base_volume = traffic_base,
      calc_volume = traffic_calc
    )
}

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

index_season <- 
  dplyr::bind_rows(
    city_index_monthly_2022,
    city_indexes_tidy |> 
      dplyr::select(year, month, base_volume, calc_volume) |> 
      dplyr::filter(
        !(month == 12 & year %in% c(2025))
      ),
    city_index_dec_21
  )  |> 
  dplyr::mutate(
    season_year =
      dplyr::case_when(
        month == 12 ~ year + 1,
        TRUE ~ year
      ),
    season = 
      dplyr::case_when(
        month %in% c(12, 1, 2) ~ "winter",
        month %in% c(3:5) ~ "spring",
        month %in% c(6:8) ~ "summer",
        month %in% c(9:11) ~ "fall"
      ),
      season_id = paste0(season_year, "_", stringr::str_sub(season, 1, 4))
  ) |> 
  dplyr::summarise(
    sum_base = sum(base_volume),
    sum_calc = sum(calc_volume),
    index_i = sum_calc / sum_base,
    index_p = 100 * (index_i - 1),
    .by = c(season_id, season, season_year)
  )

index_season_chained <-
  index_season |> 
  dplyr::summarise(
    index_i = prod(index_i),
    .by = season
  ) |>
  dplyr::mutate(
    index_p = 100 * (index_i - 1),
  )




# Removing summer
# index_without_summer <-
#   city_indexes_tidy |>
#   dplyr::filter(
#     period == "month",
#     year < 2024,
#     !(month %in% c(5:8))
#   ) |>
#   dplyr::summarise(
#     sum_base = sum(base_volume),
#     sum_calc = sum(calc_volume),
#     index_i = sum_calc / sum_base,
#     index_p_without_summer = 100 * (index_i - 1),
#     .by = year
#   )

# index_just_summer <-
#   city_indexes_tidy |>
#   dplyr::filter(
#     period == "month",
#     year < 2024,
#     month %in% c(5:8)
#   ) |>
#   dplyr::summarise(
#     sum_base = sum(base_volume),
#     sum_calc = sum(calc_volume),
#     index_i = sum_calc / sum_base,
#     index_p_summer = 100 * (index_i - 1),
#     .by = year
#   )

# Compare
# index_seasons <-
#   year_to_date_from_api |>
#   dplyr::select(
#     year,
#     index_p_all_year = index_p
#   ) |>
#   dplyr::left_join(
#     index_without_summer,
#     by = join_by(year)
#   ) |>
#   dplyr::left_join(
#     index_just_summer,
#     by = join_by(year)
#   ) |>
#   dplyr::select(
#     year,
#     index_p_all_year,
#     index_p_without_summer,
#     index_p_summer
#   ) |>
#   dplyr::mutate(
#     dplyr::across(
#       tidyselect::starts_with("index_p"),
#       ~ round(.x, 1)
#     )
#   )

# readr::write_csv2(
#   index_seasons,
#   "spesialuttak/krs_summer_index.csv"
# )


# Chaining
# chained_index_season <-
#   city_indexes_tidy |>
#   dplyr::filter(
#     period == "month",
#     year < 2024
#   ) |>
#   dplyr::mutate(
#     summer = month %in% c(5:8)
#   ) |>
#   dplyr::summarise(
#     index_i = sum(calc_volume) / sum(base_volume),
#     .by = c(year, summer),
#   ) |>
#   dplyr::summarise(
#     chained_index_i = prod(index_i),
#     .by = c(summer)
#   ) |>
#   dplyr::mutate(
#     chained_index_p = 100 * (chained_index_i - 1),
#   )
